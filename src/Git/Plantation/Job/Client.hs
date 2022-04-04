{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Git.Plantation.Job.Client where

import           RIO
import qualified RIO.ByteString              as B
import qualified RIO.ByteString.Lazy         as BL
import           RIO.Process

import           Data.Extensible
import           Git.Plantation.Data.Job     (Job)
import qualified Git.Plantation.Data.Job     as Job
import qualified Git.Plantation.Job.Docker   as Docker
import qualified Git.Plantation.Job.Protocol as Protocol
import           Mix.Plugin.Logger           ()
import qualified Network.WebSockets          as WS
import           UnliftIO.Concurrent         (forkIO)

type Env = Record
  '[ "logger"         >: LogFunc
   , "processContext" >: ProcessContext
   , "config"         >: TVar Job.Config
   , "queue"          >: TQueue Job
   ]

instance Lookup xs "processContext" ProcessContext => HasProcessContext (Record xs) where
  processContextL = lens (view #processContext) (\x y -> x & #processContext `set` y)

-- dest is expected 'localhost:8080/hoge/fuga'
parseDest :: String -> Maybe (String, Int, String)
parseDest dest =
  (host,, path) <$> readMaybe port'
  where
    (addr, path)  = span (/= '/') dest
    (host, port') = drop 1 <$> span (/= ':') addr

run :: String -> RIO Env ()
run dest =
  case parseDest dest of
    Nothing ->
      logError $ "cannot parse " <> fromString dest

    Just (host, port, path) -> do
      logDebug $ "Connecting to " <> fromString dest
      runInIO <- askRunInIO
      let opt = WS.defaultConnectionOptions { WS.connectionOnPong = runInIO $ logDebug "Pong" }
      liftIO $ WS.runClientWith host port path opt [] $ \conn ->
        liftIO $ WS.withPingThread conn 30 (runInIO $ logDebug "Ping") $ do
          runInIO $ logDebug $ "Connected to " <> fromString dest
          _ <- forkIO $ forever (runInIO $ runJob conn)
          forever (runInIO $ receive conn) `finally` runInIO (logDebug "Close worker")

receive :: WS.Connection -> RIO Env ()
receive conn = do
  p <- liftIO $ WS.receiveData conn
  case p of
    Protocol.JobConfig newConfigs -> do
      configs <- asks (view #config)
      atomically $ writeTVar configs newConfigs
    Protocol.Enqueue jid pid tid uid -> do
      queue <- asks (view #queue)
      atomically $ writeTQueue queue (Job.new pid tid uid jid)
    _ ->
      pure ()

runJob :: WS.Connection -> RIO Env ()
runJob conn = do
  job <- atomically . readTQueue =<< asks (view #queue)
  config <- readTVarIO =<< asks (view #config)
  case (Job.findProblem config job, Job.findTeam config job) of
    (Just problem, Just team) -> do
      liftIO $ WS.sendBinaryData conn (Protocol.JobRunning $ job ^. #id)
      logDebug $ "Run: " <> display (problem ^. #name) <> "/" <> display (team ^. #name)
      (code, out, err) <- Docker.run config problem team
      let out' = BL.toStrict out
          err' = BL.toStrict err
      unless (B.null out') $ do
        logDebug $ "=== STDOUT ===\n" <> displayBytesUtf8 out'
      unless (B.null err') $ do
        logDebug $ "=== STDERR ===\n" <> displayBytesUtf8 err'
      case code of
        ExitSuccess ->
          liftIO $ WS.sendBinaryData conn (Protocol.JobSuccess (job ^. #id) out' err')
        ExitFailure _ ->
          liftIO $ WS.sendBinaryData conn (Protocol.JobFailure (job ^. #id) out' err')
    _ ->
      liftIO $ WS.sendBinaryData conn (Protocol.JobFailure (job ^. #id) "" "")

