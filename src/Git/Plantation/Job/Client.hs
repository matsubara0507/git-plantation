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
import qualified RIO.ByteString.Lazy         as BL
import           RIO.Process

import           Data.Coerce                 (coerce)
import           Data.Extensible
import qualified Data.List                   as List
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
   , "configs"        >: TVar [Job.Config]
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
    Protocol.JobConfigs newConfigs -> do
      configs <- asks (view #configs)
      atomically $ writeTVar configs newConfigs
    Protocol.Enqueue jid jname -> do
      queue <- asks (view #queue)
      atomically $ writeTQueue queue (Job.new jname jid)
    _ ->
      pure ()

runJob :: WS.Connection -> RIO Env ()
runJob conn = do
  job <- atomically . readTQueue =<< asks (view #queue)
  configs <- readTVarIO =<< asks (view #configs)
  case List.find (\config -> config ^. #name == (job ^. #name :: Job.Name)) configs of
    Nothing ->
      liftIO $ WS.sendBinaryData conn (Protocol.JobFailure $ job ^. #id)
    Just config -> do
      liftIO $ WS.sendBinaryData conn (Protocol.JobRunning $ job ^. #id)
      logDebug $ "Run: " <> fromString (coerce $ config ^. #name)
      (code, out, err) <- Docker.run config
      unless (BL.null out) $ do
        logDebug $ "=== STDOUT ===\n" <> displayBytesUtf8 (BL.toStrict out)
      unless (BL.null err) $ do
        logDebug $ "=== STDERR ===\n" <> displayBytesUtf8 (BL.toStrict err)
      case code of
        ExitSuccess ->
          liftIO $ WS.sendBinaryData conn (Protocol.JobSuccess $ job ^. #id)
        ExitFailure _ ->
          liftIO $ WS.sendBinaryData conn (Protocol.JobFailure $ job ^. #id)
