{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

module Git.Plantation.Job.Server where

import           RIO
import qualified RIO.List                    as List

import           Git.Plantation.Data.Job     (Job)
import qualified Git.Plantation.Data.Job     as Job
import qualified Git.Plantation.Job.Protocol as Protocol
import qualified Git.Plantation.Job.Worker   as Worker
import qualified Mix.Plugin.Persist.Sqlite   as MixDB
import qualified Network.WebSockets          as WS

data Error
    = JobIsNotFount Job.Name
    | WorkerIsNotExist
    deriving (Show, Eq)

type ServerEnv env = (Worker.HasWorkers env, MixDB.HasSqliteConfig env, HasLogFunc env)

kickJob :: ServerEnv env => [Job.Config] -> Job.Name -> RIO env (Either Error Job)
kickJob configs name =
      case List.find (\config -> config ^. #name == name) configs of
        Nothing ->
          pure $ Left (JobIsNotFount name)
        Just _ -> do
          w <- Worker.getRandom
          case w of
            Nothing ->
              pure $ Left WorkerIsNotExist
            Just worker -> do
              job <- Job.create name
              liftIO $ WS.sendBinaryData (worker ^. #conn) $ Protocol.Enqueue (job ^. #id) (job ^. #name)
              pure $ Right job

serveRunner :: ServerEnv env => WS.Connection -> RIO env ()
serveRunner conn = do
  worker <- Worker.connected conn
  logDebug $ "Connected worker " <> display (worker ^. #id)
  -- WS.sendBinaryData worker.conn (Protocol.JobConfigs $ config.job)
  logDebug $ "Setuped worker " <> display (worker ^. #id)
  _ <- forever (receive worker) `finally` Worker.disconnected (worker ^. #id)
  logDebug $ "Disconnected worker " <> display (worker ^. #id)
  where
    receive worker = do
      p <- liftIO $ WS.receiveData (worker ^. #conn)
      case p of
        Protocol.JobRunning jid ->
          Job.updateToRunning jid
        Protocol.JobSuccess jid ->
          Job.updateToSuccess jid
        Protocol.JobFailure jid ->
          Job.updateToFailure jid
        _ ->
          pure ()

serveRunner' :: ServerEnv env => WS.PendingConnection -> RIO env ()
serveRunner' pending = serveRunner =<< liftIO (WS.acceptRequest pending)

migrate :: (MixDB.HasSqliteConfig env, HasLogFunc env) => RIO env ()
migrate = do
  (MixDB.Config config) <- asks (view MixDB.configL)
  let connName = config ^. (#info . MixDB.sqlConnectionStr)
  logInfo (display $ "Migate SQLite: " <> connName)
  MixDB.runMigrate Job.migrateAll
