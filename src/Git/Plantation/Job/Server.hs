{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

module Git.Plantation.Job.Server where

import           RIO
import qualified RIO.List                    as List

import           Data.Extensible
import           Git.Plantation.Config       (Config, askConfig)
import           Git.Plantation.Data.Job     (Job)
import qualified Git.Plantation.Data.Job     as Job
import qualified Git.Plantation.Data.Problem as Problem
import qualified Git.Plantation.Data.Team    as Team
import qualified Git.Plantation.Data.User    as User
import qualified Git.Plantation.Job.Protocol as Protocol
import qualified Git.Plantation.Job.Store    as Store
import qualified Git.Plantation.Job.Worker   as Worker
import qualified Mix.Plugin.Config           as MixConfig
import qualified Mix.Plugin.Persist.Sqlite   as MixDB
import qualified Network.WebSockets          as WS

data Error
    = ProblemIsNotFount Problem.Id
    | TeamIsNotFount Team.Id
    | UserIsNotFound User.GitHubId
    | WorkerIsNotExist
    deriving (Show, Eq)

type ServerEnv env =
    ( Worker.HasWorkers env
    , Store.HasStore env
    , MixDB.HasSqliteConfig env
    , MixConfig.HasConfig Config env
    , HasLogFunc env
    )

kickJob :: ServerEnv env => Problem.Id -> Team.Id -> User.GitHubId -> RIO env (Either Error Job)
kickJob pid tid uid = do
  config <- askConfig
  case (List.find (\p -> p ^. #id == pid) $ config ^. #problems, List.find (\t -> t ^. #id == tid) $ config ^. #teams) of
    (Nothing, _) ->
      pure $ Left (ProblemIsNotFount pid)
    (_, Nothing) ->
      pure $ Left (TeamIsNotFount tid)
    (Just _, Just team) ->
      if List.any (\u -> u ^. #github == uid) $ team ^. #member then do
        w <- Worker.getRandom
        case w of
          Nothing ->
            pure $ Left WorkerIsNotExist
          Just worker -> do
            job <- Store.withStore $ Job.create pid tid uid
            liftIO $ WS.sendBinaryData (worker ^. #conn) $ Protocol.Enqueue (job ^. #id) pid tid uid
            pure $ Right job
      else
        pure $ Left (UserIsNotFound uid)

serveRunner :: ServerEnv env => WS.Connection -> RIO env ()
serveRunner conn = do
  config <- askConfig
  worker <- Worker.connected conn
  logDebug $ "Connected worker " <> display (worker ^. #id)
  liftIO $ WS.sendBinaryData (worker ^. #conn) (Protocol.JobConfig $ shrink config)
  logDebug $ "Setuped worker " <> display (worker ^. #id)
  _ <- forever (receive worker) `finally` Worker.disconnected (worker ^. #id)
  logDebug $ "Disconnected worker " <> display (worker ^. #id)
  where
    receive worker = do
      p <- liftIO $ WS.receiveData (worker ^. #conn)
      case p of
        Protocol.JobRunning jid -> do
          _ <- Store.withStore $ Job.updateToRunning jid
          pure ()
        Protocol.JobSuccess jid -> do
          _ <- Store.withStore $ Job.updateToSuccess jid
          pure ()
        Protocol.JobFailure jid -> do
          _ <- Store.withStore $ Job.updateToFailure jid
          pure ()
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
