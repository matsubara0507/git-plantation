{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

module Git.Plantation.Job.Server
  ( ServerEnv
  , Error (..)
  , kickJob
  , serveRunner
  , serveRunner'
  , migrate
  ) where

import           RIO
import qualified RIO.List                    as List
import qualified RIO.Text                    as Text

import           Data.Coerce                 (coerce)
import           Data.Extensible
import           Git.Plantation.Config       (Config, askConfig)
import           Git.Plantation.Data.Job     (Job)
import qualified Git.Plantation.Data.Job     as Job
import           Git.Plantation.Data.Problem (Problem)
import qualified Git.Plantation.Data.Problem as Problem
import qualified Git.Plantation.Data.Slack   as Slack
import           Git.Plantation.Data.Team    (Team)
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
    , Slack.HasSlackNotifyConfig env
    , MixDB.HasSqliteConfig env
    , MixConfig.HasConfig Config env
    , HasLogFunc env
    )

kickJob :: ServerEnv env => Problem.Id -> Team.Id -> Maybe User.GitHubId -> RIO env (Either Error Job)
kickJob pid tid uid = do
  config <- askConfig
  case (findProblem config pid, findTeam config tid) of
    (Nothing, _) ->
      pure $ Left (ProblemIsNotFount pid)
    (_, Nothing) ->
      pure $ Left (TeamIsNotFount tid)
    (Just _, Just _) -> do
      w <- Worker.getRandom
      case w of
        Nothing ->
          pure $ Left WorkerIsNotExist
        Just worker -> do
          job <- Store.withStore $ Job.create pid tid uid
          liftIO $ WS.sendBinaryData (worker ^. #conn) $ Protocol.Enqueue (job ^. #id) pid tid uid
          pure $ Right job

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
        Protocol.JobSuccess jid out err -> do
          let out' = Text.decodeUtf8With Text.lenientDecode out
              err' = Text.decodeUtf8With Text.lenientDecode err
          job <- Store.withStore $ Job.updateToSuccess jid out' err'
          notifySlack job
        Protocol.JobFailure jid out err -> do
          let out' = Text.decodeUtf8With Text.lenientDecode out
              err' = Text.decodeUtf8With Text.lenientDecode err
          job <- Store.withStore $ Job.updateToFailure jid out' err'
          notifySlack job
        _ ->
          pure ()

serveRunner' :: ServerEnv env => WS.PendingConnection -> RIO env ()
serveRunner' pending = serveRunner =<< liftIO (WS.acceptRequest pending)

findProblem :: Config -> Problem.Id -> Maybe Problem
findProblem config pid = List.find (\p -> p ^. #id == pid) $ config ^. #problems

findTeam :: Config -> Team.Id -> Maybe Team
findTeam config tid = List.find (\t -> t ^. #id == tid) $ config ^. #teams

notifySlack :: ServerEnv env => Job -> RIO env ()
notifySlack job = do
  config <- askConfig
  case (findProblem config $ job ^. #problem, findTeam config $ job ^. #team) of
    (Just problem, Just team) ->
        Slack.uploadFile
            $ #content         @= job ^. #stdout
           <: #filename        @= coerce (team ^. #name) <> "-" <> coerce (problem ^. #name) <> "-log.txt"
           <: #filetype        @= "text"
           <: #initial_comment @= mkMessage problem team
           <: nil
    (Nothing, _) ->
        logWarn "problem is not found when notify slack"
    (_, Nothing) ->
        logWarn "team is not found when notify slack"
  where
    mkMessage problem team = mconcat $
      if job ^. #success then
        [ "チーム ", coerce $ team ^. #name, " が ", coerce $ problem ^. #name," を正解したみたいだよ！すごーい！！" ]
      else
        [ "チーム ", coerce $ team ^. #name, " の ", coerce $ problem ^. #name," は不正解だね...残念！" ]

migrate :: (MixDB.HasSqliteConfig env, HasLogFunc env) => RIO env ()
migrate = do
  (MixDB.Config config) <- asks (view MixDB.configL)
  let connName = config ^. (#info . MixDB.sqlConnectionStr)
  logInfo (display $ "Migate SQLite: " <> connName)
  MixDB.runMigrate Job.migrateAll
