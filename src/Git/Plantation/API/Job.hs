{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Git.Plantation.API.Job where

import           RIO
import qualified RIO.List                    as List
import qualified RIO.Map                     as Map
import qualified RIO.Text                    as Text

import           Data.Coerce                 (coerce)
import           Data.Extensible
import           Git.Plantation.Data.Job     (Job)
import qualified Git.Plantation.Data.Job     as Job
import qualified Git.Plantation.Data.Problem as Problem
import qualified Git.Plantation.Data.Team    as Team
import qualified Git.Plantation.Data.User    as User
import qualified Git.Plantation.Job.Server   as Job
import           Git.Plantation.Job.Store    (askStore)
import qualified Git.Plantation.Job.Worker   as Worker
import qualified Network.Wreq                as W
import           Servant

type API
      = "workers" :> Get '[JSON] [Worker.Info]
   :<|> "jobs" :> Get '[JSON] [Job] -- get by cache
   :<|> "jobs" :> "queuing" :> Get '[JSON] [Job]
   :<|> "jobs" :> "running" :> Get '[JSON] [Job]
   :<|> "jobs" :> "team" :> Capture "team" Team.Id :> Get '[JSON] [Job]
   :<|> "jobs" :> "problem" :> Capture "problem" Problem.Id :> Get '[JSON] [Job]
   :<|> "jobs" :> "requeue" :> Capture "job" Job.Id :> Post '[JSON] Job
   :<|> "jobs" :> "kick" :> Capture "problem" Problem.Id :> Capture "team" Team.Id :> Post '[JSON] Job
   :<|> "jobs" :> "kick" :> Capture "problem" Problem.Id :> Capture "team" Team.Id :> Capture "user" User.GitHubId :> Post '[JSON] Job

api :: Proxy API
api = Proxy

server :: Job.ServerEnv env => ServerT API (RIO env)
server = gethWorkers
    :<|> getJobs :<|> getQueuingJobs :<|> getRunningJobs :<|> getTeamJobs :<|> getProblemJobs
    :<|> requeueJob
    :<|> postJobWithoutUser :<|> postJobWithUser
  where
    gethWorkers = do
      logInfo "[GET] /workers"
      map shrink <$> Worker.getAllConnected
    getJobs = do
      logInfo "[GET] /job"
      store <- askStore
      Map.elems <$> readTVarIO store
    getQueuingJobs = do
      store <- askStore
      filter (view #queuing) . Map.elems <$> readTVarIO store
    getRunningJobs = do
      store <- askStore
      filter (view #running) . Map.elems <$> readTVarIO store
    getTeamJobs tid = do
      store <- askStore
      filter (\job -> job ^. #team == tid) . Map.elems <$> readTVarIO store
    getProblemJobs pid = do
      store <- askStore
      filter (\job -> job ^. #problem == pid) . Map.elems <$> readTVarIO store
    requeueJob jid = do
      unwrapError =<< Job.enqueueJob jid
    postJobWithoutUser pid tid = postJob pid tid Nothing
    postJobWithUser pid tid = postJob pid tid . Just
    postJob pid tid uid =  unwrapError =<< Job.kickJob pid tid uid

unwrapError :: Either Job.Error Job -> RIO env Job
unwrapError = \case
  Right job ->
    pure job
  Left (Job.ProblemIsNotFound _) ->
    throwM err404
  Left (Job.TeamIsNotFound _) ->
    throwM err404
  Left (Job.UserIsNotFound _) ->
    throwM err404
  Left Job.WorkerIsNotExist ->
    throwM err500
  Left (Job.JobIsNotFound _) ->
    throwM err404

fetchJobs :: (MonadThrow m, MonadIO m) => String -> m [Job]
fetchJobs host = do
  resp <- W.asJSON =<< liftIO (W.get $ host ++ "/jobs")
  pure $ resp ^. W.responseBody

kickJob :: (MonadThrow m, MonadIO m) => String -> Problem.Id -> Team.Id -> Maybe User.GitHubId -> m Job
kickJob host pid tid uid = do
  resp <- W.asJSON =<< liftIO (W.post url ("" :: ByteString))
  pure $ resp ^. W.responseBody
  where
    url = List.intercalate "/" $ [host, show pid, Text.unpack (coerce tid)] ++ maybeToList (Text.unpack . coerce <$> uid)
