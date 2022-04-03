{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Git.Plantation.API.Job where

import           RIO
import qualified RIO.Map                     as Map
import qualified RIO.Text                    as Text

import           Data.Coerce                 (coerce)
import           Data.Extensible
import           Git.Plantation.Data.Job     (Job)
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
   :<|> "jobs" :> Capture "problem" Problem.Id :> Capture "team" Team.Id :> Capture "user" User.GitHubId :> Post '[JSON] Job

api :: Proxy API
api = Proxy

server :: Job.ServerEnv env => ServerT API (RIO env)
server = gethWorkers :<|> getJobs :<|> postJob
  where
    gethWorkers = do
      logInfo "[GET] /workers"
      map shrink <$> Worker.getAllConnected
    getJobs = do
      logInfo "[GET] /job"
      store <- askStore
      Map.elems <$> readTVarIO store
    postJob pid tid uid = do
      result <- Job.kickJob pid tid uid
      case result of
        Right job ->
          pure job
        Left (Job.ProblemIsNotFount _) ->
          throwM err404
        Left (Job.TeamIsNotFount _) ->
          throwM err404
        Left (Job.UserIsNotFound _) ->
          throwM err404
        Left Job.WorkerIsNotExist ->
          throwM err500

fetchJobs :: (MonadThrow m, MonadIO m) => String -> m [Job]
fetchJobs host = do
  resp <- W.asJSON =<< liftIO (W.get $ host ++ "/jobs")
  pure $ resp ^. W.responseBody

kickJob :: (MonadThrow m, MonadIO m) => String -> Problem.Id -> Team.Id -> User.GitHubId -> m Job
kickJob host pid tid uid = do
  resp <- W.asJSON =<< liftIO (W.post url ("" :: ByteString))
  pure $ resp ^. W.responseBody
  where
    url = concat [host, "/", show pid, "/", Text.unpack (coerce tid), "/", Text.unpack (coerce uid)]
