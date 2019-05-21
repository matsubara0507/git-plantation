{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.CRUD where

import           RIO
import qualified RIO.List             as L
import qualified RIO.Map              as Map

import qualified Drone.Endpoints      as Drone
import qualified Drone.Types          as Drone
import           Git.Plantation       (Problem, Team)
import           Git.Plantation.Cmd   (splitRepoName)
import           Git.Plantation.Env   (Plant)
import           Git.Plantation.Score (ScoreR, Scores)
import qualified Git.Plantation.Score as Score
import qualified Mix.Plugin.Drone     as MixDrone
import           Network.HTTP.Req
import           Network.HTTP.Req     as Req
import           Servant

type CRUD
      = GetAPI
   :<|> "scores" :> Capture "problem" Int :> Capture "build" Int :> Put '[JSON] ()

type GetAPI
     = "teams"    :> Get '[JSON] [Team]
  :<|> "problems" :> Get '[JSON] [Problem]
  :<|> "scores"   :> Get '[JSON] [ScoreR]


crud :: TVar Scores -> ServerT CRUD Plant
crud scores = getAPI :<|> putScore scores
  where
    getAPI = getTeams :<|> getProblems :<|> getScores scores

getTeams :: Plant [Team]
getTeams = do
  logInfo "[GET] /teams"
  asks (view #teams . view #config)

getProblems :: Plant [Problem]
getProblems = do
  logInfo "[GET] /problems"
  asks (view #problems . view #config)

getScores :: TVar Scores -> Plant [ScoreR]
getScores scores = do
  logInfo "[GET] /scores"
  liftIO $ map Score.toResponse . Map.elems <$> atomically (readTVar scores)

putScore :: TVar Scores -> Int -> Int -> Plant ()
putScore scores pid bid = do
  config <- asks (view #config)
  case L.find (\p -> p ^. #id == pid) (config ^. #problems) of
    Nothing -> logWarn $ fromString ("not found problem id: " <> show pid)
    Just p  -> updateScoresWithBuild config (splitRepoName $ p ^. #repo)
  where
    updateScoresWithBuild conf (owner, repo) = do
      tryAny (MixDrone.fetch $ \c -> Drone.getBuild c owner repo bid) >>= \case
        Left err   -> logError (display err)
        Right resp -> liftIO $ atomically (modifyTVar scores $ update conf resp)
    update conf = Score.updateStatWithBuild conf pid . Req.responseBody
