{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.CRUD where

import           RIO
import qualified RIO.List                 as L

import qualified Git.Plantation.API.Job   as Job
import           Git.Plantation.Data      (Problem, Team)
import qualified Git.Plantation.Data.Team as Team
import qualified Git.Plantation.Data.User as User
import           Git.Plantation.Env       (Plant)
import           Git.Plantation.Score     (Score, mkPlayerScore, mkScore)
import           Servant

type GetAPI
     = "teams"    :> Get '[JSON] [Team]
  :<|> "problems" :> Get '[JSON] [Problem]
  :<|> "scores"   :> Get '[JSON] [Score]
  :<|> "scores"   :> Capture "team" Team.Id :> Get '[JSON] [Score]
  :<|> "scores"   :> Capture "team" Team.Id :> Capture "player" User.GitHubId  :> Get '[JSON] [Score]

getAPI :: ServerT GetAPI Plant
getAPI = getTeams
   :<|> getProblems
   :<|> getScores
   :<|> getTeamScore
   :<|> getPlayerScore

getTeams :: Plant [Team]
getTeams = do
  logInfo "[GET] /teams"
  asks (view #teams . view #config)

getProblems :: Plant [Problem]
getProblems = do
  logInfo "[GET] /problems"
  asks (view #problems . view #config)

getScores :: Plant [Score]
getScores = do
  logInfo "[GET] /scores"
  host <- asks (view #jobserver)
  jobs <- tryAny (Job.fetchJobs host) >>= \case
    Left err -> logError (displayShow err) >> pure mempty
    Right x  -> pure x
  config <- asks (view #config)
  pure $ map (flip (mkScore $ config ^. #problems) jobs) (config ^. #teams)

getTeamScore :: Team.Id -> Plant [Score]
getTeamScore teamID = do
  logInfo $ "[GET] /scores/" <> display teamID
  host <- asks (view #jobserver)
  jobs <- tryAny (Job.fetchJobs host) >>= \case
    Left err -> logError (displayShow err) >> pure mempty
    Right x  -> pure x
  config <- asks (view #config)
  teams <- case L.find (\team -> team ^. #id == teamID) (config ^. #teams) of
    Nothing   -> logError "team not found." >> pure mempty
    Just team -> pure [team]
  pure $ map (flip (mkScore $ config ^. #problems) jobs) teams

getPlayerScore :: Team.Id -> User.GitHubId -> Plant [Score]
getPlayerScore teamID userID = do
  logInfo $ "[GET] /scores/" <> display teamID <> "/" <> display userID
  host <- asks (view #jobserver)
  jobs <- tryAny (Job.fetchJobs host) >>= \case
    Left err -> logError (displayShow err) >> pure mempty
    Right x  -> pure x
  config <- asks (view #config)
  teams <- case L.find (\team -> team ^. #id == teamID) (config ^. #teams) of
    Nothing   -> logError "team not found." >> pure mempty
    Just team -> pure [team]
  pure . catMaybes $
    liftA2 fmap (\t u -> mkPlayerScore (config ^. #problems) t u jobs) (Team.lookupUser userID) <$> teams
