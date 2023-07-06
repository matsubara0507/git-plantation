{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.CRUD where

import           RIO
import qualified RIO.List                    as L

import           Data.Coerce                 (coerce)
import           Data.Extensible
import qualified Git.Plantation.API.Job      as Job
import qualified Git.Plantation.Cmd          as Cmd
import           Git.Plantation.Config       (Config)
import           Git.Plantation.Data         (Problem, Repo, Team)
import qualified Git.Plantation.Data.Problem as Problem
import qualified Git.Plantation.Data.Slack   as Slack
import qualified Git.Plantation.Data.Team    as Team
import qualified Git.Plantation.Data.User    as User
import           Git.Plantation.Env          (Plant)
import           Git.Plantation.Score        (Score, mkPlayerScore, mkScore)
import qualified Mix.Plugin.Config           as Mix
import           Servant
import           UnliftIO.Concurrent         (forkIO)

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

type ResetAPI
     = "reset" :> Capture "team" Team.Id :> Capture "probrem" Problem.Id :> Post '[JSON] ()

resetAPI :: User.GitHubId -> ServerT ResetAPI Plant
resetAPI = resetRepo

resetRepo :: User.GitHubId -> Team.Id -> Problem.Id -> Plant ()
resetRepo userID teamID problemID = do
  logInfo $ "[POST] /reset/" <> display teamID <> "/" <> display problemID <> " from " <> display userID
  config <- Mix.askConfig
  if (fromMaybe True $ config ^. #scoreboard ^. #resetable) then
    case findInfos config (teamID, problemID) of
      Nothing ->
        logError "team and problem not found."
      Just (team, problem, repo) ->
        if isNothing (Team.lookupUser userID team) then
          logError "user not found."
        else do
          Slack.sendMessage (team ^. #channel_id) $ (coerce $ team ^. #name) <> "の" <> (coerce $ problem ^. #name)  <> "をリセットするね！"
          forkIO ((logError . display) `handleIO` reset team problem repo) >> pure ()
  else
    logInfo "cannot reset"
  pure ()
  where
    reset :: Team -> Problem -> Repo -> Plant ()
    reset team problem repo =
      tryIO (Cmd.resetRepo $ #repo @= repo <: #team @= team <: nil) >>= \case
        Left err -> logError (display err)
        Right _  -> Slack.sendMessage (team ^. #channel_id) $ (coerce $ team ^. #name) <> " の " <> (coerce $ problem ^. #name)  <> " をリセットした！"

findInfos :: Config -> (Team.Id, Problem.Id) -> Maybe (Team, Problem, Repo)
findInfos config (teamID, problemID) = do
  team <- L.find (\t -> t ^. #id == teamID) $ config ^. #teams
  problem <- L.find (\p -> p ^. #id == problemID) $ config ^. #problems
  repo <- Team.lookupRepo problem team
  pure (team, problem, repo)
