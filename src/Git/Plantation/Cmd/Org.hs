{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Org
  ( OrgCmdArg
  , GitHubTeamArg
  , actForGitHubTeam
  , createGitHubTeam
  ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Arg
import           Git.Plantation.Cmd.Env               (CmdEnv)
import           Git.Plantation.Data.Team
import           Git.Plantation.Env
import           GitHub.Data.Name                     (mkName)
import qualified GitHub.Endpoints.Organizations.Teams as GitHub
import qualified Mix.Plugin.GitHub                    as MixGitHub
import qualified Mix.Plugin.Logger.JSON               as Mix

type OrgCmdArg = Record
  '[ "team"     >: TeamId
   , "gh_teams" >: [GitHubTeamName]
   ]

type GitHubTeamArg = Record
  '[ "team"    >: Team
   , "org"     >: Text
   , "gh_team" >: Text
   ]

actForGitHubTeam :: CmdEnv env => (GitHubTeamArg -> RIO env ()) -> OrgCmdArg -> RIO env ()
actForGitHubTeam act args =
  findByIdWith (view #teams) (args ^. #team) >>= \case
    Nothing   -> Mix.logErrorR "not found by config" (toArgInfo $ args ^. #team)
    Just team -> do
      (org, ghTeams) <- findGitHubTeams (args ^. #gh_teams) team
      mapM_ act $ hsequence $ #team <@=> [team] <: #org <@=> org <: #gh_team <@=> ghTeams <: nil

findGitHubTeams :: CmdEnv env => [GitHubTeamName] -> Team -> RIO env ([Text], [Text])
findGitHubTeams ids team = case (team ^. #org, ids) of
  (Nothing, _)   -> logError "Undefined GitHub org on team config." >> pure ([],[])
  (Just org, []) -> pure ([org], team ^. #gh_teams)
  (Just org, _)  -> ([org],) <$> findGitHubTeams'
  where
    findGitHubTeams' = fmap catMaybes . forM ids $ \idx ->
      case findById idx (team ^. #gh_teams) of
        Nothing -> Mix.logErrorR "not found by config" (toArgInfo idx) >> pure Nothing
        Just t  -> pure (Just t)

createGitHubTeam :: CmdEnv env => GitHubTeamArg -> RIO env ()
createGitHubTeam args = do
  resp <- MixGitHub.fetch $ GitHub.createTeamForR
    (mkName Proxy $ args ^. #org)
    (GitHub.CreateTeam (mkName Proxy $ args ^. #gh_team) Nothing mempty GitHub.PrivacyClosed GitHub.PermissionPush)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (failure err)
    Right _  -> logInfo $ display success
  where
    failure err = CreateGitHubTeamError err (args ^. #team) (args ^. #gh_team)
    success = mconcat
      [ "Success: create GitHub team: ", args ^. #org, ":", args ^. #gh_team ]
