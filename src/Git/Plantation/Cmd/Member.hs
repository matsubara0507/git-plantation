{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Member
  ( MemberCmdArg
  , MemberArg
  , actForMember
  , inviteUserToRepo
  , kickUserFromRepo
  , actForMemberWithOrg
  , inviteUserToGitHubOrg
  , kickUserFromGitHubOrg
  , actForMemberWithGitHubTeam
  , inviteUserToGitHubOrgTeam
  , kickUserFromGitHubOrgTeam
  ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Arg
import           Git.Plantation.Cmd.Env                 (CmdEnv)
import           Git.Plantation.Cmd.Repo                (repoGithub,
                                                         splitRepoName)
import           Git.Plantation.Data
import           Git.Plantation.Env
import           GitHub.Data.Name                       (mkName)
import qualified GitHub.Endpoints.Organizations.Members as GitHub
import qualified GitHub.Endpoints.Organizations.Teams   as GitHub
import qualified GitHub.Endpoints.Repos.Collaborators   as GitHub
import qualified Mix.Plugin.GitHub                      as MixGitHub
import qualified Mix.Plugin.Logger.JSON                 as Mix

type MemberCmdArg = Record
  '[ "team"    >: TeamId
   , "repos"   >: [RepoId]
   , "user"    >: Maybe UserId
   , "org"     >: Bool
   , "gh_team" >: Maybe Text
   ]

type MemberArg = Record
  '[ "user" >: User
   , "repo" >: Repo
   ]

type MemberWithOrgArg = Record
  '[ "user" >: User
   , "org"  >: Text
   ]

type MemberWithGitHubTeamArg = Record
  '[ "user"    >: User
   , "org"  >: Text
   , "gh_team" >: Text
   ]

actForMember :: CmdEnv env => (MemberArg -> RIO env ()) -> MemberCmdArg -> RIO env ()
actForMember act args =
  findByIdWith (view #teams) (args ^. #team) >>= \case
    Nothing   -> Mix.logErrorR "not found by config" (toArgInfo $ args ^. #team)
    Just team -> do
      member <- findMember (args ^. #user) team
      repos  <- findRepos (args ^. #repos) team
      mapM_ act $ hsequence $ #user <@=> member <: #repo <@=> repos <: nil

actForMemberWithOrg ::
  CmdEnv env => (MemberWithOrgArg -> RIO env ()) -> MemberCmdArg -> RIO env ()
actForMemberWithOrg act args =
  findByIdWith (view #teams) (args ^. #team) >>= \case
    Nothing   -> Mix.logErrorR "not found by config" (toArgInfo $ args ^. #team)
    Just team -> do
      member <- findMember (args ^. #user) team
      ghOrg  <- findGitHubOrg team
      mapM_ act $ hsequence $ #user <@=> member <: #org <@=> ghOrg <: nil

actForMemberWithGitHubTeam ::
  CmdEnv env => (MemberWithGitHubTeamArg -> RIO env ()) -> MemberCmdArg -> RIO env ()
actForMemberWithGitHubTeam act args =
  findByIdWith (view #teams) (args ^. #team) >>= \case
    Nothing   -> Mix.logErrorR "not found by config" (toArgInfo $ args ^. #team)
    Just team -> do
      member <- findMember (args ^. #user) team
      ghOrg  <- findGitHubOrg team
      ghTeam <- findGitHubTeam (args ^. #gh_team) team
      mapM_ act $ hsequence $ #user <@=> member <: #org <@=> ghOrg <: #gh_team <@=> ghTeam <: nil

findMember :: CmdEnv env => Maybe UserId -> Team -> RIO env [User]
findMember Nothing team = pure $ team ^. #member
findMember (Just idx) team =
  case findById idx (team ^. #member) of
    Nothing   -> Mix.logErrorR "not found by config" (toArgInfo idx) >> pure []
    Just user -> pure [user]

findRepos :: CmdEnv env => [RepoId] -> Team -> RIO env [Repo]
findRepos [] team = pure $ team ^. #repos
findRepos ids team = fmap catMaybes . forM ids $ \idx ->
  case findById idx (team ^. #repos) of
    Nothing -> Mix.logErrorR "not found by config" (toArgInfo idx) >> pure Nothing
    Just r  -> pure (Just r)

findGitHubOrg :: CmdEnv env => Team -> RIO env [Text]
findGitHubOrg team = case team ^. #org of
  Nothing  -> Mix.logErrorR "undefined GitHub org in config" nil >> pure []
  Just org -> pure [org]

findGitHubTeam :: CmdEnv env => Maybe Text -> Team -> RIO env [Text]
findGitHubTeam Nothing _ = pure []
findGitHubTeam (Just name) team
  | name `elem` team ^. #gh_teams = pure [name]
  | otherwise = Mix.logErrorR "not found by config" (#gh_team @= name <: nil) >> pure []

inviteUserToRepo :: CmdEnv env => MemberArg -> RIO env ()
inviteUserToRepo args = do
  github <- repoGithub $ args ^. #repo
  let (owner, repo) = splitRepoName github
  resp <- MixGitHub.fetch $ GitHub.addCollaboratorR
    (mkName Proxy owner)
    (mkName Proxy repo)
    (mkName Proxy $ args ^. #user ^. #github)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (failure err)
    Right _  -> logInfo $ display (success github)
  where
    failure err = InviteUserError err (args ^. #user) (TargetRepo $ args ^. #repo)
    success githubPath = mconcat
      [ "Success: invite "
      , args ^. #user ^. #name, "(", args ^. #user ^. #github, ")"
      , " to ", githubPath, "."
      ]

kickUserFromRepo :: CmdEnv env => MemberArg -> RIO env ()
kickUserFromRepo args = do
  github <- repoGithub $ args ^. #repo
  let (owner, repo) = splitRepoName github
  resp <- MixGitHub.fetch $ GitHub.removeCollaboratorR
    (mkName Proxy owner)
    (mkName Proxy repo)
    (mkName Proxy $ args ^. #user ^. #github)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (failure err)
    Right _  -> logInfo $ display (success github)
  where
    failure err = KickUserError err (args ^. #user) (TargetRepo $ args ^. #repo)
    success githubPath = mconcat
      [ "Success: kick "
      , args ^. #user ^. #name, "(", args ^. #user ^. #github, ")"
      , " from ", githubPath, "."
      ]

inviteUserToGitHubOrg :: CmdEnv env => MemberWithOrgArg -> RIO env ()
inviteUserToGitHubOrg args = do
  resp <- MixGitHub.fetch $ GitHub.addOrUpdateMembershipR
    (mkName Proxy $ args ^. #org)
    (mkName Proxy $ args ^. #user ^. #github)
    False
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (failure err)
    Right _  -> logInfo $ display success
  where
    failure err = InviteUserError err (args ^. #user) (TargetOrg $ args ^. #org)
    success = mconcat
      [ "Success: invite "
      , args ^. #user ^. #name, "(", args ^. #user ^. #github, ")"
      , " to ", args ^. #org, "."
      ]

kickUserFromGitHubOrg :: CmdEnv env => MemberWithOrgArg -> RIO env ()
kickUserFromGitHubOrg args = do
  resp <- MixGitHub.fetch $ GitHub.removeMembershipR
    (mkName Proxy $ args ^. #org)
    (mkName Proxy $ args ^. #user ^. #github)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (failure err)
    Right _  -> logInfo $ display success
  where
    failure err = KickUserError err (args ^. #user) (TargetOrg $ args ^. #org)
    success = mconcat
      [ "Success: kick "
      , args ^. #user ^. #name, "(", args ^. #user ^. #github, ")"
      , " from ", args ^. #org, "."
      ]

inviteUserToGitHubOrgTeam :: CmdEnv env => MemberWithGitHubTeamArg -> RIO env ()
inviteUserToGitHubOrgTeam args = do
  resp <- MixGitHub.fetch $ GitHub.teamInfoByNameR
    (mkName Proxy $ args ^. #org)
    (mkName Proxy $ args ^. #gh_team)
  team <- case resp of
    Left err   -> logDebug (displayShow err) >> throwIO (failure err)
    Right team -> pure team
  resp' <- MixGitHub.fetch $ GitHub.addTeamMembershipForR
    (GitHub.teamId team)
    (mkName Proxy $ args ^. #user ^. #github)
    GitHub.RoleMember
  case resp' of
    Left err -> logDebug (displayShow err) >> throwIO (failure err)
    Right _  -> logInfo $ display success
  where
    failure err = InviteUserError err (args ^. #user) (TargetTeam (args ^. #org) $ args ^. #gh_team)
    success = mconcat
      [ "Success: invite "
      , args ^. #user ^. #name, "(", args ^. #user ^. #github, ")"
      , " to ", args ^. #org, ":", args ^. #gh_team, "."
      ]

kickUserFromGitHubOrgTeam :: CmdEnv env => MemberWithGitHubTeamArg -> RIO env ()
kickUserFromGitHubOrgTeam args = do
  resp <- MixGitHub.fetch $ GitHub.teamInfoByNameR
    (mkName Proxy $ args ^. #org)
    (mkName Proxy $ args ^. #gh_team)
  team <- case resp of
    Left err   -> logDebug (displayShow err) >> throwIO (failure err)
    Right team -> pure team
  resp' <- MixGitHub.fetch $ GitHub.deleteTeamMembershipForR
    (GitHub.teamId team)
    (mkName Proxy $ args ^. #user ^. #github)
  case resp' of
    Left err -> logDebug (displayShow err) >> throwIO (failure err)
    Right _  -> logInfo $ display success
  where
    failure err = KickUserError err (args ^. #user) (TargetTeam (args ^. #org) $ args ^. #gh_team)
    success = mconcat
      [ "Success: kick "
      , args ^. #user ^. #name, "(", args ^. #user ^. #github, ")"
      , " from ", args ^. #org, ":", args ^. #gh_team, "."
      ]
