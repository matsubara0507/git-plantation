{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Member where

import           RIO

import           Data.Aeson.Text                      (encodeToLazyText)
import           Data.Extensible
import           Git.Plantation.Cmd.Arg
import           Git.Plantation.Cmd.Repo              (repoGithub,
                                                       splitRepoName)
import           Git.Plantation.Data
import           Git.Plantation.Env
import           GitHub.Data.Name                     (mkName)
import qualified GitHub.Endpoints.Repos.Collaborators as GitHub
import qualified Mix.Plugin.GitHub                    as MixGitHub

type MemberCmdArg = Record
  '[ "team"  >: TeamId
   , "repos" >: [RepoId]
   , "user"  >: Maybe UserId
   ]

type MemberArg = Record
  '[ "user" >: User
   , "repo" >: Repo
   ]

actForMember :: (MemberArg -> Plant ()) -> MemberCmdArg -> Plant ()
actForMember act args =
  findByIdWith (view #teams) (args ^. #team) >>= \case
    Nothing   -> logError $ display (encodeToLazyText $ errMsg $ args ^. #team)
    Just team -> do
      member <- findMember (args ^. #user) team
      repos  <- findRepos (args ^. #repos) team
      mapM_ act $ hsequence $ #user <@=> member <: #repo <@=> repos <: nil

findMember :: Maybe UserId -> Team -> Plant [User]
findMember Nothing team = pure $ team ^. #member
findMember (Just idx) team =
  case findById idx (team ^. #member) of
    Nothing   -> logError (display $ encodeToLazyText $ errMsg idx) >> pure []
    Just user -> pure [user]

findRepos :: [RepoId] -> Team -> Plant [Repo]
findRepos [] team = pure $ team ^. #repos
findRepos ids team = fmap catMaybes . forM ids $ \idx ->
  case findById idx (team ^. #repos) of
    Nothing -> logError (display $ encodeToLazyText $ errMsg idx) >> pure Nothing
    Just r  -> pure (Just r)

inviteUserToRepo :: MemberArg -> Plant ()
inviteUserToRepo args = do
  github <- repoGithub $ args ^. #repo
  let (owner, repo) = splitRepoName github
  resp <- MixGitHub.fetch $ \auth -> GitHub.addCollaborator auth
    (mkName Proxy owner)
    (mkName Proxy repo)
    (mkName Proxy $ args ^. #user ^. #github)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (failure err)
    Right _  -> logInfo $ display (success github)
  where
    failure err = InviteUserError err (args ^. #user) (args ^. #repo)
    success githubPath = mconcat
      [ "Success: invite "
      , args ^. #user ^. #name, "(", args ^. #user ^. #github, ")"
      , " to ", githubPath, "."
      ]

kickUserFromRepo :: MemberArg -> Plant ()
kickUserFromRepo args = do
  github <- repoGithub $ args ^. #repo
  let (owner, repo) = splitRepoName github
  resp <- MixGitHub.fetch $ \auth -> GitHub.removeCollaborator auth
    (mkName Proxy owner)
    (mkName Proxy repo)
    (mkName Proxy $ args ^. #user ^. #github)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (failure err)
    Right _  -> logInfo $ display (success github)
  where
    failure err = KickUserError err (args ^. #user) (args ^. #repo)
    success githubPath = mconcat
      [ "Success: kick "
      , args ^. #user ^. #name, "(", args ^. #user ^. #github, ")"
      , " from ", githubPath, "."
      ]
