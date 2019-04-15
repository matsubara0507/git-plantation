{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Member where

import           RIO

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
actForMember act args = do
  config <- asks $ view #config
  mapM_ act $ do
    team <- maybeToList $ findById (args ^. #team) (config ^. #teams)
    repo <- filterByIdWithDefault (team ^. #repos) (args ^. #repos)
    user <- filterByIdWithDefault (team ^. #member) (maybeToList $ args ^. #user)
    pure $ #user @= user <: #repo @= repo <: nil

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
