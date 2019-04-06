{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Member where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Repo              (repoGithub,
                                                       splitRepoName)
import           Git.Plantation.Data
import           Git.Plantation.Env
import           GitHub.Data.Name                     (mkName)
import           GitHub.Endpoints.Repos               (Auth (..))
import qualified GitHub.Endpoints.Repos.Collaborators as GitHub

type InviteMemberCmd = Record MemeberCmdFields

type KickMemberCmd = Record MemeberCmdFields

type MemeberCmdFields =
  '[ "team"  >: Text
   , "repos" >: [Int]
   , "user"  >: Maybe Text
   ]

actForMember :: (User -> Repo -> Plant ()) -> Record MemeberCmdFields -> Team -> Plant ()
actForMember act args team = case args ^. #repos of
  [] -> forM_ (team ^. #repos) $ \repo -> forM_ member $ act' repo
  _  -> forM_ repos            $ \repo -> forM_ member $ act' repo
  where
    act' repo user = tryAnyWithLogError $ act user repo
    repos  = catMaybes $ flip lookupRepoByProblemId team <$> args ^. #repos
    member = maybe (team ^. #member) (: []) $ flip lookupUser team =<< args ^. #user

inviteUserToRepo :: User -> Repo -> Plant ()
inviteUserToRepo user target = do
  token  <- asks (view #token)
  github <- repoGithub target
  let (owner, repo) = splitRepoName github
  resp <- liftIO $ GitHub.addCollaborator
    (OAuth token)
    (mkName Proxy owner)
    (mkName Proxy repo)
    (mkName Proxy $ user ^. #github)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (InviteUserError err user target)
    Right _  -> logInfo $ display (success github)
  where
    success githubPath = mconcat
      [ "Success: invite "
      , user ^. #name, "(", user ^. #github, ")"
      , " to ", githubPath, "."
      ]

kickUserFromRepo :: User -> Repo -> Plant ()
kickUserFromRepo user target = do
  token  <- asks (view #token)
  github <- repoGithub target
  let (owner, repo) = splitRepoName github
  resp <- liftIO $ GitHub.removeCollaborator
    (OAuth token)
    (mkName Proxy owner)
    (mkName Proxy repo)
    (mkName Proxy $ user ^. #github)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (KickUserError err user target)
    Right _  -> logInfo $ display (success github)
  where
    success githubPath = mconcat
      [ "Success: kick "
      , user ^. #name, "(", user ^. #github, ")"
      , " from ", githubPath, "."
      ]
