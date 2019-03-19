{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
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

type InviteMemberCmd = Record
  '[ "team" >: Text
   , "repo" >: Maybe Int
   , "user" >: Maybe Text
   ]

inviteMember :: InviteMemberCmd -> Team -> Plant ()
inviteMember args team =
  forM_ repos $ \repo -> forM_ member $ \user ->
    tryAnyWithLogError $ inviteUserToRepo user repo
  where
    repos  = maybe (team ^. #repos)  (: []) $ flip lookupRepoByProblemId team =<< args ^. #repo
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
    (mkName Proxy github)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (InviteUserError err user target)
    Right _  -> logInfo $ display (success github)
  where
    success githubPath = mconcat
      [ "Success: invite "
      , user ^. #name, "(", user ^. #github, ")"
      , " to ", githubPath, "."
      ]
