{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Member where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Repo              (splitRepoName)
import           Git.Plantation.Data
import           Git.Plantation.Env
import           GitHub.Data.Name                     (mkName)
import           GitHub.Endpoints.Repos               (Auth (..))
import qualified GitHub.Endpoints.Repos.Collaborators as GitHub

type InviteMemberCmd = Record
  '[ "team" >: Text
   , "repo" >: Maybe Text
   , "user" >: Maybe Text
   ]

inviteMember :: InviteMemberCmd -> Team -> Plant ()
inviteMember args team =
  forM_ repos $ \repo -> forM_ member $ \user ->
    tryAnyWithLogError $ inviteUserToRepo user repo
  where
    repos  = maybe (team ^. #repos)  (: []) $ flip lookupRepo' team =<< args ^. #repo
    member = maybe (team ^. #member) (: []) $ flip lookupUser team =<< args ^. #user

inviteUserToRepo :: User -> Repo -> Plant ()
inviteUserToRepo user target = do
  token <- asks (view #token)
  resp <- liftIO $ GitHub.addCollaborator
    (OAuth token)
    (mkName Proxy owner)
    (mkName Proxy repo)
    (mkName Proxy $ user ^. #github)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (InviteUserError err user target)
    Right _  -> logInfo $ display success
  where
    (owner, repo) = splitRepoName $ target ^. #github
    success = mconcat
      [ "Success: invite "
      , user ^. #name, "(", user ^. #github, ")"
      , " to ", target ^. #github, "."
      ]
