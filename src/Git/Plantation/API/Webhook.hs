{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.Webhook where

import           RIO
import qualified RIO.List                     as L

import           Git.Plantation.Cmd.Repo
import           Git.Plantation.Env           (Plant)
import           Git.Plantation.Problem       (Problem)
import           Git.Plantation.Team          (Team)
import           GitHub.Data.Webhooks.Events
import           GitHub.Data.Webhooks.Payload
import           Servant
import           Servant.GitHub.Webhook

type WebhookAPI
     = GitHubEvent '[ 'WebhookPingEvent ] :> GitHubSignedReqBody '[JSON] PublicEvent :> Post '[JSON] ()
  :<|> GitHubEvent '[ 'WebhookPushEvent ] :> GitHubSignedReqBody '[JSON] PushEvent :> Post '[JSON] ()

webhook :: ServerT WebhookAPI Plant
webhook =
  pingWebhook :<|> pushWebhook

pingWebhook :: RepoWebhookEvent -> ((), PublicEvent) -> Plant ()
pingWebhook _ (_, ev) = do
  logInfo $ "Hook Ping Event: " <> displayShow ev

pushWebhook :: RepoWebhookEvent -> ((), PushEvent) -> Plant ()
pushWebhook _ (_, ev) = do
  logInfo $ "Hook Push Event: " <> displayShow ev
  config <- asks (view #config)
  let team = findTeamByPushEvent ev $ config ^. #teams
  logInfo $ "Team: " <> displayShow team
  let problem = findProblemByPushEvent ev $ config ^. #problems
  logInfo $ "Problem: " <> displayShow problem
  case (team, problem) of
    (Just t, Just p) -> pushForCI t p
    _                -> logError "Team or Problem not found."

findTeamByPushEvent :: PushEvent -> [Team] -> Maybe Team
findTeamByPushEvent ev ts = do
  owner <- whOrgLogin <$> evPushOrganization ev
  L.find (\t -> t ^. #github == owner) ts

findProblemByPushEvent :: PushEvent -> [Problem] -> Maybe Problem
findProblemByPushEvent ev =
  L.find $ \p -> let (_, repo') = splitRepoName (p ^. #repo_name) in repo' == repo
  where
    repo = whRepoName $ evPushRepository ev
