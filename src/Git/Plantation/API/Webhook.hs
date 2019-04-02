{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.Webhook where

import           RIO
import qualified RIO.List                     as L

import           Git.Plantation.Cmd.Repo
import           Git.Plantation.Data          (Problem, Team)
import qualified Git.Plantation.Data.Team     as Team
import           Git.Plantation.Env           (Plant)
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
  case findByPushEvent ev (config ^. #teams) (config ^. #problems) of
    Just (team, problem) -> pushForCI team problem
    Nothing              -> logError "team or problem is not found."

findByPushEvent :: PushEvent -> [Team] -> [Problem] -> Maybe (Team, Problem)
findByPushEvent ev teams problems = do
  (team, repo) <- join $ L.find isJust repos
  problem      <- L.find (\p -> p ^. #id == repo ^. #problem) problems
  if evPushRef ev == "refs/heads/" <> problem ^. #answer_branch then
    pure (team, problem)
  else
    Nothing
  where
    repos    = map (\t -> (t,) <$> Team.lookupRepoByGithub repoName t) teams
    repoName = whRepoFullName $ evPushRepository ev
