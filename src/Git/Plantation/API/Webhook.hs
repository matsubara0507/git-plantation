{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.Webhook where

import           RIO
import qualified RIO.List                     as L

import           Git.Plantation.Cmd.Repo
import           Git.Plantation.Data          (Problem, Team)
import qualified Git.Plantation.Data.Slack    as Slack
import qualified Git.Plantation.Data.Team     as Team
import           Git.Plantation.Env           (Plant)
import           Git.Plantation.Score         (Scores, toPendingScore)
import           GitHub.Data.Webhooks.Events
import           GitHub.Data.Webhooks.Payload
import           Servant
import           Servant.GitHub.Webhook

type WebhookAPI
     = GitHubEvent '[ 'WebhookPingEvent ] :> GitHubSignedReqBody '[JSON] PublicEvent :> Post '[JSON] ()
  :<|> GitHubEvent '[ 'WebhookPushEvent ] :> GitHubSignedReqBody '[JSON] PushEvent :> Post '[JSON] ()

webhook :: TVar Scores -> ServerT WebhookAPI Plant
webhook scores =
  pingWebhook :<|> pushWebhook scores

pingWebhook :: RepoWebhookEvent -> ((), PublicEvent) -> Plant ()
pingWebhook _ (_, ev) =
  logInfo $ "Hook Ping Event: " <> displayShow ev

pushWebhook :: TVar Scores -> RepoWebhookEvent -> ((), PushEvent) -> Plant ()
pushWebhook scores _ (_, ev) = do
  logInfo $ "Hook Push Event: " <> displayShow ev
  config <- asks (view #config)
  case findByPushEvent ev (config ^. #teams) (config ^. #problems) of
    Just (team, problem) -> startScoring config team problem
    Nothing              -> logError "team or problem is not found."
  where
    startScoring config team problem = do
      notifySlack ev team problem
      liftIO $ atomically (modifyTVar scores $ toPendingScore config team problem)
      pushForCI team problem

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

notifySlack :: PushEvent -> Team -> Problem -> Plant ()
notifySlack ev team problem = do
  conf <- (view #webhook =<<) <$> asks (view #slack)
  case (conf, Team.lookupUser sender team) of
    (Just url, Just user) -> Slack.sendWebhook url (mkMessage user)
    (Nothing, _)          -> logWarn "webhook url is not found when notify slack"
    (_, Nothing)          -> logWarn "sender is not found when notify slack"
  where
    sender = whUserLogin $ evPushSender ev
    mkMessage user = Slack.mkMessage $ mconcat
      [ team ^. #name, " の ", user ^. #name, " が ", problem ^. #name, " にプッシュしたみたい！" ]
