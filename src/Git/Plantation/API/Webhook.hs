{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.Webhook where

import           RIO
import qualified RIO.List                     as L

import           Git.Plantation.API.CRUD      (updateScore)
import           Git.Plantation.Cmd.Repo
import           Git.Plantation.Data          (Problem, Team)
import qualified Git.Plantation.Data.Slack    as Slack
import qualified Git.Plantation.Data.Team     as Team
import           Git.Plantation.Env           (Plant)
import           GitHub.Data.Webhooks.Events
import           GitHub.Data.Webhooks.Payload
import           Servant
import           Servant.GitHub.Webhook
import           UnliftIO.Concurrent          (forkIO)

type WebhookAPI
     = GitHubEvent '[ 'WebhookPingEvent ] :> GitHubSignedReqBody '[JSON] PublicEvent :> Post '[JSON] NoContent
  :<|> GitHubEvent '[ 'WebhookPushEvent ] :> GitHubSignedReqBody '[JSON] PushEvent :> Post '[JSON] NoContent

webhook :: ServerT WebhookAPI Plant
webhook =
  pingWebhook :<|> pushWebhook

pingWebhook :: RepoWebhookEvent -> ((), PublicEvent) -> Plant NoContent
pingWebhook _ (_, ev) = do
  logInfo $ "Hook Ping Event: " <> displayShow ev
  pure NoContent

pushWebhook :: RepoWebhookEvent -> ((), PushEvent) -> Plant NoContent
pushWebhook _ (_, ev) = do
  logInfo $ "Hook Push Event: " <> displayShow ev
  _ <- forkIO $ do
    config <- asks (view #config)
    case findByPushEvent ev (config ^. #teams) (config ^. #problems) of
      Just (team, problem) -> startScoring team problem
      Nothing              -> logError "team or problem is not found."
  pure NoContent
  where
    startScoring team problem = do
      notifySlack ev team problem
      pushForCI team problem
      _ <- updateScore (problem ^. #id)
      pure ()

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
