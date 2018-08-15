{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Git.Plantation.API.Webhook where

import           RIO

import           Git.Plantation.Env          (Plant)
import           GitHub.Data.Webhooks.Events
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
