{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Git.Plantation.API where

import           RIO

import           Git.Plantation.API.Webhook (WebhookAPI, webhook)
import           Git.Plantation.Env         (Plant)
import           Servant

type API
      = "hook" :> WebhookAPI

api :: Proxy API
api = Proxy

server :: ServerT API Plant
server = webhook
