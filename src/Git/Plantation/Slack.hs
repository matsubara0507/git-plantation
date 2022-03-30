{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Git.Plantation.Slack where

import           RIO

import           Data.Extensible
import           Git.Plantation.API.Slack  (SlackAPI, slackAPI)
import           Git.Plantation.Config
import qualified Git.Plantation.Data.Slack as Slack
import           Git.Plantation.Env        hiding (Env)
import qualified Mix.Plugin.GitHub         as GitHub
import           Servant

type Env = Record
  '[ "config"  >: Config
   , "github"  >: GitHub.Token
   , "slack"   >: Slack.Config
   , "work"    >: FilePath
   , "webhook" >: WebhookConfig
   , "logger"  >: LogFunc
   ]

type API = SlackAPI

api :: Proxy API
api = Proxy

server :: ServerT API (RIO Env)
server = slackAPI
