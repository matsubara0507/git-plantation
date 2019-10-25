{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Slack where

import           RIO

import           Git.Plantation.API.Slack (SlackAPI, slackAPI)
import           Git.Plantation.Env       (Plant)
import           Servant

type API = SlackAPI

api :: Proxy API
api = Proxy

server :: ServerT API Plant
server = slackAPI
