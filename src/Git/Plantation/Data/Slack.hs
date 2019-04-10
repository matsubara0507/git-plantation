{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Git.Plantation.Data.Slack where

import           RIO

import           Data.Extensible

type Config = Record
  '[ "token"      >: Text
   , "user_ids"   >: [Text]
   ]

type OutgoingWebhookData = Record
  '[ "token"        >: Text
   , "team_id"      >: Text
   , "team_domain"  >: Text
   , "channel_id"   >: Text
   , "channel_name" >: Text
   , "timestamp"    >: Text
   , "user_id"      >: Text
   , "user_name"    >: Text
   , "text"         >: Text
   , "trigger_word" >: Text
   ]

type Message = Record '[ "text" >: Text ]
