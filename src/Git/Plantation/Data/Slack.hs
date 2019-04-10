{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Git.Plantation.Data.Slack where

import           RIO

import           Data.Extensible

type Config = Record
  '[ "token"          >: Text
   , "team_id"        >: Text
   , "channel_ids"    >: [Text]
   , "user_ids"       >: [Text]
   , "reset_repo_cmd" >: Text
   ]

type SlashCmdData = Record
  '[ "token"        >: Text
   , "team_id"      >: Text
   , "team_domain"  >: Text
   , "channel_id"   >: Text
   , "channel_name" >: Text
   , "user_id"      >: Text
   , "user_name"    >: Text
   , "text"         >: Text
   , "command"      >: Text
   , "response_url" >: Text
   ]

type Message = Record '[ "text" >: Text ]

type DisplayLogData = Record
  '[ "team_domain"  >: Text
   , "channel_name" >: Text
   , "user_name"    >: Text
   , "text"         >: Text
   , "command"      >: Text
   ]
