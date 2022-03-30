{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Git.Plantation.Data.Slack where

import           RIO
import qualified RIO.Text        as Text

import qualified Data.Aeson      as J
import           Data.Extensible
import qualified Network.Wreq    as W

type Config = Record
  '[ "token"          >: Text
   , "team_id"        >: Text
   , "channel_ids"    >: [Text]
   , "user_ids"       >: [Text]
   , "reset_repo_cmd" >: Text
   , "webhook"        >: Maybe Text
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

class HasSlackConfig env where
  slackConfigL :: Lens' env Config

instance Lookup xs "slack" Config => HasSlackConfig (Record xs) where
  slackConfigL = lens (view #slack) (\x y -> x & #slack `set` y)

askSlackConfig :: HasSlackConfig env => RIO env Config
askSlackConfig = view slackConfigL

mkMessage :: Text -> Message
mkMessage txt = #text @= txt <: nil

respondMessage :: MonadIO m => SlashCmdData -> Message -> m ()
respondMessage postData message = do
  let url = Text.unpack $ postData ^. #response_url
  _ <- liftIO $ W.post url (J.toJSON message)
  pure ()

sendWebhook :: MonadIO m => Text -> Message -> m ()
sendWebhook url msg =
  liftIO (W.post (Text.unpack url) $ J.toJSON msg) >> pure ()
