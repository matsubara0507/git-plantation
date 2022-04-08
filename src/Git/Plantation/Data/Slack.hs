{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Git.Plantation.Data.Slack
  ( module Slack
  , SlashCmdConfig
  , HasSlackSlashCmdConfig
  , askSlashCmdConfig
  , SlashCmdData
  , DisplayLogData
  , verifySlashCmd
  , SlashCmd
  , Message
  , mkMessage
  , respondMessage
  , sendWebhook
  , NotifyConfig
  , HasSlackNotifyConfig
  , askNotifyConfig
  , SnipetMessage
  , uploadFile
  ) where

import           RIO
import qualified RIO.Text                               as Text

import           Control.Arrow                          ((+++))
import qualified Data.Aeson                             as J
import           Data.Extensible
import           Git.Plantation.Data.Slack.Verification as Slack
import qualified Network.HTTP.Media                     as M
import qualified Network.Wreq                           as W
import           Servant.API.ContentTypes               (Accept (..),
                                                         MimeUnrender (..))
import           Web.FormUrlEncoded                     (urlDecodeAsForm)

import           Orphans                                ()

type SlashCmdConfig = Record
  '[ "signing_secret" >: Slack.SigningSecret
   , "verify_token"   >: Text
   , "team_id"        >: Text
   , "channel_ids"    >: [Text]
   , "reset_repo_cmd" >: Text
   , "webhook"        >: Maybe Text
   ]

class HasSlackSlashCmdConfig env where
  slackSlashCmdConfigL :: Lens' env SlashCmdConfig

instance Lookup xs "slash" SlashCmdConfig => HasSlackSlashCmdConfig (Record xs) where
  slackSlashCmdConfigL = lens (view #slash) (\x y -> x & #slash `set` y)

askSlashCmdConfig :: HasSlackSlashCmdConfig env => RIO env SlashCmdConfig
askSlashCmdConfig = view slackSlashCmdConfigL

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

type DisplayLogData = Record
  '[ "team_domain"  >: Text
   , "channel_name" >: Text
   , "user_name"    >: Text
   , "text"         >: Text
   , "command"      >: Text
   ]

verifySlashCmd :: HasSlackSlashCmdConfig env => SlashCmdData -> RIO env (Either Text ())
verifySlashCmd dat = do
  config <- askSlashCmdConfig
  pure $ if
    | dat ^. #token /= config ^. #verify_token            -> Left "Invalid token..."
    | dat ^. #team_id /= config ^. #team_id               -> Left "Invalid team..."
    | dat ^. #channel_id `notElem` config ^. #channel_ids -> Left "Invalid channel..."
    | dat ^. #command /= config ^. #reset_repo_cmd        -> Left "Invalid command..."
    | otherwise                                           -> pure ()

data SlashCmd

instance Accept SlashCmd where
  contentType _ = "application" M.// "x-www-form-urlencoded"

instance MimeUnrender SlashCmd (LByteString, SlashCmdData) where
  mimeUnrender _ bs = Text.unpack +++ (bs,) $ urlDecodeAsForm bs

type Message = Record '[ "text" >: Text ]

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

type NotifyConfig = Record
  '[ "api_token"  >: ByteString
   , "channel_id" >: Text
   ]

class HasSlackNotifyConfig env where
  slackNotifyConfigL :: Lens' env NotifyConfig

instance Lookup xs "notify" NotifyConfig => HasSlackNotifyConfig (Record xs) where
  slackNotifyConfigL = lens (view #notify) (\x y -> x & #notify `set` y)

askNotifyConfig :: HasSlackNotifyConfig env => RIO env NotifyConfig
askNotifyConfig = view slackNotifyConfigL

type SnipetMessage = Record
  '[ "content"         >: Text
   , "filename"        >: Text
   , "filetype"        >: Text
   , "initial_comment" >: Text
   ]

uploadFile :: (HasLogFunc env, HasSlackNotifyConfig env) => SnipetMessage -> RIO env ()
uploadFile msg = do
  config <- askNotifyConfig
  let opts = W.defaults
           & W.header "Content-Type" .~ ["application/x-www-form-urlencoded"]
           & W.header "Authorization" .~ ["Bearer " <> config ^. #api_token]
      dat =
        [ "content" W.:= msg ^. #content
        , "filename" W.:= msg ^. #filename
        , "filetype" W.:= msg ^. #filetype
        , "initial_comment" W.:= msg ^. #initial_comment
        , "channels" W.:= config ^. #channel_id
        ]
  _ <- liftIO (W.postWith opts "https://slack.com/api/files.upload" dat)
  pure ()
