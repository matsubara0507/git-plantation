{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Git.Plantation.API where

import           RIO

import           Git.Plantation.API.Webhook (WebhookAPI, webhook)
import           Git.Plantation.Env         (Plant)
import           Network.HTTP.Media         ((//), (/:))
import           Servant
import           Servant.Utils.StaticFiles  (serveDirectoryFileServer)

type API
      = Get '[HTML] LByteString
   :<|> "static" :> Raw
   :<|> "hook" :> WebhookAPI

api :: Proxy API
api = Proxy

server :: LByteString -> ServerT API Plant
server indexHtml
      = pure indexHtml
   :<|> serveDirectoryFileServer "static"
   :<|> webhook

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML LByteString where
  mimeRender _ bs = bs
