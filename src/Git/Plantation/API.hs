{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Git.Plantation.API where

import           RIO

import qualified Data.Aeson.Text             as Json
import           Git.Plantation.API.CRUD     (CRUD, crud)
import           Git.Plantation.API.Webhook  (WebhookAPI, webhook)
import           Git.Plantation.Env          (Plant)
import           Servant
import           Servant.HTML.Blaze
import           Servant.Server.StaticFiles  (serveDirectoryFileServer)
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as H

type API
      = Get '[HTML] H.Html
   :<|> "graph"  :> Get '[HTML] H.Html
   :<|> "static" :> Raw
   :<|> "hook"   :> WebhookAPI
   :<|> "api"    :> CRUD

api :: Proxy API
api = Proxy

server :: ServerT API Plant
server = indexHtml
    :<|> graphHtml
    :<|> serveDirectoryFileServer "static"
    :<|> webhook
    :<|> crud

indexHtml :: Plant H.Html
indexHtml = do
  config <- asks (view #config)
  pure $ H.docTypeHtml $ do
    H.head $ do
      stylesheet "https://cdnjs.cloudflare.com/ajax/libs/Primer/10.8.1/build.css"
      stylesheet "https://use.fontawesome.com/releases/v5.2.0/css/all.css"
    H.div ! H.id "main" $ H.text ""
    H.script ! H.type_ "application/json" ! H.id "config" $
      H.preEscapedLazyText (Json.encodeToLazyText config)
    H.script ! H.src "static/main.js" $ H.text ""
    H.script ! H.src "static/index.js" $ H.text ""

graphHtml :: Plant H.Html
graphHtml = do
  config <- asks (view #config)
  pure $ H.docTypeHtml $ do
    H.head $ do
      stylesheet "https://cdnjs.cloudflare.com/ajax/libs/Primer/10.8.1/build.css"
      stylesheet "https://use.fontawesome.com/releases/v5.2.0/css/all.css"
    H.div ! H.id "main" $ H.text ""
    H.script ! H.type_ "application/json" ! H.id "config" $
      H.preEscapedLazyText (Json.encodeToLazyText config)
    H.script ! H.src "static/graph.js" $ H.text ""
    H.script ! H.src "static/index.js" $ H.text ""

stylesheet :: H.AttributeValue -> H.Html
stylesheet url =
  H.link ! H.rel "stylesheet" ! H.type_ "text/css" ! H.href url ! H.media "all"
