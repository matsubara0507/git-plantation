{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Git.Plantation.API where

import           RIO

import qualified Data.Aeson.Text             as Json
import           Data.Extensible
import           Data.Fallible
import           Git.Plantation.API.CRUD     (CRUD, crud)
import           Git.Plantation.API.Webhook  (WebhookAPI, webhook)
import qualified Git.Plantation.Auth.GitHub  as Auth
import           Git.Plantation.Env          (Plant)
import qualified GitHub
import           Servant
import           Servant.Auth.Server         (Auth)
import qualified Servant.Auth.Server         as Auth
import           Servant.HTML.Blaze
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as H

type Account = Record '[ "login" >: Text ]

toAccount :: GitHub.User -> Account
toAccount user = #login @= GitHub.untagName (GitHub.userLogin user) <: nil

type API = (Auth '[Auth.Cookie] Account :> Protected) :<|> Unprotected

type Protected = "api" :> CRUD :<|> Index

type Index
      = Get '[HTML] H.Html
   :<|> "graph" :> Get '[HTML] H.Html
   :<|> "teams" :> Capture "id" Text :> Get '[HTML] H.Html
   :<|> "teams" :> Capture "id" Text :> Capture "user" Text :> Get '[HTML] H.Html

type Unprotected
      = "static"   :> Raw
   :<|> "hook"     :> WebhookAPI
   :<|> "login"    :> GetRedirected '[]
   :<|> "callback" :> QueryParam "code" String :> GetRedirected JWTCookieHeaders

type GetRedirected headers =
  Verb 'GET 308 '[HTML] (Headers (Header "Location" String ': headers) NoContent)

type JWTCookieHeaders =
  '[ Header "Set-Cookie" Auth.SetCookie, Header "Set-Cookie" Auth.SetCookie ]

api :: Proxy API
api = Proxy

server :: ServerT API Plant
server = protected
    :<|> serveDirectoryFileServer "static"
    :<|> webhook
    :<|> login
    :<|> callback
    where
      login = redirectTo <$> Auth.authorizeUrl
      callback code = evalContT $ do
        code' <- code ??? exit throw401
        token <- lift $ Auth.fetchToken code'
        user  <- lift (Auth.fetchUser token) !?= const (exit throw401)
        env   <- lift ask
        applyCookies <- acceptLogin' env (toAccount user) !?? exit throw401
        pure $ addHeader "/" (applyCookies NoContent)
      acceptLogin' env session =
        liftIO $ Auth.acceptLogin (env ^. #cookie) (env ^. #jwt) session
      throw401 = Auth.throwAll err401

protected :: Auth.AuthResult Account -> ServerT Protected Plant
protected = \case
  Auth.Authenticated _ -> crud :<|> index
  _                    -> Auth.throwAll err401
  where
    index = indexHtml :<|> indexHtml :<|> const indexHtml :<|> (\_ _ -> indexHtml)

indexHtml :: Plant H.Html
indexHtml = do
  config <- asks (view #config)
  pure $ H.docTypeHtml $ do
    H.head $ do
      stylesheet "https://unpkg.com/@primer/css@13.2.0/dist/primer.css"
      stylesheet "https://use.fontawesome.com/releases/v5.2.0/css/all.css"
    H.div ! H.id "main" $ H.text ""
    H.script ! H.type_ "application/json" ! H.id "config" $
      H.preEscapedLazyText (Json.encodeToLazyText config)
    H.script ! H.src "/static/main.js" $ H.text ""
    H.script ! H.src "/static/index.js" $ H.text ""

stylesheet :: H.AttributeValue -> H.Html
stylesheet url =
  H.link ! H.rel "stylesheet" ! H.type_ "text/css" ! H.href url ! H.media "all"

redirectTo :: AddHeader "Location" String NoContent a => String -> a
redirectTo url = addHeader url NoContent
