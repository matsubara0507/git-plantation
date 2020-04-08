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
   :<|> "login"    :> Get '[HTML] H.Html
   :<|> "callback" :> QueryParam "code" String :> GetRedirected JWTCookieHeaders

type GetRedirected headers =
  Verb 'GET 303 '[HTML] (Headers (Header "Location" String ': headers) NoContent)

type JWTCookieHeaders =
  '[ Header "Set-Cookie" Auth.SetCookie, Header "Set-Cookie" Auth.SetCookie ]

api :: Proxy API
api = Proxy

server :: [Text] -> ServerT API Plant
server whitelist
       = protected whitelist
    :<|> serveDirectoryFileServer "static"
    :<|> webhook
    :<|> loginHtml
    :<|> callback
    where
      callback code = evalContT $ do
        code'  <- code ??? exit throw401
        config <- asks (view #oauth) !?? exit throw401
        token  <- lift $ Auth.fetchToken config code'
        user   <- lift (Auth.fetchUser token) !?= const (exit throw401)
        applyCookies <- acceptLogin' config (toAccount user) !?? exit throw401
        pure $ addHeader "/" (applyCookies NoContent)
      acceptLogin' config session =
        liftIO $ Auth.acceptLogin (config ^. #cookie) (config ^. #jwt) session
      throw401 = Auth.throwAll err401

protected :: [Text] -> Auth.AuthResult Account -> ServerT Protected Plant
protected whitelist = \case
  Auth.Authenticated a | a ^. #login `elem` whitelist -> crud :<|> index
  Auth.Indefinite                                     -> Auth.throwAll login
  _                                                   -> Auth.throwAll err401
  where
    index = indexHtml :<|> indexHtml :<|> const indexHtml :<|> (\_ _ -> indexHtml)
    login = err302 { errHeaders = [("Location", "/login")] }

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

loginHtml :: Plant H.Html
loginHtml = do
  config <- asks (view #oauth)
  let url = fromString $ fromMaybe "" (Auth.authorizeUrl <$> config)
  pure $ H.docTypeHtml $ do
    H.head $ do
      stylesheet "https://unpkg.com/@primer/css@13.2.0/dist/primer.css"
      stylesheet "https://use.fontawesome.com/releases/v5.2.0/css/all.css"
    H.div ! H.class_ "m-3" $
      H.a ! H.href url $ "Login by GitHub"

stylesheet :: H.AttributeValue -> H.Html
stylesheet url =
  H.link ! H.rel "stylesheet" ! H.type_ "text/css" ! H.href url ! H.media "all"

redirectTo :: AddHeader "Location" String NoContent a => String -> a
redirectTo url = addHeader url NoContent
