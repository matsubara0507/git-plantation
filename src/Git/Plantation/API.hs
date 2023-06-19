{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Git.Plantation.API where

import           RIO
import qualified RIO.Text                    as T

import qualified Crypto.Hash                 as Hash
import qualified Crypto.Random               as Random
import qualified Data.Aeson.Text             as Json
import           Data.Coerce                 (coerce)
import           Data.Extensible
import           Data.Fallible
import           Git.Plantation.API.CRUD     (GetAPI, getAPI, ResetAPI, resetAPI)
import qualified Git.Plantation.API.GitHub   as GitHub
import qualified Git.Plantation.API.Slack    as Slack
import qualified Git.Plantation.Auth.GitHub  as Auth
import qualified Git.Plantation.Data.User    as User
import           Git.Plantation.Env          (Plant)
import qualified GitHub
import           Servant
import qualified Servant.Auth.Server         as Auth
import           Servant.Auth.Server         (Auth)
import           Servant.HTML.Blaze
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5.Attributes as H

type LoginPageSession = Record
    '[ "state" >: Text
     , "salt"  >: Text
     ]

newSession :: MonadIO m => m LoginPageSession
newSession = do
  gen <- liftIO Random.drgNew
  let (stat, gen') = randomGenerateBS gen
      (salt, _)    = randomGenerateBS gen'
  pure $ #state @= tshow (Hash.hash stat :: Hash.Digest Hash.SHA256)
      <: #salt  @= tshow (Hash.hash salt :: Hash.Digest Hash.SHA256)
      <: nil
  where
    randomGenerateBS :: Random.ChaChaDRG -> (ByteString, Random.ChaChaDRG)
    randomGenerateBS = Random.randomBytesGenerate 32

toState :: LoginPageSession -> String
toState session = T.unpack $ session ^. #state

type Account = Record '[ "login" >: User.GitHubId ]

toAccount :: GitHub.User -> Account
toAccount user = #login @= coerce (GitHub.untagName $ GitHub.userLogin user) <: nil

type API
      = LoginPage
   :<|> (Auth '[Auth.Cookie] LoginPageSession :> LoginCollback)
   :<|> (Auth '[Auth.Cookie] Account :> Protected)
   :<|> Unprotected

type LoginPage
     = "login" :> Get '[HTML] (Headers JWTCookieHeaders H.Html)

type LoginCollback
     = "callback"
     :> QueryParam "code" String
     :> QueryParam "state" String
     :> GetRedirected JWTCookieHeaders

type Protected = "api" :> (GetAPI :<|> ResetAPI) :<|> Index

type Index
      = Get '[HTML] H.Html
   :<|> "graph" :> Get '[HTML] H.Html
   :<|> "teams" :> Capture "id" Text :> Get '[HTML] H.Html
   :<|> "teams" :> Capture "id" Text :> Capture "user" Text :> Get '[HTML] H.Html

type Unprotected
      = "static" :> Raw
   :<|> "hook"   :> GitHub.WebhookAPI
   :<|> "slash"  :> Slack.SlashCmdAPI

type GetRedirected headers =
  Verb 'GET 303 '[HTML] (Headers (Header "Location" String ': headers) NoContent)

type JWTCookieHeaders =
  '[ Header "Set-Cookie" Auth.SetCookie, Header "Set-Cookie" Auth.SetCookie ]

api :: Proxy API
api = Proxy

type LoginConfig = Record
  '[ "whitelist"   >: [User.GitHubId]
   , "allow_guest" >: Bool
   ]

server :: LoginConfig -> ServerT API Plant
server config
       = loginPage
    :<|> callback
    :<|> protected config
    :<|> serveDirectoryFileServer "static"
    :<|> GitHub.webhook
    :<|> Slack.slashCmdApi

loginPage :: Plant (Headers JWTCookieHeaders H.Html)
loginPage = evalContT $ do
  config  <- asks (view #oauth)
  session <- lift newSession
  applyCookies <- acceptLogin' config session !?? exit throw401
  let url = fromString $ Auth.authorizeUrl config (toState session)
  pure $ applyCookies (loginHtml url)
  where
    loginHtml loginUrl =
      H.docTypeHtml $ do
        H.head $ do
          stylesheet "https://unpkg.com/@primer/css@13.2.0/dist/primer.css"
          stylesheet "https://use.fontawesome.com/releases/v5.2.0/css/all.css"
        H.div ! H.class_ "m-3" $
          H.a ! H.href loginUrl $ "Login by GitHub"
    acceptLogin' conf = liftIO . Auth.acceptLogin (conf ^. #cookie) (conf ^. #jwt)
    throw401 = Auth.throwAll err401

callback
  :: Auth.AuthResult LoginPageSession
  -> Maybe String
  -> Maybe String
  -> Plant (Headers (Header "Location" String ': JWTCookieHeaders) NoContent)
callback auth code state =
  case auth of
    Auth.Authenticated s | Just (toState s) == state -> do
      logDebug $ fromString ("[GET] /callback: " <> toState s)
      callback'
    _  -> do
      logWarn "[GET] /callback: no session"
      throw401
  where
    callback' = evalContT $ do
      code'  <- code ??? exit throw401
      config <- asks (view #oauth)
      token  <- lift $ Auth.fetchToken config code'
      user   <- lift (Auth.fetchUser token) !?= const (exit throw401)
      applyCookies <- acceptLogin' config (toAccount user) !?? exit throw401
      pure $ addHeader "/" (applyCookies NoContent)
    acceptLogin' conf = liftIO . Auth.acceptLogin (conf ^. #cookie) (conf ^. #jwt)
    throw401 = Auth.throwAll err401

protected :: LoginConfig -> Auth.AuthResult Account -> ServerT Protected Plant
protected config = \case
  Auth.Authenticated a | a ^. #login `elem` config ^. #whitelist ->
      (getAPI :<|> resetAPI (a  ^. #login)) :<|> index
  Auth.Indefinite | config ^. #allow_guest ->
      (getAPI :<|> resetAPI "") :<|> index
  Auth.Indefinite ->
      Auth.throwAll login
  _ ->
      Auth.throwAll err401
  where
    index = indexHtml :<|> indexHtml :<|> const indexHtml :<|> (\_ _ -> indexHtml)
    login = err303 { errHeaders = [("Location", "/login")] }

indexHtml :: Plant H.Html
indexHtml = do
  config <- asks (view #config)
  pure $ H.docTypeHtml $ do
    H.head $ do
      stylesheet "https://unpkg.com/@primer/css@^20.2.4/dist/primer.css"
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
