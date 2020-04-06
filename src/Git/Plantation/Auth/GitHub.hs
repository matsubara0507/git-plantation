{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Auth.GitHub where

import           RIO

import           Data.Extensible
import           Git.Plantation.Env
import qualified GitHub
import           Network.HTTP.Req

authorizeUrl :: Plant String
authorizeUrl = do
  clientId <- asks (view #client_id . view #oauth)
  pure $ "https://github.com/login/oauth/authorize?client_id=" ++ clientId

fetchUser :: MonadIO m => ByteString -> m (Either GitHub.Error GitHub.User)
fetchUser token =
  liftIO $ GitHub.github (GitHub.OAuth token) GitHub.userInfoCurrentR

fetchToken :: String -> Plant ByteString
fetchToken code = do
  config <- asks (view #oauth)
  let params = shrink (#code @= code <: config)
  runReq defaultHttpConfig $ (toToken . responseBody) <$> postTokenRequest params

postTokenRequest :: MonadHttp m => TokenParams -> m (JsonResponse TokenInfo)
postTokenRequest params =
  req POST url (ReqBodyJson params) jsonResponse mempty
  where
    url = https "github.com" /: "login" /: "oauth" /: "access_token"

toToken :: TokenInfo -> ByteString
toToken info = fromString $ info ^. #access_token

type TokenInfo = Record '[ "access_token" >: String ]

type TokenParams = Record
  '[ "client_id"     >: String
   , "client_secret" >: String
   , "code"          >: String
   ]
