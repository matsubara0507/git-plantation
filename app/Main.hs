{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           RIO                      hiding (Handler)

import           Data.Extensible
import           Git.Plantation
import           Git.Plantation.API       (api, server)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import qualified Servant.GitHub.Webhook   (GitHubKey, gitHubKey)
import           System.Environment       (getArgs, getEnv)

main :: IO ()
main = (listToMaybe <$> getArgs) >>= \case
  Nothing   -> error "please input config file path."
  Just path -> runServer =<< readConfig path

runServer :: Config -> IO ()
runServer config = do
  logOpts <- logOptionsHandle stdout False
  withLogFunc logOpts $ \logger -> do
    let env = #config @= config
           <: #token  @= ""
           <: #work   @= ""
           <: #logger @= logger
           <: nil :: Env
    hPutBuilder stdout "Listening on port 8080"
    Warp.run 8080 $ app env (gitHubKey $ fromString <$> getEnv "GH_SECRET")

app :: Env -> GitHubKey -> Application
app env key =
  serveWithContext api (key :. EmptyContext) $
    hoistServerWithContext api context (runRIO env) server

context :: Proxy '[ GitHubKey ]
context = Proxy

-- HACK
newtype GitHubKey = GitHubKey (forall result. Servant.GitHub.Webhook.GitHubKey result)

gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (Servant.GitHub.Webhook.gitHubKey k)

instance HasContextEntry '[GitHubKey] (Servant.GitHub.Webhook.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x
