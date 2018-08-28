{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Paths_git_plantation     (version)
import           RIO
import qualified RIO.ByteString           as B

import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version             (Version)
import qualified Data.Version             as Version
import           Development.GitRev
import qualified Drone.Client             as Drone
import           Git.Plantation
import           Git.Plantation.API       (api, server)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import qualified Servant.GitHub.Webhook   (GitHubKey, gitHubKey)
import           System.Environment       (getEnv)

main :: IO ()
main = withGetOpt "[options] [config-file]" opts $ \r args ->
  case (r ^. #version, listToMaybe args) of
    (True, _)      -> B.putStr $ fromString (showVersion version) <> "\n"
    (_, Nothing)   -> error "please input config file path."
    (_, Just path) -> runServer r =<< readConfig path
  where
    opts = #port    @= portOpt
        <: #work    @= workOpt
        <: #verbose @= verboseOpt
        <: #version @= versionOpt
        <: nil

type Options = Record
  '[ "port"    >: Int
   , "work"    >: FilePath
   , "verbose" >: Bool
   , "version" >: Bool
   ]

portOpt :: OptDescr' Int
portOpt = optionReqArg
  (pure . fromMaybe 8080 . (readMaybe <=< listToMaybe))
  ['p'] ["port"] "PORT" "Set port to PORT instead of 8080."

workOpt :: OptDescr' FilePath
workOpt = optionReqArg
  (pure . fromMaybe ".temp" . listToMaybe)
  [] ["work"] "DIR" "Set workdir to DIR instead of ./.temp"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

runServer :: Options -> Config -> IO ()
runServer opts config = do
  logOpts   <- logOptionsHandle stdout $ opts ^. #verbose
  token     <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  dHost     <- liftIO $ fromString <$> getEnv "DRONE_HOST"
  dToken    <- liftIO $ fromString <$> getEnv "DRONE_TOKEN"
  indexHtml <- fromStrictBytes <$> readFileBinary "index.html"
  let client = Drone.HttpsClient (#host @= dHost <: #token @= dToken <: nil)
  withLogFunc logOpts $ \logger -> do
    let env = #config @= config
           <: #token  @= token
           <: #work   @= (opts ^. #work)
           <: #client @= client
           <: #logger @= logger
           <: nil :: Env
    B.putStr $ "Listening on port " <> (fromString . show) (opts ^. #port) <> "\n"
    Warp.run (opts ^. #port) $
      app env (gitHubKey $ fromString <$> getEnv "GH_SECRET") indexHtml

app :: Env -> GitHubKey -> LByteString -> Application
app env key indexHtml =
  serveWithContext api (key :. EmptyContext) $
    hoistServerWithContext api context (runRIO env) (server indexHtml)

context :: Proxy '[ GitHubKey ]
context = Proxy

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]

-- HACK
newtype GitHubKey = GitHubKey (forall result. Servant.GitHub.Webhook.GitHubKey result)

gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (Servant.GitHub.Webhook.gitHubKey k)

instance HasContextEntry '[GitHubKey] (Servant.GitHub.Webhook.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x
