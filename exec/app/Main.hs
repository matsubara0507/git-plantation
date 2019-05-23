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
import qualified RIO.Text                 as Text

import           Configuration.Dotenv     (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version             (Version)
import qualified Data.Version             as Version
import           Development.GitRev
import           Git.Plantation
import           Git.Plantation.API       (api, server)
import qualified Mix.Plugin               as Mix (withPlugin)
import qualified Mix.Plugin.Drone         as MixDrone
import qualified Mix.Plugin.GitHub        as MixGitHub
import qualified Mix.Plugin.Logger        as MixLogger
import qualified Mix.Plugin.Shell         as MixShell
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import qualified Servant.GitHub.Webhook   (GitHubKey, gitHubKey)
import           System.Environment       (getEnv, lookupEnv)

import           Orphans                  ()

main :: IO ()
main = withGetOpt "[options] [config-file]" opts $ \r args -> do
  _ <- tryIO $ loadFile defaultConfig
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
  token         <- liftIO $ fromString  <$> getEnv "GH_TOKEN"
  dHost         <- liftIO $ fromString  <$> getEnv "DRONE_HOST"
  dToken        <- liftIO $ fromString  <$> getEnv "DRONE_TOKEN"
  dPort         <- liftIO $ readMaybe   <$> getEnv "DRONE_PORT"
  sToken        <- liftIO $ fromString  <$> getEnv "SLACK_API_TOKEN"
  sTeam         <- liftIO $ fromString  <$> getEnv "SLACK_TEAM_ID"
  sChannels     <- liftIO $ readListEnv <$> getEnv "SLACK_CHANNEL_IDS"
  sResetRepoCmd <- liftIO $ fromString  <$> getEnv "SLACK_RESET_REPO_CMD"
  sWebhook      <- liftIO $ fromString  <$> getEnv "SLACK_WEBHOOK"
  storeUrl      <- liftIO $ fromString  <$> getEnv "STORE_URL"
  dHttp         <- lookupEnv "DRONE_HTTP"
  let client    = #host @= dHost <: #port @= dPort <: #token @= dToken <: nil
      logConf   = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      slackConf
          = #token          @= sToken
         <: #team_id        @= sTeam
         <: #channel_ids    @= sChannels
         <: #user_ids       @= []
         <: #reset_repo_cmd @= sResetRepoCmd
         <: #webhook        @= Just sWebhook
         <: nil
      plugin    = hsequence
          $ #config  <@=> pure config
         <: #github  <@=> MixGitHub.buildPlugin token
         <: #slack   <@=> pure (Just slackConf)
         <: #work    <@=> MixShell.buildPlugin (opts ^. #work)
         <: #drone   <@=> MixDrone.buildPlugin client (dHttp == Just "true")
         <: #webhook <@=> pure mempty
         <: #store   <@=> pure storeUrl
         <: #logger  <@=> MixLogger.buildPlugin logConf
         <: nil
  B.putStr $ "Listening on port " <> (fromString . show) (opts ^. #port) <> "\n"
  flip Mix.withPlugin plugin $ \env ->
    Warp.run (opts ^. #port) $ app env $ gitHubKey $ fromString <$> getEnv "GH_SECRET"

app :: Env -> GitHubKey ->Application
app env key =
  serveWithContext api (key :. EmptyContext) $
    hoistServerWithContext api context (runRIO env) server

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

readListEnv :: Read a => String -> [a]
readListEnv = mapMaybe (readMaybe . show) . Text.split (== ',') . fromString

-- HACK
newtype GitHubKey = GitHubKey (forall result. Servant.GitHub.Webhook.GitHubKey result)

gitHubKey :: IO ByteString -> GitHubKey
gitHubKey k = GitHubKey (Servant.GitHub.Webhook.gitHubKey k)

instance HasContextEntry '[GitHubKey] (Servant.GitHub.Webhook.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x
