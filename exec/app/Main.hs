{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import           Paths_git_plantation     (version)
import           RIO                      hiding (catch)
import qualified RIO.ByteString           as B
import qualified RIO.Text                 as Text
import qualified RIO.Time                 as Time

import           Configuration.Dotenv     (defaultConfig, loadFile)
import           Control.Monad.Catch      (catch)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version             (Version)
import qualified Data.Version             as Version
import           Development.GitRev
import           Git.Plantation
import           Git.Plantation.API       (LoginConfig, api, server)
import qualified Mix.Plugin               as Mix (withPlugin)
import qualified Mix.Plugin.GitHub        as MixGitHub
import qualified Mix.Plugin.Logger        as MixLogger
import qualified Mix.Plugin.Shell         as MixShell
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import qualified Servant.Auth.Server      as Auth
import qualified Servant.GitHub.Webhook   (GitHubKey, gitHubKey)
import           System.Environment       (getEnv)

import           Orphans                  ()

main :: IO ()
main = withGetOpt "[options] [config-file]" opts $ \r args -> do
  _ <- tryIO $ loadFile defaultConfig
  if r ^. #version then
    B.putStr $ fromString (showVersion version) <> "\n"
  else
    case listToMaybe args of
      Nothing   -> error "please input config file path."
      Just path -> runServer r =<< readConfig path
  where
    opts = #port    @= portOpt
        <: #work    @= workOpt
        <: #guest   @= guestOpt
        <: #verbose @= verboseOpt
        <: #version @= versionOpt
        <: nil

type Options = Record
  '[ "port"    >: Int
   , "work"    >: FilePath
   , "guest"   >: Bool
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

guestOpt :: OptDescr' Bool
guestOpt = optFlag [] ["guest"] "Allow guest mode"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

runServer :: Options -> Config -> IO ()
runServer opts config = do
  token         <- liftIO $ fromString  <$> getEnv "GH_TOKEN"
  sSignSecret   <- liftIO $ fromString  <$> getEnv "SLACK_SIGNING_SECRET"
  sVerifyToken  <- liftIO $ fromString  <$> getEnv "SLACK_VERIFY_TOKEN"
  slashTeam     <- liftIO $ fromString  <$> getEnv "SLACK_SLASH_TEAM_ID"
  slashChannels <- liftIO $ readListEnv <$> getEnv "SLACK_SLASH_CHANNEL_IDS"
  sResetRepoCmd <- liftIO $ fromString  <$> getEnv "SLACK_SLASH_RESET_REPO_CMD"
  slackWebhook  <- liftIO $ fromString  <$> getEnv "SLACK_WEBHOOK"
  clientId      <- liftIO $ fromString  <$> getEnv "AUTHN_CLIENT_ID"
  clientSecret  <- liftIO $ fromString  <$> getEnv "AUTHN_CLIENT_SECRET"
  jobserverHost <- liftIO $ getEnv "JOBSERVER_HOST"
  jwtSettings   <- Auth.defaultJWTSettings <$> Auth.generateKey
  let logConf = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      slashConf
          = #signing_secret @= sSignSecret
         <: #verify_token   @= sVerifyToken
         <: #team_id        @= slashTeam
         <: #channel_ids    @= slashChannels
         <: #reset_repo_cmd @= sResetRepoCmd
         <: #webhook        @= Just slackWebhook
         <: nil
      oauthConf
          = #client_id     @= clientId
         <: #client_secret @= clientSecret
         <: #cookie        @= cookieSettings
         <: #jwt           @= jwtSettings
         <: nil
      plugin = hsequence
          $ #config    <@=> pure config
         <: #github    <@=> MixGitHub.buildPlugin token
         <: #slack     <@=> pure slackWebhook
         <: #slash     <@=> pure slashConf
         <: #work      <@=> MixShell.buildPlugin (opts ^. #work)
         <: #webhook   <@=> pure mempty
         <: #jobserver <@=> pure jobserverHost
         <: #logger    <@=> MixLogger.buildPlugin logConf
         <: #oauth     <@=> pure oauthConf
         <: nil
  B.putStr $ "Listening on port " <> (fromString . show) (opts ^. #port) <> "\n"
  flip Mix.withPlugin plugin $ \env -> do
    let key = gitHubKey $ fromString <$> getEnv "GH_SECRET"
        loginConf
            = #whitelist   @= mkAuthnWhitelist (env ^. #config)
           <: #allow_guest @= opts ^. #guest
           <: nil
    Warp.run (opts ^. #port) (app loginConf env cookieSettings jwtSettings key)
  where
    cookieSettings = Auth.defaultCookieSettings
      { Auth.cookieMaxAge = Just $ Time.secondsToDiffTime (3 * 60)
      , Auth.cookieXsrfSetting = Nothing
      }

app :: LoginConfig -> Env -> Auth.CookieSettings -> Auth.JWTSettings -> GitHubKey -> Application
app loginConfig env cookie jwt key =
  serveWithContext api (cookie :. jwt :. key :. EmptyContext) $
    hoistServerWithContext api context
      (\x -> runRIO env x `catch` throwError) (server loginConfig)

context :: Proxy '[ Auth.CookieSettings, Auth.JWTSettings, GitHubKey ]
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
