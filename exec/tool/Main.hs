{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Options
import qualified Paths_git_plantation  as Meta
import           RIO

import           Configuration.Dotenv  (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Version          (Version)
import qualified Data.Version          as Version
import           Development.GitRev
import qualified Drone.Client          as Drone
import           GHC.TypeLits          hiding (Mod)
import           Git.Plantation.Cmd    as Cmd
import           Git.Plantation.Config (readConfig)
import           Git.Plantation.Env    (mkWebhookConf)
import           Options.Applicative
import           System.Environment    (getEnv)

import qualified Mix
import qualified Mix.Plugin.Drone      as MixDrone
import qualified Mix.Plugin.GitHub     as MixGitHub
import qualified Mix.Plugin.Logger     as MixLogger
import qualified Mix.Plugin.Shell      as MixShell

main :: IO ()
main = execParser parser >>= \opts -> do
  _ <- tryIO $ loadFile defaultConfig
  config  <- readConfig (opts ^. #config)
  token   <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  secret  <- liftIO $ fromString <$> getEnv "GH_SECRET"
  appUrl  <- liftIO $ fromString <$> getEnv "APP_SERVER"
  let client  = #host @= "" <: #port @= Nothing <: #token @= "" <: nil
      logConf = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      plugin  = hsequence
          $ #config  <@=> pure config
         <: #github  <@=> MixGitHub.buildPlugin token
         <: #slack   <@=> pure Nothing
         <: #work    <@=> MixShell.buildPlugin (opts ^. #work)
         <: #drone   <@=> MixDrone.buildPlugin client Drone.HttpsClient
         <: #webhook <@=> pure (mkWebhookConf (appUrl <> "/hook") secret)
         <: #logger  <@=> MixLogger.buildPlugin logConf
         <: nil
  Mix.run plugin $ Cmd.run (opts ^. #subcmd)
  where
    parser = info (options <**> version Meta.version <**> helper)
           $ fullDesc
          <> header "taskpad - operate daily tasks"

options :: Parser Options
options = hsequence
    $ #verbose <@=> switch (long "verbose" <> short 'v' <> help "Enable verbose mode: verbosity level \"debug\"")
   <: #config  <@=> strOption (long "config" <> short 'c' <> value "config.yaml" <> metavar "PATH" <> help "Configuration file")
   <: #work    <@=> strOption (long "work" <> value "~/.git-plantation" <> metavar "PATH" <> help "Work directory to exec git commands")
   <: #subcmd  <@=> subcmdParser
   <: nil

subcmdParser :: Parser SubCmd
subcmdParser = variantFrom
    $ #verify           @= pure ()               `withInfo` "Verify config file."
   <: #new_repo         @= newRepoCmdParser      `withInfo` "Create repository for team."
   <: #new_github_repo  @= singleRepoCmdParser   `withInfo` "Create new repository for team in GitHub"
   <: #init_github_repo @= singleRepoCmdParser   `withInfo` "Init repository for team in GitHub"
   <: #setup_webhook    @= singleRepoCmdParser   `withInfo` "Setup GitHub Webhook to team repository"
   <: #init_ci          @= singleRepoCmdParser   `withInfo` "Init CI repository by team repository"
   <: #reset_repo       @= singleRepoCmdParser   `withInfo` "Reset repository for team"
   <: #delete_repo      @= deleteRepoCmdParser   `withInfo` "Delete repository for team."
   <: #invite_member    @= inviteMemberCmdParser `withInfo` "Invite member to team repository"
   <: #kick_member      @= kickMemberCmdParser   `withInfo` "Kick member from team repository"
   <: nil

newRepoCmdParser :: Parser NewRepoCmd
newRepoCmdParser = hsequence
    $ #repos              <@=> option comma (long "repos" <> value [] <> metavar "IDS" <> help "Sets reopsitory that want to controll by problem id.")
   <: #team               <@=> strArgument (metavar "TEXT" <> help "Sets team that want to controll.")
   <: #skip_create_repo   <@=> switch (long "skip_create_repo" <> help "Flag for skip create new repository in GitHub")
   <: #skip_init_repo     <@=> switch (long "skip_init_repo" <> help "Flag for skip init repository in GitHub")
   <: #skip_setup_webhook <@=> switch (long "skip_setup_webhook" <> help "Flag for skip setup GitHub Webhook to repository")
   <: #skip_init_ci       <@=> switch (long "skip_init_ci" <> help "Flag for skip init CI by repository")
   <: nil

singleRepoCmdParser :: Parser (Record RepoCmdFields)
singleRepoCmdParser = hsequence
    $ #repo <@=> option auto (long "repo" <> metavar "ID" <> help "Sets reopsitory that want to controll by problem id.")
   <: #team <@=> strArgument (metavar "TEXT" <> help "Sets team that want to controll.")
   <: nil

deleteRepoCmdParser :: Parser DeleteRepoCmd
deleteRepoCmdParser = hsequence
    $ #repos              <@=> option comma (long "repos" <> value [] <> metavar "IDS" <> help "Sets reopsitory that want to controll by problem id.")
   <: #team               <@=> strArgument (metavar "TEXT" <> help "Sets team that want to controll.")
   <: nil

inviteMemberCmdParser :: Parser InviteMemberCmd
inviteMemberCmdParser = hsequence
    $ #team  <@=> strArgument (metavar "TEXT" <> help "Sets team that want to controll.")
   <: #repos <@=> option comma (long "repos" <> value [] <> metavar "ID" <> help "Sets reopsitory that want to controll by problem id.")
   <: #user  <@=> option (Just <$> str) (long "user" <> value Nothing <> metavar "TEXT" <> help "Sets user that want to controll.")
   <: nil

kickMemberCmdParser :: Parser KickMemberCmd
kickMemberCmdParser = inviteMemberCmdParser

variantFrom ::
  Forall (KeyIs KnownSymbol) xs => RecordOf ParserInfo xs -> Parser (Variant xs)
variantFrom = subparser . subcmdVariant
  where
    subcmdVariant = hfoldMapWithIndexFor (Proxy @ (KeyIs KnownSymbol)) $ \m x ->
      let k = symbolVal (proxyAssocKey m)
      in command k (EmbedAt m . Field . pure <$> getField x)

instance Wrapper ParserInfo where
  type Repr ParserInfo a = ParserInfo a
  _Wrapper = id

-- |
-- support `--hoge 1,2,3`
comma :: Read a => ReadM [a]
comma = maybeReader (\s -> readMaybe $ "[" ++ s ++ "]")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts = info (helper <*> opts) . progDesc

version :: Version -> Parser (a -> a)
version v = infoOption (showVersion v)
    $ long "version"
   <> help "Show version"

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
