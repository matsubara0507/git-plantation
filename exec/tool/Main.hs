{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Options
import qualified Paths_git_plantation  as Meta
import           RIO

import           Configuration.Dotenv  (defaultConfig, loadFile)
import           Data.Extensible
import           Git.Plantation.Cmd    as Cmd
import           Git.Plantation.Config (readConfig)
import           Git.Plantation.Env    (mkWebhookConf)
import           Options.Applicative
import           System.Environment    (getEnv)

import qualified Mix
import qualified Mix.Plugin.GitHub     as MixGitHub
import qualified Mix.Plugin.Logger     as MixLogger
import qualified Mix.Plugin.Shell      as MixShell

main :: IO ()
main = execParser parser >>= \opts -> do
  _ <- tryIO $ loadFile defaultConfig
  config <- readConfig (opts ^. #config)
  token  <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  secret <- liftIO $ fromString <$> getEnv "GH_SECRET"
  appUrl <- liftIO $ fromString <$> getEnv "APP_SERVER"
  let logConf = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      plugin  = hsequence
          $ #config  <@=> pure config
         <: #github  <@=> MixGitHub.buildPlugin token
         <: #work    <@=> MixShell.buildPlugin (opts ^. #work)
         <: #webhook <@=> pure (mkWebhookConf (appUrl <> "/hook") secret)
         <: #logger  <@=> MixLogger.buildPlugin logConf
         <: nil
  Mix.run plugin $ Cmd.run (opts ^. #subcmd)
  where
    parser = info (options <**> version Meta.version <**> helper)
           $ fullDesc
          <> header "git-plantation-tool - operate repository for git-plantation"
