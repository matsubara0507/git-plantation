{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd
    ( module X
    , run
    ) where

import           RIO

import           Data.Extensible
import qualified Drone.Client               as Drone
import           Git.Plantation.Cmd.Member  as X
import           Git.Plantation.Cmd.Options as X
import           Git.Plantation.Cmd.Repo    as X
import           Git.Plantation.Cmd.Run     as X
import           Git.Plantation.Config      (readConfig)
import           Git.Plantation.Env         (mkWebhookConf)
import qualified Mix
import qualified Mix.Plugin.Drone           as MixDrone
import qualified Mix.Plugin.GitHub          as MixGitHub
import qualified Mix.Plugin.Logger          as MixLogger
import qualified Mix.Plugin.Shell           as MixShell
import           System.Environment         (getEnv)

run :: MonadUnliftIO m => Options -> m ()
run opts = do
  config  <- readConfig (opts ^. #config)
  token   <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  secret  <- liftIO $ fromString <$> getEnv "GH_SECRET"
  appUrl  <- liftIO $ fromString <$> getEnv "APP_SERVER"
  let client  = #host @= "" <: #port @= Nothing <: #token @= "" <: nil
      logConf = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      plugin  = hsequence
          $ #config  <@=> pure config
         <: #github  <@=> MixGitHub.buildPlugin token
         <: #work    <@=> MixShell.buildPlugin (opts ^. #work)
         <: #drone   <@=> MixDrone.buildPlugin client Drone.HttpsClient
         <: #webhook <@=> pure (mkWebhookConf (appUrl <> "/hook") secret)
         <: #logger  <@=> MixLogger.buildPlugin logConf
         <: nil
  Mix.run plugin $ matchField
      (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
      (opts ^. #subcmd)
