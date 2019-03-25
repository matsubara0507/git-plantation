{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd
    ( module X
    , run
    ) where

import           RIO
import qualified RIO.Text                   as Text

import           Data.Extensible
import qualified Drone.Client               as Drone
import           Git.Plantation.Cmd.Member  as X
import           Git.Plantation.Cmd.Options as X
import           Git.Plantation.Cmd.Repo    as X
import           Git.Plantation.Cmd.Run     as X
import           Git.Plantation.Config      (readConfig)
import           System.Environment         (getEnv, lookupEnv)

run :: MonadUnliftIO m => Options -> m ()
run opts = do
  config  <- readConfig (opts ^. #config)
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  token   <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  appHost <- liftIO $ fromString <$> getEnv "APP_HOST"
  appPort <- liftIO $ maybe "" fromString <$> lookupEnv "APP_PORT"
  withLogFunc logOpts $ \logger -> do
    let client = #host @= "" <: #port @= Nothing <: #token @= "" <: nil
        webhookUrl = mconcat ["http://", appHost, if Text.null appPort then "" else ":", appPort, "/api"]
        env = #config  @= config
           <: #token   @= token
           <: #work    @= opts ^. #work
           <: #client  @= Drone.HttpsClient client
           <: #webhook @= webhookUrl
           <: #logger  @= logger
           <: nil
    runRIO env $ matchField
      (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
      (opts ^. #subcmd)
