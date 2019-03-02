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
import           System.Environment         (getEnv)

run :: MonadUnliftIO m => Options -> m ()
run opts = do
  config  <- readConfig (opts ^. #config)
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  token   <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  withLogFunc logOpts $ \logger -> do
    let client = #host @= "" <: #port @= Nothing <: #token @= "" <: nil
        env = #config @= config
           <: #token  @= token
           <: #work   @= opts ^. #work
           <: #client @= Drone.HttpsClient client
           <: #logger @= logger
           <: nil
    runRIO env $ matchField
      (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
      (opts ^. #subcmd)
