{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Cmd.Run where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Options
import           Git.Plantation.Config      (readConfig)
import           Git.Plantation.Env

run :: (MonadUnliftIO m, MonadThrow m) => Options -> m ()
run opts = do
  config  <- readConfig (opts ^. #config)
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #config @= config
           <: #logger @= logger
           <: nil
    runRIO env $ matchField
      (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
      (opts ^. #subcmd)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."

class Run kv where
  run' :: proxy kv -> AssocValue kv -> Plant ()

instance Run ("repo" >: Text) where
  run' _ _ = showNotImpl
