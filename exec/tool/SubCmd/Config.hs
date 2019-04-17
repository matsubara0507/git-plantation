{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Config
  ( ConfigCmd (..)
  ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd
import           Git.Plantation.Config as Config

newtype ConfigCmd = ConfigCmd (Variant CmdFields)

type CmdFields =
  '[ "verify" >: ()
   ]

instance Run ("verify" >: ()) where
  run' _ _ = do
    conf <- asks (view #config)
    case Config.verify conf of
      Left err -> logError $ "invalid config: " <> display err
      Right _  -> logInfo "valid config"
