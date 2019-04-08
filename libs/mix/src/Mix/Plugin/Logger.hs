{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Mix.Plugin.Logger
  ( LogFunc
  , MixLoggerConfig
  , buildPlugin
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import           RIO

import           Control.Monad.Cont
import           Data.Extensible
import           Mix.Plugin         (Plugin, toPlugin)

type MixLoggerConfig = Record
  '[ "handle"  >: Handle
   , "verbose" >: Bool
   ]

buildPlugin :: MonadUnliftIO m => MixLoggerConfig -> Plugin a m LogFunc
buildPlugin conf = do
  opts <- logOptionsHandle (conf ^. #handle) (conf ^. #verbose)
  toPlugin $ withLogFunc opts

instance Associate "logger" LogFunc xs => HasLogFunc (Record xs) where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)
