{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Mix.Plugin.Config
  ( HasConfig (..)
  , buildPlugin
  ) where

import           RIO

import           Data.Extensible
import           Mix.Plugin      (Plugin)

buildPlugin :: MonadIO m => c -> Plugin a m c
buildPlugin = pure

class HasConfig c env where
  configL :: Lens' env c

instance Associate "config" c xs => HasConfig c (Record xs) where
  configL = lens (view #config) (\x y -> x & #config `set` y)
