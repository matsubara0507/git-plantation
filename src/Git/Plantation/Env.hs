{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Env where

import           RIO

import           Data.Extensible
import           Git.Plantation.Config

type Plant = RIO Env

type Env = Record
  '[ "config" >: Config
   , "logger" >: LogFunc
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)
