{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Env where

import           RIO

import           Data.Extensible
import           Git.Plantation.Config
import qualified GitHub.Auth           as GitHub

type Plant = RIO Env

type Env = Record
  '[ "config" >: Config
   , "token"  >: GitHub.Token
   , "work"   >: FilePath
   , "logger" >: LogFunc
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)
