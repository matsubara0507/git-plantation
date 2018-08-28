{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Env where

import           RIO

import           Data.Extensible
import qualified Drone.Client          as Drone
import           Git.Plantation.Config
import qualified GitHub.Auth           as GitHub

type Plant = RIO Env

type Env = Record
  '[ "config" >: Config
   , "token"  >: GitHub.Token
   , "work"   >: FilePath
   , "client" >: Drone.HttpsClient
   , "logger" >: LogFunc
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)
