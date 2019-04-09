{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO

import           Data.Extensible
import           Mix
import qualified Mix.Plugin.Logger as MixLogger
import qualified Mix.Plugin.Shell  as MixShell
import qualified Shh               as Shell


type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "work"   >: FilePath
   ]

main :: IO ()
main = Mix.run plugin $ MixShell.runShell $ do
  MixShell.pwd
  MixShell.git "--version"
  where
    plugin :: Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin (#handle @= stdout <: #verbose @= True <: nil)
       <: #work   <@=> pure ".temp"
       <: nil
