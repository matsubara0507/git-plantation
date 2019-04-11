{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           RIO

import           Data.Extensible
import           Mix
import           Mix.Plugin.Logger as MixLogger

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "name" >: Text
   ]

main :: IO ()
main = Mix.run plugin $ do
  name <- asks (view #name)
  MixLogger.logDebug $ display ("This is debug: " <> name)
  MixLogger.logInfo  $ display ("This is info: "  <> name)
  MixLogger.logWarn  $ display ("This is warn: "  <> name)
  MixLogger.logError $ display ("This is error: " <> name)
  where
    plugin :: Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin (#handle @= stdout <: #verbose @= True <: nil)
       <: #name   <@=> pure "Hoge"
       <: nil
