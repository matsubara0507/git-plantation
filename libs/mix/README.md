# mix

mix features as plugin to `RIO`

## Example

use `MixLogger` (this is logger in rio):

```Haskell
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
   ]

main :: IO ()
main = Mix.run plugin $ do
  MixLogger.logDebug "This is debug"
  MixLogger.logInfo  "This is info"
  MixLogger.logWarn  "This is warn"
  MixLogger.logError "This is error"
  where
    plugin :: Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin (#handle @= stdout <: #verbose @= True <: nil)
       <: nil
```

exec

```sh
$ stack runghc sample/Main.hs
2019-04-08 14:30:11.904663: [debug] This is debug
@(libs/mix/sample/Main.hs:21:3)
2019-04-08 14:30:11.906410: [info] This is info
@(libs/mix/sample/Main.hs:22:3)
2019-04-08 14:30:11.906490: [warn] This is warn
@(libs/mix/sample/Main.hs:23:3)
2019-04-08 14:30:11.906557: [error] This is error
@(libs/mix/sample/Main.hs:24:3)
```
