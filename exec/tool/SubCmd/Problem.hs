{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Problem
  ( ProblemCmd (..)
  ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd

newtype ProblemCmd = ProblemCmd (Variant CmdField)

type CmdField =
  '[ "show"        >: ProblemCmdArg
   ]

instance Run ("show" >: ProblemCmdArg) where
  run' _ args =
    actForProblem showProblem args `catchAny` (logError . displayShow)
