{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Problem
  ( ProblemCmd (..)
  ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Problem
import           Git.Plantation.Cmd.Run

newtype ProblemCmd = ProblemCmd (Variant CmdField)

type CmdField =
  '[ "show"        >: ProblemCmdArg
   , "activate_ci" >: ProblemCmdArg
   ]

instance Run ("show" >: ProblemCmdArg) where
  run' _ args =
    actForProblem showProblem args `catchAny` (logError . displayShow)

instance Run ("activate_ci" >: ProblemCmdArg) where
  run' _ args =
    actForProblem activateCI args `catchAny` (logError . displayShow)
