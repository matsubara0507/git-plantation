{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Problem
  ( ProblemCmd (..)
  ) where

import           Data.Extensible
import           Git.Plantation.Cmd.Problem
import           Git.Plantation.Cmd.Run

newtype ProblemCmd = ProblemCmd (Variant CmdField)

type CmdField =
  '[ "show" >: ProblemCmdArg
   ]

instance Run ("show" >: ProblemCmdArg) where
  run' _ = actForProblem showProblem
