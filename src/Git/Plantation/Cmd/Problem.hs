{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Problem
  ( ProblemCmdArg
  , ProblemArg
  , actForProblem
  , showProblem
  ) where

import           RIO

import           Data.Coerce                 (coerce)
import           Data.Extensible
import           Git.Plantation.Cmd.Arg
import           Git.Plantation.Cmd.Env      (CmdEnv)
import qualified Git.Plantation.Config       as Config
import           Git.Plantation.Data.Problem
import qualified Git.Plantation.Data.Problem as Problem
import qualified Mix.Plugin.Logger.JSON      as Mix

type ProblemCmdArg = Record
  '[ "problems" >: [Problem.Id]
   ]

type ProblemArg = Record
  '[ "problem" >: Problem
   ]

actForProblem :: CmdEnv env => (ProblemArg -> RIO env ()) -> ProblemCmdArg -> RIO env ()
actForProblem act args = do
  problems <- findProblems $ args ^. #problems
  mapM_ act $ hsequence $ #problem <@=> problems <: nil

findProblems :: CmdEnv env => [Problem.Id] -> RIO env [Problem]
findProblems []  = do
  config <- Config.askConfig
  pure $ config ^. #problems
findProblems ids = fmap catMaybes . forM ids $ \idx ->
  findByIdWith (view #problems) idx >>= \case
    Nothing -> Mix.logErrorR "not found by config" (toArgInfo idx) >> pure Nothing
    Just r  -> pure (Just r)

showProblem :: CmdEnv env => ProblemArg -> RIO env ()
showProblem args = logInfo $ display $ mconcat
  [ "- ", tshow $ args ^. #problem ^. #id, ": "
  , coerce $ args ^. #problem ^. #name
  , "(⭐️ x", tshow $ args ^. #problem ^. #difficulty, ") at "
  , "https://github.com/", args ^. #problem ^. #repo
  ]
