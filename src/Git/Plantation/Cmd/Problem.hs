{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Problem
  ( ProblemCmdArg
  , ProblemArg
  , actForProblem
  , showProblem
  , activateCI
  ) where

import           RIO

import           Data.Extensible
import qualified Drone.Endpoints.Repo        as Drone
import           Git.Plantation.Cmd.Arg
import           Git.Plantation.Cmd.Env      (CmdEnv)
import           Git.Plantation.Cmd.Repo     (splitRepoName)
import qualified Git.Plantation.Config       as Config
import           Git.Plantation.Data.Problem
import qualified Mix.Plugin.Drone            as MixDrone
import qualified Mix.Plugin.Logger.JSON      as Mix

type ProblemCmdArg = Record
  '[ "problems" >: [ProblemId]
   ]

type ProblemArg = Record
  '[ "problem" >: Problem
   ]

actForProblem :: CmdEnv env => (ProblemArg -> RIO env ()) -> ProblemCmdArg -> RIO env ()
actForProblem act args = do
  problems <- findProblems $ args ^. #problems
  mapM_ act $ hsequence $ #problem <@=> problems <: nil

findProblems :: CmdEnv env => [ProblemId] -> RIO env [Problem]
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
  , args ^. #problem ^. #name
  , "(⭐️ x", tshow $ args ^. #problem ^. #difficulty, ") at "
  , "https://github.com/", args ^. #problem ^. #repo
  ]

activateCI :: (MixDrone.HasDroneClient env, CmdEnv env) => ProblemArg -> RIO env ()
activateCI args = do
  Mix.logDebugR "activate ci for problem repository" (args ^. #problem)
  let (owner, repo) = splitRepoName $ args ^. #problem ^. #repo
  tryAny (MixDrone.fetch $ \c -> Drone.enableRepo c owner repo) >>= \case
    Left err -> logDebug (displayShow err) >> logError (fromString emessage)
    Right _  -> logInfo $ fromString $ "activated: " <> show (args ^. #problem ^. #id)
  where
    emessage = "can't activate: " <> show (args ^. #problem ^. #id)
