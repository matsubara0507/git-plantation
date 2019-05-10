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
import           Git.Plantation.Cmd.Repo     (splitRepoName)
import           Git.Plantation.Data.Problem
import           Git.Plantation.Env
import qualified Mix.Plugin.Drone            as MixDrone
import qualified Mix.Plugin.Logger.JSON      as Mix

type ProblemCmdArg = Record
  '[ "problems" >: [ProblemId]
   ]

type ProblemArg = Record
  '[ "problem" >: Problem
   ]

actForProblem :: (ProblemArg -> Plant ()) -> ProblemCmdArg -> Plant ()
actForProblem act args = do
  problems <- findProblems $ args ^. #problems
  mapM_ act $ hsequence $ #problem <@=> problems <: nil

findProblems :: [ProblemId] -> Plant [Problem]
findProblems []  = asks (view #problems . view #config)
findProblems ids = fmap catMaybes . forM ids $ \idx ->
  findByIdWith (view #problems) idx >>= \case
    Nothing -> Mix.logErrorR "not found by config" (toArgInfo idx) >> pure Nothing
    Just r  -> pure (Just r)

showProblem :: ProblemArg -> Plant ()
showProblem args = logInfo $ display $ mconcat
  [ "- ", tshow $ args ^. #problem ^. #id, ": "
  , args ^. #problem ^. #name
  , "(⭐️ x", tshow $ args ^. #problem ^. #difficulty, ") at "
  , "https://github.com/", args ^. #problem ^. #repo
  ]

activateCI :: ProblemArg -> Plant ()
activateCI args = do
  Mix.logDebugR "activate ci for problem repository" (args ^. #problem)
  let (owner, repo) = splitRepoName $ args ^. #problem ^. #repo
  tryAny (MixDrone.fetch $ \c -> Drone.enableRepo c owner repo) >>= \case
    Left err -> logDebug (displayShow err) >> logError (fromString emessage)
    Right _  -> logInfo $ fromString $ "activated: " <> show (args ^. #problem ^. #id)
  where
    emessage = "can't activate: " <> show (args ^. #problem ^. #id)
