{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Repo
  ( RepoCmd (..)
  ) where

import           RIO
import qualified RIO.List                 as L

import           Data.Extensible
import           Git.Plantation.Cmd.Repo
import           Git.Plantation.Cmd.Run
import           Git.Plantation.Data      (Problem, Team)
import qualified Git.Plantation.Data.Team as Team
import           Git.Plantation.Env

newtype RepoCmd = RepoCmd (Variant CmdFields)

type CmdFields =
  '[ "new"           >: NewRepoCmd
   , "new_github"    >: NewGitHubRepoCmd
   , "init_github"   >: InitGitHubRepoCmd
   , "setup_webhook" >: SetupWebhookCmd
   , "init_ci"       >: InitCICmd
   , "reset"         >: ResetRepoCmd
   , "delete"        >: DeleteRepoCmd
   ]

instance Run ("new" >: NewRepoCmd) where
  run' _ args = do
    conf <- asks (view #config)
    let team  = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
        flags = shrink args
    case (team, args ^. #repos) of
      (Nothing, _)       -> logError $ "team is not found: " <> display (args ^. #team)
      (Just team', [])   -> forM_ (conf ^. #problems) (tryAnyWithLogError . createRepo flags team')
      (Just team', pids) -> forM_ pids $ actByProblemId (createRepo flags) team'

instance Run ("new_github" >: NewGitHubRepoCmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    createRepoInGitHub info team problem

instance Run ("init_github" >: InitGitHubRepoCmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    initRepoInGitHub info team problem

instance Run ("setup_webhook" >: SetupWebhookCmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    setupWebhook info

instance Run ("init_ci" >: InitCICmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    initProblemCI info team problem

instance Run ("reset" >: ResetRepoCmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    resetRepo info team problem

runRepoCmd :: (Team -> Problem -> Plant ()) -> Record RepoCmdFields -> Plant ()
runRepoCmd act args = do
  conf <- asks (view #config)
  let team = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
  case (team, args ^. #repo) of
    (Nothing, _)      -> logError $ "team is not found: " <> display (args ^. #team)
    (Just team', pid) -> actByProblemId act team' pid

instance Run ("delete" >: DeleteRepoCmd) where
  run' _ args = do
    conf <- asks (view #config)
    let team = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
    case (team, args ^. #repos) of
      (Nothing, _)       -> logError $ "team is not found: " <> display (args ^. #team)
      (Just team', [])   -> forM_ (conf ^. #problems) (tryAnyWithLogError . deleteRepo team')
      (Just team', pids) -> forM_ pids $ actByProblemId deleteRepo team'
