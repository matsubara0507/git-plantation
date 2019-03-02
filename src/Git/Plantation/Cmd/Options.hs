{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Cmd.Options where

import           RIO
import qualified RIO.List                  as L

import           Data.Extensible
import           Git.Plantation.Cmd.Member
import           Git.Plantation.Cmd.Repo
import           Git.Plantation.Cmd.Run
import           Git.Plantation.Data       (Problem, Team)
import qualified Git.Plantation.Data.Team  as Team
import           Git.Plantation.Env

type Options = Record
  '[ "verbose" >: Bool
   , "config"  >: FilePath
   , "work"    >: FilePath
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "new_repo"         >: NewRepoCmd
   , "new_github_repo"  >: NewGitHubRepoCmd
   , "init_github_repo" >: InitGitHubRepoCmd
   , "init_ci"          >: InitCICmd
   , "invite_member"    >: InviteMemberCmd
   ]

instance Run ("new_repo" >: NewRepoCmd) where
  run' _ args = do
    conf <- asks (view #config)
    let team = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
    case (team, args ^. #repo) of
      (Nothing, _)            -> logError $ "team is not found: " <> display (args ^. #team)
      (Just team', Just name) -> actByRepoName createRepo team' name
      (Just team', _)         -> forM_ (conf ^. #problems) (tryAnyWithLogError . createRepo team')

instance Run ("new_github_repo" >: NewGitHubRepoCmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    createRepoInGitHub info team problem

instance Run ("init_github_repo" >: InitGitHubRepoCmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    initRepoInGitHub info team problem

instance Run ("init_ci" >: InitCICmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    initProblemCI info team problem

runRepoCmd :: (Team -> Problem -> Plant ()) -> Record RepoCmdFields -> Plant ()
runRepoCmd act args = do
  conf <- asks (view #config)
  let team = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
  case (team, args ^. #repo) of
    (Nothing, _)       -> logError $ "team is not found: " <> display (args ^. #team)
    (Just team', name) -> actByRepoName act team' name

instance Run ("invite_member" >: InviteMemberCmd) where
  run' _ args = do
    conf <- asks (view #config)
    let team = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
    case team of
      Nothing    -> logError $ "team is not found: " <> display (args ^. #team)
      Just team' -> inviteMember args team'
