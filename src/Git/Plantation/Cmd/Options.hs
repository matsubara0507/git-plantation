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
import           Git.Plantation.Config     as Config
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
  '[ "verify"           >: ()
   , "new_repo"         >: NewRepoCmd
   , "new_github_repo"  >: NewGitHubRepoCmd
   , "init_github_repo" >: InitGitHubRepoCmd
   , "setup_webhook"    >: SetupWebhookCmd
   , "init_ci"          >: InitCICmd
   , "reset_repo"       >: ResetRepoCmd
   , "delete_repo"      >: DeleteRepoCmd
   , "invite_member"    >: InviteMemberCmd
   ]

instance Run ("verify" >: ()) where
  run' _ _ = do
    conf <- asks (view #config)
    case Config.verify conf of
      Left err -> logError $ "invalid config: " <> display err
      Right _  -> logInfo "valid config"

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

instance Run ("setup_webhook" >: SetupWebhookCmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    setupWebhook info

instance Run ("init_ci" >: InitCICmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    initProblemCI info team problem

instance Run ("reset_repo" >: ResetRepoCmd) where
  run' _ = runRepoCmd $ \team problem -> do
    info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
    resetRepo info team problem

runRepoCmd :: (Team -> Problem -> Plant ()) -> Record RepoCmdFields -> Plant ()
runRepoCmd act args = do
  conf <- asks (view #config)
  let team = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
  case (team, args ^. #repo) of
    (Nothing, _)       -> logError $ "team is not found: " <> display (args ^. #team)
    (Just team', name) -> actByRepoName act team' name

instance Run ("delete_repo" >: DeleteRepoCmd) where
  run' _ args = do
    conf <- asks (view #config)
    let team = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
    case (team, args ^. #repo) of
      (Nothing, _)            -> logError $ "team is not found: " <> display (args ^. #team)
      (Just team', Just name) -> actByRepoName deleteRepo team' name
      (Just team', _)         -> forM_ (conf ^. #problems) (tryAnyWithLogError . deleteRepo team')

instance Run ("invite_member" >: InviteMemberCmd) where
  run' _ args = do
    conf <- asks (view #config)
    let team = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
    case team of
      Nothing    -> logError $ "team is not found: " <> display (args ^. #team)
      Just team' -> inviteMember args team'
