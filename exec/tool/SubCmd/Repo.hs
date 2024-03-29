{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Repo
  ( RepoCmd (..)
  ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd

newtype RepoCmd = RepoCmd (Variant CmdFields)

type CmdFields =
  '[ "new"                  >: (RepoCmdArg, NewRepoFlags)
   , "new_github"           >: RepoCmdArg
   , "init_github"          >: RepoCmdArg
   , "setup_default_branch" >: RepoCmdArg
   , "setup_webhook"        >: RepoCmdArg
   , "init_ci"              >: RepoCmdArg
   , "reset"                >: RepoCmdArg
   , "delete"               >: RepoCmdArg
   , "add_gh_team"          >: RepoCmdArg
   ]

instance Run ("new" >: (RepoCmdArg, NewRepoFlags)) where
  run' _ (args, flags) =
    actForRepo (createRepo flags) args `catchAny` (logError . displayShow)

instance Run ("new_github" >: RepoCmdArg) where
  run' _ args =
    actForRepo createRepoInGitHub args `catchAny` (logError . displayShow)

instance Run ("init_github" >: RepoCmdArg) where
  run' _ args =
    actForRepo initRepoInGitHub args `catchAny` (logError . displayShow)

instance Run ("setup_default_branch" >: RepoCmdArg) where
  run' _ args =
    actForRepo setupDefaultBranch args `catchAny` (logError . displayShow)

instance Run ("setup_webhook" >: RepoCmdArg) where
  run' _ args =
    actForRepo setupWebhook args `catchAny` (logError . displayShow)

instance Run ("init_ci" >: RepoCmdArg) where
  run' _ args =
    actForRepo initProblemCI args `catchAny` (logError . displayShow)

instance Run ("reset" >: RepoCmdArg) where
  run' _ args =
    actForRepo resetRepo args `catchAny` (logError . displayShow)

instance Run ("delete" >: RepoCmdArg) where
  run' _ args =
    actForRepo deleteRepo args `catchAny` (logError . displayShow)

instance Run ("add_gh_team" >: RepoCmdArg) where
  run' _ args =
    actForRepo addGitHubTeam args `catchAny` (logError . displayShow)
