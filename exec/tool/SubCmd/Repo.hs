{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Repo
  ( RepoCmd (..)
  ) where

import           Data.Extensible
import           Git.Plantation.Cmd.Repo
import           Git.Plantation.Cmd.Run

newtype RepoCmd = RepoCmd (Variant CmdFields)

type CmdFields =
  '[ "new"           >: (RepoCmdArg, NewRepoFlags)
   , "new_github"    >: RepoCmdArg
   , "init_github"   >: RepoCmdArg
   , "setup_webhook" >: RepoCmdArg
   , "init_ci"       >: RepoCmdArg
   , "reset"         >: RepoCmdArg
   , "delete"        >: RepoCmdArg
   ]

instance Run ("new" >: (RepoCmdArg, NewRepoFlags)) where
  run' _ (args, flags) = actForRepo (createRepo flags) args

instance Run ("new_github" >: RepoCmdArg) where
  run' _ = actForRepo createRepoInGitHub

instance Run ("init_github" >: RepoCmdArg) where
  run' _ = actForRepo initRepoInGitHub

instance Run ("setup_webhook" >: RepoCmdArg) where
  run' _ = actForRepo setupWebhook

instance Run ("init_ci" >: RepoCmdArg) where
  run' _ = actForRepo initProblemCI

instance Run ("reset" >: RepoCmdArg) where
  run' _ = actForRepo resetRepo

instance Run ("delete" >: RepoCmdArg) where
  run' _ = actForRepo deleteRepo
