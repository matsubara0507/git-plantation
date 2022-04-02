{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd
  ( module X
  , SubCmd
  ) where

import           SubCmd.Config      as X (ConfigCmd (..))
import           SubCmd.Member      as X (MemberCmd (..))
import           SubCmd.Org         as X (OrgCmd (..))
import           SubCmd.Problem     as X (ProblemCmd (..))
import           SubCmd.Repo        as X (RepoCmd (..))

import           Data.Extensible
import           Git.Plantation.Cmd


type SubCmd = Variant
  '[ "config"  >: ConfigCmd
   , "repo"    >: RepoCmd
   , "member"  >: MemberCmd
   , "problem" >: ProblemCmd
   , "org"     >: OrgCmd
   ]

instance Run ("config" >: ConfigCmd) where
  run' _ (ConfigCmd cmd) = run cmd

instance Run ("repo" >: RepoCmd) where
  run' _ (RepoCmd cmd) = run cmd

instance Run ("member" >: MemberCmd) where
  run' _ (MemberCmd cmd) = run cmd

instance Run ("problem" >: ProblemCmd) where
  run' _ (ProblemCmd cmd) = run cmd

instance Run ("org" >: OrgCmd) where
  run' _ (OrgCmd cmd) = run cmd
