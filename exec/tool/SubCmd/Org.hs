{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Org
  ( OrgCmd (..)
  ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Org
import           Git.Plantation.Cmd.Run

newtype OrgCmd = OrgCmd (Variant CmdField)

type CmdField =
  '[ "create_team" >: OrgCmdArg
   ]

instance Run ("create_team" >: OrgCmdArg) where
  run' _ args =
    actForGitHubTeam createGitHubTeam args `catchAny` (logError . displayShow)
