{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Member
  ( MemberCmd (..)
  ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Member
import           Git.Plantation.Cmd.Run

newtype MemberCmd = MemberCmd (Variant CmdFields)

type CmdFields =
  '[ "invite" >: MemberCmdArg
   , "kick"   >: MemberCmdArg
   ]

instance Run ("invite" >: MemberCmdArg) where
  run' _ args =
    actForMember inviteUserToRepo args `catchAny` (logError . displayShow)

instance Run ("kick" >: MemberCmdArg) where
  run' _ args =
    actForMember kickUserFromRepo args `catchAny` (logError . displayShow)
