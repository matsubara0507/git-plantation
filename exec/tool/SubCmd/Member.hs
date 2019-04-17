{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Member
  ( MemberCmd (..)
  ) where

import           Data.Extensible
import           Git.Plantation.Cmd.Member
import           Git.Plantation.Cmd.Run

newtype MemberCmd = MemberCmd (Variant CmdFields)

type CmdFields =
  '[ "invite" >: MemberCmdArg
   , "kick"   >: MemberCmdArg
   ]

instance Run ("invite" >: MemberCmdArg) where
  run' _ = actForMember inviteUserToRepo

instance Run ("kick" >: MemberCmdArg) where
  run' _ = actForMember kickUserFromRepo
