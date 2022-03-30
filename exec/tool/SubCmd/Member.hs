{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SubCmd.Member
  ( MemberCmd (..)
  ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd

newtype MemberCmd = MemberCmd (Variant CmdFields)

type CmdFields =
  '[ "invite" >: MemberCmdArg
   , "kick"   >: MemberCmdArg
   ]

instance Run ("invite" >: MemberCmdArg) where
  run' _ args = handleAny (logError . displayShow) $ if
    | isJust (args ^. #gh_team) -> actForMemberWithGitHubTeam inviteUserToGitHubOrgTeam args
    | args ^. #org              -> actForMemberWithOrg inviteUserToGitHubOrg args
    | otherwise                 -> actForMember inviteUserToRepo args

instance Run ("kick" >: MemberCmdArg) where
  run' _ args = handleAny (logError . displayShow) $ if
    | isJust (args ^. #gh_team) -> actForMemberWithGitHubTeam kickUserFromGitHubOrgTeam args
    | args ^. #org              -> actForMemberWithOrg kickUserFromGitHubOrg args
    | otherwise                 -> actForMember kickUserFromRepo args
