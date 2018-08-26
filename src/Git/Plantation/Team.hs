{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Team where

import           RIO

import           Data.Extensible
import           Elm                 (ElmType (..))
import           Git.Plantation.Data (User)
import           Language.Elm

type Team = Record
  '[ "name"   >: Text
   , "github" >: Text
   , "member" >: [User]
   ]

instance ElmType Team where
  toElmType = toElmRecordType "Team"
