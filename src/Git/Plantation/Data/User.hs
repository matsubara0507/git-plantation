{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Data.User where

import           RIO

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Binary                 (Binary)
import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import           Web.HttpApiData             (FromHttpApiData)

newtype Name = Name Text
  deriving newtype (Show, Eq, Ord, IsString, Binary, FromJSON, ToJSON, FromHttpApiData, Display, IsElmType)

newtype GitHubId = GitHubId Text
  deriving newtype (Show, Eq, Ord, IsString, Binary, FromJSON, ToJSON, FromHttpApiData, Display, IsElmType)


type User = Record
  '[ "name"   >: Name
   , "github" >: GitHubId
   ]

instance IsElmType User where
  compileElmType = compileElmRecordTypeWith "User"

instance IsElmDefinition User where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "User"
