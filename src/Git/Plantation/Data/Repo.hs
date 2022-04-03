{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Data.Repo where

import           RIO

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Binary                 (Binary)
import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import qualified Git.Plantation.Data.Problem as Problem
import           Language.Haskell.TH.Syntax  (Lift)
import           Web.HttpApiData             (FromHttpApiData)

newtype Name = Name Text
  deriving newtype (Show, Eq, Ord, IsString, Binary, FromJSON, ToJSON, FromHttpApiData, Display, IsElmType)
  deriving (Lift)

type Repo = Record
  '[ "name"    >: Name
   , "owner"   >: Maybe Text
   , "org"     >: Maybe Text
   , "problem" >: Problem.Id
   , "private" >: Bool
   , "only"    >: Maybe Text -- GitHub Org Team
   ]

instance IsElmType Repo where
  compileElmType = compileElmRecordTypeWith "Repo"

instance IsElmDefinition Repo where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Repo"
