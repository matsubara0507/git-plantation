{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Data.Problem where

import           RIO

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Binary                 (Binary)
import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import           Web.HttpApiData             (FromHttpApiData)

newtype Id = Id Int
  deriving newtype (Show, Read, Eq, Ord, Num, Binary, FromJSON, ToJSON, FromHttpApiData, Display, IsElmType)

newtype Name = Name Text
  deriving newtype (Show, Eq, Ord, IsString, Binary, FromJSON, ToJSON, FromHttpApiData, Display, IsElmType)

type Problem = Record
  '[ "id"                 >: Id
   , "name"               >: Name
   , "repo"               >: Text
   , "difficulty"         >: Int
   , "challenge_branches" >: [Branch]
   , "answer_branch"      >: Branch
   , "ci_branch"          >: Branch
   , "default_branch"     >: Branch
   ]

instance IsElmType Problem where
  compileElmType = compileElmRecordTypeWith "Problem"

instance IsElmDefinition Problem where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Problem"

type Branch = Text
