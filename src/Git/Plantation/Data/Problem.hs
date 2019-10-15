{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Data.Problem where

import           RIO

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping

type Problem = Record
  '[ "id"                 >: Int
   , "name"               >: Text
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
