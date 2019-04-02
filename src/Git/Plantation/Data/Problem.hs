{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Data.Problem where

import           RIO

import           Data.Extensible
import           Language.Elm

type Problem = Record
  '[ "id"                 >: Int
   , "name"               >: Text
   , "repo"               >: Text
   , "difficulty"         >: Int
   , "challenge_branches" >: [Branch]
   , "answer_branch"      >: Branch
   , "ci_branch"          >: Branch
   ]

instance ElmType Problem where
  toElmType = toElmRecordType "Problem"

type Branch = Text
