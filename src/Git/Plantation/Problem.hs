{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Problem where

import           RIO

import           Data.Extensible
import           Elm                 (ElmType (..))
import           Git.Plantation.Data (Branch)
import           Language.Elm

type Problem = Record
  '[ "problem_name" >: Text
   , "repo_name" >: Text
   , "difficulty" >: Int
   , "challenge_branches" >: [Branch]
   , "ci_branch" >: Branch
   ]

instance ElmType Problem where
  toElmType = toElmRecordType "Problem"
