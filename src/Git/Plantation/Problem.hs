{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Git.Plantation.Problem where

import           RIO

import           Data.Extensible
import           Git.Plantation.Data (Branch)

type Problem = Record
  '[ "problem_name" >: Text
   , "repo_name" >: Text
   , "difficulty" >: Int
   , "challenge_branches" >: [Branch]
   , "ci_branch" >: Branch
   ]
