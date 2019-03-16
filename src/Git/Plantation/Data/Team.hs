{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Data.Team where

import           RIO
import qualified RIO.List                    as L

import           Data.Extensible
import           Git.Plantation.Data.Problem
import           Language.Elm

type Team = Record
  '[ "name"   >: Text
   , "id"     >: Text
   , "repos"  >: [Repo]
   , "member" >: [User]
   ]

instance ElmType Team where
  toElmType = toElmRecordType "Team"

type User = Record
  '[ "name"   >: Text
   , "github" >: Text
   ]

instance ElmType User where
  toElmType = toElmRecordType "User"

type Repo = Record
  '[ "problem" >: Text
   , "github"  >: Text
   ]

instance ElmType Repo where
  toElmType = toElmRecordType "Repo"

lookupRepo :: Problem -> Team -> Maybe Repo
lookupRepo problem = lookupRepo' (problem ^. #repo_name)

lookupRepo' :: Text -> Team -> Maybe Repo
lookupRepo' repoName team =
  L.find (\repo -> repoName == repo ^. #problem) (team ^. #repos)

lookupUser :: Text -> Team -> Maybe User
lookupUser github team =
  L.find (\user -> github == user ^. #github) (team ^. #member)
