{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Data.Team where

import           RIO
import qualified RIO.List                    as L

import           Data.Aeson                  (ToJSON (..))
import           Data.Extensible
import           Git.Plantation.Data.Problem
import           Language.Elm

type Team = Record
  '[ "id"       >: Text
   , "name"     >: Text
   , "repos"    >: [Repo]
   , "member"   >: [User]
   , "org"      >: Maybe Text
   , "gh_teams" >: [Text]
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
  '[ "name"    >: Text
   , "owner"   >: Maybe Text
   , "org"     >: Maybe Text
   , "problem" >: Int
   , "private" >: Bool
   , "only"    >: Maybe Text -- GitHub Org Team
   ]

instance ElmType Repo where
  toElmType = toElmRecordType "Repo"

data MemberTarget
  = TargetRepo Repo
  | TargetOrg Text
  | TargetTeam Text Text
  deriving (Show, Eq)

toMemberTargetRecord :: MemberTarget -> Record '[ "type" >: Text, "target" >: Maybe Text ]
toMemberTargetRecord target = case target of
  TargetRepo repo     -> #type @= "repo" <: #target @= repoGithubPath repo <: nil
  TargetOrg org       -> #type @= "org"  <: #target @= Just org <: emptyRecord
  TargetTeam org team -> #type @= "team" <: #target @= Just (org <> ":" <> team) <: nil

lookupRepo :: Problem -> Team -> Maybe Repo
lookupRepo problem = lookupRepoByProblemId (problem ^. #id)

lookupRepoByProblemId :: Int -> Team -> Maybe Repo
lookupRepoByProblemId pid team =
  L.find (\repo -> repo ^. #problem == pid) (team ^. #repos)

lookupRepoByGithub :: Text -> Team -> Maybe Repo
lookupRepoByGithub github team =
  L.find (\repo -> repoGithubPath repo == Just github) (team ^. #repos)

lookupUser :: Text -> Team -> Maybe User
lookupUser github team =
  L.find (\user -> github == user ^. #github) (team ^. #member)

repoGithubPath :: Repo -> Maybe Text
repoGithubPath repo = case (repo ^. #owner, repo ^. #org) of
  (Just owner, _) -> Just $ owner <> "/" <> repo ^. #name
  (_, Just org)   -> Just $ org <> "/" <> repo ^. #name
  _               -> Nothing

repoIsOrg :: Repo -> Bool
repoIsOrg repo = isJust $ repo ^. #org
