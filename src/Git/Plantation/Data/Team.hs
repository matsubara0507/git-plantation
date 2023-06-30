{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Data.Team where

import           RIO
import qualified RIO.List                    as L

import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Binary                 (Binary)
import           Data.Coerce                 (coerce)
import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import           Git.Plantation.Data.Problem (Problem)
import qualified Git.Plantation.Data.Problem as Problem
import           Git.Plantation.Data.Repo    (Repo)
import qualified Git.Plantation.Data.Repo    as Repo
import           Git.Plantation.Data.User    (GitHubId, User)
import           Language.Haskell.TH.Syntax  (Lift)
import           Web.HttpApiData             (FromHttpApiData)

newtype Id = Id Text
  deriving newtype (Show, Eq, Ord, IsString, Binary, FromJSON, ToJSON, FromHttpApiData, Display, IsElmType)
  deriving (Lift)

newtype Name = Name Text
  deriving newtype (Show, Eq, Ord, IsString, Binary, FromJSON, ToJSON, FromHttpApiData, Display, IsElmType)
  deriving (Lift)

type Team = Record
  '[ "id"         >: Id
   , "name"       >: Name
   , "repos"      >: [Repo]
   , "member"     >: [User]
   , "org"        >: Maybe Text
   , "gh_teams"   >: [Text]
   , "channel_id" >: Text
   ]

instance IsElmType Team where
  compileElmType = compileElmRecordTypeWith "Team"

instance IsElmDefinition Team where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Team"

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

lookupRepoByProblemId :: Problem.Id -> Team -> Maybe Repo
lookupRepoByProblemId pid team =
  L.find (\repo -> repo ^. #problem == pid) (team ^. #repos)

lookupRepoByGithub :: Text -> Team -> Maybe Repo
lookupRepoByGithub github team =
  L.find (\repo -> repoGithubPath repo == Just github) (team ^. #repos)

lookupUser :: GitHubId -> Team -> Maybe User
lookupUser github team =
  L.find (\user -> github == user ^. #github) (team ^. #member)

repoGithubPath :: Repo -> Maybe Text
repoGithubPath repo = case (repo ^. #owner, repo ^. #org) of
  (Just owner, _) -> Just $ owner <> "/" <> coerce (repo ^. #name)
  (_, Just org)   -> Just $ org <> "/" <> coerce (repo ^. #name)
  _               -> Nothing

repoIsOrg :: Repo -> Bool
repoIsOrg repo = isJust $ repo ^. #org
