{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Cmd.Arg.Team where

import           RIO
import qualified RIO.List                        as L

import           Data.Aeson                      (ToJSON)
import           Data.Coerce                     (coerce)
import           Data.Extensible
import           Git.Plantation.Cmd.Arg.Internal
import qualified Git.Plantation.Data.Problem     as Problem
import           Git.Plantation.Data.Repo        (Repo)
import           Git.Plantation.Data.Team        (Team)
import qualified Git.Plantation.Data.Team        as Team
import           Git.Plantation.Data.User        (User)
import qualified Git.Plantation.Data.User        as User

instance IdArg Team.Id Team where
  findById idx = L.find (\t -> t ^. #id == coerce idx)
  toArgInfo idx
      = #type @= "Team"
     <: #id   @= coerce idx
     <: nil

newtype RepoId = RepoId Problem.Id
  deriving (Show, Read, Num, ToJSON) via Problem.Id

instance IdArg RepoId Repo where
  findById idx = L.find (\r -> r ^. #problem == coerce idx)
  toArgInfo idx
      = #type @= "Repo"
     <: #id   @= tshow idx
     <: nil

instance IdArg User.GitHubId User where
  findById idx = L.find (\u -> u ^. #github == coerce idx)
  toArgInfo idx
      = #type @= "User"
     <: #id   @= coerce idx
     <: nil

newtype GitHubTeamName = GitHubTeamName Text
  deriving (IsString, ToJSON) via Text

instance IdArg GitHubTeamName Text where
  findById idx = L.find (== coerce idx)
  toArgInfo idx
      = #type @= "GitHub Team"
      <: #id  @= coerce idx
      <: nil
