{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}

module Git.Plantation.Cmd.Arg.Team where

import           RIO
import qualified RIO.List                        as L

import           Data.Coerce                     (coerce)
import           Data.Extensible
import           Git.Plantation.Cmd.Arg.Internal
import           Git.Plantation.Data.Team

newtype TeamId = TeamId Text
  deriving (IsString) via Text

instance IdArg TeamId Team where
  findById idx = L.find (\t -> t ^. #id == coerce idx)
  errMsg idx
      = #message @= "not found by config: Team"
     <: #id @= coerce idx
     <: nil

newtype RepoId = RepoId Int -- same problem id
  deriving (Read) via Int

instance IdArg RepoId Repo where
  findById idx = L.find (\r -> r ^. #problem == coerce idx)
  errMsg idx
      = #message @= "not found by config: Repo"
     <: #id @= tshow (coerce idx :: Int)
     <: nil

newtype UserId = UserId Text -- github id
  deriving (IsString) via Text

instance IdArg UserId User where
  findById idx = L.find (\u -> u ^. #github == coerce idx)
  errMsg idx
      = #message @= "not found by config: User"
     <: #id @= coerce idx
     <: nil
