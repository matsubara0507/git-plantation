{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}

module Git.Plantation.Cmd.Arg.Problem where

import           RIO
import qualified RIO.List                        as L

import           Data.Aeson                      (ToJSON)
import           Data.Coerce                     (coerce)
import           Data.Extensible
import           Git.Plantation.Cmd.Arg.Internal
import           Git.Plantation.Data.Problem

newtype ProblemId = ProblemId Int
  deriving (Read, ToJSON) via Int

instance IdArg ProblemId Problem where
  findById idx = L.find (\p -> p ^. #id == coerce idx)
  toArgInfo idx
      = #type @= "Problem"
     <: #id   @= tshow (coerce idx :: Int)
     <: nil
