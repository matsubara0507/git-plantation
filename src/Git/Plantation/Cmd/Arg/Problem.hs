{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Cmd.Arg.Problem where

import           RIO
import qualified RIO.List                        as L

import           Data.Coerce                     (coerce)
import           Data.Extensible
import           Git.Plantation.Cmd.Arg.Internal
import           Git.Plantation.Data.Problem
import qualified Git.Plantation.Data.Problem     as Problem

instance IdArg Problem.Id Problem where
  findById idx = L.find (\p -> p ^. #id == coerce idx)
  toArgInfo idx
      = #type @= "Problem"
     <: #id   @= tshow (coerce idx :: Int)
     <: nil
