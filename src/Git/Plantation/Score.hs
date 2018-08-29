{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Score where

import           RIO

import           Data.Extensible
import           Elm             (ElmType (..))
import           Language.Elm

type Score = Record
  '[ "team"  >: Text
   , "point" >: Int
   , "stats" >: [Status]
   ]

type Status = Record
  '[ "problem" >: Text
   , "correct" >: Bool
   ]

instance ElmType Score where
  toElmType = toElmRecordType "Score"

instance ElmType Status where
  toElmType = toElmRecordType "Status"
