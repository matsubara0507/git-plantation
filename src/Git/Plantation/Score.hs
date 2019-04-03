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
   , "links" >: [Link]
   ]

type Status = Record
  '[ "problem" >: Text
   , "correct" >: Bool
   , "pending" >: Bool
   ]

type Link = Record
  '[ "problem_id" >: Int
   , "url"        >: Text
   ]

instance ElmType Score where
  toElmType = toElmRecordType "Score"

instance ElmType Status where
  toElmType = toElmRecordType "Status"

instance ElmType Link where
  toElmType = toElmRecordType "Link"
