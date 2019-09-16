{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Score where

import           RIO

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import qualified Data.IntMap.Strict          as IntMap
import           Elm.Mapping
import           Git.Plantation.Data         (Problem, Repo, Team,
                                              repoGithubPath)
import           Git.Plantation.Store        (Store)
import qualified Git.Plantation.Store        as Store

type Score = Record
  '[ "team"  >: Text
   , "point" >: Int
   , "stats" >: [Status]
   , "links" >: [Link]
   ]

type Status = Record
  '[ "problem_id" >: Int
   , "correct"    >: Bool
   , "pending"    >: Bool
   ]

type Link = Record
  '[ "problem_id" >: Int
   , "url"        >: Text
   ]

instance IsElmType Score where
  compileElmType = compileElmRecordTypeWith "Score"

instance IsElmDefinition Score where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Score"

instance IsElmType Status where
  compileElmType = compileElmRecordTypeWith "Status"

instance IsElmDefinition Status where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Status"

instance IsElmType Link where
  compileElmType = compileElmRecordTypeWith "Link"

instance IsElmDefinition Link where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Link"

mkScore :: [Problem] -> Store -> Team -> Score
mkScore problems store team
    = #team  @= team ^. #id
   <: #point @= calcPoint stats problems
   <: #stats @= IntMap.elems stats
   <: #links @= links
   <: nil
  where
    isTeamBuild b = b ^. #source == team ^. #id
    builds = IntMap.filter (not . null) $ fmap (filter isTeamBuild) store
    stats = IntMap.mapWithKey toStatus builds
    links = map toLink $ team ^. #repos

toStatus :: Int -> [Store.Build] -> Status
toStatus idx builds
    = #problem_id @= idx
   <: #correct    @= any (\b -> b ^. #status == "success") builds
   <: #pending    @= any (\b -> b ^. #status == "running" || b ^. #status == "pending") builds
   <: nil

calcPoint :: IntMap Status -> [Problem] -> Int
calcPoint stats = sum . map (toPoint stats)

toPoint :: IntMap Status -> Problem -> Int
toPoint stats problem =
  case IntMap.lookup (problem ^. #id) stats of
    Just s | s ^. #correct -> problem ^. #difficulty
    _                      -> 0

toLink :: Repo -> Link
toLink repo
    = #problem_id @= repo ^. #problem
   <: #url        @= maybe "" ("https://github.com/" <>) (repoGithubPath repo)
   <: nil
