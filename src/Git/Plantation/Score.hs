{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Score
  ( Score
  , Status
  , Link
  , mkScore
  , mkPlayerScore
  ) where

import           RIO
import qualified RIO.List                    as L
import qualified RIO.Map                     as Map

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import           Elm.Mapping
import           Git.Plantation.Data         (Job, Problem, Repo, Team, User,
                                              repoGithubPath)
import qualified Git.Plantation.Data.Problem as Problem
import qualified Git.Plantation.Data.Team    as Team
import qualified Git.Plantation.Data.User    as User

import           Orphans                     ()

type Score = Record
  '[ "team"  >: Team.Id
   , "point" >: Int
   , "stats" >: [Status]
   , "links" >: [Link]
   ]

type Status = Record
  '[ "problem_id"   >: Problem.Id
   , "correct"      >: Bool
   , "pending"      >: Bool
   , "corrected_at" >: Maybe Int64
   , "answerer"     >: Maybe User.GitHubId
   ]

type Link = Record
  '[ "problem_id" >: Problem.Id
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

mkScore :: [Problem] -> Team -> [Job] -> Score
mkScore problems team jobs
    = #team  @= team ^. #id
   <: #point @= calcPoint stats problems
   <: #stats @= Map.elems stats
   <: #links @= links
   <: nil
  where
    stats = Map.mapWithKey toStatus $ mkGroupedTeamJobs problems team jobs
    links = map toLink $ team ^. #repos

mkPlayerScore :: [Problem] -> Team -> User -> [Job] -> Score
mkPlayerScore problems team user jobs
    = #team  @= team ^. #id
   <: #point @= calcPoint stats problems
   <: #stats @= Map.elems stats
   <: #links @= links
   <: nil
  where
    teamJobs = mkGroupedTeamJobs problems team jobs
    playerJobs = filter (\job -> job ^. #author == user ^. #github) <$> teamJobs
    stats = Map.mapWithKey toStatus playerJobs
    links = map toLink $ team ^. #repos

mkGroupedTeamJobs :: [Problem] -> Team -> [Job] -> Map Problem.Id [Job]
mkGroupedTeamJobs problems team jobs =
  Map.fromListWith (++) $ fmap (\job -> (job ^. #problem, [job])) teamJobs
  where
    teamJobs = mkTeamJobs problems team jobs

mkTeamJobs :: [Problem] -> Team -> [Job] -> [Job]
mkTeamJobs problems team =
  filter (\job -> job ^. #team == team ^. #id && any (\p -> p ^. #id == job ^. #problem) problems)

toStatus :: Problem.Id -> [Job] -> Status
toStatus pid jobs
    = #problem_id   @= pid
   <: #correct      @= any (\job -> job ^. #success) jobs
   <: #pending      @= any (\job -> job ^. #queuing || job ^. #running) jobs
   <: #corrected_at @= L.minimumMaybe (view #created <$> filter (\job -> job ^. #success) jobs)
   <: #answerer     @= listToMaybe (map (view #author) jobs)
   <: nil

calcPoint :: Map Problem.Id Status -> [Problem] -> Int
calcPoint stats = sum . map (toPoint stats)

toPoint :: Map Problem.Id Status -> Problem -> Int
toPoint stats problem =
  case Map.lookup (problem ^. #id) stats of
    Just s | s ^. #correct -> problem ^. #difficulty
    _                      -> 0

toLink :: Repo -> Link
toLink repo
    = #problem_id @= repo ^. #problem
   <: #url        @= maybe "" ("https://github.com/" <>) (repoGithubPath repo)
   <: nil
