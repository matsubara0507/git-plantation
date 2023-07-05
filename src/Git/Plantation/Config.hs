{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Config where

import           RIO

import           Data.Extensible
import           Data.Extensible.Elm.Mapping
import qualified Data.Yaml                   as Y
import           Elm.Mapping
import           Git.Plantation.Data         (GitHubId, Problem, Team, User)
import qualified Mix.Plugin.Config           as MixConfig
import           Orphans                     ()

type Config = Record
  '[ "scoreboard" >: ScoreBoardConfig
   , "problems"   >: [Problem]
   , "teams"      >: [Team]
   , "owners"     >: [User]
   , "image"      >: Text -- ToDo
   ]

type ScoreBoardConfig = Record
  '[ "interval"   >: Float
   , "start_time" >: Maybe Int64  -- unix time
   , "end_time"   >: Maybe Int64  -- unix time
   , "zone"       >: Maybe Text
   , "scoring"    >: Maybe Bool
   , "resetable"  >: Maybe Bool
   ]

askConfig :: MixConfig.HasConfig Config env => RIO env Config
askConfig = MixConfig.askConfig

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow

mkAuthnWhitelist :: Config -> [GitHubId]
mkAuthnWhitelist config = map (view #github) (config ^. #owners <> players)
  where
    players = concatMap (view #member) $ config ^. #teams

instance IsElmType Config where
  compileElmType = compileElmRecordTypeWith "Config"

instance IsElmDefinition Config where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "Config"

instance IsElmType ScoreBoardConfig where
  compileElmType = compileElmRecordTypeWith "ScoreBoardConfig"

instance IsElmDefinition ScoreBoardConfig where
  compileElmDef = ETypeAlias . compileElmRecordAliasWith "ScoreBoardConfig"

verify :: Config -> Either Text Config
verify = pure
