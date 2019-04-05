{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Config where

import           RIO

import           Data.Extensible
import qualified Data.Yaml           as Y
import           Elm                 (ElmType (..))
import           Git.Plantation.Data (Problem, Team)
import           Language.Elm

type Config = Record
  '[ "scoreboard" >: ScoreBoardConfig
   , "problems"   >: [Problem]
   , "teams"      >: [Team]
   ]

type ScoreBoardConfig = Record
  '[ "interval" >: Float
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow

instance ElmType Config where
  toElmType = toElmRecordType "Config"

instance ElmType ScoreBoardConfig where
  toElmType = toElmRecordType "ScoreBoardConfig"

verify :: Config -> Either Text Config
verify = pure
