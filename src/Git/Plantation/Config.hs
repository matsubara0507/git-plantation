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
  '[ "problems" >: [Problem]
   , "teams"    >: [Team]
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow

instance ElmType Config where
  toElmType = toElmRecordType "Config"

verify :: Config -> Either Text Config
verify = pure
