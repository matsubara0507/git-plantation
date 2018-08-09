{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Git.Plantation.Config where

import           RIO

import           Data.Extensible
import qualified Data.Yaml              as Y
import           Git.Plantation.Problem (Problem)
import           Git.Plantation.Team    (Team)

type Config = Record
  '[ "problems" >: [Problem]
   , "teams"    >: [Team]
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow
