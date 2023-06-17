{-# LANGUAGE TemplateHaskell #-}

module Fixture
    ( config
    ) where

import           Data.Yaml.TH          (decodeFile)
import           Git.Plantation.Config (Config)
import           Instances.TH.Lift     ()
import           Language.Haskell.TH   (Code (..))

config :: Config
config = $$(Code (decodeFile "config/.git-plantation.yaml"))
