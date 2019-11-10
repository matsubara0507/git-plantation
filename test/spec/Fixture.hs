{-# LANGUAGE TemplateHaskell #-}

module Fixture
    ( config
    ) where

import           Data.Yaml.TH          (decodeFile)
import           Git.Plantation.Config (Config)
import           Instances.TH.Lift     ()

config :: Config
config = $$(decodeFile "config/.git-plantation.yaml")
