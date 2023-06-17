{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Cmd.Env where

import           RIO

import           Data.Extensible
import           Git.Plantation.Config
import qualified Mix.Plugin.Config     as MixConfig
import qualified Mix.Plugin.GitHub     as MixGitHub
import qualified Mix.Plugin.Shell      as MixShell

class HasGitHubUser env where
  ghUserL :: Lens' env Text

instance Lookup xs "gh_user" Text => HasGitHubUser (Record xs) where
  ghUserL = lens (view #gh_user) (\x y -> x & #gh_user `set` y)

type CmdEnv env = (MixConfig.HasConfig Config env, MixGitHub.HasGitHubToken env, MixShell.HasWorkDir env, HasLogFunc env, HasGitHubUser env)
