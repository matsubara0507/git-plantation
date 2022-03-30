{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Cmd.Env where

import           RIO

import           Git.Plantation.Config
import qualified Mix.Plugin.Config     as MixConfig
import qualified Mix.Plugin.GitHub     as MixGitHub
import qualified Mix.Plugin.Shell      as MixShell

type CmdEnv env = (MixConfig.HasConfig Config env, MixGitHub.HasGitHubToken env, MixShell.HasWorkDir env, HasLogFunc env)
