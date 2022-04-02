{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

module Git.Plantation.Job.Docker where

import           RIO
import           RIO.Process
import qualified RIO.Text                    as Text

import           Data.Coerce                 (coerce)
import           Git.Plantation.Data         (Problem, Team)
import qualified Git.Plantation.Data.Job     as Job
import qualified Git.Plantation.Data.Problem
import qualified Git.Plantation.Data.Team

run ::
  (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => Job.Config
  -> Problem
  -> Team
  -> m (ExitCode, LByteString, LByteString)
run config problem team = do
  logDebug $ fromString ("docker " ++ unwords args)
  proc "docker" args readProcess
  where
    args = ["run", "--rm", Text.unpack $ config ^. #image, "/bin/bash", "-c", Text.unpack cmd] -- ToDo
    cmd  = "echo " <> coerce (team ^. #name) <> "/" <> coerce (problem ^. #name)
