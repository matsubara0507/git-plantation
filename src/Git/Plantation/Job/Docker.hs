{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

module Git.Plantation.Job.Docker where

import           RIO
import qualified RIO.Map                     as Map
import           RIO.Process
import qualified RIO.Text                    as Text

import           Data.Coerce                 (coerce)
import           Git.Plantation.Data         (Problem, Team)
import qualified Git.Plantation.Data.Job     as Job
import qualified Git.Plantation.Data.Problem
import qualified Git.Plantation.Data.Team
import qualified Git.Plantation.Data.Team    as Team

run ::
  (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => Job.Config
  -> Text
  -> m (ExitCode, LByteString, LByteString)
run config cmd = do
  logDebug $ fromString ("docker " ++ unwords args)
  proc "docker" args readProcess
  where
    args = ["run", "--rm", Text.unpack $ config ^. #image, "/bin/bash", "-c", Text.unpack cmd]

echo ::
  (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => Job.Config
  -> Problem
  -> Team
  -> m (ExitCode, LByteString, LByteString)
echo config problem team = run config cmd
  where
    cmd = "echo " <> coerce (team ^. #name) <> "/" <> coerce (problem ^. #name)

testScript :: (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => Job.Config
  -> Problem
  -> Team
  -> m (ExitCode, LByteString, LByteString)
testScript config problem team =
  case Team.repoGithubPath =<< Team.lookupRepo problem team of
    Nothing   ->
      pure (ExitFailure 1, "", "team repository is not found in config.")
    Just repo ->
      withModifyEnvVars (Map.insert "REPOSITORY" repo . Map.insert "PROBLEM" (coerce $ problem ^. #name)) $ run config ""
