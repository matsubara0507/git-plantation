{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

module Git.Plantation.Job.Docker where

import           RIO
import           RIO.Process

import qualified Git.Plantation.Data.Job as Job

run ::
  (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => Job.Config
  -> m (ExitCode, LByteString, LByteString)
run config =
  proc "docker" args readProcess
  where
    args = ["run", "--rm", config ^. #image] <> catMaybes [config ^. #command]
