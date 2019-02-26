{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Env where

import           RIO

import           Data.Extensible
import qualified Drone.Client          as Drone
import           Git.Plantation.Config
import           Git.Plantation.Data   (Problem, Team)
import qualified GitHub.Auth           as GitHub
import qualified GitHub.Data           as GitHub
import           Shelly                hiding (FilePath)

type Plant = RIO Env

type Env = Record
  '[ "config" >: Config
   , "token"  >: GitHub.Token
   , "work"   >: FilePath
   , "client" >: Drone.HttpsClient
   , "logger" >: LogFunc
   ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)

fromJustWithThrow :: Exception e => Maybe a -> e -> Plant a
fromJustWithThrow (Just x) _ = pure x
fromJustWithThrow Nothing  e = throwIO e

tryAnyWithLogError :: Plant () -> Plant ()
tryAnyWithLogError act = tryAny act >>= \case
  Left  e -> logError $ display e
  Right _ -> pure ()

shelly' :: Sh a -> Plant a
shelly' sh = do
  env <- ask
  shelly
    $ (log_stdout_with (runRIO env . logDebug . display))
    $ (log_stderr_with (runRIO env . logDebug . display))
    $ sh

mkLogMessage :: Text -> Record xs -> Record ("message" >: Text ': xs)
mkLogMessage message r = #message @= message <: r

data GitPlantException
  = UndefinedTeamProblem Team Problem
  | CreateRepoError GitHub.Error Team Problem
  deriving (Typeable)

instance Exception GitPlantException

instance Show GitPlantException where
  show = \case
    UndefinedTeamProblem team problem ->
      show $ mkLogMessage "undefined team repo" (#team @= team <: #problem @= problem <: nil)
    CreateRepoError err team problem ->
      show $ mkLogMessage "can't create repository" (#team @= team <: #problem @= problem <: #github_error @= err <: nil)
