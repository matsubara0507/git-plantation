{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Env where

import           RIO

import           Data.Aeson            (ToJSON)
import qualified Data.Aeson.Text       as Json
import           Data.Extensible
import qualified Drone.Client          as Drone
import           Git.Plantation.Config
import           Git.Plantation.Data   (Problem, Repo, Team, User)
import qualified GitHub.Auth           as GitHub
import qualified GitHub.Data           as GitHub
import qualified RIO.Text.Lazy         as TL
import           Shelly                hiding (FilePath)

type Plant = RIO Env

type Env = Record
  '[ "config"  >: Config
   , "token"   >: GitHub.Token
   , "work"    >: FilePath
   , "client"  >: Drone.HttpsClient
   , "webhook" >: WebhookConfig
   , "logger"  >: LogFunc
   ]

type WebhookConfig = [(Text, Text)]

mkWebhookConf :: Text -> Text -> WebhookConfig
mkWebhookConf url secret =
  [ ("url", url)
  , ("content_type", "json")
  , ("secret", secret)
  ]

instance HasLogFunc Env where
  logFuncL = lens (view #logger) (\x y -> x & #logger `set` y)

fromJustWithThrow :: Exception e => Maybe a -> e -> Plant a
fromJustWithThrow (Just x) _ = pure x
fromJustWithThrow Nothing  e = throwIO e

tryAnyWithLogError :: Plant a -> Plant ()
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

mkLogMessage :: Text -> Record xs -> Record ("error_message" >: Text ': xs)
mkLogMessage message r = #error_message @= message <: r

mkLogMessage' ::
  Forall (KeyValue KnownSymbol (Instance1 ToJSON Identity)) xs
  => Text -> Record xs -> String
mkLogMessage' message =
  TL.unpack . Json.encodeToLazyText . mkLogMessage message

data GitPlantException
  = UndefinedTeamProblem Team Problem
  | CreateRepoError GitHub.Error Team Problem
  | DeleteRepoError GitHub.Error Repo
  | SetupWebhookError GitHub.Error Repo
  | InviteUserError GitHub.Error User Repo
  | InvalidRepoConfig Repo
  deriving (Typeable)

instance Exception GitPlantException

instance Show GitPlantException where
  show = \case
    UndefinedTeamProblem team problem ->
      mkLogMessage'
        "undefined team repo"
        (#team @= team <: #problem @= problem <: nil)
    CreateRepoError _err team problem ->
      mkLogMessage'
        "can't create repository"
        (#team @= team <: #problem @= problem <: nil)
    DeleteRepoError _err repo ->
      mkLogMessage'
        "can't delete repository"
        (#repo @= repo <: nil)
    SetupWebhookError _err repo ->
      mkLogMessage'
        "can't setup github webhook"
        (#repo @= repo <: nil)
    InviteUserError _err user repo ->
      mkLogMessage'
        "can't invite user to repository"
        (#user @= user <: #repo @= repo <: nil)
    InvalidRepoConfig repo ->
      mkLogMessage'
        "invalid repo config"
        (#repo @= repo <: nil)
