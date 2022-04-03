{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Env where

import           RIO

import           Data.Aeson                (ToJSON)
import qualified Data.Aeson.Text           as Json
import           Data.Extensible
import           Git.Plantation.Config
import           Git.Plantation.Data
import qualified Git.Plantation.Data.Slack as Slack
import qualified GitHub.Data               as GitHub
import qualified Mix.Plugin.GitHub         as GitHub
import           Mix.Plugin.Logger         ()
import qualified Mix.Plugin.Logger.JSON    as Mix
import qualified RIO.Text.Lazy             as TL
import qualified Servant.Auth.Server       as Auth

type Plant = RIO Env

type Env = Record
  '[ "config"    >: Config
   , "github"    >: GitHub.Token
   , "slack"     >: Slack.Config
   , "work"      >: FilePath
   , "webhook"   >: WebhookConfig
   , "jobserver" >: String -- URL for jobserver
   , "logger"    >: LogFunc
   , "oauth"     >: OAuthSettings
   ]

type WebhookConfig = [(Text, Text)]

mkWebhookConf :: Text -> Text -> WebhookConfig
mkWebhookConf url secret =
  [ ("url", url)
  , ("content_type", "json")
  , ("secret", secret)
  ]

class HasWebhookConfig env where
  webhookConfigL :: Lens' env WebhookConfig

instance Lookup xs "webhook" WebhookConfig => HasWebhookConfig (Record xs) where
  webhookConfigL = lens (view #webhook) (\x y -> x & #webhook `set` y)

askWebhookConfig :: HasWebhookConfig env => RIO env WebhookConfig
askWebhookConfig = view webhookConfigL

type OAuthSettings = Record
  '[ "client_id"     >: String
   , "client_secret" >: String
   , "cookie"        >: Auth.CookieSettings
   , "jwt"           >: Auth.JWTSettings
   ]

fromJustWithThrow :: Exception e => Maybe a -> e -> RIO env a
fromJustWithThrow (Just x) _ = pure x
fromJustWithThrow Nothing e  = throwIO e

mkLogMessage' ::
  Forall (KeyTargetAre KnownSymbol (Instance1 ToJSON Identity)) xs
  => Text -> Record xs -> String
mkLogMessage' message =
  TL.unpack . Json.encodeToLazyText . Mix.mkLogMessage message LevelError

data GitPlantException = UndefinedTeamProblem Team Problem
    | UndefinedProblem Int
    | CreateRepoError GitHub.Error Team Repo
    | DeleteRepoError GitHub.Error Repo
    | SetupWebhookError GitHub.Error Repo
    | AddRepoToGitHubTeamError GitHub.Error Text Text Repo
    | InviteUserError GitHub.Error User MemberTarget
    | KickUserError GitHub.Error User MemberTarget
    | CreateGitHubTeamError GitHub.Error Team Text
    | InvalidRepoConfig Repo
    deriving (Typeable)

instance Exception GitPlantException

instance Show GitPlantException where
  show = \case
    UndefinedTeamProblem team problem ->
      mkLogMessage'
        "undefined team repo"
        (#team @= team <: #problem @= problem <: nil)
    UndefinedProblem idx ->
      mkLogMessage'
        "undefined problem"
        (#id @= idx <: nil)
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
    AddRepoToGitHubTeamError _err org name repo ->
      mkLogMessage'
        "cant't add repository to GitHub team"
        (#org @= org <: #gh_team @= name <: #repo @= repo <: nil)
    InviteUserError _err user target ->
      mkLogMessage'
        "can't invite user to repository"
        (#user @= user <: #target @= toMemberTargetRecord target <: nil)
    KickUserError _err user target ->
      mkLogMessage'
        "can't kick user from repository"
        (#user @= user <: #target @= toMemberTargetRecord target <: nil)
    CreateGitHubTeamError _err team name ->
      mkLogMessage'
        "can't create GitHub team in org"
        (#team @= team <: #gh_team @= name <: nil)
    InvalidRepoConfig repo ->
      mkLogMessage'
        "invalid repo config"
        (#repo @= repo <: nil)
