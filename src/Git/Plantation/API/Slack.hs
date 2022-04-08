{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}

module Git.Plantation.API.Slack
  ( SlackAPIEnv
  , SlashCmdAPI
  , slashCmdApi
  ) where

import           RIO
import qualified RIO.ByteString.Lazy         as BL
import qualified RIO.List                    as L
import qualified RIO.Text                    as Text
import qualified RIO.Text.Lazy               as TL

import qualified Data.Aeson.Text             as Json
import           Data.Coerce                 (coerce)
import           Data.Extensible
import qualified Git.Plantation.Cmd          as Cmd
import           Git.Plantation.Config       (Config)
import           Git.Plantation.Data         (Problem, Repo, Team)
import qualified Git.Plantation.Data.Problem as Problem
import           Git.Plantation.Data.Slack   (askSlashCmdConfig)
import qualified Git.Plantation.Data.Slack   as Slack
import qualified Git.Plantation.Data.Team    as Team
import qualified Mix.Plugin.Config           as Mix
import           Servant
import           UnliftIO.Concurrent         (forkIO)

type SlackAPIEnv env = (Cmd.CmdEnv env, Mix.HasConfig Config env, Slack.HasSlackSlashCmdConfig env, HasLogFunc env)

type SlashCmdAPI
    = ReqBody '[Slack.SlashCmd] (LByteString, Slack.SlashCmdData)
      :> Header "X-Slack-Request-Timestamp" Slack.RequestTimestamp
      :> Header "X-Slack-Signature" Slack.SignatureHeader
      :> Post '[JSON] NoContent

slashCmdApi :: SlackAPIEnv env => ServerT SlashCmdAPI (RIO env)
slashCmdApi = verifiedRequest resetRepo

verifiedRequest
  :: SlackAPIEnv env
  => (Slack.SlashCmdData -> RIO env a)
  -> (LByteString, Slack.SlashCmdData)
  -> Maybe Slack.RequestTimestamp
  -> Maybe Slack.SignatureHeader
  -> RIO env a
verifiedRequest next (body, postData) (Just ts) (Just sign) = do
  secret <- view #signing_secret <$> askSlashCmdConfig
  let digest = Slack.encodeSignature secret ts (BL.toStrict body)
  if Just digest == Slack.convertSignatureHeader sign then
    next postData
  else
    throwM err401
verifiedRequest _ _ _ _ =
  throwM err401

resetRepo :: SlackAPIEnv env => Slack.SlashCmdData -> RIO env NoContent
resetRepo postData = do
  logInfo $ fromString $ mconcat
    [ "[POST] /slack/reset-repo "
    , TL.unpack $ Json.encodeToLazyText (shrink postData :: Slack.DisplayLogData)
    ]
  _ <- forkIO $ Slack.verifySlashCmd postData >>= \case
    Left err -> logError $ display err
    Right _  -> (logError . display) `handleIO` resetRepo' (postData ^. #text)
  pure NoContent
  where
    returnMessage :: MonadIO m => Text -> m Slack.Message
    returnMessage txt = pure $ #text @= txt <: nil

    resetRepo' :: SlackAPIEnv env => Text -> RIO env ()
    resetRepo' text = do
      logDebug "reset-cmd: find repository by message"
      Slack.respondMessage postData $ Slack.mkMessage "リポジトリをリセットするね！"
      config  <- Mix.askConfig
      message <- case findInfos config text of
        Nothing   -> returnMessage "うーん、リポジトリが見つからなーい..."
        Just info -> returnMessage =<< reset info
      logDebug $ display ("reset-cmd: response: " <> message ^. #text)
      slackConfig <- Slack.askSlashCmdConfig
      case slackConfig ^. #webhook of
        Just url -> Slack.sendWebhook url message
        Nothing  -> Slack.respondMessage postData message

    reset :: SlackAPIEnv env => (Team, Problem, Repo) -> RIO env Text
    reset (team, problem, repo) = do
      let success = [coerce $ team ^. #name, " の ", coerce $ problem ^. #name, " をリセットしました！"]
      tryIO (Cmd.resetRepo $ #repo @= repo <: #team @= team <: nil) >>= \case
        Left err -> logError (display err) >> pure "うーん、なんか失敗したみたい..."
        Right _  -> pure $ mconcat success

findInfos :: Config -> Text -> Maybe (Team, Problem, Repo)
findInfos config txt = do
  (team, repo) <- L.find (\(_, r) -> Team.repoGithubPath r == Just ghPath) repos
  problem      <- L.find (\p -> p ^. #id == repo ^. #problem) $ config ^. #problems
  pure (team, problem, repo)
  where
    ghPath = Text.dropPrefix "https://github.com/" txt
    repos  = concatMap (\t -> (t,) <$> t ^. #repos) $ config ^. #teams
