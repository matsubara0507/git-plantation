{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.Slack where

import           RIO
import qualified RIO.List                  as L
import qualified RIO.Text                  as Text

import           Data.Extensible
import qualified Git.Plantation.Cmd.Repo   as Cmd
import           Git.Plantation.Config     (Config)
import           Git.Plantation.Data       (Problem, Repo, Team)
import qualified Git.Plantation.Data.Slack as Slack
import qualified Git.Plantation.Data.Team  as Team
import           Git.Plantation.Env        (Plant)
import           Servant

type SlackAPI
     = "reset-repo" :> ReqBody '[FormUrlEncoded] Slack.OutgoingWebhookData :> Post '[JSON] Slack.Message

slackAPI :: ServerT SlackAPI Plant
slackAPI
      = resetRepo

resetRepo :: Slack.OutgoingWebhookData -> Plant Slack.Message
resetRepo postData = do
  logInfo $ display $ mconcat
    [ "[POST] /slack/reset-repo"
    , " text='", postData ^. #text, "'"
    , " trigger_word='", postData ^. #trigger_word, "'"
    ]
  slackConfig <- asks (view #slack)
  case verify slackConfig postData of
    Left err -> returnMessage err
    Right _  -> resetRepo' $ toMessageBody postData
  where
    returnMessage :: MonadIO m => Text -> m Slack.Message
    returnMessage txt = pure $ #text @= txt <: nil

    resetRepo' :: Text -> Plant Slack.Message
    resetRepo' text = do
      config <- asks (view #config)
      case findInfos config text of
        Nothing   -> returnMessage "リポジトリが見つからなーい"
        Just info -> returnMessage =<< reset info

    reset :: (Team, Problem, Repo) -> Plant Text
    reset (team, problem, repo) = do
      let success = [team ^. #name, " の ", problem ^. #name, " をリセットしたよ！"]
      tryAny (Cmd.resetRepo repo team problem) >>= \case
        Left  e -> logError (display e) >> pure "うーん、なんか失敗しました。"
        Right _ -> pure $ mconcat success

verify :: Maybe Slack.Config -> Slack.OutgoingWebhookData -> Either Text ()
verify Nothing _ = Left "Undefined Token..."
verify (Just config) postData
  | config ^. #token /= postData ^. #token = Left "Invalid Token..."
  | otherwise                              = pure ()

toMessageBody :: Slack.OutgoingWebhookData -> Text
toMessageBody postData =
  Text.strip $ fromMaybe "" $ Text.stripPrefix (postData ^. #trigger_word) (postData ^. #text)

findInfos :: Config -> Text -> Maybe (Team, Problem, Repo)
findInfos config txt = do
  (team, repo) <- L.find (\(_, r) -> Team.repoGithubPath r == Just ghPath) repos
  problem      <- L.find (\p -> p ^. #id == repo ^. #problem) $ config ^. #problems
  pure (team, problem, repo)
  where
    ghPath = Text.dropSuffix ">" $ Text.dropPrefix "<https://github.com/" txt
    repos  = concatMap (\t -> (t,) <$> t ^. #repos) $ config ^. #teams
