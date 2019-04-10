{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.Slack where

import           RIO
import qualified RIO.List                  as L
import qualified RIO.Text                  as Text
import qualified RIO.Text.Lazy             as TL

import qualified Data.Aeson.Text           as Json
import           Data.Extensible
import qualified Git.Plantation.Cmd.Repo   as Cmd
import           Git.Plantation.Config     (Config)
import           Git.Plantation.Data       (Problem, Repo, Team)
import qualified Git.Plantation.Data.Slack as Slack
import qualified Git.Plantation.Data.Team  as Team
import           Git.Plantation.Env        (Plant)
import           Servant
import           UnliftIO.Concurrent       (forkIO)

type SlackAPI
     = "reset-repo" :> ReqBody '[FormUrlEncoded] Slack.SlashCmdData :> Post '[JSON] Slack.Message

slackAPI :: ServerT SlackAPI Plant
slackAPI
      = resetRepo

resetRepo :: Slack.SlashCmdData -> Plant Slack.Message
resetRepo postData = do
  logInfo $ fromString $ mconcat
    [ "[POST] /slack/reset-repo "
    , TL.unpack $ Json.encodeToLazyText (shrink postData :: Slack.DisplayLogData)
    ]
  slackConfig <- asks (view #slack)
  case verify slackConfig (view #reset_repo_cmd) postData of
    Left err -> returnMessage err
    Right _  -> resetRepo' (postData ^. #text)
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
      let success = [team ^. #name, " の ", problem ^. #name, " をリセットするね！"]
      _ <- forkIO $ Cmd.resetRepo repo team problem
      pure $ mconcat success

verify :: Maybe Slack.Config -> (Slack.Config -> Text) -> Slack.SlashCmdData -> Either Text ()
verify Nothing _ _ = Left "Undefined Token..."
verify (Just config) cmd postData
  | config ^. #token /= postData ^. #token                   = Left "Invalid token..."
  | postData ^. #team_id /= config ^. #team_id               = Left "Invalid team..."
  | postData ^. #channel_id `notElem` config ^. #channel_ids = Left "Invalid channel..."
  | cmd config /= postData ^. #command                       = Left "Invalid command..."
  | otherwise                                                = pure ()

findInfos :: Config -> Text -> Maybe (Team, Problem, Repo)
findInfos config txt = do
  (team, repo) <- L.find (\(_, r) -> Team.repoGithubPath r == Just ghPath) repos
  problem      <- L.find (\p -> p ^. #id == repo ^. #problem) $ config ^. #problems
  pure (team, problem, repo)
  where
    ghPath = Text.dropPrefix "https://github.com/" txt
    repos  = concatMap (\t -> (t,) <$> t ^. #repos) $ config ^. #teams
