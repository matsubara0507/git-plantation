{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE TypeOperators      #-}

module Git.Plantation.API.CRUD where

import           RIO
import qualified RIO.List                 as L
import qualified RIO.Text                 as Text

import           Git.Plantation           (Problem, Team)
import qualified Git.Plantation.Data.Team as Team
import           Git.Plantation.Env       (Plant)
import           Git.Plantation.Score     (Score, mkPlayerScore, mkScore)
import           Git.Plantation.Store     (Store)
import qualified Git.Plantation.Store     as Store
import qualified Network.Wreq             as W
import           Servant
import           UnliftIO.Concurrent      (forkIO)

type GetAPI
     = "teams"    :> Get '[JSON] [Team]
  :<|> "problems" :> Get '[JSON] [Problem]
  :<|> "scores"   :> Get '[JSON] [Score]
  :<|> "scores"   :> Capture "team" Text :> Get '[JSON] [Score]
  :<|> "scores"   :> Capture "team" Text :> Capture "player" Text :> Get '[JSON] [Score]

type PutAPI
      = "scores" :> Capture "owner" Text :> Capture "repo" Text :> Put '[JSON] NoContent

getAPI :: ServerT GetAPI Plant
getAPI = getTeams
   :<|> getProblems
   :<|> getScores
   :<|> getTeamScore
   :<|> getPlayerScore

getTeams :: Plant [Team]
getTeams = do
  logInfo "[GET] /teams"
  asks (view #teams . view #config)

getProblems :: Plant [Problem]
getProblems = do
  logInfo "[GET] /problems"
  asks (view #problems . view #config)

getScores :: Plant [Score]
getScores = do
  logInfo "[GET] /scores"
  store <- tryAny fetchStore >>= \case
    Left err -> logError (displayShow err) >> pure mempty
    Right s  -> pure $ Store.uniqByTeam <$> s
  config <- asks (view #config)
  pure $ map (mkScore (config ^. #problems) store) (config ^. #teams)

getTeamScore :: Text -> Plant [Score]
getTeamScore teamID = do
  logInfo $ display ("[GET] /scores/" <> teamID)
  store <- tryAny fetchStore >>= \case
    Left err -> logError (displayShow err) >> pure mempty
    Right s  -> pure $ Store.uniqByTeam <$> s
  config <- asks (view #config)
  teams <- case L.find (\team -> team ^. #id == teamID) (config ^. #teams) of
    Nothing   -> logError "team not found." >> pure mempty
    Just team -> pure [team]
  pure $ map (mkScore (config ^. #problems) store) teams

getPlayerScore :: Text -> Text -> Plant [Score]
getPlayerScore teamID userID = do
  logInfo $ display ("[GET] /scores/" <> teamID <> "/" <> userID)
  store <- tryAny fetchStore >>= \case
    Left err -> logError (displayShow err) >> pure mempty
    Right s  -> pure s
  config <- asks (view #config)
  teams <- case L.find (\team -> team ^. #id == teamID) (config ^. #teams) of
    Nothing   -> logError "team not found." >> pure mempty
    Just team -> pure [team]
  pure . catMaybes $
    liftA2 fmap (mkPlayerScore (config ^. #problems) store) (Team.lookupUser userID) <$> teams

fetchStore :: Plant Store
fetchStore = do
  url  <- Text.unpack <$> asks (view #store)
  resp <- W.asJSON =<< liftIO (W.get url)
  pure $ resp ^. W.responseBody

updateScore :: Text -> Text -> Plant NoContent
updateScore owner repo = do
  _ <- forkIO $ do
    problems <- asks (view #problems . view #config)
    let repo' = owner <> "/" <> repo
    logInfo $ display ("[PUT] /score/" <> repo')
    threadDelay 3_000_000 -- ToDo
    case L.find (\p -> p ^. #repo == repo') problems of
      Nothing -> logError $ display ("not found problem: " <> repo')
      Just p  -> updateScore' $ p ^. #id
  pure NoContent

updateScore' :: Int -> Plant ()
updateScore' pid = do
  url <- Text.unpack <$> asks (view #store)
  tryAny (liftIO $ W.customMethod "PATCH" $ url <> "/" <> show pid) >>= \case
    Left err -> logError (displayShow err)
    Right _  -> pure ()
