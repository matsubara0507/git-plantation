{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.CRUD where

import           RIO
import qualified RIO.Map              as Map

import           Data.Default.Class
import           Data.Extensible
import qualified Drone.Client         as Drone
import qualified Drone.Endpoints      as Drone
import qualified Drone.Types          as Drone
import           Git.Plantation       (Problem, Team)
import           Git.Plantation.Cmd   (splitRepoName)
import           Git.Plantation.Env   (Plant)
import           Git.Plantation.Score (Score, Status)
import           Network.HTTP.Req
import           Servant

type CRUD
      = "teams"    :> Get '[JSON] [Team]
   :<|> "problems" :> Get '[JSON] [Problem]
   :<|> "scores"   :> Get '[JSON] [Score]

crud :: ServerT CRUD Plant
crud
      = getTeams
   :<|> getProblems
   :<|> getScores

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
  teams    <- asks (view #teams . view #config)
  problems <- asks (view #problems . view #config)
  client   <- asks (view #client)
  builds   <- Map.fromList <$> mapM (fetchBuilds client) problems
  pure $ map (mkScore problems builds) teams

fetchBuilds :: Drone.Client c => c -> Problem -> Plant (Text, [Drone.Build])
fetchBuilds client problem = do
  let (owner, repo) = splitRepoName $ problem ^. #repo
  builds <- tryAny (runReq def $ Drone.getBuilds client owner repo Nothing) >>= \case
    Left err   -> logError (display err) >> pure []
    Right resp -> pure $ responseBody resp
  pure (problem ^. #name, builds)

mkScore :: [Problem] -> Map Text [Drone.Build] -> Team -> Score
mkScore problems builds team
    = #team  @= team ^. #name
   <: #point @= sum (map (toPoint stats) problems)
   <: #stats @= stats
   <: nil
  where
    isTeamBuild b = b ^. #source == team ^. #name
    builds' = Map.filter (not . null) $ Map.map (filter isTeamBuild) builds
    stats = Map.elems $ Map.mapWithKey toStatus builds'

toStatus :: Text -> [Drone.Build] -> Status
toStatus name builds
    = #problem @= name
   <: #correct @= any (\b -> b ^. #status == "success") builds
   <: #pending @= any (\b -> b ^. #status == "pending") builds
   <: nil

toPoint :: [Status] -> Problem -> Int
toPoint stats problem =
  if elem (problem ^. #name) $ map (view #problem) stats then
    problem ^. #difficulty
  else
    0
