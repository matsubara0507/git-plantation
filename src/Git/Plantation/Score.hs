{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Score where

import           RIO
import qualified RIO.List              as L
import qualified RIO.Map               as Map

import           Data.Extensible
import qualified Data.IntMap.Strict    as IntMap
import qualified Drone.Endpoints       as Drone
import qualified Drone.Types           as Drone
import           Elm                   (ElmType (..))
import           Git.Plantation.Cmd    (splitRepoName)
import           Git.Plantation.Config (Config)
import           Git.Plantation.Data   (Problem, Repo, Team, repoGithubPath)
import           Git.Plantation.Env    (Plant)
import           Language.Elm
import qualified Mix.Plugin.Drone      as MixDrone
import           Network.HTTP.Req      as Req

type Scores = Map Text Score

type Score = Record
  '[ "team"  >: Text
   , "point" >: Int
   , "stats" >: IntMap Status
   , "links" >: [Link]
   ]

-- | Score type for json response
type ScoreR = Record
  '[ "team"  >: Text
   , "point" >: Int
   , "stats" >: [Status]
   , "links" >: [Link]
   ]

type Status = Record
  '[ "problem_id" >: Int
   , "correct"    >: Bool
   , "pending"    >: Bool
   ]

type Link = Record
  '[ "problem_id" >: Int
   , "url"        >: Text
   ]

instance ElmType ScoreR where
  toElmType = toElmRecordType "Score"

instance ElmType Status where
  toElmType = toElmRecordType "Status"

instance ElmType Link where
  toElmType = toElmRecordType "Link"

toResponse :: Score -> ScoreR
toResponse score
    = #team  @= (score ^. #team)
   <: #point @= (score ^. #point)
   <: #stats @= (IntMap.elems $ score ^. #stats)
   <: #links @= (score ^. #links)
   <: nil

initScores :: Plant Scores
initScores = do
   teams    <- asks (view #teams . view #config)
   problems <- asks (view #problems . view #config)
   builds   <- IntMap.fromList <$> mapM fetchBuilds problems
   pure $ Map.fromList $ map (mkScoreWithId problems builds) teams

fetchBuilds :: Problem -> Plant (Int, [Drone.Build])
fetchBuilds problem = do
  let (owner, repo) = splitRepoName $ problem ^. #repo
  builds <- tryAny (getAllBuilds owner repo) >>= \case
    Left err     -> logError (display err) >> pure []
    Right builds -> pure builds
  pure (problem ^. #id, builds)

mkScore :: [Problem] -> IntMap [Drone.Build] -> Team -> Score
mkScore problems builds team
    = #team  @= team ^. #id
   <: #point @= calcPoint stats problems
   <: #stats @= stats
   <: #links @= links
   <: nil
  where
    isTeamBuild b = b ^. #source == team ^. #id
    builds' = IntMap.filter (not . null) $ fmap (filter isTeamBuild) builds
    stats = IntMap.mapWithKey toStatus builds'
    links = map toLink $ team ^. #repos

mkScoreWithId :: [Problem] -> IntMap [Drone.Build] -> Team -> (Text, Score)
mkScoreWithId problems builds team = (score ^. #team, score)
  where
    score = mkScore problems builds team

toStatus :: Int -> [Drone.Build] -> Status
toStatus idx builds
    = #problem_id @= idx
   <: #correct    @= any (\b -> b ^. #status == "success") builds
   <: #pending    @= any (\b -> b ^. #status == "running" || b ^. #status == "pending") builds
   <: nil

calcPoint :: IntMap Status -> [Problem] -> Int
calcPoint stats = sum . map (toPoint stats)

toPoint :: IntMap Status -> Problem -> Int
toPoint stats problem =
  case IntMap.lookup (problem ^. #id) stats of
    Just s | s ^. #correct -> problem ^. #difficulty
    _                      -> 0

toLink :: Repo -> Link
toLink repo
    = #problem_id @= repo ^. #problem
   <: #url        @= maybe "" ("https://github.com/" <>) (repoGithubPath repo)
   <: nil

getAllBuilds :: Text -> Text -> Plant [Drone.Build]
getAllBuilds owner repo = mconcat <$> getAllBuilds' [] 1
  where
    getAllBuilds' xss n = do
      resp <- MixDrone.fetch $ \client -> Drone.getBuilds client owner repo (Just n)
      case Req.responseBody resp of
        []     -> pure xss
        builds -> getAllBuilds' (builds : xss) (n + 1)

toPendingScore :: Config -> Team -> Problem -> Scores -> Scores
toPendingScore config team problem scores = Map.insert (team ^. #id) score' scores
  where
    def    = mkScore (config ^. #problems) mempty team
    score  = fromMaybe def $ Map.lookup (team ^. #id) scores
    score' = score & #stats `over` IntMap.update (Just . toPending) (problem ^. #id)

toPending :: Status -> Status
toPending stat =
  if stat ^. #correct then stat else stat & #pending `set` True

updateStatWithBuild :: Config -> Int -> Drone.Build -> Scores -> Scores
updateStatWithBuild config pid build scores =
  case L.find (\t -> t ^. #id == build ^. #source) (config ^. #teams) of
    Nothing   -> scores
    Just team -> Map.insert (team ^. #id) (mkScore' team) scores
  where
    mkScore' team =
      score' & #point `set` calcPoint (score' ^. #stats) (config ^. #problems)
      where
        score' = score & #stats `over` IntMap.update (Just . updateStat) pid
        def    = mkScore (config ^. #problems) mempty team
        score  = fromMaybe def $ Map.lookup (team ^. #id) scores
    updateStat stat =
      if stat ^. #correct then stat else stat & #correct `set` result
    result = build ^. #status == "success"
