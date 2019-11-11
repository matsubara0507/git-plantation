{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Store
  ( Store
  , Build
  , isCorrect
  , isPending
  , toAnswer
  , initial
  , modifyWith
  , uniqByTeam
  , uniqByPlayer
  , fetchBuilds
  , getAllBuilds
  , API
  , api
  , server
  ) where

import           RIO
import qualified RIO.List            as L
import qualified RIO.Text            as T

import           Data.Extensible
import qualified Data.IntMap.Strict  as IntMap
import qualified Drone.Endpoints     as Drone
import qualified Drone.Types         as Drone
import           Git.Plantation.Cmd  (splitRepoName)
import           Git.Plantation.Data (Problem)
import           Git.Plantation.Env  (Plant)
import qualified Mix.Plugin.Drone    as MixDrone
import           Network.HTTP.Req    as Req
import           Servant

type Store = IntMap [Build]

type Build = Record
  '[ "source"      >: Text
   , "status"      >: Text
   , "source_repo" >: Text
   , "created"     >: Int64
   , "message"     >: Text
   ]

isCorrect :: Build -> Bool
isCorrect b = b ^. #status == "success"

isPending :: Build -> Bool
isPending b = b ^. #status == "running" || b ^. #status == "pending"

toAnswer :: Build -> Maybe Text
toAnswer build =
  case T.takeWhileEnd (/= '@') (build ^. #message) of
    ""      -> Nothing
    account -> Just account

initial :: Plant Store
initial = do
  problems <- asks (view #problems . view #config)
  builds   <- forM problems (\p -> (p ^. #id,) <$> fetchBuilds p)
  pure $ uniqByPlayer <$> IntMap.fromList builds

modifyWith :: Problem -> [Build] -> Store -> Store
modifyWith problem = IntMap.insert (problem ^. #id)

uniqByTeam :: [Build] -> [Build]
uniqByTeam =
  mapMaybe (L.maximumByMaybe ordStatus . L.sortOn (view #created)) . groupByTeam

uniqByPlayer :: [Build] -> [Build]
uniqByPlayer =
  mapMaybe (L.maximumByMaybe ordStatus . L.sortOn (view #created)) . groupByPlayer

groupByTeam :: [Build] -> [[Build]]
groupByTeam = L.groupBy ((==) `on` view #source) . L.sortOn (view #source)

groupByPlayer :: [Build] -> [[Build]]
groupByPlayer =
  L.concatMap (L.groupBy ((==) `on` toAnswer) . L.sortOn (view #message)) . groupByTeam

ordStatus :: Build -> Build -> Ordering
ordStatus = ordStatus' `on` view #status
  where
    ordStatus' "success" _  = GT
    ordStatus'  _ "success" = LT
    ordStatus' "running" _  = GT
    ordStatus'  _ "running" = LT
    ordStatus' "pending" _  = GT
    ordStatus'  _ "pending" = LT
    ordStatus' _ _          = EQ

filterWithTime :: [Build] -> Plant [Build]
filterWithTime builds = do
  config <- asks (view #scoreboard . view #config)
  pure $ case (config ^. #start_time, config ^. #end_time) of
    (Just st, Just et) -> filter (\b -> b ^. #created >= st && b ^. #created <= et) builds
    (Just st, Nothing) -> filter (\b -> b ^. #created >= st) builds
    (Nothing, Just et) -> filter (\b -> et <= b ^. #created) builds
    _                  -> builds

fetchBuilds :: Problem -> Plant [Build]
fetchBuilds problem = do
  let (owner, repo) = splitRepoName $ problem ^. #repo
  builds <- tryAny (getAllBuilds owner repo) >>= \case
    Left err     -> logError (display err) >> pure []
    Right builds -> pure builds
  filterWithTime $ shrink <$> builds

getAllBuilds :: Text -> Text -> Plant [Drone.Build]
getAllBuilds owner repo = mconcat <$> getAllBuilds' [] 1
  where
    getAllBuilds' xss n = do
      resp <- MixDrone.fetch $ \client -> Drone.getBuilds client owner repo (Just n)
      case Req.responseBody resp of
        []     -> pure xss
        builds -> getAllBuilds' (builds : xss) (n + 1)

type API
      = Get '[JSON] Store
   :<|> Capture "problem" Int :> Patch '[JSON] NoContent

api :: Proxy API
api = Proxy

server :: TVar Store -> ServerT API Plant
server store = getStore :<|> putStore
  where
    getStore = do
      logInfo "[GET] /store"
      store' <- liftIO $ readTVarIO store
      logDebug (displayShow store')
      pure $ uniqByPlayer <$> store'
    putStore pid = do
      logInfo $ fromString ("[PATCH] /store/" <> show pid)
      findProblemWith pid $ \problem -> do
        builds <- uniqByPlayer <$> fetchBuilds problem
        liftIO $ atomically (modifyTVar' store $ modifyWith problem builds)
      pure NoContent

findProblemWith :: Int -> (Problem -> Plant ()) -> Plant ()
findProblemWith pid act = do
  problems <- asks (view #problems . view #config)
  case L.find (\p -> p ^. #id == pid) problems of
    Nothing -> logWarn (fromString $ "not found problem id: " <> show pid)
    Just p  -> act p
