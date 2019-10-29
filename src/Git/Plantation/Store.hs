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
  , initial
  , modifyWith
  , uniqByTeam
  , fetchBuilds
  , getAllBuilds
  , API
  , api
  , server
  ) where

import           RIO
import qualified RIO.List            as L

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
   ]

isCorrect :: Build -> Bool
isCorrect b = b ^. #status == "success"

isPending :: Build -> Bool
isPending b = b ^. #status == "running" || b ^. #status == "pending"

initial :: Plant Store
initial = do
  problems <- asks (view #problems . view #config)
  builds   <- forM problems (\p -> (p ^. #id,) <$> fetchBuilds p)
  pure $ uniqByTeam <$> IntMap.fromList builds

modifyWith :: Problem -> [Build] -> Store -> Store
modifyWith problem builds = IntMap.insert (problem ^. #id) builds

uniqByTeam :: [Build] -> [Build]
uniqByTeam =
  mapMaybe (L.maximumByMaybe ordStatus)
    . L.groupBy (\a b -> a ^. #source == b ^. #source)
  where
    ordStatus a b = ordStatus' (a ^. #status) (b ^. #status)
    ordStatus' "success" _  = GT
    ordStatus'  _ "success" = LT
    ordStatus' "running" _  = GT
    ordStatus'  _ "running" = LT
    ordStatus' "pending" _  = GT
    ordStatus'  _ "pending" = LT
    ordStatus' _ _          = EQ

filterWithStartTime :: [Build] -> Plant [Build]
filterWithStartTime builds =
  asks (view #start_time . view #config) <&> \case
    Nothing -> builds
    Just st -> filter (\b -> b ^. #created >= st) builds

fetchBuilds :: Problem -> Plant [Build]
fetchBuilds problem = do
  let (owner, repo) = splitRepoName $ problem ^. #repo
  builds <- tryAny (getAllBuilds owner repo) >>= \case
    Left err     -> logError (display err) >> pure []
    Right builds -> pure builds
  filterWithStartTime $ shrink <$> builds

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
      pure store'
    putStore pid = do
      logInfo $ fromString ("[PATCH] /store/" <> show pid)
      findProblemWith pid $ \problem -> do
        builds <- uniqByTeam <$> fetchBuilds problem
        liftIO $ atomically (modifyTVar' store $ modifyWith problem builds)
      pure NoContent

findProblemWith :: Int -> (Problem -> Plant ()) -> Plant ()
findProblemWith pid act = do
  problems <- asks (view #problems . view #config)
  case L.find (\p -> p ^. #id == pid) problems of
    Nothing -> logWarn (fromString $ "not found problem id: " <> show pid)
    Just p  -> act p
