{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Store where

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
   ]

initial :: Plant Store
initial = do
  problems <- asks (view #problems . view #config)
  builds   <- forM problems (\p -> (p ^. #id,) <$> fetchBuilds p)
  pure $ uniqByTeam <$> IntMap.fromList builds

update :: Problem -> Store -> Plant Store
update p s = ($ s) <$> update' p

update' :: Problem -> Plant (Store -> Store)
update' problem = do
  builds <- uniqByTeam <$> fetchBuilds problem
  pure $ IntMap.insert (problem ^. #id) builds

uniqByTeam :: [Build] -> [Build]
uniqByTeam =
  mapMaybe (L.minimumByMaybe ordStatus)
    . L.groupBy (\a b -> a ^. #source == b ^. #source)
  where
    ordStatus a b = ordStatus' (a ^. #status) (b ^. #status)
    ordStatus' "success" _  = GT
    ordStatus'  _ "success" = LT
    ordStatus' "running" _  = GT
    ordStatus'  _ "running" = GT
    ordStatus' "pending" _  = GT
    ordStatus'  _ "pending" = GT
    ordStatus' _ _          = GT

fetchBuilds :: Problem -> Plant [Build]
fetchBuilds problem = do
  let (owner, repo) = splitRepoName $ problem ^. #repo
  builds <- tryAny (getAllBuilds owner repo) >>= \case
    Left err     -> logError (display err) >> pure []
    Right builds -> pure builds
  pure $ shrink <$> builds

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
      problems <- asks (view #problems . view #config)
      case L.find (\p -> p ^. #id == pid) problems of
        Nothing -> logWarn (fromString $ "not found problem id: " <> show pid)
        Just p  -> liftIO . atomically . modifyTVar' store =<< update' p
      pure NoContent
