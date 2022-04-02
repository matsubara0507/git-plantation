{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Store
  ( Store
  , Env
  , API
  , api
  , server
  ) where

import           RIO
import qualified RIO.Map                 as Map

import           Data.Extensible
import           Git.Plantation.Config
import           Git.Plantation.Data.Job (Job)
import qualified Git.Plantation.Data.Job as Job
import           Mix.Plugin.Logger       ()
import           Servant

type Env = Record
  '[ "config" >: Config
   , "logger" >: LogFunc
   ]

type Store = Map Job.Id Job

type API
      = Get '[JSON] [Job]
   :<|> ReqBody '[JSON] Job :> Post '[JSON] NoContent
   :<|> "initialize" :> ReqBody '[JSON] [Job] :> Post '[JSON] NoContent

api :: Proxy API
api = Proxy

server :: TVar Store -> ServerT API (RIO Env)
server store = getStore :<|> putStore :<|> initializeStore'
  where
    getStore = do
      logInfo "[GET] /store"
      Map.elems <$> readTVarIO store
    putStore job = do
      logInfo $ "[PUT] /store/" <> display (job ^. #id)
      atomically $ modifyTVar' store (Map.insert (job ^. #id) job)
      pure NoContent
    initializeStore' jobs = do
      logInfo "[POST] /store/initialize"
      atomically $ writeTVar store (initializeStore jobs)
      pure NoContent

initializeStore :: [Job] -> Store
initializeStore = Map.fromList . fmap (\job -> (job ^. #id, job))
