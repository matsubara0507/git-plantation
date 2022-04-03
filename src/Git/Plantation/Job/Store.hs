{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Git.Plantation.Job.Store where

import           RIO
import qualified RIO.Map                 as Map

import           Data.Extensible
import           Git.Plantation.Data.Job (Job)
import qualified Git.Plantation.Data.Job as Job

type Store = TVar (Map Job.Id Job)

class HasStore env where
  storeL :: Lens' env Store

instance Lookup xs "store" Store => HasStore (Record xs) where
  storeL = lens (view #store) (\x y -> x & #store `set` y)

askStore :: HasStore env => RIO env Store
askStore = view storeL

newStore :: MonadIO m => m Store
newStore = newTVarIO mempty

initializeStore :: HasStore env => [Job] -> RIO env ()
initializeStore jobs = do
  store <- askStore
  atomically $ writeTVar store (Map.fromList $ fmap (\job -> (job ^. #id, job)) jobs)

withStore :: HasStore env => RIO env Job -> RIO env Job
withStore m = do
  job <- m
  store <- askStore
  atomically $ modifyTVar' store (Map.insert (job ^. #id) job)
  pure job
