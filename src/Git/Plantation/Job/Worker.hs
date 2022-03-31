{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Git.Plantation.Job.Worker where

import           RIO
import           RIO.List.Partial   ((!!))
import qualified RIO.Map            as Map

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Binary        (Binary)
import           Data.Extensible
import qualified Network.WebSockets as WS
import           System.Random      (randomRIO)

newtype Id = Id Int32
  deriving newtype (Show, Eq, Ord, Num, Binary, FromJSON, ToJSON, Display)

type Worker = Record
  '[ "id" >: Id
   , "working" >: Bool
   , "conn" >: WS.Connection
   ]

new :: Id -> WS.Connection -> Worker
new wid conn
    = #id      @= wid
   <: #working @= False
   <: #conn    @= conn
   <: nil

work, finish :: Worker -> Worker
work w = w & #working `set` True
finish w = w & #working `set` False

type Info = Record
  '[ "id" >: Id
   , "working" >: Bool
   ]

type Workers = TVar (Map Id (Maybe Worker))

newWorkers :: MonadIO m => m Workers
newWorkers = newTVarIO mempty

class HasWorkers env where
  workersL :: Lens' env Workers

instance Lookup xs "workers" Workers => HasWorkers (Record xs) where
  workersL = lens (view #workers) (\x y -> x & #workers `set` y)

askWorkers :: HasWorkers env => RIO env Workers
askWorkers = view workersL

getAllConnected :: HasWorkers env => RIO env [Worker]
getAllConnected = fmap (catMaybes . Map.elems) $ readTVarIO =<< askWorkers

getRandom :: HasWorkers env => RIO env (Maybe Worker)
getRandom = do
  workers <- getAllConnected
  case workers of
    [] ->
      pure Nothing
    _ -> do
      idx <- (\x -> x - 1) <$> randomRIO (1, length workers)
      pure $ Just (workers !! idx)

connected :: HasWorkers env => WS.Connection -> RIO env Worker
connected conn = do
  workers <- askWorkers
  atomically $ do
    maxId <- Map.size <$> readTVar workers
    let worker = new (fromIntegral $ maxId + 1) conn
    modifyTVar workers (Map.insert (worker ^. #id) $ Just worker)
    pure worker

disconnected :: HasWorkers env => Id -> RIO env ()
disconnected wid = do
  workers <- askWorkers
  atomically $ modifyTVar workers (Map.update (\_ -> Just Nothing) wid)


