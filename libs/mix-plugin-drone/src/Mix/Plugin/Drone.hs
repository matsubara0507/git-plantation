{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Mix.Plugin.Drone
  ( HasDroneClient (..)
  , DroneConfig (..)
  , buildPlugin
  , fetch
  ) where

import           RIO

import           Data.Default.Class (def)
import           Data.Extensible
import           Drone
import           Mix.Plugin         (Plugin, toPlugin)
import           Network.HTTP.Req   (Req, runReq)

newtype DroneConfig = DroneConfig Config

type Config = Record
  '[ "base"    >: Drone.BaseClient
   , "is_http" >: Bool
   ]

buildPlugin :: MonadIO m => Drone.BaseClient -> Bool -> Plugin a m DroneConfig
buildPlugin base isHttp =
  toPlugin $ \f -> f (DroneConfig $ #base @= base <: #is_http @= isHttp <: nil)

class HasDroneClient env where
  clientL :: Lens' env DroneConfig

instance Associate "drone" DroneConfig xs => HasDroneClient (Record xs) where
  clientL = lens (view #drone) (\x y -> x & #drone `set` y)

fetch ::
  ( MonadIO m
  , MonadReader env m
  , HasDroneClient env
  ) => (forall c . Drone.Client c => c -> Req a) -> m a
fetch req = do
  (DroneConfig config) <- view clientL
  if config ^. #is_http then
    runReq def $ req (Drone.HttpClient $ config ^. #base)
  else
    runReq def $ req (Drone.HttpsClient $ config ^. #base)
