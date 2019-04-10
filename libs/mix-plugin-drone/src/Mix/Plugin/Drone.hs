{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Mix.Plugin.Drone
  ( HasDroneClient (..)
  , buildPlugin
  , fetch
  ) where

import           RIO

import           Data.Default.Class (def)
import           Data.Extensible
import           Drone
import           Mix.Plugin         (Plugin, toPlugin)
import           Network.HTTP.Req   (Req, runReq)

buildPlugin :: (MonadIO m, Drone.Client c)
  => Drone.BaseClient -> (Drone.BaseClient -> c) -> Plugin a m c
buildPlugin base scheme = toPlugin $ \f -> f (scheme base)

class Drone.Client c => HasDroneClient c env where
  clientL :: Lens' env c

instance (Drone.Client c, Associate "drone" c xs) => HasDroneClient c (Record xs) where
  clientL = lens (view #drone) (\x y -> x & #drone `set` y)

fetch ::
  ( MonadIO m
  , MonadReader env m
  , HasDroneClient c env
  ) => (c -> Req a) -> m a
fetch req = (runReq def . req) =<< view clientL
