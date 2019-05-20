{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Mix.Plugin.Shell
  ( HasWorkDir (..)
  , buildPlugin
  , workText
  , exec
  , exec'
  , shellyWithLogDebug
  , Sh
  ) where

import           RIO

import           Data.Extensible
import           Mix.Plugin      (Plugin, toPlugin)
import           Shelly          (Sh)
import qualified Shelly          as Sh

buildPlugin :: MonadIO m => FilePath -> Plugin a m FilePath
buildPlugin path = toPlugin $ \f -> f path

class HasWorkDir env where
  workL :: Lens' env FilePath

instance Associate "work" FilePath xs => HasWorkDir (Record xs) where
  workL = lens (view #work) (\x y -> x & #work `set` y)

workText :: (MonadIO m, MonadReader env m, HasWorkDir env) => m Text
workText = fromString <$> view workL

exec ::
  ( MonadUnliftIO m
  , MonadReader env m
  , HasWorkDir env
  , HasLogFunc env
  ) => Sh () -> m ()
exec act = do
  work <- view workL
  shellyWithLogDebug $ Sh.chdir_p (fromString work) act

shellyWithLogDebug ::
  ( MonadUnliftIO m
  , MonadReader env m
  , HasLogFunc env
  ) => Sh a -> m a
shellyWithLogDebug act = do
  runInIO <- askRunInIO
  Sh.shelly
    . Sh.log_stdout_with (runInIO . logDebug . display)
    . Sh.log_stderr_with (runInIO . logDebug . display)
    $ act

exec' ::
  ( MonadUnliftIO m
  , MonadReader env m
  , HasWorkDir env
  ) => Sh () -> m ()
exec' act = do
  work <- view workL
  Sh.shelly $ Sh.chdir_p (fromString work) act
