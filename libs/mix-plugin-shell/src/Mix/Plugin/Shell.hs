{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Mix.Plugin.Shell
  ( HasWorkDir (..)
  , buildPlugin
  , workText
  , runShell
  , Shell.git
  , Shell.mkdir
  , Shell.rm
  , Shell.cd
  , Shell.pwd
  , Shell.touch
  ) where

import           RIO

import           Data.Extensible
import           Mix.Plugin                (Plugin, toPlugin)
import qualified Mix.Plugin.Logger         as Mix
import qualified Mix.Plugin.Shell.Internal as Shell
import qualified Shh                       as Shell

buildPlugin :: MonadIO m => FilePath -> Plugin a m FilePath
buildPlugin path = toPlugin $ \f -> f path

class HasWorkDir env where
  workL :: Lens' env FilePath

instance Associate "work" FilePath xs => HasWorkDir (Record xs) where
  workL = lens (view #work) (\x y -> x & #work `set` y)

workText :: (MonadIO m, MonadReader env m, HasWorkDir env) => m Text
workText = fromString <$> view workL

runShell ::
  ( MonadIO m
  , MonadReader env m
  , HasWorkDir env
  , HasLogFunc env
  ) => Shell.Proc () -> m ()
runShell act = do
  path <- liftIO $ takeWhile (/= '\n') <$> Shell.readProc Shell.pwd
  work <- view workL
  result <- liftIO $ Shell.catchFailure $ Shell.readProc $ do
    liftIO $ Shell.cd work
    act
  case result of
    Left err  -> Mix.logError `Mix.withlines` displayShow err
    Right out -> Mix.logDebug `Mix.withlines` fromString out
  liftIO $ Shell.cd path
