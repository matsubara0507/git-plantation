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
  , runShell
  , git
  , Shell.cat
  , Shell.echo
  , Shell.ls
  , Shell.mkdir
  , Shell.rm
  , Shell.cd
  , Shell.pwd
  , Shell.test
  , Shell.touch
  , test_d
  , test_f
  ) where

import           RIO
import qualified RIO.Text                  as Text

import           Data.Extensible
import           Mix.Plugin                (Plugin, toPlugin)
import qualified Mix.Plugin.Logger         as Mix
import qualified Mix.Plugin.Shell.Internal as Shell
import qualified Shh.Internal              as Shell

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
    Shell.mkdir ("-p" :: String) work
    liftIO $ Shell.cd work
    act
  case result of
    Left err  -> Mix.logError `Mix.withlines` displayShow err
    Right out -> Mix.logDebug `Mix.withlines` fromString out
  liftIO $ Shell.cd path

test_d :: (Shell.ProcFailure m, Functor m) => FilePath -> m Bool
test_d path = (== 0) <$> Shell.catchCode (Shell.test ("-d" :: String) path)

test_f :: (Shell.ProcFailure m, Functor m) => FilePath -> m Bool
test_f path = (== 0) <$> Shell.catchCode (Shell.test ("-f" :: String) path)

git :: Text -> [Text] -> Shell.Proc ()
git cmd args = Shell.mkProc "git" $ Text.unpack <$> (cmd : args)
