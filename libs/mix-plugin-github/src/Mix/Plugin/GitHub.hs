{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Mix.Plugin.GitHub
  ( HasGitHubToken (..)
  , buildPlugin
  , tokenText
  , auth
  , fetch
  ) where

import           RIO

import           Data.Extensible
import qualified GitHub.Auth     as GitHub
import           Mix.Plugin      (Plugin, toPlugin)

buildPlugin ::  MonadIO m => GitHub.Token -> Plugin a m GitHub.Token
buildPlugin token = toPlugin $ \f -> f token

class HasGitHubToken env where
  tokenL :: Lens' env GitHub.Token

instance Associate "github" GitHub.Token xs => HasGitHubToken (Record xs) where
  tokenL = lens (view #github) (\x y -> x & #github `set` y)

tokenText :: (MonadIO m, MonadReader env m, HasGitHubToken env) => m Text
tokenText = decodeUtf8With lenientDecode <$> view tokenL

auth :: (MonadIO m, MonadReader env m, HasGitHubToken env) => m GitHub.Auth
auth = GitHub.OAuth <$> view tokenL

fetch :: (MonadIO m, MonadReader env m, HasGitHubToken env) => (GitHub.Auth -> IO a) -> m a
fetch act = (liftIO . act) =<< auth
