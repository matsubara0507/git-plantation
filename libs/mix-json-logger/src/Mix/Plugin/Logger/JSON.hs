{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mix.Plugin.Logger.JSON
  ( LogMessage
  , mkLogMessage
  , displayJSON
  , logDebugR
  , logInfoR
  , logWarnR
  , logErrorR
  , logDebugR'
  , logInfoR'
  , logWarnR'
  , logErrorR'
  ) where

import           RIO

import           Data.Aeson      (ToJSON (..))
import qualified Data.Aeson      as J
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Extensible

type LogMessage xs = Record ("message" >: Text ': "level" >: LogLevel ': xs)

mkLogMessage :: Text -> LogLevel -> Record xs -> LogMessage xs
mkLogMessage msg level r = #message @= msg <: #level @= level <: r

displayJSON ::
  Forall (KeyValue KnownSymbol (Instance1 ToJSON Identity)) xs
  => LogMessage xs -> Utf8Builder
displayJSON = display . encodeToLazyText

logDebugR, logInfoR, logWarnR, logErrorR ::
  ( MonadIO m
  , MonadReader env m
  , HasLogFunc env
  , HasCallStack
  , Forall (KeyValue KnownSymbol (Instance1 ToJSON Identity)) xs
  ) => Text -> Record xs -> m ()
logDebugR msg = logGeneric "" LevelDebug . displayJSON . mkLogMessage msg LevelDebug
logInfoR  msg = logGeneric "" LevelInfo  . displayJSON . mkLogMessage msg LevelInfo
logWarnR  msg = logGeneric "" LevelWarn  . displayJSON . mkLogMessage msg LevelWarn
logErrorR msg = logGeneric "" LevelError . displayJSON . mkLogMessage msg LevelError

logDebugR', logInfoR', logWarnR', logErrorR' ::
  ( MonadIO m
  , MonadReader env m
  , HasLogFunc env
  , HasCallStack
  , Forall (KeyValue KnownSymbol (Instance1 ToJSON Identity)) xs
  ) => LogMessage xs -> m ()
logDebugR' = logGeneric "" LevelDebug . displayJSON
logInfoR'  = logGeneric "" LevelInfo  . displayJSON
logWarnR'  = logGeneric "" LevelWarn  . displayJSON
logErrorR' = logGeneric "" LevelError . displayJSON

-- Orphan instance
instance ToJSON LogLevel where
  toJSON LevelDebug        = J.String "debug"
  toJSON LevelInfo         = J.String "info"
  toJSON LevelWarn         = J.String "warn"
  toJSON LevelError        = J.String "error"
  toJSON (LevelOther name) = J.String name
