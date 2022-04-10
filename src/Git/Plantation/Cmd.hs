{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd
    ( module X
    , Env
    , Run (..)
    , run
    , showNotImpl
    ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Arg     as X
import           Git.Plantation.Cmd.Env     as X
import           Git.Plantation.Cmd.Member  as X
import           Git.Plantation.Cmd.Org     as X
import           Git.Plantation.Cmd.Problem as X
import           Git.Plantation.Cmd.Repo    as X
import           Git.Plantation.Config      (Config)
import           Git.Plantation.Env         (WebhookConfig)
import qualified Mix.Plugin.GitHub          as GitHub


type Env = Record
  '[ "config"  >: Config
   , "github"  >: GitHub.Token
   , "work"    >: FilePath
   , "webhook" >: WebhookConfig
   , "logger"  >: LogFunc
   ]

class Run kv where
  run' :: proxy kv -> TargetOf kv -> RIO Env ()

run :: Forall Run xs => Variant xs -> RIO Env ()
run = matchField (htabulateFor (Proxy @Run) $ \m -> Field (Match $ run' m . runIdentity))

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
