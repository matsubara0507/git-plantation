module Mix.Plugin where

import           RIO

import           Control.Monad.Cont

type Plugin a m env = ContT a m env

toPlugin :: ((env -> m a) -> m a) -> Plugin a m env
toPlugin = ContT

withPlugin :: (env -> m a) -> Plugin a m env -> m a
withPlugin = flip runContT
