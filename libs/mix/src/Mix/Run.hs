module Mix.Run where

import           RIO

import           Mix.Plugin (Plugin, withPlugin)

run :: MonadIO m => Plugin a m env -> RIO env a -> m a
run plugin act = (`runRIO` act) `withPlugin` plugin
