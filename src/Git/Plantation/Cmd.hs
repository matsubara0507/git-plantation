{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd
    ( module X
    , run
    ) where

import           RIO

import           Data.Extensible
import           Git.Plantation.Cmd.Arg    as X
import           Git.Plantation.Cmd.Member as X
import           Git.Plantation.Cmd.Repo   as X
import           Git.Plantation.Cmd.Run    as X
import           Git.Plantation.Env        (Plant)

run :: Forall Run xs => Variant xs -> Plant ()
run = matchField (htabulateFor (Proxy @ Run) $ \m -> Field (Match $ run' m . runIdentity))
