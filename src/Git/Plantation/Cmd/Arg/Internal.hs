{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}

module Git.Plantation.Cmd.Arg.Internal where

import           RIO

import           Data.Extensible
import           Git.Plantation.Config
import           Mix.Plugin.Config     (HasConfig (..))

class IdArg id a | id -> a where
  findById :: id -> [a] -> Maybe a
  errMsg :: id -> ErrMsg

type ErrMsg = Record
  '[ "message" >: Text
   , "id" >: Text
   ]

asksConfig :: (HasConfig Config env, MonadReader env m) => m Config
asksConfig = view configL

findByIdWith :: (IdArg id a, HasConfig Config env, MonadReader env m)
  => (Config -> [a]) -> id -> m (Maybe a)
findByIdWith f idx = findById idx . f <$> asksConfig
