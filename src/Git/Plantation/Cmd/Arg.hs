module Git.Plantation.Cmd.Arg
  ( module X
  , mapMaybeWithDefault
  , filterByIdWithDefault
  ) where

import           RIO

import           Git.Plantation.Cmd.Arg.Internal as X (ErrMsg, IdArg (..),
                                                       findByIdWith)
import           Git.Plantation.Cmd.Arg.Problem  as X
import           Git.Plantation.Cmd.Arg.Team     as X

mapMaybeWithDefault :: [b] -> (a -> Maybe b) -> [a] -> [b]
mapMaybeWithDefault bs f as = if null as then bs else mapMaybe f as

filterByIdWithDefault :: IdArg id a => [a] -> [id] -> [a]
filterByIdWithDefault xs = mapMaybeWithDefault xs (`findById` xs)
