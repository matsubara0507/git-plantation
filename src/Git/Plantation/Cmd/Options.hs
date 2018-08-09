{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Git.Plantation.Cmd.Options where

import           RIO

import           Data.Extensible

type Options = Record
  '[ "verbose" >: Bool
   , "config"  >: FilePath
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "repo" >: Text
   ]
