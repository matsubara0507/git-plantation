{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Git.Plantation.Team where

import           RIO

import           Data.Extensible
import           Git.Plantation.Data (User)

type Team = Record
  '[ "name"   >: Text
   , "github" >: Text
   , "member" >: [User]
   ]
