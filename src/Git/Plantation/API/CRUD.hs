{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.CRUD where

import           RIO

import           Git.Plantation.Env     (Plant)
import           Git.Plantation.Problem (Problem)
import           Git.Plantation.Team    (Team)
import           Servant

type CRUD
      = "teams"    :> Get '[JSON] [Team]
   :<|> "problems" :> Get '[JSON] [Problem]

crud :: ServerT CRUD Plant
crud
      = getTeams
   :<|> getProblems

getTeams :: Plant [Team]
getTeams = asks (view #teams . view #config)

getProblems :: Plant [Problem]
getProblems = asks (view #problems . view #config)
