{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.API.CRUD where

import           RIO

import           Data.Extensible
import           Git.Plantation.Env     (Plant)
import           Git.Plantation.Problem (Problem)
import           Git.Plantation.Score   (Score)
import           Git.Plantation.Team    (Team)
import           Servant

type CRUD
      = "teams"    :> Get '[JSON] [Team]
   :<|> "problems" :> Get '[JSON] [Problem]
   :<|> "scores"   :> Get '[JSON] [Score]

crud :: ServerT CRUD Plant
crud
      = getTeams
   :<|> getProblems
   :<|> getScores

getTeams :: Plant [Team]
getTeams = asks (view #teams . view #config)

getProblems :: Plant [Problem]
getProblems = asks (view #problems . view #config)

getScores :: Plant [Score]
getScores = map toScore <$> getTeams

-- ToDo
toScore :: Team -> Score
toScore team
    = #team  @= (team ^. #name)
   <: #point @= 0
   <: #stats @= [ stat1, stat2 ]
   <: nil
   where
     stat1 = #problem @= "tutorial"    <: #correct @= True  <: nil
     stat2 = #problem @= "minesweeper" <: #correct @= False <: nil
