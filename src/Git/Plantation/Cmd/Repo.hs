module Git.Plantation.Cmd.Repo where

import           RIO

import           Git.Plantation.Env     (Plant)
import           Git.Plantation.Problem (Problem)
import           Git.Plantation.Team    (Team)

createRepo :: Team -> Problem -> Plant ()
createRepo _ _ = undefined
