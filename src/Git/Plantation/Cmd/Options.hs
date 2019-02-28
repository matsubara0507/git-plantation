{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Plantation.Cmd.Options where

import           RIO
import qualified RIO.List                as L

import           Data.Extensible
import           Git.Plantation.Cmd.Repo
import           Git.Plantation.Cmd.Run
import           Git.Plantation.Data     (Problem, Team)
import           Git.Plantation.Env

type Options = Record
  '[ "verbose" >: Bool
   , "config"  >: FilePath
   , "work"    >: FilePath
   , "subcmd"  >: SubCmd
   ]

type SubCmd = Variant SubCmdFields

type SubCmdFields =
  '[ "new_repo" >: NewRepoCmd
   ]

instance Run ("new_repo" >: NewRepoCmd) where
  run' _ = runRepoCmd createRepo

runRepoCmd :: (Team -> Problem -> Plant ()) -> Record RepoFields -> Plant ()
runRepoCmd act args = do
  conf <- asks (view #config)
  let team = L.find (\t -> t ^. #name == args ^. #team) $ conf ^. #teams
  case (team, args ^. #repo) of
    (Nothing, _)            -> logError $ "team is not found: " <> display (args ^. #team)
    (Just team', Just name) -> actByRepoName act team' name
    (Just team', _)         -> forM_ (conf ^. #problems) (tryAnyWithLogError . act team')
