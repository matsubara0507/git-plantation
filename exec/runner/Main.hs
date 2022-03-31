{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Paths_git_plantation      (version)
import           RIO
import qualified RIO.ByteString            as B
import           RIO.Process               (mkDefaultProcessContext)

import           Configuration.Dotenv      (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version              (Version)
import qualified Data.Version              as Version
import           Development.GitRev
import           Git.Plantation.Job.Client as Client
import qualified Mix
import qualified Mix.Plugin.Logger         as MixLogger

import           Orphans                  ()


main :: IO ()
main = withGetOpt "[options] DESTINATION" opts $ \r args -> do
  _ <- tryIO $ loadFile defaultConfig
  case (r ^. #version, listToMaybe args) of
    (True, _)      -> B.putStr $ fromString (showVersion version) <> "\n"
    (_, Nothing)   -> error "please input DESTINATION"
    (_, Just dest) -> runClient r dest
  where
    opts = #verbose @= verboseOpt
        <: #version @= versionOpt
        <: nil

type Options = Record
  '[ "verbose" >: Bool
   , "version" >: Bool
   ]

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

runClient :: Options -> String -> IO ()
runClient opts dest =
  let logConf = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      plugin = hsequence
             $ #logger         <@=> MixLogger.buildPlugin logConf
            <: #processContext <@=> mkDefaultProcessContext
            <: #configs        <@=> newTVarIO mempty
            <: #queue          <@=> newTQueueIO
            <: nil
  in Mix.run plugin $ Client.run dest

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
