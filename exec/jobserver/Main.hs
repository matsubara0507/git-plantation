{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Paths_git_plantation           (version)
import           RIO                            hiding (catch)
import qualified RIO.ByteString                 as B

import           Configuration.Dotenv           (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version                   (Version)
import qualified Data.Version                   as Version
import           Development.GitRev
import           Git.Plantation                 (Config, readConfig)
import qualified Git.Plantation.API.Job         as Job
import qualified Git.Plantation.Data.Job        as Job
import qualified Git.Plantation.Job.Server      as Job
import qualified Git.Plantation.Job.Store       as Job
import qualified Git.Plantation.Job.Worker      as Job
import qualified Mix
import qualified Mix.Plugin                     as Mix (withPlugin)
import qualified Mix.Plugin.Logger              as MixLogger
import qualified Mix.Plugin.Persist.Sqlite      as MixDB
import qualified Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS
import           Servant
import           System.Environment             (getEnv)

import           Orphans                        ()

main :: IO ()
main = withGetOpt "[options] [config-file]" opts $ \r args -> do
  _ <- tryIO $ loadFile defaultConfig
  if
    | r ^. #version -> B.putStr $ fromString (showVersion version) <> "\n"
    | r ^. #migrate -> runMigration r
    | otherwise ->
        case listToMaybe args of
          Nothing   -> error "please input config file path."
          Just path -> runServer r =<< readConfig path
  where
    opts = #port    @= portOpt
        <: #verbose @= verboseOpt
        <: #version @= versionOpt
        <: #migrate @= migrateOpt
        <: nil

type Options = Record
  '[ "port"    >: Int
   , "verbose" >: Bool
   , "version" >: Bool
   , "migrate" >: Bool
   ]

portOpt :: OptDescr' Int
portOpt = optionReqArg
  (pure . fromMaybe 8080 . (readMaybe <=< listToMaybe))
  ['p'] ["port"] "PORT" "Set port to PORT instead of 8080."

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

migrateOpt :: OptDescr' Bool
migrateOpt = optFlag [] ["migrate"] "Migrate SQLite tables"

runMigration :: Options -> IO ()
runMigration opts = do
  sqlitePath <- liftIO $ fromString  <$> getEnv "SQLITE_PATH"
  let logConf = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      plugin  = hsequence
          $ #logger <@=> MixLogger.buildPlugin logConf
         <: #sqlite <@=> MixDB.buildPluginWithoutPool sqlitePath
         <: nil
  Mix.run plugin (Job.migrate @ (Record '[ "logger" >: LogFunc, "sqlite" >: MixDB.Config ]))

type Env = Record
  '[ "config"  >: Config
   , "logger"  >: LogFunc
   , "workers" >: Job.Workers
   , "store"   >: Job.Store
   , "sqlite"  >: MixDB.Config
   ]

runServer :: Options -> Config -> IO ()
runServer opts config = do
  sqlitePath <- liftIO $ fromString  <$> getEnv "SQLITE_PATH"
  let logConf = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      plugin    = hsequence
          $ #config  <@=> pure config
         <: #logger  <@=> MixLogger.buildPlugin logConf
         <: #workers <@=> Job.newWorkers
         <: #store   <@=> Job.newStore
         <: #sqlite  <@=> MixDB.buildPlugin sqlitePath 2
         <: nil
  B.putStr $ "Listening on port " <> (fromString . show) (opts ^. #port) <> "\n"
  flip Mix.withPlugin plugin $ \env -> do
    runRIO env $ do
      jobs <- Job.selectAll
      Job.initializeStore jobs
    Warp.run (opts ^. #port) $
      websocketsOr
        WS.defaultConnectionOptions
        (runRIO env . Job.serveRunner')
        (app env)

app :: Env -> Application
app env =
  serve Job.api $ hoistServer Job.api (runRIO env) Job.server

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
