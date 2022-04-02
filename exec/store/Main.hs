{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Paths_git_plantation     (version)
import           RIO
import qualified RIO.ByteString           as B
import qualified RIO.Text                 as Text

import           Configuration.Dotenv     (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version             (Version)
import qualified Data.Version             as Version
import           Development.GitRev
import           Git.Plantation           (Config, readConfig)
import           Git.Plantation.Store     (Store)
import qualified Git.Plantation.Store     as Store
import qualified Mix.Plugin               as Mix (withPlugin)
import qualified Mix.Plugin.Logger        as MixLogger
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

import           Orphans                  ()

main :: IO ()
main = withGetOpt "[options] [config-file]" opts $ \r args -> do
  _ <- tryIO $ loadFile defaultConfig
  case (r ^. #version, listToMaybe args) of
    (True, _)      -> B.putStr $ fromString (showVersion version) <> "\n"
    (_, Nothing)   -> error "please input config file path."
    (_, Just path) -> runServer r =<< readConfig path
  where
    opts = #port    @= portOpt
        <: #verbose @= verboseOpt
        <: #version @= versionOpt
        <: nil

type Options = Record
  '[ "port"    >: Int
   , "verbose" >: Bool
   , "version" >: Bool
   ]

portOpt :: OptDescr' Int
portOpt = optionReqArg
  (pure . fromMaybe 8090 . (readMaybe <=< listToMaybe))
  ['p'] ["port"] "PORT" "Set port to PORT instead of 8090."

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

runServer :: Options -> Config -> IO ()
runServer opts config = do
  let logConf   = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
      plugin    = hsequence
          $ #config  <@=> pure config
         <: #logger  <@=> MixLogger.buildPlugin logConf
         <: nil
  B.putStr $ "Listening on port " <> (fromString . show) (opts ^. #port) <> "\n"
  flip Mix.withPlugin plugin $ \env -> do
    store <- newTVarIO mempty
    Warp.run (opts ^. #port) $ app env store

app :: Store.Env -> TVar Store -> Application
app env store =
  serve Store.api $ hoistServer Store.api (runRIO env) (Store.server store)

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]

readListEnv :: Read a => String -> [a]
readListEnv = mapMaybe (readMaybe . show) . Text.split (== ',') . fromString
