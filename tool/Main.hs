{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Paths_git_plantation as Meta
import           RIO

import           Data.Extensible
import           Data.Version         (Version)
import qualified Data.Version         as Version
import           Development.GitRev
import           GHC.TypeLits
import           Git.Plantation.Cmd
import           Options.Applicative

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> version Meta.version <**> helper)
         $ fullDesc
        <> header "taskpad - operate daily tasks"

options :: Parser Options
options = hsequence
    $ #verbose <@=> switch (long "verbose" <> short 'v' <> help "Enable verbose mode: verbosity level \"debug\"")
   <: #config  <@=> strOption (long "config" <> short 'c' <> value "config.yaml" <> metavar "PATH" <> help "Configuration file")
   <: #subcmd  <@=> subcmdParser
   <: nil

subcmdParser :: Parser SubCmd
subcmdParser = variantFrom
    $ #repo @= (strArgument (metavar "TEAM" <> help "Create repository to TEAM. If arg is all then create repo to all team.") `withInfo` "Create repository")
   <: nil

variantFrom ::
  Forall (KeyIs KnownSymbol) xs => RecordOf ParserInfo xs -> Parser (Variant xs)
variantFrom = subparser . subcmdVariant
  where
    subcmdVariant = hfoldMapWithIndexFor (Proxy @ (KeyIs KnownSymbol)) $ \m x ->
      let k = symbolVal (proxyAssocKey m)
      in command k ((EmbedAt m . Field . pure) <$> getField x)

instance Wrapper ParserInfo where
  type Repr ParserInfo a = ParserInfo a
  _Wrapper = id

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts = info (helper <*> opts) . progDesc

version :: Version -> Parser (a -> a)
version v = infoOption (showVersion v)
    $ long "version"
   <> help "Show version"

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
