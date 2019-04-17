{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Options where

import           RIO
import           SubCmd

import           Data.Extensible
import           Data.Version        (Version)
import qualified Data.Version        as Version
import           Development.GitRev
import           GHC.TypeLits        hiding (Mod)
import           Git.Plantation.Cmd
import           Options.Applicative

type Options = Record
  '[ "verbose" >: Bool
   , "config"  >: FilePath
   , "work"    >: FilePath
   , "subcmd"  >: SubCmd
   ]

options :: Parser Options
options = hsequence
    $ #verbose <@=> switch (long "verbose" <> short 'v' <> help "Enable verbose mode: verbosity level \"debug\"")
   <: #config  <@=> strOption (long "config" <> short 'c' <> value "config.yaml" <> metavar "PATH" <> help "Configuration file")
   <: #work    <@=> strOption (long "work" <> value "~/.git-plantation" <> metavar "PATH" <> help "Work directory to exec git commands")
   <: #subcmd  <@=> subcmdParser
   <: nil

subcmdParser :: Parser SubCmd
subcmdParser = variantFrom
    $ #config  @= configCmdParser  `withInfo` "Manage git-plantation config file."
   <: #repo    @= repoCmdParser    `withInfo` "Manage team repository in GitHub."
   <: #member  @= memberCmdParser  `withInfo` "Manage team member with GitHub Account."
   <: #problem @= problemCmdParser `withInfo` "Manage problem repository in GitHub."
   <: nil

configCmdParser :: Parser ConfigCmd
configCmdParser = fmap ConfigCmd . variantFrom
    $ #verify @= pure () `withInfo` "Verify git-plantation config file."
   <: nil

repoCmdParser :: Parser RepoCmd
repoCmdParser = fmap RepoCmd . variantFrom
    $ #new           @= newRepoCmdParser    `withInfo` "Create repository for team."
   <: #new_github    @= singleRepoCmdParser `withInfo` "Create new repository for team in GitHub"
   <: #init_github   @= singleRepoCmdParser `withInfo` "Init repository for team in GitHub"
   <: #setup_webhook @= singleRepoCmdParser `withInfo` "Setup GitHub Webhook to team repository"
   <: #init_ci       @= singleRepoCmdParser `withInfo` "Init CI repository by team repository"
   <: #reset         @= singleRepoCmdParser `withInfo` "Reset repository for team"
   <: #delete        @= deleteRepoCmdParser `withInfo` "Delete repository for team."
   <: nil

memberCmdParser :: Parser MemberCmd
memberCmdParser = fmap MemberCmd . variantFrom
    $ #invite @= memberCmdArgParser `withInfo` "Invite member to team repository"
   <: #kick   @= memberCmdArgParser `withInfo` "Kick member from team repository"
   <: nil

memberCmdArgParser :: Parser MemberCmdArg
memberCmdArgParser = hsequence
    $ #team  <@=> strArgument (metavar "TEXT" <> help "Sets team that want to controll.")
   <: #repos <@=> option comma (long "repos" <> value [] <> metavar "ID" <> help "Sets reopsitory that want to controll by problem id.")
   <: #user  <@=> option (Just <$> str) (long "user" <> value Nothing <> metavar "TEXT" <> help "Sets user that want to controll.")
   <: nil

problemCmdParser :: Parser ProblemCmd
problemCmdParser = fmap ProblemCmd . variantFrom
    $ #show @= pure () `withInfo` "Display proble info."
   <: nil

newRepoCmdParser :: Parser NewRepoCmd
newRepoCmdParser = hsequence
    $ #repos              <@=> option comma (long "repos" <> value [] <> metavar "IDS" <> help "Sets reopsitory that want to controll by problem id.")
   <: #team               <@=> strArgument (metavar "TEXT" <> help "Sets team that want to controll.")
   <: #skip_create_repo   <@=> switch (long "skip_create_repo" <> help "Flag for skip create new repository in GitHub")
   <: #skip_init_repo     <@=> switch (long "skip_init_repo" <> help "Flag for skip init repository in GitHub")
   <: #skip_setup_webhook <@=> switch (long "skip_setup_webhook" <> help "Flag for skip setup GitHub Webhook to repository")
   <: #skip_init_ci       <@=> switch (long "skip_init_ci" <> help "Flag for skip init CI by repository")
   <: nil

singleRepoCmdParser :: Parser (Record RepoCmdFields)
singleRepoCmdParser = hsequence
    $ #repo <@=> option auto (long "repo" <> metavar "ID" <> help "Sets reopsitory that want to controll by problem id.")
   <: #team <@=> strArgument (metavar "TEXT" <> help "Sets team that want to controll.")
   <: nil

deleteRepoCmdParser :: Parser DeleteRepoCmd
deleteRepoCmdParser = hsequence
    $ #repos <@=> option comma (long "repos" <> value [] <> metavar "IDS" <> help "Sets reopsitory that want to controll by problem id.")
   <: #team  <@=> strArgument (metavar "TEXT" <> help "Sets team that want to controll.")
   <: nil

variantFrom ::
  Forall (KeyIs KnownSymbol) xs => RecordOf ParserInfo xs -> Parser (Variant xs)
variantFrom = subparser . subcmdVariant
  where
    subcmdVariant = hfoldMapWithIndexFor (Proxy @ (KeyIs KnownSymbol)) $ \m x ->
      let k = symbolVal (proxyAssocKey m)
      in command k (EmbedAt m . Field . pure <$> getField x)

instance Wrapper ParserInfo where
  type Repr ParserInfo a = ParserInfo a
  _Wrapper = id

-- |
-- support `--hoge 1,2,3`
comma :: Read a => ReadM [a]
comma = maybeReader (\s -> readMaybe $ "[" ++ s ++ "]")

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
