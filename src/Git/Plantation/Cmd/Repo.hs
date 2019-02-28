{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Repo where

import           RIO
import qualified RIO.List                 as L
import qualified RIO.Text                 as Text

import           Data.Extensible
import qualified Git.Cmd                  as Git
import           Git.Plantation.Data      (Problem, Team)
import qualified Git.Plantation.Data.Team as Team
import           Git.Plantation.Env
import           GitHub.Data.Name         (mkName)
import           GitHub.Data.Repos        (newRepo)
import           GitHub.Endpoints.Repos   (Auth (..))
import qualified GitHub.Endpoints.Repos   as GitHub
import           Shelly                   hiding (FilePath)

type RepoFields =
  '[ "repo"    >: Maybe Text
   , "team"    >: Text
   ]

type NewRepoCmd = Record RepoFields

actByRepoName :: (Team -> Problem -> Plant ()) -> Team -> Text -> Plant ()
actByRepoName act team repoName = do
  conf <- asks (view #config)
  let problem = L.find (\p -> p ^. #repo_name == repoName) $ conf ^. #problems
  case problem of
    Nothing       -> logError $ "repo is not found: " <> display repoName
    Just problem' -> tryAnyWithLogError $ act team problem'

createRepo :: Team -> Problem -> Plant ()
createRepo team problem = do
  logInfo $ mconcat
    [ "create repo: ", displayShow $ problem ^. #repo_name
    , " to team: ", displayShow $ team ^. #name
    ]

  teamRepo <- createRepoInGitHub team problem
  token    <- getTextToken
  workDir  <- asks (view #work)
  let (owner, repo) = splitRepoName $ problem ^. #repo_name
      teamUrl       = mconcat ["https://", token, "@github.com/", teamRepo, ".git"]
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  shelly' $ chdir_p (workDir </> (team ^. #name)) (Git.cloneOrFetch teamUrl repo)
  shelly' $ chdir_p (workDir </> (team ^. #name) </> repo) $ do
    Git.remote ["add", "problem", problemUrl]
    Git.fetch ["--all"]
    forM_ (problem ^. #challenge_branches) $
      \branch -> Git.checkout ["-b", branch, "problem/" <> branch]
    Git.push $ "-u" : "origin" : problem ^. #challenge_branches
  logInfo $ "Success: create repo as " <> displayShow teamRepo

  shelly' $ chdir_p (workDir </> owner) (Git.cloneOrFetch problemUrl repo)
  shelly' $ chdir_p (workDir </> owner </> repo) $ do
    Git.checkout [problem ^. #ci_branch]
    Git.existBranch (team ^. #name) >>= \case
      False -> Git.checkout ["-b", team ^. #name]
      True  -> Git.checkout [team ^. #name]
    writefile ciFileName teamRepo
    Git.add [ciFileName]
    Git.commit ["-m", "[CI SKIP] Add ci branch"]
    Git.push ["-u", "origin", team ^. #name]
  logInfo $ "Success: create ci branch in " <> displayShow problemUrl

createRepoInGitHub :: Team -> Problem -> Plant Text
createRepoInGitHub team problem = do
  token <- asks (view #token)
  (owner, repo) <- fromJustWithThrow
    ((splitRepoName . view #github) <$> Team.lookupRepo problem team)
    (UndefinedTeamProblem team problem)
  logInfo $ "create repo in github: " <> displayShow (owner <> "/" <> repo)
  resp <- liftIO $ GitHub.createOrganizationRepo'
    (OAuth token)
    (mkName Proxy owner)
    (newRepo $ mkName Proxy repo)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (CreateRepoError err team problem)
    Right _  -> pure (owner <> "/" <> repo)

pushForCI :: Team -> Problem -> Plant ()
pushForCI team problem = do
  token   <- getTextToken
  workDir <- asks (view #work)
  let (owner, repo) = splitRepoName $ problem ^. #repo_name
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]
  shelly' $ chdir_p (workDir </> owner) (Git.cloneOrFetch problemUrl repo)
  shelly' $ chdir_p (workDir </> owner </> repo) $ do
    Git.fetch []
    Git.checkout [team ^. #name]
    Git.commit ["--allow-empty", "-m", "Empty Commit!!"]
    Git.push ["origin", team ^. #name]
  logInfo "Success push"

splitRepoName :: Text -> (Text, Text)
splitRepoName = fmap (Text.drop 1) . Text.span(/= '/')

getTextToken :: Plant Text
getTextToken =
  Text.decodeUtf8' <$> asks (view #token) >>= \case
    Left  _ -> logError "cannot decode token to utf8." >> pure ""
    Right t -> pure t

ciFileName :: IsString s => s
ciFileName = "REPOSITORY"
