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
import           Git.Plantation.Data      (Problem, Repo, Team)
import qualified Git.Plantation.Data.Team as Team
import           Git.Plantation.Env
import           GitHub.Data.Name         (mkName)
import           GitHub.Data.Repos        (newRepo, newRepoPrivate)
import           GitHub.Endpoints.Repos   (Auth (..))
import qualified GitHub.Endpoints.Repos   as GitHub
import           Shelly                   hiding (FilePath)

type NewRepoCmd = Record
  '[ "repo" >: Maybe Text
   , "team" >: Text
   ]

type RepoCmdFields =
  '[ "repo" >: Text
   , "team" >: Text
   ]

type NewGitHubRepoCmd = Record RepoCmdFields

type InitGitHubRepoCmd = Record RepoCmdFields

type InitCICmd = Record RepoCmdFields

type ResetRepoCmd = Record RepoCmdFields

actByRepoName :: (Team -> Problem -> Plant a) -> Team -> Text -> Plant ()
actByRepoName act team repoName = do
  conf <- asks (view #config)
  let problem = L.find (\p -> p ^. #repo == repoName) $ conf ^. #problems
  case problem of
    Nothing       -> logError $ "repo is not found: " <> display repoName
    Just problem' -> tryAnyWithLogError $ act team problem'

createRepo :: Team -> Problem -> Plant ()
createRepo team problem = do
  logInfo $ mconcat
    [ "create repo: ", displayShow $ problem ^. #repo
    , " to team: ", displayShow $ team ^. #name
    ]
  info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
  createRepoInGitHub info team problem
  initRepoInGitHub info team problem
  initProblemCI info team problem

createRepoInGitHub :: Repo -> Team -> Problem -> Plant ()
createRepoInGitHub info team problem = do
  (owner, repo) <- splitRepoName <$> repoGithub info
  token <- asks (view #token)
  logInfo $ "create repo in github: " <> displayShow (owner <> "/" <> repo)
  resp <- liftIO $ request owner (OAuth token)
    ((newRepo $ mkName Proxy repo) { newRepoPrivate = Just (info ^. #private) })
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (CreateRepoError err team problem)
    Right _  -> logInfo "Success: create repository in GitHub"
  where
    request owner =
      if Team.repoIsOrg info then
        flip GitHub.createOrganizationRepo' (mkName Proxy owner)
      else
        GitHub.createRepo'

initRepoInGitHub :: Repo -> Team -> Problem -> Plant ()
initRepoInGitHub info team problem = do
  token   <- getTextToken
  workDir <- asks (view #work)
  github  <- repoGithub info
  let (owner, repo) = splitRepoName $ problem ^. #repo
      teamUrl       = mconcat ["https://", token, "@github.com/", github, ".git"]
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  shelly' $ chdir_p (workDir </> (team ^. #name)) (Git.cloneOrFetch teamUrl repo)
  shelly' $ chdir_p (workDir </> (team ^. #name) </> repo) $ do
    Git.checkout [ "-b", "temp"]
    errExit False $ Git.branch $ "-D" : problem ^. #challenge_branches
    Git.remote ["add", "problem", problemUrl]
    Git.fetch ["--all"]
    forM_ (problem ^. #challenge_branches) $
      \branch -> Git.checkout ["-b", branch, "problem/" <> branch]
    Git.push $ "-f" : "-u" : "origin" : problem ^. #challenge_branches
    errExit False $ Git.branch ["-D", "temp"]
  logInfo $ "Success: create repo as " <> displayShow github

initProblemCI :: Repo -> Team -> Problem -> Plant ()
initProblemCI info team problem = do
  token   <- getTextToken
  workDir <- asks (view #work)
  github  <- repoGithub info
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  shelly' $ chdir_p (workDir </> owner) (Git.cloneOrFetch problemUrl repo)
  shelly' $ chdir_p (workDir </> owner </> repo) $ do
    Git.checkout [problem ^. #ci_branch]
    Git.existBranch (team ^. #name) >>= \case
      False -> Git.checkout ["-b", team ^. #name]
      True  -> Git.checkout [team ^. #name]
    writefile ciFileName github
    Git.add [ciFileName]
    Git.commit ["-m", "[CI SKIP] Add ci branch"]
    Git.push ["-u", "origin", team ^. #name]
  logInfo $ "Success: create ci branch in " <> displayShow problemUrl

resetRepo :: Repo -> Team -> Problem -> Plant ()
resetRepo info team problem = do
  workDir <- asks (view #work)
  let (_, repo) = splitRepoName $ problem ^. #repo
  paths <- shelly' $ chdir_p (workDir </> (team ^. #name) </> repo) $ ls "."
  logDebug $ "Remove file: " <> display (Text.intercalate " " $ map toTextIgnore paths)
  shelly' $ chdir_p (workDir </> (team ^. #name)) $ rm_rf (fromText repo)
  initRepoInGitHub info team problem

pushForCI :: Team -> Problem -> Plant ()
pushForCI team problem = do
  token   <- getTextToken
  workDir <- asks (view #work)
  let (owner, repo) = splitRepoName $ problem ^. #repo
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

repoGithub :: Repo -> Plant Text
repoGithub repo =
  Team.repoGithubPath repo `fromJustWithThrow` InvalidRepoConfig repo

ciFileName :: IsString s => s
ciFileName = "REPOSITORY"
