{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE TypeOperators        #-}

module Git.Plantation.Cmd.Repo where

import           RIO
import qualified RIO.List                        as L
import qualified RIO.Map                         as Map
import qualified RIO.Text                        as Text
import qualified RIO.Vector                      as V

import           Data.Extensible
import           Git.Plantation.Data             (Problem, Repo, Team)
import qualified Git.Plantation.Data.Team        as Team
import           Git.Plantation.Env
import           GitHub.Data.Name                (mkName)
import           GitHub.Data.Repos               (newRepo, newRepoPrivate)
import           GitHub.Data.Webhooks            (NewRepoWebhook (..),
                                                  RepoWebhookEvent (..))
import qualified GitHub.Endpoints.Repos          as GitHub
import qualified GitHub.Endpoints.Repos.Webhooks as GitHub

import qualified Mix.Plugin.GitHub               as MixGitHub
import qualified Mix.Plugin.Shell                as MixShell
import           Shh                             ((&>))
import qualified Shh                             as Shell

type NewRepoCmd = Record
  '[ "repos" >: [Int]
   , "team"  >: Text
   , "skip_create_repo"   >: Bool
   , "skip_init_repo"     >: Bool
   , "skip_setup_webhook" >: Bool
   , "skip_init_ci"       >: Bool
   ]

type NewRepoSkipFlags = Record
  '[ "skip_create_repo"   >: Bool
   , "skip_init_repo"     >: Bool
   , "skip_setup_webhook" >: Bool
   , "skip_init_ci"       >: Bool
   ]

type DeleteRepoCmd = Record
  '[ "repos" >: [Int]
   , "team"  >: Text
   ]

type RepoCmdFields =
  '[ "repo" >: Int
   , "team" >: Text
   ]

type NewGitHubRepoCmd = Record RepoCmdFields

type InitGitHubRepoCmd = Record RepoCmdFields

type SetupWebhookCmd = Record RepoCmdFields

type InitCICmd = Record RepoCmdFields

type ResetRepoCmd = Record RepoCmdFields

actByProblemId :: (Team -> Problem -> Plant a) -> Team -> Int -> Plant ()
actByProblemId act team pid = do
  conf <- asks (view #config)
  let problem = L.find (\p -> p ^. #id == pid) $ conf ^. #problems
  case problem of
    Nothing       -> logError $ "repo is not found by problem id: " <> display pid
    Just problem' -> tryAnyWithLogError $ act team problem'

createRepo :: NewRepoSkipFlags -> Team -> Problem -> Plant ()
createRepo flags team problem = do
  logInfo $ mconcat
    [ "create repo: ", displayShow $ problem ^. #repo
    , " to team: ", displayShow $ team ^. #name
    ]
  info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
  unless (flags ^. #skip_create_repo)   $ createRepoInGitHub info team problem
  unless (flags ^. #skip_init_repo)     $ initRepoInGitHub info team problem
  unless (flags ^. #skip_setup_webhook) $ setupWebhook info
  unless (flags ^. #skip_init_ci)       $ initProblemCI info team problem

createRepoInGitHub :: Repo -> Team -> Problem -> Plant ()
createRepoInGitHub info team problem = do
  (owner, repo) <- splitRepoName <$> repoGithub info
  logInfo $ "create repo in github: " <> displayShow (owner <> "/" <> repo)
  resp <- MixGitHub.fetch $ \auth -> request owner auth
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
  token  <- MixGitHub.tokenText
  github <- repoGithub info
  let (owner, repo) = splitRepoName $ problem ^. #repo
      (_, teamRepo) = splitRepoName github
      teamUrl       = mconcat ["https://", token, "@github.com/", github, ".git"]
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  runGitForTeam team teamRepo teamUrl $ do
    Shell.ignoreFailure $ MixShell.git "branch" ["-D", "temp"]
    Shell.ignoreFailure $ MixShell.git "checkout" ["-b", "temp"]
    Shell.ignoreFailure $ MixShell.git "branch" $ "-D" : problem ^. #challenge_branches
    Shell.ignoreFailure $ MixShell.git "remote" ["add", "problem", problemUrl]
    MixShell.git "fetch" ["--all"]
    forM_ (problem ^. #challenge_branches) $
      \branch -> MixShell.git "checkout" ["-b", branch, "problem/" <> branch]
    MixShell.git "push" $ "-f" : "-u" : "origin" : problem ^. #challenge_branches
  logInfo $ "Success: create repo as " <> displayShow github

setupWebhook :: Repo -> Plant ()
setupWebhook info = do
  (owner, repo) <- splitRepoName <$> repoGithub info
  webhookConfig <- asks (view #webhook)
  logInfo $ "setup github webhook to repo: " <> displayShow (owner <> "/" <> repo)
  resp <- MixGitHub.fetch $ \auth -> GitHub.createRepoWebhook'
    auth (mkName Proxy owner) (mkName Proxy repo) (webhook webhookConfig)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (SetupWebhookError err info)
    Right _  -> logDebug "Success: setup GitHub Webhook to repository"
  where
    webhook conf = NewRepoWebhook
      { newRepoWebhookName   = "web"
      , newRepoWebhookConfig = Map.fromList conf
      , newRepoWebhookEvents = Just $ V.fromList [WebhookPushEvent]
      , newRepoWebhookActive = Just True
      }

initProblemCI :: Repo -> Team -> Problem -> Plant ()
initProblemCI info team problem = do
  token  <- MixGitHub.tokenText
  github <- repoGithub info
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  runGitForTeam team repo problemUrl $ do
    MixShell.git "checkout" [problem ^. #ci_branch]
    MixShell.git "pull" []
    Shell.ignoreFailure $ MixShell.git "branch" ["-D", team ^. #name]
    MixShell.git "checkout" ["-b", team ^. #name]
    MixShell.echo (Text.unpack github) &> Shell.Truncate (Text.unpack ciFileName)
    MixShell.git "add" [ciFileName]
    MixShell.git "commit" ["-m", "[CI SKIP] Add ci branch"]
    MixShell.git "push" ["-f", "-u", "origin", team ^. #name]
  logInfo $ "Success: create ci branch in " <> displayShow (problem ^. #repo)

resetRepo :: Repo -> Team -> Problem -> Plant ()
resetRepo info team problem = do
  let (_, repo) = splitRepoName $ problem ^. #repo
  local (over #work $ toTeamWork team) $ do
    local (over #work $ toWorkWith $ Text.unpack repo) $ MixShell.runShell MixShell.ls
    MixShell.runShell $ MixShell.rm ("-rf" :: String) (Text.unpack repo)
  initRepoInGitHub info team problem

pushForCI :: Team -> Problem -> Plant ()
pushForCI team problem = do
  token <- MixGitHub.tokenText
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  runGitForTeam team repo problemUrl $ do
    MixShell.git "checkout" [team ^. #name]
    MixShell.git "pull" []
    MixShell.git "commit" ["--allow-empty", "-m", "Empty Commit!!"]
    MixShell.git "push" ["origin", team ^. #name]
  logInfo "Success push"

deleteRepo :: Team -> Problem -> Plant ()
deleteRepo team problem = do
  logInfo $ mconcat
    [ "delete repo: ", displayShow $ problem ^. #repo
    , " to team: ", displayShow $ team ^. #name
    ]
  info <- Team.lookupRepo problem team `fromJustWithThrow` UndefinedTeamProblem team problem
  deleteRepoInGithub info
  deleteProblemCI team problem

deleteRepoInGithub :: Repo -> Plant ()
deleteRepoInGithub info = do
  (owner, repo) <- splitRepoName <$> repoGithub info
  logInfo $ "delete repo in github: " <> displayShow (owner <> "/" <> repo)
  resp <- MixGitHub.fetch $ \auth ->
    GitHub.deleteRepo auth (mkName Proxy owner) (mkName Proxy repo)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (DeleteRepoError err info)
    Right _  -> logDebug "Success: delete repository in GitHub"

deleteProblemCI :: Team -> Problem -> Plant ()
deleteProblemCI team problem = do
  token <- MixGitHub.tokenText
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]
  runGitForTeam team repo problemUrl $
    Shell.ignoreFailure $ MixShell.git "push"  [ "--delete", "origin", team ^. #name]
  logInfo $ "Success: delete ci branch in " <> displayShow (problem ^. #repo)

runGitForTeam :: Team -> Text -> Text -> Shell.Proc () -> Plant ()
runGitForTeam team repo url act = local (over #work $ toTeamWork team) $ do
  MixShell.runShell $ unlessM (MixShell.test_d repo') $ MixShell.git "clone" [url, repo]
  local (over #work $ toWorkWith repo') $ MixShell.runShell act
  where
    repo' = Text.unpack repo


-- |
-- helper functions

splitRepoName :: Text -> (Text, Text)
splitRepoName = fmap (Text.drop 1) . Text.span(/= '/')

repoGithub :: Repo -> Plant Text
repoGithub repo =
  Team.repoGithubPath repo `fromJustWithThrow` InvalidRepoConfig repo

ciFileName :: IsString s => s
ciFileName = "REPOSITORY"

toWorkWith :: FilePath -> FilePath -> FilePath
toWorkWith path = (<> "/" <> path)

toTeamWork :: Team -> FilePath -> FilePath
toTeamWork team = toWorkWith $ Text.unpack $ team ^. #name
