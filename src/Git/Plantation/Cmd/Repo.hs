{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE TypeOperators        #-}

module Git.Plantation.Cmd.Repo
  ( RepoCmdArg
  , RepoArg
  , NewRepoFlags
  , actForRepo
  , createRepo
  , createRepoInGitHub
  , initRepoInGitHub
  , setupDefaultBranch
  , setupWebhook
  , initProblemCI
  , resetRepo
  , pushForCI
  , deleteRepo
  , addGitHubTeam
  , splitRepoName
  , repoGithub
  ) where

import           RIO
import qualified RIO.Map                              as Map
import qualified RIO.Text                             as Text
import qualified RIO.Vector                           as V

import           Data.Coerce                          (coerce)
import           Data.Extensible
import           Git.Plantation.Cmd.Arg
import           Git.Plantation.Data                  (Problem, Repo, Team)
import qualified Git.Plantation.Data.Team             as Team
import           Git.Plantation.Env
import           GitHub.Data.Name                     (mkName)
import           GitHub.Data.Repos                    (newRepo, newRepoPrivate)
import           GitHub.Data.Webhooks                 (NewRepoWebhook (..),
                                                       RepoWebhookEvent (..))
import qualified GitHub.Endpoints.Organizations.Teams as GitHub
import qualified GitHub.Endpoints.Repos               as GitHub
import qualified GitHub.Endpoints.Repos.Webhooks      as GitHub

import qualified Mix.Plugin.GitHub                    as MixGitHub
import qualified Mix.Plugin.Logger.JSON               as Mix
import qualified Mix.Plugin.Shell                     as MixShell
import           Shh                                  ((&>))
import qualified Shh                                  as Shell
import qualified Shh.Command                          as Shell
import qualified Shh.Command.Git                      as Git

type RepoCmdArg = Record
  '[ "repos" >: [RepoId]
   , "team"  >: TeamId
   ]

type RepoArg = Record
  '[ "repo" >: Repo
   , "team" >: Team
   ]

type NewRepoFlags = Record
  '[ "skip_create_repo"          >: Bool
   , "skip_init_repo"            >: Bool
   , "skip_setup_default_branch" >: Bool
   , "skip_setup_webhook"        >: Bool
   , "skip_init_ci"              >: Bool
   ]

actForRepo :: (RepoArg -> Plant ()) -> RepoCmdArg -> Plant ()
actForRepo act args =
  findByIdWith (view #teams) (args ^. #team) >>= \case
    Nothing   -> Mix.logErrorR "not found by config" (toArgInfo $ args ^. #team)
    Just team -> do
      repos <- findRepos (args ^. #repos) team
      mapM_ act $ hsequence $ #repo <@=> repos <: #team <@=> pure team <: nil

findRepos :: [RepoId] -> Team -> Plant [Repo]
findRepos [] team = pure $ team ^. #repos
findRepos ids team = fmap catMaybes . forM ids $ \idx ->
  case findById idx (team ^. #repos) of
    Nothing -> Mix.logErrorR "not found by config" (toArgInfo idx) >> pure Nothing
    Just r  -> pure (Just r)

findProblemWithThrow :: ProblemId -> Plant Problem
findProblemWithThrow idx =
  findByIdWith (view #problems) idx >>= \case
    Nothing  -> throwIO $ UndefinedProblem (coerce idx)
    Just p   -> pure p

createRepo :: NewRepoFlags -> RepoArg -> Plant ()
createRepo flags args = do
  Mix.logInfoR "create team repository" args
  unless (flags ^. #skip_create_repo)          $ createRepoInGitHub args
  unless (flags ^. #skip_init_repo)            $ initRepoInGitHub args
  unless (flags ^. #skip_setup_default_branch) $ setupDefaultBranch args
  unless (flags ^. #skip_setup_webhook)        $ setupWebhook args
  unless (flags ^. #skip_init_ci)              $ initProblemCI args

createRepoInGitHub :: RepoArg -> Plant ()
createRepoInGitHub args = do
  (owner, repo) <- splitRepoName <$> repoGithub (args ^. #repo)
  logInfo $ "create repo in github: " <> displayShow (owner <> "/" <> repo)
  resp <- MixGitHub.fetch $ \auth -> request owner auth
    ((newRepo $ mkName Proxy repo) { newRepoPrivate = Just (args ^. #repo ^. #private) })
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (mkErr err)
    Right _  -> logInfo "Success: create repository in GitHub"
  where
    request owner =
      if Team.repoIsOrg (args ^. #repo) then
        flip GitHub.createOrganizationRepo' (mkName Proxy owner)
      else
        GitHub.createRepo'
    mkErr err = CreateRepoError err (args ^. #team) (args ^. #repo)

initRepoInGitHub :: RepoArg -> Plant ()
initRepoInGitHub args = do
  token   <- MixGitHub.tokenText
  github  <- repoGithub (args ^. #repo)
  problem <- findProblemWithThrow (ProblemId $ args ^. #repo ^. #problem)
  let (owner, repo) = splitRepoName $ problem ^. #repo
      (_, teamRepo) = splitRepoName github
      teamUrl       = mconcat ["https://", token, "@github.com/", github, ".git"]
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  execGitForTeam (args ^. #team) teamRepo False teamUrl $ do
    Shell.ignoreFailure $ Git.branch ["-D", "temp"]
    Shell.ignoreFailure $ Git.checkout ["-b", "temp"]
    Shell.ignoreFailure $ Git.branch $ "-D" : problem ^. #challenge_branches
    Shell.ignoreFailure $ Git.remote ["add", "problem", problemUrl]
    Git.fetch ["--all"]
    forM_ (problem ^. #challenge_branches) $
      \branch -> Git.checkout ["-b", branch, "problem/" <> branch]
    Git.push $ "-f" : "-u" : "origin" : problem ^. #challenge_branches
  logInfo $ "Success: create repo as " <> displayShow github

setupDefaultBranch :: RepoArg -> Plant ()
setupDefaultBranch args = do
  (owner, repo) <- splitRepoName <$> repoGithub (args ^. #repo)
  problem <- findProblemWithThrow (ProblemId $ args ^. #repo ^. #problem)
  resp <- MixGitHub.fetch $ \auth -> GitHub.editRepo auth
    (mkName Proxy owner)
    (mkName Proxy repo)
    (edit { GitHub.editDefaultBranch = Just $ problem ^. #default_branch })
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (DeleteRepoError err $ args ^. #repo)
    Right _  -> logInfo "Success: set default branch in GitHub"
  where
    edit = GitHub.EditRepo
      Nothing Nothing Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing Nothing Nothing

setupWebhook :: RepoArg -> Plant ()
setupWebhook args = do
  (owner, repo) <- splitRepoName <$> repoGithub (args ^. #repo)
  webhookConfig <- asks (view #webhook)
  logInfo $ "setup github webhook to repo: " <> displayShow (owner <> "/" <> repo)
  resp <- MixGitHub.fetch $ \auth -> GitHub.createRepoWebhook'
    auth (mkName Proxy owner) (mkName Proxy repo) (webhook webhookConfig)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (mkErr err)
    Right _  -> logInfo "Success: setup GitHub Webhook to repository"
  where
    webhook conf = NewRepoWebhook
      { newRepoWebhookName   = "web"
      , newRepoWebhookConfig = Map.fromList conf
      , newRepoWebhookEvents = Just $ V.fromList [WebhookPushEvent]
      , newRepoWebhookActive = Just True
      }
    mkErr err = SetupWebhookError err (args ^. #repo)

initProblemCI :: RepoArg -> Plant ()
initProblemCI args = do
  token   <- MixGitHub.tokenText
  github  <- repoGithub (args ^. #repo)
  problem <- findProblemWithThrow (ProblemId $ args ^. #repo ^. #problem)
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  execGitForTeam (args ^. #team) repo True problemUrl $ do
    Git.checkout [problem ^. #ci_branch]
    Git.pull []
    Shell.ignoreFailure $ Git.branch ["-D", args ^. #team ^. #name]
    Git.checkout ["-b", args ^. #team ^. #name]
    Shell.echo github &> Shell.Truncate (Text.unpack ciFileName)
    Git.add [ciFileName]
    Git.commit ["-m", "[CI SKIP] Add ci branch"]
    Git.push ["-f", "-u", "origin", args ^. #team ^. #name]
  logInfo $ "Success: create ci branch in " <> displayShow (problem ^. #repo)

resetRepo :: RepoArg -> Plant ()
resetRepo args = do
  problem <- findProblemWithThrow (ProblemId $ args ^. #repo ^. #problem)
  let (_, repo) = splitRepoName $ problem ^. #repo
  local (over #work $ toTeamWork (args ^. #team) False) $ do
    local (over #work $ toWorkWith $ Text.unpack repo) $ MixShell.exec (Shell.ls [] ".")
    MixShell.exec $ Shell.rm ["-rf"] repo
  initRepoInGitHub args

pushForCI :: Team -> Problem -> Plant ()
pushForCI team problem = do
  token <- MixGitHub.tokenText
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  execGitForTeam team repo True problemUrl $ do
    Git.checkout [team ^. #name]
    Git.pull []
    Git.commit ["--allow-empty", "-m", "Empty Commit!!"]
    Git.push ["origin", team ^. #name]
  logInfo "Success push"

deleteRepo :: RepoArg -> Plant ()
deleteRepo args = do
  Mix.logInfoR "delete team repository" args
  problem <- findProblemWithThrow (ProblemId $ args ^. #repo ^. #problem)
  deleteRepoInGithub (args ^. #repo)
  deleteProblemCI (args ^. #team) problem

deleteRepoInGithub :: Repo -> Plant ()
deleteRepoInGithub info = do
  (owner, repo) <- splitRepoName <$> repoGithub info
  logInfo $ "delete repo in github: " <> displayShow (owner <> "/" <> repo)
  resp <- MixGitHub.fetch $ \auth ->
    GitHub.deleteRepo auth (mkName Proxy owner) (mkName Proxy repo)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (DeleteRepoError err info)
    Right _  -> logInfo "Success: delete repository in GitHub"

deleteProblemCI :: Team -> Problem -> Plant ()
deleteProblemCI team problem = do
  token <- MixGitHub.tokenText
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]
  execGitForTeam team repo True problemUrl $
    Shell.ignoreFailure $ Git.push  [ "--delete", "origin", team ^. #name]
  logInfo $ "Success: delete ci branch in " <> displayShow (problem ^. #repo)

execGitForTeam :: Team -> Text -> Bool -> Text -> Shell.Proc () -> Plant ()
execGitForTeam team repo isProblem url act =
  local (over #work $ toTeamWork team isProblem) $ do
    MixShell.exec $ unlessM (Shell.test_d repo') $ Git.clone [url, repo]
    local (over #work $ toWorkWith repo') $ MixShell.exec act
  where
    repo' = Text.unpack repo

addGitHubTeam :: RepoArg -> Plant ()
addGitHubTeam args = case (args ^. #team ^. #org, args ^. #repo ^. #only) of
  (Nothing, _)   -> logError "Undefined GitHub org in team"
  (_, Nothing)   -> logError "Undefined 'only' option in team repo."
  (Just org, Just name)
    | valid name -> addGitHubTeam' org name (args ^. #repo)
    | otherwise  -> logError $ display $ "Undefined GitHub team in team: " <> name
  where
    valid name = name `elem` args ^. #team ^. #gh_teams

    addGitHubTeam' :: Text -> Text -> Repo -> Plant ()
    addGitHubTeam' org name repo = do
      resp <- MixGitHub.fetch $ \auth -> GitHub.teamInfoByName' (Just auth)
        (mkName Proxy org)
        (mkName Proxy name)
      team <- case resp of
        Left err   -> logDebug (displayShow err) >> throwIO (failure err org name)
        Right team -> pure team
      (owner, repoName) <- splitRepoName <$> repoGithub repo
      resp' <- MixGitHub.fetch $ \auth -> GitHub.addOrUpdateTeamRepo' auth
        (GitHub.teamId team)
        (mkName Proxy $ owner)
        (mkName Proxy $ repoName)
        GitHub.PermissionPush
      case resp' of
        Left err -> logDebug (displayShow err) >> throwIO (failure err org name)
        Right _  -> logInfo $ display $ "Success: add repository to GitHub team: " <> name

    failure err org name =
      AddRepoToGitHubTeamError err org name $ args ^. #repo

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

toTeamWork :: Team -> Bool -> FilePath -> FilePath
toTeamWork team isProblem =
  toWorkWith $ Text.unpack $ team ^. #name <> if isProblem then "/problem" else ""
