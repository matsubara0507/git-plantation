{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

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
import           Git.Plantation.Cmd.Env               (CmdEnv)
import           Git.Plantation.Data                  (Problem, Repo, Team,
                                                       User)
import qualified Git.Plantation.Data.Problem          as Problem
import qualified Git.Plantation.Data.Team             as Team
import qualified Git.Plantation.Data.User             as User
import           Git.Plantation.Env
import           GitHub.Data.Name                     (mkName)
import           GitHub.Data.Repos                    (newRepo, newRepoPrivate)
import           GitHub.Data.Webhooks                 (NewRepoWebhook (..),
                                                       RepoWebhookEvent (..))
import qualified GitHub.Endpoints.Organizations.Teams as GitHub
import qualified GitHub.Endpoints.Repos               as GitHub
import qualified GitHub.Endpoints.Repos.Webhooks      as GitHub

import qualified Git.Cmd                              as Git
import qualified Mix.Plugin.GitHub                    as MixGitHub
import qualified Mix.Plugin.Logger.JSON               as Mix
import qualified Mix.Plugin.Shell                     as MixShell
import qualified Shelly                               as Shell

type RepoCmdArg = Record
  '[ "repos" >: [RepoId]
   , "team"  >: Team.Id
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

actForRepo :: CmdEnv env => (RepoArg -> RIO env ()) -> RepoCmdArg -> RIO env ()
actForRepo act args =
  findByIdWith (view #teams) (args ^. #team) >>= \case
    Nothing   -> Mix.logErrorR "not found by config" (toArgInfo $ args ^. #team)
    Just team -> do
      repos <- findRepos (args ^. #repos) team
      mapM_ act $ hsequence $ #repo <@=> repos <: #team <@=> pure team <: nil

findRepos :: CmdEnv env => [RepoId] -> Team -> RIO env [Repo]
findRepos [] team = pure $ team ^. #repos
findRepos ids team = fmap catMaybes . forM ids $ \idx ->
  case findById idx (team ^. #repos) of
    Nothing -> Mix.logErrorR "not found by config" (toArgInfo idx) >> pure Nothing
    Just r  -> pure (Just r)

findProblemWithThrow :: CmdEnv env => Problem.Id -> RIO env Problem
findProblemWithThrow idx =
  findByIdWith (view #problems) idx >>= \case
    Nothing -> throwIO $ UndefinedProblem (coerce idx)
    Just p  -> pure p

createRepo :: (HasWebhookConfig env, CmdEnv env) => NewRepoFlags -> RepoArg -> RIO env ()
createRepo flags args = do
  Mix.logInfoR "create team repository" args
  unless (flags ^. #skip_create_repo)          $ createRepoInGitHub args
  unless (flags ^. #skip_init_repo)            $ initRepoInGitHub args
  unless (flags ^. #skip_setup_default_branch) $ setupDefaultBranch args
  unless (flags ^. #skip_setup_webhook)        $ setupWebhook args
  unless (flags ^. #skip_init_ci)              $ initProblemCI args

createRepoInGitHub :: CmdEnv env => RepoArg -> RIO env ()
createRepoInGitHub args = do
  (owner, repo) <- splitRepoName <$> repoGithub (args ^. #repo)
  logInfo $ "create repo in github: " <> displayShow (owner <> "/" <> repo)
  resp <- MixGitHub.fetch $ request owner
    ((newRepo $ mkName Proxy repo) { newRepoPrivate = Just (args ^. #repo ^. #private) })
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (mkErr err)
    Right _  -> logInfo "Success: create repository in GitHub"
  where
    request owner =
      if Team.repoIsOrg (args ^. #repo) then
        GitHub.createOrganizationRepoR (mkName Proxy owner)
      else
        GitHub.createRepoR
    mkErr err = CreateRepoError err (args ^. #team) (args ^. #repo)

initRepoInGitHub :: CmdEnv env => RepoArg -> RIO env ()
initRepoInGitHub args = do
  token   <- MixGitHub.tokenText
  github  <- repoGithub (args ^. #repo)
  problem <- findProblemWithThrow (args ^. #repo ^. #problem)
  let (owner, repo) = splitRepoName $ problem ^. #repo
      (_, teamRepo) = splitRepoName github
      teamUrl       = mconcat ["https://", token, "@github.com/", github, ".git"]
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  execGitForTeam (args ^. #team) teamRepo False teamUrl $ do
    Shell.errExit False $ Git.branch ["-D", "temp"]
    Shell.errExit False $ Git.checkout ["-b", "temp"]
    Shell.errExit False $ Git.branch $ "-D" : problem ^. #challenge_branches
    Shell.errExit False $ Git.remote ["add", "problem", problemUrl]
    Git.fetch ["--all"]
    forM_ (problem ^. #challenge_branches) $
      \branch -> Git.checkout ["-b", branch, "problem/" <> branch]
    Git.push $ "-f" : "-u" : "origin" : problem ^. #challenge_branches
  logInfo $ "Success: create repo as " <> displayShow github

setupDefaultBranch :: CmdEnv env => RepoArg -> RIO env ()
setupDefaultBranch args = do
  (owner, repo) <- splitRepoName <$> repoGithub (args ^. #repo)
  problem <- findProblemWithThrow (args ^. #repo ^. #problem)
  resp <- MixGitHub.fetch $ GitHub.editRepoR
    (mkName Proxy owner)
    (mkName Proxy repo)
    $ edit { GitHub.editDefaultBranch = Just $ problem ^. #default_branch
           , GitHub.editPrivate = Just (args ^. #repo ^. #private)
           }
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (DeleteRepoError err $ args ^. #repo)
    Right _  -> logInfo "Success: set default branch in GitHub"
  where
    edit = GitHub.EditRepo
      Nothing Nothing Nothing Nothing Nothing Nothing
      Nothing Nothing Nothing Nothing Nothing Nothing

setupWebhook :: (HasWebhookConfig env, CmdEnv env) => RepoArg -> RIO env ()
setupWebhook args = do
  (owner, repo) <- splitRepoName <$> repoGithub (args ^. #repo)
  webhookConfig <- askWebhookConfig
  logInfo $ "setup github webhook to repo: " <> displayShow (owner <> "/" <> repo)
  resp <- MixGitHub.fetch $
    GitHub.createRepoWebhookR (mkName Proxy owner) (mkName Proxy repo) (webhook webhookConfig)
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

initProblemCI :: CmdEnv env => RepoArg -> RIO env ()
initProblemCI args = do
  token   <- MixGitHub.tokenText
  github  <- repoGithub (args ^. #repo)
  problem <- findProblemWithThrow (args ^. #repo ^. #problem)
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  execGitForTeam (args ^. #team) repo True problemUrl $ do
    Git.checkout [problem ^. #ci_branch]
    Git.pull []
    Shell.errExit False $ Git.branch ["-D", coerce $ args ^. #team ^. #name]
    Git.checkout ["-b", coerce $ args ^. #team ^. #name]
    Shell.writefile ciFileName github
    Git.add [ciFileName]
    Git.commit ["-m", "[CI SKIP] Add ci branch"]
    Git.push ["-f", "-u", "origin", coerce $ args ^. #team ^. #name]
  logInfo $ "Success: create ci branch in " <> displayShow (problem ^. #repo)

resetRepo :: CmdEnv env => RepoArg -> RIO env ()
resetRepo args = do
  problem <- findProblemWithThrow (args ^. #repo ^. #problem)
  let (_, repo) = splitRepoName $ problem ^. #repo
  local (over MixShell.workL $ toTeamWork (args ^. #team) False) $ do
    local (over MixShell.workL $ toWorkWith $ Text.unpack repo) $ MixShell.exec (Shell.ls "." >> pure ())
    MixShell.exec $ Shell.rm_rf $ Shell.fromText repo
  initRepoInGitHub args

pushForCI :: CmdEnv env => Team -> Problem -> Maybe User -> RIO env ()
pushForCI team problem user = do
  token <- MixGitHub.tokenText
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

  execGitForTeam team repo True problemUrl $ do
    Git.checkout [coerce $ team ^. #name]
    Git.pull []
    Git.commit ["--allow-empty", "-m", "pushed by: @" <> userAccount]
    Git.push ["origin", coerce $ team ^. #name]
  logInfo "Success push"
  where
    userAccount = coerce $ maybe "" (view #github) user

deleteRepo :: CmdEnv env => RepoArg -> RIO env ()
deleteRepo args = do
  Mix.logInfoR "delete team repository" args
  problem <- findProblemWithThrow (args ^. #repo ^. #problem)
  deleteRepoInGithub (args ^. #repo)
  deleteProblemCI (args ^. #team) problem

deleteRepoInGithub :: CmdEnv env => Repo -> RIO env ()
deleteRepoInGithub info = do
  (owner, repo) <- splitRepoName <$> repoGithub info
  logInfo $ "delete repo in github: " <> displayShow (owner <> "/" <> repo)
  resp <- MixGitHub.fetch $
    GitHub.deleteRepoR (mkName Proxy owner) (mkName Proxy repo)
  case resp of
    Left err -> logDebug (displayShow err) >> throwIO (DeleteRepoError err info)
    Right _  -> logInfo "Success: delete repository in GitHub"

deleteProblemCI :: CmdEnv env => Team -> Problem -> RIO env ()
deleteProblemCI team problem = do
  token <- MixGitHub.tokenText
  let (owner, repo) = splitRepoName $ problem ^. #repo
      problemUrl    = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]
  execGitForTeam team repo True problemUrl $
    Shell.errExit False $ Git.push  [ "--delete", "origin", coerce $ team ^. #name]
  logInfo $ "Success: delete ci branch in " <> displayShow (problem ^. #repo)

execGitForTeam :: CmdEnv env => Team -> Text -> Bool -> Text -> MixShell.Sh () -> RIO env ()
execGitForTeam team repo isProblem url act =
  local (over MixShell.workL $ toTeamWork team isProblem) $ do
    MixShell.exec $ unlessM (Shell.test_d $ fromString repo') $ Git.clone [url, repo]
    local (over MixShell.workL $ toWorkWith repo') $ MixShell.exec act
  where
    repo' = Text.unpack repo

addGitHubTeam :: CmdEnv env => RepoArg -> RIO env ()
addGitHubTeam args = case (args ^. #team ^. #org, args ^. #repo ^. #only) of
  (Nothing, _)   -> logError "Undefined GitHub org in team"
  (_, Nothing)   -> logError "Undefined 'only' option in team repo."
  (Just org, Just name)
    | valid name -> addGitHubTeam' org name (args ^. #repo)
    | otherwise  -> logError $ display $ "Undefined GitHub team in team: " <> name
  where
    valid name = name `elem` args ^. #team ^. #gh_teams

    addGitHubTeam' :: CmdEnv env => Text -> Text -> Repo -> RIO env ()
    addGitHubTeam' org name repo = do
      resp <- MixGitHub.fetch $ GitHub.teamInfoByNameR
        (mkName Proxy org)
        (mkName Proxy name)
      team <- case resp of
        Left err   -> logDebug (displayShow err) >> throwIO (failure err org name)
        Right team -> pure team
      (owner, repoName) <- splitRepoName <$> repoGithub repo
      resp' <- MixGitHub.fetch $ GitHub.addOrUpdateTeamRepoR
        (GitHub.teamId team)
        (mkName Proxy owner)
        (mkName Proxy repoName)
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

repoGithub :: CmdEnv env => Repo -> RIO env Text
repoGithub repo =
  Team.repoGithubPath repo `fromJustWithThrow` InvalidRepoConfig repo

ciFileName :: IsString s => s
ciFileName = "REPOSITORY"

toWorkWith :: FilePath -> FilePath -> FilePath
toWorkWith path = (<> "/" <> path)

toTeamWork :: Team -> Bool -> FilePath -> FilePath
toTeamWork team isProblem =
  toWorkWith $ Text.unpack $ coerce (team ^. #name) <> if isProblem then "/problem" else ""
