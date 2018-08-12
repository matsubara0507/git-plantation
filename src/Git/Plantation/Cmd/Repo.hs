{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Git.Plantation.Cmd.Repo where

import           RIO
import qualified RIO.List               as L
import qualified RIO.Text               as Text

import           Data.Extensible
import qualified Git.Cmd                as Git
import           Git.Plantation.Env     (Plant)
import           Git.Plantation.Problem (Problem)
import           Git.Plantation.Team    (Team)
import           GitHub.Data.Name       (mkName)
import           GitHub.Data.Repos      (newRepo)
import           GitHub.Endpoints.Repos (Auth (..))
import qualified GitHub.Endpoints.Repos as GitHub
import           Shelly                 hiding (FilePath)

type NewRepoCmd = Record
  '[ "repo"    >: Maybe Text
   , "team"    >: Text
   ]

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
  shelly $ chdir_p (workDir </> (team ^. #github)) (Git.clone [teamUrl, repo])
  shelly $ chdir_p (workDir </> (team ^. #github) </> repo) $ do
    Git.remote ["add", "problem", problemUrl]
    Git.fetch ["--all"]
    forM_ (problem ^. #challenge_branches) $
      \branch -> Git.checkout ["-b", branch, "problem/" <> branch]
    Git.push $ "-u" : "origin" : problem ^. #challenge_branches

createRepoByRepoName :: Team -> Text -> Plant ()
createRepoByRepoName team repoName = do
  conf <- asks (view #config)
  let problem = L.find (\p -> p ^. #repo_name == repoName) $ conf ^. #problems
  case problem of
    Nothing       -> logError $ "repo is not found: " <> display repoName
    Just problem' -> createRepo team problem'

createRepoInGitHub :: Team -> Problem -> Plant Text
createRepoInGitHub team problem = do
  token <- asks (view #token)
  let (_, repo) = splitRepoName $ problem ^. #repo_name
  logInfo $ "create repo in github: " <> displayShow (problem ^. #repo_name)
  resp <- liftIO $ GitHub.createOrganizationRepo'
    (OAuth token)
    (mkName Proxy $ team ^. #github)
    (newRepo $ mkName Proxy repo)
  case resp of
    Left err -> logError "Error: create github repo" >> fail (show err)
    Right _  -> pure (team ^. #github <> "/" <> repo)

splitRepoName :: Text -> (Text, Text)
splitRepoName = fmap (Text.drop 1) . Text.span(/= '/')

getTextToken :: Plant Text
getTextToken =
  Text.decodeUtf8' <$> asks (view #token) >>= \case
    Left  _ -> logError "cannot decode token to utf8." >> pure ""
    Right t -> pure t
