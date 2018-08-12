module Git.Cmd where

import           RIO
import qualified RIO.Text as Text

import           Shelly   hiding (FilePath)

isExistRepoInWorkDir :: MonadIO m => FilePath -> Text -> Text -> m Bool
isExistRepoInWorkDir workDir owner repo =
  shelly $ test_d (workDir </> Text.unpack owner </> Text.unpack repo)

cloneRepo :: MonadIO m => Text -> FilePath -> Text -> Text -> m ()
cloneRepo token workDir owner repo = shelly $ chdir_p
  (workDir </> Text.unpack owner)
  (command1_ "git" [] "clone" [url])
  where
    url = mconcat ["https://", token, "@github.com/", owner, "/", repo, ".git"]

fetchRepo :: MonadIO m => FilePath -> Text -> Text -> m ()
fetchRepo workDir owner repo = shelly $ chdir_p
  (workDir </> Text.unpack owner </> Text.unpack repo)
  (command1_ "git" [] "fetch" [])
