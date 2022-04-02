{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

module Git.Plantation.API.Store where

import           RIO
import qualified RIO.Text                as Text

import           Data.Aeson              (toJSON)
import           Git.Plantation.Data.Job (Job)
import qualified Git.Plantation.Data.Job as Job
import           Git.Plantation.Env      (Plant)
import qualified Network.Wreq            as W

fetchJobsByStore :: Plant [Job]
fetchJobsByStore = do
  url  <- Text.unpack <$> asks (view #store)
  resp <- W.asJSON =<< liftIO (W.get url)
  pure $ resp ^. W.responseBody

putJobToStore :: Job -> Plant ()
putJobToStore job = do
  url <- Text.unpack <$> asks (view #store)
  _   <- liftIO (W.post url $ toJSON job)
  pure ()

initializeStore :: Plant ()
initializeStore = do
  jobs <- Job.selectAll
  url <- Text.unpack <$> asks (view #store)
  _   <- liftIO (W.post (url <> "/initialize") $ toJSON jobs)
  pure ()
