{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Git.Plantation.Data.Job where

import           RIO
import           RIO.Time

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Binary                     (Binary)
import           Data.Coerce                     (coerce)
import           Data.Extensible
import           Database.Esqueleto.Experimental hiding (set, (^.))
import qualified Database.Esqueleto.Experimental as DB
import           Database.Persist.TH
import qualified Mix.Plugin.Persist.Sqlite       as MixDB
import           Web.HttpApiData                 (FromHttpApiData)

newtype Id = Id Int64
  deriving newtype (Show, Eq, Ord, Num, Binary, FromJSON, ToJSON, Display)

newtype Name = Name String
  deriving newtype (Show, Eq, Ord, Binary, FromJSON, ToJSON, FromHttpApiData)

type Job = Record
  '[ "id"      >: Id
   , "name"    >: Name
   , "queuing" >: Bool
   , "running" >: Bool
   , "success" >: Bool
   ]

new :: Name -> Id -> Job
new jobName jid
    = #id      @= jid
   <: #name    @= jobName
   <: #queuing @= True
   <: #running @= False
   <: #success @= False
   <: nil

type Config = Record
  '[ "name"    >: Name
   , "image"   >: String
   , "command" >: Maybe String
   ]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
JobData
  name String
  queuing Bool
  running Bool
  success Bool
  created UTCTime default=CURRENT_TIME
  updated UTCTime default=CURRENT_TIME
  deriving Show
|]

fromData :: Int64 -> JobData -> Job
fromData jid JobData{..}
    = #id      @= coerce jid
   <: #name    @= coerce jobDataName
   <: #queuing @= jobDataQueuing
   <: #running @= jobDataRunning
   <: #success @= jobDataSuccess
   <: nil

type SQLitable m env = (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)

create :: SQLitable m env => Name -> m Job
create name = MixDB.run $ do
  jid <- insert dat
  pure $ job & #id `set` coerce jid
  where
    job = new name 0
    dat = JobData
      (coerce $ job ^. #name)
      (job ^. #queuing)
      (job ^. #running)
      (job ^. #success)
      zeroTime
      zeroTime
    zeroTime = UTCTime (ModifiedJulianDay 0) 0

updateToRunning, updateToSuccess, updateToFailure :: SQLitable m env => Id -> m ()
updateToRunning jid = MixDB.run $
  update $ \job -> do
    DB.set job [ JobDataQueuing =. val False, JobDataRunning =. val True ]
    where_ $ job DB.^. JobDataId ==. val (coerce jid)
updateToSuccess jid = MixDB.run $
  update $ \job -> do
    DB.set job [ JobDataRunning =. val False, JobDataSuccess =. val True ]
    where_ $ job DB.^. JobDataId ==. val (coerce jid)
updateToFailure jid = MixDB.run $
  update $ \job -> do
    DB.set job [ JobDataRunning =. val False, JobDataSuccess =. val False ]
    where_ $ job DB.^. JobDataId ==. val (coerce jid)

selectAll :: SQLitable m env => m [Job]
selectAll = MixDB.run $ do
  jobs <- select $ from $ table @JobData
  pure $ map (\dat -> fromData (fromSqlKey $ entityKey dat) $ entityVal dat) jobs
