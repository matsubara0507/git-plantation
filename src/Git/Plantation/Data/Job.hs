{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Git.Plantation.Data.Job where

import           RIO
import qualified RIO.List                        as List
import           RIO.Time

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Binary                     (Binary)
import           Data.Coerce                     (coerce)
import           Data.Extensible
import           Database.Esqueleto.Experimental hiding (set, (^.))
import qualified Database.Esqueleto.Experimental as DB
import           Database.Persist.TH
import           Git.Plantation.Data.Problem     (Problem)
import qualified Git.Plantation.Data.Problem     as Problem
import           Git.Plantation.Data.Team        (Team)
import qualified Git.Plantation.Data.Team        as Team
import           Git.Plantation.Data.User        (User)
import qualified Git.Plantation.Data.User        as User
import qualified Mix.Plugin.Persist.Sqlite       as MixDB


newtype Id = Id Int64
  deriving newtype (Show, Eq, Ord, Num, Binary, FromJSON, ToJSON, Display)

type Job = Record
  '[ "id"      >: Id
   , "problem" >: Problem.Id
   , "team"    >: Team.Id
   , "author"  >: User.GitHubId
   , "queuing" >: Bool
   , "running" >: Bool
   , "success" >: Bool
   , "created" >: Int64
   ]

new :: Problem.Id -> Team.Id -> User.GitHubId -> Id -> Job
new pid tid uid jid
    = #id      @= jid
   <: #problem @= pid
   <: #team    @= tid
   <: #author  @= uid
   <: #queuing @= True
   <: #running @= False
   <: #success @= False
   <: #created @= 0
   <: nil

type Config = Record
  '[ "problems" >: [Problem]
   , "teams"    >: [Team]
   , "image"    >: Text
   ]

emptyConfig :: Config
emptyConfig
    = #problems @= mempty
   <: #teams    @= mempty
   <: #image    @= mempty
   <: nil

findProblem :: Config -> Job -> Maybe Problem
findProblem config job =
  List.find (\p -> p ^. #id == job ^. #problem) $ config ^. #problems

findTeam :: Config -> Job -> Maybe Team
findTeam config job =
  List.find (\t -> t ^. #id == job ^. #team) $ config ^. #teams

findUser :: Team -> Job -> Maybe User
findUser team job =
  List.find (\u -> u ^. #github == job ^. #author) $ team ^. #member

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
JobData
  problem Int
  team    Text
  author  Text
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
   <: #problem @= coerce jobDataProblem
   <: #team    @= coerce jobDataTeam
   <: #author  @= coerce jobDataAuthor
   <: #queuing @= jobDataQueuing
   <: #running @= jobDataRunning
   <: #success @= jobDataSuccess
   <: #created @= utcTimeToInt64 jobDataCreated
   <: nil

utcTimeToInt64 :: UTCTime -> Int64
utcTimeToInt64 (UTCTime (ModifiedJulianDay d) t)
  = 86400 * (fromIntegral d - unixEpochDay)
    + fromIntegral (diffTimeToPicoseconds t) `div` 1_000_000_000_000
  where
    unixEpochDay = 40587

type SQLitable m env = (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)

create :: SQLitable m env => Problem.Id -> Team.Id -> User.GitHubId -> m Job
create pid tid uid = MixDB.run $ do
  currentTime <- getCurrentTime
  let dat = JobData
        (coerce $ job ^. #problem)
        (coerce $ job ^. #team)
        (coerce $ job ^. #author)
        (job ^. #queuing)
        (job ^. #running)
        (job ^. #success)
        currentTime
        currentTime
  jid <- insert dat
  pure $ job & #id `set` coerce jid
  where
    job = new pid tid uid 0

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
