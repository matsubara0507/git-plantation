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
import qualified Database.Persist                as Persist
import           Database.Persist.TH
import           Git.Plantation.Data.Problem     (Problem)
import qualified Git.Plantation.Data.Problem     as Problem
import           Git.Plantation.Data.Team        (Team)
import qualified Git.Plantation.Data.Team        as Team
import           Git.Plantation.Data.User        (User)
import qualified Git.Plantation.Data.User        as User
import qualified Mix.Plugin.Persist.Sqlite       as MixDB
import           Web.HttpApiData                 (FromHttpApiData)

newtype Id = Id Int64
  deriving newtype (Show, Eq, Ord, Num, Binary, FromJSON, ToJSON, FromHttpApiData, Display)

type Job = Record
  '[ "id"      >: Id
   , "problem" >: Problem.Id
   , "team"    >: Team.Id
   , "author"  >: Maybe User.GitHubId
   , "queuing" >: Bool
   , "running" >: Bool
   , "success" >: Bool
   , "stdout"  >: Text
   , "stderr"  >: Text
   , "created" >: Int64
   ]

new :: Problem.Id -> Team.Id -> Maybe User.GitHubId -> Id -> Job
new pid tid uid jid
    = #id      @= jid
   <: #problem @= pid
   <: #team    @= tid
   <: #author  @= uid
   <: #queuing @= True
   <: #running @= False
   <: #success @= False
   <: #stdout  @= ""
   <: #stderr  @= ""
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
  case job ^. #author of
    Nothing     -> Nothing
    Just author -> List.find (\u -> u ^. #github == author) $ team ^. #member

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
JobData
  problem Int
  team    Text
  author  Text Maybe
  queuing Bool
  running Bool
  success Bool
  stdout  Text
  stderr  Text
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
   <: #stdout  @= jobDataStdout
   <: #stderr  @= jobDataStderr
   <: #created @= utcTimeToInt64 jobDataCreated
   <: nil

utcTimeToInt64 :: UTCTime -> Int64
utcTimeToInt64 (UTCTime (ModifiedJulianDay d) t)
  = 86400 * (fromIntegral d - unixEpochDay)
    + fromIntegral (diffTimeToPicoseconds t) `div` 1_000_000_000_000
  where
    unixEpochDay = 40587

type SQLitable m env = (MixDB.HasSqliteConfig env, HasLogFunc env, MonadReader env m, MonadUnliftIO m)

create :: SQLitable m env => Problem.Id -> Team.Id -> Maybe User.GitHubId -> m Job
create pid tid uid = MixDB.run $ do
  currentTime <- getCurrentTime
  let dat = JobData
        (coerce $ job ^. #problem)
        (coerce $ job ^. #team)
        (coerce $ job ^. #author)
        (job ^. #queuing)
        (job ^. #running)
        (job ^. #success)
        (job ^. #stdout)
        (job ^. #stderr)
        currentTime
        currentTime
  jid <- insert dat
  pure $ job & #id `set` coerce jid
  where
    job = new pid tid uid 0

updateToRunning :: SQLitable m env => Id -> m Job
updateToRunning jid =
  MixDB.run $ fromData (coerce jid) <$> updateGet (toSqlKey $ coerce jid)
    [ JobDataQueuing Persist.=. False
    , JobDataRunning Persist.=. True
    ]

updateToSuccess, updateToFailure :: SQLitable m env => Id -> Text -> Text -> m Job
updateToSuccess jid out err =
  MixDB.run $ fromData (coerce jid) <$> updateGet (toSqlKey $ coerce jid)
    [ JobDataRunning Persist.=. False
    , JobDataSuccess Persist.=. True
    , JobDataStdout Persist.=. out
    , JobDataStderr Persist.=. err
    ]
updateToFailure jid out err =
  MixDB.run $ fromData (coerce jid) <$> updateGet (toSqlKey $ coerce jid)
    [ JobDataRunning Persist.=. False
    , JobDataSuccess Persist.=. False
    , JobDataStdout Persist.=. out
    , JobDataStderr Persist.=. err
    ]

selectAll :: SQLitable m env => m [Job]
selectAll = MixDB.run $ do
  jobs <- select $ from $ table @JobData
  pure $ map (\dat -> fromData (fromSqlKey $ entityKey dat) $ entityVal dat) jobs

findById :: SQLitable m env => Id -> m (Maybe Job)
findById jid =
  MixDB.run $ fmap (fromData $ coerce jid) <$> get (toSqlKey $ coerce jid)
