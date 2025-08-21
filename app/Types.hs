{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}



module Types where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Time (UTCTime)
import Database.SQLite.Simple.FromRow (FromRow(..))
import Database.SQLite.Simple (field)
import Data.Aeson (FromJSON, ToJSON)

data IngestEvent = IngestEvent {
    path :: Text
    , referrer :: Maybe Text
    , sessionId :: Text
    , userAgent :: Maybe Text
    , ip :: Maybe Text
} deriving (Show, Generic)

instance FromJSON IngestEvent

data EventRow = EventRow {
    eid :: Int64
    , ets :: UTCTime
    , epath :: Text
    , ereferrer :: Maybe Text
    , esession :: Text
    , eua :: Maybe Text
    , eip :: Maybe Text 
}

instance FromRow EventRow where
    fromRow = EventRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field 

data Summary = Summary { 
    
totalPageviews :: Int
, uniqueSessions :: Int
, sinceHours :: Int
, distinctPages :: Int
} deriving (Show, Generic)

instance ToJSON Types.Summary


data TopPage = TopPage { 
    page :: Text
    , views :: Int
} deriving (Show, Generic)

instance ToJSON TopPage

data Bucket = Bucket { 
    bucketStart :: Text
    , bucketViews :: Int
} deriving (Show, Generic)

instance ToJSON Bucket


type HealthPath      = "health"
type EventPath       = "e"
type StatsPath       = "stats"
type SummaryPath     = "summary"
type TopPagesPath    = "top-pages"
type TimeSeriesPath  = "timeseries"

type SinceHoursParam = "since_hours"
type LimitParam      = "limit"
type BucketParam     = "bucket"

