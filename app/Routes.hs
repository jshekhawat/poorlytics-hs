{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Servant ( Proxy(..), PlainText, type (:>), Get, Server, ReqBody, type (:<|>) ((:<|>)), QueryParam, Post )
import Database.SQLite.Simple (Connection)
import Types (IngestEvent, Summary, TopPage, Bucket, HealthPath, EventPath, StatsPath, SummaryPath, TopPagesPath, TimeSeriesPath, SinceHoursParam, LimitParam, BucketParam)
import Servant.API (JSON)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text
import Db (insertEvent, getSummary, getTopPages, getTimeSeries)
import Data.Maybe (fromMaybe)


api :: Proxy API
api = Proxy

defaultToDay :: Maybe Int -> Int
defaultToDay = fromMaybe 24

defaultToHour :: Maybe Text -> Text
defaultToHour = fromMaybe "hour"

defaultToTenMin :: Maybe Int -> Int
defaultToTenMin  = fromMaybe 10

type HealthEP = HealthPath :> Get '[PlainText] String
type EventEP = EventPath :> ReqBody '[JSON] IngestEvent :> Post '[JSON] String
type SummaryEP =  StatsPath :> SummaryPath 
    :> QueryParam SinceHoursParam Int :> Get '[JSON] Summary
type TopPagesEP = StatsPath :> TopPagesPath 
    :> QueryParam SinceHoursParam Int :> QueryParam LimitParam Int :> Get '[JSON] [TopPage]
type TimeSeriesEP = StatsPath :> TimeSeriesPath 
    :> QueryParam SinceHoursParam Int :> QueryParam BucketParam Text :> Get '[JSON] [Bucket]

type API =
    HealthEP
    :<|> EventEP
    :<|> SummaryEP
    :<|> TopPagesEP
    :<|> TimeSeriesEP


server :: Connection -> Server API
server conn =
        pure "ok"
    :<|> (\ev -> liftIO (insertEvent conn ev) >> pure "accepted")
    :<|> (liftIO . getSummary conn . fromMaybe 24)
    :<|> (\mhrs mlim -> liftIO $ getTopPages conn (defaultToDay mhrs) (defaultToTenMin mlim))
    :<|> (\mhrs mbucket -> liftIO $ getTimeSeries conn (defaultToDay mhrs) (defaultToHour mbucket))