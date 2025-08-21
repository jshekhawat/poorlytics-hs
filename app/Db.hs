{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  DataKinds #-}


module Db where

import Data.Time (addUTCTime, getCurrentTime, UTCTime)
import Database.SQLite.Simple (Connection, Only (Only), execute, execute_, query, Query (Query))
import Types (IngestEvent (..), Summary (Summary), TopPage (TopPage), Bucket (Bucket))
import qualified Data.Text as T

initDb :: Connection -> IO ()
initDb conn = do
  execute_ conn "PRAGMA journal_mode=WAL;"
  execute_ conn "PRAGMA synchronous=NORMAL;"
  execute_
    conn
    " CREATE TABLE IF NOT EXISTS events (\n\
    \ id INTEGER PRIMARY KEY AUTOINCREMENT,\n\
    \ ts DATETIME NOT NULL,\n\
    \ path TEXT NOT NULL,\n\
    \ referrer TEXT,\n\
    \ session_id TEXT NOT NULL,\n\
    \ user_agent TEXT,\n\
    \ ip TEXT\n\
    \ );\n "
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_events_ts ON events(ts);"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_events_path_ts ON events(path, ts);"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_events_session_ts ON events(session_id, ts);"

insertEvent :: Connection -> IngestEvent -> IO ()
insertEvent conn ev = do
  now <- getCurrentTime
  execute conn 
    "INSERT INTO events (ts, path, referrer, session_id, user_agent, ip) VALUES (?,?,?,?,?,?)" 
    (now, path ev, referrer ev, sessionId ev, userAgent ev, ip ev)
  pure ()

getSummary :: Connection -> Int -> IO Summary
getSummary conn hrs = do
  now <- getCurrentTime
  [Only total] <- query conn "SELECT COUNT(*) FROM events WHERE ts >= ?" (Only (getCutoff hrs now))
  [Only uniq] <- query conn 
    "SELECT COUNT(DISTINCT session_id) FROM events WHERE ts >= ?" 
        (Only (getCutoff hrs now))
  [Only pages] <- query conn 
    "SELECT COUNT(DISTINCT path) FROM events WHERE ts >= ?" 
        (Only (getCutoff hrs now))
  pure $ Summary total uniq hrs pages

getTopPages :: Connection -> Int -> Int -> IO [TopPage]
getTopPages conn hrs lim = do
  now <- getCurrentTime
  rows <- query conn 
        "SELECT path, COUNT(*) as c FROM events WHERE ts >= ? GROUP BY path ORDER BY c DESC LIMIT ?" 
        (getCutoff hrs now, lim)
  pure $ map (uncurry TopPage) (rows :: [(T.Text, Int)])

getTimeSeries :: Connection -> Int -> T.Text -> IO [Bucket]
getTimeSeries conn hrs bucket = do
    now <- getCurrentTime
    let fmt = case bucket of
            "day" -> "%Y-%m-%dT00:00:00Z"
            _ -> "%Y-%m-%dT%H:00:00Z"
    rows <- query conn
        (Query $ T.concat
        [ "SELECT strftime('"
        , fmt
        , "', ts) as b, COUNT(*) as c \n"
        , "FROM events WHERE ts >= ? \n"
        , "GROUP BY b ORDER BY b ASC;"
        ])
        (Only (getCutoff hrs now))
    pure $ map (uncurry Bucket) (rows :: [(T.Text, Int)])

getCutoff :: Int -> UTCTime -> UTCTime
getCutoff hrs = addUTCTime (negate $ realToFrac (hrs * 3600))
   