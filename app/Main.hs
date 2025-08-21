module Main where
import Db (initDb)
import Database.SQLite.Simple (open)
import Network.Wai.Handler.Warp (run)
import Servant ( serve )
import Routes (api, server)


main :: IO ()
main = do
    let dpPath = "poorlytics.db"
    conn <- open dpPath
    initDb conn
    putStrLn "Running on Port 8008"
    run 8008 (serve api (server conn))