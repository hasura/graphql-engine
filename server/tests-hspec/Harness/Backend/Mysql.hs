{-# OPTIONS -Wno-redundant-constraints #-}

-- | MySQL helpers.
module Harness.Backend.Mysql
  ( livenessCheck,
    run_,
    defaultSourceMetadata,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.String
import Database.MySQL.Simple as Mysql
import GHC.Stack
import Harness.Constants as Constants
import Harness.Quoter.Yaml (yaml)
import System.Process.Typed
import Prelude

-- | Check that the MySQL service is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.mysqlLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for MySQL.")
    loop attempts =
      catch
        ( bracket
            (Mysql.connect Constants.mysqlConnectInfo)
            Mysql.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            threadDelay
              Constants.mysqlLivenessCheckIntervalMicroseconds
            loop (attempts - 1)
        )

-- | Run a plain SQL string against the server, ignore the
-- result. Just checks for errors.
run_ :: HasCallStack => String -> IO ()
run_ query' =
  catch
    ( bracket
        (Mysql.connect Constants.mysqlConnectInfo)
        Mysql.close
        (\conn -> void (Mysql.execute_ conn (fromString query')))
    )
    ( \(e :: SomeException) ->
        error
          ( unlines
              [ "MySQL query error:",
                show e,
                "SQL was:",
                query'
              ]
          )
    )

-- | Metadata source information for the default Mysql instance.
defaultSourceMetadata :: Value
defaultSourceMetadata =
  [yaml|
name: mysql
kind: mysql
tables: []
configuration:
  database: *mysqlDatabase
  user: *mysqlUser
  password: *mysqlPassword
  host: *mysqlHost
  port: *mysqlPort
  pool_settings: {}
  |]
