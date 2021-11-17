{-# LANGUAGE DisambiguateRecordFields #-}

-- | Constant configurations used throughout the test suite.
module Harness.Constants
  ( postgresPassword,
    postgresUser,
    postgresDb,
    postgresHost,
    postgresPort,
    postgresqlConnectionString,
    postgresLivenessCheckAttempts,
    postgresLivenessCheckIntervalSeconds,
    postgresLivenessCheckIntervalMicroseconds,
    mysqlLivenessCheckAttempts,
    mysqlLivenessCheckIntervalSeconds,
    mysqlLivenessCheckIntervalMicroseconds,
    mysqlPassword,
    mysqlUser,
    mysqlDatabase,
    mysqlHost,
    mysqlPort,
    mysqlConnectInfo,
    httpHealthCheckAttempts,
    httpHealthCheckIntervalSeconds,
    httpHealthCheckIntervalMicroseconds,
    graphqlEngineUrlPrefix,
  )
where

import Data.Word
import Database.MySQL.Simple qualified as Mysql
import Prelude

postgresPassword :: String
postgresPassword = "chinook"

postgresUser :: String
postgresUser = "chinook"

postgresDb :: String
postgresDb = "chinook"

postgresHost :: String
postgresHost = "127.0.0.1"

postgresPort :: Word16
postgresPort = 5432

postgresqlConnectionString :: String
postgresqlConnectionString =
  "postgres://"
    ++ postgresUser
    ++ ":"
    ++ postgresPassword
    ++ "@"
    ++ postgresHost
    ++ ":"
    ++ show postgresPort
    ++ "/"
    ++ postgresDb

postgresLivenessCheckAttempts :: Int
postgresLivenessCheckAttempts = 5

postgresLivenessCheckIntervalSeconds :: Int
postgresLivenessCheckIntervalSeconds = 1

postgresLivenessCheckIntervalMicroseconds :: Int
postgresLivenessCheckIntervalMicroseconds = 1000 * 1000 * postgresLivenessCheckIntervalSeconds

mysqlLivenessCheckAttempts :: Int
mysqlLivenessCheckAttempts = 5

mysqlLivenessCheckIntervalSeconds :: Int
mysqlLivenessCheckIntervalSeconds = 1

mysqlLivenessCheckIntervalMicroseconds :: Int
mysqlLivenessCheckIntervalMicroseconds = 1000 * 1000 * mysqlLivenessCheckIntervalSeconds

mysqlPassword :: String
mysqlPassword = "hasuraMySQL1"

mysqlUser :: String
mysqlUser = "root"

mysqlDatabase :: String
mysqlDatabase = "hasura"

mysqlHost :: String
mysqlHost = "127.0.0.1"

mysqlPort :: Word16
mysqlPort = 3306

mysqlConnectInfo :: Mysql.ConnectInfo
mysqlConnectInfo =
  Mysql.defaultConnectInfo
    { Mysql.connectUser = mysqlUser,
      Mysql.connectPassword = mysqlPassword,
      Mysql.connectDatabase = mysqlDatabase,
      Mysql.connectHost = mysqlHost,
      Mysql.connectPort = mysqlPort
    }

httpHealthCheckAttempts :: Int
httpHealthCheckAttempts = 5

httpHealthCheckIntervalSeconds :: Int
httpHealthCheckIntervalSeconds = 1

httpHealthCheckIntervalMicroseconds :: Int
httpHealthCheckIntervalMicroseconds = 1000 * 1000 * httpHealthCheckIntervalSeconds

graphqlEngineUrlPrefix :: String
graphqlEngineUrlPrefix = "http://127.0.0.1:8080"
