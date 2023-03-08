-- | This module houses low-level functions and types to help access and work
-- with Postgres servers. For functions and types to help test the Postgres HGE
-- backend, see 'Harness.Backend.Postgres'.
module Harness.Services.Postgres
  ( FreshPostgresDb (..),
    PostgresServerUrl (..),
    run,
    createDatabase,
    dropDatabase,
    mkFreshPostgresDb,
    mkFreshDbConnectionString,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Extended (sleep)
import Control.Monad.Managed
import Data.Aeson (ToJSON)
import Data.ByteString.Char8 qualified as S8
import Data.Char qualified
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Exceptions
import Harness.Logging
import Hasura.Prelude

newtype PostgresServerUrl = PostgresServerUrl {getPostgresServerUrl :: Text}
  deriving newtype (ToJSON)

newtype FreshPostgresDb = FreshPostgresDb {freshDbName :: Text}

--- | Run a plain SQL query.
-- On error, print something useful for debugging.
run :: HasCallStack => Logger -> PostgresServerUrl -> Text -> IO ()
run logger (PostgresServerUrl connectionString) query = do
  startTime <- getCurrentTime
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            (encodeUtf8 connectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString $ T.unpack query)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "PostgreSQL query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                T.unpack query
              ]
          )
    )
  endTime <- getCurrentTime
  runLogger logger $ LogDBQuery connectionString query (diffUTCTime endTime startTime)

mkFreshPostgresDb :: Logger -> PostgresServerUrl -> Managed FreshPostgresDb
mkFreshPostgresDb logger pgServerUrl = do
  freshDbName <- liftIO $ drawFreshDbName
  managed $
    bracket
      (createDatabase logger pgServerUrl freshDbName)
      (\_ -> dropDatabase logger pgServerUrl freshDbName)
  return $ FreshPostgresDb freshDbName
  where
    drawFreshDbName :: IO Text
    drawFreshDbName = do
      uuid <- tshow <$> liftIO UUID.nextRandom
      return $
        "freshdb_"
          <> T.map
            ( \a ->
                if Data.Char.isAlphaNum a
                  then a
                  else '_'
            )
            uuid

mkFreshDbConnectionString :: PostgresServerUrl -> FreshPostgresDb -> PostgresServerUrl
mkFreshDbConnectionString (PostgresServerUrl pgUrl) (FreshPostgresDb db) =
  PostgresServerUrl $ T.dropWhileEnd ((/= '/')) pgUrl <> db

-- | create a database to use and later drop for these tests
-- note we use the 'initial' connection string here, ie, the one we started
-- with.
createDatabase :: Logger -> PostgresServerUrl -> Text -> IO ()
createDatabase logger pgServerUrl dbName = do
  run
    logger
    pgServerUrl
    ("CREATE DATABASE " <> dbName)

-- | we drop databases at the end of test runs so we don't need to do DB clean
-- up.
dropDatabase :: Logger -> PostgresServerUrl -> Text -> IO ()
dropDatabase logger pgServerUrl dbName =
  void $ forkIO $ do
    -- we don't really mind when this happens, and we don't want to block on it,
    -- so we drop the DB in a thread, and wait 10 seconds first to let any
    -- business finish
    sleep 10
    -- if this fails, don't make the test fail, we're doing our best to clean up
    run
      logger
      pgServerUrl
      ("DROP DATABASE " <> dbName <> ";")
      `catch` \(ex :: SomeException) -> runLogger logger (LogDropDBFailedWarning dbName ex)
