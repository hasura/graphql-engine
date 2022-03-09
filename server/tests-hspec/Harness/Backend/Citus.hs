{-# OPTIONS -Wno-redundant-constraints #-}

-- | CitusQL helpers. Pretty much the same as postgres. Could refactor
-- if we add more things here.
module Harness.Backend.Citus
  ( livenessCheck,
    run_,
    defaultSourceMetadata,
    createTable,
    insertTable,
    trackTable,
    dropTable,
    untrackTable,
    setup,
    teardown,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as S8
import Data.Foldable (for_)
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Database.PostgreSQL.Simple qualified as Postgres
import GHC.Stack
import Harness.Constants as Constants
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.State (State)
import Harness.Test.Schema qualified as Schema
import System.Process.Typed
import Prelude

-- | Check the citus server is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.postgresLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for Citus.")
    loop attempts =
      catch
        ( bracket
            ( Postgres.connectPostgreSQL
                (fromString Constants.citusConnectionString)
            )
            Postgres.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            threadDelay
              Constants.postgresLivenessCheckIntervalMicroseconds
            loop (attempts - 1)
        )

-- | Run a plain SQL query. On error, print something useful for
-- debugging.
run_ :: HasCallStack => String -> IO ()
run_ q =
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            (fromString Constants.citusConnectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString q)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "Citus query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                q
              ]
          )
    )

-- | Metadata source information for the default Citus instance.
defaultSourceMetadata :: Value
defaultSourceMetadata =
  [yaml|
name: citus
kind: citus
tables: []
configuration:
  connection_info:
    database_url: *citusConnectionString
    pool_settings: {}
  |]

-- | Serialize Table into a Citus-SQL statement, as needed, and execute it on the Citus backend
createTable :: Schema.Table -> IO ()
createTable Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences} = do
  run_ $
    T.unpack $
      T.unwords
        [ "CREATE TABLE",
          T.pack Constants.citusDb <> "." <> tableName,
          "(",
          commaSeparated $
            (mkColumn <$> tableColumns)
              <> (bool [mkPrimaryKey pk] [] (null pk))
              <> (mkReference <$> tableReferences),
          ");"
        ]
  where
    scalarType :: Schema.ScalarType -> Text
    scalarType = \case
      Schema.TInt -> "SERIAL"
      Schema.TStr -> "VARCHAR"
    mkColumn :: Schema.Column -> Text
    mkColumn Schema.Column {columnName, columnType, columnNullable, columnDefault} =
      T.unwords
        [ columnName,
          scalarType columnType,
          bool "NOT NULL" "DEFAULT NULL" columnNullable,
          maybe "" ("DEFAULT " <>) columnDefault
        ]
    mkPrimaryKey :: [Text] -> Text
    mkPrimaryKey key =
      T.unwords
        [ "PRIMARY KEY",
          "(",
          commaSeparated key,
          ")"
        ]
    mkReference :: Schema.Reference -> Text
    mkReference Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} =
      T.unwords
        [ "CONSTRAINT FOREIGN KEY",
          referenceLocalColumn,
          "REFERENCES",
          referenceTargetTable,
          "(",
          referenceTargetColumn,
          ")",
          "ON DELETE CASCADE",
          "ON UPDATE CASCADE"
        ]

-- | Serialize tableData into a Citus-SQL insert statement and execute it.
insertTable :: Schema.Table -> IO ()
insertTable Schema.Table {tableName, tableColumns, tableData} =
  run_ $
    T.unpack $
      T.unwords
        [ "INSERT INTO",
          T.pack Constants.citusDb <> "." <> tableName,
          "(",
          commaSeparated (Schema.columnName <$> tableColumns),
          ")",
          "VALUES",
          commaSeparated $ mkRow <$> tableData,
          ";"
        ]
  where
    mkRow :: [Schema.ScalarValue] -> Text
    mkRow row =
      T.unwords
        [ "(",
          commaSeparated $ Schema.serialize <$> row,
          ")"
        ]

-- | Post an http request to start tracking the table
trackTable :: State -> Schema.Table -> IO ()
trackTable state Schema.Table {tableName} = do
  let schemaName = T.pack Constants.citusDb
  GraphqlEngine.postMetadata_ state $
    [yaml|
type: citus_track_table
args:
  source: citus
  table:
    schema: *schemaName
    name: *tableName
|]

-- | Serialize Table into a Citus-SQL DROP statement and execute it
dropTable :: Schema.Table -> IO ()
dropTable Schema.Table {tableName} = do
  run_ $
    T.unpack $
      T.unwords
        [ "DROP TABLE IF EXISTS",
          T.pack Constants.citusDb <> "." <> tableName,
          ";"
        ]

-- | Post an http request to stop tracking the table
untrackTable :: State -> Schema.Table -> IO ()
untrackTable state Schema.Table {tableName} = do
  let schemaName = T.pack Constants.citusDb
  GraphqlEngine.postMetadata_ state $
    [yaml|
type: citus_untrack_table
args:
  source: citus
  table:
    schema: *schemaName
    name: *tableName
|]

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: [Schema.Table] -> (State, ()) -> IO ()
setup tables (state, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource state defaultSourceMetadata

  -- Setup and track tables
  for_ tables $ \table -> do
    createTable table
    insertTable table
    trackTable state table

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: [Schema.Table] -> (State, ()) -> IO ()
teardown tables (state, _) =
  for_ tables $ \table -> do
    untrackTable state table
    dropTable table
