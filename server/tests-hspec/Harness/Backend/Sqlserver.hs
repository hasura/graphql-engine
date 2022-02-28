{-# OPTIONS -Wno-redundant-constraints #-}

-- | SQLServer helpers.
module Harness.Backend.Sqlserver
  ( livenessCheck,
    run_,
    defaultSourceMetadata,
    defaultSourceConfiguration,
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
import Data.Foldable (for_)
import Data.String
import Data.Text (Text)
import Data.Text qualified as T (pack, unpack, unwords)
import Data.Text.Extended (commaSeparated)
import Database.ODBC.SQLServer qualified as Sqlserver
import GHC.Stack
import Harness.Constants as Constants
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.State (State)
import Harness.Test.Schema qualified as Schema
import System.Process.Typed
import Prelude

-- | Check that the SQLServer service is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.sqlserverLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for SQLServer.")
    loop attempts =
      catch
        ( bracket
            (Sqlserver.connect Constants.sqlserverConnectInfo)
            Sqlserver.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            threadDelay
              Constants.sqlserverLivenessCheckIntervalMicroseconds
            loop (attempts - 1)
        )

-- | Run a plain SQL string against the server, ignore the
-- result. Just checks for errors.
run_ :: HasCallStack => String -> IO ()
run_ query' =
  catch
    ( bracket
        (Sqlserver.connect Constants.sqlserverConnectInfo)
        Sqlserver.close
        (\conn -> void (Sqlserver.exec conn (fromString query')))
    )
    ( \(e :: SomeException) ->
        error
          ( unlines
              [ "SQLServer query error:",
                show e,
                "SQL was:",
                query'
              ]
          )
    )

-- | Metadata source information for the default MSSQL instance.
defaultSourceMetadata :: Value
defaultSourceMetadata =
  [yaml|
name: mssql
kind: mssql
tables: []
configuration: *defaultSourceConfiguration
  |]

defaultSourceConfiguration :: Value
defaultSourceConfiguration =
  [yaml|
connection_info:
  database_url: *sqlserverConnectInfo
  pool_settings: {}
|]

-- | Serialize Table into a T-SQL statement, as needed, and execute it on the Sqlserver backend
createTable :: Schema.Table -> IO ()
createTable Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences} = do
  run_ $
    T.unpack $
      T.unwords
        [ "CREATE TABLE",
          T.pack Constants.sqlserverDb <> "." <> tableName,
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
      Schema.TInt -> "INT"
      Schema.TStr -> "TEXT"
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

-- | Serialize tableData into a T-SQL insert statement and execute it.
insertTable :: Schema.Table -> IO ()
insertTable Schema.Table {tableName, tableColumns, tableData} =
  run_ $
    T.unpack $
      T.unwords
        [ "INSERT INTO",
          T.pack Constants.sqlserverDb <> "." <> tableName,
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
  let schemaName = T.pack Constants.sqlserverDb
  GraphqlEngine.postMetadata_ state $
    [yaml|
type: mssql_track_table
args:
  source: mssql
  table:
    schema: *schemaName
    name: *tableName
|]

-- | Serialize Table into a T-SQL DROP statement and execute it
dropTable :: Schema.Table -> IO ()
dropTable Schema.Table {tableName} = do
  run_ $
    T.unpack $
      T.unwords
        [ "DROP TABLE",
          T.pack Constants.sqlserverDb <> "." <> tableName,
          ";"
        ]

-- | Post an http request to stop tracking the table
untrackTable :: State -> Schema.Table -> IO ()
untrackTable state Schema.Table {tableName} = do
  let schemaName = T.pack Constants.sqlserverDb
  GraphqlEngine.postMetadata_ state $
    [yaml|
type: mssql_untrack_table
args:
  source: mssql
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
