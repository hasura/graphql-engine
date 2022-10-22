{-# OPTIONS -Wno-redundant-constraints #-}

-- | PostgreSQL helpers.
module Harness.Backend.Postgres
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
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.State (State)
import Harness.Test.Context (BackendType (Postgres), defaultBackendTypeString, defaultSource)
import Harness.Test.Schema qualified as Schema
import System.Process.Typed
import Prelude

-- | Check the postgres server is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.postgresLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for PostgreSQL.")
    loop attempts =
      catch
        ( bracket
            ( Postgres.connectPostgreSQL
                (fromString Constants.postgresqlConnectionString)
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
            (fromString Constants.postgresqlConnectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString q)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "PostgreSQL query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                q
              ]
          )
    )

-- | Metadata source information for the default Postgres instance.
defaultSourceMetadata :: Value
defaultSourceMetadata =
  let source = defaultSource Postgres
      backendType = defaultBackendTypeString Postgres
   in [yaml|
name: *source
kind: *backendType
tables: []
configuration: *defaultSourceConfiguration
|]

defaultSourceConfiguration :: Value
defaultSourceConfiguration =
  [yaml|
connection_info:
  database_url: *postgresqlConnectionString
  pool_settings: {}
|]

-- | Serialize Table into a PL-SQL statement, as needed, and execute it on the Postgres backend
createTable :: Schema.Table -> IO ()
createTable Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences} = do
  run_ $
    T.unpack $
      T.unwords
        [ "CREATE TABLE",
          T.pack Constants.postgresDb <> "." <> tableName,
          "(",
          commaSeparated $
            (mkColumn <$> tableColumns)
              <> (bool [mkPrimaryKey pk] [] (null pk))
              <> (mkReference <$> tableReferences),
          ");"
        ]

scalarType :: HasCallStack => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "INT"
  Schema.TStr -> "VARCHAR"
  Schema.TUTCTime -> "TIMESTAMP"
  Schema.TBool -> "BOOLEAN"
  t -> error $ "Unexpected scalar type used for Postgres: " <> show t

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
      "(",
      referenceLocalColumn,
      ")",
      "REFERENCES",
      referenceTargetTable,
      "(",
      referenceTargetColumn,
      ")",
      "ON DELETE CASCADE",
      "ON UPDATE CASCADE"
    ]

-- | Serialize tableData into a PL-SQL insert statement and execute it.
insertTable :: Schema.Table -> IO ()
insertTable Schema.Table {tableName, tableColumns, tableData}
  | null tableData = pure ()
  | otherwise = do
    run_ $
      T.unpack $
        T.unwords
          [ "INSERT INTO",
            T.pack Constants.postgresDb <> "." <> tableName,
            "(",
            commaSeparated (Schema.columnName <$> tableColumns),
            ")",
            "VALUES",
            commaSeparated $ mkRow <$> tableData,
            ";"
          ]

mkRow :: [Schema.ScalarValue] -> Text
mkRow row =
  T.unwords
    [ "(",
      commaSeparated $ Schema.serialize <$> row,
      ")"
    ]

-- | Serialize Table into a PL-SQL DROP statement and execute it
dropTable :: Schema.Table -> IO ()
dropTable Schema.Table {tableName} = do
  run_ $
    T.unpack $
      T.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          T.pack Constants.postgresDb <> "." <> tableName,
          ";"
        ]

-- | Post an http request to start tracking the table
trackTable :: State -> Schema.Table -> IO ()
trackTable state table =
  Schema.trackTable Postgres (defaultSource Postgres) table state

-- | Post an http request to stop tracking the table
untrackTable :: State -> Schema.Table -> IO ()
untrackTable state table =
  Schema.untrackTable Postgres (defaultSource Postgres) table state

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
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships Postgres table state
    Schema.trackArrayRelationships Postgres table state

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: [Schema.Table] -> (State, ()) -> IO ()
teardown tables (state, _) = do
  for_ (reverse tables) $ \table ->
    finally
      (Schema.untrackRelationships Postgres table state)
      ( finally
          (untrackTable state table)
          (dropTable table)
      )
