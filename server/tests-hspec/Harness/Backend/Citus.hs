{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

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
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as S8
import Data.Foldable (for_)
import Data.String
import Data.Text (Text, pack, replace)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Time (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Context (BackendType (Citus), defaultSource)
import Harness.Test.Schema (BackendScalarType (..), BackendScalarValue (..), ScalarValue (..))
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude (tshow)
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
createTable :: HasCallStack => Schema.Table -> IO ()
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

scalarType :: HasCallStack => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "SERIAL"
  Schema.TStr -> "VARCHAR"
  Schema.TUTCTime -> "TIMESTAMP"
  Schema.TBool -> "BOOLEAN"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstCitus

mkColumn :: Schema.Column -> Text
mkColumn Schema.Column {columnName, columnType, columnNullable, columnDefault} =
  T.unwords
    [ wrapIdentifier columnName,
      scalarType columnType,
      bool "NOT NULL" "DEFAULT NULL" columnNullable,
      maybe "" ("DEFAULT " <>) columnDefault
    ]

mkPrimaryKey :: [Text] -> Text
mkPrimaryKey key =
  T.unwords
    [ "PRIMARY KEY",
      "(",
      commaSeparated $ map wrapIdentifier key,
      ")"
    ]

mkReference :: Schema.Reference -> Text
mkReference Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} =
  T.unwords
    [ "FOREIGN KEY",
      "(",
      wrapIdentifier referenceLocalColumn,
      ")",
      "REFERENCES",
      referenceTargetTable,
      "(",
      wrapIdentifier referenceTargetColumn,
      ")",
      "ON DELETE CASCADE",
      "ON UPDATE CASCADE"
    ]

-- | Serialize tableData into a Citus-SQL insert statement and execute it.
insertTable :: HasCallStack => Schema.Table -> IO ()
insertTable Schema.Table {tableName, tableColumns, tableData}
  | null tableData = pure ()
  | otherwise = do
    run_ $
      T.unpack $
        T.unwords
          [ "INSERT INTO",
            T.pack Constants.citusDb <> "." <> wrapIdentifier tableName,
            "(",
            commaSeparated (wrapIdentifier . Schema.columnName <$> tableColumns),
            ")",
            "VALUES",
            commaSeparated $ mkRow <$> tableData,
            ";"
          ]

-- | Citus identifiers which may be case-sensitive needs to be wrapped in @""@.
wrapIdentifier :: Text -> Text
wrapIdentifier identifier = "\"" <> identifier <> "\""

-- | 'ScalarValue' serializer for Citus
serialize :: ScalarValue -> Text
serialize = \case
  VInt i -> tshow i
  VStr s -> "'" <> replace "'" "\'" s <> "'"
  VUTCTime t -> pack $ formatTime defaultTimeLocale "'%F %T'" t
  VBool b -> tshow @Int $ if b then 1 else 0
  VNull -> "NULL"
  VCustomValue bsv -> Schema.formatBackendScalarValueType $ Schema.backendScalarValue bsv bsvCitus

mkRow :: [Schema.ScalarValue] -> Text
mkRow row =
  T.unwords
    [ "(",
      commaSeparated $ serialize <$> row,
      ")"
    ]

-- | Serialize Table into a Citus-SQL DROP statement and execute it
dropTable :: HasCallStack => Schema.Table -> IO ()
dropTable Schema.Table {tableName} = do
  run_ $
    T.unpack $
      T.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          T.pack Constants.citusDb <> "." <> tableName,
          ";"
        ]

-- | Post an http request to start tracking the table
trackTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment table =
  Schema.trackTable Citus (defaultSource Citus) table testEnvironment

-- | Post an http request to stop tracking the table
untrackTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
untrackTable testEnvironment table =
  Schema.untrackTable Citus (defaultSource Citus) table testEnvironment

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: HasCallStack => [Schema.Table] -> (TestEnvironment, ()) -> IO ()
setup tables (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment defaultSourceMetadata Nothing
  -- Setup and track tables
  for_ tables $ \table -> do
    createTable table
    insertTable table
    trackTable testEnvironment table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships Citus table testEnvironment
    Schema.trackArrayRelationships Citus table testEnvironment

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: HasCallStack => [Schema.Table] -> (TestEnvironment, ()) -> IO ()
teardown tables (testEnvironment, _) = do
  forFinally_ (reverse tables) $ \table ->
    finally
      (Schema.untrackRelationships Citus table testEnvironment)
      ( finally
          (untrackTable testEnvironment table)
          (dropTable table)
      )
