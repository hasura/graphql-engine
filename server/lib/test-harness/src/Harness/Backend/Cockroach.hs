{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Cockroach helpers.
module Harness.Backend.Cockroach
  ( livenessCheck,
    run_,
    defaultSourceMetadata,
    defaultSourceConfiguration,
    createTable,
    insertTable,
    trackTable,
    dropTable,
    dropTableIfExists,
    untrackTable,
    setup,
    teardown,
    setupPermissions,
    teardownPermissions,
    setupTablesAction,
    setupPermissionsAction,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as S8
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Time (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (Cockroach), defaultBackendTypeString, defaultSource)
import Harness.Test.Fixture (SetupAction (..))
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema (BackendScalarType (..), BackendScalarValue (..), ScalarValue (..), SchemaName (..))
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import System.Process.Typed

-- | Check the cockroach server is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.postgresLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for CockroachDB.")
    loop attempts =
      catch
        ( bracket
            ( Postgres.connectPostgreSQL
                (fromString Constants.cockroachConnectionString)
            )
            Postgres.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            sleep Constants.postgresLivenessCheckIntervalSeconds
            loop (attempts - 1)
        )

-- | Run a plain SQL query. On error, print something useful for
-- debugging.
run_ :: HasCallStack => String -> IO ()
run_ q =
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            (fromString Constants.cockroachConnectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString q)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "CockroachDB query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                q
              ]
          )
    )

-- | Metadata source information for the default CockroachDB instance.
defaultSourceMetadata :: Value
defaultSourceMetadata =
  let source = defaultSource Cockroach
      backendType = defaultBackendTypeString Cockroach
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
  database_url: *cockroachConnectionString
  pool_settings: {}
|]

-- | Serialize Table into a PL-SQL statement, as needed, and execute it on the Cockroach backend
createTable :: TestEnvironment -> Schema.Table -> IO ()
createTable testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableUniqueConstraints} = do
  let schemaName = Schema.getSchemaName testEnv
  run_ $
    T.unpack $
      T.unwords
        [ "CREATE TABLE",
          T.pack Constants.cockroachDb <> "." <> wrapIdentifier tableName,
          "(",
          commaSeparated $
            (mkColumn <$> tableColumns)
              <> (bool [mkPrimaryKey pk] [] (null pk))
              <> (mkReference schemaName <$> tableReferences),
          ");"
        ]

  for_ tableUniqueConstraints (createUniqueConstraint tableName)

createUniqueConstraint :: Text -> Schema.UniqueConstraint -> IO ()
createUniqueConstraint tableName (Schema.UniqueConstraintColumns cols) =
  run_ $ T.unpack $ T.unwords $ ["CREATE UNIQUE INDEX ON ", tableName, "("] ++ [commaSeparated cols] ++ [")"]
createUniqueConstraint tableName (Schema.UniqueConstraintExpression ex) =
  run_ $ T.unpack $ T.unwords $ ["CREATE UNIQUE INDEX ON ", tableName, "((", ex, "))"]

scalarType :: HasCallStack => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "INT"
  Schema.TStr -> "VARCHAR"
  Schema.TUTCTime -> "TIMESTAMP"
  Schema.TBool -> "BOOLEAN"
  Schema.TGeography -> "GEOGRAPHY"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstCockroach

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

mkReference :: SchemaName -> Schema.Reference -> Text
mkReference schemaName Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} =
  T.unwords
    [ "FOREIGN KEY",
      "(",
      wrapIdentifier referenceLocalColumn,
      ")",
      "REFERENCES",
      unSchemaName schemaName <> "." <> wrapIdentifier referenceTargetTable,
      "(",
      wrapIdentifier referenceTargetColumn,
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
            T.pack Constants.cockroachDb <> "." <> wrapIdentifier tableName,
            "(",
            commaSeparated (wrapIdentifier . Schema.columnName <$> tableColumns),
            ")",
            "VALUES",
            commaSeparated $ mkRow <$> tableData,
            ";"
          ]

-- | Identifiers which may be case-sensitive needs to be wrapped in @""@.
--
--   More information can be found in the postgres docs:
--   https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
wrapIdentifier :: Text -> Text
wrapIdentifier identifier = "\"" <> identifier <> "\""

-- | 'ScalarValue' serializer for Cockroach
serialize :: ScalarValue -> Text
serialize = \case
  VInt i -> tshow i
  VStr s -> "'" <> T.replace "'" "\'" s <> "'"
  VUTCTime t -> T.pack $ formatTime defaultTimeLocale "'%F %T'" t
  VBool b -> if b then "TRUE" else "FALSE"
  VGeography (Schema.WKT wkt) -> T.concat ["st_geogfromtext(\'", wkt, "\')"]
  VNull -> "NULL"
  VCustomValue bsv -> Schema.formatBackendScalarValueType $ Schema.backendScalarValue bsv bsvCockroach

mkRow :: [Schema.ScalarValue] -> Text
mkRow row =
  T.unwords
    [ "(",
      commaSeparated $ serialize <$> row,
      ")"
    ]

-- | Serialize Table into a PL-SQL DROP statement and execute it
dropTable :: Schema.Table -> IO ()
dropTable Schema.Table {tableName} = do
  run_ $
    T.unpack $
      T.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          T.pack Constants.cockroachDb <> "." <> tableName,
          ";"
        ]

dropTableIfExists :: Schema.Table -> IO ()
dropTableIfExists Schema.Table {tableName} = do
  run_ $
    T.unpack $
      T.unwords
        [ "DROP TABLE IF EXISTS",
          T.pack Constants.cockroachDb <> "." <> tableName
        ]

-- | Post an http request to start tracking the table
trackTable :: TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment table =
  Schema.trackTable Cockroach (defaultSource Cockroach) table testEnvironment

-- | Post an http request to stop tracking the table
untrackTable :: TestEnvironment -> Schema.Table -> IO ()
untrackTable testEnvironment table =
  Schema.untrackTable Cockroach (defaultSource Cockroach) table testEnvironment

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
setup tables (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment defaultSourceMetadata Nothing
  -- Setup and track tables
  for_ tables $ \table -> do
    createTable testEnvironment table
    insertTable table
    trackTable testEnvironment table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships Cockroach table testEnvironment
    Schema.trackArrayRelationships Cockroach table testEnvironment

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
teardown (reverse -> tables) (testEnvironment, _) = do
  finally
    -- Teardown relationships first
    ( forFinally_ tables $ \table ->
        Schema.untrackRelationships Cockroach table testEnvironment
    )
    -- Then teardown tables
    ( forFinally_ tables $ \table ->
        finally
          (untrackTable testEnvironment table)
          (dropTable table)
    )

setupTablesAction :: [Schema.Table] -> TestEnvironment -> SetupAction
setupTablesAction ts env =
  SetupAction
    (setup ts (env, ()))
    (const $ teardown ts (env, ()))

setupPermissionsAction :: [Permissions.Permission] -> TestEnvironment -> SetupAction
setupPermissionsAction permissions env =
  SetupAction
    (setupPermissions permissions env)
    (const $ teardownPermissions permissions env)

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setupPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
setupPermissions permissions env = Permissions.setup Cockroach permissions env

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardownPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
teardownPermissions permissions env = Permissions.teardown Cockroach permissions env
