{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Cockroach helpers.
module Harness.Backend.Cockroach
  ( livenessCheck,
    run_,
    defaultSourceMetadata,
    defaultSourceConfiguration,
    createDatabase,
    dropDatabase,
    createTable,
    insertTable,
    trackTable,
    dropTable,
    dropTableIfExists,
    untrackTable,
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
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Time (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Backend.Postgres qualified as Postgres
  ( createUniqueIndexSql,
    mkPrimaryKeySql,
    mkReferenceSql,
    uniqueConstraintSql,
  )
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Logging
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.BackendType (BackendType (Cockroach), defaultBackendTypeString, defaultSource)
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema (BackendScalarType (..), BackendScalarValue (..), ScalarValue (..), SchemaName (..))
import Harness.Test.Schema qualified as Schema
import Harness.Test.SetupAction (SetupAction (..))
import Harness.TestEnvironment (TestEnvironment (..), testLogMessage)
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
                (fromString Constants.defaultCockroachConnectionString)
            )
            Postgres.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            sleep Constants.postgresLivenessCheckIntervalSeconds
            loop (attempts - 1)
        )

-- | when we are creating databases, we want to connect with the 'original' DB
-- we started with
runWithInitialDb_ :: HasCallStack => TestEnvironment -> String -> IO ()
runWithInitialDb_ testEnvironment =
  runInternal testEnvironment Constants.defaultCockroachConnectionString

-- | Run a plain SQL query.
-- On error, print something useful for debugging.
run_ :: HasCallStack => TestEnvironment -> String -> IO ()
run_ testEnvironment =
  runInternal testEnvironment (Constants.cockroachConnectionString (uniqueTestId testEnvironment))

--- | Run a plain SQL query.
-- On error, print something useful for debugging.
runInternal :: HasCallStack => TestEnvironment -> String -> String -> IO ()
runInternal testEnvironment connectionString query = do
  testLogMessage testEnvironment $ LogDBQuery (T.pack connectionString) (T.pack query)
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            (fromString connectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString query)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "CockroachDB query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                query
              ]
          )
    )

-- | Metadata source information for the default CockroachDB instance.
defaultSourceMetadata :: TestEnvironment -> Value
defaultSourceMetadata testEnvironment =
  [interpolateYaml|
    name: #{ defaultSource Cockroach }
    kind: #{ defaultBackendTypeString Cockroach }
    tables: []
    configuration: #{ defaultSourceConfiguration testEnvironment }
  |]

defaultSourceConfiguration :: TestEnvironment -> Value
defaultSourceConfiguration testEnvironment =
  let databaseUrl = cockroachConnectionString (uniqueTestId testEnvironment)
   in [interpolateYaml|
    connection_info:
      database_url: #{ databaseUrl }
      pool_settings: {}
  |]

-- | Serialize Table into a PL-SQL statement, as needed, and execute it on the Cockroach backend
createTable :: TestEnvironment -> Schema.Table -> IO ()
createTable testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableConstraints, tableUniqueIndexes} = do
  let schemaName = Schema.getSchemaName testEnv

  run_
    testEnv
    [i|
      CREATE TABLE #{ Constants.cockroachDb }."#{ tableName }"
        (#{
          commaSeparated $
            (mkColumnSql <$> tableColumns)
              <> (bool [Postgres.mkPrimaryKeySql pk] [] (null pk))
              <> (Postgres.mkReferenceSql schemaName <$> tableReferences)
              <> map Postgres.uniqueConstraintSql tableConstraints
        });
    |]

  for_ tableUniqueIndexes (run_ testEnv . Postgres.createUniqueIndexSql schemaName tableName)

scalarType :: HasCallStack => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "INT"
  Schema.TStr -> "VARCHAR"
  Schema.TUTCTime -> "TIMESTAMP"
  Schema.TBool -> "BOOLEAN"
  Schema.TGeography -> "GEOGRAPHY"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstCockroach

mkColumnSql :: Schema.Column -> Text
mkColumnSql Schema.Column {columnName, columnType, columnNullable, columnDefault} =
  T.unwords
    [ wrapIdentifier columnName,
      scalarType columnType,
      bool "NOT NULL" "DEFAULT NULL" columnNullable,
      maybe "" ("DEFAULT " <>) columnDefault
    ]

-- | Serialize tableData into a PL-SQL insert statement and execute it.
insertTable :: TestEnvironment -> Schema.Table -> IO ()
insertTable testEnvironment Schema.Table {tableName, tableColumns, tableData}
  | null tableData = pure ()
  | otherwise = do
      run_ testEnvironment $
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
  VInt n -> tshow n
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
dropTable :: TestEnvironment -> Schema.Table -> IO ()
dropTable testEnvironment Schema.Table {tableName} = do
  run_ testEnvironment $
    T.unpack $
      T.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          T.pack Constants.cockroachDb <> "." <> tableName,
          ";"
        ]

dropTableIfExists :: TestEnvironment -> Schema.Table -> IO ()
dropTableIfExists testEnvironment Schema.Table {tableName} = do
  run_ testEnvironment $
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

-- | create a database to use and later drop for these tests
-- note we use the 'initial' connection string here, ie, the one we started
-- with.
createDatabase :: TestEnvironment -> IO ()
createDatabase testEnvironment = do
  runWithInitialDb_
    testEnvironment
    ("CREATE DATABASE " <> uniqueDbName (uniqueTestId testEnvironment) <> ";")
  createSchema testEnvironment

-- | we drop databases at the end of test runs so we don't need to do DB clean
-- up.
dropDatabase :: TestEnvironment -> IO ()
dropDatabase testEnvironment = do
  let dbName = uniqueDbName (uniqueTestId testEnvironment)
  runWithInitialDb_
    testEnvironment
    ("DROP DATABASE " <> dbName <> ";")
    `catch` \(ex :: SomeException) -> testLogMessage testEnvironment (LogDropDBFailedWarning (T.pack dbName) ex)

-- Because the test harness sets the schema name we use for testing, we need
-- to make sure it exists before we run the tests.
createSchema :: TestEnvironment -> IO ()
createSchema testEnvironment = do
  let schemaName = Schema.getSchemaName testEnvironment
  run_
    testEnvironment
    [i|
      BEGIN;
      SET LOCAL client_min_messages = warning;
      CREATE SCHEMA IF NOT EXISTS #{unSchemaName schemaName};
      COMMIT;
  |]

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
setup tables (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment (defaultSourceMetadata testEnvironment) Nothing

  -- Setup and track tables
  for_ tables $ \table -> do
    createTable testEnvironment table
    insertTable testEnvironment table
    trackTable testEnvironment table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships Cockroach table testEnvironment
    Schema.trackArrayRelationships Cockroach table testEnvironment

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
-- Because the Fixture takes care of dropping the DB, all we do here is
-- clear the metadata with `replace_metadata`.
teardown :: HasCallStack => [Schema.Table] -> (TestEnvironment, ()) -> IO ()
teardown _ (testEnvironment, _) =
  GraphqlEngine.setSources testEnvironment mempty Nothing

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
