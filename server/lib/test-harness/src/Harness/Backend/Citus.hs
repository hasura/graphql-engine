{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | CitusQL helpers. Pretty much the same as postgres. Could refactor
-- if we add more things here.
module Harness.Backend.Citus
  ( livenessCheck,
    run_,
    defaultSourceMetadata,
    defaultSourceConfiguration,
    createTable,
    insertTable,
    createDatabase,
    dropDatabase,
    trackTable,
    dropTable,
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
    wrapIdentifier,
  )
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Logging
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.BackendType (BackendType (Citus), defaultBackendTypeString, defaultSource)
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema (BackendScalarType (..), BackendScalarValue (..), ScalarValue (..), SchemaName (..))
import Harness.Test.Schema qualified as Schema
import Harness.Test.SetupAction (SetupAction (..))
import Harness.TestEnvironment (TestEnvironment (..), testLogMessage)
import Hasura.Prelude
import System.Process.Typed

-- | Check the citus server is live and ready to accept connections.
livenessCheck :: HasCallStack => IO ()
livenessCheck = loop Constants.postgresLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for Citus.")
    loop attempts =
      catch
        ( bracket
            ( Postgres.connectPostgreSQL
                (fromString Constants.defaultCitusConnectionString)
            )
            Postgres.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            sleep Constants.httpHealthCheckIntervalSeconds
            loop (attempts - 1)
        )

-- | when we are creating databases, we want to connect with the 'original' DB
-- we started with
runWithInitialDb_ :: HasCallStack => TestEnvironment -> String -> IO ()
runWithInitialDb_ testEnvironment =
  runInternal testEnvironment Constants.defaultCitusConnectionString

-- | Run a plain SQL query.
run_ :: HasCallStack => TestEnvironment -> String -> IO ()
run_ testEnvironment =
  runInternal testEnvironment (Constants.citusConnectionString (uniqueTestId testEnvironment))

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
              [ "Citus query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                query
              ]
          )
    )

-- | Metadata source information for the default Citus instance.
defaultSourceMetadata :: TestEnvironment -> Value
defaultSourceMetadata testEnvironment =
  [interpolateYaml|
    name: #{ defaultSource Citus }
    kind: #{ defaultBackendTypeString Citus }
    tables: []
    configuration: #{ defaultSourceConfiguration testEnvironment }
  |]

defaultSourceConfiguration :: TestEnvironment -> Value
defaultSourceConfiguration testEnvironment =
  let databaseUrl = citusConnectionString (uniqueTestId testEnvironment)
   in [interpolateYaml|
    connection_info:
      database_url: #{ databaseUrl }
      pool_settings: {}
  |]

-- | Serialize Table into a Citus-SQL statement, as needed, and execute it on the Citus backend
createTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
createTable testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableConstraints, tableUniqueIndexes} = do
  let schemaName = Schema.getSchemaName testEnv

  run_ testEnv $
    [i|
      CREATE TABLE #{ unSchemaName schemaName }."#{ tableName }"
        ( #{ commaSeparated $
              (mkColumnSql <$> tableColumns)
                <> (bool [Postgres.mkPrimaryKeySql pk] [] (null pk))
                <> (Postgres.mkReferenceSql schemaName <$> tableReferences)
                <> map Postgres.uniqueConstraintSql tableConstraints
          }
        );
    |]

  for_ tableUniqueIndexes (run_ testEnv . Postgres.createUniqueIndexSql schemaName tableName)

scalarType :: HasCallStack => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "INT"
  Schema.TStr -> "VARCHAR"
  Schema.TUTCTime -> "TIMESTAMP"
  Schema.TBool -> "BOOLEAN"
  Schema.TGeography -> "GEOGRAPHY"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstCitus

mkColumnSql :: Schema.Column -> Text
mkColumnSql Schema.Column {columnName, columnType, columnNullable, columnDefault} =
  T.unwords
    [ Postgres.wrapIdentifier columnName,
      scalarType columnType,
      bool "NOT NULL" "DEFAULT NULL" columnNullable,
      maybe "" ("DEFAULT " <>) columnDefault
    ]

-- | Serialize tableData into a Citus-SQL insert statement and execute it.
insertTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
insertTable testEnv Schema.Table {tableName, tableColumns, tableData}
  | null tableData = pure ()
  | otherwise = do
      let schemaName = Schema.unSchemaName $ Schema.getSchemaName testEnv
      run_ testEnv $
        [i|
          INSERT INTO "#{ schemaName }"."#{ tableName }"
            (#{ commaSeparated (Postgres.wrapIdentifier . Schema.columnName <$> tableColumns) })
            VALUES #{ commaSeparated $ mkRow <$> tableData };
        |]

-- | 'ScalarValue' serializer for Citus
serialize :: ScalarValue -> Text
serialize = \case
  VInt n -> tshow n
  VStr s -> "'" <> T.replace "'" "\'" s <> "'"
  VUTCTime t -> T.pack $ formatTime defaultTimeLocale "'%F %T'" t
  VBool b -> if b then "TRUE" else "FALSE"
  VGeography (Schema.WKT wkt) -> T.concat ["st_geogfromtext(\'", wkt, "\')"]
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
dropTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
dropTable testEnvironment Schema.Table {tableName} = do
  let schemaName = Schema.unSchemaName $ Schema.getSchemaName testEnvironment
  -- We don't want @IF EXISTS@ here, because we don't want this to fail silently.
  run_ testEnvironment $ [i| DROP TABLE #{ schemaName }.#{ tableName }; |]

-- | Post an http request to start tracking the table
trackTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment table =
  Schema.trackTable Citus (defaultSource Citus) table testEnvironment

-- | Post an http request to stop tracking the table
untrackTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
untrackTable testEnvironment table =
  Schema.untrackTable Citus (defaultSource Citus) table testEnvironment

-- Because the test harness sets the schema name we use for testing, we need
-- to make sure it exists before we run the tests.
-- we also need to add the `citus` extension: https://docs.citusdata.com/en/v11.1/admin_guide/cluster_management.html
createSchema :: TestEnvironment -> IO ()
createSchema testEnvironment = do
  let schemaName = Schema.getSchemaName testEnvironment
  run_
    testEnvironment
    [i|
        BEGIN;
        SET client_min_messages = error;
        SET log_min_messages = panic;
        CREATE SCHEMA IF NOT EXISTS #{unSchemaName schemaName};
        CREATE EXTENSION citus;
        COMMIT;
      |]

-- | create a database to use and later drop for these tests
-- note we use the 'initial' connection string here, ie, the one we started
-- with.
-- Citus is very 'helpful' so we tell it to stop filling our tests with noisy
-- notices
createDatabase :: TestEnvironment -> IO ()
createDatabase testEnvironment = do
  let dbName = uniqueDbName (uniqueTestId testEnvironment)
  -- please citus be quiet
  runWithInitialDb_
    testEnvironment
    [i|
        CREATE DATABASE #{dbName};
      |]

  createSchema testEnvironment

-- | we drop databases at the end of test runs so we don't need to do DB clean
-- up.
dropDatabase :: TestEnvironment -> IO ()
dropDatabase testEnvironment = do
  let dbName = uniqueDbName (uniqueTestId testEnvironment)
  runWithInitialDb_
    testEnvironment
    ("DROP DATABASE " <> dbName <> " WITH (FORCE);")
    `catch` \(ex :: SomeException) -> testLogMessage testEnvironment (LogDropDBFailedWarning (T.pack dbName) ex)

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: HasCallStack => [Schema.Table] -> (TestEnvironment, ()) -> IO ()
setup tables (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment (defaultSourceMetadata testEnvironment) Nothing

  let schemaName = Schema.getSchemaName testEnvironment
  -- Because the test harness sets the schema name we use for testing, we need
  -- to make sure it exists before we run the tests. We may want to consider
  -- removing it again in 'teardown'.
  run_
    testEnvironment
    [i|
      BEGIN;
      CREATE SCHEMA IF NOT EXISTS #{unSchemaName schemaName};
      COMMIT;
    |]

  -- Setup and track tables
  for_ tables $ \table -> do
    createTable testEnvironment table
    insertTable testEnvironment table
    trackTable testEnvironment table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships Citus table testEnvironment
    Schema.trackArrayRelationships Citus table testEnvironment

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

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: HasCallStack => [Schema.Table] -> (TestEnvironment, ()) -> IO ()
teardown _ (testEnvironment, _) =
  GraphqlEngine.setSources testEnvironment mempty Nothing

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setupPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
setupPermissions permissions env = Permissions.setup Citus permissions env

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardownPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
teardownPermissions permissions env = Permissions.teardown Citus permissions env
