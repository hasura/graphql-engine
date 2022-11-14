{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | PostgreSQL helpers.
module Harness.Backend.Postgres
  ( livenessCheck,
    makeFreshDbConnectionString,
    metadataLivenessCheck,
    run_,
    runWithInitialDb_,
    runSQL,
    defaultSourceMetadata,
    defaultSourceConfiguration,
    createDatabase,
    dropDatabase,
    createTable,
    insertTable,
    dropTable,
    dropTableIfExists,
    trackTable,
    untrackTable,
    setupTablesAction,
    setupTablesActionDiscardingTeardownErrors,
    setupPermissionsAction,
    setupFunctionRootFieldAction,
    setupComputedFieldAction,
    -- sql generation for other postgres-like backends
    uniqueConstraintSql,
    createUniqueIndexSql,
    mkPrimaryKeySql,
    mkReferenceSql,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as S8
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Time (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendType (Postgres), defaultBackendTypeString, defaultSource)
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema
  ( BackendScalarType (..),
    BackendScalarValue (..),
    ScalarValue (..),
    SchemaName (..),
  )
import Harness.Test.Schema qualified as Schema
import Harness.Test.SetupAction (SetupAction (..))
import Harness.TestEnvironment (TestEnvironment (..), TestingMode (..), testLogHarness)
import Hasura.Prelude
import System.Process.Typed

-- | The default connection information based on the 'TestingMode'. The
-- interesting thing here is the database: in both modes, we specify an
-- /initial/ database (returned by this function), which we use only as a way
-- to create other databases for testing.
defaultConnectInfo :: TestEnvironment -> Postgres.ConnectInfo
defaultConnectInfo testEnvironment =
  case testingMode testEnvironment of
    TestAllBackends ->
      Postgres.ConnectInfo
        { connectHost = Constants.postgresHost,
          connectUser = Constants.postgresUser,
          connectPort = Constants.postgresPort,
          connectPassword = Constants.postgresPassword,
          connectDatabase = Constants.postgresDb
        }
    TestNewPostgresVariant {..} ->
      Postgres.ConnectInfo
        { connectHost = postgresSourceHost,
          connectPort = postgresSourcePort,
          connectUser = postgresSourceUser,
          connectPassword = postgresSourcePassword,
          connectDatabase = postgresSourceInitialDatabase
        }

-- | Create a connection string for whatever unique database has been generated
-- for this 'TestEnvironment'.
makeFreshDbConnectionString :: TestEnvironment -> S8.ByteString
makeFreshDbConnectionString testEnvironment =
  Postgres.postgreSQLConnectionString
    (defaultConnectInfo testEnvironment)
      { Postgres.connectDatabase = uniqueDbName (uniqueTestId testEnvironment)
      }

metadataLivenessCheck :: HasCallStack => IO ()
metadataLivenessCheck =
  doLivenessCheck (fromString postgresqlMetadataConnectionString)

livenessCheck :: HasCallStack => TestEnvironment -> IO ()
livenessCheck = doLivenessCheck . makeFreshDbConnectionString

-- | Check the postgres server is live and ready to accept connections.
doLivenessCheck :: HasCallStack => BS.ByteString -> IO ()
doLivenessCheck connectionString = loop Constants.postgresLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for PostgreSQL.")
    loop attempts =
      catch
        ( bracket
            ( Postgres.connectPostgreSQL connectionString
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
  runInternal testEnvironment $
    Postgres.postgreSQLConnectionString (defaultConnectInfo testEnvironment)

-- | Run a plain SQL query.
-- On error, print something useful for debugging.
run_ :: HasCallStack => TestEnvironment -> String -> IO ()
run_ testEnvironment =
  runInternal testEnvironment (makeFreshDbConnectionString testEnvironment)

--- | Run a plain SQL query.
-- On error, print something useful for debugging.
runInternal :: HasCallStack => TestEnvironment -> S8.ByteString -> String -> IO ()
runInternal testEnvironment connectionString query = do
  testLogHarness
    testEnvironment
    ( "Executing connection string: "
        <> show connectionString
        <> "\n"
        <> "Query: "
        <> query
    )
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            connectionString
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString query)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "PostgreSQL query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                query
              ]
          )
    )

-- | when we are creating databases, we want to connect with the 'original' DB
-- we started with
queryWithInitialDb :: (Postgres.FromRow a, HasCallStack) => TestEnvironment -> String -> IO [a]
queryWithInitialDb testEnvironment =
  queryInternal testEnvironment (makeFreshDbConnectionString testEnvironment)

--- | Run a plain SQL query.
-- On error, print something useful for debugging.
queryInternal :: (Postgres.FromRow a) => HasCallStack => TestEnvironment -> S8.ByteString -> String -> IO [a]
queryInternal testEnvironment connectionString query = do
  testLogHarness
    testEnvironment
    ( "Querying connection string: "
        <> show connectionString
        <> "\n"
        <> "Query: "
        <> query
    )
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            connectionString
        )
        Postgres.close
        (\conn -> Postgres.query_ conn (fromString query))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "PostgreSQL query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                query
              ]
          )
    )

runSQL :: String -> TestEnvironment -> IO ()
runSQL = Schema.runSQL Postgres (defaultSource Postgres)

-- | Metadata source information for the default Postgres instance.
defaultSourceMetadata :: TestEnvironment -> Value
defaultSourceMetadata testEnv =
  let source = defaultSource Postgres
      backendType = defaultBackendTypeString Postgres
      sourceConfiguration = defaultSourceConfiguration testEnv
   in [yaml|
name: *source
kind: *backendType
tables: []
configuration: *sourceConfiguration
|]

defaultSourceConfiguration :: TestEnvironment -> Value
defaultSourceConfiguration testEnv =
  let databaseUrl = makeFreshDbConnectionString testEnv
   in [yaml|
    connection_info:
      database_url: *databaseUrl
      pool_settings: {}
   |]

-- | Serialize Table into a PL-SQL statement, as needed, and execute it on the Postgres backend
createTable :: TestEnvironment -> Schema.Table -> IO ()
createTable testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableConstraints, tableUniqueIndexes} = do
  let schemaName = Schema.getSchemaName testEnv

  run_ testEnv $
    T.unpack $
      T.unwords
        [ "CREATE TABLE",
          T.pack Constants.postgresDb <> "." <> wrapIdentifier tableName,
          "(",
          commaSeparated $
            (mkColumnSql <$> tableColumns)
              <> (bool [mkPrimaryKeySql pk] [] (null pk))
              <> (mkReferenceSql schemaName <$> tableReferences)
              <> map uniqueConstraintSql tableConstraints,
          ");"
        ]

  for_ tableUniqueIndexes (run_ testEnv . createUniqueIndexSql schemaName tableName)

uniqueConstraintSql :: Schema.Constraint -> Text
uniqueConstraintSql = \case
  Schema.UniqueConstraintColumns cols ->
    T.unwords $ ["UNIQUE ", "("] ++ [commaSeparated cols] ++ [")"]
  Schema.CheckConstraintExpression ex ->
    T.unwords $ ["CHECK ", "(", ex, ")"]

createUniqueIndexSql :: SchemaName -> Text -> Schema.UniqueIndex -> String
createUniqueIndexSql schemaName tableName = \case
  Schema.UniqueIndexColumns cols ->
    T.unpack $ T.unwords $ ["CREATE UNIQUE INDEX ON ", qualifiedTableName, "("] ++ [commaSeparated cols] ++ [")"]
  Schema.UniqueIndexExpression ex ->
    T.unpack $ T.unwords $ ["CREATE UNIQUE INDEX ON ", qualifiedTableName, "((", ex, "))"]
  where
    qualifiedTableName = wrapIdentifier (unSchemaName schemaName) <> "." <> wrapIdentifier tableName

scalarType :: HasCallStack => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "INT"
  Schema.TStr -> "VARCHAR"
  Schema.TUTCTime -> "TIMESTAMP"
  Schema.TBool -> "BOOLEAN"
  Schema.TGeography -> "GEOGRAPHY"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstPostgres

mkColumnSql :: Schema.Column -> Text
mkColumnSql Schema.Column {columnName, columnType, columnNullable, columnDefault} =
  T.unwords
    [ wrapIdentifier columnName,
      scalarType columnType,
      bool "NOT NULL" "DEFAULT NULL" columnNullable,
      maybe "" ("DEFAULT " <>) columnDefault
    ]

mkPrimaryKeySql :: [Text] -> Text
mkPrimaryKeySql key =
  T.unwords
    [ "PRIMARY KEY",
      "(",
      commaSeparated $ map wrapIdentifier key,
      ")"
    ]

mkReferenceSql :: SchemaName -> Schema.Reference -> Text
mkReferenceSql schemaName Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} =
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
insertTable :: TestEnvironment -> Schema.Table -> IO ()
insertTable testEnv Schema.Table {tableName, tableColumns, tableData}
  | null tableData = pure ()
  | otherwise = do
      run_ testEnv $
        T.unpack $
          T.unwords
            [ "INSERT INTO",
              T.pack Constants.postgresDb <> "." <> wrapIdentifier tableName,
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

-- | 'ScalarValue' serializer for Postgres
serialize :: ScalarValue -> Text
serialize = \case
  VInt n -> tshow n
  VStr s -> "'" <> T.replace "'" "\'" s <> "'"
  VUTCTime t -> T.pack $ formatTime defaultTimeLocale "'%F %T'" t
  VBool b -> if b then "TRUE" else "FALSE"
  VGeography (Schema.WKT wkt) -> T.concat ["st_geogfromtext(\'", wkt, "\')"]
  VNull -> "NULL"
  VCustomValue bsv -> Schema.formatBackendScalarValueType $ Schema.backendScalarValue bsv bsvPostgres

mkRow :: [Schema.ScalarValue] -> Text
mkRow row =
  T.unwords
    [ "(",
      commaSeparated $ serialize <$> row,
      ")"
    ]

-- | Serialize Table into a PL-SQL DROP statement and execute it
dropTable :: TestEnvironment -> Schema.Table -> IO ()
dropTable testEnv Schema.Table {tableName} = do
  run_ testEnv $
    T.unpack $
      T.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          T.pack Constants.postgresDb <> "." <> tableName,
          -- "CASCADE",
          ";"
        ]

dropTableIfExists :: TestEnvironment -> Schema.Table -> IO ()
dropTableIfExists testEnv Schema.Table {tableName} = do
  run_ testEnv $
    T.unpack $
      T.unwords
        [ "SET client_min_messages TO WARNING;", -- suppress a NOTICE if the table isn't there
          "DROP TABLE IF EXISTS",
          T.pack Constants.postgresDb <> "." <> wrapIdentifier tableName
        ]

-- | Post an http request to start tracking the table
trackTable :: TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment table =
  Schema.trackTable Postgres (defaultSource Postgres) table testEnvironment

-- | Post an http request to stop tracking the table
untrackTable :: TestEnvironment -> Schema.Table -> IO ()
untrackTable testEnvironment table =
  Schema.untrackTable Postgres (defaultSource Postgres) table testEnvironment

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
-- up. we can't use DROP DATABASE <dbname> WITH (FORCE) because we're using PG
-- < 13 in CI so instead we boot all the active users then drop as normal.
dropDatabase :: TestEnvironment -> IO ()
dropDatabase testEnvironment = do
  let dbName = uniqueDbName (uniqueTestId testEnvironment)
  void $
    queryWithInitialDb @(Postgres.Only Bool)
      testEnvironment
      [i|
      SELECT pg_terminate_backend(pg_stat_activity.pid)
      FROM pg_stat_activity
      WHERE pg_stat_activity.datname = '#{dbName}'
      AND pid <> pg_backend_pid();
    |]

  -- if this fails, don't make the test fail
  runWithInitialDb_
    testEnvironment
    ("DROP DATABASE " <> dbName <> ";")
    `catch` \(_ :: SomeException) -> pure ()

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
    Schema.trackObjectRelationships Postgres table testEnvironment
    Schema.trackArrayRelationships Postgres table testEnvironment

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
-- we replace metadata with nothing.
teardown :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
teardown _ (testEnvironment, _) =
  GraphqlEngine.setSources testEnvironment mempty Nothing

setupTablesAction :: [Schema.Table] -> TestEnvironment -> SetupAction
setupTablesAction ts env =
  SetupAction
    (setup ts (env, ()))
    (const $ teardown ts (env, ()))

setupTablesActionDiscardingTeardownErrors :: [Schema.Table] -> TestEnvironment -> SetupAction
setupTablesActionDiscardingTeardownErrors ts env =
  SetupAction
    (setup ts (env, ()))
    (const $ teardown ts (env, ()) `catchAny` \ex -> testLogHarness env ("Teardown failed: " <> show ex))

setupPermissionsAction :: [Permissions.Permission] -> TestEnvironment -> SetupAction
setupPermissionsAction permissions env =
  SetupAction
    (setupPermissions permissions env)
    (const $ teardownPermissions permissions env)

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setupPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
setupPermissions permissions env = Permissions.setup Postgres permissions env

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardownPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
teardownPermissions permissions env = Permissions.teardown Postgres permissions env

setupFunctionRootFieldAction :: String -> TestEnvironment -> SetupAction
setupFunctionRootFieldAction functionName env =
  SetupAction
    ( Schema.trackFunction
        Postgres
        (defaultSource Postgres)
        functionName
        env
    )
    ( \_ ->
        Schema.untrackFunction
          Postgres
          (defaultSource Postgres)
          functionName
          env
    )

setupComputedFieldAction :: Schema.Table -> String -> String -> TestEnvironment -> SetupAction
setupComputedFieldAction table functionName asFieldName env =
  SetupAction
    ( Schema.trackComputedField
        Postgres
        (defaultSource Postgres)
        table
        functionName
        asFieldName
        env
    )
    ( \_ ->
        Schema.untrackComputedField
          Postgres
          (defaultSource Postgres)
          table
          asFieldName
          env
    )
