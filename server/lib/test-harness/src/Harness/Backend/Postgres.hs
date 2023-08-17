{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | PostgreSQL helpers.
module Harness.Backend.Postgres
  ( backendTypeMetadata,
    livenessCheck,
    makeFreshDbConnectionString,
    metadataLivenessCheck,
    run_,
    runCustomDB_,
    runSQL,
    defaultConnectInfo,
    defaultSourceMetadata,
    defaultNamedSourceMetadata,
    defaultSourceConfiguration,
    defaultPostgresConnectionString,
    createDatabase,
    dropDatabase,
    createTable,
    insertTable,
    dropTable,
    dropTableIfExists,
    trackTable,
    untrackTable,
    setupTablesAction,
    createUntrackedTablesAction,
    setupFunctionRootFieldAction,
    setupComputedFieldAction,
    -- sql generation for other postgres-like backends
    uniqueConstraintSql,
    createUniqueIndexSql,
    mkPrimaryKeySql,
    mkReferenceSql,
    wrapIdentifier,
    createCustomDatabase,
    createTableOnCustomDB,
    insertTableOnCustomDB,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent.Extended (sleep)
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.Aeson qualified as J
import Data.Monoid (Last (..))
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Text.Lazy qualified as TL
import Data.Time (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.Options (Options (..))
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema
  ( BackendScalarType (..),
    BackendScalarValue (..),
    ScalarValue (..),
    SchemaName (..),
  )
import Harness.Schema qualified as Schema
import Harness.Services.Database.Postgres qualified as Postgres
import Harness.Test.BackendType (BackendTypeConfig, postgresishGraphQLType)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.SetupAction (SetupAction (..))
import Harness.TestEnvironment (GlobalTestEnvironment (..), TestEnvironment (..), TestingMode (..))
import Hasura.Prelude
import System.Process.Typed
import Text.Pretty.Simple (pShow)

--------------------------------------------------------------------------------

backendTypeMetadata :: BackendTypeConfig
backendTypeMetadata =
  BackendType.BackendTypeConfig
    { backendType = BackendType.Postgres,
      backendSourceName = "postgres",
      backendCapabilities = Nothing,
      backendTypeString = "pg",
      backendDisplayNameString = "pg",
      backendReleaseNameString = Nothing,
      backendServerUrl = Nothing,
      backendSchemaKeyword = "schema",
      backendScalarType = scalarType,
      backendGraphQLType = postgresishGraphQLType
    }

--------------------------------------------------------------------------------

-- | The default connection information based on the 'TestingMode'. The
-- interesting thing here is the database: in both modes, we specify an
-- /initial/ database (returned by this function), which we use only as a way
-- to create other databases for testing.
defaultConnectInfo :: (HasCallStack) => GlobalTestEnvironment -> Postgres.ConnectInfo
defaultConnectInfo globalTestEnvironment =
  case testingMode globalTestEnvironment of
    TestNewPostgresVariant opts@Options {..} ->
      let getComponent :: forall a. String -> Last a -> a
          getComponent component =
            fromMaybe
              ( error
                  $ unlines
                    [ "Postgres URI is missing its " <> component <> " component.",
                      "Postgres options: " <> TL.unpack (pShow opts)
                    ]
              )
              . getLast
          defaultPort = Last (Just (Postgres.connectPort Postgres.defaultConnectInfo))
       in Postgres.ConnectInfo
            { connectUser = getComponent "user" user,
              connectPassword = getComponent "password" password,
              connectHost = getComponent "host" $ hostaddr <> host,
              connectPort = getComponent "port" $ defaultPort <> (fromIntegral <$> port),
              connectDatabase = getComponent "dbname" $ dbname
            }
    _otherTestingMode ->
      Postgres.ConnectInfo
        { connectHost = T.unpack Constants.postgresHost,
          connectUser = T.unpack Constants.postgresUser,
          connectPort = Constants.postgresPort,
          connectPassword = T.unpack Constants.postgresPassword,
          connectDatabase = T.unpack Constants.postgresDb
        }

-- | Create a connection string for whatever unique database has been generated
-- for this 'TestEnvironment'.
makeFreshDbConnectionString :: TestEnvironment -> Postgres.PostgresServerUrl
makeFreshDbConnectionString testEnvironment =
  Postgres.PostgresServerUrl
    $ bsToTxt
    $ Postgres.postgreSQLConnectionString
      (defaultConnectInfo (globalEnvironment testEnvironment))
        { Postgres.connectDatabase = T.unpack (uniqueDbName (uniqueTestId testEnvironment))
        }

-- | Default Postgres connection string that we use for our admin purposes
-- (setting up / deleting per-test databases)
defaultPostgresConnectionString :: GlobalTestEnvironment -> Postgres.PostgresServerUrl
defaultPostgresConnectionString =
  Postgres.PostgresServerUrl
    . bsToTxt
    . Postgres.postgreSQLConnectionString
    . defaultConnectInfo

metadataLivenessCheck :: (HasCallStack) => IO ()
metadataLivenessCheck =
  doLivenessCheck (Postgres.PostgresServerUrl $ T.pack postgresqlMetadataConnectionString)

livenessCheck :: (HasCallStack) => TestEnvironment -> IO ()
livenessCheck = doLivenessCheck . makeFreshDbConnectionString

-- | Check the postgres server is live and ready to accept connections.
doLivenessCheck :: (HasCallStack) => Postgres.PostgresServerUrl -> IO ()
doLivenessCheck (Postgres.PostgresServerUrl connectionString) = loop Constants.postgresLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for PostgreSQL.")
    loop attempts =
      catch
        ( bracket
            ( Postgres.connectPostgreSQL (txtToBs connectionString)
            )
            Postgres.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            sleep Constants.postgresLivenessCheckIntervalSeconds
            loop (attempts - 1)
        )

-- | Run a plain SQL query.
-- On error, print something useful for debugging.
run_ :: (HasCallStack) => TestEnvironment -> Text -> IO ()
run_ testEnvironment =
  Postgres.run (makeFreshDbConnectionString testEnvironment, testEnvironment)

runCustomDB_ :: (HasCallStack) => TestEnvironment -> Postgres.ConnectInfo -> Text -> IO ()
runCustomDB_ testEnvironment connectionInfo =
  Postgres.run (postgresServerUrl connectionInfo, testEnvironment)

runSQL :: String -> TestEnvironment -> IO ()
runSQL = Schema.runSQL (BackendType.backendSourceName backendTypeMetadata)

-- | Metadata source information for the default Postgres instance.
defaultSourceMetadata :: TestEnvironment -> Value
defaultSourceMetadata testEnv =
  [interpolateYaml|
    name: #{ BackendType.backendSourceName backendTypeMetadata }
    kind: #{ BackendType.backendTypeString backendTypeMetadata }
    tables: []
    configuration: #{ defaultSourceConfiguration testEnv }
  |]

-- | Metadata source information for the "default" named Postgres instance.
defaultNamedSourceMetadata :: TestEnvironment -> Value
defaultNamedSourceMetadata testEnv =
  [interpolateYaml|
    name: default
    kind: #{ BackendType.backendTypeString backendTypeMetadata }
    tables: []
    configuration: #{ defaultSourceConfiguration testEnv }
  |]

defaultSourceConfiguration :: TestEnvironment -> Value
defaultSourceConfiguration testEnv = do
  let (Postgres.PostgresServerUrl connectionString) = makeFreshDbConnectionString testEnv

  [interpolateYaml|
    connection_info:
      database_url: #{ connectionString }
      pool_settings: {}
  |]

qualifiedTableName :: TestEnvironment -> Schema.Table -> Text
qualifiedTableName testEnv table =
  let schemaName = Schema.resolveTableSchema testEnv table
   in [i| #{ Schema.unSchemaName schemaName }."#{ Schema.tableName table }" |]

-- | Serialize Table into a PL-SQL statement, as needed, and execute it on the Postgres backend
createTable :: TestEnvironment -> Schema.Table -> IO ()
createTable testEnv table@(Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableConstraints, tableUniqueIndexes}) = do
  let schemaName = Schema.resolveTableSchema testEnv table

  -- \| create schema for this table
  createSchema testEnv table

  run_
    testEnv
    [i|
      CREATE TABLE #{ qualifiedTableName testEnv table }
        (#{
          commaSeparated $
            (mkColumnSql <$> tableColumns)
              <> (bool [mkPrimaryKeySql pk] [] (null pk))
              <> (mkReferenceSql schemaName <$> tableReferences)
              <> map uniqueConstraintSql tableConstraints
        });
    |]

  for_ tableUniqueIndexes (run_ testEnv . createUniqueIndexSql schemaName tableName)

createTableOnCustomDB :: TestEnvironment -> Schema.Table -> Postgres.ConnectInfo -> IO ()
createTableOnCustomDB testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableConstraints, tableUniqueIndexes} connectionInfo = do
  let schemaName = Schema.getSchemaName testEnv

  runCustomDB_
    testEnv
    connectionInfo
    [i|
      CREATE TABLE #{ Constants.postgresDb }."#{ tableName }"
        (#{
          commaSeparated $
            (mkColumnSql <$> tableColumns)
              <> (bool [mkPrimaryKeySql pk] [] (null pk))
              <> (mkReferenceSql schemaName <$> tableReferences)
              <> map uniqueConstraintSql tableConstraints
        });
    |]

  for_ tableUniqueIndexes (run_ testEnv . createUniqueIndexSql schemaName tableName)

uniqueConstraintSql :: Schema.Constraint -> Text
uniqueConstraintSql = \case
  Schema.UniqueConstraintColumns cols ->
    [i| UNIQUE (#{ commaSeparated cols }) |]
  Schema.CheckConstraintExpression ex ->
    [i| CHECK (#{ ex }) |]

createUniqueIndexSql :: SchemaName -> Text -> Schema.UniqueIndex -> Text
createUniqueIndexSql (SchemaName schemaName) tableName = \case
  Schema.UniqueIndexColumns cols ->
    [i| CREATE UNIQUE INDEX ON "#{ schemaName }"."#{ tableName }" (#{ commaSeparated cols }) |]
  Schema.UniqueIndexExpression ex ->
    [i| CREATE UNIQUE INDEX ON "#{ schemaName }"."#{ tableName }" ((#{ ex })) |]

scalarType :: (HasCallStack) => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "integer"
  Schema.TDouble -> "double precision"
  Schema.TStr -> "text"
  Schema.TUTCTime -> "timestamp"
  Schema.TBool -> "boolean"
  Schema.TGeography -> "geography"
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
mkReferenceSql (SchemaName localSchemaName) Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn, referenceTargetQualifiers} =
  let schemaName = maybe localSchemaName Schema.unSchemaName (Schema.resolveReferenceSchema referenceTargetQualifiers)
   in [i|
    FOREIGN KEY ("#{ referenceLocalColumn }")
    REFERENCES "#{ schemaName }"."#{ referenceTargetTable }" ("#{ referenceTargetColumn }")
    ON DELETE CASCADE ON UPDATE CASCADE
  |]

-- | Serialize tableData into a PL-SQL insert statement and execute it.
insertTable :: TestEnvironment -> Schema.Table -> IO ()
insertTable testEnv table@(Schema.Table {tableColumns, tableData}) = unless (null tableData) do
  run_
    testEnv
    [i|
      INSERT INTO #{ qualifiedTableName testEnv table }
        (#{ commaSeparated (wrapIdentifier . Schema.columnName <$> tableColumns) })
      VALUES
        #{ commaSeparated $ mkRow <$> tableData };
    |]

insertTableOnCustomDB :: TestEnvironment -> Schema.Table -> Postgres.ConnectInfo -> IO ()
insertTableOnCustomDB testEnv Schema.Table {tableName, tableColumns, tableData} connectionInfo = unless (null tableData) do
  runCustomDB_
    testEnv
    connectionInfo
    [i|
      INSERT INTO "#{ Constants.postgresDb }"."#{ tableName }"
        (#{ commaSeparated (wrapIdentifier . Schema.columnName <$> tableColumns) })
      VALUES
        #{ commaSeparated $ mkRow <$> tableData };
    |]

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
  VDouble d -> tshow d
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
-- We don't want @IF EXISTS@ here, because we don't want this to fail silently.
dropTable :: TestEnvironment -> Schema.Table -> IO ()
dropTable testEnv Schema.Table {tableName} =
  run_
    testEnv
    [i| DROP TABLE #{ Constants.postgresDb }.#{ tableName }; |]

dropTableIfExists :: TestEnvironment -> Schema.Table -> IO ()
dropTableIfExists testEnv Schema.Table {tableName} = do
  -- A transaction means that the @SET LOCAL@ is scoped to this operation.
  -- In other words, whatever the @client_min_messages@ flag's previous value
  -- was will be restored after running this.
  run_
    testEnv
    [i|
      BEGIN;
      SET LOCAL client_min_messages = warning;
      DROP TABLE IF EXISTS #{ Constants.postgresDb }."#{ tableName }";
      COMMIT;
    |]

-- | Post an http request to start tracking the table
trackTable :: TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment table =
  Schema.trackTable (BackendType.backendSourceName backendTypeMetadata) table testEnvironment

-- | Post an http request to stop tracking the table
untrackTable :: TestEnvironment -> Schema.Table -> IO ()
untrackTable testEnvironment table =
  Schema.untrackTable (BackendType.backendSourceName backendTypeMetadata) table testEnvironment

-- | create a database to use and later drop for these tests
-- note we use the 'initial' connection string here, ie, the one we started
-- with.
createDatabase :: TestEnvironment -> IO ()
createDatabase testEnvironment = do
  Postgres.createDatabase
    testEnvironment
    (uniqueDbName (uniqueTestId testEnvironment))

dropDatabase :: TestEnvironment -> IO ()
dropDatabase testEnvironment = do
  Postgres.dropDatabase
    testEnvironment
    (uniqueDbName (uniqueTestId testEnvironment))

createCustomDatabase :: TestEnvironment -> Postgres.ConnectInfo -> IO ()
createCustomDatabase testEnvironment connectionInfo = do
  let customDbName = T.pack $ Postgres.connectDatabase connectionInfo

  Postgres.run
    testEnvironment
    ("CREATE DATABASE " <> customDbName <> ";")

  -- Create schema
  let schemaName = Schema.getSchemaName testEnvironment
  Postgres.run
    (postgresServerUrl connectionInfo, testEnvironment)
    [i|
      BEGIN;
      SET LOCAL client_min_messages = warning;
      CREATE SCHEMA IF NOT EXISTS #{unSchemaName schemaName};
      COMMIT;
    |]

-- | Build @'PostgresServerUrl' from @'ConnectInfo' data structure
postgresServerUrl :: Postgres.ConnectInfo -> Postgres.PostgresServerUrl
postgresServerUrl = Postgres.PostgresServerUrl . bsToTxt . Postgres.postgreSQLConnectionString

-- Because the test harness sets the schema name we use for testing, we need
-- to make sure it exists before we run the tests.
createSchema :: TestEnvironment -> Schema.Table -> IO ()
createSchema testEnvironment table = do
  let schemaName = Schema.resolveTableSchema testEnvironment table

  -- A transaction means that the @SET LOCAL@ is scoped to this operation.
  -- In other words, whatever the @client_min_messages@ flag's previous value
  -- was will be restored after running this.
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

  -- enable open telemetry output in tests
  GraphqlEngine.postMetadata_ testEnvironment Schema.enableOpenTelemetryCommand

  -- Setup and track tables
  for_ tables $ \table -> do
    createTable testEnvironment table
    insertTable testEnvironment table
    trackTable testEnvironment table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships table testEnvironment
    Schema.trackArrayRelationships table testEnvironment

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

createUntrackedTables :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
createUntrackedTables tables (testEnvironment, _) = do
  -- Setup tables
  for_ tables $ \table -> do
    createTable testEnvironment table
    insertTable testEnvironment table

createUntrackedTablesAction :: [Schema.Table] -> TestEnvironment -> SetupAction
createUntrackedTablesAction ts env =
  SetupAction
    (createUntrackedTables ts (env, ()))
    (const $ pure ())

setupFunctionRootFieldAction :: String -> TestEnvironment -> SetupAction
setupFunctionRootFieldAction functionName env =
  SetupAction
    ( Schema.trackFunction
        (BackendType.backendSourceName backendTypeMetadata)
        functionName
        env
    )
    ( \_ ->
        Schema.untrackFunction
          (BackendType.backendSourceName backendTypeMetadata)
          functionName
          env
    )

setupComputedFieldAction :: Schema.Table -> String -> String -> TestEnvironment -> SetupAction
setupComputedFieldAction table functionName asFieldName env =
  SetupAction
    ( Schema.trackComputedField
        (BackendType.backendSourceName backendTypeMetadata)
        table
        functionName
        asFieldName
        J.Null
        J.Null
        env
    )
    ( \_ ->
        Schema.untrackComputedField
          (BackendType.backendSourceName backendTypeMetadata)
          table
          asFieldName
          env
    )
