{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | PostgreSQL helpers.
module Harness.Backend.Postgres
  ( backendTypeMetadata,
    livenessCheck,
    makeFreshDbConnectionString,
    metadataLivenessCheck,
    run_,
    runWithInitialDb_,
    runSQL,
    defaultSourceMetadata,
    defaultSourceConfiguration,
    createMetadataDatabase,
    createDatabase,
    dropMetadataDatabase,
    dropDatabase,
    createTable,
    insertTable,
    dropTable,
    dropTableIfExists,
    trackTable,
    untrackTable,
    setupTablesAction,
    setupPermissionsAction,
    setupFunctionRootFieldAction,
    setupComputedFieldAction,
    -- sql generation for other postgres-like backends
    uniqueConstraintSql,
    createUniqueIndexSql,
    mkPrimaryKeySql,
    mkReferenceSql,
    wrapIdentifier,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent.Extended (sleep)
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as S8
import Data.Monoid (Last, getLast)
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Extended (commaSeparated)
import Data.Text.Lazy qualified as TL
import Data.Time (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple qualified as Postgres
import Database.PostgreSQL.Simple.Options (Options (..))
import Harness.Constants as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Logging
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.BackendType (BackendTypeConfig)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema
  ( BackendScalarType (..),
    BackendScalarValue (..),
    ScalarValue (..),
    SchemaName (..),
  )
import Harness.Test.Schema qualified as Schema
import Harness.Test.SetupAction (SetupAction (..))
import Harness.TestEnvironment (GlobalTestEnvironment (..), TestEnvironment (..), TestingMode (..), UniqueTestId, testLogMessage)
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
      backendServerUrl = Nothing,
      backendSchemaKeyword = "schema"
    }

--------------------------------------------------------------------------------

-- | The default connection information based on the 'TestingMode'. The
-- interesting thing here is the database: in both modes, we specify an
-- /initial/ database (returned by this function), which we use only as a way
-- to create other databases for testing.
defaultConnectInfo :: HasCallStack => GlobalTestEnvironment -> Postgres.ConnectInfo
defaultConnectInfo globalTestEnvironment =
  case testingMode globalTestEnvironment of
    TestNewPostgresVariant opts@Options {..} ->
      let getComponent :: forall a. String -> Last a -> a
          getComponent component =
            fromMaybe
              ( error $
                  unlines
                    [ "Postgres URI is missing its " <> component <> " component.",
                      "Postgres options: " <> TL.unpack (pShow opts)
                    ]
              )
              . getLast
       in Postgres.ConnectInfo
            { connectUser = getComponent "user" user,
              connectPassword = getComponent "password" password,
              connectHost = getComponent "host" $ hostaddr <> host,
              connectPort = fromIntegral . getComponent "port" $ port,
              connectDatabase = getComponent "dbname" $ dbname
            }
    _otherTestingMode ->
      Postgres.ConnectInfo
        { connectHost = Constants.postgresHost,
          connectUser = Constants.postgresUser,
          connectPort = Constants.postgresPort,
          connectPassword = Constants.postgresPassword,
          connectDatabase = Constants.postgresDb
        }

-- | Create a connection string for whatever unique database has been generated
-- for this 'TestEnvironment'.
makeFreshDbConnectionString :: TestEnvironment -> S8.ByteString
makeFreshDbConnectionString testEnvironment =
  Postgres.postgreSQLConnectionString
    (defaultConnectInfo (globalEnvironment testEnvironment))
      { Postgres.connectDatabase = uniqueDbName (uniqueTestId testEnvironment)
      }

metadataLivenessCheck :: HasCallStack => TestEnvironment -> IO ()
metadataLivenessCheck =
  doLivenessCheck
    . fromString
    . postgresqlMetadataConnectionString
    . uniqueTestId

livenessCheck :: HasCallStack => TestEnvironment -> IO ()
livenessCheck = doLivenessCheck . makeFreshDbConnectionString

-- PostgreSQL 15.1 on x86_64-pc-linux-musl, com ....
-- forgive me, padre
parsePostgresVersion :: String -> Maybe Int
parsePostgresVersion =
  readMaybe
    . takeWhile (not . (==) '.')
    . drop (length @[] "PostgreSQL ")

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
runWithInitialDb_ :: HasCallStack => GlobalTestEnvironment -> String -> IO ()
runWithInitialDb_ globalTestEnvironment =
  runInternal globalTestEnvironment $
    Postgres.postgreSQLConnectionString (defaultConnectInfo globalTestEnvironment)

-- | Run a plain SQL query.
-- On error, print something useful for debugging.
run_ :: HasCallStack => TestEnvironment -> String -> IO ()
run_ testEnvironment =
  runInternal (globalEnvironment testEnvironment) (makeFreshDbConnectionString testEnvironment)

--- | Run a plain SQL query.
-- On error, print something useful for debugging.
runInternal :: HasCallStack => GlobalTestEnvironment -> S8.ByteString -> String -> IO ()
runInternal globalTestEnvironment connectionString query = do
  runLogger (logger globalTestEnvironment) $ LogDBQuery (decodeUtf8 connectionString) (T.pack query)
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

runSQL :: String -> TestEnvironment -> IO ()
runSQL = Schema.runSQL (BackendType.backendSourceName backendTypeMetadata)

--- | Run a plain SQL query.
-- On error, print something useful for debugging.
queryInternal :: (Postgres.FromRow a) => HasCallStack => TestEnvironment -> S8.ByteString -> String -> IO [a]
queryInternal testEnvironment connectionString query = do
  testLogMessage testEnvironment $ LogDBQuery (decodeUtf8 connectionString) (T.pack query)
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

-- | when we are creating databases, we want to connect with the 'original' DB
-- we started with
queryWithInitialDb :: (Postgres.FromRow a, HasCallStack) => TestEnvironment -> String -> IO [a]
queryWithInitialDb testEnvironment =
  queryInternal
    testEnvironment
    (Postgres.postgreSQLConnectionString (defaultConnectInfo $ globalEnvironment testEnvironment))

-- | Metadata source information for the default Postgres instance.
defaultSourceMetadata :: TestEnvironment -> Value
defaultSourceMetadata testEnv =
  [interpolateYaml|
    name: #{ BackendType.backendSourceName backendTypeMetadata }
    kind: #{ BackendType.backendTypeString backendTypeMetadata }
    tables: []
    configuration: #{ defaultSourceConfiguration testEnv }
  |]

defaultSourceConfiguration :: TestEnvironment -> Value
defaultSourceConfiguration testEnv = do
  let connectionString :: Text
      connectionString = bsToTxt $ makeFreshDbConnectionString testEnv

  [interpolateYaml|
    connection_info:
      database_url: #{ connectionString }
      pool_settings: {}
  |]

-- | Serialize Table into a PL-SQL statement, as needed, and execute it on the Postgres backend
createTable :: TestEnvironment -> Schema.Table -> IO ()
createTable testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableConstraints, tableUniqueIndexes} = do
  let schemaName = Schema.getSchemaName testEnv

  run_
    testEnv
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

createUniqueIndexSql :: SchemaName -> Text -> Schema.UniqueIndex -> String
createUniqueIndexSql (SchemaName schemaName) tableName = \case
  Schema.UniqueIndexColumns cols ->
    [i| CREATE UNIQUE INDEX ON "#{ schemaName }"."#{ tableName }" (#{ commaSeparated cols }) |]
  Schema.UniqueIndexExpression ex ->
    [i| CREATE UNIQUE INDEX ON "#{ schemaName }"."#{ tableName }" ((#{ ex })) |]

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
mkReferenceSql (SchemaName schemaName) Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} =
  [i|
    FOREIGN KEY ("#{ referenceLocalColumn }")
    REFERENCES #{ schemaName }."#{ referenceTargetTable }" ("#{ referenceTargetColumn }")
    ON DELETE CASCADE ON UPDATE CASCADE
  |]

-- | Serialize tableData into a PL-SQL insert statement and execute it.
insertTable :: TestEnvironment -> Schema.Table -> IO ()
insertTable testEnv Schema.Table {tableName, tableColumns, tableData} = unless (null tableData) do
  run_
    testEnv
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

createMetadataDatabase :: GlobalTestEnvironment -> UniqueTestId -> IO ()
createMetadataDatabase globalTestEnvironment uniqueTestId =
  runWithInitialDb_
    globalTestEnvironment
    ("CREATE DATABASE " <> Constants.postgresMetadataDb uniqueTestId <> ";")

dropMetadataDatabase :: TestEnvironment -> IO ()
dropMetadataDatabase testEnvironment = do
  let dbName = postgresMetadataDb (uniqueTestId testEnvironment)
  dropDatabaseInternal dbName testEnvironment

-- | create a database to use and later drop for these tests
-- note we use the 'initial' connection string here, ie, the one we started
-- with.
createDatabase :: TestEnvironment -> IO ()
createDatabase testEnvironment = do
  runWithInitialDb_
    (globalEnvironment testEnvironment)
    ("CREATE DATABASE " <> uniqueDbName (uniqueTestId testEnvironment) <> ";")
  createSchema testEnvironment

dropDatabase :: TestEnvironment -> IO ()
dropDatabase testEnvironment = do
  let dbName = uniqueDbName (uniqueTestId testEnvironment)
  dropDatabaseInternal dbName testEnvironment

-- | we drop databases at the end of test runs so we don't need to do DB clean
-- up.
dropDatabaseInternal :: String -> TestEnvironment -> IO ()
dropDatabaseInternal dbName testEnvironment = do
  ([Postgres.Only version]) <-
    queryWithInitialDb @(Postgres.Only String)
      testEnvironment
      "SELECT version();"

  case parsePostgresVersion version of
    Just pgVersion | pgVersion >= 13 -> do
      -- if we are on Postgres 13 or more, we can use WITH (FORCE);
      runWithInitialDb_
        (globalEnvironment testEnvironment)
        ("DROP DATABASE " <> dbName <> " WITH (FORCE);")
        `catch` \(ex :: SomeException) -> testLogMessage testEnvironment (LogDropDBFailedWarning (T.pack dbName) ex)

    -- for older Postgres versions, we Do Our Best
    _ -> do
      -- throw all the other users off the database
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
        (globalEnvironment testEnvironment)
        ("DROP DATABASE " <> dbName <> ";")
        `catch` \(ex :: SomeException) -> testLogMessage testEnvironment (LogDropDBFailedWarning (T.pack dbName) ex)

-- Because the test harness sets the schema name we use for testing, we need
-- to make sure it exists before we run the tests.
createSchema :: TestEnvironment -> IO ()
createSchema testEnvironment = do
  let schemaName = Schema.getSchemaName testEnvironment

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
teardown _ _ = pure ()

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
setupPermissions permissions env = Permissions.setup permissions env

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardownPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
teardownPermissions permissions env = Permissions.teardown backendTypeMetadata permissions env

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
        Aeson.Null
        Aeson.Null
        env
    )
    ( \_ ->
        Schema.untrackComputedField
          (BackendType.backendSourceName backendTypeMetadata)
          table
          asFieldName
          env
    )
