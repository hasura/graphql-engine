{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | SQLServer helpers.
module Harness.Backend.Sqlserver
  ( backendTypeMetadata,
    livenessCheck,
    run_,
    defaultSourceMetadata,
    defaultSourceConfiguration,
    createDatabase,
    dropDatabase,
    createTable,
    insertTable,
    trackTable,
    dropTable,
    untrackTable,
    setupTablesAction,
    createUntrackedTablesAction,
  )
where

--------------------------------------------------------------------------------

import Control.Concurrent.Extended (sleep)
import Control.Monad.Reader
import Data.Aeson (Value)
import Data.String (fromString)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Time (defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Database.ODBC.SQLServer qualified as Sqlserver
import Harness.Constants qualified as Constants
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Logging
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (BackendScalarType (..), BackendScalarValue (..), ScalarValue (..))
import Harness.Schema qualified as Schema
import Harness.Test.BackendType (BackendType (SQLServer), BackendTypeConfig (..), postgresishGraphQLType)
import Harness.Test.SetupAction (SetupAction (..))
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Prelude
import System.Process.Typed

--------------------------------------------------------------------------------

backendTypeMetadata :: BackendTypeConfig
backendTypeMetadata =
  BackendTypeConfig
    { backendType = SQLServer,
      backendSourceName = "mssql",
      backendCapabilities = Nothing,
      backendTypeString = "mssql",
      backendDisplayNameString = "mssql",
      backendReleaseNameString = Nothing,
      backendServerUrl = Nothing,
      backendSchemaKeyword = "schema",
      backendScalarType = scalarType,
      backendGraphQLType = postgresishGraphQLType
    }

--------------------------------------------------------------------------------

-- | Check that the SQLServer service is live and ready to accept connections.
livenessCheck :: (HasCallStack) => IO ()
livenessCheck = loop Constants.sqlserverLivenessCheckAttempts
  where
    loop 0 = error ("Liveness check failed for SQLServer.")
    loop attempts =
      catch
        ( bracket
            (Sqlserver.connect Constants.sqlserverAdminConnectInfo)
            Sqlserver.close
            (const (pure ()))
        )
        ( \(_failure :: ExitCodeException) -> do
            sleep Constants.sqlserverLivenessCheckIntervalSeconds
            loop (attempts - 1)
        )

-- | run SQL with the currently created DB for this test
run_ :: (HasCallStack) => TestEnvironment -> String -> IO ()
run_ testEnvironment =
  runInternal testEnvironment (Constants.sqlserverConnectInfo (uniqueTestId testEnvironment))

-- | when we are creating databases, we want to connect with the 'original' DB
-- we started with
runWithInitialDb_ :: (HasCallStack) => TestEnvironment -> String -> IO ()
runWithInitialDb_ testEnvironment =
  runInternal testEnvironment Constants.sqlserverAdminConnectInfo

-- | Run a plain SQL string against the server, ignore the
-- result. Just checks for errors.
runInternal :: (HasCallStack) => TestEnvironment -> Text -> String -> IO ()
runInternal testEnvironment connectionString query' = do
  startTime <- getCurrentTime
  catch
    ( bracket
        (Sqlserver.connect connectionString)
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
  endTime <- getCurrentTime
  testLogMessage testEnvironment $ LogDBQuery connectionString (T.pack query') (diffUTCTime endTime startTime)

-- | Metadata source information for the default MSSQL instance.
defaultSourceMetadata :: TestEnvironment -> Value
defaultSourceMetadata testEnvironment =
  let source = backendSourceName backendTypeMetadata
      backendType = backendTypeString backendTypeMetadata
      sourceConfiguration = defaultSourceConfiguration testEnvironment
   in [yaml|
name: *source
kind: *backendType
tables: []
configuration: *sourceConfiguration
  |]

defaultSourceConfiguration :: TestEnvironment -> Value
defaultSourceConfiguration testEnvironment =
  [yaml|
connection_info:
  database_url: *sqlserverConnectInfo
  pool_settings: {}
|]
  where
    sqlserverConnectInfo = Constants.sqlserverConnectInfo (uniqueTestId testEnvironment)

-- | Serialize Table into a T-SQL statement, as needed, and execute it on the Sqlserver backend
createTable :: TestEnvironment -> Schema.Table -> IO ()
createTable _ Schema.Table {tableUniqueIndexes = _ : _} = error "Not Implemented: SqlServer test harness support for unique indexes"
createTable _ Schema.Table {tableConstraints = _ : _} = error "Not Implemented: SqlServer test harness support for constraints"
createTable testEnvironment Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences} = do
  let schemaName = Schema.getSchemaName testEnvironment
  run_ testEnvironment
    $ T.unpack
    $ T.unwords
      [ "CREATE TABLE",
        Schema.unSchemaName schemaName <> "." <> tableName,
        "(",
        commaSeparated
          $ (mkColumn <$> tableColumns)
          <> (bool [mkPrimaryKey pk] [] (null pk))
          <> (mkReference <$> tableReferences),
        ");"
      ]

scalarType :: (HasCallStack) => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "int"
  Schema.TDouble -> "float(53)"
  Schema.TStr -> "nvarchar(127)"
  Schema.TUTCTime -> "time"
  Schema.TBool -> "bit"
  Schema.TGeography -> "geography"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstMssql

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
mkReference Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn, referenceCascade} =
  T.unwords
    $ [ "CONSTRAINT ",
        constraintName,
        "FOREIGN KEY ",
        "(",
        wrapIdentifier referenceLocalColumn,
        ")",
        "REFERENCES",
        T.pack Constants.sqlserverDb <> "." <> referenceTargetTable,
        "(",
        wrapIdentifier referenceTargetColumn,
        ")"
      ]
    ++ [ x | referenceCascade, x <-
                                 [ "ON DELETE CASCADE",
                                   "ON UPDATE CASCADE"
                                 ]
       ]
  where
    constraintName :: Text
    constraintName =
      "FK_"
        <> referenceTargetTable
        <> "_"
        <> referenceTargetColumn
        <> "_"
        <> referenceLocalColumn

-- | Serialize tableData into a T-SQL insert statement and execute it.
insertTable :: (HasCallStack) => TestEnvironment -> Schema.Table -> IO ()
insertTable testEnvironment Schema.Table {tableName, tableColumns, tableData}
  | null tableData = pure ()
  | otherwise = do
      let schemaName = Schema.getSchemaName testEnvironment
      run_ testEnvironment
        $ T.unpack
        $ T.unwords
          [ "INSERT INTO",
            Schema.unSchemaName schemaName <> "." <> wrapIdentifier tableName,
            "(",
            commaSeparated (wrapIdentifier . Schema.columnName <$> tableColumns),
            ")",
            "VALUES",
            commaSeparated $ mkRow <$> tableData,
            ";"
          ]

-- | MSSQL identifiers which may contain spaces or be case-sensitive needs to be wrapped in @[]@.
--
--   More information can be found in the mssql docs:
--   https://docs.microsoft.com/en-us/sql/relational-databases/databases/database-identifiers
wrapIdentifier :: Text -> Text
wrapIdentifier identifier = "[" <> T.replace "]" "]]" identifier <> "]"

-- | 'ScalarValue' serializer for Mssql
serialize :: ScalarValue -> Text
serialize = \case
  VInt int -> tshow int
  VDouble d -> tshow d
  VStr s -> "'" <> T.replace "'" "\'" s <> "'"
  VUTCTime t -> T.pack $ formatTime defaultTimeLocale "'%F %T'" t
  VBool b -> tshow @Int $ if b then 1 else 0
  VGeography (Schema.WKT wkt) -> T.concat ["st_geogfromtext(\'", wkt, "\')"]
  VNull -> "NULL"
  VCustomValue bsv -> Schema.formatBackendScalarValueType $ Schema.backendScalarValue bsv bsvMssql

mkRow :: [Schema.ScalarValue] -> Text
mkRow row =
  T.unwords
    [ "(",
      commaSeparated $ serialize <$> row,
      ")"
    ]

-- | Serialize Table into a T-SQL DROP statement and execute it
dropTable :: (HasCallStack) => TestEnvironment -> Schema.Table -> IO ()
dropTable testEnvironment Schema.Table {tableName} = do
  run_ testEnvironment
    $ T.unpack
    $ T.unwords
      [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
        T.pack Constants.sqlserverDb <> "." <> tableName,
        ";"
      ]

-- | Post an http request to start tracking the table
trackTable :: (HasCallStack) => TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment table =
  Schema.trackTable (backendSourceName backendTypeMetadata) table testEnvironment

-- | Post an http request to stop tracking the table
untrackTable :: (HasCallStack) => TestEnvironment -> Schema.Table -> IO ()
untrackTable testEnvironment table =
  Schema.untrackTable (backendSourceName backendTypeMetadata) table testEnvironment

-- | create a database to use and later drop for these tests
-- note we use the 'initial' connection string here, ie, the one we started
-- with.
createDatabase :: TestEnvironment -> IO ()
createDatabase testEnvironment = do
  let dbName = Constants.uniqueDbName (uniqueTestId testEnvironment)
  runWithInitialDb_
    testEnvironment
    [i|CREATE DATABASE #{dbName}|]

  let schemaName = Schema.getSchemaName testEnvironment
  createSchema testEnvironment schemaName

-- | We drop databases at the end of test runs so we don't need to do DB cleanup.
dropDatabase :: TestEnvironment -> IO ()
dropDatabase testEnvironment = do
  let dbName = Constants.uniqueDbName (uniqueTestId testEnvironment)

  runWithInitialDb_
    testEnvironment
    [i|DROP DATABASE #{dbName}|]
    `catch` \(ex :: SomeException) -> testLogMessage testEnvironment (LogDropDBFailedWarning dbName ex)

-- Because the test harness sets the schema name we use for testing, we need
-- to make sure it exists before we run the tests.
createSchema :: TestEnvironment -> Schema.SchemaName -> IO ()
createSchema testEnvironment schemaName = do
  run_
    testEnvironment
    [i|
      CREATE SCHEMA #{Schema.unSchemaName schemaName};
    |]

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: (HasCallStack) => [Schema.Table] -> (TestEnvironment, ()) -> IO ()
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
teardown :: (HasCallStack) => [Schema.Table] -> (TestEnvironment, ()) -> IO ()
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
