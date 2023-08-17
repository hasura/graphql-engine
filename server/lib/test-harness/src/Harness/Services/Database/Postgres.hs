{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

-- | This module houses low-level functions and types to help access and work
-- with Postgres servers. For functions and types to help test the Postgres HGE
-- backend, see 'Harness.Backend.Postgres'.
module Harness.Services.Database.Postgres
  ( FreshPostgresDb (..),
    PostgresServerUrl (..),
    run,
    createDatabase,
    dropDatabase,
    mkFreshPostgresDb,
    mkFreshPostgresDbIO,
    mkFreshDbConnectionString,
    withFreshPostgresDb,
    createTable,
    insertTable,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Extended (sleep)
import Control.Monad.Managed
import Data.Aeson (ToJSON)
import Data.ByteString.Char8 qualified as S8
import Data.Char qualified
import Data.Has
import Data.String
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Extended (commaSeparated)
import Data.Time (defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple qualified as Postgres
import Harness.Exceptions
import Harness.Logging
import Harness.Schema.Name
import Harness.Schema.Table qualified as Schema
import Harness.Test.ScalarType
import Hasura.Prelude
import Test.Hspec

newtype PostgresServerUrl = PostgresServerUrl {getPostgresServerUrl :: Text}
  deriving newtype (ToJSON)
  deriving (Eq, Generic)

instance Hashable PostgresServerUrl

newtype FreshPostgresDb = FreshPostgresDb {freshDbName :: Text}

--- | Run a plain SQL query.
-- On error, print something useful for debugging.
run ::
  forall testEnv.
  ( Has Logger testEnv,
    Has PostgresServerUrl testEnv,
    HasCallStack
  ) =>
  testEnv ->
  Text ->
  IO ()
run testEnv query = do
  let emitLog = runLogger $ getter @Logger testEnv
      (PostgresServerUrl connectionString) = getter testEnv

  startTime <- getCurrentTime
  catch
    ( bracket
        ( Postgres.connectPostgreSQL
            (encodeUtf8 connectionString)
        )
        Postgres.close
        (\conn -> void (Postgres.execute_ conn (fromString $ T.unpack query)))
    )
    ( \(e :: Postgres.SqlError) ->
        error
          ( unlines
              [ "PostgreSQL query error:",
                S8.unpack (Postgres.sqlErrorMsg e),
                "SQL was:",
                T.unpack query
              ]
          )
    )
  endTime <- getCurrentTime
  emitLog $ LogDBQuery connectionString query (diffUTCTime endTime startTime)

mkFreshPostgresDb :: (Has Logger testEnvironment, Has PostgresServerUrl testEnvironment) => testEnvironment -> Managed FreshPostgresDb
mkFreshPostgresDb testEnv = do
  (res, cleanup) <- liftIO $ mkFreshPostgresDbIO testEnv
  managed_ (<* cleanup)
  return res

mkFreshPostgresDbIO :: (Has Logger testEnvironment, Has PostgresServerUrl testEnvironment) => testEnvironment -> IO (FreshPostgresDb, IO ())
mkFreshPostgresDbIO testEnv = do
  freshDbName <- drawFreshDbName
  createDatabase testEnv freshDbName
  return
    $ ( FreshPostgresDb freshDbName,
        dropDatabase testEnv freshDbName
      )
  where
    drawFreshDbName :: IO Text
    drawFreshDbName = do
      uuid <- tshow <$> liftIO UUID.nextRandom
      return
        $ "freshdb_"
        <> T.map
          ( \a ->
              if Data.Char.isAlphaNum a
                then a
                else '_'
          )
          uuid

mkFreshDbConnectionString :: PostgresServerUrl -> FreshPostgresDb -> PostgresServerUrl
mkFreshDbConnectionString (PostgresServerUrl pgUrl) (FreshPostgresDb db) =
  PostgresServerUrl $ T.dropWhileEnd ((/= '/')) pgUrl <> db

-- | create a database to use and later drop for these tests
-- note we use the 'initial' connection string here, ie, the one we started
-- with.
createDatabase :: (Has Logger testEnvironment, Has PostgresServerUrl testEnvironment) => testEnvironment -> Text -> IO ()
createDatabase testEnv dbName = do
  run testEnv ("CREATE DATABASE " <> dbName)

-- | we drop databases at the end of test runs so we don't need to do DB clean
-- up.
dropDatabase ::
  ( Has Logger testEnvironment,
    Has PostgresServerUrl testEnvironment
  ) =>
  testEnvironment ->
  Text ->
  IO ()
dropDatabase testEnv dbName = do
  let emitLog = runLogger $ getter testEnv
  void $ forkIO $ do
    -- we don't really mind when this happens, and we don't want to block on it,
    -- so we drop the DB in a thread, and wait 10 seconds first to let any
    -- business finish
    sleep 10
    -- if this fails, don't make the test fail, we're doing our best to clean up
    run
      testEnv
      ("DROP DATABASE " <> dbName <> ";")
      `catch` \(ex :: SomeException) -> emitLog (LogDropDBFailedWarning dbName ex)

withFreshPostgresDb ::
  ( Has PostgresServerUrl testEnv,
    Has Logger testEnv
  ) =>
  SpecWith (FreshPostgresDb, testEnv) ->
  SpecWith testEnv
withFreshPostgresDb specs =
  flip aroundWith specs \action testEnv -> runManaged do
    liftIO $ testLogMessage testEnv (logHarness ("making fresh db" :: Text))
    db <- mkFreshPostgresDb testEnv
    liftIO $ action (db, testEnv)

-- | Serialize Table into a PL-SQL statement, as needed, and execute it on the Postgres backend
createTable ::
  ( Has Logger testEnvironment,
    Has PostgresServerUrl testEnvironment,
    Has SchemaName testEnvironment
  ) =>
  testEnvironment ->
  Schema.Table ->
  IO ()
createTable testEnv Schema.Table {..} = do
  let schemaName = getter testEnv

  createSchema testEnv schemaName

  run
    testEnv
    [i|
      CREATE TABLE #{ (unSchemaName $ schemaName) <> "." <> tableName }
        (#{
          commaSeparated $
            (mkColumnSql <$> tableColumns)
              <> (bool [mkPrimaryKeySql tablePrimaryKey] [] (null tablePrimaryKey))
              <> (mkReferenceSql schemaName <$> tableReferences)
              <> map uniqueConstraintSql tableConstraints
        });
    |]

  for_ tableUniqueIndexes (run testEnv . createUniqueIndexSql schemaName tableName)

-- Because the test harness sets the schema name we use for testing, we need
-- to make sure it exists before we run the tests.
createSchema ::
  ( Has Logger testEnvironment,
    Has PostgresServerUrl testEnvironment
  ) =>
  testEnvironment ->
  SchemaName ->
  IO ()
createSchema testEnv schemaName = do
  -- A transaction means that the @SET LOCAL@ is scoped to this operation.
  -- In other words, whatever the @client_min_messages@ flag's previous value
  -- was will be restored after running this.
  run
    testEnv
    [i|
      BEGIN;
      SET LOCAL client_min_messages = warning;
      CREATE SCHEMA IF NOT EXISTS #{unSchemaName schemaName};
      COMMIT;
  |]

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
  let schemaName = maybe localSchemaName unSchemaName (resolveReferenceSchema referenceTargetQualifiers)
   in [i|
        FOREIGN KEY ("#{ referenceLocalColumn }")
        REFERENCES "#{ schemaName }"."#{ referenceTargetTable }" ("#{ referenceTargetColumn }")
        ON DELETE CASCADE ON UPDATE CASCADE
      |]

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

-- | Identifiers which may be case-sensitive needs to be wrapped in @""@.
--
--   More information can be found in the postgres docs:
--   https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
wrapIdentifier :: Text -> Text
wrapIdentifier identifier = "\"" <> identifier <> "\""

scalarType :: (HasCallStack) => Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "integer"
  Schema.TDouble -> "double precision"
  Schema.TStr -> "varchar"
  Schema.TUTCTime -> "timestamp"
  Schema.TBool -> "boolean"
  Schema.TGeography -> "geography"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstPostgres

-- | Serialize tableData into a PL-SQL insert statement and execute it.
insertTable ::
  ( Has Logger testEnvironment,
    Has SchemaName testEnvironment,
    Has PostgresServerUrl testEnvironment
  ) =>
  testEnvironment ->
  Schema.Table ->
  IO ()
insertTable testEnv table@(Schema.Table {tableColumns, tableData}) = unless (null tableData) do
  let schemaName = getter testEnv
  run
    testEnv
    [i|
      INSERT INTO #{ qualifiedTableName schemaName table }
        (#{ commaSeparated (wrapIdentifier . Schema.columnName <$> tableColumns) })
      VALUES
        #{ commaSeparated $ mkRow <$> tableData };
    |]

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

qualifiedTableName :: SchemaName -> Schema.Table -> Text
qualifiedTableName schemaName table =
  [i| #{ unSchemaName schemaName }."#{ Schema.tableName table }" |]
