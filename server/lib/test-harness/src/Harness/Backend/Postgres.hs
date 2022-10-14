{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | PostgreSQL helpers.
module Harness.Backend.Postgres
  ( livenessCheck,
    run_,
    runSQL,
    defaultSourceMetadata,
    defaultSourceConfiguration,
    createTable,
    insertTable,
    dropTable,
    dropTableIfExists,
    trackTable,
    untrackTable,
    setup,
    teardown,
    setupTablesAction,
    setupPermissionsAction,
    setupFunctionRootFieldAction,
    setupComputedFieldAction,
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
import Harness.Test.BackendType (BackendType (Postgres), defaultBackendTypeString, defaultSource)
import Harness.Test.Fixture (SetupAction (..))
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema
  ( BackendScalarType (..),
    BackendScalarValue (..),
    ScalarValue (..),
    SchemaName (..),
  )
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude
import System.Process.Typed

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
            sleep Constants.postgresLivenessCheckIntervalSeconds
            loop (attempts - 1)
        )

-- | Run a plain SQL query.
-- On error, print something useful for debugging.
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

runSQL :: String -> TestEnvironment -> IO ()
runSQL = Schema.runSQL Postgres (defaultSource Postgres)

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
createTable :: TestEnvironment -> Schema.Table -> IO ()
createTable testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableUniqueConstraints} = do
  let schemaName = Schema.getSchemaName testEnv
  run_ $
    T.unpack $
      T.unwords
        [ "CREATE TABLE",
          T.pack Constants.postgresDb <> "." <> wrapIdentifier tableName,
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
  Schema.TCustomType txt -> Schema.getBackendScalarType txt bstPostgres

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
  VInt i -> tshow i
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
dropTable :: Schema.Table -> IO ()
dropTable Schema.Table {tableName} = do
  run_ $
    T.unpack $
      T.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          T.pack Constants.postgresDb <> "." <> tableName,
          -- "CASCADE",
          ";"
        ]

dropTableIfExists :: Schema.Table -> IO ()
dropTableIfExists Schema.Table {tableName} = do
  run_ $
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
    Schema.trackObjectRelationships Postgres table testEnvironment
    Schema.trackArrayRelationships Postgres table testEnvironment

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
teardown (reverse -> tables) (testEnvironment, _) = do
  finally
    -- Teardown relationships first
    ( forFinally_ tables $ \table ->
        Schema.untrackRelationships Postgres table testEnvironment
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
