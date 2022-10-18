{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Harness.Backend.DataConnector.Sqlite
  ( setupTablesAction,
    setupPermissionsAction,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.Extended (commaSeparated)
import Data.Time (defaultTimeLocale, formatTime)
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture
  ( BackendType (..),
    SetupAction (..),
    defaultBackendTypeString,
    defaultSource,
  )
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema (SchemaName)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setupPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
setupPermissions permissions env = Permissions.setup DataConnectorSqlite permissions env

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardownPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
teardownPermissions permissions env = Permissions.teardown DataConnectorSqlite permissions env

setupPermissionsAction :: [Permissions.Permission] -> TestEnvironment -> SetupAction
setupPermissionsAction permissions env =
  SetupAction
    (setupPermissions permissions env)
    (const $ teardownPermissions permissions env)

--------------------------------------------------------------------------------

setupTablesAction :: [Schema.Table] -> TestEnvironment -> SetupAction
setupTablesAction ts env =
  SetupAction
    (setup ts (env, ()))
    (const $ teardown ts (env, ()))

-- | Metadata source information for the default Sqlite instance.
sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = defaultSource DataConnectorSqlite
      backendType = defaultBackendTypeString DataConnectorSqlite
   in [yaml|
name: *source
kind: *backendType
tables: []
configuration:
  db: "/db.sqlite"
  explicit_main_schema: true
|]

backendConfig :: Aeson.Value
backendConfig =
  let backendType = defaultBackendTypeString DataConnectorSqlite
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://127.0.0.1:65007/"
|]

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
setup tables (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment sourceMetadata (Just backendConfig)
  -- Setup and track tables
  for_ tables $ \table -> do
    createTable testEnvironment table
    insertTable testEnvironment table
    trackTable testEnvironment table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships DataConnectorSqlite table testEnvironment
    Schema.trackArrayRelationships DataConnectorSqlite table testEnvironment

-- | Post an http request to start tracking the table
trackTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment Schema.Table {tableName} = do
  let backendType = defaultBackendTypeString DataConnectorSqlite
      requestType = backendType <> "_track_table"
      source = defaultSource DataConnectorSqlite
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: *requestType
args:
  source: *source
  table:
    - "main"
    - *tableName
|]

-- | Post an http request to stop tracking the table
untrackTable :: TestEnvironment -> Schema.Table -> IO ()
untrackTable testEnvironment Schema.Table {tableName} = do
  let backendType = defaultBackendTypeString DataConnectorSqlite
      source = defaultSource DataConnectorSqlite
      requestType = backendType <> "_untrack_table"
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: *requestType
args:
  source: *source
  table:
    - "main"
    - *tableName
|]

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
          (dropTable testEnvironment table)
    )

-- | Call the Metadata API and pass in a Raw SQL statement to the
-- SQLite Agent.
runSql :: TestEnvironment -> String -> String -> IO ()
runSql testEnvironment source sql = do
  Schema.runSQL DataConnectorSqlite source sql testEnvironment
  GraphqlEngine.reloadMetadata testEnvironment

-- | Serialize Table into a SQLite statement, as needed, and execute it on the SQLite backend
createTable :: TestEnvironment -> Schema.Table -> IO ()
createTable testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableUniqueConstraints} = do
  let schemaName = Schema.getSchemaName testEnv

  -- Build a SQL QUERY AND THEN CALL run_sql
  let expr =
        Text.unpack $
          Text.unwords
            [ "CREATE TABLE",
              wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName,
              "(",
              commaSeparated $
                (mkColumn <$> tableColumns)
                  <> (bool [mkPrimaryKey pk] [] (null pk))
                  <> (mkReference schemaName <$> tableReferences),
              ");"
            ]
  runSql testEnv (defaultSource DataConnectorSqlite) expr
  for_ tableUniqueConstraints (createUniqueConstraint testEnv tableName)

indexName :: Text -> [Text] -> Text
indexName tableName cols = Text.intercalate "-" $ tableName : cols

createUniqueConstraint :: TestEnvironment -> Text -> Schema.UniqueConstraint -> IO ()
createUniqueConstraint testEnv tableName = \case
  Schema.UniqueConstraintColumns cols -> do
    let schemaName = Schema.getSchemaName testEnv
        tableIdentifier = wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName
    runSql testEnv (defaultSource DataConnectorSqlite) $ Text.unpack $ Text.unwords $ ["CREATE UNIQUE INDEX", indexName tableIdentifier cols, " ON ", tableName, "("] ++ [commaSeparated cols] ++ [")"]
  Schema.UniqueConstraintExpression ex -> do
    let schemaName = Schema.getSchemaName testEnv
        tableIdentifier = wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName
    runSql testEnv (defaultSource DataConnectorSqlite) $ Text.unpack $ Text.unwords $ ["CREATE UNIQUE INDEX", indexName tableIdentifier [ex], " ON ", tableName, "((", ex, "))"]

mkColumn :: Schema.Column -> Text
mkColumn Schema.Column {columnName, columnType, columnNullable, columnDefault} =
  Text.unwords
    [ wrapIdentifier columnName,
      scalarType columnType,
      bool "NOT NULL" "DEFAULT NULL" columnNullable,
      maybe "" ("DEFAULT " <>) columnDefault
    ]

mkPrimaryKey :: [Text] -> Text
mkPrimaryKey key =
  Text.unwords
    [ "PRIMARY KEY",
      "(",
      commaSeparated $ map wrapIdentifier key,
      ")"
    ]

mkReference :: SchemaName -> Schema.Reference -> Text
mkReference _schemaName Schema.Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} =
  Text.unwords
    [ "FOREIGN KEY",
      "(",
      wrapIdentifier referenceLocalColumn,
      ")",
      "REFERENCES",
      -- NOTE: Sqlite doesn't allow schema names in references. Can we resolve this with an alias?
      --wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier referenceTargetTable,
      wrapIdentifier referenceTargetTable,
      "(",
      wrapIdentifier referenceTargetColumn,
      ")",
      "ON DELETE CASCADE",
      "ON UPDATE CASCADE"
    ]

scalarType :: Schema.ScalarType -> Text
scalarType = \case
  Schema.TInt -> "INTEGER"
  Schema.TStr -> "TEXT"
  Schema.TUTCTime -> "TIMESTAMP"
  Schema.TBool -> "BOOLEAN"
  Schema.TGeography -> "BLOB"
  Schema.TCustomType txt -> Schema.getBackendScalarType txt Schema.bstSqlite

-- | 'ScalarValue' serializer for Sqlite
serialize :: Schema.ScalarValue -> Text
serialize = \case
  Schema.VInt i -> tshow i
  Schema.VStr s -> "'" <> Text.replace "'" "\'" s <> "'"
  Schema.VUTCTime t -> Text.pack $ formatTime defaultTimeLocale "'%F %T'" t
  Schema.VBool b -> if b then "1" else "0"
  Schema.VGeography (Schema.WKT wkt) -> Text.concat ["st_geogfromtext(\'", wkt, "\')"]
  Schema.VNull -> "NULL"
  Schema.VCustomValue bsv -> Schema.formatBackendScalarValueType $ Schema.backendScalarValue bsv Schema.bsvSqlite

-- | Serialize tableData into a SQLITE-SQL insert statement and execute it.
insertTable :: TestEnvironment -> Schema.Table -> IO ()
insertTable testEnv Schema.Table {tableName, tableColumns, tableData}
  | null tableData = pure ()
  | otherwise = do
    let schemaName = Schema.getSchemaName testEnv
    runSql testEnv (defaultSource DataConnectorSqlite) $
      Text.unpack $
        Text.unwords
          [ "INSERT INTO",
            wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName,
            "(",
            commaSeparated (wrapIdentifier . Schema.columnName <$> tableColumns),
            ")",
            "VALUES",
            commaSeparated $ mkRow <$> tableData,
            ";"
          ]

mkRow :: [Schema.ScalarValue] -> Text
mkRow row =
  Text.unwords
    [ "(",
      commaSeparated $ serialize <$> row,
      ")"
    ]

-- | Serialize Table into a PL-SQL DROP statement and execute it
dropTable :: TestEnvironment -> Schema.Table -> IO ()
dropTable testEnv Schema.Table {tableName} = do
  let schemaName = Schema.getSchemaName testEnv
  runSql testEnv (defaultSource DataConnectorSqlite) $
    Text.unpack $
      Text.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName,
          -- "CASCADE",
          ";"
        ]

wrapIdentifier :: Text -> Text
wrapIdentifier identifier = "\"" <> identifier <> "\""
