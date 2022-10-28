{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | General Purpose Sqlite GDC Fixture.
--
-- NOTE: This module is intended to be imported qualified.
module Harness.Backend.DataConnector.Sqlite
  ( setupTablesAction,
    setupPermissionsAction,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Text.Extended qualified as Text (commaSeparated)
import Data.Time qualified as Time
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Permissions qualified as Permissions
import Harness.Test.Schema (SchemaName)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | Setup the given permissions to the graphql engine in a TestEnvironment.
setupPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
setupPermissions permissions env = Permissions.setup Fixture.DataConnectorSqlite permissions env

-- | Remove the given permissions from the graphql engine in a TestEnvironment.
teardownPermissions :: [Permissions.Permission] -> TestEnvironment -> IO ()
teardownPermissions permissions env = Permissions.teardown Fixture.DataConnectorSqlite permissions env

setupPermissionsAction :: [Permissions.Permission] -> TestEnvironment -> Fixture.SetupAction
setupPermissionsAction permissions env =
  Fixture.SetupAction
    (setupPermissions permissions env)
    (const $ teardownPermissions permissions env)

--------------------------------------------------------------------------------

setupTablesAction :: [Schema.Table] -> TestEnvironment -> Fixture.SetupAction
setupTablesAction ts env =
  Fixture.SetupAction
    (setup ts (env, ()))
    (const $ teardown ts (env, ()))

-- | Metadata source information for the default Sqlite instance.
sourceMetadata :: Aeson.Value
sourceMetadata =
  let source = Fixture.defaultSource Fixture.DataConnectorSqlite
      backendType = Fixture.defaultBackendTypeString Fixture.DataConnectorSqlite
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
  let backendType = Fixture.defaultBackendTypeString Fixture.DataConnectorSqlite
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
    Schema.trackObjectRelationships Fixture.DataConnectorSqlite table testEnvironment
    Schema.trackArrayRelationships Fixture.DataConnectorSqlite table testEnvironment

-- | Post an http request to start tracking the table
trackTable :: HasCallStack => TestEnvironment -> Schema.Table -> IO ()
trackTable testEnvironment Schema.Table {tableName} = do
  let backendType = Fixture.defaultBackendTypeString Fixture.DataConnectorSqlite
      requestType = backendType <> "_track_table"
      source = Fixture.defaultSource Fixture.DataConnectorSqlite
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
  let backendType = Fixture.defaultBackendTypeString Fixture.DataConnectorSqlite
      source = Fixture.defaultSource Fixture.DataConnectorSqlite
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
        Schema.untrackRelationships Fixture.DataConnectorSqlite table testEnvironment
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
  Schema.runSQL Fixture.DataConnectorSqlite source sql testEnvironment
  GraphqlEngine.reloadMetadata testEnvironment

-- | Serialize Table into a SQLite statement, as needed, and execute it on the SQLite backend
createTable :: TestEnvironment -> Schema.Table -> IO ()
createTable testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableConstraints, tableUniqueIndexes} = do
  let schemaName = Schema.getSchemaName testEnv

  -- Build a SQL QUERY AND THEN CALL run_sql
  let expr =
        Text.unpack $
          Text.unwords
            [ "CREATE TABLE",
              wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName,
              "(",
              Text.commaSeparated $
                (mkColumn <$> tableColumns)
                  <> (bool [mkPrimaryKey pk] [] (null pk))
                  <> (mkReference schemaName <$> tableReferences)
                  <> map uniqueConstraint tableConstraints,
              ");"
            ]
  runSql testEnv (Fixture.defaultSource Fixture.DataConnectorSqlite) expr
  for_ tableUniqueIndexes (createUniqueIndex testEnv tableName)

indexName :: Text -> [Text] -> Text
indexName tableName cols = Text.intercalate "-" $ tableName : cols

uniqueConstraint :: Schema.Constraint -> Text
uniqueConstraint = \case
  Schema.UniqueConstraintColumns cols ->
    Text.unwords $ ["UNIQUE ", "("] ++ [Text.commaSeparated cols] ++ [")"]
  Schema.CheckConstraintExpression ex ->
    Text.unwords $ ["CHECK ", "(", ex, ")"]

createUniqueIndex :: TestEnvironment -> Text -> Schema.UniqueIndex -> IO ()
createUniqueIndex testEnv tableName = \case
  Schema.UniqueIndexColumns cols -> do
    let schemaName = Schema.getSchemaName testEnv
        tableIdentifier = wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName
    runSql testEnv (Fixture.defaultSource Fixture.DataConnectorSqlite) $ Text.unpack $ Text.unwords $ ["CREATE UNIQUE INDEX", indexName tableIdentifier cols, " ON ", tableName, "("] ++ [Text.commaSeparated cols] ++ [")"]
  Schema.UniqueIndexExpression ex -> do
    let schemaName = Schema.getSchemaName testEnv
        tableIdentifier = wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName
    runSql testEnv (Fixture.defaultSource Fixture.DataConnectorSqlite) $ Text.unpack $ Text.unwords $ ["CREATE UNIQUE INDEX", indexName tableIdentifier [ex], " ON ", tableName, "((", ex, "))"]

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
      Text.commaSeparated $ map wrapIdentifier key,
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
  Schema.VUTCTime t -> Text.pack $ Time.formatTime Time.defaultTimeLocale "'%F %T'" t
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
    runSql testEnv (Fixture.defaultSource Fixture.DataConnectorSqlite) $
      Text.unpack $
        Text.unwords
          [ "INSERT INTO",
            wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName,
            "(",
            Text.commaSeparated (wrapIdentifier . Schema.columnName <$> tableColumns),
            ")",
            "VALUES",
            Text.commaSeparated $ mkRow <$> tableData,
            ";"
          ]

mkRow :: [Schema.ScalarValue] -> Text
mkRow row =
  Text.unwords
    [ "(",
      Text.commaSeparated $ serialize <$> row,
      ")"
    ]

-- | Serialize Table into a PL-SQL DROP statement and execute it
dropTable :: TestEnvironment -> Schema.Table -> IO ()
dropTable testEnv Schema.Table {tableName} = do
  let schemaName = Schema.getSchemaName testEnv
  runSql testEnv (Fixture.defaultSource Fixture.DataConnectorSqlite) $
    Text.unpack $
      Text.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName,
          -- "CASCADE",
          ";"
        ]

wrapIdentifier :: Text -> Text
wrapIdentifier identifier = "\"" <> identifier <> "\""
