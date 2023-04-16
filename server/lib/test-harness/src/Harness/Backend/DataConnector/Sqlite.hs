{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | General Purpose Sqlite GDC Fixture.
--
-- NOTE: This module is intended to be imported qualified.
module Harness.Backend.DataConnector.Sqlite
  ( backendTypeMetadata,
    setupTablesAction,
    setupSqliteAgent,
    createEmptyDatasetCloneSourceConfig,
    deleteDatasetClone,
    createTable,
    insertTable,
    trackTable,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as Text
import Data.Text.Extended qualified as Text (commaSeparated)
import Data.Time qualified as Time
import Harness.DataConnectorAgent (createClone, createClone', deleteClone, deleteClone')
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (SchemaName)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude

--------------------------------------------------------------------------------

backendTypeMetadata :: BackendType.BackendTypeConfig
backendTypeMetadata =
  BackendType.BackendTypeConfig
    { backendType = BackendType.DataConnectorSqlite,
      backendSourceName = "chinook_sqlite",
      backendCapabilities =
        Just
          [yaml|
        data_schema:
          supports_primary_keys: true
          supports_foreign_keys: true
        scalar_types:
          DateTime: {}
        queries: {}
        relationships: {}
        comparisons:
          subquery:
            supports_relations: true
        explain: {}
        metrics: {}
        raw: {}
    |],
      backendTypeString = "sqlite",
      backendDisplayNameString = "Hasura SQLite (sqlite)",
      backendReleaseNameString = Nothing,
      backendServerUrl = Just "http://localhost:65007",
      backendSchemaKeyword = "schema",
      backendScalarType = scalarType
    }

--------------------------------------------------------------------------------

setupTablesAction :: [Schema.Table] -> TestEnvironment -> Fixture.SetupAction
setupTablesAction ts env =
  Fixture.SetupAction
    (setup ts (env, ()))
    (const $ teardown ts (env, ()))

-- | Metadata source information for the default Sqlite instance.
sourceMetadata :: API.Config -> Aeson.Value
sourceMetadata (API.Config config) =
  let source = Fixture.backendSourceName backendTypeMetadata
      backendType = BackendType.backendTypeString backendTypeMetadata
   in [yaml|
        name: *source
        kind: *backendType
        tables: []
        configuration:
          value: *config
      |]

backendConfig :: Aeson.Value
backendConfig =
  let backendType = BackendType.backendTypeString backendTypeMetadata
   in [yaml|
dataconnector:
  *backendType:
    uri: *sqliteAgentUri
|]

sqliteAgentUri :: String
sqliteAgentUri = "http://127.0.0.1:65007/"

createEmptyDatasetCloneSourceConfig :: API.DatasetCloneName -> IO API.Config
createEmptyDatasetCloneSourceConfig cloneName = do
  enableExplicitMainSchema . API._dccrConfig <$> createClone' sqliteAgentUri cloneName (API.DatasetTemplateName "Empty")

deleteDatasetClone :: API.DatasetCloneName -> IO API.DatasetDeleteCloneResponse
deleteDatasetClone cloneName =
  deleteClone' sqliteAgentUri cloneName

enableExplicitMainSchema :: API.Config -> API.Config
enableExplicitMainSchema (API.Config config) =
  API.Config $ KeyMap.insert "explicit_main_schema" (Aeson.Bool True) config

setupSqliteAgent :: TestEnvironment -> IO ()
setupSqliteAgent testEnvironment =
  GraphqlEngine.addDataConnectorAgent testEnvironment (BackendType.backendTypeString backendTypeMetadata) sqliteAgentUri

-- | Setup the schema in the most expected way.
-- NOTE: Certain test modules may warrant having their own local version.
setup :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
setup tables (testEnvironment, _) = do
  -- Create the database clone
  cloneConfig <- enableExplicitMainSchema . API._dccrConfig <$> createClone sqliteAgentUri testEnvironment (API.DatasetTemplateName "Empty")
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment (sourceMetadata cloneConfig) (Just backendConfig)
  -- Setup and track tables
  let sourceName = Fixture.backendSourceName backendTypeMetadata
  for_ tables $ \table -> do
    createTable sourceName testEnvironment table
    insertTable sourceName testEnvironment table
    trackTable sourceName testEnvironment table
  -- Setup relationships
  for_ tables $ \table -> do
    Schema.trackObjectRelationships table testEnvironment
    Schema.trackArrayRelationships table testEnvironment

-- | Post an http request to start tracking the table
trackTable :: HasCallStack => String -> TestEnvironment -> Schema.Table -> IO ()
trackTable sourceName testEnvironment Schema.Table {tableName} = do
  let backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_track_table"
      schemaName = Schema.getSchemaName testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: *requestType
      args:
        source: *sourceName
        table:
          - *schemaName
          - *tableName
    |]

-- | Post an http request to stop tracking the table
untrackTable :: String -> TestEnvironment -> Schema.Table -> IO ()
untrackTable sourceName testEnvironment Schema.Table {tableName} = do
  let backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_table"
      schemaName = Schema.getSchemaName testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: *requestType
      args:
        source: *sourceName
        table:
          - *schemaName
          - *tableName
    |]

-- | Teardown the schema and tracking in the most expected way.
-- NOTE: Certain test modules may warrant having their own version.
teardown :: [Schema.Table] -> (TestEnvironment, ()) -> IO ()
teardown (reverse -> tables) (testEnvironment, _) = do
  let sourceName = Fixture.backendSourceName backendTypeMetadata
  finally
    -- Teardown relationships first
    ( forFinally_ tables $ \table ->
        Schema.untrackRelationships table testEnvironment
    )
    ( finally
        -- Then teardown tables
        ( forFinally_ tables $ \table ->
            finally
              (untrackTable sourceName testEnvironment table)
              (dropTable sourceName testEnvironment table)
        )
        -- Then delete the db clone
        (deleteClone sqliteAgentUri testEnvironment)
    )

-- | Call the Metadata API and pass in a Raw SQL statement to the
-- SQLite Agent.
runSql :: TestEnvironment -> String -> String -> IO ()
runSql testEnvironment source sql = do
  Schema.runSQL source sql testEnvironment

-- | Serialize Table into a SQLite statement, as needed, and execute it on the SQLite backend
createTable :: String -> TestEnvironment -> Schema.Table -> IO ()
createTable sourceName testEnv Schema.Table {tableName, tableColumns, tablePrimaryKey = pk, tableReferences, tableConstraints, tableUniqueIndexes} = do
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
  runSql testEnv sourceName expr
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
    runSql testEnv (Fixture.backendSourceName backendTypeMetadata) $ Text.unpack $ Text.unwords $ ["CREATE UNIQUE INDEX", indexName tableIdentifier cols, " ON ", tableName, "("] ++ [Text.commaSeparated cols] ++ [")"]
  Schema.UniqueIndexExpression ex -> do
    let schemaName = Schema.getSchemaName testEnv
        tableIdentifier = wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName
    runSql testEnv (Fixture.backendSourceName backendTypeMetadata) $ Text.unpack $ Text.unwords $ ["CREATE UNIQUE INDEX", indexName tableIdentifier [ex], " ON ", tableName, "((", ex, "))"]

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
      -- wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier referenceTargetTable,
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
insertTable :: String -> TestEnvironment -> Schema.Table -> IO ()
insertTable sourceName testEnv Schema.Table {tableName, tableColumns, tableData}
  | null tableData = pure ()
  | otherwise = do
      let schemaName = Schema.getSchemaName testEnv
      runSql testEnv sourceName $
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
dropTable :: String -> TestEnvironment -> Schema.Table -> IO ()
dropTable sourceName testEnv Schema.Table {tableName} = do
  let schemaName = Schema.getSchemaName testEnv
  runSql testEnv sourceName $
    Text.unpack $
      Text.unwords
        [ "DROP TABLE", -- we don't want @IF EXISTS@ here, because we don't want this to fail silently
          wrapIdentifier (Schema.unSchemaName schemaName) <> "." <> wrapIdentifier tableName,
          -- "CASCADE",
          ";"
        ]

wrapIdentifier :: Text -> Text
wrapIdentifier identifier = "\"" <> identifier <> "\""
