{-# LANGUAGE QuasiQuotes #-}

-- | Common interface for setup/teardown for all backends - schema and data
module Harness.Schema
  ( ScalarType (..),
    ScalarValue (..),
    WKT (..),
    TableQualifier (..),
    Constraint (..),
    UniqueIndex (..),
    BackendScalarType (..),
    BackendScalarValue (..),
    BackendScalarValueType (..),
    SchemaName (..),
    NativeQuery (..),
    NativeQueryColumn (..),
    StoredProcedure (..),
    StoredProcedureColumn (..),
    enableOpenTelemetryCommand,
    resolveTableSchema,
    trackTable,
    trackTables,
    trackTablesWithStatus,
    untrackTable,
    untrackTables,
    untrackTablesWithStatus,
    mkTableField,
    trackObjectRelationships,
    trackArrayRelationships,
    untrackRelationships,
    mkObjectRelationshipName,
    mkArrayRelationshipName,
    createTableToNativeQueryRelationship,
    trackFunction,
    untrackFunction,
    trackComputedField,
    untrackComputedField,
    runSQL,
    addSource,
    getSchemaName,
    bulkAtomicCommand,
    module Harness.Schema.Table,
    module Harness.Schema.Name,
    module Harness.Schema.LogicalModel,
    module Harness.Schema.NativeQuery,
    module Harness.Schema.StoredProcedure,
  )
where

import Data.Aeson (Value, (.=))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.Types qualified as J
import Data.Vector qualified as V
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema.LogicalModel
import Harness.Schema.Name
import Harness.Schema.NativeQuery
import Harness.Schema.StoredProcedure
import Harness.Schema.Table
import Harness.Test.BackendType (BackendTypeConfig)
import Harness.Test.BackendType qualified as BackendType
import Harness.TestEnvironment (TestEnvironment (..), getBackendTypeConfig, getSchemaName)
import Hasura.Prelude
import Hasura.RQL.DDL.Warnings (AllowWarnings)

-- | we assume we are using the default schema unless a table tells us
-- otherwise
-- when multiple qualifiers are passed, we assume the last one is the schema
resolveTableSchema :: TestEnvironment -> Table -> SchemaName
resolveTableSchema testEnv tbl =
  case resolveReferenceSchema (coerce $ tableQualifiers tbl) of
    Nothing -> getSchemaName testEnv
    Just schemaName -> schemaName

-- | track_table API call
trackTable :: (HasCallStack) => String -> Table -> TestEnvironment -> IO ()
trackTable source tbl testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_track_table"
      trackTableArgs = mkTrackTableV2ApiObject testEnvironment backendTypeMetadata source tbl
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: *requestType
      args: *trackTableArgs
    |]

mkTrackTableV2ApiObject :: (HasCallStack) => TestEnvironment -> BackendTypeConfig -> String -> Table -> Value
mkTrackTableV2ApiObject testEnvironment backendTypeConfig source tbl@(Table {tableName}) = do
  let schema = resolveTableSchema testEnvironment tbl
      tableField = mkTableField backendTypeConfig schema tableName
      column_config = columnsConfig (tableColumns tbl)
  [yaml|
    source: *source
    table: *tableField
    configuration:
      column_config: *column_config
    |]
  where
    columnsConfig :: [Column] -> Value
    columnsConfig = J.object . mapMaybe columnConfig

    columnConfig :: Column -> Maybe J.Pair
    columnConfig col = do
      alias <- columnGqlAlias col
      return
        $ ( K.fromText $ columnName col,
            [yaml|
        custom_name: *alias
        |]
          )

-- | track_tables API call
trackTables :: (HasCallStack) => String -> [Table] -> TestEnvironment -> IO Value
trackTables source tables testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_track_tables"
      trackTablesArgs = mkTrackTableV2ApiObject testEnvironment backendTypeMetadata source <$> tables
  GraphqlEngine.postMetadata
    testEnvironment
    [yaml|
      type: *requestType
      args:
        tables: *trackTablesArgs
    |]

-- | track_tables API call, with the allow_warnings setting and expecting a specific http response status code
trackTablesWithStatus :: (HasCallStack) => String -> [Table] -> AllowWarnings -> Int -> TestEnvironment -> IO Value
trackTablesWithStatus source tables allowWarnings expectedHttpStatus testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_track_tables"
      trackTablesArgs = mkTrackTableV2ApiObject testEnvironment backendTypeMetadata source <$> tables
  GraphqlEngine.postMetadataWithStatus
    expectedHttpStatus
    testEnvironment
    [yaml|
      type: *requestType
      args:
        tables: *trackTablesArgs
        allow_warnings: *allowWarnings
    |]

-- | untrack_table API call
untrackTable :: (HasCallStack) => String -> Table -> TestEnvironment -> IO ()
untrackTable source tbl testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_table"
      untrackTableArgs = mkUntrackTableApiObject testEnvironment backendTypeMetadata source tbl False
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: *requestType
      args: *untrackTableArgs
    |]

mkUntrackTableApiObject :: (HasCallStack) => TestEnvironment -> BackendTypeConfig -> String -> Table -> Bool -> Value
mkUntrackTableApiObject testEnvironment backendTypeConfig source tbl@(Table {tableName}) cascade = do
  let schema = resolveTableSchema testEnvironment tbl
      tableField = mkTableField backendTypeConfig schema tableName
   in [yaml|
        source: *source
        table: *tableField
        cascade: *cascade
      |]

-- | untrack_tables API call
untrackTables :: (HasCallStack) => String -> [(Table, Bool)] -> TestEnvironment -> IO Value
untrackTables source tablesWithCascade testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_tables"
      untrackTablesArgs = uncurry (mkUntrackTableApiObject testEnvironment backendTypeMetadata source) <$> tablesWithCascade
  GraphqlEngine.postMetadata
    testEnvironment
    [yaml|
      type: *requestType
      args:
        source: *source
        tables: *untrackTablesArgs
    |]

-- | untrack_tables API call, with the allow_warnings setting and expecting a specific http response status code
untrackTablesWithStatus :: (HasCallStack) => String -> [(Table, Bool)] -> AllowWarnings -> Int -> TestEnvironment -> IO Value
untrackTablesWithStatus source tablesWithCascade allowWarnings expectedHttpStatus testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_tables"
      untrackTablesArgs = uncurry (mkUntrackTableApiObject testEnvironment backendTypeMetadata source) <$> tablesWithCascade
  GraphqlEngine.postMetadataWithStatus
    expectedHttpStatus
    testEnvironment
    [yaml|
      type: *requestType
      args:
        source: *source
        tables: *untrackTablesArgs
        allow_warnings: *allowWarnings
    |]

trackFunction :: (HasCallStack) => String -> String -> TestEnvironment -> IO ()
trackFunction source functionName testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      schema = getSchemaName testEnvironment
      requestType = backendType <> "_track_function"
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: *requestType
args:
  function:
    schema: *schema
    name: *functionName
  source: *source
|]

-- | Unified untrack function
untrackFunction :: (HasCallStack) => String -> String -> TestEnvironment -> IO ()
untrackFunction source functionName testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      schema = getSchemaName testEnvironment
  let requestType = backendType <> "_untrack_function"
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: *requestType
args:
  source: *source
  function:
    schema: *schema
    name: *functionName
|]

trackComputedField ::
  (HasCallStack) =>
  String ->
  Table ->
  String ->
  String ->
  J.Value ->
  J.Value ->
  TestEnvironment ->
  IO ()
trackComputedField source Table {tableName} functionName asFieldName argumentMapping returnTable testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      schema = getSchemaName testEnvironment
      schemaKey = BackendType.backendSchemaKeyword backendTypeMetadata
      requestType = backendType <> "_add_computed_field"
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: *requestType
args:
  source: *source
  comment: null
  table:
    *schemaKey: *schema
    name: *tableName
  name: *asFieldName
  definition:
    function:
      *schemaKey: *schema
      name: *functionName
    table_argument: null
    session_argument: null
    argument_mapping: *argumentMapping
    return_table: *returnTable
|]

-- | Unified untrack computed field
untrackComputedField :: (HasCallStack) => String -> Table -> String -> TestEnvironment -> IO ()
untrackComputedField source Table {tableName} fieldName testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      schema = getSchemaName testEnvironment
      schemaKey = BackendType.backendSchemaKeyword backendTypeMetadata
  let requestType = backendType <> "_drop_computed_field"

  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: *requestType
      args:
        source: *source
        table:
          *schemaKey: *schema
          name: *tableName
        name: *fieldName
      |]

-- | Helper to create the object relationship name
mkObjectRelationshipName :: Reference -> Text
mkObjectRelationshipName Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn, referenceTargetQualifiers} =
  let columnName = case resolveReferenceSchema referenceTargetQualifiers of
        Just (SchemaName targetSchema) -> targetSchema <> "_" <> referenceTargetColumn
        Nothing -> referenceTargetColumn
   in referenceTargetTable <> "_by_" <> referenceLocalColumn <> "_to_" <> columnName

-- | Helper to create the object relationship name
mkNativeQueryRelationshipName :: NativeQueryRelationship -> Text
mkNativeQueryRelationshipName NativeQueryRelationship {nqRelationshipLocalColumn, nqRelationshipTarget, nqRelationshipType} =
  let joiner = case nqRelationshipType of
        ObjectRelationship -> "_object_by_"
        ArrayRelationship -> "_array_by_"
   in nqRelationshipTarget <> joiner <> nqRelationshipLocalColumn <> "_to_" <> nqRelationshipLocalColumn

-- | Build an 'J.Value' representing a 'BackendType' specific @TableName@.
mkTableField :: BackendTypeConfig -> SchemaName -> Text -> J.Value
mkTableField backendTypeMetadata schemaName tableName =
  let dcFieldName = J.Array $ V.fromList [J.String (unSchemaName schemaName), J.String tableName]
      nativeFieldName = J.object [BackendType.backendSchemaKeyword backendTypeMetadata .= J.String (unSchemaName schemaName), "name" .= J.String tableName]
   in case BackendType.backendType backendTypeMetadata of
        BackendType.Postgres -> nativeFieldName
        BackendType.SQLServer -> nativeFieldName
        BackendType.BigQuery -> nativeFieldName
        BackendType.Citus -> nativeFieldName
        BackendType.Cockroach -> nativeFieldName
        BackendType.DataConnector _ -> dcFieldName

mkInsertionOrder :: InsertOrder -> Text
mkInsertionOrder BeforeParent = "before_parent"
mkInsertionOrder AfterParent = "after_parent"

-- | Unified track object relationships
trackObjectRelationships :: (HasCallStack) => Table -> TestEnvironment -> IO ()
trackObjectRelationships tbl@(Table {tableName, tableReferences, tableNativeQueryRelationships, tableManualRelationships}) testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      localSchema = resolveTableSchema testEnvironment tbl
      backendType = BackendType.backendTypeString backendTypeMetadata
      source = BackendType.backendSourceName backendTypeMetadata
      tableField = mkTableField backendTypeMetadata localSchema tableName
      requestType = backendType <> "_create_object_relationship"

  for_ tableReferences $ \ref@Reference {referenceLocalColumn} -> do
    let relationshipName = mkObjectRelationshipName ref
    GraphqlEngine.postMetadata_
      testEnvironment
      [yaml|
        type: *requestType
        args:
          source: *source
          table: *tableField
          name: *relationshipName
          using:
            foreign_key_constraint_on: *referenceLocalColumn
      |]

  for_ tableManualRelationships
    $ \ref@Reference
         { referenceLocalColumn,
           referenceTargetTable,
           referenceTargetColumn,
           referenceTargetQualifiers,
           referenceInsertionOrder
         } -> do
        let targetSchema = case resolveReferenceSchema referenceTargetQualifiers of
              Just schema -> schema
              Nothing -> getSchemaName testEnvironment
            relationshipName = mkObjectRelationshipName ref
            insertion_order = mkInsertionOrder referenceInsertionOrder
            targetTableField = mkTableField backendTypeMetadata targetSchema referenceTargetTable
            manualConfiguration :: J.Value
            manualConfiguration =
              J.object
                [ "remote_table" .= targetTableField,
                  "column_mapping"
                    .= J.object [K.fromText referenceLocalColumn .= referenceTargetColumn],
                  "insertion_order" .= J.String insertion_order
                ]
            payload =
              [yaml|
            type: *requestType
            args:
              source: *source
              table: *tableField
              name: *relationshipName
              using:
                manual_configuration: *manualConfiguration
          |]

        GraphqlEngine.postMetadata_ testEnvironment payload

  for_ tableNativeQueryRelationships
    $ \relationship ->
      GraphqlEngine.postMetadata_
        testEnvironment
        (createTableToNativeQueryRelationship testEnvironment localSchema tableName relationship)

createTableToNativeQueryRelationship :: TestEnvironment -> SchemaName -> Text -> NativeQueryRelationship -> Value
createTableToNativeQueryRelationship testEnvironment localSchema tableName ref@NativeQueryRelationship {nqRelationshipLocalColumn, nqRelationshipTarget, nqRelationshipTargetColumn, nqRelationshipType} =
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      tableField = mkTableField backendTypeMetadata localSchema tableName
      source = BackendType.backendSourceName backendTypeMetadata
      backendType = BackendType.backendTypeString backendTypeMetadata
      relationshipName = mkNativeQueryRelationshipName ref
   in case nqRelationshipType of
        ArrayRelationship ->
          let manualConfiguration :: J.Value
              manualConfiguration =
                J.object
                  [ "remote_native_query" .= nqRelationshipTarget,
                    "column_mapping"
                      .= J.object [K.fromText nqRelationshipLocalColumn .= nqRelationshipTargetColumn],
                    "insertion_order" .= J.Null
                  ]
              requestType = backendType <> "_create_array_relationship"
           in [yaml|
                type: *requestType
                args:
                  source: *source
                  table: *tableField
                  name: *relationshipName
                  using:
                    manual_configuration: *manualConfiguration
              |]
        ObjectRelationship ->
          let manualConfiguration :: J.Value
              manualConfiguration =
                J.object
                  [ "remote_native_query" .= nqRelationshipTarget,
                    "column_mapping"
                      .= J.object [K.fromText nqRelationshipLocalColumn .= nqRelationshipTargetColumn],
                    "insertion_order" .= J.Null
                  ]

              requestType = backendType <> "_create_object_relationship"
           in [yaml|
                type: *requestType
                args:
                  source: *source
                  table: *tableField
                  name: *relationshipName
                  using:
                    manual_configuration: *manualConfiguration
              |]

-- | Helper to create the array relationship name
mkArrayRelationshipName :: Text -> Text -> Text -> [Text] -> Text
mkArrayRelationshipName tableName referenceLocalColumn referenceTargetColumn referenceTargetQualifiers =
  let columnName = case resolveReferenceSchema referenceTargetQualifiers of
        Just (SchemaName targetSchema) -> targetSchema <> "_" <> referenceTargetColumn
        Nothing -> referenceTargetColumn
   in tableName <> "s_by_" <> referenceLocalColumn <> "_to_" <> columnName

-- | Unified track array relationships
trackArrayRelationships :: (HasCallStack) => Table -> TestEnvironment -> IO ()
trackArrayRelationships tbl@(Table {tableName, tableReferences, tableNativeQueryRelationships, tableManualRelationships}) testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      localSchema = resolveTableSchema testEnvironment tbl
      backendType = BackendType.backendTypeString backendTypeMetadata
      source = BackendType.backendSourceName backendTypeMetadata
      tableField = mkTableField backendTypeMetadata localSchema tableName
      requestType = backendType <> "_create_array_relationship"

  for_ tableReferences
    $ \Reference
         { referenceLocalColumn,
           referenceTargetTable,
           referenceTargetColumn,
           referenceTargetQualifiers
         } -> do
        let targetSchema = case resolveReferenceSchema referenceTargetQualifiers of
              Just schema -> schema
              Nothing -> getSchemaName testEnvironment
            relationshipName = mkArrayRelationshipName tableName referenceTargetColumn referenceLocalColumn referenceTargetQualifiers
            targetTableField = mkTableField backendTypeMetadata targetSchema referenceTargetTable
        GraphqlEngine.postMetadata_
          testEnvironment
          [yaml|
        type: *requestType
        args:
          source: *source
          table: *targetTableField
          name: *relationshipName
          using:
            foreign_key_constraint_on:
              table: *tableField
              column: *referenceLocalColumn
      |]

  for_ tableManualRelationships
    $ \Reference
         { referenceLocalColumn,
           referenceTargetTable,
           referenceTargetColumn,
           referenceTargetQualifiers,
           referenceInsertionOrder
         } -> do
        let targetSchema = case resolveReferenceSchema referenceTargetQualifiers of
              Just schema -> schema
              Nothing -> getSchemaName testEnvironment
            relationshipName = mkArrayRelationshipName tableName referenceTargetColumn referenceLocalColumn referenceTargetQualifiers
            targetTableField = mkTableField backendTypeMetadata targetSchema referenceTargetTable
            insertion_order = mkInsertionOrder referenceInsertionOrder
            manualConfiguration :: J.Value
            manualConfiguration =
              J.object
                [ "remote_table"
                    .= tableField,
                  "column_mapping"
                    .= J.object [K.fromText referenceTargetColumn .= referenceLocalColumn],
                  "insertion_order" .= J.String insertion_order
                ]
            payload =
              [yaml|
                type: *requestType
                args:
                  source: *source
                  table: *targetTableField
                  name: *relationshipName
                  using:
                    manual_configuration: *manualConfiguration
                |]

        GraphqlEngine.postMetadata_ testEnvironment payload

  for_ tableNativeQueryRelationships
    $ \relationship ->
      GraphqlEngine.postMetadata_
        testEnvironment
        (createTableToNativeQueryRelationship testEnvironment localSchema tableName relationship)

-- | Unified untrack relationships
untrackRelationships :: (HasCallStack) => Table -> TestEnvironment -> IO ()
untrackRelationships Table {tableName, tableReferences, tableManualRelationships} testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      schema = getSchemaName testEnvironment
      source = BackendType.backendSourceName backendTypeMetadata
      backendType = BackendType.backendTypeString backendTypeMetadata
      tableField = mkTableField backendTypeMetadata schema tableName
      requestType = backendType <> "_drop_relationship"

  forFinally_ (tableManualRelationships <> tableReferences) $ \ref@Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn, referenceTargetQualifiers} -> do
    let arrayRelationshipName = mkArrayRelationshipName tableName referenceTargetColumn referenceLocalColumn referenceTargetQualifiers
        objectRelationshipName = mkObjectRelationshipName ref
        targetTableField = mkTableField backendTypeMetadata schema referenceTargetTable
    finally
      ( -- drop array relationship
        GraphqlEngine.postMetadata_
          testEnvironment
          [yaml|
    type: *requestType
    args:
      source: *source
      table: *targetTableField
      relationship: *arrayRelationshipName
    |]
      )
      ( -- drop object relationship
        GraphqlEngine.postMetadata_
          testEnvironment
          [yaml|
    type: *requestType
    args:
      source: *source
      table: *tableField
      relationship: *objectRelationshipName
    |]
      )

-- | Unified RunSQL
runSQL :: (HasCallStack) => String -> String -> TestEnvironment -> IO ()
runSQL source sql testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      prefix = BackendType.backendTypeString backendTypeMetadata
      requestType = prefix <> "_run_sql"
  GraphqlEngine.postV2Query_
    testEnvironment
    [yaml|
type: *requestType
args:
  source: *source
  sql: *sql
  cascade: false
  read_only: false
|]

addSource :: (HasCallStack) => Text -> Value -> TestEnvironment -> IO ()
addSource sourceName sourceConfig testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
  GraphqlEngine.postMetadata_
    testEnvironment
    [interpolateYaml|
      type: #{ backendType }_add_source
      args:
        name: #{ sourceName }
        configuration: #{ sourceConfig }
      |]

bulkAtomicCommand :: [Value] -> Value
bulkAtomicCommand subCommands =
  [yaml|
      type: bulk_atomic
      args: *subCommands
    |]

-- | metadata command to enable sending OTel traces
-- this will be ignored in OSS
-- `4318` is the "standard" port for an OTel receiver
-- a batch size of `3` is unusually low, but if we go higher
-- then we tend to miss traces when running single tests
-- we can re-address this if this is tanking test performance
enableOpenTelemetryCommand :: Value
enableOpenTelemetryCommand =
  [yaml|
      type: set_opentelemetry_config
      args:
        status: "enabled"
        data_types:
          - "traces"
        exporter_otlp:
          headers:
            - name: "header_value"
              value: "value"
          otlp_traces_endpoint: "http://localhost:4318/v1/traces"
          protocol: "http/protobuf"
          resource_attributes:
            - name: "attribute-name"
              value: "attribute-value"
        batch_span_processor:
          max_export_batch_size: 3
    |]
