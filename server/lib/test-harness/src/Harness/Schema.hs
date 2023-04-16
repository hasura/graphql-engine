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
    ManualRelationship (..),
    SchemaName (..),
    NativeQuery (..),
    NativeQueryColumn (..),
    trackNativeQueryCommand,
    untrackNativeQueryCommand,
    CustomType (..),
    trackCustomType,
    trackCustomTypeCommand,
    untrackCustomType,
    untrackCustomTypeCommand,
    resolveTableSchema,
    trackTable,
    untrackTable,
    mkTableField,
    trackObjectRelationships,
    trackArrayRelationships,
    untrackRelationships,
    mkObjectRelationshipName,
    mkArrayRelationshipName,
    trackFunction,
    untrackFunction,
    trackComputedField,
    untrackComputedField,
    runSQL,
    addSource,
    trackNativeQuery,
    untrackNativeQuery,
    module Harness.Schema.Table,
    module Harness.Schema.Name,
    getSchemaName,
  )
where

import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.Types qualified as Aeson
import Data.Vector qualified as V
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema.Name
import Harness.Schema.Table
import Harness.Test.BackendType (BackendTypeConfig)
import Harness.Test.BackendType qualified as BackendType
import Harness.TestEnvironment (TestEnvironment (..), getBackendTypeConfig, getSchemaName)
import Hasura.Prelude

-- | we assume we are using the default schema unless a table tells us
-- otherwise
-- when multiple qualifiers are passed, we assume the last one is the schema
resolveTableSchema :: TestEnvironment -> Table -> SchemaName
resolveTableSchema testEnv tbl =
  case resolveReferenceSchema (coerce $ tableQualifiers tbl) of
    Nothing -> getSchemaName testEnv
    Just schemaName -> schemaName

-- | Native Backend track table
--
-- Data Connector backends expect an @[String]@ for the table name.
trackTable :: HasCallStack => String -> Table -> TestEnvironment -> IO ()
trackTable source tbl@(Table {tableName}) testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      schema = resolveTableSchema testEnvironment tbl
      requestType = backendType <> "_track_table"
      column_config = columnsConfig (tableColumns tbl)
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: *requestType
      args:
        source: *source
        table:
          schema: *schema
          name: *tableName
        configuration:
          column_config: *column_config
    |]
  where
    columnsConfig :: [Column] -> Value
    columnsConfig = Aeson.object . mapMaybe columnConfig

    columnConfig :: Column -> Maybe Aeson.Pair
    columnConfig col = do
      alias <- columnGqlAlias col
      return $
        ( K.fromText $ columnName col,
          [yaml|
        custom_name: *alias
        |]
        )

-- | Native Backend track table
--
-- Data Connector backends expect an @[String]@ for the table name.
untrackTable :: HasCallStack => String -> Table -> TestEnvironment -> IO ()
untrackTable source Table {tableName} testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      schema = getSchemaName testEnvironment
      requestType = backendType <> "_untrack_table"
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: *requestType
args:
  source: *source
  table:
    schema: *schema
    name: *tableName
|]

trackFunction :: HasCallStack => String -> String -> TestEnvironment -> IO ()
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
untrackFunction :: HasCallStack => String -> String -> TestEnvironment -> IO ()
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
  HasCallStack =>
  String ->
  Table ->
  String ->
  String ->
  Aeson.Value ->
  Aeson.Value ->
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
untrackComputedField :: HasCallStack => String -> Table -> String -> TestEnvironment -> IO ()
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

-- | Build an 'Aeson.Value' representing a 'BackendType' specific @TableName@.
mkTableField :: BackendTypeConfig -> SchemaName -> Text -> Aeson.Value
mkTableField backendTypeMetadata schemaName tableName =
  let dcFieldName = Aeson.Array $ V.fromList [Aeson.String (unSchemaName schemaName), Aeson.String tableName]
      nativeFieldName = Aeson.object [BackendType.backendSchemaKeyword backendTypeMetadata .= Aeson.String (unSchemaName schemaName), "name" .= Aeson.String tableName]
   in case BackendType.backendType backendTypeMetadata of
        BackendType.Postgres -> nativeFieldName
        BackendType.SQLServer -> nativeFieldName
        BackendType.BigQuery -> nativeFieldName
        BackendType.Citus -> nativeFieldName
        BackendType.Cockroach -> nativeFieldName
        BackendType.DataConnector _ -> dcFieldName

-- | Unified track object relationships
trackObjectRelationships :: HasCallStack => Table -> TestEnvironment -> IO ()
trackObjectRelationships tbl@(Table {tableName, tableReferences, tableManualRelationships}) testEnvironment = do
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

  for_ tableManualRelationships $ \ref@Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn, referenceTargetQualifiers} -> do
    let targetSchema = case resolveReferenceSchema referenceTargetQualifiers of
          Just schema -> schema
          Nothing -> getSchemaName testEnvironment
        relationshipName = mkObjectRelationshipName ref
        targetTableField = mkTableField backendTypeMetadata targetSchema referenceTargetTable
        manualConfiguration :: Aeson.Value
        manualConfiguration =
          Aeson.object
            [ "remote_table" .= targetTableField,
              "column_mapping"
                .= Aeson.object [K.fromText referenceLocalColumn .= referenceTargetColumn]
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

-- | Helper to create the array relationship name
mkArrayRelationshipName :: Text -> Text -> Text -> [Text] -> Text
mkArrayRelationshipName tableName referenceLocalColumn referenceTargetColumn referenceTargetQualifiers =
  let columnName = case resolveReferenceSchema referenceTargetQualifiers of
        Just (SchemaName targetSchema) -> targetSchema <> "_" <> referenceTargetColumn
        Nothing -> referenceTargetColumn
   in tableName <> "s_by_" <> referenceLocalColumn <> "_to_" <> columnName

-- | Unified track array relationships
trackArrayRelationships :: HasCallStack => Table -> TestEnvironment -> IO ()
trackArrayRelationships tbl@(Table {tableName, tableReferences, tableManualRelationships}) testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      localSchema = resolveTableSchema testEnvironment tbl
      backendType = BackendType.backendTypeString backendTypeMetadata
      source = BackendType.backendSourceName backendTypeMetadata
      tableField = mkTableField backendTypeMetadata localSchema tableName
      requestType = backendType <> "_create_array_relationship"

  for_ tableReferences $ \Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn, referenceTargetQualifiers} -> do
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

  for_ tableManualRelationships $ \Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn, referenceTargetQualifiers} -> do
    let targetSchema = case resolveReferenceSchema referenceTargetQualifiers of
          Just schema -> schema
          Nothing -> getSchemaName testEnvironment
        relationshipName = mkArrayRelationshipName tableName referenceTargetColumn referenceLocalColumn referenceTargetQualifiers
        targetTableField = mkTableField backendTypeMetadata targetSchema referenceTargetTable
        manualConfiguration :: Aeson.Value
        manualConfiguration =
          Aeson.object
            [ "remote_table"
                .= tableField,
              "column_mapping"
                .= Aeson.object [K.fromText referenceTargetColumn .= referenceLocalColumn]
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

-- | Unified untrack relationships
untrackRelationships :: HasCallStack => Table -> TestEnvironment -> IO ()
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
runSQL :: HasCallStack => String -> String -> TestEnvironment -> IO ()
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

addSource :: HasCallStack => Text -> Value -> TestEnvironment -> IO ()
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

trackNativeQueryCommand :: String -> BackendTypeConfig -> NativeQuery -> Value
trackNativeQueryCommand sourceName backendTypeConfig (NativeQuery {nativeQueryName, nativeQueryArguments, nativeQueryQuery, nativeQueryReturnType}) =
  -- arguments are a map from name to type details
  let argsToJson =
        Aeson.object
          . fmap
            ( \NativeQueryColumn {..} ->
                let key = K.fromText nativeQueryColumnName
                    descriptionPair = case nativeQueryColumnDescription of
                      Just desc -> [(K.fromText "description", Aeson.String desc)]
                      Nothing -> []

                    value =
                      Aeson.object $
                        [ (K.fromText "type", Aeson.String ((BackendType.backendScalarType backendTypeConfig) nativeQueryColumnType)),
                          (K.fromText "nullable", Aeson.Bool nativeQueryColumnNullable)
                        ]
                          <> descriptionPair
                 in (key, value)
            )

      arguments = argsToJson nativeQueryArguments

      backendType = BackendType.backendTypeString backendTypeConfig

      requestType = backendType <> "_track_native_query"
   in [yaml|
        type: *requestType
        args:
          type: query
          source: *sourceName
          root_field_name: *nativeQueryName 
          code: *nativeQueryQuery
          arguments: *arguments
          returns: *nativeQueryReturnType
      |]

trackNativeQuery :: HasCallStack => String -> NativeQuery -> TestEnvironment -> IO ()
trackNativeQuery sourceName logMod testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = trackNativeQueryCommand sourceName backendTypeMetadata logMod

  GraphqlEngine.postMetadata_ testEnvironment command

untrackNativeQueryCommand :: String -> BackendTypeConfig -> NativeQuery -> Value
untrackNativeQueryCommand source backendTypeMetadata NativeQuery {nativeQueryName} =
  let backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_native_query"
   in [yaml|
      type: *requestType
      args:
        source: *source
        root_field_name: *nativeQueryName 
    |]

untrackNativeQuery :: HasCallStack => String -> NativeQuery -> TestEnvironment -> IO ()
untrackNativeQuery source logMod testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = untrackNativeQueryCommand source backendTypeMetadata logMod

  GraphqlEngine.postMetadata_
    testEnvironment
    command

trackCustomTypeCommand :: String -> BackendTypeConfig -> CustomType -> Value
trackCustomTypeCommand sourceName backendTypeConfig (CustomType {customTypeDescription, customTypeName, customTypeColumns}) =
  -- return type is an array of items
  let returnTypeToJson =
        Aeson.Array
          . V.fromList
          . fmap
            ( \NativeQueryColumn {..} ->
                let descriptionPair = case nativeQueryColumnDescription of
                      Just desc -> [(K.fromText "description", Aeson.String desc)]
                      Nothing -> []
                 in Aeson.object $
                      [ (K.fromText "name", Aeson.String nativeQueryColumnName),
                        (K.fromText "type", Aeson.String ((BackendType.backendScalarType backendTypeConfig) nativeQueryColumnType)),
                        (K.fromText "nullable", Aeson.Bool nativeQueryColumnNullable)
                      ]
                        <> descriptionPair
            )

      columns = returnTypeToJson customTypeColumns

      -- need to make this only appear if it's Just, for now fall back to empty
      -- string for lols
      description = fromMaybe "" customTypeDescription

      backendType = BackendType.backendTypeString backendTypeConfig

      requestType = backendType <> "_track_custom_return_type"
   in [yaml|
        type: *requestType
        args:
          source: *sourceName
          description: *description 
          name: *customTypeName
          fields: *columns
      |]

trackCustomType :: HasCallStack => String -> CustomType -> TestEnvironment -> IO ()
trackCustomType sourceName ctmType testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = trackCustomTypeCommand sourceName backendTypeMetadata ctmType

  GraphqlEngine.postMetadata_ testEnvironment command

untrackCustomTypeCommand :: String -> BackendTypeConfig -> CustomType -> Value
untrackCustomTypeCommand source backendTypeMetadata CustomType {customTypeName} =
  let backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_custom_return_type"
   in [yaml|
      type: *requestType
      args:
        source: *source
        name: *customTypeName
    |]

untrackCustomType :: HasCallStack => String -> CustomType -> TestEnvironment -> IO ()
untrackCustomType source ctmType testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = untrackCustomTypeCommand source backendTypeMetadata ctmType

  GraphqlEngine.postMetadata_
    testEnvironment
    command
