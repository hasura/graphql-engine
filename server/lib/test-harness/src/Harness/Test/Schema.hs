{-# LANGUAGE QuasiQuotes #-}

-- | Common interface for setup/teardown for all backends - schema and data
module Harness.Test.Schema
  ( Table (..),
    table,
    Reference (..),
    Column (..),
    ScalarType (..),
    defaultSerialType,
    ScalarValue (..),
    WKT (..),
    UniqueConstraint (..),
    BackendScalarType (..),
    BackendScalarValue (..),
    BackendScalarValueType (..),
    ManualRelationship (..),
    SchemaName (..),
    quotedValue,
    unquotedValue,
    backendScalarValue,
    column,
    columnNull,
    defaultBackendScalarType,
    getBackendScalarType,
    defaultBackendScalarValue,
    formatBackendScalarValueType,
    parseUTCTimeOrError,
    trackTable,
    untrackTable,
    mkTableField,
    trackObjectRelationships,
    trackArrayRelationships,
    untrackRelationships,
    mkObjectRelationshipName,
    mkArrayRelationshipName,
    getSchemaName,
    trackFunction,
    untrackFunction,
    trackComputedField,
    untrackComputedField,
    runSQL,
  )
where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as K
import Data.Time (UTCTime, defaultTimeLocale)
import Data.Time.Format (parseTimeOrError)
import Data.Vector qualified as V
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType
import Harness.Test.SchemaName
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude

-- | Generic type to use to specify schema tables for all backends.
-- Usually a list of these make up a "schema" to pass to the respective
-- @Harness.Backend.<Backend>.{setup,teardown}@ functions
--
-- NOTE: There is neither a type-level check to assert that the length of
-- tableColumns matches the length of each row in tableData, nor that the
-- tablePrimaryKey only contains names of columns already in tableColumns or
-- that tableReferences are valid references to other Tables. Test author will
-- need to be just a bit careful while constructing Tables.
data Table = Table
  { tableName :: Text,
    -- | Columns that are references (foreign keys) should be null-able
    tableColumns :: [Column],
    tablePrimaryKey :: [Text],
    tableReferences :: [Reference],
    tableManualRelationships :: [Reference],
    tableData :: [[ScalarValue]],
    tableUniqueConstraints :: [UniqueConstraint]
  }
  deriving (Show, Eq)

data UniqueConstraint = UniqueConstraintColumns [Text] | UniqueConstraintExpression Text
  deriving (Show, Eq)

-- | Create a table from just a name.
-- Use record updates to modify the result.
table :: Text -> Table
table tableName = Table tableName [] [] [] [] [] []

-- | Foreign keys for backends that support it.
data Reference = Reference
  { referenceLocalColumn :: Text,
    referenceTargetTable :: Text,
    referenceTargetColumn :: Text
  }
  deriving (Show, Eq)

-- | Type representing manual relationship between tables. This is
-- only used for BigQuery backend currently where additional
-- relationships has to be manually specified.
data ManualRelationship = ManualRelationship
  { relSourceTable :: Text,
    relTargetTable :: Text,
    relSourceColumn :: Text,
    relTargetColumn :: Text
  }
  deriving (Show, Eq)

-- | Generic type to construct columns for all backends
data Column = Column
  { columnName :: Text,
    columnType :: ScalarType,
    columnNullable :: Bool,
    columnDefault :: Maybe Text
  }
  deriving (Show, Eq)

-- | Generic type to represent ScalarType for multiple backends. This
-- type can be used to encapsulate the column types for different
-- backends by providing explicit name of the datatype. This provides
-- flexibility and scalability which is difficult to achieve by just
-- extending ScalarType.
--
-- To give a concrete usecase, right now we have 'ScalarType' with
-- value 'TUTCTime'. This is treated as TIMESTAMP for Citus and
-- DATETIME for MSSQL server. There might be usecases where you want
-- your table column to treat it as TIMESTAMP for Citus and
-- <https://docs.microsoft.com/en-us/sql/t-sql/data-types/datetime2-transact-sql?redirectedfrom=MSDN&view=sql-server-ver15
-- DATETIME2> for MSSQL server. BackendScalarType makes such use case
-- very simple to achive instead of making you define a new sum type
-- and handling it.
data BackendScalarType = BackendScalarType
  { bstMysql :: Maybe Text,
    bstCitus :: Maybe Text,
    bstCockroach :: Maybe Text,
    bstPostgres :: Maybe Text,
    bstBigQuery :: Maybe Text,
    bstMssql :: Maybe Text
  }
  deriving (Show, Eq)

-- | Default value for 'BackendScalarType' initialized with 'Nothing'
-- for all the fields.
defaultBackendScalarType :: BackendScalarType
defaultBackendScalarType =
  BackendScalarType
    { bstMysql = Nothing,
      bstCitus = Nothing,
      bstCockroach = Nothing,
      bstMssql = Nothing,
      bstPostgres = Nothing,
      bstBigQuery = Nothing
    }

-- | Access specific backend scalar type out of 'BackendScalarType'
getBackendScalarType :: BackendScalarType -> (BackendScalarType -> Maybe Text) -> Text
getBackendScalarType bst fn =
  case fn bst of
    Just scalarType -> scalarType
    Nothing -> error $ "getBackendScalarType: BackendScalarType is Nothing, passed " <> show bst

-- | This type represents how the serialization of a value should
-- happen for a particular item. 'Quoted' text indicates that the text
-- will be enclosed with double quotes whereas 'Unqouted' text will have
-- none.
--
-- Usually, texts (or strings) should be represented as quoted and
-- numbers might not require any quotes. Although, consult the
-- particular database backend for the exact behavior. This type has
-- been introduced to allow flexibility while construting values for
-- the columns.
data BackendScalarValueType = Quoted Text | Unquoted Text deriving (Show, Eq)

quotedValue :: Text -> Maybe BackendScalarValueType
quotedValue = Just . Quoted

unquotedValue :: Text -> Maybe BackendScalarValueType
unquotedValue = Just . Unquoted

formatBackendScalarValueType :: BackendScalarValueType -> Text
formatBackendScalarValueType (Quoted text) = "'" <> text <> "'"
formatBackendScalarValueType (Unquoted text) = text

-- | Generic type to represent ScalarValue for multiple backends. This
-- type can be used to encapsulate the column values for different
-- backends by providing explicit data for individual backend. This provides
-- flexibility and scalability which is difficult to achieve by just
-- extending ScalarValue.
--
-- To give a concrete usecase, right now we have timestamp column for
-- out database. Depending on the database, the value can be
-- different. For postgres backend, we use 2017-09-21T09:39:44 to
-- represent timestamp. But we would want to use 2017-09-21T09:39:44Z
-- for Microsoft's SQL server backend. This type provides flexibility
-- to provide such options.
data BackendScalarValue = BackendScalarValue
  { bsvMysql :: Maybe BackendScalarValueType,
    bsvCitus :: Maybe BackendScalarValueType,
    bsvCockroach :: Maybe BackendScalarValueType,
    bsvPostgres :: Maybe BackendScalarValueType,
    bsvBigQuery :: Maybe BackendScalarValueType,
    bsvMssql :: Maybe BackendScalarValueType
  }
  deriving (Show, Eq)

-- | Default value for 'BackendScalarValue' initialized with 'Nothing'
-- for all the fields.
defaultBackendScalarValue :: BackendScalarValue
defaultBackendScalarValue =
  BackendScalarValue
    { bsvMysql = Nothing,
      bsvCitus = Nothing,
      bsvCockroach = Nothing,
      bsvPostgres = Nothing,
      bsvBigQuery = Nothing,
      bsvMssql = Nothing
    }

-- | Generic scalar type for all backends, for simplicity.
-- Ideally, we would be wiring in @'Backend@ specific scalar types here to make
-- sure all backend-specific scalar types are also covered by tests, perhaps in
-- a future iteration.
data ScalarType
  = TInt
  | TStr
  | TUTCTime
  | TBool
  | TGeography
  | TCustomType BackendScalarType
  deriving (Show, Eq)

-- | Generic scalar value type for all backends, that should directly correspond
-- to 'ScalarType'
data ScalarValue
  = VInt Int
  | VStr Text
  | VUTCTime UTCTime
  | VBool Bool
  | VGeography WKT
  | VNull
  | VCustomValue BackendScalarValue
  deriving (Show, Eq)

-- | Describe Geography values using the WKT representation
-- https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry
-- https://cloud.google.com/bigquery/docs/geospatial-data#loading_wkt_or_wkb_data
newtype WKT = WKT Text
  deriving (Eq, Show, IsString)

backendScalarValue :: BackendScalarValue -> (BackendScalarValue -> Maybe BackendScalarValueType) -> BackendScalarValueType
backendScalarValue bsv fn = case fn bsv of
  Nothing -> error $ "backendScalarValue: Retrieved value is Nothing, passed " <> show bsv
  Just scalarValue -> scalarValue

defaultSerialType :: ScalarType
defaultSerialType =
  TCustomType $
    defaultBackendScalarType
      { bstMysql = Nothing,
        bstMssql = Just "INT IDENTITY(1,1)",
        bstCitus = Just "SERIAL",
        bstCockroach = Just "SERIAL",
        bstPostgres = Just "SERIAL",
        bstBigQuery = Nothing
      }

-- | Helper function to construct 'Column's with common defaults
column :: Text -> ScalarType -> Column
column name typ = Column name typ False Nothing

-- | Helper function to construct 'Column's that are null-able
columnNull :: Text -> ScalarType -> Column
columnNull name typ = Column name typ True Nothing

-- | Helper to construct UTCTime using @%F %T@ format. For e.g. @YYYY-MM-DD HH:MM:SS@
parseUTCTimeOrError :: String -> ScalarValue
parseUTCTimeOrError = VUTCTime . parseTimeOrError True defaultTimeLocale "%F %T"

-- | Unified track table
trackTable :: HasCallStack => BackendType -> String -> Table -> TestEnvironment -> IO ()
trackTable backend source Table {tableName} testEnvironment = do
  let backendType = defaultBackendTypeString backend
      schema = getSchemaName testEnvironment
      requestType = backendType <> "_track_table"
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

-- | Unified untrack table
untrackTable :: HasCallStack => BackendType -> String -> Table -> TestEnvironment -> IO ()
untrackTable backend source Table {tableName} testEnvironment = do
  let backendType = defaultBackendTypeString backend
      schema = getSchemaName testEnvironment
  let requestType = backendType <> "_untrack_table"
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

trackFunction :: HasCallStack => BackendType -> String -> String -> TestEnvironment -> IO ()
trackFunction backend source functionName testEnvironment = do
  let backendType = defaultBackendTypeString backend
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
untrackFunction :: HasCallStack => BackendType -> String -> String -> TestEnvironment -> IO ()
untrackFunction backend source functionName testEnvironment = do
  let backendType = defaultBackendTypeString backend
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
  BackendType ->
  String ->
  Table ->
  String ->
  String ->
  TestEnvironment ->
  IO ()
trackComputedField backend source Table {tableName} functionName asFieldName testEnvironment = do
  let backendType = defaultBackendTypeString backend
      schema = getSchemaName testEnvironment
      requestType = backendType <> "_add_computed_field"
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: *requestType
args:
  source: *source
  comment: null
  table:
    schema: *schema
    name: *tableName
  name: *asFieldName
  definition:
    function:
      schema: *schema
      name: *functionName
    table_argument: null
    session_argument: null
|]

-- | Unified untrack computed field
untrackComputedField :: HasCallStack => BackendType -> String -> Table -> String -> TestEnvironment -> IO ()
untrackComputedField backend source Table {tableName} fieldName testEnvironment = do
  let backendType = defaultBackendTypeString backend
      schema = getSchemaName testEnvironment
  let requestType = backendType <> "_drop_computed_field"
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: *requestType
args:
  source: *source
  table:
    schema: *schema
    name: *tableName
  name: *fieldName
|]

-- | Helper to create the object relationship name
mkObjectRelationshipName :: Reference -> Text
mkObjectRelationshipName Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} =
  referenceTargetTable <> "_by_" <> referenceLocalColumn <> "_to_" <> referenceTargetColumn

-- | Build an 'Aeson.Value' representing a 'BackendType' specific @TableName@.
mkTableField :: BackendType -> SchemaName -> Text -> Aeson.Value
mkTableField backend schemaName tableName =
  let dcFieldName = Aeson.Array $ V.fromList [Aeson.String (unSchemaName schemaName), Aeson.String tableName]
      nativeFieldName = Aeson.object [schemaKeyword backend .= Aeson.String (unSchemaName schemaName), "name" .= Aeson.String tableName]
   in case backend of
        Postgres -> nativeFieldName
        MySQL -> nativeFieldName
        SQLServer -> nativeFieldName
        BigQuery -> nativeFieldName
        Citus -> nativeFieldName
        Cockroach -> nativeFieldName
        DataConnectorMock -> dcFieldName
        DataConnectorReference -> dcFieldName
        DataConnectorSqlite -> dcFieldName

-- | Unified track object relationships
trackObjectRelationships :: HasCallStack => BackendType -> Table -> TestEnvironment -> IO ()
trackObjectRelationships backend Table {tableName, tableReferences, tableManualRelationships} testEnvironment = do
  let schema = getSchemaName testEnvironment
      backendType = defaultBackendTypeString backend
      source = defaultSource backend
      tableField = mkTableField backend schema tableName
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

  for_ tableManualRelationships $ \ref@Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} -> do
    let relationshipName = mkObjectRelationshipName ref
        targetTableField = mkTableField backend schema referenceTargetTable
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
mkArrayRelationshipName :: Text -> Text -> Text -> Text
mkArrayRelationshipName tableName referenceLocalColumn referenceTargetColumn =
  tableName <> "s_by_" <> referenceLocalColumn <> "_to_" <> referenceTargetColumn

-- | Unified track array relationships
trackArrayRelationships :: HasCallStack => BackendType -> Table -> TestEnvironment -> IO ()
trackArrayRelationships backend Table {tableName, tableReferences, tableManualRelationships} testEnvironment = do
  let schema = getSchemaName testEnvironment
      backendType = defaultBackendTypeString backend
      source = defaultSource backend
      tableField = mkTableField backend schema tableName
      requestType = backendType <> "_create_array_relationship"

  for_ tableReferences $ \Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} -> do
    let relationshipName = mkArrayRelationshipName tableName referenceTargetColumn referenceLocalColumn
        targetTableField = mkTableField backend schema referenceTargetTable
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

  for_ tableManualRelationships $ \Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} -> do
    let relationshipName = mkArrayRelationshipName tableName referenceTargetColumn referenceLocalColumn
        targetTableField = mkTableField backend schema referenceTargetTable
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
untrackRelationships :: HasCallStack => BackendType -> Table -> TestEnvironment -> IO ()
untrackRelationships backend Table {tableName, tableReferences, tableManualRelationships} testEnvironment = do
  let schema = getSchemaName testEnvironment
      source = defaultSource backend
      tableField = mkTableField backend schema tableName
      requestType = source <> "_drop_relationship"

  forFinally_ (tableManualRelationships <> tableReferences) $ \ref@Reference {referenceLocalColumn, referenceTargetTable, referenceTargetColumn} -> do
    let arrayRelationshipName = mkArrayRelationshipName tableName referenceTargetColumn referenceLocalColumn
        objectRelationshipName = mkObjectRelationshipName ref
        targetTableField = mkTableField backend schema referenceTargetTable
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
runSQL :: HasCallStack => BackendType -> String -> String -> TestEnvironment -> IO ()
runSQL backend source sql testEnvironment = do
  let prefix = case backend of
        Postgres -> ""
        _ -> defaultBackendTypeString backend <> "_"
      requestType = prefix <> "run_sql"
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
