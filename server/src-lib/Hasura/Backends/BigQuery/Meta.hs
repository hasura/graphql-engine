{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.BigQuery.Meta
  ( MetadataError (..),
    getTables,
    RestTableReference (..),
    RestTable (..),
    RestTableSchema (..),
    RestFieldSchema (..),
    RestType (..),
    Mode (..),
    RestRoutineType (..),
    RestArgument (..),
    RestStandardSqlField (..),
    RestStandardSqlTableType (..),
    RestRoutineReference (..),
    routineReferenceToFunctionName,
    RestRoutine (..),
    getRoutines,
  )
where

import Control.Concurrent.Extended (forLimitedConcurrentlyEIO)
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson qualified as J
import Data.Foldable
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List qualified as List
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.SerializableBlob (fromText)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Vector qualified as V
import GHC.Generics
import Hasura.Backends.BigQuery.Connection
import Hasura.Backends.BigQuery.Execute qualified as Execute
import Hasura.Backends.BigQuery.Source
import Hasura.Backends.BigQuery.Types
import Hasura.Logging (Hasura, LogLevel (..), Logger (..), UnstructuredLog (..))
import Hasura.Prelude
import Network.HTTP.Simple
import Network.HTTP.Types
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------
-- Types

data MetadataError
  = RestProblem RestProblem
  deriving (Show)

data RestProblem
  = GetTablesProblem SomeException
  | GetTableProblem SomeException
  | GetRoutineProblem SomeException
  | GetMetaDecodeProblem String
  | GetTablesBigQueryProblem BigQueryProblem
  | GetRoutinesBigQueryProblem BigQueryProblem
  | RESTRequestNonOK Status
  | InformationSchemaQueryProblem String
  deriving (Show)

-- | https://developers.google.com/workspace/drive/api/guides/fields-parameter
getTableReferencesFieldMask :: Text
getTableReferencesFieldMask = "kind,nextPageToken,tables(tableReference)"

-- | NOTE: if you add new fields in this nested structure or modify the
-- 'FromJSON' instance you must also change 'getTableReferencesFieldMask'
data RestTableList = RestTableList
  { nextPageToken :: Maybe Text,
    tables :: [RestTableBrief]
  }
  deriving (Show)

instance FromJSON RestTableList where
  parseJSON =
    withObject
      "RestTableList"
      ( \o -> do
          kind <- o .: "kind"
          case kind of
            ("bigquery#tableList" :: Text) -> do
              nextPageToken <- o .:? "nextPageToken"
              tables <- o .:? "tables" .!= []
              pure RestTableList {..}
            _ -> fail "Expected kind of bigquery#tableList"
      )

data RestTableBrief = RestTableBrief
  { tableReference :: RestTableReference
  }
  deriving (Show, Generic)

instance FromJSON RestTableBrief

data RestTableReference = RestTableReference
  { datasetId :: Text,
    projectId :: Text,
    tableId :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON RestTableReference

-- | https://developers.google.com/workspace/drive/api/guides/fields-parameter
getTableFieldMask :: Text
getTableFieldMask = "tableReference,schema(fields(name,type,mode))"

-- | NOTE: if you add new fields in this nested structure you must also change
-- 'getTableFieldMask'
data RestTable = RestTable
  { tableReference :: RestTableReference,
    schema :: RestTableSchema
  }
  deriving (Show, Eq, Generic)

instance FromJSON RestTable

data RestTableSchema = RestTableSchema
  { fields :: [RestFieldSchema]
  }
  deriving (Show, Eq, Generic)

instance FromJSON RestTableSchema

data RestFieldSchema = RestFieldSchema
  { name :: Text,
    -- | The field data type. Possible values include STRING, BYTES,
    -- INTEGER, INT64 (same as INTEGER), FLOAT, FLOAT64 (same as
    -- FLOAT), BOOLEAN, BOOL (same as BOOLEAN), TIMESTAMP, DATE, TIME,
    -- DATETIME, GEOGRAPHY, NUMERIC, RECORD (where RECORD indicates
    -- that the field contains a nested schema) or STRUCT (same as
    -- RECORD).
    type' :: RestType,
    mode :: Mode
    -- The field mode. Possible values include NULLABLE, REQUIRED and
    -- REPEATED. The default value is NULLABLE.
  }
  deriving (Show, Eq, Generic)

instance FromJSON RestFieldSchema where
  parseJSON =
    withObject
      "RestFieldSchema"
      ( \o -> do
          type' <- o .: "type"
          name <- o .: "name"
          mode <- o .:? "mode" .!= Nullable
          pure RestFieldSchema {..}
      )

data Mode = Nullable | Required | Repeated deriving (Show, Eq, Ord)

instance FromJSON Mode where
  parseJSON j = do
    s <- parseJSON j
    case s :: Text of
      "NULLABLE" -> pure Nullable
      "REQUIRED" -> pure Required
      "REPEATED" -> pure Repeated
      _ -> fail ("invalid mode " ++ show s)

data RestType
  = STRING
  | BYTES
  | INTEGER
  | FLOAT
  | BOOL
  | TIMESTAMP
  | DATE
  | TIME
  | DATETIME
  | GEOGRAPHY
  | DECIMAL
  | BIGDECIMAL
  | JSON
  | STRUCT -- (same as RECORD).
  deriving (Eq, Show)

instance FromJSON RestType where
  parseJSON j = do
    s <- parseJSON j
    case s :: Text of
      "STRING" -> pure STRING
      "BYTES" -> pure BYTES
      "INTEGER" -> pure INTEGER
      "INT64" -> pure INTEGER
      "FLOAT" -> pure FLOAT
      "FLOAT64" -> pure FLOAT
      "BOOLEAN" -> pure BOOL
      "BOOL" -> pure BOOL
      "TIMESTAMP" -> pure TIMESTAMP
      "DATE" -> pure DATE
      "TIME" -> pure TIME
      "DATETIME" -> pure DATETIME
      "GEOGRAPHY" -> pure GEOGRAPHY
      "NUMERIC" -> pure DECIMAL
      "DECIMAL" -> pure DECIMAL
      "BIGNUMERIC" -> pure BIGDECIMAL
      "BIGDECIMAL" -> pure BIGDECIMAL
      "JSON" -> pure JSON
      "RECORD" -> pure STRUCT
      "STRUCT" -> pure STRUCT
      -- TODO this should maybe read "unsupported type"? e.g. `RANGE` is not handled here
      _ -> fail ("invalid type " ++ show s)

instance ToJSON RestType where
  toJSON =
    String . \case
      STRING -> "STRING"
      BYTES -> "BYTES"
      INTEGER -> "INTEGER"
      FLOAT -> "FLOAT"
      BOOL -> "BOOLEAN"
      TIMESTAMP -> "TIMESTAMP"
      DATE -> "DATE"
      TIME -> "TIME"
      DATETIME -> "DATETIME"
      GEOGRAPHY -> "GEOGRAPHY"
      DECIMAL -> "DECIMAL"
      BIGDECIMAL -> "BIGDECIMAL"
      JSON -> "JSON"
      STRUCT -> "STRUCT"

--------------------------------------------------------------------------------
-- REST request

-- | Get all tables from all specified data sets.
--
-- Supports three modes controlled by environment variables:
-- 1. Default: Try fast INFORMATION_SCHEMA path, fallback to slow REST API on error
-- 2. Debug mode (HASURA_DEBUGGING_ASSERT_CORRECT_BIGQUERY_INTROSPECTION=true):
--    Run both paths and error if results don't match
-- 3. Force slow mode (HASURA_BIGQUERY_SLOW_INTROSPECTION_FALLBACK=true):
--    Skip fast path entirely, only use REST API
--
-- (3) is for safety, in case for some customers INFORMATION_SCHEMA is
-- returning wrong results and we need to revert. To be safe and conservative,
-- (1) includes a fallback to the old slow method on error, since BigQuery is
-- complicated and test coverage is poor.
getTables ::
  (MonadIO m) =>
  Logger Hasura ->
  BigQuerySourceConfig ->
  m (Either RestProblem [RestTable])
getTables logger config = liftIO $ do
  -- For now these are kept undocumented
  debugMode <- isJust <$> lookupEnv "HASURA_DEBUGGING_ASSERT_CORRECT_BIGQUERY_INTROSPECTION"
  forceSlowMode <- isJust <$> lookupEnv "HASURA_BIGQUERY_SLOW_INTROSPECTION_FALLBACK"
  runExceptT
    $ if
      | forceSlowMode -> getTablesSlow config
      | debugMode -> getTablesDebugMode config
      | otherwise -> getTablesWithFallback logger config

-- | Default mode: Try fast path, log error and fallback to slow path on failure
getTablesWithFallback :: (MonadIO m) => Logger Hasura -> BigQuerySourceConfig -> ExceptT RestProblem m [RestTable]
getTablesWithFallback logger config = do
  -- Try the fast INFORMATION_SCHEMA path
  getTablesFast config
    `catchError` \err -> do
      liftIO
        $ unLogger logger
        $ UnstructuredLog LevelInfo
        $ fromText
        $ "BigQuery INFORMATION_SCHEMA introspection failed, falling back to REST API: "
        <> tshow err
      getTablesSlow config

-- | Debug mode: Run both paths and error if results don't match. This is
-- intended to be an assertion turned on during testing, but might also be
-- useful for live debugging customers.
getTablesDebugMode :: (MonadIO m) => BigQuerySourceConfig -> ExceptT RestProblem m [RestTable]
getTablesDebugMode config = do
  oldTables <- getTablesSlow config
  newTables <- getTablesFast config
  -- make sure two successful introspections match
  unless (normalized oldTables == normalized newTables)
    $ throwError
    $ InformationSchemaQueryProblem
    $ "BigQuery introspection mismatch! \nOld: "
    <> show oldTables
    <> "\nNew: "
    <> show newTables
  pure oldTables
  where
    normalized = map normalizeRestTable . List.sortOn (\RestTable {tableReference} -> tableReference)
    -- or maybe 'fields' should be 'Set':
    normalizeRestTable :: RestTable -> RestTable
    normalizeRestTable t@RestTable {schema = RestTableSchema {fields}} =
      t {schema = RestTableSchema $ List.sortOn name fields}

getTablesFast :: (MonadIO m) => BigQuerySourceConfig -> ExceptT RestProblem m [RestTable]
getTablesFast BigQuerySourceConfig {..} =
  concat
    <$>
    -- concurrency limit here is arbitrary
    forLimitedConcurrentlyEIO
      10
      _scDatasets
      (getTablesForDataSetViaInformationSchema _scConnection)

-- | Slow path only: Use REST API for all tables
getTablesSlow :: (MonadIO m) => BigQuerySourceConfig -> ExceptT RestProblem m [RestTable]
getTablesSlow BigQuerySourceConfig {..} =
  concat <$> traverse (getTablesForDataSetViaREST _scConnection) _scDatasets

-- | Get schema information for all the tables in the dataset, via REST API.
--
-- NOTE: this (formerly the only option we had) is very inefficient as the API
-- forces us to make a separate HTTP request for every table.
getTablesForDataSetViaREST ::
  (MonadIO m) =>
  BigQueryConnection ->
  BigQueryDataset ->
  ExceptT RestProblem m [RestTable]
getTablesForDataSetViaREST conn dataSet = ExceptT do
  -- FYI: refactoring all of the code in this function to use ExceptT is
  -- complicated by catchAny
  result <-
    liftIO (catchAny (getTableReferences Nothing mempty) (pure . Left . GetTablesProblem))
  case result of
    Left e -> pure (Left e)
    Right briefs ->
      runExceptT
        $ forLimitedConcurrentlyEIO safeTableFetchConcurrency briefs
        $ \RestTableBrief {tableReference = RestTableReference {tableId}} ->
          ExceptT $ getTable conn dataSet tableId
  where
    -- page through all table references so we can look up table metadata (what
    -- we need is not available on the list API)
    getTableReferences pageToken acc = do
      let req =
            setRequestHeader "Content-Type" ["application/json"]
              $ parseRequest_ url
      eResp <- runBigQuery conn req
      case eResp of
        Left e -> pure (Left (GetTablesBigQueryProblem e))
        Right resp ->
          case getResponseStatusCode resp of
            200 ->
              case J.eitherDecode (getResponseBody resp) of
                Left e -> pure (Left (GetMetaDecodeProblem e))
                Right RestTableList {nextPageToken, tables} ->
                  case nextPageToken of
                    Nothing -> pure (Right (toList (acc <> Seq.fromList tables)))
                    Just token -> getTableReferences (pure token) (acc <> Seq.fromList tables)
            _ -> pure (Left (RESTRequestNonOK (getResponseStatus resp)))
      where
        url =
          T.unpack
            $ "GET https://bigquery.googleapis.com/bigquery/v2/projects/"
            <> getBigQueryProjectId (_bqProjectId conn)
            <> "/datasets/"
            <> getBigQueryDataset dataSet
            <> "/tables?alt=json&"
            <> encodeParams extraParameters
        extraParameters = pageTokenParam
          where
            pageTokenParam =
              case pageToken of
                Nothing -> [("fields", getTableReferencesFieldMask)]
                Just token -> [("fields", getTableReferencesFieldMask), ("pageToken", token)]

-- | We're limited to 100RPS to REST APIs before being throttled. I observed
-- ~8.5RPS max for 'getTable' for US region when in both CA and VA so spread
-- across 10 threads which seems to keep us well below the limit in practice
-- (there's not a good rate-limiter on hand).
safeTableFetchConcurrency :: Int
safeTableFetchConcurrency = 10

-- | Get a table in the dataset.
getTable ::
  (MonadIO m) =>
  BigQueryConnection ->
  BigQueryDataset ->
  Text ->
  m (Either RestProblem RestTable)
getTable conn dataSet tableId = do
  liftIO (catchAny run (pure . Left . GetTableProblem))
  where
    run = do
      let req =
            setRequestHeader "Content-Type" ["application/json"]
              $ parseRequest_ url
      eResp <- runBigQuery conn req
      case eResp of
        Left e -> pure (Left (GetTablesBigQueryProblem e))
        Right resp ->
          case getResponseStatusCode resp of
            200 ->
              case J.eitherDecode (getResponseBody resp) of
                Left e -> pure (Left (GetMetaDecodeProblem e))
                Right table -> pure (Right table)
            _ -> pure (Left (RESTRequestNonOK (getResponseStatus resp)))
      where
        url =
          T.unpack
            $ "GET https://bigquery.googleapis.com/bigquery/v2/projects/"
            <> getBigQueryProjectId (_bqProjectId conn)
            <> "/datasets/"
            <> getBigQueryDataset dataSet
            <> "/tables/"
            <> tableId
            <> "?alt=json&"
            <> encodeParams extraParameters
        extraParameters = [("fields", getTableFieldMask)]

encodeParams :: [(Text, Text)] -> Text
encodeParams = T.intercalate "&" . map (\(k, v) -> k <> "=" <> v)

-- | Get schema information for all tables in the dataset using INFORMATION_SCHEMA.
-- This is much more efficient than making individual REST API calls per table.
getTablesForDataSetViaInformationSchema ::
  (MonadIO m) =>
  BigQueryConnection ->
  BigQueryDataset ->
  ExceptT RestProblem m [RestTable]
getTablesForDataSetViaInformationSchema conn dataSet = do
  -- Build and execute the INFORMATION_SCHEMA query
  let projectId_dirty = getBigQueryProjectId (_bqProjectId conn)
      dataset_dirty = getBigQueryDataset dataSet
  -- NOTE: BigQuery doesn't support parameterizing table/dataset identifiers,
  -- only values. We cannot implement proper validation of e.g. BigQueryDataset
  -- because we already store untrusted data from customers. We don't have a
  -- threat model in mind here (this input currently comes from admins AFAICT),
  -- but keep this tight anyway
  unless (sanitary $ projectId_dirty <> dataset_dirty)
    $ throwError
    $ InformationSchemaQueryProblem
    $ "invalid dataset id or project id: "
    <> show dataset_dirty
    <> ", "
    <> show projectId_dirty
  let query = buildInformationSchemaQuery projectId_dirty dataset_dirty
      bigQuery = Execute.BigQuery {query, parameters = mempty}

  (_job, recordSet) <-
    withExceptT toRestProblem
      $ ExceptT
      $ Execute.streamBigQuery conn bigQuery
  liftEither $ parseRestTablesFromRecordSet recordSet
  where
    toRestProblem = InformationSchemaQueryProblem . T.unpack . Execute.executeProblemMessage Execute.HideDetails
    -- loose validation of dataset/project ids, only suitable for preventing
    -- SQL injection issues.
    sanitary = T.all (`Set.member` looseAllowedChars)
    looseAllowedChars = Set.fromList $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_-"

-- | Build the INFORMATION_SCHEMA query that returns JSON matching 'RestTable'
-- structure, so we can rely on the same parsing machinery and model types for
-- both the REST API and this new (much faster) method.
--
-- FYI: BigQuery has a 100MB row size, 10K max columns per table; here a column
-- is <100B generally, so we shouldn't be affected by this limit.
--
-- NOTE: inputs are interpolated, caller must sanitize!
buildInformationSchemaQuery :: Text -> Text -> LT.Text
buildInformationSchemaQuery projectId dataset =
  [i|
    WITH results AS (
        SELECT
          table_schema as datasetId,
          table_catalog as projectId,
          table_name as tableId,
          column_name as name,
          -- REST API-ish type (STRING/INT64/RECORD/...) and mode (nullable/required/repeated) where for some
          -- unholy reason top-level ARRAY<FOO> are represent as type FOO REPEATED...
          UPPER(
            CASE
              WHEN STARTS_WITH(data_type, 'ARRAY<STRUCT<') THEN 'RECORD'
              WHEN STARTS_WITH(data_type, 'STRUCT<')       THEN 'RECORD'
              WHEN STARTS_WITH(data_type, 'ARRAY<')        THEN REGEXP_EXTRACT(data_type, r'^ARRAY<(.+)>$$')
              ELSE REGEXP_REPLACE(data_type, r'\\(.*\\)$$', '')
            END
          ) AS type,
          CASE
            WHEN STARTS_WITH(data_type, 'ARRAY<') THEN 'REPEATED'
            WHEN is_nullable = 'NO'              THEN 'REQUIRED'
            ELSE 'NULLABLE'
          END AS mode
        FROM `#{projectId}.#{dataset}.INFORMATION_SCHEMA.COLUMNS`
        -- strip out pseudo-columns:
        where column_name != '_PARTITIONTIME'
          AND column_name != '_PARTITIONDATE'
        ORDER BY table_schema, table_name, ordinal_position
    )
    SELECT
      -- finally, reconstruct the same JSON shape we get from the REST API
      JSON_OBJECT(
       'tableReference',
          JSON_OBJECT('datasetId', datasetId, 'projectId', projectId, 'tableId', tableId),
       'schema',
          JSON_OBJECT('fields',
            TO_JSON(ARRAY_AGG(STRUCT(name, type, mode) ORDER BY name))
          )
      )
    FROM results
    GROUP BY tableId, datasetId, projectId
    ORDER BY tableId  -- so we don't have to sort this level for equality comparisons
  |]

-- | Parse RecordSet containing JSON into @[RestTable]@
parseRestTablesFromRecordSet :: Execute.RecordSet -> Either RestProblem [RestTable]
parseRestTablesFromRecordSet Execute.RecordSet {rows} = do
  traverse parseRow (V.toList rows)
  where
    parseRow row = do
      -- The query returns a single JSON_OBJECT column, extract it
      jsonValue <- case listToMaybe (InsOrdHashMap.elems row) of
        Just (Execute.JsonOutputValue val) -> Right val
        Just other -> Left (GetMetaDecodeProblem $ "Expected JSON value but got: " <> show other)
        Nothing -> Left (GetMetaDecodeProblem "Empty row in result set")

      -- Parse the JSON into RestTable using existing FromJSON instance
      case J.fromJSON jsonValue of
        J.Success table -> Right table
        J.Error err -> Left (GetMetaDecodeProblem $ "Failed to parse RestTable from JSON: " <> err)

-- Routines related

-- | The fine-grained type of the routine
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines#RoutineType
data RestRoutineType
  = ROUTINE_TYPE_UNSPECIFIED
  | SCALAR_FUNCTION
  | PROCEDURE
  | TABLE_VALUED_FUNCTION
  | AGGREGATE_FUNCTION
  deriving (Show, Eq, Generic)

instance FromJSON RestRoutineType

instance ToJSON RestRoutineType

-- | Input argument of a function/routine.
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines#Argument
data RestArgument = RestArgument
  { -- | The name of this argument. Can be absent for function return argument.
    _raName :: Maybe Text,
    _raDataType :: Maybe RestType
  }
  deriving (Eq, Show, Generic)

instance FromJSON RestArgument where
  parseJSON =
    withObject
      "RestArgument"
      ( \o -> do
          name <- o .:? "name"
          typeObject <- o .:? "dataType"

          -- (Hopefully) very temporary fix: right now, we don't have an
          -- understanding of @ARRAY@ as a 'RestType', which is causing issues
          -- in production. With this change, we ignore any BigQuery argument
          -- types that we don't recognise. While not ideal, this should be
          -- safe, as we shouldn't get types from BigQuery that it can't itself
          -- understand.
          type' <- mapM (.: "typeKind") typeObject <|> pure Nothing
          pure $ RestArgument name type'
      )

instance ToJSON RestArgument where
  toJSON (RestArgument name ty) =
    object
      [ "name" .= name,
        "dataType" .= object ["typeKind" .= ty]
      ]

-- | A field or a column.
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/StandardSqlField
data RestStandardSqlField = RestStandardSqlField
  { -- | The field name is optional and is absent for fields with STRUCT type.
    _rssfName :: Maybe Text,
    _rssType :: Maybe RestType
  }
  deriving (Eq, Show, Generic)

instance FromJSON RestStandardSqlField where
  parseJSON =
    withObject
      "RestStandardSqlField"
      ( \o -> do
          name <- o .:? "name"
          typeObject <- o .:? "type"

          -- (Hopefully) very temporary fix: right now, we don't have an
          -- understanding of @ARRAY@ as a 'RestType', which is causing issues
          -- in production. With this change, we ignore any BigQuery argument
          -- types that we don't recognise. While not ideal, this should be
          -- safe, as we shouldn't get types from BigQuery that it can't itself
          -- understand.
          type' <- mapM (.: "typeKind") typeObject <|> pure Nothing
          pure $ RestStandardSqlField name type'
      )

instance ToJSON RestStandardSqlField where
  toJSON (RestStandardSqlField name ty) =
    object ["name" .= name, "type" .= (object ["typeKind" .= ty])]

-- | A table type, which has only list of columns with names and types.
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines#StandardSqlTableType
data RestStandardSqlTableType = RestStandardSqlTableType
  { _rrttColumns :: [RestStandardSqlField]
  }
  deriving (Eq, Show, Generic)

instance FromJSON RestStandardSqlTableType where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON RestStandardSqlTableType where
  toJSON = genericToJSON hasuraJSON

-- | Id path of a routine.
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines#RoutineReference
data RestRoutineReference = RestRoutineReference
  { datasetId :: Text,
    projectId :: Text,
    routineId :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON RestRoutineReference

instance ToJSON RestRoutineReference

routineReferenceToFunctionName :: RestRoutineReference -> FunctionName
routineReferenceToFunctionName RestRoutineReference {..} =
  FunctionName {functionName = routineId, functionNameSchema = Just datasetId}

-- | A user-defined function.
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines#Routine
data RestRoutine = RestRoutine
  { -- | Reference describing the ID of this routine
    routineReference :: RestRoutineReference,
    -- | The type of routine
    routineType :: RestRoutineType,
    -- | List of arguments defined
    arguments :: Maybe [RestArgument],
    -- | Routines defined with 'RETURNS TABLE' clause has this information
    returnTableType :: Maybe RestStandardSqlTableType
  }
  deriving (Eq, Show, Generic)

instance FromJSON RestRoutine

instance ToJSON RestRoutine

-- | List of routines
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines/list
data RestRoutineList = RestRoutineList
  { _rrlRoutines :: [RestRoutine],
    _rrlNextPageToken :: Maybe Text
  }
  deriving (Show)

instance FromJSON RestRoutineList where
  parseJSON = withObject "Object" $ \o ->
    RestRoutineList
      <$> o
      .:? "routines"
      .!= [] -- "routine" field is absent when there are no routines defined
      <*> o
      .:? "nextPageToken"

-- | Get all routines from all specified data sets.
getRoutines ::
  (MonadIO m) =>
  BigQuerySourceConfig ->
  m (Either RestProblem [RestRoutine])
getRoutines BigQuerySourceConfig {..} =
  runExceptT
    (fmap concat (traverse (ExceptT . getRoutinesForDataSet _scConnection) _scDatasets))

-- | Get routines in the dataset.
getRoutinesForDataSet ::
  (MonadIO m) =>
  BigQueryConnection ->
  BigQueryDataset ->
  m (Either RestProblem [RestRoutine])
getRoutinesForDataSet conn dataSet = do
  liftIO (catchAny (run Nothing mempty) (pure . Left . GetRoutineProblem))
  where
    run pageToken acc = do
      let req =
            setRequestHeader "Content-Type" ["application/json"]
              $ parseRequest_ url
      eResp <- runBigQuery conn req
      case eResp of
        Left e -> pure (Left (GetRoutinesBigQueryProblem e))
        Right resp ->
          case getResponseStatusCode resp of
            200 ->
              case J.eitherDecode (getResponseBody resp) of
                Left e -> pure (Left (GetMetaDecodeProblem e))
                Right RestRoutineList {_rrlRoutines = routines, _rrlNextPageToken = nextPageToken} ->
                  case nextPageToken of
                    Nothing -> pure (Right (toList (acc <> Seq.fromList routines)))
                    Just token -> run (pure token) (acc <> Seq.fromList routines)
            _ -> pure (Left (RESTRequestNonOK (getResponseStatus resp)))
      where
        url =
          T.unpack
            $ "GET https://bigquery.googleapis.com/bigquery/v2/projects/"
            <> getBigQueryProjectId (_bqProjectId conn)
            <> "/datasets/"
            <> getBigQueryDataset dataSet
            <> "/routines?alt=json&"
            <> encodeParams extraParameters
        extraParameters = pageTokenParam <> readMaskParam
          where
            pageTokenParam =
              case pageToken of
                Nothing -> []
                Just token -> [("pageToken", token)]
            readMaskParam = [("readMask", "routineType,arguments,returnTableType")]
