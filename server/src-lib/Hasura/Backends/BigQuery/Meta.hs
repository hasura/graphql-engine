{-# LANGUAGE DuplicateRecordFields #-}

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

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson qualified as J
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import GHC.Generics
import Hasura.Backends.BigQuery.Connection
import Hasura.Backends.BigQuery.Source
import Hasura.Backends.BigQuery.Types
import Hasura.Prelude
import Network.HTTP.Simple
import Network.HTTP.Types

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
  deriving (Show)

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
  deriving (Show, Generic)

instance FromJSON RestTableReference

data RestTable = RestTable
  { tableReference :: RestTableReference,
    schema :: RestTableSchema
  }
  deriving (Show, Generic)

instance FromJSON RestTable

data RestTableSchema = RestTableSchema
  { fields :: [RestFieldSchema]
  }
  deriving (Show, Generic)

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
  deriving (Show, Generic)

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

data Mode = Nullable | Required | Repeated deriving (Show)

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
getTables ::
  (MonadIO m) =>
  BigQuerySourceConfig ->
  m (Either RestProblem [RestTable])
getTables BigQuerySourceConfig {..} =
  runExceptT
    (fmap concat (traverse (ExceptT . getTablesForDataSet _scConnection) _scDatasets))

-- | Get tables in the dataset.
getTablesForDataSet ::
  (MonadIO m) =>
  BigQueryConnection ->
  BigQueryDataset ->
  m (Either RestProblem [RestTable])
getTablesForDataSet conn dataSet = do
  result <-
    liftIO (catchAny (run Nothing mempty) (pure . Left . GetTablesProblem))
  case result of
    Left e -> pure (Left e)
    Right briefs ->
      fmap
        sequence
        ( traverse
            ( \RestTableBrief {tableReference = RestTableReference {tableId}} ->
                getTable conn dataSet tableId
            )
            briefs
        )
  where
    run pageToken acc = do
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
                    Just token -> run (pure token) (acc <> Seq.fromList tables)
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
                Nothing -> []
                Just token -> [("pageToken", token)]

-- | Get tables in the schema.
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
        extraParameters = []

encodeParams :: [(Text, Text)] -> Text
encodeParams = T.intercalate "&" . map (\(k, v) -> k <> "=" <> v)

-- Routines related

-- | The fine-grained type of the routine
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines#RoutineType
data RestRoutineType
  = ROUTINE_TYPE_UNSPECIFIED
  | SCALAR_FUNCTION
  | PROCEDURE
  | TABLE_VALUED_FUNCTION
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
