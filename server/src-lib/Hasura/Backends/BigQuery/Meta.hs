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
import Data.Aeson qualified as Aeson
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Hasura.Backends.BigQuery.Connection
import Hasura.Backends.BigQuery.Source
import Hasura.Backends.BigQuery.Types
import Hasura.Prelude (hasuraJSON)
import Network.HTTP.Simple
import Network.HTTP.Types
import Prelude

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
  | STRUCT -- (same as RECORD).
  deriving (Show)

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
      "RECORD" -> pure STRUCT
      "STRUCT" -> pure STRUCT
      _ -> fail ("invalid type " ++ show s)

--------------------------------------------------------------------------------
-- REST request

-- | Get all tables from all specified data sets.
getTables ::
  MonadIO m =>
  BigQuerySourceConfig ->
  m (Either RestProblem [RestTable])
getTables BigQuerySourceConfig {..} =
  runExceptT
    (fmap concat (traverse (ExceptT . getTablesForDataSet _scConnection) _scDatasets))

-- | Get tables in the dataset.
getTablesForDataSet ::
  MonadIO m =>
  BigQueryConnection ->
  Text ->
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
            setRequestHeader "Content-Type" ["application/json"] $
              parseRequest_ url
      eResp <- runBigQuery conn req
      case eResp of
        Left e -> pure (Left (GetTablesBigQueryProblem e))
        Right resp ->
          case getResponseStatusCode resp of
            200 ->
              case Aeson.eitherDecode (getResponseBody resp) of
                Left e -> pure (Left (GetMetaDecodeProblem e))
                Right RestTableList {nextPageToken, tables} ->
                  case nextPageToken of
                    Nothing -> pure (Right (toList (acc <> Seq.fromList tables)))
                    Just token -> run (pure token) (acc <> Seq.fromList tables)
            _ -> pure (Left (RESTRequestNonOK (getResponseStatus resp)))
      where
        url =
          "GET https://bigquery.googleapis.com/bigquery/v2/projects/"
            <> T.unpack (_bqProjectId conn)
            <> "/datasets/"
            <> T.unpack dataSet
            <> "/tables?alt=json&"
            <> T.unpack (encodeParams extraParameters)
        extraParameters = pageTokenParam
          where
            pageTokenParam =
              case pageToken of
                Nothing -> []
                Just token -> [("pageToken", token)]

-- | Get tables in the schema.
getTable ::
  MonadIO m =>
  BigQueryConnection ->
  Text ->
  Text ->
  m (Either RestProblem RestTable)
getTable conn dataSet tableId = do
  liftIO (catchAny run (pure . Left . GetTableProblem))
  where
    run = do
      let req =
            setRequestHeader "Content-Type" ["application/json"] $
              parseRequest_ url
      eResp <- runBigQuery conn req
      case eResp of
        Left e -> pure (Left (GetTablesBigQueryProblem e))
        Right resp ->
          case getResponseStatusCode resp of
            200 ->
              case Aeson.eitherDecode (getResponseBody resp) of
                Left e -> pure (Left (GetMetaDecodeProblem e))
                Right table -> pure (Right table)
            _ -> pure (Left (RESTRequestNonOK (getResponseStatus resp)))
      where
        url =
          "GET https://bigquery.googleapis.com/bigquery/v2/projects/"
            <> T.unpack (_bqProjectId conn)
            <> "/datasets/"
            <> T.unpack dataSet
            <> "/tables/"
            <> T.unpack tableId
            <> "?alt=json&"
            <> T.unpack (encodeParams extraParameters)
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
  deriving (Show, Generic)

instance FromJSON RestRoutineType

-- | Input argument of a function/routine.
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines#Argument
data RestArgument = RestArgument
  { -- | The name of this argument. Can be absent for function return argument.
    _raName :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON RestArgument where
  parseJSON = genericParseJSON hasuraJSON

-- | A field or a column.
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/StandardSqlField
data RestStandardSqlField = RestStandardSqlField
  { -- | The field name is optional and is absent for fields with STRUCT type.
    _rssfName :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON RestStandardSqlField where
  parseJSON = genericParseJSON hasuraJSON

-- | A table type, which has only list of columns with names and types.
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines#StandardSqlTableType
data RestStandardSqlTableType = RestStandardSqlTableType
  { _rrttColumns :: [RestStandardSqlField]
  }
  deriving (Show, Generic)

instance FromJSON RestStandardSqlTableType where
  parseJSON = genericParseJSON hasuraJSON

-- | Id path of a routine.
-- Ref: https://cloud.google.com/bigquery/docs/reference/rest/v2/routines#RoutineReference
data RestRoutineReference = RestRoutineReference
  { datasetId :: Text,
    projectId :: Text,
    routineId :: Text
  }
  deriving (Show, Generic)

instance FromJSON RestRoutineReference

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
  deriving (Show, Generic)

instance FromJSON RestRoutine

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
      <$> o .:? "routines" .!= [] -- "routine" field is absent when there are no routines defined
      <*> o .:? "nextPageToken"

-- | Get all routines from all specified data sets.
getRoutines ::
  MonadIO m =>
  BigQuerySourceConfig ->
  m (Either RestProblem [RestRoutine])
getRoutines BigQuerySourceConfig {..} =
  runExceptT
    (fmap concat (traverse (ExceptT . getRoutinesForDataSet _scConnection) _scDatasets))

-- | Get routines in the dataset.
getRoutinesForDataSet ::
  MonadIO m =>
  BigQueryConnection ->
  Text ->
  m (Either RestProblem [RestRoutine])
getRoutinesForDataSet conn dataSet = do
  liftIO (catchAny (run Nothing mempty) (pure . Left . GetRoutineProblem))
  where
    run pageToken acc = do
      let req =
            setRequestHeader "Content-Type" ["application/json"] $
              parseRequest_ url
      eResp <- runBigQuery conn req
      case eResp of
        Left e -> pure (Left (GetRoutinesBigQueryProblem e))
        Right resp ->
          case getResponseStatusCode resp of
            200 ->
              case Aeson.eitherDecode (getResponseBody resp) of
                Left e -> pure (Left (GetMetaDecodeProblem e))
                Right RestRoutineList {_rrlRoutines = routines, _rrlNextPageToken = nextPageToken} ->
                  case nextPageToken of
                    Nothing -> pure (Right (toList (acc <> Seq.fromList routines)))
                    Just token -> run (pure token) (acc <> Seq.fromList routines)
            _ -> pure (Left (RESTRequestNonOK (getResponseStatus resp)))
      where
        url =
          "GET https://bigquery.googleapis.com/bigquery/v2/projects/"
            <> T.unpack (_bqProjectId conn)
            <> "/datasets/"
            <> T.unpack dataSet
            <> "/routines?alt=json&"
            <> T.unpack (encodeParams extraParameters)
        extraParameters = pageTokenParam
          where
            pageTokenParam =
              case pageToken of
                Nothing -> []
                Just token -> [("pageToken", token)]
