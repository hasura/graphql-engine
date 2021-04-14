{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Hasura.Backends.BigQuery.Meta
  ( MetadataError(..)
  , getTables
  , RestTableReference(..)
  , RestTable(..)
  , RestTableSchema(..)
  , RestFieldSchema(..)
  , RestType(..)
  , Mode(..)
  ) where


import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Foldable
import           Data.Maybe
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Hasura.Backends.BigQuery.Connection
import           Hasura.Backends.BigQuery.Source
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Prelude

--------------------------------------------------------------------------------
-- Types

data MetadataError
  = RestProblem RestProblem
  deriving (Show)

data RestProblem
  = GetTablesProblem SomeException
  | GetTableProblem SomeException
  | GetMetaDecodeProblem String
  | GetTablesBigQueryProblem BigQueryProblem
  | RESTRequestNonOK Status
  deriving (Show)

data RestTableList = RestTableList
  { nextPageToken :: Maybe Text
  , tables :: [RestTableBrief]
  } deriving (Show)

instance FromJSON RestTableList where
  parseJSON =
    withObject
      "RestTableList"
      (\o -> do
         kind <- o .: "kind"
         case kind of
           ("bigquery#tableList"::Text) -> do
             nextPageToken <- o .:? "nextPageToken"
             tables <- o .: "tables"
             pure RestTableList {..}
           _ -> fail "Expected kind of bigquery#tableList")

data RestTableBrief = RestTableBrief
  { tableReference :: RestTableReference
  } deriving (Show, Generic)
instance FromJSON RestTableBrief

data RestTableReference = RestTableReference
  { datasetId :: Text
  , projectId :: Text
  , tableId :: Text
  } deriving (Show, Generic)
instance FromJSON RestTableReference

data RestTable = RestTable
  { tableReference :: RestTableReference
  , schema :: RestTableSchema
  } deriving (Show, Generic)
instance FromJSON RestTable

data RestTableSchema = RestTableSchema
  { fields :: [RestFieldSchema]
  } deriving (Show, Generic)
instance FromJSON RestTableSchema

data RestFieldSchema = RestFieldSchema
  { name :: Text
  , type' :: RestType
    -- ^ The field data type. Possible values include STRING, BYTES,
    -- INTEGER, INT64 (same as INTEGER), FLOAT, FLOAT64 (same as
    -- FLOAT), BOOLEAN, BOOL (same as BOOLEAN), TIMESTAMP, DATE, TIME,
    -- DATETIME, GEOGRAPHY, NUMERIC, RECORD (where RECORD indicates
    -- that the field contains a nested schema) or STRUCT (same as
    -- RECORD).
  , mode :: Mode
  -- The field mode. Possible values include NULLABLE, REQUIRED and
  -- REPEATED. The default value is NULLABLE.
  } deriving (Show, Generic)
instance FromJSON RestFieldSchema where
  parseJSON =
    withObject
      "RestFieldSchema"
      (\o -> do
         type' <- o .: "type"
         name <- o .: "name"
         mode <- fmap (fromMaybe Nullable) (o .:? "mode")
         pure RestFieldSchema {..})

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
     MonadIO m
  => BigQuerySourceConfig
  -> m (Either RestProblem [RestTable])
getTables sc@BigQuerySourceConfig {..} =
  runExceptT
    (fmap concat (traverse (ExceptT . getTablesForDataSet sc) _scDatasets))

-- | Get tables in the dataset.
getTablesForDataSet ::
     MonadIO m
  => BigQuerySourceConfig
  -> Text
  -> m (Either RestProblem [RestTable])
getTablesForDataSet sc@BigQuerySourceConfig{..} dataSet = do
  result <-
    liftIO (catchAny (run Nothing mempty) (pure . Left . GetTablesProblem))
  case result of
    Left e -> pure (Left e)
    Right briefs ->
      fmap
        sequence
        (traverse
           (\RestTableBrief {tableReference = RestTableReference {tableId}} ->
              getTable sc dataSet tableId)
           briefs)
  where
    run pageToken acc = do
      let req = setRequestHeader "Content-Type" ["application/json"]
                  $ parseRequest_ url
      eResp <- runBigQuery sc req
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
          "GET https://bigquery.googleapis.com/bigquery/v2/projects/" <>
           T.unpack _scProjectId  <>
           "/datasets/" <>
           T.unpack dataSet <>
           "/tables?alt=json&key=" <>
           -- T.unpack apiToken <>
           "&" <>
           T.unpack (encodeParams extraParameters)
        extraParameters = pageTokenParam
          where
            pageTokenParam =
              case pageToken of
                Nothing -> []
                Just token -> [("pageToken", token)]

-- | Get tables in the schema.
getTable ::
     MonadIO m
  => BigQuerySourceConfig
  -> Text -> Text
  -> m (Either RestProblem RestTable)
getTable sc@BigQuerySourceConfig {..} dataSet tableId = do
  liftIO (catchAny run (pure . Left . GetTableProblem))
  where
    run = do
      let req =
            setRequestHeader "Content-Type" ["application/json"] $
            parseRequest_ url
      eResp <- runBigQuery sc req
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
          "GET https://bigquery.googleapis.com/bigquery/v2/projects/" <>
          T.unpack _scProjectId <>
          "/datasets/" <>
          T.unpack dataSet <>
          "/tables/" <>
          T.unpack tableId <>
          "?alt=json&key=" <>
           -- T.unpack apiToken <>
          "&" <>
          T.unpack (encodeParams extraParameters)
        extraParameters = []

encodeParams :: [(Text, Text)] -> Text
encodeParams = T.intercalate "&" . map (\(k, v) -> k <> "=" <> v)
