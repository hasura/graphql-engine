{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Hasura.SQL.BigQuery.Meta where

import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.Maybe
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           Hasura.SQL.BigQuery.Credentials
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Prelude

--------------------------------------------------------------------------------
-- Types

data RestProblem
  = GetTablesProblem SomeException
  | GetTableProblem SomeException
  | GetMetaDecodeProblem String
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
  { name :: String
  , type' :: Type
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

data Type
  = STRING
  | BYTES
  | INTEGER
  | INT64 -- (same as INTEGER)
  | FLOAT
  | FLOAT64 -- (same as FLOAT)
  | BOOLEAN
  | BOOL -- (same as BOOLEAN)
  | TIMESTAMP
  | DATE
  | TIME
  | DATETIME
  | GEOGRAPHY
  | NUMERIC
  | RECORD -- (where RECORD indicates that the field contains a nested schema)
  | STRUCT -- (same as RECORD).
   deriving (Show)
instance FromJSON Type where
  parseJSON j = do
    s <- parseJSON j
    case s :: Text of
      "STRING" -> pure STRING
      "BYTES" -> pure BYTES
      "INTEGER" -> pure INTEGER
      "INT64" -> pure INTEGER
      "FLOAT" -> pure FLOAT
      "FLOAT64" -> pure FLOAT
      "BOOLEAN" -> pure BOOLEAN
      "BOOL" -> pure BOOLEAN
      "TIMESTAMP" -> pure TIMESTAMP
      "DATE" -> pure DATE
      "TIME" -> pure TIME
      "DATETIME" -> pure DATETIME
      "GEOGRAPHY" -> pure GEOGRAPHY
      "NUMERIC" -> pure NUMERIC
      "RECORD" -> pure RECORD
      "STRUCT" -> pure RECORD
      _ -> fail ("invalid type " ++ show s)

--------------------------------------------------------------------------------
-- REST request

-- | Get tables in the schema.
getTables ::
     MonadIO m
  => Credentials -> Text
  -> m (Either RestProblem [RestTable])
getTables credentials@Credentials {..} dataSet = do
  result <-
    liftIO (catchAny (run Nothing mempty) (pure . Left . GetTablesProblem))
  case result of
    Left e -> pure (Left e)
    Right briefs ->
      fmap
        sequence
        (traverse
           (\RestTableBrief {tableReference = RestTableReference {tableId}} ->
              getTable credentials dataSet tableId)
           briefs)
  where
    run pageToken acc = do
      let
      req <- parseRequest url
      let request =
            req
              { requestHeaders =
                  [ ("Authorization", "Bearer " <> T.encodeUtf8 accessToken)
                  , ("Content-Type", "application/json")
                  , ("User-Agent", "curl/7.54")
                  ]
              , checkResponse = \_ _resp -> pure ()
              , method = "GET"
              }
      mgr <- newManager tlsManagerSettings
      resp <- httpLbs request mgr
      case statusCode (responseStatus resp) of
        200 ->
          case Aeson.eitherDecode (responseBody resp) of
            Left e -> pure (Left (GetMetaDecodeProblem e))
            Right RestTableList {nextPageToken, tables} ->
              case nextPageToken of
                Nothing -> pure (Right tables)
                Just token -> run (pure token) (acc <> Seq.fromList tables)
        _ -> pure (Left (RESTRequestNonOK (responseStatus resp)))
      where
        url =
          ("https://bigquery.googleapis.com/bigquery/v2/projects/" <>
           T.unpack projectName <>
           "/datasets/" <>
           T.unpack dataSet <>
           "/tables?alt=json&key=" <>
           T.unpack apiToken <>
           "&" <>
           T.unpack (encodeParams extraParameters))
        extraParameters = pageTokenParam <> [("maxResults", "5")]
          where
            pageTokenParam =
              case pageToken of
                Nothing -> []
                Just token -> [("pageToken", token)]

-- | Get tables in the schema.
getTable ::
     MonadIO m
  => Credentials
  -> Text
  -> Text
  -> m (Either RestProblem RestTable)
getTable Credentials {..} dataSet tableId =
  liftIO (catchAny run (pure . Left . GetTableProblem))
  where
    run = do
      let
      req <- parseRequest url
      let request =
            req
              { requestHeaders =
                  [ ("Authorization", "Bearer " <> T.encodeUtf8 accessToken)
                  , ("Content-Type", "application/json")
                  , ("User-Agent", "curl/7.54")
                  ]
              , checkResponse = \_ _resp -> pure ()
              , method = "GET"
              }
      mgr <- newManager tlsManagerSettings
      resp <- httpLbs request mgr
      case statusCode (responseStatus resp) of
        200 ->
          case Aeson.eitherDecode (responseBody resp) of
            Left e -> pure (Left (GetMetaDecodeProblem e))
            Right table -> pure (Right table)
        _ -> pure (Left (RESTRequestNonOK (responseStatus resp)))
      where
        url =
          ("https://bigquery.googleapis.com/bigquery/v2/projects/" <>
           T.unpack projectName <>
           "/datasets/" <>
           T.unpack dataSet <>
           "/tables/" <>
           T.unpack tableId <>
           "?alt=json&key=" <>
           T.unpack apiToken <>
           "&" <>
           T.unpack (encodeParams extraParameters))
        extraParameters = []

encodeParams :: [(Text, Text)] -> Text
encodeParams = T.intercalate "&" . map (\(k, v) -> k <> "=" <> v)
