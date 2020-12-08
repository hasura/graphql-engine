{-# LANGUAGE DuplicateRecordFields #-}

-- |

module Hasura.SQL.BigQuery.Meta where

import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.Validate
import           Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as L
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List (find)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           Hasura.SQL.BigQuery.Credentials
import           Hasura.SQL.BigQuery.Types
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Prelude

--------------------------------------------------------------------------------
-- Types

data UnifyProblem
  = MissingTable UserTableName
  | MissingTableForRelationship UserUsing
  | MissingColumnForRelationship UserUsing
  | UnknownScalarType Text
  deriving (Show)

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
-- Schema loaders

loadMetadata :: FilePath -> IO (Either String UserMetadata)
loadMetadata fp = fmap eitherDecode $ L.readFile fp

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

--------------------------------------------------------------------------------
-- Unification

unifyTables ::
     [UserTableMetadata]
  -> [RestTable]
  -> Validate (NonEmpty UnifyProblem) [UnifiedTableMetadata]
unifyTables userTableMetadatas sysTables = do
  let indexedRestTables =
        HM.fromList
          (map
             (\sysTable@RestTable {tableReference} ->
                (restTableReferenceToUserTableName tableReference, sysTable))
             sysTables)
  tablesStage1 <- traverse (unifyTable indexedRestTables) userTableMetadatas
  let indexedValidTables =
        HM.fromList
          (map
             (\(UserTableMetadata {table}, sysTable) -> (table, sysTable))
             tablesStage1)
  traverse (unifyRelationships indexedValidTables) tablesStage1

-- Hierarchy: Project / Dataset / Table
-- see <https://cloud.google.com/bigquery/docs/datasets-intro>
restTableReferenceToUserTableName :: RestTableReference -> UserTableName
restTableReferenceToUserTableName RestTableReference {..} =
  UserTableName {name = tableId, schema = datasetId}
  -- We ignore project id and push that requirement up to the top to
  -- the data source level.

unifyTable ::
     HashMap UserTableName RestTable
  -> UserTableMetadata
  -> Validate (NonEmpty UnifyProblem) (UserTableMetadata, RestTable)
unifyTable indexedRestTables userMetaTable@UserTableMetadata {table} = do
  case HM.lookup table indexedRestTables of
    Nothing -> refute (pure (MissingTable table))
    Just sysTable -> pure (userMetaTable, sysTable)

unifyRelationships ::
     HashMap UserTableName RestTable
  -> (UserTableMetadata, RestTable)
  -> Validate (NonEmpty UnifyProblem) UnifiedTableMetadata
unifyRelationships indexedValidTables (UserTableMetadata {..}, RestTable {schema = RestTableSchema {fields}}) = do
  object_relationships' <-
    traverse
      (\UserObjectRelationship {using = using0, ..} -> do
         using <- checkUsing using0
         pure UnifiedObjectRelationship {..})
      object_relationships
  array_relationships' <-
    traverse
      (\UserArrayRelationship {using = using0, ..} -> do
         using <- checkUsing using0
         pure UnifiedArrayRelationship {..})
      array_relationships
  columns <-
    traverse
      (\RestFieldSchema {..} -> do
         pure UnifiedColumn {type' = restToScalarType type', ..})
      fields
  pure
    UnifiedTableMetadata
      { table =
          let UserTableName {..} = table
           in UnifiedTableName {..}
      , object_relationships = object_relationships'
      , array_relationships = array_relationships'
      , columns
      }
  where
    checkUsing using@UserUsing {foreign_key_constraint_on} =
      case HM.lookup referencedTable indexedValidTables of
        Nothing -> refute (pure (MissingTableForRelationship using))
        Just RestTable {schema = RestTableSchema {fields = remoteFields}} ->
          case find
                 (\RestFieldSchema {name = referencedColumn} ->
                    referencedColumn == column)
                 remoteFields of
            Nothing -> refute (pure (MissingColumnForRelationship using))
            Just {} ->
              pure
                (UnifiedUsing
                   { foreign_key_constraint_on =
                       UnifiedOn
                         { table =
                             let UserTableName {..} = referencedTable
                              in UnifiedTableName {..}
                         , ..
                         }
                   })
      where
        UserOn {table = referencedTable, column} = foreign_key_constraint_on

restToScalarType :: RestType -> ScalarType
restToScalarType =
 \case
    STRING -> StringScalarType
    BYTES -> BytesScalarType
    INTEGER -> IntegerScalarType
    INT64 -> Int64ScalarType
    FLOAT -> FloatScalarType
    FLOAT64 -> Float64ScalarType
    BOOLEAN -> BooleanScalarType
    BOOL -> BoolScalarType
    TIMESTAMP -> TimestampScalarType
    DATE -> DateScalarType
    TIME -> TimeScalarType
    DATETIME -> DatetimeScalarType
    GEOGRAPHY -> GeographyScalarType
    NUMERIC -> NumericScalarType
    RECORD -> RecordScalarType
    STRUCT -> StructScalarType

--------------------------------------------------------------------------------
-- Sample rest tables

sample :: [RestTable]
sample =
  [ RestTable
      { tableReference =
          RestTableReference
            { datasetId = "chinook"
            , projectId = "secret-epsilon-291908"
            , tableId = "Album"
            }
      , schema =
          RestTableSchema
            { fields =
                [ RestFieldSchema
                    {name = "AlbumId", type' = INTEGER, mode = Required}
                , RestFieldSchema
                    {name = "ArtistId", type' = INTEGER, mode = Required}
                , RestFieldSchema
                    {name = "Title", type' = STRING, mode = Required}
                ]
            }
      }
  , RestTable
      { tableReference =
          RestTableReference
            { datasetId = "chinook"
            , projectId = "secret-epsilon-291908"
            , tableId = "Artist"
            }
      , schema =
          RestTableSchema
            { fields =
                [ RestFieldSchema
                    {name = "ArtistId", type' = INTEGER, mode = Required}
                , RestFieldSchema
                    {name = "Name", type' = STRING, mode = Required}
                ]
            }
      }
  , RestTable
      { tableReference =
          RestTableReference
            { datasetId = "chinook"
            , projectId = "secret-epsilon-291908"
            , tableId = "Award"
            }
      , schema =
          RestTableSchema
            { fields =
                [ RestFieldSchema
                    {name = "TrackId", type' = INTEGER, mode = Required}
                , RestFieldSchema
                    {name = "Name", type' = STRING, mode = Required}
                ]
            }
      }
  , RestTable
      { tableReference =
          RestTableReference
            { datasetId = "chinook"
            , projectId = "secret-epsilon-291908"
            , tableId = "Genre"
            }
      , schema =
          RestTableSchema
            { fields =
                [ RestFieldSchema
                    {name = "GenreId", type' = INTEGER, mode = Required}
                , RestFieldSchema
                    {name = "Name", type' = STRING, mode = Required}
                ]
            }
      }
  , RestTable
      { tableReference =
          RestTableReference
            { datasetId = "chinook"
            , projectId = "secret-epsilon-291908"
            , tableId = "Track"
            }
      , schema =
          RestTableSchema
            { fields =
                [ RestFieldSchema
                    {name = "AlbumId", type' = INTEGER, mode = Required}
                , RestFieldSchema
                    {name = "Name", type' = STRING, mode = Required}
                , RestFieldSchema
                    {name = "TrackId", type' = INTEGER, mode = Nullable}
                , RestFieldSchema
                    {name = "GenreId", type' = INTEGER, mode = Nullable}
                ]
            }
      }
  ]

--------------------------------------------------------------------------------
-- Example

{-

> do Right (UserMetadata{tables}) <- loadMetadata "schema.json"; pure $ runValidate $ unifyTables tables sample

Right
  [ UnifiedTableMetadata
      { table = UnifiedTableName {schema = "chinook", name = "Album"}
      , object_relationships =
          [ UnifiedObjectRelationship
              { using =
                  UnifiedUsing
                    { foreign_key_constraint_on =
                        UnifiedOn
                          { table =
                              UnifiedTableName
                                {schema = "chinook", name = "Artist"}
                          , column = "ArtistId"
                          }
                    }
              , name = "Artist"
              }
          ]
      , array_relationships =
          [ UnifiedArrayRelationship
              { using =
                  UnifiedUsing
                    { foreign_key_constraint_on =
                        UnifiedOn
                          { table =
                              UnifiedTableName
                                {schema = "chinook", name = "Track"}
                          , column = "AlbumId"
                          }
                    }
              , name = "Track"
              }
          ]
      , columns =
          [ UnifiedColumn {name = "AlbumId", type' = IntegerScalarType}
          , UnifiedColumn {name = "ArtistId", type' = IntegerScalarType}
          , UnifiedColumn {name = "Title", type' = StringScalarType}
          ]
      }
  , UnifiedTableMetadata
      { table = UnifiedTableName {schema = "chinook", name = "Genre"}
      , object_relationships = []
      , array_relationships =
          [ UnifiedArrayRelationship
              { using =
                  UnifiedUsing
                    { foreign_key_constraint_on =
                        UnifiedOn
                          { table =
                              UnifiedTableName
                                {schema = "chinook", name = "Track"}
                          , column = "GenreId"
                          }
                    }
              , name = "Track"
              }
          ]
      , columns =
          [ UnifiedColumn {name = "GenreId", type' = IntegerScalarType}
          , UnifiedColumn {name = "Name", type' = StringScalarType}
          ]
      }
  , UnifiedTableMetadata
      { table = UnifiedTableName {schema = "chinook", name = "Track"}
      , object_relationships =
          [ UnifiedObjectRelationship
              { using =
                  UnifiedUsing
                    { foreign_key_constraint_on =
                        UnifiedOn
                          { table =
                              UnifiedTableName
                                {schema = "chinook", name = "Album"}
                          , column = "AlbumId"
                          }
                    }
              , name = "album"
              }
          , UnifiedObjectRelationship
              { using =
                  UnifiedUsing
                    { foreign_key_constraint_on =
                        UnifiedOn
                          { table =
                              UnifiedTableName
                                {schema = "chinook", name = "Genre"}
                          , column = "GenreId"
                          }
                    }
              , name = "genre"
              }
          ]
      , array_relationships = []
      , columns =
          [ UnifiedColumn {name = "AlbumId", type' = IntegerScalarType}
          , UnifiedColumn {name = "Name", type' = StringScalarType}
          , UnifiedColumn {name = "TrackId", type' = IntegerScalarType}
          , UnifiedColumn {name = "GenreId", type' = IntegerScalarType}
          ]
      }
  , UnifiedTableMetadata
      { table = UnifiedTableName {schema = "chinook", name = "Artist"}
      , object_relationships = []
      , array_relationships =
          [ UnifiedArrayRelationship
              { using =
                  UnifiedUsing
                    { foreign_key_constraint_on =
                        UnifiedOn
                          { table =
                              UnifiedTableName
                                {schema = "chinook", name = "Album"}
                          , column = "ArtistId"
                          }
                    }
              , name = "Album"
              }
          ]
      , columns =
          [ UnifiedColumn {name = "ArtistId", type' = IntegerScalarType}
          , UnifiedColumn {name = "Name", type' = StringScalarType}
          ]
      }
  ]


-}
