{-# language DuplicateRecordFields #-}

-- |

module Hasura.Backends.MSSQL.Meta where

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseEither)
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Builder as SB
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import           Database.ODBC.SQLServer
import           Hasura.Backends.MSSQL.Types
import           Hasura.Prelude

loadMetadata :: FilePath -> IO (Either String UserMetadata)
loadMetadata fp = fmap eitherDecode $ L.readFile fp

loadCatalogMetadata :: Text -> UserMetadata -> IO CatalogMetadata
loadCatalogMetadata connStr _userMetadata = do
  conn <- connect connStr
  schemas :: [Schema] <-
    queryJson conn "select * from sys.schemas for json path;"
  tables :: [SysObject] <-
    queryJson conn "select * from sys.objects for json path;"
  -- Next step: table column types and relationships.
  _columns :: [SysColumn] <-
    queryJson conn "select * from sys.columns for json path;"
  _types :: [SysType] <-
    queryJson conn "select * from sys.types for json path;"
  _foreign_key_columns :: [SysForeignKeyColumn] <-
    queryJson conn "select * from sys.foreign_key_columns for json path;"
  -- Then consolidate relationships in user metadata vs catalog metadata.
  let schemaMap = HM.fromList (map (\Schema {..} -> (schema_id, name)) schemas)
  pure
    CatalogMetadata
      { tables =
          mapMaybe
            (\SysObject {name, schema_id, type_desc} ->
               if type_desc == "USER_TABLE"
                 then pure
                        (CatalogTableMetadata
                           { object_relationships = []
                           , array_relationships = []
                           , table =
                               CatalogTableName
                                 {schema = HM.lookup schema_id schemaMap, name}
                           })
                 else Nothing)
            tables
      }

--------------------------------------------------------------------------------
-- MSSQL-specific types

data Schema = Schema
  { name :: Text
  , schema_id :: Int
  } deriving (Show, Generic)
instance FromJSON Schema

data SysObject = SysObject
  { name :: Text
  , schema_id :: Int
  , object_id :: Int
  , type_desc :: Text
  } deriving (Show, Generic)
instance FromJSON SysObject

data SysColumn = SysColumn
  { name :: Text
  , column_id :: Int
  , object_id :: Int
  , system_type_id :: Int
  , is_nullable :: Bool
  } deriving (Show, Generic)
instance FromJSON SysColumn

data SysType = SysType
  { name :: Text
  , schema_id :: Int
  , system_type_id :: Int
  } deriving (Show, Generic)
instance FromJSON SysType

data SysForeignKeyColumn = SysForeignKeyColumn
  { constraint_object_id :: Int
  , constraint_column_id :: Int
  , parent_object_id :: Int
  , parent_column_id :: Int
  , referenced_object_id :: Int
  , referenced_column_id :: Int
  } deriving (Show, Generic)
instance FromJSON SysForeignKeyColumn

--------------------------------------------------------------------------------
-- Quick catalog queries

-- (0.05 secs, 3,628,240 bytes)
queryJson :: FromJSON a => Connection -> Query -> IO [a]
queryJson conn query' = do
  (steps, iresult) <-
    stream
      conn
      query'
      (\(!steps, parser) input ->
         pure (Continue (steps + 1, feed parser (T.encodeUtf8 input))))
      (0::Int, parse json mempty)
  case steps of
    0 -> pure []
    _ ->
      case iresult of
        Done _ jvalue ->
          case parseEither parseJSON jvalue of
            Left e -> error e
            Right as -> pure as
        Partial {} -> error "Incomplete output from SQL Server."
        Fail _ _ctx err -> error ("JSON parser error: " <> err)

-- (0.08 secs, 6,340,568 bytes)
_queryJson :: FromJSON a => Connection -> Query -> IO [a]
_queryJson conn query' = do
  chunks <- query conn query'
  case eitherDecode
         (SB.toLazyByteString
            (foldMap (SB.lazyByteString . LT.encodeUtf8) chunks)) of
    Left e -> error e
    Right v -> pure v
