{-# language DuplicateRecordFields #-}

-- |

module Hasura.Backends.MSSQL.Meta where

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseEither)
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Builder as SB
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import           Data.String
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import           Database.ODBC.SQLServer as Odbc
import           Hasura.Backends.MSSQL.Types
import           Hasura.Prelude

loadMetadata :: FilePath -> IO (Either String UserMetadata)
loadMetadata fp = fmap eitherDecode $ L.readFile fp

loadCatalogMetadata :: Text -> UserMetadata -> IO ()
loadCatalogMetadata connStr userMetadata = do
  conn <- connect connStr
  sql <- readFile "sql.sql"
  objects :: [SysObject] <- queryJson conn (fromString sql)
  print (filter (\SysObject{name} -> elem name ["Track","Artist","Album"]) objects)
  pure ()

--------------------------------------------------------------------------------
-- MSSQL-specific types

data SysObject = SysObject
  { name :: Text
  , object_id :: Int
  , type_desc :: Text
  , joined_sys_column :: [SysColumn]
  , joined_sys_schema :: SysSchema
  } deriving (Show, Generic)
instance FromJSON SysObject

data SysSchema = SysSchema
  { name :: Text
  , schema_id :: Int
  } deriving (Show, Generic)
instance FromJSON SysSchema

data SysColumn = SysColumn
  { name :: Text
  , column_id :: Int
  , system_type_id :: Int
  , is_nullable :: Bool
  , joined_sys_type :: SysType
  , joined_foreign_key_columns :: [SysForeignKeyColumn]
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
  , joined_referenced_table_name :: Text
  , joined_referenced_column_name :: Text
  , joined_referenced_sys_schema :: SysSchema
  } deriving (Show, Generic)
instance FromJSON SysForeignKeyColumn

--------------------------------------------------------------------------------
-- Quick catalog queries

queryJson :: FromJSON a => Connection -> Query -> IO [a]
queryJson conn query' = do
  (steps, iresult) <-
    stream
      conn
      query'
      (\(!steps, parser) input ->
         pure (Continue (steps + 1, feed parser (T.encodeUtf8 input))))
      (0 :: Int, parse json mempty)
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
