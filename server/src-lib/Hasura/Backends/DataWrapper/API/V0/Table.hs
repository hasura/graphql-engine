{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataWrapper.API.V0.Table
  ( TableInfo (..),
    TableName (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Hasura.Backends.DataWrapper.API.V0.Column qualified as API.V0
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

newtype TableName = TableName {unTableName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Cacheable, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TableName

instance HasCodec TableName where
  codec = dimapCodec TableName unTableName textCodec

--------------------------------------------------------------------------------

-- | Table schema data from the 'SchemaResponse'.
data TableInfo = TableInfo
  { dtiName :: TableName,
    dtiColumns :: [API.V0.ColumnInfo],
    dtiPrimaryKey :: Maybe Text,
    dtiDescription :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Cacheable, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TableInfo

instance HasCodec TableInfo where
  codec =
    object "TableInfo" $
      TableInfo
        <$> requiredField "table_name" "The name of the table" .= dtiName
        <*> requiredField "columns" "The columns of the table" .= dtiColumns
        <*> optionalFieldOrNull "primary_key" "The primary key of the table" .= dtiPrimaryKey
        <*> optionalFieldOrNull "description" "Description of the table" .= dtiDescription
