{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataConnector.API.V0.Table
  ( TableInfo (..),
    TableName (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Prelude

--------------------------------------------------------------------------------

newtype TableName = TableName {unTableName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
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
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TableInfo

instance HasCodec TableInfo where
  codec =
    object "TableInfo" $
      TableInfo
        <$> requiredField "name" "The name of the table" .= dtiName
        <*> requiredField "columns" "The columns of the table" .= dtiColumns
        <*> optionalFieldOrNull "primary_key" "The primary key of the table" .= dtiPrimaryKey
        <*> optionalFieldOrNull "description" "Description of the table" .= dtiDescription
