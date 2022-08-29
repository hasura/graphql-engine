{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataConnector.API.V0.Table
  ( TableInfo (..),
    TableName (..),
    ForeignKeys (..),
    ConstraintName (..),
    Constraint (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Prelude

--------------------------------------------------------------------------------

newtype TableName = TableName {unTableName :: NonEmpty Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TableName

instance HasCodec TableName where
  codec =
    named "TableName" $
      dimapCodec TableName unTableName codec
        <?> "The fully qualified name of a table, where the last item in the array is the table name and any earlier items represent the namespacing of the table name"

--------------------------------------------------------------------------------

-- | Table schema data from the 'SchemaResponse'.
data TableInfo = TableInfo
  { dtiName :: TableName,
    dtiColumns :: [API.V0.ColumnInfo],
    dtiPrimaryKey :: Maybe [API.V0.ColumnName],
    dtiDescription :: Maybe Text,
    dtiForeignKeys :: Maybe ForeignKeys
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
        <*> optionalFieldOrNull "foreign_keys" "Foreign Key Constraints" .= dtiForeignKeys

--------------------------------------------------------------------------------

newtype ForeignKeys = ForeignKeys {unConstraints :: HashMap ConstraintName Constraint}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON) via Autodocodec ForeignKeys

instance HasCodec ForeignKeys where
  codec = dimapCodec ForeignKeys unConstraints $ codec @(HashMap ConstraintName Constraint)

newtype ConstraintName = ConstraintName {unConstraintName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
  deriving anyclass (NFData, Hashable)

data Constraint = Constraint
  { cForeignTable :: Text,
    cColumnMapping :: HashMap Text Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON) via Autodocodec Constraint

instance HasCodec Constraint where
  codec =
    object "Constraint" $
      Constraint
        <$> requiredField "foreign_table" "The table referenced by the foreign key in the child table." .= cForeignTable
        <*> requiredField "column_mapping" "The columns on which you want want to define the foreign key." .= cColumnMapping
