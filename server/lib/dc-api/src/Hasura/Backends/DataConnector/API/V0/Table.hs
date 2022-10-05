{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

--
module Hasura.Backends.DataConnector.API.V0.Table
  ( TableInfo (..),
    tiName,
    tiColumns,
    tiPrimaryKey,
    tiForeignKeys,
    tiDescription,
    TableName (..),
    ForeignKeys (..),
    ConstraintName (..),
    Constraint (..),
    cForeignTable,
    cColumnMapping,
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses)
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

-- | The fully qualified name of a table. The last element in the list is the table name
-- and all other elements represent namespacing of the table name.
-- For example, for a database that has schemas, the name would be '[<schema>,<table name>]'
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
  { _tiName :: TableName,
    _tiColumns :: [API.V0.ColumnInfo],
    _tiPrimaryKey :: Maybe [API.V0.ColumnName],
    _tiForeignKeys :: Maybe ForeignKeys,
    _tiDescription :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TableInfo

instance HasCodec TableInfo where
  codec =
    object "TableInfo" $
      TableInfo
        <$> requiredField "name" "The name of the table" .= _tiName
        <*> requiredField "columns" "The columns of the table" .= _tiColumns
        <*> optionalFieldOrNull "primary_key" "The primary key of the table" .= _tiPrimaryKey
        <*> optionalFieldOrNull "foreign_keys" "Foreign key constraints" .= _tiForeignKeys
        <*> optionalFieldOrNull "description" "Description of the table" .= _tiDescription

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
  { _cForeignTable :: TableName,
    _cColumnMapping :: HashMap API.V0.ColumnName API.V0.ColumnName
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON) via Autodocodec Constraint

instance HasCodec Constraint where
  codec =
    object "Constraint" $
      Constraint
        <$> requiredField "foreign_table" "The table referenced by the foreign key in the child table." .= _cForeignTable
        <*> requiredField "column_mapping" "The columns on which you want want to define the foreign key." .= _cColumnMapping

$(makeLenses ''TableInfo)
$(makeLenses ''Constraint)
