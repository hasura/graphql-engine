{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Table
  ( TableName (..),
    TableInfo (..),
    singletonTableName,
    tableNameToText,
    tiName,
    tiType,
    tiColumns,
    tiPrimaryKey,
    tiForeignKeys,
    tiDescription,
    tiInsertable,
    tiUpdatable,
    tiDeletable,
    TableType (..),
    ForeignKeys (..),
    unForeignKeys,
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
import Data.List.NonEmpty qualified as NonEmpty
import Data.OpenApi (ToSchema)
import Data.Text (Text, intercalate)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Prelude

--------------------------------------------------------------------------------

-- | The fully qualified name of a table. The last element in the list is the table name
-- and all other elements represent namespacing of the table name.
-- For example, for a database that has schemas, the name would be '[<schema>,<table name>]'
newtype TableName = TableName {unTableName :: NonEmpty.NonEmpty Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TableName

singletonTableName :: Text -> TableName
singletonTableName name = TableName (NonEmpty.singleton name)

tableNameToText :: TableName -> Text
tableNameToText (TableName tns) = intercalate "." (NonEmpty.toList tns)

instance HasCodec TableName where
  codec =
    named "TableName" $
      dimapCodec TableName unTableName codec
        <?> "The fully qualified name of a table, where the last item in the array is the table name and any earlier items represent the namespacing of the table name"

--------------------------------------------------------------------------------

-- | Table schema data from the 'SchemaResponse'.
data TableInfo = TableInfo
  { _tiName :: TableName,
    _tiType :: TableType,
    _tiColumns :: [API.V0.ColumnInfo],
    _tiPrimaryKey :: Maybe (NonEmpty API.V0.ColumnName),
    _tiForeignKeys :: ForeignKeys,
    _tiDescription :: Maybe Text,
    _tiInsertable :: Bool,
    _tiUpdatable :: Bool,
    _tiDeletable :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TableInfo

instance HasCodec TableInfo where
  codec =
    object "TableInfo" $
      TableInfo
        <$> requiredField "name" "The name of the table" .= _tiName
        <*> optionalFieldWithDefault "type" Table "The type of table" .= _tiType
        <*> optionalFieldWithDefault "columns" [] "The columns of the table" .= _tiColumns
        <*> dimapMaybeNonEmpty (optionalFieldWithOmittedDefault "primary_key" [] "The primary key of the table") .= _tiPrimaryKey
        <*> optionalFieldWithOmittedDefault "foreign_keys" (ForeignKeys mempty) "Foreign key constraints" .= _tiForeignKeys
        <*> optionalFieldOrNull "description" "Description of the table" .= _tiDescription
        <*> optionalFieldWithDefault "insertable" False "Whether or not new rows can be inserted into the table" .= _tiInsertable
        <*> optionalFieldWithDefault "updatable" False "Whether or not existing rows can be updated in the table" .= _tiUpdatable
        <*> optionalFieldWithDefault "deletable" False "Whether or not existing rows can be deleted in the table" .= _tiDeletable
    where
      dimapMaybeNonEmpty :: Codec context [a] [a] -> Codec context (Maybe (NonEmpty a)) (Maybe (NonEmpty a))
      dimapMaybeNonEmpty = dimapCodec NonEmpty.nonEmpty (maybe [] NonEmpty.toList)

--------------------------------------------------------------------------------

data TableType
  = Table
  | View
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON) via Autodocodec TableType

instance HasCodec TableType where
  codec =
    named "TableType" $
      ( stringConstCodec
          [ (Table, "table"),
            (View, "view")
          ]
      )

--------------------------------------------------------------------------------

newtype ForeignKeys = ForeignKeys {_unForeignKeys :: HashMap ConstraintName Constraint}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON) via Autodocodec ForeignKeys

instance HasCodec ForeignKeys where
  codec = dimapCodec ForeignKeys _unForeignKeys $ codec @(HashMap ConstraintName Constraint)

--------------------------------------------------------------------------------

newtype ConstraintName = ConstraintName {unConstraintName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
  deriving anyclass (NFData, Hashable)

data Constraint = Constraint
  { _cForeignTable :: TableName,
    _cColumnMapping :: API.V0.ColumnPathMapping
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Hashable)
  deriving (FromJSON, ToJSON) via Autodocodec Constraint

instance HasCodec Constraint where
  codec =
    object "Constraint" $
      Constraint
        <$> requiredField "foreign_table" "The table referenced by the foreign key in the child table." .= _cForeignTable
        <*> requiredField "column_mapping" "The columns on which you want want to define the foreign key." .= _cColumnMapping

--------------------------------------------------------------------------------

$(makeLenses ''TableInfo)
$(makeLenses ''Constraint)
$(makeLenses ''ForeignKeys)
