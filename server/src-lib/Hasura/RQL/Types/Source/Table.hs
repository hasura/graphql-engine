{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.Source.Table
  ( SourceTableInfo (..),
    stiName,
    stiType,
    stiColumns,
    stiLogicalModels,
    stiPrimaryKey,
    stiForeignKeys,
    stiDescription,
    stiInsertable,
    stiUpdatable,
    stiDeletable,
    SourceTableType (..),
    SourceForeignKeys (..),
    unSourceForeignKeys,
    SourceConstraint (..),
    scForeignTable,
    scColumnMapping,
  )
where

--------------------------------------------------------------------------------

import Autodocodec
import Autodocodec.OpenAPI ()
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.LogicalModel.Metadata
import Hasura.RQL.Types.Backend (Backend (..), ConstraintName)
import Hasura.RQL.Types.Relationships.Local (RelMapping (..))
import Hasura.RQL.Types.Source.Column (SourceColumnInfo)
import Hasura.RQL.Types.Source.TableType
import Prelude

--------------------------------------------------------------------------------

-- | Table schema data from the 'SchemaResponse'.
data SourceTableInfo b = SourceTableInfo
  { _stiName :: TableName b,
    _stiType :: SourceTableType,
    _stiColumns :: [SourceColumnInfo b],
    _stiLogicalModels :: [LogicalModelMetadata b],
    _stiPrimaryKey :: Maybe (NonEmpty (Column b)),
    _stiForeignKeys :: SourceForeignKeys b,
    _stiDescription :: Maybe Text,
    _stiInsertable :: Bool,
    _stiUpdatable :: Bool,
    _stiDeletable :: Bool
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec (SourceTableInfo b)

deriving stock instance (Backend b) => Eq (SourceTableInfo b)

deriving stock instance (Backend b) => Show (SourceTableInfo b)

instance (Backend b) => HasCodec (SourceTableInfo b) where
  codec =
    object "TableInfo" $
      SourceTableInfo
        <$> requiredField "name" "The name of the table" .= _stiName
        <*> optionalFieldWithDefault "type" Table "The type of table" .= _stiType
        <*> requiredField "columns" "The columns of the table" .= _stiColumns
        <*> optionalFieldWithOmittedDefault "logical_models" [] "The logical models referenced by the table's column types" .= _stiLogicalModels
        <*> dimapMaybeNonEmpty (optionalFieldWithOmittedDefault "primary_key" [] "The primary key of the table") .= _stiPrimaryKey
        <*> optionalFieldWithOmittedDefault "foreign_keys" (SourceForeignKeys mempty) "Foreign key constraints" .= _stiForeignKeys
        <*> optionalFieldOrNull "description" "Description of the table" .= _stiDescription
        <*> optionalFieldWithDefault "insertable" False "Whether or not new rows can be inserted into the table" .= _stiInsertable
        <*> optionalFieldWithDefault "updatable" False "Whether or not existing rows can be updated in the table" .= _stiUpdatable
        <*> optionalFieldWithDefault "deletable" False "Whether or not existing rows can be deleted in the table" .= _stiDeletable
    where
      dimapMaybeNonEmpty :: Codec context [a] [a] -> Codec context (Maybe (NonEmpty a)) (Maybe (NonEmpty a))
      dimapMaybeNonEmpty = dimapCodec NonEmpty.nonEmpty (maybe [] NonEmpty.toList)

--------------------------------------------------------------------------------

newtype SourceForeignKeys b = SourceForeignKeys {_unSourceForeignKeys :: HashMap (ConstraintName b) (SourceConstraint b)}
  deriving stock (Generic)
  deriving anyclass (Hashable)
  deriving (FromJSON, ToJSON) via Autodocodec (SourceForeignKeys b)

deriving stock instance (Backend b) => Eq (SourceForeignKeys b)

deriving stock instance (Backend b) => Ord (SourceForeignKeys b)

deriving stock instance (Backend b) => Show (SourceForeignKeys b)

instance (Backend b) => HasCodec (SourceForeignKeys b) where
  codec = dimapCodec SourceForeignKeys _unSourceForeignKeys $ codec @(HashMap (ConstraintName b) (SourceConstraint b))

--------------------------------------------------------------------------------

data SourceConstraint b = SourceConstraint
  { _scForeignTable :: TableName b,
    _scColumnMapping :: RelMapping b
  }
  deriving stock (Generic)
  deriving anyclass (Hashable)
  deriving (FromJSON, ToJSON) via Autodocodec (SourceConstraint b)

deriving stock instance (Backend b) => Eq (SourceConstraint b)

deriving stock instance (Backend b) => Ord (SourceConstraint b)

deriving stock instance (Backend b) => Show (SourceConstraint b)

instance (Backend b) => HasCodec (SourceConstraint b) where
  codec =
    object "SourceConstraint" $
      SourceConstraint
        <$> requiredField "foreign_table" "The table referenced by the foreign key in the child table." .= _scForeignTable
        <*> requiredField "column_mapping" "The columns on which you want want to define the foreign key." .= _scColumnMapping

--------------------------------------------------------------------------------

$(makeLenses ''SourceTableInfo)
$(makeLenses ''SourceConstraint)
$(makeLenses ''SourceForeignKeys)
