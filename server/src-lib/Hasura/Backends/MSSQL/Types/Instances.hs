{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Types Instances
--
-- Instances for types from "Hasura.Backends.MSSQL.Types.Internal" that're slow to compile.
module Hasura.Backends.MSSQL.Types.Instances () where

import Data.Aeson.Extended
import Data.Aeson.Types
import Data.Text.Extended (ToTxt (..))
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Types.Internal
import Hasura.Incremental.Internal.Dependency
import Hasura.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

$( fmap concat $ for
     [''Aliased]
     \name ->
       [d|
         deriving instance Generic ($(conT name) a)

         instance Hashable a => Hashable ($(conT name) a)

         instance Cacheable a => Cacheable ($(conT name) a)

         deriving instance Eq a => Eq ($(conT name) a)

         instance NFData a => NFData ($(conT name) a)

         deriving instance Show a => Show ($(conT name) a)

         deriving instance Functor $(conT name)

         deriving instance Data a => Data ($(conT name) a)
         |]
 )

$( fmap concat $ for
     [ ''UnifiedTableName,
       ''UnifiedObjectRelationship,
       ''UnifiedArrayRelationship,
       ''UnifiedUsing,
       ''UnifiedOn,
       ''UnifiedColumn,
       ''TempTableName,
       ''SomeTableName
     ]
     \name ->
       [d|
         deriving instance Generic $(conT name)

         instance Hashable $(conT name)

         instance Cacheable $(conT name)

         deriving instance Eq $(conT name)

         deriving instance Show $(conT name)

         deriving instance Data $(conT name)

         instance NFData $(conT name)

         instance FromJSON $(conT name)

         deriving instance Ord $(conT name)
         |]
 )

$( fmap concat $ for
     [ ''Where,
       ''For,
       ''Aggregate,
       ''EntityAlias,
       ''ForJson,
       ''JsonCardinality,
       ''Root,
       ''OrderBy,
       ''JoinAlias,
       ''Reselect,
       ''ColumnName,
       ''DataLength,
       ''Expression,
       ''FunctionApplicationExpression,
       ''MethodApplicationExpression,
       ''NullsOrder,
       ''Order,
       ''ScalarType,
       ''TableName,
       ''Select,
       ''With,
       ''Top,
       ''FieldName,
       ''JsonPath,
       ''Op,
       ''SpatialOp,
       ''Projection,
       ''From,
       ''OpenJson,
       ''JsonFieldSpec,
       ''Join,
       ''JoinSource,
       ''SelectIntoTempTable,
       ''SITTConstraints,
       ''InsertValuesIntoTempTable,
       ''InsertOutput,
       ''Inserted,
       ''OutputColumn,
       ''TempTable,
       ''Deleted,
       ''DeleteOutput,
       ''Values,
       ''Delete,
       ''Insert,
       ''Merge,
       ''MergeUsing,
       ''MergeOn,
       ''MergeWhenMatched,
       ''MergeWhenNotMatched
     ]
     \name ->
       [d|
         deriving instance Generic $(conT name)

         instance Hashable $(conT name)

         instance Cacheable $(conT name)

         deriving instance Eq $(conT name)

         deriving instance Show $(conT name)

         deriving instance Data $(conT name)

         instance NFData $(conT name)
         |]
 )

$( fmap concat $ for
     [''TableName, ''ScalarType]
     \name -> [d|deriving instance Ord $(conT name)|]
 )

$( fmap concat $ for
     [''TableName, ''NullsOrder, ''Order]
     \name -> [d|deriving instance Lift $(conT name)|]
 )

--------------------------------------------------------------------------------
-- Third-party types

instance Cacheable ODBC.Value

instance Cacheable ODBC.Binary

--------------------------------------------------------------------------------
-- Debug instances

instance ToTxt ScalarType where
  toTxt = tshow -- TODO: include schema

instance ToTxt TableName where
  toTxt (TableName tableName (SchemaName tableSchema)) =
    if tableSchema == "dbo"
      then tableName
      else tableSchema <> "." <> tableName

instance ToTxt ColumnName where
  toTxt = columnNameText

$( fmap concat $ for
     [''Order, ''NullsOrder, ''ScalarType, ''FieldName]
     \name ->
       [d|
         instance ToJSON $(conT name) where
           toJSON = genericToJSON hasuraJSON

         instance FromJSON $(conT name) where
           parseJSON = genericParseJSON hasuraJSON
         |]
 )

deriving instance FromJSON ColumnName

deriving instance ToJSON ColumnName

instance FromJSON TableName where
  parseJSON v@(String _) =
    TableName <$> parseJSON v <*> pure "dbo"
  parseJSON (Object o) =
    TableName
      <$> o .: "name"
      <*> o .:? "schema" .!= "dbo"
  parseJSON _ =
    fail "expecting a string/object for TableName"

instance ToJSON TableName where
  toJSON = genericToJSON hasuraJSON

instance ToJSONKey TableName where
  toJSONKey = toJSONKeyText $ \(TableName name (SchemaName schema)) -> schema <> "." <> name

instance ToJSONKey ScalarType

-- NOTE!: an empty (default) instance declaration here caused a bug; instead
-- use standalone deriving via underlying Int instance
deriving newtype instance ToJSONKey ColumnName

deriving newtype instance FromJSONKey ColumnName

--------------------------------------------------------------------------------
-- Manual instances

deriving instance Generic (Countable n)

instance Hashable n => Hashable (Countable n)

instance Cacheable n => Cacheable (Countable n)

deriving instance Eq n => Eq (Countable n)

deriving instance Show n => Show (Countable n)

deriving instance Data n => Data (Countable n)

instance NFData n => NFData (Countable n)

instance ToJSON n => ToJSON (Countable n)

instance FromJSON n => FromJSON (Countable n)

deriving instance Ord ColumnName

deriving instance Monoid Where

deriving instance Semigroup Where

instance Monoid Top where
  mempty = NoTop

instance Semigroup Top where
  (<>) :: Top -> Top -> Top
  (<>) NoTop x = x
  (<>) x NoTop = x
  (<>) (Top x) (Top y) = Top (min x y)

deriving instance Generic (BooleanOperators a)

deriving instance Functor BooleanOperators

deriving instance Foldable BooleanOperators

deriving instance Traversable BooleanOperators

deriving instance Show a => Show (BooleanOperators a)

deriving instance Eq a => Eq (BooleanOperators a)

instance NFData a => NFData (BooleanOperators a)

instance Hashable a => Hashable (BooleanOperators a)

instance Cacheable a => Cacheable (BooleanOperators a)

instance ToJSON a => ToJSONKeyValue (BooleanOperators a) where
  toJSONKeyValue = \case
    ASTContains a -> ("_st_contains", toJSON a)
    ASTCrosses a -> ("_st_crosses", toJSON a)
    ASTEquals a -> ("_st_equals", toJSON a)
    ASTIntersects a -> ("_st_intersects", toJSON a)
    ASTOverlaps a -> ("_st_overlaps", toJSON a)
    ASTTouches a -> ("_st_touches", toJSON a)
    ASTWithin a -> ("_st_within", toJSON a)
