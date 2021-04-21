{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances that're slow to compile.

module Hasura.Backends.MSSQL.Types.Instances where

import           Hasura.Prelude

import qualified Database.ODBC.SQLServer                as ODBC

import           Data.Aeson.Extended
import           Data.Aeson.Types
import           Data.Text.Extended                     (ToTxt (..))
import           Hasura.Backends.MSSQL.Types.Internal
import           Hasura.Incremental.Internal.Dependency
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax


$(fmap concat $ for [''Aliased]
  \name -> [d|
  deriving instance Generic ($(conT name) a)
  instance Hashable a => Hashable ($(conT name) a)
  instance Cacheable a => Cacheable ($(conT name) a)
  deriving instance Eq a => Eq ($(conT name) a)
  instance NFData a => NFData ($(conT name) a)
  deriving instance Show a => Show ($(conT name) a)
  deriving instance Functor $(conT name)
  deriving instance Data a => Data ($(conT name) a)
  |])

$(fmap concat $ for [ ''UnifiedTableName
                    , ''UnifiedObjectRelationship
                    , ''UnifiedArrayRelationship
                    , ''UnifiedUsing
                    , ''UnifiedOn
                    , ''UnifiedColumn
                    ]
  \name -> [d|
  deriving instance Generic $(conT name)
  instance Hashable $(conT name)
  instance Cacheable $(conT name)
  deriving instance Eq $(conT name)
  deriving instance Show $(conT name)
  deriving instance Data $(conT name)
  instance FromJSON $(conT name)
  deriving instance Ord $(conT name)
  |])

$(fmap concat $ for [ ''Where
                    , ''For
                    , ''Aggregate
                    , ''EntityAlias
                    , ''ForJson
                    , ''JsonCardinality
                    , ''Root
                    , ''OrderBy
                    , ''JoinAlias
                    , ''Reselect
                    , ''ColumnName
                    , ''Expression
                    , ''NullsOrder
                    , ''Order
                    , ''ScalarType
                    , ''TableName
                    , ''Select
                    , ''Top
                    , ''FieldName
                    , ''JsonPath
                    , ''Op
                    , ''SpatialOp
                    , ''Projection
                    , ''From
                    , ''OpenJson
                    , ''JsonFieldSpec
                    , ''Join
                    , ''JoinSource
                    ]
  \name -> [d|
  deriving instance Generic $(conT name)
  instance Hashable $(conT name)
  instance Cacheable $(conT name)
  deriving instance Eq $(conT name)
  deriving instance Show $(conT name)
  deriving instance Data $(conT name)
  instance NFData $(conT name)
  |])


$(fmap concat $ for [''TableName, ''ScalarType]
  \name -> [d|deriving instance Ord $(conT name) |])

$(fmap concat $ for [''TableName, ''NullsOrder, ''Order]
  \name -> [d|deriving instance Lift $(conT name) |])


--------------------------------------------------------------------------------
-- Third-party types

instance Cacheable ODBC.Value
instance Cacheable ODBC.Binary


--------------------------------------------------------------------------------
-- Debug instances

instance ToTxt ScalarType where
  toTxt = tshow -- TODO: include schema

instance ToTxt TableName where
  toTxt TableName { tableName, tableSchema } =
    if tableSchema == "dbo"
      then tableName
      else tableSchema <> "." <> tableName

instance ToTxt ColumnName where
  toTxt = columnNameText

$(fmap concat $ for [''Order, ''NullsOrder, ''ScalarType, ''FieldName]
  \name -> [d|
  instance ToJSON $(conT name) where
    toJSON = genericToJSON hasuraJSON
  instance FromJSON $(conT name) where
    parseJSON = genericParseJSON hasuraJSON |])

deriving instance FromJSON ColumnName
deriving instance ToJSON ColumnName

instance FromJSON TableName where
  parseJSON v@(String _) =
    TableName <$> parseJSON v <*> pure "dbo"
  parseJSON (Object o) =
    TableName <$>
    o .: "name" <*>
    o .:? "schema" .!= "dbo"
  parseJSON _ =
    fail "expecting a string/object for TableName"

instance ToJSON TableName where
  toJSON = genericToJSON hasuraJSON

instance ToJSONKey TableName where
  toJSONKey = toJSONKeyText $ \(TableName schema name) -> schema <> "." <> name

instance ToJSONKey ScalarType

-- NOTE!: an empty (default) instance declaration here caused a bug; instead
-- use standalone deriving via underlying Int instance
deriving newtype instance ToJSONKey ColumnName
deriving newtype instance FromJSONKey ColumnName

instance Arbitrary ColumnName where
  arbitrary = genericArbitrary

instance Arbitrary TableName where
  arbitrary = genericArbitrary

instance ToTxt () where
  toTxt = tshow


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

instance Monoid Where where
  mempty = Where mempty

instance Semigroup Where where
  (Where x) <> (Where y) = Where (x <> y)

instance Monoid Top where
  mempty = NoTop

instance Semigroup Top where
  (<>) :: Top -> Top -> Top
  (<>) NoTop x         = x
  (<>) x NoTop         = x
  (<>) (Top x) (Top y) = Top (min x y)


deriving instance Generic (BooleanOperators a)
deriving instance Functor     BooleanOperators
deriving instance Foldable    BooleanOperators
deriving instance Traversable BooleanOperators
deriving instance Show      a => Show      (BooleanOperators a)
deriving instance Eq        a => Eq        (BooleanOperators a)
instance          NFData    a => NFData    (BooleanOperators a)
instance          Hashable  a => Hashable  (BooleanOperators a)
instance          Cacheable a => Cacheable (BooleanOperators a)

instance ToJSON a => ToJSONKeyValue (BooleanOperators a) where
  toJSONKeyValue = \case
    ASTContains    a -> ("_st_contains",   toJSON a)
    ASTCrosses     a -> ("_st_crosses",    toJSON a)
    ASTEquals      a -> ("_st_equals",     toJSON a)
    ASTIntersects  a -> ("_st_intersects", toJSON a)
    ASTOverlaps    a -> ("_st_overlaps",   toJSON a)
    ASTTouches     a -> ("_st_touches",    toJSON a)
    ASTWithin      a -> ("_st_within",     toJSON a)
