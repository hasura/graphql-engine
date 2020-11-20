{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances that're slow to compile.

module Hasura.Backends.MSSQL.Types.Instances where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Data
import           Data.Function
import           Data.Hashable
import qualified Data.Text as T
import           Data.Text.Extended (ToTxt(..))
import qualified Database.ODBC.SQLServer as Odbc
import           GHC.Generics
import           Hasura.Backends.MSSQL.Types.Internal
import           Hasura.Incremental.Internal.Dependency
import           Hasura.Prelude
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

instance FromJSONKey UserTableName

$(fmap concat $ for [ ''UserMetadata
                    , ''UserTableMetadata
                    , ''UserTableName
                    , ''UserObjectRelationship
                    , ''UserArrayRelationship
                    , ''UserUsing
                    , ''UserOn
                    , ''CatalogMetadata
                    , ''CatalogTableMetadata
                    , ''CatalogTableName
                    , ''CatalogObjectRelationship
                    , ''CatalogArrayRelationship
                    , ''CatalogUsing
                    , ''CatalogOn
                    , ''CatalogColumn
                    ]
  \name -> [d|
  deriving instance Generic $(conT name)
  instance Hashable $(conT name)
  instance Cacheable $(conT name)
  deriving instance Eq $(conT name)
  deriving instance Show $(conT name)
  deriving instance Data $(conT name)
  instance FromJSON $(conT name)
  |])

$(fmap concat $ for [ ''Where
                    , ''For
                    , ''Aggregate
                    , ''Countable
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

-- Why are these needed by the Backend class?
$(fmap concat $ for [''Order, ''NullsOrder, ''Countable, ''ColumnName, ''ScalarType, ''FieldName, ''TableName]
  \name -> [d|
  instance ToJSON $(conT name)
  instance FromJSON $(conT name) |])

instance ToJSONKey ColumnName
instance FromJSONKey ColumnName

$(fmap concat $ for [''TableName, ''ScalarType]
  \name -> [d|deriving instance Ord $(conT name) |])

$(fmap concat $ for [''TableName, ''NullsOrder, ''Order]
  \name -> [d|deriving instance Lift $(conT name) |])

--------------------------------------------------------------------------------
-- Third-party types

instance Cacheable Odbc.Value
instance Cacheable Odbc.Binary

--------------------------------------------------------------------------------
-- Manual instances

instance ToTxt ScalarType where
  toTxt = T.pack . show -- TODO:

instance ToTxt TableName where
  toTxt = T.pack . show -- TODO:

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
