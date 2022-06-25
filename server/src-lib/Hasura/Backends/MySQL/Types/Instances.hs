{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances that're slow to compile.
module Hasura.Backends.MySQL.Types.Instances () where

import Control.DeepSeq
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Extended
import Data.Aeson.TH qualified as J
import Data.Aeson.Types
import Data.Pool
import Data.Text.Extended (ToTxt (..))
import Database.MySQL.Base (Connection)
import Database.MySQL.Base.Types qualified as MySQLTypes (Type (..))
import Hasura.Backends.MySQL.Types.Internal
import Hasura.Incremental.Internal.Dependency
import Hasura.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

----
---- Countable instances

-- These instances must be defined before the TH-defined instances below.

deriving instance Generic (Countable n)

instance Hashable n => Hashable (Countable n)

instance Cacheable n => Cacheable (Countable n)

deriving instance Eq n => Eq (Countable n)

deriving instance Show n => Show (Countable n)

deriving instance Data n => Data (Countable n)

instance NFData n => NFData (Countable n)

instance ToJSON n => ToJSON (Countable n)

instance FromJSON n => FromJSON (Countable n)

----
---- TH-defined instances

$( concat <$> for
     [ ''ScalarType
     ]
     \name ->
       [d|
         deriving instance Generic $(conT name)

         instance Hashable $(conT name)

         instance Cacheable $(conT name)

         deriving instance Data $(conT name)

         instance NFData $(conT name)
         |]
 )

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

$( concat <$> for
     [ ''Where,
       ''Aggregate,
       ''EntityAlias,
       ''OrderBy,
       ''JoinAlias,
       ''Reselect,
       ''Expression,
       ''NullsOrder,
       ''Order,
       ''Top,
       ''TableName,
       ''Select,
       ''FieldName,
       ''FieldOrigin,
       ''Projection,
       ''From,
       ''Join,
       ''Op,
       ''JoinType
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

$( concat <$> for
     [''TableName, ''ScalarType]
     \name -> [d|deriving instance Ord $(conT name)|]
 )

$( concat <$> for
     [''TableName, ''NullsOrder, ''Order]
     \name -> [d|deriving instance Lift $(conT name)|]
 )

$( concat <$> for
     [''Order, ''NullsOrder, ''ScalarType, ''FieldName]
     \name ->
       [d|
         instance ToJSON $(conT name) where
           toJSON = genericToJSON hasuraJSON

         instance FromJSON $(conT name) where
           parseJSON = genericParseJSON hasuraJSON
         |]
 )

----
---- Manually-defined instances

instance ToTxt TableName where
  toTxt TableName {..} = name

instance FromJSON TableName where
  parseJSON v@(String _) =
    TableName <$> parseJSON v <*> pure Nothing
  parseJSON (Object o) =
    TableName
      <$> o .: "name"
      <*> o .:? "schema"
  parseJSON _ =
    fail "expecting a string/object for TableName"

instance ToJSON TableName where
  toJSON TableName {..} = object ["name" .= name, "schema" .= schema]

instance ToJSONKey TableName where
  toJSONKey =
    toJSONKeyText $ \(TableName {schema, name}) ->
      maybe "" (<> ".") schema <> name

instance ToJSONKey ScalarType

instance ToTxt ScalarType where
  toTxt = tshow

deriving newtype instance Monoid Where

deriving newtype instance Semigroup Where

instance Monoid Top where
  mempty = NoTop

instance Semigroup Top where
  (<>) :: Top -> Top -> Top
  (<>) NoTop x = x
  (<>) x NoTop = x
  (<>) (Top x) (Top y) = Top (min x y)

instance J.FromJSON ConnPoolSettings where
  parseJSON = J.withObject "MySQL pool settings" $ \o ->
    ConnPoolSettings
      <$> o J..:? "max_connections" J..!= _cscMaxConnections defaultConnPoolSettings
      <*> o J..:? "idle_timeout" J..!= _cscIdleTimeout defaultConnPoolSettings

$(J.deriveToJSON hasuraJSON ''ConnPoolSettings)

instance J.ToJSON Expression where
  toJSON (ValueExpression scalarValue) = J.toJSON scalarValue
  toJSON expr = error $ "ToJSON: not implemented" <> show expr -- https://github.com/hasura/graphql-engine-mono/issues/1951

instance J.FromJSON Expression where
  parseJSON value = ValueExpression <$> J.parseJSON value

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) {J.omitNothingFields = False} ''ConnSourceConfig)

instance J.ToJSON (Pool Connection) where
  toJSON = const (J.String "_REDACTED_")

instance Eq (Pool Connection) where
  _ == _ = True

instance Cacheable SourceConfig where
  unchanged _ = (==)

deriving instance Eq SourceConfig

deriving instance Generic SourceConfig

deriving instance J.ToJSON SourceConfig

deriving instance Cacheable ConnPoolSettings

deriving instance Cacheable ConnSourceConfig
