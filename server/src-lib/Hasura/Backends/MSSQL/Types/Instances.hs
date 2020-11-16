{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances that're slow to compile.

module Hasura.Backends.MSSQL.Types.Instances where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Function
import           Data.Hashable
import qualified Data.Text as T
import           Data.Text.Extended (ToTxt(..))
import qualified Database.ODBC.SQLServer as Odbc
import           Hasura.Backends.MSSQL.Types.Internal
import           Hasura.Incremental.Internal.Dependency
import           Prelude

instance Cacheable Odbc.Value
instance Cacheable Odbc.Binary

instance FromJSON ColumnName
instance FromJSON NullsOrder
instance FromJSON Order
instance FromJSON ScalarType
instance FromJSON TableName
instance FromJSONKey ColumnName

instance ToJSON ColumnName
instance ToJSON NullsOrder
instance ToJSON Order
instance ToJSON ScalarType
instance ToJSON TableName
instance ToJSONKey ColumnName

instance Hashable Where
instance Hashable For
instance Hashable Aggregate
instance Hashable Countable
instance Hashable ForJson
instance Hashable JsonCardinality
instance Hashable Root
instance Hashable OrderBy
instance Hashable JoinAlias
instance Hashable Reselect
instance Hashable ColumnName
instance Hashable Expression
instance Hashable NullsOrder
instance Hashable Order
instance Hashable ScalarType
instance Hashable TableName
instance Hashable Select
instance Hashable Top
instance Hashable FieldName
instance Hashable JsonPath
instance Hashable Op
instance Hashable Projection
instance Hashable a => Hashable (Aliased a)
instance Hashable From
instance Hashable OpenJson
instance Hashable JsonFieldSpec
instance Hashable Join
instance Hashable JoinSource

instance Cacheable Where
instance Cacheable For
instance Cacheable Aggregate
instance Cacheable Countable
instance Cacheable ForJson
instance Cacheable JsonCardinality
instance Cacheable Root
instance Cacheable OrderBy
instance Cacheable JoinAlias
instance Cacheable Reselect
instance Cacheable ColumnName
instance Cacheable Expression
instance Cacheable NullsOrder
instance Cacheable Order
instance Cacheable ScalarType
instance Cacheable TableName
instance Cacheable Select
instance Cacheable Top
instance Cacheable FieldName
instance Cacheable JsonPath
instance Cacheable Op
instance Cacheable Projection
instance Cacheable a => Cacheable (Aliased a)
instance Cacheable From
instance Cacheable OpenJson
instance Cacheable JsonFieldSpec
instance Cacheable Join
instance Cacheable JoinSource

instance NFData Where
instance NFData For
instance NFData Aggregate
instance NFData Countable
instance NFData ForJson
instance NFData JsonCardinality
instance NFData Root
instance NFData OrderBy
instance NFData JoinAlias
instance NFData Reselect
instance NFData ColumnName
instance NFData Expression
instance NFData NullsOrder
instance NFData Order
instance NFData ScalarType
instance NFData TableName
instance NFData Select
instance NFData Top
instance NFData FieldName
instance NFData JsonPath
instance NFData Op
instance NFData Projection
instance NFData a => NFData (Aliased a)
instance NFData From
instance NFData OpenJson
instance NFData JsonFieldSpec
instance NFData Join
instance NFData JoinSource

instance ToTxt ScalarType where toTxt = T.pack . show -- TODO:
instance ToTxt TableName where toTxt = T.pack . show -- TODO:
