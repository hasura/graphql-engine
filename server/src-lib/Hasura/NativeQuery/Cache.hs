{-# LANGUAGE UndecidableInstances #-}

-- | The representation of native queries as derived from the schema cache.
module Hasura.NativeQuery.Cache
  ( NativeQueryInfo (..),
    NativeQueryCache,
  )
where

import Data.Aeson (ToJSON (toJSON), genericToJSON)
import Hasura.LogicalModel.Cache (LogicalModelInfo)
import Hasura.NativeQuery.Metadata (ArgumentName, InterpolatedQuery, NativeQueryName)
import Hasura.NativeQuery.Types (NullableScalarType)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Common (RelName)
import Hasura.RQL.Types.Relationships.Local (RelInfo)
import Hasura.Table.Cache (RolePermInfoMap)

type NativeQueryCache b = HashMap NativeQueryName (NativeQueryInfo b)

-- | The type into which 'NativeQueryMetadata' is resolved in
-- 'Hasura/RQL/DDL/Schema/Cache.buildSchemaCacheRule'.
data NativeQueryInfo (b :: BackendType) = NativeQueryInfo
  { _nqiRootFieldName :: NativeQueryName,
    _nqiCode :: InterpolatedQuery ArgumentName,
    _nqiReturns :: LogicalModelInfo b,
    _nqiArguments :: HashMap ArgumentName (NullableScalarType b),
    _nqiRelationships :: InsOrdHashMap RelName (RelInfo b),
    _nqiDescription :: Maybe Text
  }
  deriving stock (Show, Generic)

instance
  (Backend b, ToJSON (RolePermInfoMap b)) =>
  ToJSON (NativeQueryInfo b)
  where
  toJSON = genericToJSON hasuraJSON
