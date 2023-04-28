{-# LANGUAGE UndecidableInstances #-}

-- | The representation of stored procedures as derived from the schema cache.
module Hasura.StoredProcedure.Cache
  ( StoredProcedureInfo (..),
    StoredProcedureCache,
  )
where

import Data.Aeson (ToJSON (toJSON), genericToJSON)
import Hasura.LogicalModel.Cache (LogicalModelInfo)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Common (RelName)
import Hasura.RQL.Types.Relationships.Local (RelInfo)
import Hasura.RQL.Types.Table (RolePermInfoMap)
import Hasura.StoredProcedure.Metadata (InterpolatedQuery, StoredProcedureArgumentName, StoredProcedureName)
import Hasura.StoredProcedure.Types (NullableScalarType)

type StoredProcedureCache b = HashMap StoredProcedureName (StoredProcedureInfo b)

-- | The type into which 'StoredProcedureMetadata' is resolved in
-- 'Hasura/RQL/DDL/Schema/Cache.buildSchemaCacheRule'.
data StoredProcedureInfo (b :: BackendType) = StoredProcedureInfo
  { _spiRootFieldName :: StoredProcedureName,
    _spiCode :: InterpolatedQuery StoredProcedureArgumentName,
    _spiReturns :: LogicalModelInfo b,
    _spiArguments :: HashMap StoredProcedureArgumentName (NullableScalarType b),
    _spiArrayRelationships :: InsOrdHashMap RelName (RelInfo b),
    _spiDescription :: Maybe Text
  }
  deriving stock (Generic)

instance
  (Backend b, ToJSON (RolePermInfoMap b)) =>
  ToJSON (StoredProcedureInfo b)
  where
  toJSON = genericToJSON hasuraJSON
