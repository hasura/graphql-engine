{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The representation of logical models as derived from the schema cache.
module Hasura.LogicalModel.Cache
  ( LogicalModelInfo (..),
    LogicalModelCache,
    lmiRootFieldName,
    lmiCode,
    lmiReturns,
    lmiArguments,
    lmiDescription,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON (toJSON), genericToJSON)
import Hasura.CustomReturnType.Cache (CustomReturnTypeInfo)
import Hasura.LogicalModel.Metadata (InterpolatedQuery, LogicalModelArgumentName, LogicalModelName)
import Hasura.LogicalModel.Types (NullableScalarType)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Table (RolePermInfoMap)
import Hasura.SQL.Backend (BackendType)

type LogicalModelCache b = HashMap LogicalModelName (LogicalModelInfo b)

-- | The type into which 'LogicalModelMetadata' is resolved in
-- 'Hasura/RQL/DDL/Schema/Cache.buildSchemaCacheRule'.
data LogicalModelInfo (b :: BackendType) = LogicalModelInfo
  { _lmiRootFieldName :: LogicalModelName,
    _lmiCode :: InterpolatedQuery LogicalModelArgumentName,
    _lmiReturns :: CustomReturnTypeInfo b,
    _lmiArguments :: HashMap LogicalModelArgumentName (NullableScalarType b),
    _lmiDescription :: Maybe Text
  }
  deriving stock (Generic)

instance
  (Backend b, ToJSON (RolePermInfoMap b)) =>
  ToJSON (LogicalModelInfo b)
  where
  toJSON = genericToJSON hasuraJSON

makeLenses ''LogicalModelInfo
