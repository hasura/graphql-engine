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
    lmiPermissions,
    lmiDescription,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON (toJSON), genericToJSON)
import Hasura.CustomReturnType (CustomReturnType)
import Hasura.LogicalModel.Metadata (InterpolatedQuery, LogicalModelArgumentName, LogicalModelName)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend, ScalarType)
import Hasura.RQL.Types.Table (RolePermInfoMap)
import Hasura.SQL.Backend (BackendType)

type LogicalModelCache b = HashMap LogicalModelName (LogicalModelInfo b)

-- | The type into which 'LogicalModelMetadata' is resolved in
-- 'Hasura/RQL/DDL/Schema/Cache.buildSchemaCacheRule'.
data LogicalModelInfo (b :: BackendType) = LogicalModelInfo
  { _lmiRootFieldName :: LogicalModelName,
    _lmiCode :: InterpolatedQuery LogicalModelArgumentName,
    _lmiReturns :: CustomReturnType b,
    _lmiArguments :: HashMap LogicalModelArgumentName (ScalarType b),
    _lmiPermissions :: RolePermInfoMap b,
    _lmiDescription :: Maybe Text
  }
  deriving stock (Generic)

instance
  (Backend b, ToJSON (RolePermInfoMap b)) =>
  ToJSON (LogicalModelInfo b)
  where
  toJSON = genericToJSON hasuraJSON

makeLenses ''LogicalModelInfo
