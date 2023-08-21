{-# LANGUAGE UndecidableInstances #-}

module Hasura.LogicalModel.Cache
  ( LogicalModelInfo (..),
    LogicalModelCache,
  )
where

import Data.Aeson (ToJSON (..), genericToJSON)
import Hasura.LogicalModel.Types (LogicalModelFields, LogicalModelName)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.Table.Cache (RolePermInfoMap)

type LogicalModelCache b = HashMap LogicalModelName (LogicalModelInfo b)

-- | Description of a logical model for use in metadata (after schema cache)
data LogicalModelInfo (b :: BackendType) = LogicalModelInfo
  { _lmiName :: LogicalModelName,
    _lmiFields :: LogicalModelFields b,
    _lmiDescription :: Maybe Text,
    _lmiPermissions :: RolePermInfoMap b
  }
  deriving (Show, Generic)

instance
  (Backend b, ToJSON (RolePermInfoMap b)) =>
  ToJSON (LogicalModelInfo b)
  where
  toJSON = genericToJSON hasuraJSON
