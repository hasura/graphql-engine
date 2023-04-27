{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.LogicalModel.Cache
  ( LogicalModelInfo (..),
    LogicalModelCache,
    lmiName,
    lmiPermissions,
    lmiDescription,
    lmiFields,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Hasura.LogicalModel.Types (LogicalModelField, LogicalModelName)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Table (RolePermInfoMap)

type LogicalModelCache b = HashMap LogicalModelName (LogicalModelInfo b)

-- | Description of a logical model for use in metadata (after schema cache)
data LogicalModelInfo (b :: BackendType) = LogicalModelInfo
  { _lmiName :: LogicalModelName,
    _lmiFields :: InsOrdHashMap.InsOrdHashMap (Column b) (LogicalModelField b),
    _lmiDescription :: Maybe Text,
    _lmiPermissions :: RolePermInfoMap b
  }
  deriving (Generic)

makeLenses ''LogicalModelInfo

instance
  (Backend b, ToJSON (RolePermInfoMap b)) =>
  ToJSON (LogicalModelInfo b)
  where
  toJSON = genericToJSON hasuraJSON
