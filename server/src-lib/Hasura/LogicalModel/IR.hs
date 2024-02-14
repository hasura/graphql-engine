{-# LANGUAGE UndecidableInstances #-}

module Hasura.LogicalModel.IR
  ( LogicalModel (..),
  )
where

import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Hasura.LogicalModel.Types (LogicalModelField, LogicalModelName)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.BackendType (BackendType)

-- | Description of a logical model for use in IR
data LogicalModel (b :: BackendType) = LogicalModel
  { lmName :: LogicalModelName,
    lmFields :: InsOrdHashMap.InsOrdHashMap (Column b) (LogicalModelField b)
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (LogicalModel b)

deriving instance (Backend b) => Show (LogicalModel b)

instance (Backend b) => ToJSON (LogicalModel b) where
  toEncoding = genericToEncoding defaultOptions
