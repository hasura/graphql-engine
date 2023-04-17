{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.CustomReturnType.Cache
  ( CustomReturnTypeInfo (..),
    CustomReturnTypeCache,
    crtiName,
    crtiPermissions,
    crtiDescription,
    crtiFields,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Hasura.CustomReturnType.Types (CustomReturnTypeField, CustomReturnTypeName)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Table (RolePermInfoMap)
import Hasura.SQL.Backend (BackendType)

type CustomReturnTypeCache b = HashMap CustomReturnTypeName (CustomReturnTypeInfo b)

-- | Description of a custom return type for use in metadata (after schema cache)
data CustomReturnTypeInfo (b :: BackendType) = CustomReturnTypeInfo
  { _crtiName :: CustomReturnTypeName,
    _crtiFields :: InsOrd.InsOrdHashMap (Column b) (CustomReturnTypeField b),
    _crtiDescription :: Maybe Text,
    _crtiPermissions :: RolePermInfoMap b
  }
  deriving (Generic)

makeLenses ''CustomReturnTypeInfo

instance
  (Backend b, ToJSON (RolePermInfoMap b)) =>
  ToJSON (CustomReturnTypeInfo b)
  where
  toJSON = genericToJSON hasuraJSON
