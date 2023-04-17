{-# LANGUAGE UndecidableInstances #-}

module Hasura.CustomReturnType.IR
  ( CustomReturnType (..),
  )
where

import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Hasura.CustomReturnType.Types (CustomReturnTypeField, CustomReturnTypeName)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.SQL.Backend (BackendType)

-- | Description of a custom return type for use in IR
data CustomReturnType (b :: BackendType) = CustomReturnType
  { crtName :: CustomReturnTypeName,
    crtFields :: InsOrd.InsOrdHashMap (Column b) (CustomReturnTypeField b)
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (CustomReturnType b)

deriving instance (Backend b) => Show (CustomReturnType b)
