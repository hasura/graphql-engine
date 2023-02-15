{-# LANGUAGE UndecidableInstances #-}

module Hasura.CustomReturnType
  ( CustomReturnType (..),
  )
where

import Autodocodec (HasCodec, requiredField)
import Autodocodec qualified as AC
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.SQL.Backend (BackendType)

-- | Description of a custom return type for a Native Query
newtype CustomReturnType (b :: BackendType) = CustomReturnType
  { crtColumns :: HashMap (Column b) (ScalarType b)
  }

instance (Backend b) => HasCodec (CustomReturnType b) where
  codec =
    AC.CommentCodec
      ("A return type for a native query.")
      $ AC.object (codecNamePrefix @b <> "CustomReturnType")
      $ CustomReturnType
        <$> requiredField "columns" columnsDoc
          AC..= crtColumns
    where
      columnsDoc = "Return types for the native query"

deriving stock instance (Backend b) => Eq (CustomReturnType b)

deriving stock instance (Backend b) => Show (CustomReturnType b)

deriving newtype instance (Backend b) => Hashable (CustomReturnType b)

deriving newtype instance (Backend b) => NFData (CustomReturnType b)
