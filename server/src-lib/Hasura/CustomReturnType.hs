{-# LANGUAGE UndecidableInstances #-}

module Hasura.CustomReturnType
  ( CustomReturnType (..),
  )
where

import Autodocodec (Autodocodec (Autodocodec), HasCodec)
import Autodocodec qualified as AC
import Data.Aeson (ToJSON)
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Hasura.LogicalModel.Types (NullableScalarType (..), nullableScalarTypeMapCodec)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.SQL.Backend (BackendType)

-- | Description of a custom return type for a Logical Model
data CustomReturnType (b :: BackendType) = CustomReturnType
  { crtColumns :: InsOrd.InsOrdHashMap (Column b) (NullableScalarType b),
    crtDescription :: Maybe Text
  }
  deriving (Generic)

instance (Backend b) => HasCodec (CustomReturnType b) where
  codec =
    AC.CommentCodec
      ("A return type for a logical model.")
      $ AC.object (codecNamePrefix @b <> "CustomReturnType")
      $ CustomReturnType
        <$> AC.requiredFieldWith "columns" nullableScalarTypeMapCodec columnsDoc
          AC..= crtColumns
        <*> AC.optionalField "description" descriptionDoc
          AC..= crtDescription
    where
      columnsDoc = "Return types for the logical model"
      descriptionDoc = "Optional description text which appears in the GraphQL Schema."

deriving via
  (Autodocodec (CustomReturnType b))
  instance
    Backend b => ToJSON (CustomReturnType b)

deriving stock instance (Backend b) => Eq (CustomReturnType b)

deriving stock instance (Backend b) => Show (CustomReturnType b)

instance (Backend b) => Hashable (CustomReturnType b)

instance (Backend b) => NFData (CustomReturnType b)
