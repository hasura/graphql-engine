{-# LANGUAGE UndecidableInstances #-}

module Hasura.CustomReturnType
  ( CustomReturnType (..),
    CustomColumn (..),
  )
where

import Autodocodec (Autodocodec (Autodocodec), HasCodec)
import Autodocodec qualified as AC
import Data.Aeson (ToJSON)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.SQL.Backend (BackendType)

-- | Description of a custom return type for a Logical Model
data CustomReturnType (b :: BackendType) = CustomReturnType
  { crtColumns :: HashMap (Column b) (CustomColumn b),
    crtDescription :: Maybe Text
  }
  deriving (Generic)

instance (Backend b) => HasCodec (CustomReturnType b) where
  codec =
    AC.CommentCodec
      ("A return type for a logical model.")
      $ AC.object (codecNamePrefix @b <> "CustomReturnType")
      $ CustomReturnType
        <$> AC.requiredField "columns" columnsDoc
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

data CustomColumn b = CustomColumn
  { ccType :: ScalarType b,
    ccNullable :: Bool,
    ccDescription :: Maybe Text
  }
  deriving (Generic)

instance (Backend b) => HasCodec (CustomColumn b) where
  codec =
    AC.CommentCodec
      ("A column of a custom return type")
      $ AC.object (codecNamePrefix @b <> "CustomColumn")
      $ CustomColumn
        <$> AC.requiredField "type" columnDoc
          AC..= ccType
        <*> AC.optionalFieldWithDefault "nullable" False nullableDoc
          AC..= ccNullable
        <*> AC.optionalField "description" descriptionDoc
          AC..= ccDescription
    where
      columnDoc = "The type of the column"
      nullableDoc = "Whether the type is nullable"
      descriptionDoc = "Optional description text which appears in the GraphQL Schema"

deriving stock instance (Backend b) => Eq (CustomColumn b)

deriving stock instance (Backend b) => Show (CustomColumn b)

instance (Backend b) => Hashable (CustomColumn b)

instance (Backend b) => NFData (CustomColumn b)
