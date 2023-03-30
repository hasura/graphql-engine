{-# LANGUAGE UndecidableInstances #-}

module Hasura.CustomReturnType.Metadata
  ( CustomReturnTypeMetadata (..),
    CustomReturnTypeName (..),
  )
where

import Autodocodec (Autodocodec (Autodocodec), HasCodec)
import Autodocodec qualified as AC
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Hasura.CustomReturnType.Types
import Hasura.LogicalModel.Types (NullableScalarType (..), nullableScalarTypeMapCodec)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Permission (SelPermDef, _pdRole)
import Hasura.SQL.Backend (BackendType)
import Hasura.Session (RoleName)

-- | Description of a custom return type for use in metadata (before schema cache)
data CustomReturnTypeMetadata (b :: BackendType) = CustomReturnTypeMetadata
  { _ctmName :: CustomReturnTypeName,
    _ctmFields :: InsOrd.InsOrdHashMap (Column b) (NullableScalarType b),
    _ctmDescription :: Maybe Text,
    _ctmSelectPermissions :: InsOrdHashMap RoleName (SelPermDef b)
  }
  deriving (Generic)

instance (Backend b) => HasCodec (CustomReturnTypeMetadata b) where
  codec =
    AC.CommentCodec
      ("A return type.")
      $ AC.object (codecNamePrefix @b <> "CustomReturnTypeMetadata")
      $ CustomReturnTypeMetadata
        <$> AC.requiredField "name" nameDoc
          AC..= _ctmName
        <*> AC.requiredFieldWith "fields" nullableScalarTypeMapCodec fieldsDoc
          AC..= _ctmFields
        <*> AC.optionalField "description" descriptionDoc
          AC..= _ctmDescription
        <*> optSortedList "select_permissions" _pdRole
          AC..= _ctmSelectPermissions
    where
      nameDoc = "A name for a custom return type"
      fieldsDoc = "Return types for the custom return type"
      descriptionDoc = "Optional description text which appears in the GraphQL Schema."

      optSortedList name keyForElem =
        AC.optionalFieldWithOmittedDefaultWith' name (sortedElemsCodec keyForElem) mempty

deriving via
  (Autodocodec (CustomReturnTypeMetadata b))
  instance
    Backend b => FromJSON (CustomReturnTypeMetadata b)

deriving via
  (Autodocodec (CustomReturnTypeMetadata b))
  instance
    Backend b => ToJSON (CustomReturnTypeMetadata b)

deriving stock instance (Backend b) => Eq (CustomReturnTypeMetadata b)

deriving stock instance (Backend b) => Show (CustomReturnTypeMetadata b)
