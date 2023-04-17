{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.CustomReturnType.Metadata
  ( CustomReturnTypeMetadata (..),
    CustomReturnTypeName (..),
    WithCustomReturnType (..),
    crtmName,
    crtmFields,
    crtmDescription,
    crtmSelectPermissions,
  )
where

import Autodocodec (Autodocodec (Autodocodec), HasCodec)
import Autodocodec qualified as AC
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (parseJSON), ToJSON, (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.HashMap.Strict.InsOrd.Autodocodec (sortedElemsCodec)
import Hasura.CustomReturnType.Types
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Common (SourceName, ToAesonPairs (toAesonPairs), defaultSource)
import Hasura.RQL.Types.Permission (SelPermDef, _pdRole)
import Hasura.SQL.Backend (BackendType)
import Hasura.Session (RoleName)

-- | Description of a custom return type for use in metadata (before schema cache)
data CustomReturnTypeMetadata (b :: BackendType) = CustomReturnTypeMetadata
  { _crtmName :: CustomReturnTypeName,
    _crtmFields :: InsOrd.InsOrdHashMap (Column b) (CustomReturnTypeField b),
    _crtmDescription :: Maybe Text,
    _crtmSelectPermissions :: InsOrdHashMap RoleName (SelPermDef b)
  }
  deriving (Generic)

makeLenses ''CustomReturnTypeMetadata

instance (Backend b) => HasCodec (CustomReturnTypeMetadata b) where
  codec =
    AC.CommentCodec
      ("A return type.")
      $ AC.object (codecNamePrefix @b <> "CustomReturnTypeMetadata")
      $ CustomReturnTypeMetadata
        <$> AC.requiredField "name" nameDoc
          AC..= _crtmName
        <*> AC.requiredFieldWith "fields" customReturnTypeFieldMapCodec fieldsDoc
          AC..= _crtmFields
        <*> AC.optionalField "description" descriptionDoc
          AC..= _crtmDescription
        <*> optSortedList "select_permissions" _pdRole
          AC..= _crtmSelectPermissions
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

-- | A wrapper to tie something to a particular native query. Specifically, it
-- assumes the underlying '_wlmInfo' is represented as an object, and adds two
-- keys to that object: @source@ and @root_field_name@.
data WithCustomReturnType a = WithCustomReturnType
  { _wcrtSource :: SourceName,
    _wcrtName :: CustomReturnTypeName,
    _wcrtInfo :: a
  }
  deriving stock (Eq, Show)

-- | something to note here: if the `a` contains a `name` or `source` key then
-- this won't work anymore.
instance (FromJSON a) => FromJSON (WithCustomReturnType a) where
  parseJSON = Aeson.withObject "CustomReturnType" \obj -> do
    _wcrtSource <- obj .:? "source" .!= defaultSource
    _wcrtName <- obj .: "name"
    _wcrtInfo <- parseJSON (Aeson.Object obj)

    pure WithCustomReturnType {..}

instance (ToAesonPairs a) => ToJSON (WithCustomReturnType a) where
  toJSON (WithCustomReturnType source name info) =
    Aeson.object $ ("source", Aeson.toJSON source) : ("name", Aeson.toJSON name) : toAesonPairs info
