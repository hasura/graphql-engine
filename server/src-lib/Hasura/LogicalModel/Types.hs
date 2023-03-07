-- | A name for a logical model as it is recognized by the graphql schema.
module Hasura.LogicalModel.Types
  ( LogicalModelName (..),
    NullableScalarType (..),
  )
where

import Autodocodec (Autodocodec (Autodocodec), HasCodec (codec), dimapCodec)
import Autodocodec qualified as AC
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text.Extended (ToTxt)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Language.GraphQL.Draft.Syntax qualified as G

-- The name of a logical model. This appears as a root field name in the graphql schema.
newtype LogicalModelName = LogicalModelName {getLogicalModelName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Generic)

instance HasCodec LogicalModelName where
  codec = dimapCodec LogicalModelName getLogicalModelName codec

instance FromJSONKey LogicalModelName

instance ToJSONKey LogicalModelName

-- | A ScalarType that can be nullable with an optional description
data NullableScalarType b = NullableScalarType
  { nstType :: ScalarType b,
    nstNullable :: Bool,
    nstDescription :: Maybe Text
  }
  deriving (Generic)

instance (Backend b) => HasCodec (NullableScalarType b) where
  codec =
    AC.CommentCodec
      ("A scalar type that can be nullable with an optional description")
      $ AC.object (codecNamePrefix @b <> "NullableScalarType")
      $ NullableScalarType
        <$> AC.requiredField "type" columnDoc
          AC..= nstType
        <*> AC.optionalFieldWithDefault "nullable" False nullableDoc
          AC..= nstNullable
        <*> AC.optionalField "description" descriptionDoc
          AC..= nstDescription
    where
      columnDoc = "The base scalar type"
      nullableDoc = "Whether the type is nullable"
      descriptionDoc = "Optional description text which appears in the GraphQL Schema"

deriving via
  (Autodocodec (NullableScalarType b))
  instance
    Backend b => ToJSON (NullableScalarType b)

deriving stock instance (Backend b) => Eq (NullableScalarType b)

deriving stock instance (Backend b) => Show (NullableScalarType b)

instance (Backend b) => Hashable (NullableScalarType b)

instance (Backend b) => NFData (NullableScalarType b)
