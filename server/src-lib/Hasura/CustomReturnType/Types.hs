-- | A name for a custom return type as it is recognized by the graphql schema.
module Hasura.CustomReturnType.Types
  ( CustomReturnTypeName (..),
    CustomReturnTypeField (..),
    customReturnTypeFieldMapCodec,
  )
where

import Autodocodec
  ( HasCodec (codec),
    dimapCodec,
  )
import Autodocodec qualified as AC
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, Value, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.Text.Extended (ToTxt)
import Hasura.Metadata.DTO.Placeholder (placeholderCodecViaJSON)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax (Lift)

-- The name of a custom return type. This appears as a root field name in the graphql schema.
newtype CustomReturnTypeName = CustomReturnTypeName {getCustomReturnTypeName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Data, Generic, Lift)

instance HasCodec CustomReturnTypeName where
  codec = dimapCodec CustomReturnTypeName getCustomReturnTypeName codec

instance FromJSONKey CustomReturnTypeName

instance ToJSONKey CustomReturnTypeName

----

-- | either a scalar type or a reference to another custom return type
data CustomReturnTypeField b
  = CustomReturnTypeScalarField
      { crtfName :: Column b,
        crtfType :: ScalarType b, -- int, string, blah
        crtfNullable :: Bool,
        crtfDescription :: Maybe Text
      }
  | CustomReturnTypeArrayReference
      { crtfName :: Column b,
        crtfCustomReturnType :: CustomReturnTypeName -- name of another custom return type
      }
  deriving (Generic)

instance (Backend b) => HasCodec (CustomReturnTypeField b) where
  codec =
    AC.CommentCodec
      ("A scalar type or reference to another custom return type")
      $ placeholderCodecViaJSON

instance (Backend b) => FromJSON (CustomReturnTypeField b) where
  parseJSON = withObject "CustomReturnTypeField" \o ->
    parseReference o <|> parseScalar o
    where
      parseScalar obj = do
        crtfName <- obj .: "name"
        crtfType <- obj .: "type"
        crtfNullable <- obj .:? "nullable" .!= False
        crtfDescription <- obj .:? "description"
        pure (CustomReturnTypeScalarField {..})
      parseReference obj = do
        crtfCustomReturnType <- obj .: "custom_return_type"
        crtfName <- obj .: "name"
        pure (CustomReturnTypeArrayReference {..})

instance (Backend b) => ToJSON (CustomReturnTypeField b) where
  toJSON (CustomReturnTypeScalarField {..}) =
    object $
      [ "name" .= crtfName,
        "type" .= crtfType,
        "nullable" .= crtfNullable
      ]
        <> maybeDescription
    where
      maybeDescription = case crtfDescription of
        Just desc -> ["description" .= desc]
        Nothing -> []
  toJSON (CustomReturnTypeArrayReference {..}) =
    object ["name" .= crtfName, "custom_return_type" .= crtfCustomReturnType]

deriving stock instance (Backend b) => Eq (CustomReturnTypeField b)

deriving stock instance (Backend b) => Show (CustomReturnTypeField b)

instance (Backend b) => Hashable (CustomReturnTypeField b)

instance (Backend b) => NFData (CustomReturnTypeField b)

-- we parse in as an array of NullableScalarTypeFromArray and then turn into
-- InsOrdHashMap because JSON objects cannot be depended on for ordering
customReturnTypeFieldMapCodec ::
  forall b.
  (Backend b) =>
  AC.Codec
    Value
    (InsOrd.InsOrdHashMap (Column b) (CustomReturnTypeField b))
    (InsOrd.InsOrdHashMap (Column b) (CustomReturnTypeField b))
customReturnTypeFieldMapCodec =
  AC.dimapCodec
    ( InsOrd.fromList
        . fmap
          ( \crtf -> (crtfName crtf, crtf)
          )
    )
    ( fmap snd . InsOrd.toList
    )
    (AC.codec @[CustomReturnTypeField b])
