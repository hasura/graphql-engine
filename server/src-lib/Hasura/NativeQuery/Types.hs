-- | A name for a native query as it is recognized by the graphql schema.
module Hasura.NativeQuery.Types
  ( NativeQueryName (..),
    NullableScalarType (..),
    nullableScalarTypeMapCodec,
  )
where

import Autodocodec (Autodocodec (Autodocodec), HasCodec (codec), HasObjectCodec (..), bimapCodec, dimapCodec)
import Autodocodec qualified as AC
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, Value)
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.Text.Extended (ToTxt)
import Hasura.Metadata.DTO.Utils (codecNamePrefix)
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax (Lift)

-- The name of a native query. This appears as a root field name in the graphql schema.
newtype NativeQueryName = NativeQueryName {getNativeQueryName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Data, Generic, Lift)

instance HasCodec NativeQueryName where
  codec = dimapCodec NativeQueryName getNativeQueryName codec

instance FromJSONKey NativeQueryName

instance ToJSONKey NativeQueryName

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
      $ AC.objectCodec

instance (Backend b) => HasObjectCodec (NullableScalarType b) where
  objectCodec =
    NullableScalarType
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

-----

data MergedObject a b = MergedObject
  { moFst :: a,
    moSnd :: b
  }

instance (HasObjectCodec a, HasObjectCodec b) => HasObjectCodec (MergedObject a b) where
  objectCodec = MergedObject <$> bimapCodec Right moFst objectCodec <*> bimapCodec Right moSnd objectCodec

newtype NameField a = NameField {nameField :: a}

instance (HasCodec a) => HasObjectCodec (NameField a) where
  objectCodec = NameField <$> AC.requiredField "name" "name" AC..= nameField

-- we parse in as an array of NullableScalarTypeFromArray and then turn into
-- InsOrdHashMap because JSON objects cannot be depended on for ordering
nullableScalarTypeMapCodec ::
  forall b.
  (Backend b) =>
  AC.Codec
    Value
    (InsOrd.InsOrdHashMap (Column b) (NullableScalarType b))
    (InsOrd.InsOrdHashMap (Column b) (NullableScalarType b))
nullableScalarTypeMapCodec =
  AC.dimapCodec
    ( InsOrd.fromList
        . fmap
          ( \(MergedObject (NameField name) nst) ->
              (name, nst)
          )
    )
    ( fmap (\(fld, nst) -> MergedObject (NameField fld) nst) . InsOrd.toList
    )
    ( AC.listCodec $
        AC.object "NullableScalarType" $
          AC.objectCodec @(MergedObject (NameField (Column b)) (NullableScalarType b))
    )
