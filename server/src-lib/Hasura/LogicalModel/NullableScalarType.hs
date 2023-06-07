-- | A scalar type to be used for logical models and resolvers.
module Hasura.LogicalModel.NullableScalarType
  ( NullableScalarType (..),
    nullableScalarTypeMapCodec,
  )
where

import Autodocodec (Autodocodec (Autodocodec), HasCodec (codec), HasObjectCodec (..), bimapCodec)
import Autodocodec qualified as AC
import Data.Aeson (ToJSON, Value)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.BackendTag (backendPrefix)

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
      $ AC.object (backendPrefix @b <> "NullableScalarType")
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
    (Backend b) => ToJSON (NullableScalarType b)

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
    (InsOrdHashMap.InsOrdHashMap (Column b) (NullableScalarType b))
    (InsOrdHashMap.InsOrdHashMap (Column b) (NullableScalarType b))
nullableScalarTypeMapCodec =
  AC.dimapCodec
    ( InsOrdHashMap.fromList
        . fmap
          ( \(MergedObject (NameField name) nst) ->
              (name, nst)
          )
    )
    ( fmap (\(fld, nst) -> MergedObject (NameField fld) nst) . InsOrdHashMap.toList
    )
    ( AC.listCodec
        $ AC.object "NullableScalarType"
        $ AC.objectCodec @(MergedObject (NameField (Column b)) (NullableScalarType b))
    )
