-- | Common codecs shared between similar logical model resolvers.
module Hasura.LogicalModelResolver.Codec
  ( nativeQueryRelationshipsCodec,
  )
where

import Autodocodec (HasCodec (), HasObjectCodec (..), bimapCodec)
import Autodocodec qualified as AC
import Data.Aeson (Value)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Common (RelName)
import Hasura.RQL.Types.Relationships.Local (RelDef, RelManualNativeQueryConfig)

-- | Codec for native-query-only relationships
nativeQueryRelationshipsCodec ::
  forall b.
  (Backend b) =>
  AC.Codec
    Value
    (InsOrdHashMap.InsOrdHashMap RelName (RelDef (RelManualNativeQueryConfig b)))
    (InsOrdHashMap.InsOrdHashMap RelName (RelDef (RelManualNativeQueryConfig b)))
nativeQueryRelationshipsCodec =
  AC.dimapCodec
    ( InsOrdHashMap.fromList
        . fmap
          ( \(MergedObject (NameField name) nst) ->
              (name, nst)
          )
    )
    ( fmap (\(fld, nst) -> MergedObject (NameField fld) nst) . InsOrdHashMap.toList
    )
    ( AC.listCodec $
        AC.object "RelDefRelManualNativeQueryConfig" $
          AC.objectCodec @(MergedObject (NameField RelName) (RelDef (RelManualNativeQueryConfig b)))
    )

data MergedObject a b = MergedObject
  { moFst :: a,
    moSnd :: b
  }

instance (HasObjectCodec a, HasObjectCodec b) => HasObjectCodec (MergedObject a b) where
  objectCodec = MergedObject <$> bimapCodec Right moFst objectCodec <*> bimapCodec Right moSnd objectCodec

newtype NameField a = NameField {nameField :: a}

instance (HasCodec a) => HasObjectCodec (NameField a) where
  objectCodec = NameField <$> AC.requiredField "name" "name" AC..= nameField
