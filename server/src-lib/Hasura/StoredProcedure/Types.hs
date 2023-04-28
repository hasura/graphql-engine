-- | A name for a native query as it is recognized by the graphql schema.
module Hasura.StoredProcedure.Types
  ( StoredProcedureName (..),
    NullableScalarType (..),
    nullableScalarTypeMapCodec,
    storedProcedureArrayRelationshipsCodec,
  )
where

import Autodocodec (HasCodec (codec), HasObjectCodec (..), bimapCodec, dimapCodec)
import Autodocodec qualified as AC
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, Value)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended (ToTxt)
import Hasura.LogicalModel.NullableScalarType
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Common (RelName)
import Hasura.RQL.Types.Relationships.Local (RelDef, RelManualConfig)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax (Lift)

-- The name of a native query. This appears as a root field name in the graphql schema.
newtype StoredProcedureName = StoredProcedureName {getStoredProcedureName :: G.Name}
  deriving newtype (Eq, Ord, Show, Hashable, NFData, ToJSON, FromJSON, ToTxt)
  deriving stock (Data, Generic, Lift)

instance HasCodec StoredProcedureName where
  codec = dimapCodec StoredProcedureName getStoredProcedureName codec

instance FromJSONKey StoredProcedureName

instance ToJSONKey StoredProcedureName

data MergedObject a b = MergedObject
  { moFst :: a,
    moSnd :: b
  }

instance (HasObjectCodec a, HasObjectCodec b) => HasObjectCodec (MergedObject a b) where
  objectCodec = MergedObject <$> bimapCodec Right moFst objectCodec <*> bimapCodec Right moSnd objectCodec

newtype NameField a = NameField {nameField :: a}

instance (HasCodec a) => HasObjectCodec (NameField a) where
  objectCodec = NameField <$> AC.requiredField "name" "name" AC..= nameField

storedProcedureArrayRelationshipsCodec ::
  forall b.
  (Backend b) =>
  AC.Codec
    Value
    (InsOrdHashMap.InsOrdHashMap RelName (RelDef (RelManualConfig b)))
    (InsOrdHashMap.InsOrdHashMap RelName (RelDef (RelManualConfig b)))
storedProcedureArrayRelationshipsCodec =
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
        AC.object "RelDefRelManualConfig" $
          AC.objectCodec @(MergedObject (NameField RelName) (RelDef (RelManualConfig b)))
    )
