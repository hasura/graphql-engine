{-# LANGUAGE OverloadedLists #-}

-- | Types for stored procedures.
module Hasura.StoredProcedure.Types
  ( NullableScalarType (..),
    nullableScalarTypeMapCodec,
    storedProcedureArrayRelationshipsCodec,
    StoredProcedureConfig (..),
    StoredProcedureExposedAs (..),
  )
where

import Autodocodec (HasCodec (codec), HasObjectCodec (..), bimapCodec)
import Autodocodec qualified as AC
import Autodocodec.Extended (graphQLFieldNameCodec)
import Data.Aeson
import Data.Char (toLower)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Hasura.LogicalModel.NullableScalarType
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Common (RelName)
import Hasura.RQL.Types.Relationships.Local (RelDef, RelManualConfig)
import Language.GraphQL.Draft.Syntax qualified as G

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

-- * Configuration

-- | Tracked stored procedure configuration, and payload of the 'pg_track_stored procedure'.
data StoredProcedureConfig = StoredProcedureConfig
  { -- | In which top-level field should we expose this stored procedure?
    _spcExposedAs :: StoredProcedureExposedAs,
    _spcCustomName :: Maybe G.Name
  }
  deriving (Show, Eq, Generic)

instance NFData StoredProcedureConfig

instance HasCodec StoredProcedureConfig where
  codec =
    AC.object "StoredProcedureConfig" $
      StoredProcedureConfig
        <$> AC.requiredField' "exposed_as" AC..= _spcExposedAs
        <*> AC.optionalFieldWith' "custom_name" graphQLFieldNameCodec AC..= _spcCustomName

instance FromJSON StoredProcedureConfig where
  parseJSON = withObject "StoredProcedureConfig" $ \obj ->
    StoredProcedureConfig
      <$> obj .: "exposed_as"
      <*> obj .:? "custom_name"

instance ToJSON StoredProcedureConfig where
  toJSON = genericToJSON hasuraJSON {omitNothingFields = True}
  toEncoding = genericToEncoding hasuraJSON {omitNothingFields = True}

-- | Indicates whether the user requested the corresponding stored procedure to be
-- tracked as a mutation or a query, in @track_stored_procedure@.
-- currently only query is supported.
data StoredProcedureExposedAs = SPEAQuery
  deriving (Show, Eq, Generic)

instance NFData StoredProcedureExposedAs

instance HasCodec StoredProcedureExposedAs where
  codec = AC.stringConstCodec [(SPEAQuery, "query")]

instance FromJSON StoredProcedureExposedAs where
  parseJSON = genericParseJSON defaultOptions {sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 4}

instance ToJSON StoredProcedureExposedAs where
  toJSON = genericToJSON defaultOptions {sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 4}
  toEncoding = genericToEncoding defaultOptions {sumEncoding = UntaggedValue, constructorTagModifier = map toLower . drop 4}
