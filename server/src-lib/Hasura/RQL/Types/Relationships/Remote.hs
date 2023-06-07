{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Relationships.Remote
  ( RemoteRelationship,
    RemoteRelationshipDefinition (..),
    RemoteSourceRelationshipBuilder (..),
    parseRemoteRelationshipDefinition,
    RRFormat (..),
    RRParseMode (..),
    _RelationshipToSource,
    _RelationshipToSchema,
    rrName,
    rrDefinition,
    RemoteSchemaFieldInfo (..),
    RemoteSourceFieldInfo (..),
    RemoteFieldInfoRHS (..),
    RemoteFieldInfo (..),
    DBJoinField (..),
    ScalarComputedField (..),
    graphQLValueToJSON,
    LHSIdentifier (..),
    tableNameToLHSIdentifier,
  )
where

import Autodocodec (HasCodec (codec), JSONCodec, dimapCodec, disjointEitherCodec, requiredField', requiredFieldWith')
import Autodocodec qualified as AC
import Autodocodec.Extended (hashSetCodec)
import Control.Lens (makePrisms)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended (ToTxt (toTxt))
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.Relationships.ToSource
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache.Types
import Hasura.SQL.AnyBackend (AnyBackend)

type RemoteRelationship = RemoteRelationshipG RemoteRelationshipDefinition

instance HasCodec RemoteRelationship where
  codec = remoteRelationshipCodec $ remoteRelationshipDefinitionCodec RRPLenient

instance FromJSON RemoteRelationship where
  parseJSON = withObject "RemoteRelationship" $ \obj ->
    RemoteRelationship
      <$> obj
      .: "name"
      <*> (parseRemoteRelationshipDefinition RRPLenient =<< obj .: "definition")

-- | Represents the format of the metadata a remote relationship was read from
-- and must be written back as. We don't have a good way of doing metadata
-- versioning yet, and we therefore use this to keep track of the format used.
data RRFormat
  = -- | The remote relationship was parsed from the old format, that was only
    -- used only for db-to-rs schemas.
    RRFOldDBToRemoteSchema
  | -- | The remote relationship was parsed from the new unified format.
    RRFUnifiedFormat
  deriving (Show, Eq, Generic)

-- | Specify whether remote schema <> source relationships should be built
data RemoteSourceRelationshipBuilder = IncludeRemoteSourceRelationship | ExcludeRemoteSourceRelationship

-- | Metadata representation of the internal definition of a remote relationship.
data RemoteRelationshipDefinition
  = -- | Remote relationship targetting a source.
    RelationshipToSource ToSourceRelationshipDef
  | -- | Remote relationship targetting a remote schema.
    RelationshipToSchema RRFormat ToSchemaRelationshipDef
  deriving (Show, Eq, Generic)

-- See documentation for 'parseRemoteRelationshipDefinition' for why
-- this is necessary.
instance
  ( TypeError
      ( 'ShowType RemoteRelationshipDefinition
          ':<>: 'Text " has different JSON representations depending on context;"
          ':$$: 'Text "call ‘parseRemoteRelationshipDefinition’ directly instead of relying on ‘FromJSON’"
      )
  ) =>
  FromJSON RemoteRelationshipDefinition
  where
  parseJSON = error "impossible"

-- | Whether to accept legacy fields when parsing 'RemoteRelationshipDefinition'
data RRParseMode
  = -- | Only allow legacy fields when parsing 'RemoteRelationshipDefinition'
    RRPLegacy
  | -- | Allow legacy fields when parsing 'RemoteRelationshipDefinition'
    RRPLenient
  | -- | Reject legacy fields when parsing 'RemoteRelationshipDefinition'
    RRPStrict
  deriving (Show, Eq, Generic)

remoteRelationshipDefinitionCodec :: RRParseMode -> JSONCodec RemoteRelationshipDefinition
remoteRelationshipDefinitionCodec mode =
  dimapCodec
    (either RelationshipToSource (uncurry RelationshipToSchema))
    ( \case
        RelationshipToSource source -> Left source
        RelationshipToSchema format schema -> Right (format, schema)
    )
    $ disjointEitherCodec toSource toSchema
  where
    toSource = AC.object "RelationshipToSource" $ requiredField' "to_source"

    toSchema :: JSONCodec (RRFormat, ToSchemaRelationshipDef)
    toSchema = case mode of
      RRPLegacy -> dimapCodec (RRFOldDBToRemoteSchema,) snd toSchemaOldDBFormat
      RRPStrict -> dimapCodec (RRFUnifiedFormat,) snd toSchemaUnified
      RRPLenient ->
        dimapCodec
          (either (RRFUnifiedFormat,) (RRFOldDBToRemoteSchema,)) -- decoding
          ( \case
              (RRFUnifiedFormat, l) -> Left l
              (RRFOldDBToRemoteSchema, r) -> Right r
          )
          $ disjointEitherCodec toSchemaUnified toSchemaOldDBFormat

    toSchemaUnified :: JSONCodec ToSchemaRelationshipDef
    toSchemaUnified = AC.object "RelationshipToSchema" $ requiredField' "to_remote_schema"

    toSchemaOldDBFormat :: JSONCodec ToSchemaRelationshipDef
    toSchemaOldDBFormat =
      AC.object "ToSchemaRelationshipDefLegacyFormat"
        $ ToSchemaRelationshipDef
        <$> requiredField' "remote_schema"
        AC..= _trrdRemoteSchema
          <*> requiredFieldWith' "hasura_fields" hashSetCodec
        AC..= _trrdLhsFields
          <*> requiredField' "remote_field"
        AC..= _trrdRemoteField

-- | Parse 'RemoteRelationshipDefinition' letting the caller decide how lenient to be.
--
-- This is necessary because 'RemoteRelationshipDefinition' is parsed in
-- different contexts. In 'RemoteRelationship', the
-- 'RemoteRelationshipDefinition' is always parsed out from a top-level
-- @"definition" field. Thus, a legacy payload looks like this:
--
-- @
-- {
--   "name": "thing",
--   "definition": {
--     "remote_schema": "stuff",
--     "hasura_fields": ...
--     "remote_field": ...
--   }
-- }
-- @
--
-- and a new payload looks like this:
--
-- @
-- {
--   "name": "thing",
--   "definition": {
--     "to_remote_schema": {
--       "schema": "stuff",
--       "lhs_fields": ...
--       "remote_field": ...
--     }
--   }
-- }
-- @
--
-- In contrast, 'CreateFromSourceRelationship' does not have a top-
-- level @"definition"@ in its legacy format. Instead, the legacy fields
-- themselves are top-level:
--
-- @
-- {
--   "remote_schema": "stuff",
--   "hasura_fields": ...
--   "remote_field": ...
-- }
-- @
--
-- Furthermore, the presence of a @"definition"@ field is used to detect
-- that the new payload is being used:
--
-- @
-- {
--   "definition": {
--     "to_remote_schema": {
--       "schema": "stuff",
--       "lhs_fields": ...
--       "remote_field": ...
--     }
--   }
-- }
-- @
--
-- In this latter case, we should not allow @"remote_schema"@ to appear
-- under @"definition"@.
parseRemoteRelationshipDefinition :: RRParseMode -> Value -> Parser RemoteRelationshipDefinition
parseRemoteRelationshipDefinition mode = withObject ("RemoteRelationshipDefinition " <> suffix) \obj -> do
  remoteSchema <- obj .:? "remote_schema"
  case (remoteSchema, mode) of
    (Just {}, RRPStrict) -> invalid
    (Just schema, _) -> do
      hasuraFields <- obj .: "hasura_fields"
      remoteField <- obj .: "remote_field"
      pure $ RelationshipToSchema RRFOldDBToRemoteSchema $ ToSchemaRelationshipDef schema hasuraFields remoteField
    (Nothing, RRPLegacy) -> invalid
    (Nothing, _) -> do
      toSource <- obj .:? "to_source"
      toSchema <- obj .:? "to_remote_schema"
      case (toSchema, toSource) of
        (Just schema, Nothing) -> RelationshipToSchema RRFUnifiedFormat <$> parseJSON schema
        (Nothing, Just source) -> RelationshipToSource <$> parseJSON source
        _ -> invalid
  where
    (suffix, expected) = case mode of
      RRPLegacy -> ("(legacy format)", "remote_schema")
      RRPLenient -> ("(lenient format)", "remote_schema, to_source, to_remote_schema")
      RRPStrict -> ("(strict format)", "to_source, to_remote_schema")

    invalid =
      fail
        $ mconcat
          [ "remote relationship definition ",
            suffix,
            " expects exactly one of: ",
            expected
          ]

instance ToJSON RemoteRelationshipDefinition where
  toJSON = \case
    RelationshipToSource source -> object ["to_source" .= toJSON source]
    RelationshipToSchema format schema@ToSchemaRelationshipDef {..} -> case format of
      RRFUnifiedFormat -> object ["to_remote_schema" .= toJSON schema]
      RRFOldDBToRemoteSchema ->
        object
          [ "remote_schema" .= toJSON _trrdRemoteSchema,
            "hasura_fields" .= toJSON _trrdLhsFields,
            "remote_field" .= toJSON _trrdRemoteField
          ]

--------------------------------------------------------------------------------
-- schema cache

-- | Resolved remote relationship, as stored in the schema cache.
data RemoteFieldInfo lhsJoinField = RemoteFieldInfo
  { _rfiLHS :: HashMap.HashMap FieldName lhsJoinField,
    _rfiRHS :: RemoteFieldInfoRHS
  }
  deriving (Generic, Eq)

instance (ToJSON lhsJoinField) => ToJSON (RemoteFieldInfo lhsJoinField)

-- | Resolved remote relationship's RHS
data RemoteFieldInfoRHS
  = RFISchema RemoteSchemaFieldInfo
  | RFISource (AnyBackend RemoteSourceFieldInfo)
  deriving (Generic, Eq)

instance ToJSON RemoteFieldInfoRHS where
  toJSON =
    \case
      RFISchema schema -> toJSON schema
      RFISource _ -> toJSON ()

-- | Information about the field on the LHS of a join against a remote schema.
data DBJoinField (b :: BackendType)
  = JoinColumn (Column b) (ColumnType b)
  | JoinComputedField (ScalarComputedField b)
  deriving (Generic)

deriving instance (Backend b) => Eq (DBJoinField b)

deriving instance (Backend b) => Show (DBJoinField b)

instance (Backend b) => Hashable (DBJoinField b)

instance (Backend b) => ToJSON (DBJoinField b) where
  toJSON = \case
    JoinColumn column columnType -> toJSON (column, columnType)
    JoinComputedField computedField -> toJSON computedField

-- | Information about a computed field appearing on the LHS of a remote join.
-- FIXME: why do we need all of this?
data ScalarComputedField (b :: BackendType) = ScalarComputedField
  { _scfXField :: XComputedField b,
    _scfName :: ComputedFieldName,
    _scfFunction :: FunctionName b,
    _scfComputedFieldImplicitArgs :: ComputedFieldImplicitArguments b,
    _scfType :: ScalarType b
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (ScalarComputedField b)

deriving instance (Backend b) => Show (ScalarComputedField b)

instance (Backend b) => Hashable (ScalarComputedField b)

instance (Backend b) => ToJSON (ScalarComputedField b) where
  toJSON ScalarComputedField {..} =
    object
      [ "name" .= _scfName,
        "function" .= _scfFunction,
        "function_implicit_arguments" .= _scfComputedFieldImplicitArgs,
        "type" .= _scfType
      ]

-- TODO: this will probably end up in a database module when we
-- modularise databases related code
tableNameToLHSIdentifier :: (Backend b) => TableName b -> LHSIdentifier
tableNameToLHSIdentifier = LHSIdentifier . toTxt

--------------------------------------------------------------------------------
-- template haskell generation

$(makePrisms ''RemoteRelationshipDefinition)
