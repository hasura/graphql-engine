{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Relationships.Remote
  ( RemoteRelationship (..),
    RemoteRelationshipDefinition (..),
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

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.Relationships.ToSchema
import Hasura.RQL.Types.Relationships.ToSource
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.SQL.Backend

--------------------------------------------------------------------------------
-- metadata

-- | Metadata representation of a generic remote relationship, regardless of the
-- source: all sources use this same agnostic definition. The internal
-- definition field is where we differentiate between different targets.
data RemoteRelationship = RemoteRelationship
  { _rrName :: RelName,
    _rrDefinition :: RemoteRelationshipDefinition
  }
  deriving (Show, Eq, Generic)

instance Cacheable RemoteRelationship

instance FromJSON RemoteRelationship where
  parseJSON = withObject "RemoteRelationship" $ \obj ->
    RemoteRelationship
      <$> obj .: "name"
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

instance Cacheable RRFormat

-- | Metadata representation of the internal definition of a remote relationship.
data RemoteRelationshipDefinition
  = -- | Remote relationship targetting a source.
    RelationshipToSource ToSourceRelationshipDef
  | -- | Remote relationship targetting a remote schema.
    RelationshipToSchema RRFormat ToSchemaRelationshipDef
  deriving (Show, Eq, Generic)

instance Cacheable RemoteRelationshipDefinition

-- See documentation for 'parseRemoteRelationshipDefinition' for why
-- this is necessary.
instance
  TypeError
    ( 'ShowType RemoteRelationshipDefinition
        ':<>: 'Text " has different JSON representations depending on context;"
        ':$$: 'Text "call ‘parseRemoteRelationshipDefinition’ directly instead of relying on ‘FromJSON’"
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
      fail $
        mconcat
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
  { _rfiLHS :: HM.HashMap FieldName lhsJoinField,
    _rfiRHS :: RemoteFieldInfoRHS
  }
  deriving (Generic, Eq)

instance (Cacheable lhsJoinField) => Cacheable (RemoteFieldInfo lhsJoinField)

instance (ToJSON lhsJoinField) => ToJSON (RemoteFieldInfo lhsJoinField)

-- | Resolved remote relationship's RHS
data RemoteFieldInfoRHS
  = RFISchema !RemoteSchemaFieldInfo
  | RFISource !(AnyBackend RemoteSourceFieldInfo)
  deriving (Generic, Eq)

instance Cacheable RemoteFieldInfoRHS

instance ToJSON RemoteFieldInfoRHS where
  toJSON =
    \case
      RFISchema schema -> toJSON schema
      RFISource _ -> toJSON ()

-- | Information about the field on the LHS of a join against a remote schema.
data DBJoinField (b :: BackendType)
  = JoinColumn !(Column b) !(ColumnType b)
  | JoinComputedField !(ScalarComputedField b)
  deriving (Generic)

deriving instance Backend b => Eq (DBJoinField b)

deriving instance Backend b => Show (DBJoinField b)

instance Backend b => Cacheable (DBJoinField b)

instance Backend b => Hashable (DBJoinField b)

instance (Backend b) => ToJSON (DBJoinField b) where
  toJSON = \case
    JoinColumn column columnType -> toJSON (column, columnType)
    JoinComputedField computedField -> toJSON computedField

-- | Information about a computed field appearing on the LHS of a remote join.
-- FIXME: why do we need all of this?
data ScalarComputedField (b :: BackendType) = ScalarComputedField
  { _scfXField :: !(XComputedField b),
    _scfName :: !ComputedFieldName,
    _scfFunction :: !(FunctionName b),
    _scfTableArgument :: !FunctionTableArgument,
    _scfSessionArgument :: !(Maybe FunctionSessionArgument),
    _scfType :: !(ScalarType b)
  }
  deriving (Generic)

deriving instance Backend b => Eq (ScalarComputedField b)

deriving instance Backend b => Show (ScalarComputedField b)

instance Backend b => Cacheable (ScalarComputedField b)

instance Backend b => Hashable (ScalarComputedField b)

instance Backend b => ToJSON (ScalarComputedField b) where
  toJSON ScalarComputedField {..} =
    object
      [ "name" .= _scfName,
        "function" .= _scfFunction,
        "table_argument" .= _scfTableArgument,
        "session_argument" .= _scfSessionArgument,
        "type" .= _scfType
      ]

--------------------------------------------------------------------------------
-- template haskell generation

$(makeLenses ''RemoteRelationship)
$(J.deriveToJSON hasuraJSON {J.omitNothingFields = False} ''RemoteRelationship)
$(makePrisms ''RemoteRelationshipDefinition)
