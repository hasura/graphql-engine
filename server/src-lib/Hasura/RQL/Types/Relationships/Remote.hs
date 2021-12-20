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
  )
where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.Aeson.Types (Parser)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.Relationships.ToSchema
import Hasura.RQL.Types.Relationships.ToSource

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
-- template haskell generation

$(makeLenses ''RemoteRelationship)
$(J.deriveToJSON hasuraJSON {J.omitNothingFields = False} ''RemoteRelationship)
$(makePrisms ''RemoteRelationshipDefinition)
