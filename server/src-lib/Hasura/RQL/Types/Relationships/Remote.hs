module Hasura.RQL.Types.Relationships.Remote
  ( RemoteRelationship (..),
    RemoteRelationshipDefinition (..),
    RRFormat (..),
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

instance FromJSON RemoteRelationshipDefinition where
  parseJSON = withObject "RemoteRelationshipDefinition" \obj -> do
    oldRSName <- obj .:? "remote_schema"
    case oldRSName of
      Just rsName -> do
        hFields <- obj .: "hasura_fields"
        rField <- obj .: "remote_field"
        pure $ RelationshipToSchema RRFOldDBToRemoteSchema $ ToSchemaRelationshipDef rsName hFields rField
      Nothing -> do
        toSource <- obj .:? "to_source"
        toSchema <- obj .:? "to_remote_schema"
        case (toSchema, toSource) of
          (Just schema, Nothing) -> RelationshipToSchema RRFUnifiedFormat <$> parseJSON schema
          (Nothing, Just source) -> RelationshipToSource <$> parseJSON source
          _ -> fail "remote relationship definition expects exactly one of: to_source, to_remote_schema"

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
$(J.deriveJSON hasuraJSON {J.omitNothingFields = False} ''RemoteRelationship)
$(makePrisms ''RemoteRelationshipDefinition)
