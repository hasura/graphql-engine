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

instance FromJSON RemoteRelationship where
  parseJSON = withObject "RemoteRelationship" \obj -> do
    relName <- obj .: "name"
    oldRSName <- obj .:? "remote_schema"
    newDefinition <- obj .:? "definition"
    definition <- case (oldRSName, newDefinition) of
      (Just rsName, Nothing) -> do
        hFields <- obj .: "hasura_fields"
        rField <- obj .: "remote_field"
        pure $ RelationshipToSchema RRFOldDBToRemoteSchema $ ToSchemaRelationshipDef rsName hFields rField
      (Nothing, Just def) -> do
        toSource <- def .:? "to_source"
        toSchema <- def .:? "to_remote_schema"
        case (toSchema, toSource) of
          (Just schema, Nothing) -> RelationshipToSchema RRFUnifiedFormat <$> parseJSON schema
          (Nothing, Just source) -> RelationshipToSource <$> parseJSON source
          _ -> fail "remote relationship definition expects exactly one of: to_source, to_remote_schema"
      _ -> fail "remote relationship definition expects exactly one of: definition, remote_schema"
    pure $ RemoteRelationship relName definition

instance ToJSON RemoteRelationship where
  toJSON RemoteRelationship {..} =
    object $
      ("name" .= _rrName) : case _rrDefinition of
        RelationshipToSource source -> definition ["to_source" .= toJSON source]
        RelationshipToSchema format schema@ToSchemaRelationshipDef {..} -> case format of
          RRFUnifiedFormat -> definition ["to_remote_schema" .= toJSON schema]
          RRFOldDBToRemoteSchema ->
            [ "remote_schema" .= toJSON _trrdRemoteSchema,
              "hasura_fields" .= toJSON _trrdLhsFields,
              "remote_field" .= toJSON _trrdRemoteField
            ]
    where
      definition d = ["definition" .= object d]

--------------------------------------------------------------------------------
-- template haskell generation

$(makeLenses ''RemoteRelationship)
$(makePrisms ''RemoteRelationshipDefinition)
