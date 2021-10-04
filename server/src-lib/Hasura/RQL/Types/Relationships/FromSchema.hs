module Hasura.RQL.Types.Relationships.FromSchema where

import Control.Lens (makeLenses)
import Data.Aeson
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Instances ()
import Hasura.SQL.AnyBackend
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- metadata

-- | A remote relationship from a schema targets either another remote
-- schema or a source. For now, we only support targeting another
-- source.
--
-- FIXME: move this to Hasura/Metadata
data FromSchemaRelationshipDef
  = FromSchemaToSourceRelDef !FromSchemaToSourceRelationshipDef
  deriving stock (Show, Eq, Generic)

instance NFData FromSchemaRelationshipDef

instance Cacheable FromSchemaRelationshipDef

instance FromJSON FromSchemaRelationshipDef where
  parseJSON j =
    j & withObject "FromSchemaRelationshipDef" \o -> do
      (mSource :: Maybe Value) <- o .:? "remote_source"
      (mSchema :: Maybe Value) <- o .:? "remote_schema"
      case (mSource, mSchema) of
        (Nothing, Nothing) -> fail "expected one of \"remote_source\" or \"remote_schema\""
        (Just _, Just _) -> fail "expected only one of \"remote_source\" or \"remote_schema\""
        (_, Just _) -> fail "\"remote_schema\" is currently unsupported"
        (Just source, _) -> FromSchemaToSourceRelDef <$> parseJSON source

instance ToJSON FromSchemaRelationshipDef where
  toJSON = \case
    FromSchemaToSourceRelDef source -> object ["remote_source" .= toJSON source]

-- | Metadata representation of a relationship from remote schema to source.
--
-- This representation has to be backend-agnostic, as it will be parsed before
-- the source cache is built, meaning we can't decide how something backend
-- specific should be resolved based solely on the source name. But some of the
-- fields are specific to the targeted backend...There are two solutions to this
-- problem:
--  - we can either include an additional field in the serialization that tells
--    us the "kind" of the backend; but that requies an additional field that is
--    technically not required, and that could potentially be inconsistent
--  - or we can do the same thing that we do for source to source relationships:
--    we store an unparsed JSON value as far as the metadata goes, and we parse
--    it when building the schema cache, when we know the kind of the source
--    from its name
--
-- We chose the latter.
--
-- FIXME: move this to Hasura/Metadata
data FromSchemaToSourceRelationshipDef = FromSchemaToSourceRelationshipDef
  { _frtsrdFieldMapping :: !(HashMap G.Name FieldName),
    _frtsrdRelationshipType :: !RelType,
    _frtsrdSource :: !SourceName,
    _frtsrdTable :: !Value
  }
  deriving stock (Generic, Show, Eq)

instance NFData FromSchemaToSourceRelationshipDef

instance Cacheable FromSchemaToSourceRelationshipDef

instance FromJSON FromSchemaToSourceRelationshipDef where
  parseJSON = withObject "FromSchemaToSourceRelationshipDef" \o ->
    FromSchemaToSourceRelationshipDef
      <$> o .: "field_mapping"
      <*> o .: "relationship_type"
      <*> o .: "source"
      <*> o .: "table"

instance ToJSON FromSchemaToSourceRelationshipDef where
  toJSON = genericToJSON hasuraJSON

--------------------------------------------------------------------------------
-- schema cache

-- | A resolved remote relationship from a schema targets either another remote
-- schema or a source. For now, we only support targeting another source.
data ResolvedFromSchemaRelationship
  = ResolvedFromSchemaToSourceRel (AnyBackend ResolvedFromSchemaToSourceRelationship)

data ResolvedFromSchemaToSourceRelationship (b :: BackendType) = ResolvedFromSchemaToSourceRelationship
  { _rfrtsrFieldMapping :: HashMap G.Name (ScalarType b, Column b),
    _rfrtsrRelationshipType :: RelType,
    _rfrtsrSource :: SourceName,
    _rfrtsrSourceConfig :: SourceConfig b,
    _rfrtsrTable :: TableName b
  }

--------------------------------------------------------------------------------
-- template haskell generation

$(makeLenses ''FromSchemaToSourceRelationshipDef)
