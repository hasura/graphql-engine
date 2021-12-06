module Hasura.RQL.Types.Relationships.ToSource
  ( ToSourceRelationshipDef (..),
    tsrdFieldMapping,
    tsrdRelationshipType,
    tsrdSource,
    tsrdTable,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Instances ()

--------------------------------------------------------------------------------
-- metadata

-- | Metadata representation of a "remote" relationship targetting a source.
--
-- This representation has to be backend-agnostic, as it will be parsed before
-- the source cache is built, meaning we can't decide how something backend
-- specific should be resolved based solely on the source name. But the table
-- name is specific to the targeted backend... There are two solutions to this
-- problem:
--  - we can either include an additional field in the serialization that tells
--    us the "kind" of the backend; but that requies an additional field that is
--    technically not required, and that could potentially be inconsistent
--  - or we can do the same thing that we do for source to source relationships:
--    we store an unparsed JSON value as far as the metadata goes, and we parse
--    it when building the schema cache, when we know the kind of the source
--    from its name
-- We chose the latter.
--
-- Furthermore, the mapping is represented using a backend-agnostic 'FieldName',
-- whose interpretation is likewise delayed until the schema cache is built.
--
-- FIXME: move this to Hasura/Metadata
data ToSourceRelationshipDef = ToSourceRelationshipDef
  { _tsrdRelationshipType :: !RelType,
    _tsrdFieldMapping :: !(HashMap FieldName FieldName),
    _tsrdSource :: !SourceName,
    _tsrdTable :: !Value
  }
  deriving stock (Show, Eq, Generic)

instance NFData ToSourceRelationshipDef

instance Cacheable ToSourceRelationshipDef

instance ToJSON ToSourceRelationshipDef where
  toJSON = genericToJSON hasuraJSON

instance FromJSON ToSourceRelationshipDef where
  parseJSON = genericParseJSON hasuraJSON

--------------------------------------------------------------------------------
-- template haskell generation

$(makeLenses ''ToSourceRelationshipDef)
