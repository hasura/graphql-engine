module Hasura.Backends.DataConnector.IR.Query
  ( QueryRequest (..),
    Query (..),
    Field (..),
    RelationshipField (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (bimap)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Aggregate qualified as IR.A
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Expression qualified as IR.E
import Hasura.Backends.DataConnector.IR.OrderBy qualified as IR.O
import Hasura.Backends.DataConnector.IR.Relationships qualified as IR.R
import Hasura.Backends.DataConnector.IR.Table qualified as IR.T
import Hasura.Prelude
import Hasura.RQL.Types.Common (FieldName (..))
import Witch qualified

-- | An abstract request to retrieve structured data from some source.
data QueryRequest = QueryRequest
  { _qrTable :: IR.T.Name,
    _qrTableRelationships :: IR.R.TableRelationships,
    _qrQuery :: Query
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
instance ToJSON QueryRequest where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From QueryRequest API.QueryRequest where
  from QueryRequest {..} =
    API.QueryRequest
      { _qrTable = Witch.from _qrTable,
        _qrTableRelationships =
          ( \(sourceTableName, relationships) ->
              API.TableRelationships
                { _trSourceTable = Witch.from sourceTableName,
                  _trRelationships = HashMap.mapKeys Witch.from $ Witch.from <$> relationships
                }
          )
            <$> HashMap.toList (IR.R.unTableRelationships _qrTableRelationships),
        _qrQuery = Witch.from _qrQuery
      }

-- | The details of a query against a table
data Query = Query
  { -- Map of field name to Field definition.
    _qFields :: HashMap FieldName Field,
    -- Map of aggregate field name to Aggregate definition
    _qAggregates :: HashMap FieldName IR.A.Aggregate,
    -- | Optionally limit to N results.
    _qLimit :: Maybe Int,
    -- | Optionally offset from the Nth result.
    _qOffset :: Maybe Int,
    -- | Optionally constrain the results to satisfy some predicate.
    _qWhere :: Maybe IR.E.Expression,
    -- | Optionally order the results by the value of one or more fields.
    _qOrderBy :: Maybe IR.O.OrderBy
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
instance ToJSON Query where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From Query API.Query where
  from Query {..} =
    API.Query
      { _qFields = memptyToNothing . KeyMap.fromList $ (bimap (Key.fromText . getFieldNameTxt) Witch.from) <$> HashMap.toList _qFields,
        _qAggregates = memptyToNothing . KeyMap.fromList $ (bimap (Key.fromText . getFieldNameTxt) Witch.from) <$> HashMap.toList _qAggregates,
        _qLimit = _qLimit,
        _qOffset = _qOffset,
        _qWhere = fmap Witch.from _qWhere,
        _qOrderBy = Witch.from <$> _qOrderBy
      }

memptyToNothing :: (Monoid m, Eq m) => m -> Maybe m
memptyToNothing m = if m == mempty then Nothing else Just m

-- | The specific fields that are targeted by a 'Query'.
--
-- A field conceptually falls under one of the two following categories:
--   1. a "column" within the data store that the query is being issued against
--   2. a "relationship", which indicates that the field is the result of
--      another query that must be executed on its own
-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
data Field
  = ColumnField IR.C.Name
  | RelField RelationshipField
  deriving stock (Data, Eq, Generic, Ord, Show)

instance ToJSON Field where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From Field API.Field where
  from (ColumnField name) = API.ColumnField $ Witch.from name
  from (RelField relationshipField) = API.RelField $ Witch.from relationshipField

-- | A relationship consists of the following components:
--   - a sub-query, from the perspective that a relationship field will occur
--     within a broader 'Query'
--   - a join condition relating the data returned by the sub-query with that
--     of the broader 'Query'
--
-- cf. https://en.wikipedia.org/wiki/Join_(SQL)
--     https://www.postgresql.org/docs/13/tutorial-join.html
--     https://www.postgresql.org/docs/13/queries-table-expressions.html#QUERIES-FROM
--
-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
data RelationshipField = RelationshipField
  { _rfRelationship :: IR.R.RelationshipName,
    _rfQuery :: Query
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance ToJSON RelationshipField where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From RelationshipField API.RelationshipField where
  from RelationshipField {..} =
    API.RelationshipField
      { _rfRelationship = Witch.from _rfRelationship,
        _rfQuery = Witch.from _rfQuery
      }
