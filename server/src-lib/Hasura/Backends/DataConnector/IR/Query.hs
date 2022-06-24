module Hasura.Backends.DataConnector.IR.Query
  ( QueryRequest (..),
    Query (..),
    Field (..),
    RelationshipField (..),
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as J
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Expression qualified as IR.E
import Hasura.Backends.DataConnector.IR.OrderBy qualified as IR.O
import Hasura.Backends.DataConnector.IR.Relationships qualified as IR.R
import Hasura.Backends.DataConnector.IR.Table qualified as IR.T
import Hasura.Prelude

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

-- | The details of a query against a table
data Query = Query
  { -- NOTE: We should clarify what the 'Text' key is supposed to indicate.
    _qFields :: HashMap Text Field,
    -- | Optionally limit to N results.
    _qLimit :: Maybe Int,
    -- | Optionally offset from the Nth result.
    _qOffset :: Maybe Int,
    -- | Optionally constrain the results to satisfy some predicate.
    _qWhere :: Maybe IR.E.Expression,
    -- | Optionally order the results by the value of one or more fields.
    _qOrderBy :: [IR.O.OrderBy]
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
instance ToJSON Query where
  toJSON = J.genericToJSON J.defaultOptions

-- | The specific fields that are targeted by a 'Query'.
--
-- A field conceptually falls under one of the two following categories:
--   1. a "column" within the data store that the query is being issued against
--   2. a "relationship", which indicates that the field is the result of
--      another query that must be executed on its own
--   3. a "literal", which represents a piece of text, such as a
--      '__typename' field, which will appear in the final output and is
--      provided by HGE and not the Agent.
-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
data Field
  = ColumnField IR.C.Name
  | RelField RelationshipField
  | LiteralField Text
  deriving stock (Data, Eq, Generic, Ord, Show)

instance ToJSON Field where
  toJSON = J.genericToJSON J.defaultOptions

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
