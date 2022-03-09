module Hasura.Backends.DataWrapper.IR.Query
  ( Query (..),
    Field (..),
    ColumnContents (..),
    RelationshipContents (..),
  )
where

--------------------------------------------------------------------------------

import Hasura.Backends.DataWrapper.IR.Column qualified as Column (Name)
import Hasura.Backends.DataWrapper.IR.Expression (Expression)
import Hasura.Backends.DataWrapper.IR.OrderBy (OrderBy)
import Hasura.Backends.DataWrapper.IR.Table qualified as Table (Name)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | An abstract request to retrieve structured data from some source.
data Query = Query
  { -- NOTE: We should clarify what the 'Text' key is supposed to indicate.
    fields :: HashMap Text Field,
    -- | Reference to the table these fields are in.
    from :: Table.Name,
    -- | Optionally limit to N results.
    limit :: Maybe Int,
    -- | Optionally offset from the Nth result.
    offset :: Maybe Int,
    -- | Optionally constrain the results to satisfy some predicate.
    where_ :: Maybe Expression,
    -- | Optionally order the results by the value of one or more fields.
    orderBy :: [OrderBy]
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

--------------------------------------------------------------------------------

-- | The specific fields that are targeted by a 'Query'.
--
-- A field conceptually falls under one of the two following categories:
--   1. a "column" within the data store that the query is being issued against
--   2. a "relationship", which indicates that the field is the result of
--      another query that must be executed on its own
data Field
  = Column ColumnContents
  | Relationship RelationshipContents
  deriving stock (Data, Eq, Generic, Ord, Show)

newtype ColumnContents = ColumnContents
  { column :: Column.Name
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

-- | A relationship consists of the following components:
--   - a sub-query, from the perspective that a relationship field will occur
--     within a broader 'Query'
--   - a join condition relating the data returned by the sub-query with that
--     of the broader 'Query'
--
-- cf. https://en.wikipedia.org/wiki/Join_(SQL)
--     https://www.postgresql.org/docs/13/tutorial-join.html
--     https://www.postgresql.org/docs/13/queries-table-expressions.html#QUERIES-FROM
data RelationshipContents = RelationshipContents
  { joinCondition :: HashMap PrimaryKey ForeignKey,
    query :: Query
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

newtype PrimaryKey = PrimaryKey Column.Name
  deriving stock (Data, Generic)
  deriving newtype (Eq, Hashable, Ord, Show)

newtype ForeignKey = ForeignKey Column.Name
  deriving stock (Data, Generic)
  deriving newtype (Eq, Hashable, Ord, Show)
