{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataWrapper.IR.Query
  ( Query (..),
    Field (..),
    Cardinality (..),
    ColumnContents (..),
    RelationshipContents (..),
    PrimaryKey (..),
    ForeignKey (..),
  )
where

--------------------------------------------------------------------------------

import Autodocodec.Extended (ValueWrapper (ValueWrapper))
import Data.Aeson (ToJSON, ToJSONKey)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as M
import Data.List.NonEmpty (fromList)
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Backends.DataWrapper.IR.Column qualified as Column (Name)
import Hasura.Backends.DataWrapper.IR.Expression (Expression)
import Hasura.Backends.DataWrapper.IR.OrderBy (OrderBy)
import Hasura.Backends.DataWrapper.IR.Table qualified as Table (Name)
import Hasura.Prelude
import Witch

--------------------------------------------------------------------------------

-- | An abstract request to retrieve structured data from some source.
--
-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
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
    orderBy :: [OrderBy],
    -- | The cardinality of response we expect from the Agent's response.
    cardinality :: Cardinality
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

instance ToJSON Query where
  toJSON = J.genericToJSON J.defaultOptions

instance From Query API.Query where
  from Query {from = from_, ..} =
    API.Query
      { fields = M.mapMaybe id $ fmap fromField fields,
        from = Witch.from from_,
        limit = limit,
        offset = offset,
        where_ = fmap Witch.from where_,
        orderBy = case orderBy of
          [] -> Nothing
          xs -> Just $ fromList $ fmap Witch.from xs
      }

--------------------------------------------------------------------------------

-- | This data structure keeps track of what cardinality of response we should
-- send back to the client for a given query, which may be different from what
-- we receive from the backend (which is always possibly-many records).
data Cardinality
  = Many
  | OneOrZero
  deriving stock (Data, Eq, Generic, Ord, Show)

instance ToJSON Cardinality where
  toJSON = J.genericToJSON J.defaultOptions

--------------------------------------------------------------------------------

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
  = Column ColumnContents
  | Relationship RelationshipContents
  | Literal Text
  deriving stock (Data, Eq, Generic, Ord, Show)

instance ToJSON Field where
  toJSON = J.genericToJSON J.defaultOptions

fromField :: Field -> Maybe API.Field
fromField = \case
  Column contents -> Just $ Witch.from contents
  Relationship contents -> Just $ Witch.from contents
  Literal _ -> Nothing

--------------------------------------------------------------------------------

-- | TODO
--
-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
newtype ColumnContents = ColumnContents
  { column :: Column.Name
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

instance ToJSON ColumnContents where
  toJSON = J.genericToJSON J.defaultOptions

instance From ColumnContents API.Field where
  from (ColumnContents name) = API.ColumnField $ ValueWrapper $ Witch.from name

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
data RelationshipContents = RelationshipContents
  { joinCondition :: HashMap PrimaryKey ForeignKey,
    query :: Query
  }
  deriving stock (Data, Eq, Generic, Ord, Show)

instance ToJSON RelationshipContents where
  toJSON = J.genericToJSON J.defaultOptions

instance From RelationshipContents API.Field where
  from (RelationshipContents joinCondition query) =
    let joinCondition' = M.mapKeys Witch.from $ fmap Witch.from joinCondition
     in (API.RelationshipField (API.RelField joinCondition' (Witch.from query)))

--------------------------------------------------------------------------------

-- | TODO
--
-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
newtype PrimaryKey = PrimaryKey Column.Name
  deriving stock (Data, Generic)
  deriving newtype (Eq, Hashable, Ord, Show, ToJSON, ToJSONKey)

instance From API.PrimaryKey PrimaryKey where
  from (API.PrimaryKey key) = PrimaryKey (Witch.from key)

instance From PrimaryKey API.PrimaryKey where
  from (PrimaryKey key) = API.PrimaryKey (Witch.from key)

--------------------------------------------------------------------------------

-- | TODO
--
-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
newtype ForeignKey = ForeignKey Column.Name
  deriving stock (Data, Generic)
  deriving newtype (Eq, Hashable, Ord, Show, ToJSON, ToJSONKey)

instance From API.ForeignKey ForeignKey where
  from (API.ForeignKey key) = ForeignKey (Witch.from key)

instance From ForeignKey API.ForeignKey where
  from (ForeignKey key) = API.ForeignKey (Witch.from key)
