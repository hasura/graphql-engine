{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Query
  ( QueryRequest (..),
    qrTable,
    qrTableRelationships,
    qrQuery,
    qrForeach,
    FieldName (..),
    Query (..),
    qFields,
    qAggregates,
    qAggregatesLimit,
    qLimit,
    qOffset,
    qWhere,
    qOrderBy,
    Field (..),
    RelationshipField (..),
    QueryResponse (..),
    qrRows,
    qrAggregates,
    FieldValue,
    mkColumnFieldValue,
    mkRelationshipFieldValue,
    mkNestedObjFieldValue,
    mkNestedArrayFieldValue,
    deserializeAsColumnFieldValue,
    deserializeAsRelationshipFieldValue,
    deserializeAsNestedObjFieldValue,
    deserializeAsNestedArrayFieldValue,
    _ColumnFieldValue,
    _RelationshipFieldValue,
    _NestedObjFieldValue,
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.Arrow (left)
import Control.Lens (Lens', Prism', lens, prism')
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as J
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Show (appPrec, appPrec1)
import Hasura.Backends.DataConnector.API.V0.Aggregate qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Expression qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.OrderBy qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Relationships qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Scalar qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Servant.API (HasStatus (..))
import Prelude

-- | A serializable request to retrieve strutured data from some
-- source.
data QueryRequest = QueryRequest
  { _qrTable :: API.V0.TableName,
    _qrTableRelationships :: Set API.V0.TableRelationships,
    _qrQuery :: Query,
    _qrForeach :: Maybe (NonEmpty (HashMap API.V0.ColumnName API.V0.ScalarValue))
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec QueryRequest

instance HasCodec QueryRequest where
  codec =
    object "QueryRequest" $
      QueryRequest
        <$> requiredField "table" "The name of the table to query"
          .= _qrTable
        <*> requiredField "table_relationships" "The relationships between tables involved in the entire query request"
          .= _qrTableRelationships
        <*> requiredField "query" "The details of the query against the table"
          .= _qrQuery
        <*> optionalFieldOrNull "foreach" "If present, a list of columns and values for the columns that the query must be repeated for, applying the column values as a filter for each query."
          .= _qrForeach

newtype FieldName = FieldName {unFieldName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (Hashable, FromJSONKey, ToJSONKey)

-- | The details of a query against a table
data Query = Query
  { -- | Map of field name to Field definition.
    _qFields :: Maybe (HashMap FieldName Field),
    -- | Map of aggregate field name to Aggregate definition
    _qAggregates :: Maybe (HashMap FieldName API.V0.Aggregate),
    -- | Optionally limit the maximum number of rows considered while applying
    -- aggregations. This limit does not apply to returned rows.
    _qAggregatesLimit :: Maybe Int,
    -- | Optionally limit the maximum number of returned rows. This limit does not
    -- apply to records considered while apply aggregations.
    _qLimit :: Maybe Int,
    -- | Optionally offset from the Nth result. This applies to both row
    -- and aggregation results.
    _qOffset :: Maybe Int,
    -- | Optionally constrain the results to satisfy some predicate.
    _qWhere :: Maybe API.V0.Expression,
    -- | Optionally order the results by the value of one or more fields.
    _qOrderBy :: Maybe API.V0.OrderBy
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Query

instance HasCodec Query where
  codec =
    named "Query" . object "Query" $
      Query
        <$> optionalFieldOrNull "fields" "Fields of the query"
          .= _qFields
        <*> optionalFieldOrNull "aggregates" "Aggregate fields of the query"
          .= _qAggregates
        <*> optionalFieldOrNull "aggregates_limit" "Optionally limit the maximum number of rows considered while applying aggregations. This limit does not apply to returned rows."
          .= _qAggregatesLimit
        <*> optionalFieldOrNull "limit" "Optionally limit the maximum number of returned rows. This limit does not apply to records considered while apply aggregations."
          .= _qLimit
        <*> optionalFieldOrNull "offset" "Optionally offset from the Nth result. This applies to both row and aggregation results."
          .= _qOffset
        <*> optionalFieldOrNull "where" "Optionally constrain the results to satisfy some predicate"
          .= _qWhere
        <*> optionalFieldOrNull "order_by" "Optionally order the results by the value of one or more fields"
          .= _qOrderBy

-- | A relationship consists of the following components:
--   - a sub-query, from the perspective that a relationship field will occur
--     within a broader 'Query'
--   - a join condition relating the data returned by the sub-query with that
--     of the broader 'Query'. This join condition is represented by the
--     name of the relationship that defines the joining criteria.
data RelationshipField = RelationshipField
  { _rfRelationship :: API.V0.RelationshipName,
    _rfQuery :: Query
  }
  deriving stock (Eq, Ord, Show, Generic)

relationshipFieldObjectCodec :: JSONObjectCodec RelationshipField
relationshipFieldObjectCodec =
  RelationshipField
    <$> requiredField "relationship" "The name of the relationship to follow for the subquery"
      .= _rfRelationship
    <*> requiredField "query" "Relationship query"
      .= _rfQuery

-- | The specific fields that are targeted by a 'Query'.
--
-- A field conceptually falls under one of the two following categories:
--   1. a "column" within the data store that the query is being issued against
--   2. a "relationship", which indicates that the field is the result of
--      a subquery
data Field
  = ColumnField API.V0.ColumnName API.V0.ScalarType
  | RelField RelationshipField
  | NestedObjField API.V0.ColumnName Query
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Field

instance HasCodec Field where
  codec =
    named "Field" $
      object "Field" $
        discriminatedUnionCodec "type" enc dec
    where
      columnCodec =
        (,)
          <$> requiredField' "column"
            .= fst
          <*> requiredField' "column_type"
            .= snd
      nestedObjCodec =
        (,)
          <$> requiredField' "column"
            .= fst
          <*> requiredField' "query"
            .= snd
      enc = \case
        ColumnField columnName scalarType -> ("column", mapToEncoder (columnName, scalarType) columnCodec)
        RelField relField -> ("relationship", mapToEncoder relField relationshipFieldObjectCodec)
        NestedObjField columnName nestedObjQuery -> ("object", mapToEncoder (columnName, nestedObjQuery) nestedObjCodec)
      dec =
        HashMap.fromList
          [ ("column", ("ColumnField", mapToDecoder (uncurry ColumnField) columnCodec)),
            ("relationship", ("RelationshipField", mapToDecoder RelField relationshipFieldObjectCodec)),
            ("object", ("NestedObjectField", mapToDecoder (uncurry NestedObjField) nestedObjCodec))
          ]

-- | The resolved query response provided by the 'POST /query'
-- endpoint encoded as a list of JSON objects.
data QueryResponse = QueryResponse
  { _qrRows :: Maybe [HashMap FieldName FieldValue],
    _qrAggregates :: Maybe (HashMap FieldName Value)
  }
  deriving stock (Eq, Ord, Show)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec QueryResponse

instance HasCodec QueryResponse where
  codec =
    named "QueryResponse" . object "QueryResponse" $
      QueryResponse
        <$> optionalFieldOrNull "rows" "The rows returned by the query, corresponding to the query's fields"
          .= _qrRows
        <*> optionalFieldOrNull "aggregates" "The results of the aggregates returned by the query"
          .= _qrAggregates

instance HasStatus QueryResponse where
  type StatusOf QueryResponse = 200

-- | FieldValue represents the value of a field in a 'QueryResponse', which in reality can
-- be two things. One, a column field value which can be any JSON 'J.Value', or two, a
-- relationship field value which can be a 'QueryResponse'.
--
-- Unfortunately, QueryResponse overlaps entirely with 'J.Value', so it is not possible
-- without additional context to know whether a 'FieldValue' is a column field value or a
-- relationship field value. That additional context is the original query, in which
-- we describe whether we expect a field value to be a relationship or a column.
--
-- Why did we allow entirely overlapping types and not have a discriminator? Because it
-- simplifies agent implementation to not have to inject discriminators into the response
-- as it is generated. We have chosen to make this complicated to deal with in HGE as a
-- tradeoff against simplifying agent implementation.
--
-- The 'Eq', 'Ord' and 'Show' instances try to guess whether there is a relationship
-- field value or a column field value by assuming that if the JSON can be deserialized
-- into a 'QueryResponse', then it must be a relationship field value. (This is not
-- strictly true, since an agent could theoretically make a custom field type that could
-- look like a 'QueryResponse'). We do this kludge because when comparing relationship
-- field values (mainly for testing purposes), we must compare them using 'QueryResponse',
-- since raw JSON comparisons will show up immaterial differences between null properties
-- and missing properties (which we consider to be the same thing here).
newtype FieldValue = FieldValue J.Value
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec FieldValue

instance Eq FieldValue where
  l == r = deserializeFieldValueByGuessing l == deserializeFieldValueByGuessing r

instance Ord FieldValue where
  l <= r = deserializeFieldValueByGuessing l <= deserializeFieldValueByGuessing r

instance Show FieldValue where
  showsPrec d fieldValue =
    case deserializeFieldValueByGuessing fieldValue of
      Left (Left columnFieldValue) -> showParen (d > appPrec) $ showString "ColumnFieldValue " . showsPrec appPrec1 columnFieldValue
      Left (Right nestedObjFieldValue) -> showParen (d > appPrec) $ showString "NestedObjFieldValue " . showsPrec appPrec1 nestedObjFieldValue
      Right queryResponse -> showParen (d > appPrec) $ showString "RelationshipFieldValue " . showsPrec appPrec1 queryResponse

mkColumnFieldValue :: J.Value -> FieldValue
mkColumnFieldValue = FieldValue

mkRelationshipFieldValue :: QueryResponse -> FieldValue
mkRelationshipFieldValue = FieldValue . J.toJSON

mkNestedObjFieldValue :: HashMap FieldName FieldValue -> FieldValue
mkNestedObjFieldValue = FieldValue . J.toJSON

mkNestedArrayFieldValue :: [FieldValue] -> FieldValue
mkNestedArrayFieldValue = FieldValue . J.toJSON

deserializeAsColumnFieldValue :: FieldValue -> J.Value
deserializeAsColumnFieldValue (FieldValue value) = value

deserializeAsRelationshipFieldValue :: FieldValue -> Either Text QueryResponse
deserializeAsRelationshipFieldValue (FieldValue value) =
  case J.fromJSON value of
    J.Error s -> Left $ T.pack s
    J.Success queryResponse -> Right queryResponse

deserializeAsNestedObjFieldValue :: FieldValue -> Either Text (HashMap FieldName FieldValue)
deserializeAsNestedObjFieldValue (FieldValue value) =
  case J.fromJSON value of
    J.Error s -> Left $ T.pack s
    J.Success obj -> Right obj

deserializeAsNestedArrayFieldValue :: FieldValue -> Either Text [FieldValue]
deserializeAsNestedArrayFieldValue (FieldValue value) =
  case J.fromJSON value of
    J.Error s -> Left $ T.pack s
    J.Success obj -> Right obj

deserializeFieldValueByGuessing :: FieldValue -> (Either (Either (Either Value (HashMap FieldName FieldValue)) [FieldValue]) QueryResponse)
deserializeFieldValueByGuessing fieldValue =
  left
    ( const $
        left (const $ left (const $ deserializeAsColumnFieldValue fieldValue) $ deserializeAsNestedObjFieldValue fieldValue) $
          deserializeAsNestedArrayFieldValue fieldValue
    )
    $ deserializeAsRelationshipFieldValue fieldValue

-- | Even though we could just describe a FieldValue as "any JSON value", we're explicitly
-- describing it in terms of either a 'QueryResponse' or "any JSON value", in order to
-- make it clear what the options are explicitly.
instance HasCodec FieldValue where
  codec =
    dimapCodec encode decode $
      possiblyJointEitherCodec anyJsonValueCodec (possiblyJointEitherCodec queryResponseCodec nullColumnFieldValue)
    where
      queryResponseCodec :: JSONCodec QueryResponse
      queryResponseCodec = codec

      anyJsonValueCodec :: JSONCodec J.Value
      anyJsonValueCodec = named "ColumnFieldValue" valueCodec

      -- We have to explicitly call out null as a separate named type in OpenAPI
      -- to get the typescript type-generator to recognise null as a valid value here
      nullColumnFieldValue :: JSONCodec ()
      nullColumnFieldValue = named "NullColumnFieldValue" nullCodec

      encode :: Either J.Value (Either QueryResponse ()) -> FieldValue
      encode = FieldValue . either id (either J.toJSON (const J.Null))

      decode :: FieldValue -> Either J.Value (Either QueryResponse ())
      decode = Left . deserializeAsColumnFieldValue

_ColumnFieldValue :: Lens' FieldValue J.Value
_ColumnFieldValue = lens deserializeAsColumnFieldValue (const mkColumnFieldValue)

_RelationshipFieldValue :: Prism' FieldValue QueryResponse
_RelationshipFieldValue = prism' mkRelationshipFieldValue (either (const Nothing) Just . deserializeAsRelationshipFieldValue)

_NestedObjFieldValue :: Prism' FieldValue (HashMap FieldName FieldValue)
_NestedObjFieldValue = prism' mkNestedObjFieldValue (either (const Nothing) Just . deserializeAsNestedObjFieldValue)

_NestedArrayFieldValue :: Prism' FieldValue [FieldValue]
_NestedArrayFieldValue = prism' mkNestedArrayFieldValue (either (const Nothing) Just . deserializeAsNestedArrayFieldValue)

$(makeLenses ''QueryRequest)
$(makeLenses ''Query)
$(makeLenses ''QueryResponse)
$(makePrisms ''FieldValue)
