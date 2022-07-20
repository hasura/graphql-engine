{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Query
  ( QueryRequest (..),
    qrTable,
    qrTableRelationships,
    qrQuery,
    Query (..),
    qFields,
    qAggregates,
    qLimit,
    qOffset,
    qWhere,
    qOrderBy,
    Field (..),
    RelationshipField (..),
    QueryResponse (..),
    qrRows,
    qrAggregates,
    FieldValue (..),
    _ColumnFieldValue,
    _RelationshipFieldValue,
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Aggregate qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Expression qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.OrderBy qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Relationships qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Scalar.Value qualified as API.V0.Scalar
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Prelude

-- | A serializable request to retrieve strutured data from some
-- source.
data QueryRequest = QueryRequest
  { _qrTable :: API.V0.TableName,
    _qrTableRelationships :: [API.V0.TableRelationships],
    _qrQuery :: Query
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

-- | The details of a query against a table
data Query = Query
  { _qFields :: Maybe (KM.KeyMap Field),
    _qAggregates :: Maybe (KM.KeyMap API.V0.Aggregate),
    _qLimit :: Maybe Int,
    _qOffset :: Maybe Int,
    _qWhere :: Maybe API.V0.Expression,
    _qOrderBy :: Maybe (NonEmpty API.V0.OrderBy)
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

data RelationshipField = RelationshipField
  { _rfRelationship :: API.V0.RelationshipName,
    _rfQuery :: Query
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

-- | A serializable field targeted by a 'Query'.
data Field
  = ColumnField (ValueWrapper "column" API.V0.ColumnName)
  | RelField RelationshipField
  deriving stock (Eq, Ord, Show, Generic, Data)

$(makePrisms ''Field)

instance HasCodec Query where
  codec =
    named "Query" . object "Query" $
      Query
        <$> optionalFieldOrNull "fields" "Fields of the query" .= _qFields
        <*> optionalFieldOrNull "aggregates" "Aggregate fields of the query" .= _qAggregates
        <*> optionalFieldOrNull "limit" "Optionally limit to N results" .= _qLimit
        <*> optionalFieldOrNull "offset" "Optionally offset from the Nth result" .= _qOffset
        <*> optionalFieldOrNull "where" "Optionally constrain the results to satisfy some predicate" .= _qWhere
        <*> optionalFieldOrNull "order_by" "Optionally order the results by the value of one or more fields" .= _qOrderBy

deriving via (Autodocodec Query) instance FromJSON Query

deriving via (Autodocodec Query) instance ToJSON Query

deriving via (Autodocodec Query) instance ToSchema Query

instance HasObjectCodec RelationshipField where
  objectCodec =
    RelationshipField
      <$> requiredField "relationship" "The name of the relationship to follow for the subquery" .= _rfRelationship
      <*> requiredField "query" "Relationship query" .= _rfQuery

instance HasCodec QueryRequest where
  codec =
    object "QueryRequest" $
      QueryRequest
        <$> requiredField "table" "The name of the table to query" .= _qrTable
        <*> requiredField "table_relationships" "The relationships between tables involved in the entire query request" .= _qrTableRelationships
        <*> requiredField "query" "The details of the query against the table" .= _qrQuery

deriving via (Autodocodec QueryRequest) instance FromJSON QueryRequest

deriving via (Autodocodec QueryRequest) instance ToJSON QueryRequest

deriving via (Autodocodec QueryRequest) instance ToSchema QueryRequest

instance HasCodec Field where
  codec =
    named "Field" $
      sumTypeCodec
        [ TypeAlternative "ColumnField" "column" _ColumnField,
          TypeAlternative "RelationshipField" "relationship" _RelField
        ]

deriving via Autodocodec Field instance FromJSON Field

deriving via Autodocodec Field instance ToJSON Field

deriving via Autodocodec Field instance ToSchema Field

-- | The resolved query response provided by the 'POST /query'
-- endpoint encoded as a list of JSON objects.
data QueryResponse = QueryResponse
  { _qrRows :: Maybe [KM.KeyMap FieldValue],
    _qrAggregates :: Maybe (KM.KeyMap API.V0.Scalar.Value)
  }
  deriving stock (Eq, Ord, Show)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec QueryResponse

instance HasCodec QueryResponse where
  codec =
    object "QueryResponse" $
      QueryResponse
        <$> optionalFieldOrNull "rows" "The rows returned by the query, corresponding to the query's fields" .= _qrRows
        <*> optionalFieldOrNull "aggregates" "The results of the aggregates returned by the query" .= _qrAggregates

data FieldValue
  = ColumnFieldValue (ValueWrapper "value" API.V0.Scalar.Value)
  | RelationshipFieldValue (ValueWrapper "value" QueryResponse)
  deriving stock (Eq, Ord, Show)

$(makePrisms ''FieldValue)

instance HasCodec FieldValue where
  codec =
    named "FieldValue" $
      sumTypeCodec
        [ TypeAlternative "ColumnFieldValue" "column" _ColumnFieldValue,
          TypeAlternative "RelationshipFieldValue" "relationship" _RelationshipFieldValue
        ]

deriving via Autodocodec FieldValue instance FromJSON FieldValue

deriving via Autodocodec FieldValue instance ToJSON FieldValue

deriving via Autodocodec FieldValue instance ToSchema FieldValue

$(makeLenses ''QueryRequest)
$(makeLenses ''Query)
$(makeLenses ''QueryResponse)
