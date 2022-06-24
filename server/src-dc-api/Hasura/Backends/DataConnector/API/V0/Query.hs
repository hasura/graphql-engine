{-# LANGUAGE DeriveAnyClass #-}
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
    qLimit,
    qOffset,
    qWhere,
    qOrderBy,
    Field (..),
    RelationshipField (..),
    QueryResponse (..),
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.DeepSeq (NFData)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Aeson (FromJSON, Object, ToJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Expression qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.OrderBy qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Relationships qualified as API.V0
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
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec QueryRequest

instance HasCodec QueryRequest where
  codec =
    object "QueryRequest" $
      QueryRequest
        <$> requiredField "table" "The name of the table to query" .= _qrTable
        <*> requiredField "table_relationships" "The relationships between tables involved in the entire query request" .= _qrTableRelationships
        <*> requiredField "query" "The details of the query against the table" .= _qrQuery

-- | The details of a query against a table
data Query = Query
  { _qFields :: KM.KeyMap Field,
    _qLimit :: Maybe Int,
    _qOffset :: Maybe Int,
    _qWhere :: Maybe API.V0.Expression,
    _qOrderBy :: Maybe (NonEmpty API.V0.OrderBy)
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Query

instance HasCodec Query where
  codec =
    named "Query" . object "Query" $
      Query
        <$> requiredField "fields" "Fields of the query" .= _qFields
        <*> optionalFieldOrNull "limit" "Optionally limit to N results" .= _qLimit
        <*> optionalFieldOrNull "offset" "Optionally offset from the Nth result" .= _qOffset
        <*> optionalFieldOrNull "where" "Optionally constrain the results to satisfy some predicate" .= _qWhere
        <*> optionalFieldOrNull "order_by" "Optionally order the results by the value of one or more fields" .= _qOrderBy

data RelationshipField = RelationshipField
  { _rfRelationship :: API.V0.RelationshipName,
    _rfQuery :: Query
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasObjectCodec RelationshipField where
  objectCodec =
    RelationshipField
      <$> requiredField "relationship" "The name of the relationship to follow for the subquery" .= _rfRelationship
      <*> requiredField "query" "Relationship query" .= _rfQuery

-- | A serializable field targeted by a 'Query'.
data Field
  = ColumnField (ValueWrapper "column" API.V0.ColumnName)
  | RelField RelationshipField
  deriving stock (Eq, Ord, Show, Generic, Data)

$(makePrisms ''Field)

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
newtype QueryResponse = QueryResponse {getQueryResponse :: [Object]}
  deriving newtype (Eq, Ord, Show, NFData)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec [Object]

$(makeLenses ''QueryRequest)
$(makeLenses ''Query)
