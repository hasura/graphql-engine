{-# LANGUAGE OverloadedLists #-}

module Hasura.Backends.DataConnector.API.V0.Aggregate
  ( Aggregate (..),
    SingleColumnAggregate (..),
    singleColumnAggregateObjectCodec,
    ColumnCountAggregate (..),
    SingleColumnAggregateFunction (..),
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Expression qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Name (nameCodec)
import Hasura.Backends.DataConnector.API.V0.Scalar qualified as API.V0
import Language.GraphQL.Draft.Syntax qualified as GQL
import Prelude

data SingleColumnAggregate = SingleColumnAggregate
  { _scaFunction :: SingleColumnAggregateFunction,
    _scaColumn :: API.V0.ColumnName,
    _scaRedactionExpression :: Maybe API.V0.RedactionExpressionName,
    _scaResultType :: API.V0.ScalarType
  }
  deriving stock (Eq, Ord, Show, Generic)

singleColumnAggregateObjectCodec :: JSONObjectCodec SingleColumnAggregate
singleColumnAggregateObjectCodec =
  SingleColumnAggregate
    <$> requiredField "function" "The aggregation function" .= _scaFunction
    <*> requiredField "column" "The column to apply the aggregation function to" .= _scaColumn
    <*> optionalFieldOrNull "redaction_expression" "If present, the name of the redaction expression to evaluate. If the expression is false, the column value must be nulled out before being aggregated." .= _scaRedactionExpression
    <*> requiredField "result_type" "The scalar type of the result of the aggregate operation" .= _scaResultType

newtype SingleColumnAggregateFunction = SingleColumnAggregateFunction {unSingleColumnAggregateFunction :: GQL.Name}
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SingleColumnAggregateFunction

instance HasCodec SingleColumnAggregateFunction where
  codec =
    named "SingleColumnAggregateFunction" $
      dimapCodec SingleColumnAggregateFunction unSingleColumnAggregateFunction nameCodec
        <?> "Single column aggregate function name."

data ColumnCountAggregate = ColumnCountAggregate
  { _ccaColumn :: API.V0.ColumnName,
    _ccaRedactionExpression :: Maybe API.V0.RedactionExpressionName,
    _ccaDistinct :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

columnCountAggregateObjectCodec :: JSONObjectCodec ColumnCountAggregate
columnCountAggregateObjectCodec =
  ColumnCountAggregate
    <$> requiredField "column" "The column to apply the count aggregate function to" .= _ccaColumn
    <*> optionalFieldOrNull "redaction_expression" "If present, the name of the redaction expression to evaluate. If the expression is false, the column value must be nulled out before being aggregated." .= _ccaRedactionExpression
    <*> requiredField "distinct" "Whether or not only distinct items should be counted" .= _ccaDistinct

data Aggregate
  = SingleColumn SingleColumnAggregate
  | ColumnCount ColumnCountAggregate
  | StarCount
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec Aggregate

instance HasCodec Aggregate where
  codec =
    object "Aggregate" $
      discriminatedUnionCodec "type" enc dec
    where
      enc = \case
        SingleColumn sca -> ("single_column", mapToEncoder sca singleColumnAggregateObjectCodec)
        ColumnCount cca -> ("column_count", mapToEncoder cca columnCountAggregateObjectCodec)
        StarCount -> ("star_count", mapToEncoder () (pureCodec ()))
      dec =
        HashMap.fromList
          [ ("single_column", ("SingleColumnAggregate", mapToDecoder SingleColumn singleColumnAggregateObjectCodec)),
            ("column_count", ("ColumnCountAggregate", mapToDecoder ColumnCount columnCountAggregateObjectCodec)),
            ("star_count", ("StarCountAggregate", mapToDecoder (const StarCount) (pureCodec ())))
          ]
