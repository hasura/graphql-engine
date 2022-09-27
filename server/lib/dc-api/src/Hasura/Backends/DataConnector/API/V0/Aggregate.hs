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
import Prelude

data SingleColumnAggregate = SingleColumnAggregate
  { _scaFunction :: SingleColumnAggregateFunction,
    _scaColumn :: API.V0.ColumnName
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

singleColumnAggregateObjectCodec :: JSONObjectCodec SingleColumnAggregate
singleColumnAggregateObjectCodec =
  SingleColumnAggregate
    <$> requiredField "function" "The aggregation function" .= _scaFunction
    <*> requiredField "column" "The column to apply the aggregation function to" .= _scaColumn

data SingleColumnAggregateFunction
  = Average
  | Max
  | Min
  | StandardDeviationPopulation
  | StandardDeviationSample
  | Sum
  | VariancePopulation
  | VarianceSample
  deriving stock (Eq, Ord, Show, Generic, Data, Enum, Bounded)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec SingleColumnAggregateFunction

instance HasCodec SingleColumnAggregateFunction where
  codec =
    named "SingleColumnAggregateFunction" $
      stringConstCodec
        [ (Average, "avg"),
          (Max, "max"),
          (Min, "min"),
          (StandardDeviationPopulation, "stddev_pop"),
          (StandardDeviationSample, "stddev_samp"),
          (Sum, "sum"),
          (VariancePopulation, "var_pop"),
          (VarianceSample, "var_samp")
        ]

data ColumnCountAggregate = ColumnCountAggregate
  { _ccaColumn :: API.V0.ColumnName,
    _ccaDistinct :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

columnCountAggregateObjectCodec :: JSONObjectCodec ColumnCountAggregate
columnCountAggregateObjectCodec =
  ColumnCountAggregate
    <$> requiredField "column" "The column to apply the count aggregate function to" .= _ccaColumn
    <*> requiredField "distinct" "Whether or not only distinct items should be counted" .= _ccaDistinct

data Aggregate
  = SingleColumn SingleColumnAggregate
  | ColumnCount ColumnCountAggregate
  | StarCount
  deriving stock (Eq, Ord, Show, Generic, Data)
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
