{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Aggregate
  ( Aggregate (..),
    SingleColumnAggregate (..),
    ColumnCountAggregate (..),
    SingleColumnAggregateFunction (..),
  )
where

import Autodocodec.Extended
import Control.Lens.TH (makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Prelude

data SingleColumnAggregate = SingleColumnAggregate
  { _scaFunction :: SingleColumnAggregateFunction,
    _scaColumn :: API.V0.ColumnName
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasObjectCodec SingleColumnAggregate where
  objectCodec =
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
      disjointStringConstCodec
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
  { _ccaColumns :: NonEmpty API.V0.ColumnName,
    _ccaDistinct :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic, Data)

instance HasObjectCodec ColumnCountAggregate where
  objectCodec =
    ColumnCountAggregate
      <$> requiredField "columns" "The columns to apply the count aggregate function to" .= _ccaColumns
      <*> requiredField "distinct" "Whether or not only distinct items should be counted" .= _ccaDistinct

data Aggregate
  = SingleColumn SingleColumnAggregate
  | ColumnCount ColumnCountAggregate
  | StarCount
  deriving stock (Eq, Ord, Show, Generic, Data)

$(makePrisms ''Aggregate)

instance HasCodec Aggregate where
  codec =
    named "Aggregate" $
      sumTypeCodec
        [ TypeAlternative "SingleColumnAggregate" "single_column" _SingleColumn,
          TypeAlternative "ColumnCountAggregate" "column_count" _ColumnCount,
          TypeAlternative "StarCountAggregate" "star_count" _StarCount
        ]

deriving via Autodocodec Aggregate instance FromJSON Aggregate

deriving via Autodocodec Aggregate instance ToJSON Aggregate

deriving via Autodocodec Aggregate instance ToSchema Aggregate
