{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataConnector.IR.Aggregate
  ( Aggregate (..),
    SingleColumnAggregate (..),
    SingleColumnAggregateFunction (..),
    CountAggregate (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch qualified

data Aggregate
  = SingleColumn SingleColumnAggregate
  | Count CountAggregate
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From Aggregate API.Aggregate where
  from (SingleColumn singleColumn) = API.SingleColumn (Witch.from singleColumn)
  from (Count StarCount) = API.StarCount
  from (Count (ColumnCount columns)) = API.ColumnCount $ API.ColumnCountAggregate {_ccaColumns = Witch.from <$> columns, _ccaDistinct = False}
  from (Count (ColumnDistinctCount columns)) = API.ColumnCount $ API.ColumnCountAggregate {_ccaColumns = Witch.from <$> columns, _ccaDistinct = True}

data SingleColumnAggregate = SingleColumnAggregate
  { _scaFunction :: SingleColumnAggregateFunction,
    _scaColumn :: IR.C.Name
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From SingleColumnAggregate API.SingleColumnAggregate where
  from SingleColumnAggregate {..} = API.SingleColumnAggregate (Witch.from _scaFunction) (Witch.from _scaColumn)

data SingleColumnAggregateFunction
  = Average
  | Max
  | Min
  | StandardDeviationPopulation
  | StandardDeviationSample
  | Sum
  | VariancePopulation
  | VarianceSample
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance Witch.From API.SingleColumnAggregateFunction SingleColumnAggregateFunction where
  from API.Average = Average
  from API.Max = Max
  from API.Min = Min
  from API.StandardDeviationPopulation = StandardDeviationPopulation
  from API.StandardDeviationSample = StandardDeviationSample
  from API.Sum = Sum
  from API.VariancePopulation = VariancePopulation
  from API.VarianceSample = VarianceSample

instance Witch.From SingleColumnAggregateFunction API.SingleColumnAggregateFunction where
  from Average = API.Average
  from Max = API.Max
  from Min = API.Min
  from StandardDeviationPopulation = API.StandardDeviationPopulation
  from StandardDeviationSample = API.StandardDeviationSample
  from Sum = API.Sum
  from VariancePopulation = API.VariancePopulation
  from VarianceSample = API.VarianceSample

data CountAggregate
  = StarCount
  | ColumnCount (NonEmpty IR.C.Name)
  | ColumnDistinctCount (NonEmpty IR.C.Name)
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)
