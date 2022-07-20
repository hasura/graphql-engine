{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.AggregateSpec
  ( spec,
    genAggregate,
  )
where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import Test.Aeson.Utils (jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "Aggregate" $ do
    describe "SingleColumn" $ do
      testToFromJSONToSchema
        (SingleColumn $ SingleColumnAggregate Average (ColumnName "my_column_name"))
        [aesonQQ|
          { "type": "single_column",
            "function": "avg",
            "column": "my_column_name"
          }
        |]
    describe "ColumnCount" $ do
      testToFromJSONToSchema
        (ColumnCount $ ColumnCountAggregate [ColumnName "my_column_name"] True)
        [aesonQQ|
          { "type": "column_count",
            "columns": ["my_column_name"],
            "distinct": true
          }
        |]
    describe "StarCount" $ do
      testToFromJSONToSchema
        (StarCount)
        [aesonQQ|
          { "type": "star_count"
          }
        |]
    jsonOpenApiProperties genAggregate

  describe "SingleColumnAggregateFunction" $ do
    describe "Average" $
      testToFromJSONToSchema Average [aesonQQ|"avg"|]
    describe "Max" $
      testToFromJSONToSchema Max [aesonQQ|"max"|]
    describe "Min" $
      testToFromJSONToSchema Min [aesonQQ|"min"|]
    describe "StandardDeviationPopulation" $
      testToFromJSONToSchema StandardDeviationPopulation [aesonQQ|"stddev_pop"|]
    describe "StandardDeviationSample" $
      testToFromJSONToSchema StandardDeviationSample [aesonQQ|"stddev_samp"|]
    describe "Sum" $
      testToFromJSONToSchema Sum [aesonQQ|"sum"|]
    describe "VariancePopulation" $
      testToFromJSONToSchema VariancePopulation [aesonQQ|"var_pop"|]
    describe "VarianceSample" $
      testToFromJSONToSchema VarianceSample [aesonQQ|"var_samp"|]
    jsonOpenApiProperties genSingleColumnAggregateFunction

genAggregate :: MonadGen m => m Aggregate
genAggregate =
  Gen.choice
    [ SingleColumn <$> genSingleColumnAggregate,
      ColumnCount <$> genColumnCountAggregate,
      pure StarCount
    ]

genSingleColumnAggregate :: MonadGen m => m SingleColumnAggregate
genSingleColumnAggregate =
  SingleColumnAggregate
    <$> genSingleColumnAggregateFunction
    <*> genColumnName

genColumnCountAggregate :: MonadGen m => m ColumnCountAggregate
genColumnCountAggregate =
  ColumnCountAggregate
    <$> Gen.nonEmpty (linear 0 5) genColumnName
    <*> Gen.bool

genSingleColumnAggregateFunction :: MonadGen m => m SingleColumnAggregateFunction
genSingleColumnAggregateFunction = Gen.enumBounded
