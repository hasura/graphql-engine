{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.AggregateSpec
  ( spec,
    genAggregate,
    genSingleColumnAggregate,
  )
where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataConnector.API.V0.ExpressionSpec (genRedactionExpressionName)
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Language.GraphQL.Draft.Generator (genName)
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.Aeson.Utils (jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "Aggregate" $ do
    describe "SingleColumn" $ do
      testToFromJSONToSchema
        (SingleColumn $ SingleColumnAggregate (SingleColumnAggregateFunction [G.name|avg|]) (ColumnName "my_column_name") (Just $ RedactionExpressionName "RedactionExp2") (ScalarType "number"))
        [aesonQQ|
          { "type": "single_column",
            "function": "avg",
            "column": "my_column_name",
            "redaction_expression": "RedactionExp2",
            "result_type": "number"
          }
        |]
    describe "ColumnCount" $ do
      testToFromJSONToSchema
        (ColumnCount $ ColumnCountAggregate (ColumnName "my_column_name") (Just $ RedactionExpressionName "RedactionExp2") True)
        [aesonQQ|
          { "type": "column_count",
            "column": "my_column_name",
            "redaction_expression": "RedactionExp2",
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
    testToFromJSONToSchema (SingleColumnAggregateFunction [G.name|avg|]) [aesonQQ|"avg"|]
    jsonOpenApiProperties genSingleColumnAggregateFunction

genAggregate :: Gen Aggregate
genAggregate =
  Gen.choice
    [ SingleColumn <$> genSingleColumnAggregate,
      ColumnCount <$> genColumnCountAggregate,
      pure StarCount
    ]

genSingleColumnAggregate :: Gen SingleColumnAggregate
genSingleColumnAggregate =
  SingleColumnAggregate
    <$> genSingleColumnAggregateFunction
    <*> genColumnName
    <*> Gen.maybe genRedactionExpressionName
    <*> genScalarType

genColumnCountAggregate :: Gen ColumnCountAggregate
genColumnCountAggregate =
  ColumnCountAggregate
    <$> genColumnName
    <*> Gen.maybe genRedactionExpressionName
    <*> Gen.bool

genSingleColumnAggregateFunction :: Gen SingleColumnAggregateFunction
genSingleColumnAggregateFunction = SingleColumnAggregateFunction <$> genName
