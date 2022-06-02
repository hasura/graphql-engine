{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.ExpressionSpec
  ( spec,
    genBinaryComparisonOperator,
    genBinaryArrayComparisonOperator,
    genUnaryComparisonOperator,
    genComparisonValue,
    genExpression,
  )
where

import Autodocodec.Extended
import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0.API
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataConnector.API.V0.Scalar.ValueSpec (genValue)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range
import Test.Aeson.Utils (jsonOpenApiProperties, jsonProperties, testToFromJSONToSchema)
import Test.Autodocodec.Extended (genValueWrapper, genValueWrapper2, genValueWrapper3)
import Test.Hspec

spec :: Spec
spec = do
  describe "BinaryComparisonOperator" $ do
    describe "LessThan" $
      testToFromJSONToSchema LessThan [aesonQQ|"less_than"|]

    describe "LessThanOrEqual" $
      testToFromJSONToSchema LessThanOrEqual [aesonQQ|"less_than_or_equal"|]

    describe "GreaterThan" $
      testToFromJSONToSchema GreaterThan [aesonQQ|"greater_than"|]

    describe "GreaterThanOrEqual" $
      testToFromJSONToSchema GreaterThanOrEqual [aesonQQ|"greater_than_or_equal"|]

    describe "Equal" $
      testToFromJSONToSchema Equal [aesonQQ|"equal"|]

    jsonOpenApiProperties genBinaryComparisonOperator

  describe "BinaryArrayComparisonOperator" $ do
    describe "In" $
      testToFromJSONToSchema In [aesonQQ|"in"|]
    jsonOpenApiProperties genBinaryArrayComparisonOperator

  describe "UnaryComparisonOperator" $ do
    describe "IsNull" $
      testToFromJSONToSchema IsNull [aesonQQ|"is_null"|]
    jsonOpenApiProperties genUnaryComparisonOperator

  describe "ComparisonValue" $ do
    describe "AnotherColumn" $
      testToFromJSONToSchema
        (AnotherColumn $ ValueWrapper (ColumnName "my_column_name"))
        [aesonQQ|{"type": "column", "column": "my_column_name"}|]
    describe "ScalarValue" $
      testToFromJSONToSchema
        (ScalarValue . ValueWrapper $ String "scalar value")
        [aesonQQ|{"type": "scalar", "value": "scalar value"}|]

    jsonOpenApiProperties genComparisonValue

  describe "Expression" $ do
    let columnName = ColumnName "my_column_name"
    let scalarValue = ScalarValue . ValueWrapper $ String "scalar value"
    let scalarValues = [ScalarValue . ValueWrapper $ String "scalar value"]
    let unaryComparisonExpression = ApplyUnaryComparisonOperator $ ValueWrapper2 IsNull columnName

    describe "And" $ do
      testToFromJSONToSchema
        (And $ ValueWrapper [unaryComparisonExpression])
        [aesonQQ|
          {
            "type": "and",
            "expressions": [
              {
                "type": "unary_op",
                "operator": "is_null",
                "column": "my_column_name"
              }
            ]
          }
        |]

    describe "Or" $ do
      testToFromJSONToSchema
        (Or $ ValueWrapper [unaryComparisonExpression])
        [aesonQQ|
          {
            "type": "or",
            "expressions": [
              {
                "type": "unary_op",
                "operator": "is_null",
                "column": "my_column_name"
              }
            ]
          }
        |]

    describe "Not" $ do
      testToFromJSONToSchema
        (Not $ ValueWrapper unaryComparisonExpression)
        [aesonQQ|
          {
            "type": "not",
            "expression": {
              "type": "unary_op",
              "operator": "is_null",
              "column": "my_column_name"
            }
          }
        |]
    describe "BinaryComparisonOperator" $ do
      testToFromJSONToSchema
        (ApplyBinaryComparisonOperator $ ValueWrapper3 Equal columnName scalarValue)
        [aesonQQ|
          {
            "type": "binary_op",
            "operator": "equal",
            "column": "my_column_name",
            "value": {"type": "scalar", "value": "scalar value"}
          }
        |]

    describe "BinaryArrayComparisonOperator" $ do
      testToFromJSONToSchema
        (ApplyBinaryArrayComparisonOperator $ ValueWrapper3 In columnName scalarValues)
        [aesonQQ|
          {
            "type": "binary_arr_op",
            "operator": "in",
            "column": "my_column_name",
            "values": [{"type": "scalar", "value": "scalar value"}]
          }
        |]

    describe "UnaryComparisonOperator" $ do
      testToFromJSONToSchema
        unaryComparisonExpression
        [aesonQQ|
          {
            "type": "unary_op",
            "operator": "is_null",
            "column": "my_column_name"
          }
        |]

    -- Note: `validateAgainstOpenApiSchema` is very slow on arbitrary values of type `Expression`.
    -- I think this is a bug in `Data.OpeanApi.Schema.Validation`.
    -- To avoid tests taking too long we use `jsonProperties` instead of `jsonOpenApiProperties` for testing `Expression`s.
    jsonProperties genExpression

genBinaryComparisonOperator :: MonadGen m => m BinaryComparisonOperator
genBinaryComparisonOperator = Gen.enumBounded

genBinaryArrayComparisonOperator :: MonadGen m => m BinaryArrayComparisonOperator
genBinaryArrayComparisonOperator = Gen.enumBounded

genUnaryComparisonOperator :: MonadGen m => m UnaryComparisonOperator
genUnaryComparisonOperator = Gen.enumBounded

genComparisonValue :: MonadGen m => m ComparisonValue
genComparisonValue =
  Gen.choice
    [ AnotherColumn <$> genValueWrapper genColumnName,
      ScalarValue <$> genValueWrapper genValue
    ]

genExpression :: MonadGen m => m Expression
genExpression =
  Gen.choice
    [ And <$> genValueWrapper genExpressions,
      Or <$> genValueWrapper genExpressions,
      Not <$> genValueWrapper genSmallExpression,
      ApplyBinaryComparisonOperator <$> genValueWrapper3 genBinaryComparisonOperator genColumnName genComparisonValue,
      ApplyBinaryArrayComparisonOperator <$> genValueWrapper3 genBinaryArrayComparisonOperator genColumnName (Gen.list (linear 0 1) genComparisonValue),
      ApplyUnaryComparisonOperator <$> genValueWrapper2 genUnaryComparisonOperator genColumnName
    ]
  where
    genExpressions = Gen.list (linear 0 1) genSmallExpression
    genSmallExpression = Gen.small genExpression
