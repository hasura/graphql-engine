{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataWrapper.API.V0.ExpressionSpec (spec, genOperator, genExpression) where

import Autodocodec.Extended
import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataWrapper.API.V0.API
import Hasura.Backends.DataWrapper.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataWrapper.API.V0.Scalar.ValueSpec (genValue)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range
import Test.Aeson.Utils (jsonOpenApiProperties, jsonProperties, testToFromJSONToSchema)
import Test.Autodocodec.Extended (genValueWrapper, genValueWrapper2, genValueWrapper3)
import Test.Hspec

spec :: Spec
spec = do
  describe "Operator" $ do
    describe "LessThan" $
      testToFromJSONToSchema LessThan [aesonQQ|"less_than"|]
    describe "LessThanOrEqual" $
      testToFromJSONToSchema LessThanOrEqual [aesonQQ|"less_than_or_equal"|]
    describe "GreaterThan" $
      testToFromJSONToSchema GreaterThan [aesonQQ|"greater_than"|]
    describe "GreaterThanOrEqual" $
      testToFromJSONToSchema GreaterThanOrEqual [aesonQQ|"greater_than_or_equal"|]
    jsonOpenApiProperties genOperator
  describe "Expression" $ do
    let lit = Literal $ ValueWrapper Null
    let left = Literal $ ValueWrapper $ String "left"
    let right = Literal $ ValueWrapper $ String "right"
    describe "Literal" $
      testToFromJSONToSchema lit [aesonQQ|{"type": "literal", "value": null}|]
    describe "In" $ do
      testToFromJSONToSchema (In $ ValueWrapper2 lit []) [aesonQQ|{"type": "in", "expression": {"type": "literal", "value": null}, "values": []}|]
    describe "And" $ do
      testToFromJSONToSchema (And $ ValueWrapper []) [aesonQQ|{"type": "and", "expressions": []}|]
    describe "Or" $ do
      testToFromJSONToSchema (Or $ ValueWrapper []) [aesonQQ|{"type": "or", "expressions": []}|]
    describe "Not" $ do
      testToFromJSONToSchema (Not $ ValueWrapper lit) [aesonQQ|{"type": "not", "expression": {"type": "literal", "value": null}}|]
    describe "IsNull" $ do
      testToFromJSONToSchema (IsNull $ ValueWrapper lit) [aesonQQ|{"type": "is_null", "expression": {"type": "literal", "value": null}}|]
    describe "Not" $ do
      testToFromJSONToSchema (IsNotNull $ ValueWrapper lit) [aesonQQ|{"type": "is_not_null", "expression": {"type": "literal", "value": null}}|]
    describe "Column" $ do
      testToFromJSONToSchema (Column $ ValueWrapper $ ColumnName "my_column_name") [aesonQQ|{"type": "column", "column": "my_column_name"}|]
    describe "Equal" $ do
      testToFromJSONToSchema (Equal $ ValueWrapper2 left right) [aesonQQ|{"type": "equal", "left": {"type": "literal", "value": "left"}, "right": {"type": "literal", "value": "right"}}|]
    describe "NotEqual" $ do
      testToFromJSONToSchema (NotEqual $ ValueWrapper2 left right) [aesonQQ|{"type": "not_equal", "left": {"type": "literal", "value": "left"}, "right": {"type": "literal", "value": "right"}}|]
    describe "ApplyOperator" $ do
      testToFromJSONToSchema (ApplyOperator $ ValueWrapper3 LessThan left right) [aesonQQ|{"type": "op", "operator": "less_than", "left": {"type": "literal", "value": "left"}, "right": {"type": "literal", "value": "right"}}|]

    -- Note: `validateAgainstOpenApiSchema` is very slow on arbitrary values of type `Expression`.
    -- I think this is a bug in `Data.OpeanApi.Schema.Validation`.
    -- To avoid tests taking too long we use `jsonProperties` instead of `jsonOpenApiProperties` for testing `Expression`s.
    jsonProperties genExpression

genOperator :: MonadGen m => m Operator
genOperator = Gen.enumBounded

genExpression :: MonadGen m => m Expression
genExpression =
  Gen.choice
    [ Literal <$> genValueWrapper genValue,
      In <$> genValueWrapper2 smallExpression (Gen.list (linear 0 1) genValue),
      And <$> genValueWrapper genExpressions,
      Or <$> genValueWrapper genExpressions,
      Not <$> genValueWrapper smallExpression,
      IsNull <$> genValueWrapper smallExpression,
      IsNotNull <$> genValueWrapper smallExpression,
      Column <$> genValueWrapper genColumnName,
      Equal <$> genValueWrapper2 smallExpression smallExpression,
      NotEqual <$> genValueWrapper2 smallExpression smallExpression,
      ApplyOperator <$> genValueWrapper3 genOperator smallExpression smallExpression
    ]
  where
    genExpressions = Gen.list (linear 0 1) smallExpression
    smallExpression = Gen.small genExpression
