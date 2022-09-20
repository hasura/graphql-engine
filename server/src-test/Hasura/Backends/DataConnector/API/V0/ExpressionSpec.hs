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

import Data.Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataConnector.API.V0.RelationshipsSpec (genRelationshipName)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableName)
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumText)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils (genValue, jsonOpenApiProperties, testToFromJSONToSchema)
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

    describe "CustomBinaryComparisonOperator" $
      testToFromJSONToSchema (CustomBinaryComparisonOperator "foo") [aesonQQ|"foo"|]

    jsonOpenApiProperties genBinaryComparisonOperator

  describe "BinaryArrayComparisonOperator" $ do
    describe "In" $
      testToFromJSONToSchema In [aesonQQ|"in"|]

    describe "CustomBinaryArrayComparisonOperator" $
      testToFromJSONToSchema (CustomBinaryArrayComparisonOperator "foo") [aesonQQ|"foo"|]

    jsonOpenApiProperties genBinaryArrayComparisonOperator

  describe "UnaryComparisonOperator" $ do
    describe "IsNull" $
      testToFromJSONToSchema IsNull [aesonQQ|"is_null"|]

    describe "CustomUnaryComparisonOperator" $
      testToFromJSONToSchema (CustomUnaryComparisonOperator "foo") [aesonQQ|"foo"|]

    jsonOpenApiProperties genUnaryComparisonOperator

  describe "ComparisonColumn" $ do
    testToFromJSONToSchema
      (ComparisonColumn QueryTable (ColumnName "column_name"))
      [aesonQQ|{"path": ["$"], "name": "column_name"}|]

    jsonOpenApiProperties genComparisonColumn

  describe "ColumnPath" $ do
    describe "QueryTable" $
      testToFromJSONToSchema QueryTable [aesonQQ|["$"]|]
    describe "CurrentTable" $
      testToFromJSONToSchema CurrentTable [aesonQQ|[]|]
    jsonOpenApiProperties genColumnPath

  describe "ComparisonValue" $ do
    describe "AnotherColumn" $
      testToFromJSONToSchema
        (AnotherColumn $ ComparisonColumn CurrentTable (ColumnName "my_column_name"))
        [aesonQQ|{"type": "column", "column": {"name": "my_column_name"}}|]
    describe "ScalarValue" $
      testToFromJSONToSchema
        (ScalarValue $ String "scalar value")
        [aesonQQ|{"type": "scalar", "value": "scalar value"}|]

    jsonOpenApiProperties genComparisonValue

  describe "ExistsInTable" $ do
    describe "RelatedTable" $
      testToFromJSONToSchema
        (RelatedTable (RelationshipName "my_relation"))
        [aesonQQ|
          { "type": "related",
            "relationship": "my_relation"
          }
        |]
    describe "UnrelatedTable" $
      testToFromJSONToSchema
        (UnrelatedTable (TableName ["my_table_name"]))
        [aesonQQ|
          { "type": "unrelated",
            "table": ["my_table_name"]
          }
        |]
    jsonOpenApiProperties genExistsInTable

  describe "Expression" $ do
    let comparisonColumn = ComparisonColumn CurrentTable (ColumnName "my_column_name")
    let scalarValue = ScalarValue $ String "scalar value"
    let scalarValues = [String "scalar value"]
    let unaryComparisonExpression = ApplyUnaryComparisonOperator IsNull comparisonColumn

    describe "And" $ do
      testToFromJSONToSchema
        (And [unaryComparisonExpression])
        [aesonQQ|
          {
            "type": "and",
            "expressions": [
              {
                "type": "unary_op",
                "operator": "is_null",
                "column": { "name": "my_column_name" }
              }
            ]
          }
        |]

    describe "Or" $ do
      testToFromJSONToSchema
        (Or [unaryComparisonExpression])
        [aesonQQ|
          {
            "type": "or",
            "expressions": [
              {
                "type": "unary_op",
                "operator": "is_null",
                "column": { "name": "my_column_name" }
              }
            ]
          }
        |]

    describe "Not" $ do
      testToFromJSONToSchema
        (Not unaryComparisonExpression)
        [aesonQQ|
          {
            "type": "not",
            "expression": {
              "type": "unary_op",
              "operator": "is_null",
              "column": { "name": "my_column_name" }
            }
          }
        |]

    describe "Exists" $ do
      testToFromJSONToSchema
        (Exists (RelatedTable (RelationshipName "relation")) unaryComparisonExpression)
        [aesonQQ|
          {
            "type": "exists",
            "in_table": {
              "type": "related",
              "relationship": "relation"
            },
            "where": {
              "type": "unary_op",
              "operator": "is_null",
              "column": { "name": "my_column_name" }
            }
          }
        |]

    describe "BinaryComparisonOperator" $ do
      testToFromJSONToSchema
        (ApplyBinaryComparisonOperator Equal comparisonColumn scalarValue)
        [aesonQQ|
          {
            "type": "binary_op",
            "operator": "equal",
            "column": { "name": "my_column_name" },
            "value": {"type": "scalar", "value": "scalar value"}
          }
        |]

    describe "BinaryArrayComparisonOperator" $ do
      testToFromJSONToSchema
        (ApplyBinaryArrayComparisonOperator In comparisonColumn scalarValues)
        [aesonQQ|
          {
            "type": "binary_arr_op",
            "operator": "in",
            "column": { "name": "my_column_name" },
            "values": ["scalar value"]
          }
        |]

    describe "UnaryComparisonOperator" $ do
      testToFromJSONToSchema
        unaryComparisonExpression
        [aesonQQ|
          {
            "type": "unary_op",
            "operator": "is_null",
            "column": { "name": "my_column_name" }
          }
        |]

    jsonOpenApiProperties genExpression

genBinaryComparisonOperator :: MonadGen m => m BinaryComparisonOperator
genBinaryComparisonOperator =
  Gen.choice
    [ Gen.element [LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Equal],
      CustomBinaryComparisonOperator <$> genArbitraryAlphaNumText defaultRange
    ]

genBinaryArrayComparisonOperator :: MonadGen m => m BinaryArrayComparisonOperator
genBinaryArrayComparisonOperator =
  Gen.choice
    [ pure In,
      CustomBinaryArrayComparisonOperator <$> genArbitraryAlphaNumText defaultRange
    ]

genUnaryComparisonOperator :: MonadGen m => m UnaryComparisonOperator
genUnaryComparisonOperator =
  Gen.choice
    [ pure IsNull,
      CustomUnaryComparisonOperator <$> genArbitraryAlphaNumText defaultRange
    ]

genComparisonColumn :: MonadGen m => m ComparisonColumn
genComparisonColumn =
  ComparisonColumn
    <$> genColumnPath
    <*> genColumnName

genColumnPath :: MonadGen m => m ColumnPath
genColumnPath =
  Gen.element [CurrentTable, QueryTable]

genComparisonValue :: MonadGen m => m ComparisonValue
genComparisonValue =
  Gen.choice
    [ AnotherColumn <$> genComparisonColumn,
      ScalarValue <$> genValue
    ]

genExistsInTable :: MonadGen m => m ExistsInTable
genExistsInTable =
  Gen.choice
    [ RelatedTable <$> genRelationshipName,
      UnrelatedTable <$> genTableName
    ]

genExpression :: MonadGen m => m Expression
genExpression =
  Gen.recursive
    Gen.choice
    [ ApplyBinaryComparisonOperator <$> genBinaryComparisonOperator <*> genComparisonColumn <*> genComparisonValue,
      ApplyBinaryArrayComparisonOperator <$> genBinaryArrayComparisonOperator <*> genComparisonColumn <*> (Gen.list defaultRange genValue),
      ApplyUnaryComparisonOperator <$> genUnaryComparisonOperator <*> genComparisonColumn
    ]
    [ And <$> genExpressions,
      Or <$> genExpressions,
      Not <$> genExpression,
      Exists <$> genExistsInTable <*> genExpression
    ]
  where
    genExpressions = Gen.list defaultRange genExpression
