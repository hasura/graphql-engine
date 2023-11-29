{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.ExpressionSpec
  ( spec,
    genBinaryComparisonOperator,
    genBinaryArrayComparisonOperator,
    genUnaryComparisonOperator,
    genComparisonValue,
    genExpression,
    genRedactionExpressionName,
    genTargetRedactionExpressions,
  )
where

import Data.Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnSelector)
import Hasura.Backends.DataConnector.API.V0.RelationshipsSpec (genRelationshipName)
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType, genScalarValue)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableName)
import Hasura.Backends.DataConnector.API.V0.TargetSpec (genTargetName)
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumText, genArbitraryAlphaNumTextExcluding, genHashMap)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils (genValue, jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "BinaryComparisonOperator" $ do
    describe "LessThan"
      $ testToFromJSONToSchema LessThan [aesonQQ|"less_than"|]

    describe "LessThanOrEqual"
      $ testToFromJSONToSchema LessThanOrEqual [aesonQQ|"less_than_or_equal"|]

    describe "GreaterThan"
      $ testToFromJSONToSchema GreaterThan [aesonQQ|"greater_than"|]

    describe "GreaterThanOrEqual"
      $ testToFromJSONToSchema GreaterThanOrEqual [aesonQQ|"greater_than_or_equal"|]

    describe "Equal"
      $ testToFromJSONToSchema Equal [aesonQQ|"equal"|]

    describe "CustomBinaryComparisonOperator"
      $ testToFromJSONToSchema (CustomBinaryComparisonOperator "foo") [aesonQQ|"foo"|]

    jsonOpenApiProperties genBinaryComparisonOperator

  describe "BinaryArrayComparisonOperator" $ do
    describe "In"
      $ testToFromJSONToSchema In [aesonQQ|"in"|]

    describe "CustomBinaryArrayComparisonOperator"
      $ testToFromJSONToSchema (CustomBinaryArrayComparisonOperator "foo") [aesonQQ|"foo"|]

    jsonOpenApiProperties genBinaryArrayComparisonOperator

  describe "UnaryComparisonOperator" $ do
    describe "IsNull"
      $ testToFromJSONToSchema IsNull [aesonQQ|"is_null"|]

    describe "CustomUnaryComparisonOperator"
      $ testToFromJSONToSchema (CustomUnaryComparisonOperator "foo") [aesonQQ|"foo"|]

    jsonOpenApiProperties genUnaryComparisonOperator

  describe "ComparisonColumn" $ do
    testToFromJSONToSchema
      (ComparisonColumn QueryTable (mkColumnSelector $ ColumnName "column_name") (ScalarType "string") (Just $ RedactionExpressionName "RedactionExp1"))
      [aesonQQ|{"path": ["$"], "name": "column_name", "column_type": "string", "redaction_expression": "RedactionExp1"}|]

    jsonOpenApiProperties genComparisonColumn

  describe "ColumnPath" $ do
    describe "QueryTable"
      $ testToFromJSONToSchema QueryTable [aesonQQ|["$"]|]
    describe "CurrentTable"
      $ testToFromJSONToSchema CurrentTable [aesonQQ|[]|]
    jsonOpenApiProperties genColumnPath

  describe "ComparisonValue" $ do
    describe "AnotherColumnComparison"
      $ testToFromJSONToSchema
        (AnotherColumnComparison $ ComparisonColumn CurrentTable (mkColumnSelector $ ColumnName "my_column_name") (ScalarType "string") Nothing)
        [aesonQQ|{"type": "column", "column": {"name": "my_column_name", "column_type": "string"}}|]
    describe "ScalarValueComparison"
      $ testToFromJSONToSchema
        (ScalarValueComparison $ ScalarValue (String "scalar value") (ScalarType "string"))
        [aesonQQ|{"type": "scalar", "value": "scalar value", "value_type": "string"}|]

    jsonOpenApiProperties genComparisonValue

  describe "ExistsInTable" $ do
    describe "RelatedTable"
      $ testToFromJSONToSchema
        (RelatedTable (RelationshipName "my_relation"))
        [aesonQQ|
          { "type": "related",
            "relationship": "my_relation"
          }
        |]
    describe "UnrelatedTable"
      $ testToFromJSONToSchema
        (UnrelatedTable (TableName ["my_table_name"]))
        [aesonQQ|
          { "type": "unrelated",
            "table": ["my_table_name"]
          }
        |]
    jsonOpenApiProperties genExistsInTable

  describe "Expression" $ do
    let comparisonColumn = ComparisonColumn CurrentTable (mkColumnSelector $ ColumnName "my_column_name") (ScalarType "string") Nothing
    let scalarValue = ScalarValueComparison $ ScalarValue (String "scalar value") (ScalarType "string")
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
                "column": { "name": "my_column_name", "column_type": "string" }
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
                "column": { "name": "my_column_name", "column_type": "string" }
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
              "column": { "name": "my_column_name", "column_type": "string" }
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
              "column": { "name": "my_column_name", "column_type": "string" }
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
            "column": { "name": "my_column_name", "column_type": "string" },
            "value": {"type": "scalar", "value": "scalar value", "value_type": "string"}
          }
        |]

    describe "BinaryArrayComparisonOperator" $ do
      testToFromJSONToSchema
        (ApplyBinaryArrayComparisonOperator In comparisonColumn scalarValues (ScalarType "string"))
        [aesonQQ|
          {
            "type": "binary_arr_op",
            "operator": "in",
            "column": { "name": "my_column_name", "column_type": "string" },
            "values": ["scalar value"],
            "value_type": "string"
          }
        |]

    describe "UnaryComparisonOperator" $ do
      testToFromJSONToSchema
        unaryComparisonExpression
        [aesonQQ|
          {
            "type": "unary_op",
            "operator": "is_null",
            "column": { "name": "my_column_name", "column_type": "string" }
          }
        |]

    jsonOpenApiProperties genExpression

  describe "RedactionExpressionName" $ do
    testToFromJSONToSchema (RedactionExpressionName "foo") [aesonQQ|"foo"|]
    jsonOpenApiProperties genRedactionExpressionName

  describe "TargetRedactionExpressions" $ do
    testToFromJSONToSchema
      (TargetRedactionExpressions (TNTable $ TableName ["my_table_name"]) mempty)
      [aesonQQ|
        { "target": { "type": "table", "table": ["my_table_name"] },
          "expressions": {}
        }
      |]
    jsonOpenApiProperties genTargetRedactionExpressions

  describe "RedactionExpression" $ do
    testToFromJSONToSchema
      (RedactionExpression $ And [])
      [aesonQQ|
        { "type": "and",
          "expressions": []
        }
      |]
    jsonOpenApiProperties genRedactionExpression

genBinaryComparisonOperator :: (MonadGen m, GenBase m ~ Identity) => m BinaryComparisonOperator
genBinaryComparisonOperator =
  Gen.choice
    [ Gen.element [LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual, Equal],
      CustomBinaryComparisonOperator
        <$> genArbitraryAlphaNumTextExcluding
          ["less_than", "less_than_or_equal", "greater_than", "greater_than_or_equal", "equal"]
          defaultRange
    ]

genBinaryArrayComparisonOperator :: (MonadGen m, GenBase m ~ Identity) => m BinaryArrayComparisonOperator
genBinaryArrayComparisonOperator =
  Gen.choice
    [ pure In,
      CustomBinaryArrayComparisonOperator <$> genArbitraryAlphaNumTextExcluding ["in"] defaultRange
    ]

genUnaryComparisonOperator :: (MonadGen m, GenBase m ~ Identity) => m UnaryComparisonOperator
genUnaryComparisonOperator =
  Gen.choice
    [ pure IsNull,
      CustomUnaryComparisonOperator <$> genArbitraryAlphaNumTextExcluding ["is_null"] defaultRange
    ]

genComparisonColumn :: (MonadGen m, GenBase m ~ Identity) => m ComparisonColumn
genComparisonColumn =
  ComparisonColumn
    <$> genColumnPath
    <*> genColumnSelector
    <*> genScalarType
    <*> Gen.maybe genRedactionExpressionName

genColumnPath :: (MonadGen m) => m ColumnPath
genColumnPath =
  Gen.element [CurrentTable, QueryTable]

genComparisonValue :: (MonadGen m, GenBase m ~ Identity) => m ComparisonValue
genComparisonValue =
  Gen.choice
    [ AnotherColumnComparison <$> genComparisonColumn,
      ScalarValueComparison <$> genScalarValue
    ]

genExistsInTable :: (MonadGen m) => m ExistsInTable
genExistsInTable =
  Gen.choice
    [ RelatedTable <$> genRelationshipName,
      UnrelatedTable <$> genTableName
    ]

genExpression :: (MonadGen m, GenBase m ~ Identity) => m Expression
genExpression =
  Gen.recursive
    Gen.choice
    [ ApplyBinaryComparisonOperator <$> genBinaryComparisonOperator <*> genComparisonColumn <*> genComparisonValue,
      ApplyBinaryArrayComparisonOperator <$> genBinaryArrayComparisonOperator <*> genComparisonColumn <*> (Gen.list defaultRange genValue) <*> genScalarType,
      ApplyUnaryComparisonOperator <$> genUnaryComparisonOperator <*> genComparisonColumn
    ]
    [ And <$> genExpressions,
      Or <$> genExpressions,
      Not <$> genExpression,
      Exists <$> genExistsInTable <*> genExpression
    ]
  where
    genExpressions = Gen.set defaultRange genExpression

genRedactionExpressionName :: (MonadGen m) => m RedactionExpressionName
genRedactionExpressionName = RedactionExpressionName <$> genArbitraryAlphaNumText defaultRange

genTargetRedactionExpressions :: (MonadGen m, GenBase m ~ Identity) => m TargetRedactionExpressions
genTargetRedactionExpressions =
  TargetRedactionExpressions
    <$> genTargetName
    <*> genHashMap genRedactionExpressionName genRedactionExpression defaultRange

genRedactionExpression :: (MonadGen m, GenBase m ~ Identity) => m RedactionExpression
genRedactionExpression = RedactionExpression <$> genExpression
