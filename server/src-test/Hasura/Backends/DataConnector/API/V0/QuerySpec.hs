{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.QuerySpec
  ( spec,
    genFieldName,
    genFieldMap,
    genField,
    genFieldValue,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.AggregateSpec (genAggregate)
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataConnector.API.V0.ExpressionSpec (genExpression, genRedactionExpressionName, genTargetRedactionExpressions)
import Hasura.Backends.DataConnector.API.V0.FunctionSpec (genFunctionArgument, genFunctionName)
import Hasura.Backends.DataConnector.API.V0.OrderBySpec (genOrderBy)
import Hasura.Backends.DataConnector.API.V0.RelationshipsSpec (genRelationshipName, genRelationships)
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType, genScalarValue)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableTarget)
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumText, genHashMap)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils (genValue, jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "Field" $ do
    describe "ColumnField"
      $ testToFromJSONToSchema
        (ColumnField (ColumnName "my_column_name") (ScalarType "string") (Just $ RedactionExpressionName "RedactionExp0"))
        [aesonQQ|
          { "type": "column",
            "column": "my_column_name",
            "column_type": "string",
            "redaction_expression": "RedactionExp0"
          }
        |]
    describe "RelationshipField" $ do
      let query = Query (Just mempty) Nothing Nothing Nothing Nothing Nothing Nothing
      testToFromJSONToSchema
        (RelField $ RelationshipField (RelationshipName "a_relationship") query)
        [aesonQQ|
          { "type": "relationship",
            "relationship": "a_relationship",
            "query": {"fields": {}}
          }
        |]
    jsonOpenApiProperties genField

  describe "Query" $ do
    let query =
          Query
            { _qFields = Just $ HashMap.fromList [(FieldName "my_field_alias", ColumnField (ColumnName "my_field_name") (ScalarType "string") Nothing)],
              _qAggregates = Just $ HashMap.fromList [(FieldName "my_aggregate", StarCount)],
              _qAggregatesLimit = Just 5,
              _qLimit = Just 10,
              _qOffset = Just 20,
              _qWhere = Just $ And [],
              _qOrderBy = Just $ OrderBy [] (OrderByElement [] (OrderByColumn (mkColumnSelector $ ColumnName "my_column_name") Nothing) Ascending :| [])
            }
    testToFromJSONToSchema
      query
      [aesonQQ|
        { "fields": {"my_field_alias": {"type": "column", "column": "my_field_name", "column_type": "string"}},
          "aggregates": { "my_aggregate": { "type": "star_count" } },
          "aggregates_limit": 5,
          "limit": 10,
          "offset": 20,
          "where": {"type": "and", "expressions": []},
          "order_by": {
            "relations": {},
            "elements": [
              { "target_path": [],
                "target": {
                  "type": "column",
                  "column": "my_column_name"
                },
                "order_direction": "asc"
              }
            ]
          }
        }
      |]
    jsonOpenApiProperties genQuery

  describe "TableRequest" $ do
    let queryRequest =
          QueryRequest
            { _qrTarget = TTable (TargetTable (TableName ["my_table"])),
              _qrRelationships = [],
              _qrRedactionExpressions = [],
              _qrInterpolatedQueries = mempty,
              _qrQuery = Query (Just mempty) Nothing Nothing Nothing Nothing Nothing Nothing,
              _qrForeach = Just (HashMap.fromList [(ColumnName "my_id", ScalarValue (J.Number 666) (ScalarType "number"))] :| [])
            }
    testToFromJSONToSchema
      queryRequest
      [aesonQQ|
        { "target": {
            "type": "table",
            "name": ["my_table"]
          },
          "relationships": [],
          "query": { "fields": {} },
          "foreach": [
            { "my_id": { "value": 666, "value_type": "number" } }
          ]
        }
      |]
    jsonOpenApiProperties genQueryRequest

  describe "FunctionRequest" $ do
    let queryRequest =
          QueryRequest
            { _qrTarget = TFunction (TargetFunction (FunctionName ["my_function"]) []),
              _qrRelationships = [],
              _qrRedactionExpressions = [],
              _qrInterpolatedQueries = mempty,
              _qrForeach = Nothing,
              _qrQuery = Query (Just mempty) Nothing Nothing Nothing Nothing Nothing Nothing
            }
    testToFromJSONToSchema
      queryRequest
      [aesonQQ|
        { "target": {
            "type": "function",
              "name": ["my_function"],
              "arguments": []
          },
          "relationships": [],
          "query": { "fields": {} }
        }
      |]
    jsonOpenApiProperties genQueryRequest

  describe "QueryResponse" $ do
    testToFromJSONToSchema
      (QueryResponse (Just []) (Just mempty))
      [aesonQQ|
        { "rows": [],
          "aggregates": {} }
      |]
    jsonOpenApiProperties genQueryResponse

  describe "FieldValue" $ do
    describe "ColumnFieldValue - Object" $ do
      testToFromJSONToSchema
        (mkColumnFieldValue $ J.Object [("property", "Wow")])
        [aesonQQ|
          { "property": "Wow" }
        |]
    describe "ColumnFieldValue - String" $ do
      testToFromJSONToSchema
        (mkColumnFieldValue $ J.String "Test")
        [aesonQQ|
          "Test"
        |]
    describe "ColumnFieldValue - Number" $ do
      testToFromJSONToSchema
        (mkColumnFieldValue $ J.Number 123)
        [aesonQQ|
          123
        |]
    describe "ColumnFieldValue - Bool" $ do
      testToFromJSONToSchema
        (mkColumnFieldValue $ J.Bool True)
        [aesonQQ|
          true
        |]
    describe "ColumnFieldValue - Null" $ do
      testToFromJSONToSchema
        (mkColumnFieldValue J.Null)
        [aesonQQ|
          null
        |]
    describe "RelationshipFieldValue" $ do
      testToFromJSONToSchema
        (mkRelationshipFieldValue (QueryResponse (Just []) (Just mempty)))
        [aesonQQ|
          { "rows": [],
            "aggregates": {} }
        |]

genField :: Gen Field
genField =
  Gen.recursive
    Gen.choice
    [ColumnField <$> genColumnName <*> genScalarType <*> Gen.maybe genRedactionExpressionName]
    [RelField <$> genRelationshipField]

genFieldName :: Gen FieldName
genFieldName = FieldName <$> genArbitraryAlphaNumText defaultRange

genFieldMap :: Gen value -> Gen (HashMap FieldName value)
genFieldMap genValue' = genHashMap genFieldName genValue' defaultRange

genRelationshipField :: Gen RelationshipField
genRelationshipField =
  RelationshipField
    <$> genRelationshipName
    <*> genQuery

genQuery :: Gen Query
genQuery =
  Query
    <$> Gen.maybe (genFieldMap genField)
    <*> Gen.maybe (genFieldMap genAggregate)
    <*> Gen.maybe (Gen.int defaultRange)
    <*> Gen.maybe (Gen.int defaultRange)
    <*> Gen.maybe (Gen.int defaultRange)
    <*> Gen.maybe genExpression
    <*> Gen.maybe genOrderBy

genQueryRequest :: Gen QueryRequest
genQueryRequest = genTableRequest <|> genFunctionRequest -- NOTE: We should probably weight tables more than functions...

genFunctionTarget :: Gen Target
genFunctionTarget =
  TFunction
    <$> ( TargetFunction
            <$> genFunctionName
            <*> Gen.list defaultRange genFunctionArgument
        )

genFunctionRequest :: Gen QueryRequest
genFunctionRequest =
  QueryRequest
    <$> genFunctionTarget
    <*> Gen.set defaultRange genRelationships
    <*> Gen.set defaultRange genTargetRedactionExpressions
    <*> pure mempty
    <*> genQuery
    <*> pure Nothing

genTableRequest :: Gen QueryRequest
genTableRequest =
  QueryRequest
    <$> genTableTarget
    <*> Gen.set defaultRange genRelationships
    <*> Gen.set defaultRange genTargetRedactionExpressions
    <*> pure mempty
    <*> genQuery
    <*> Gen.maybe (Gen.nonEmpty defaultRange (genHashMap genColumnName genScalarValue defaultRange))

genFieldValue :: Gen FieldValue
genFieldValue =
  Gen.recursive
    Gen.choice
    [mkColumnFieldValue <$> genValue]
    [mkRelationshipFieldValue <$> genQueryResponse]

genQueryResponse :: Gen QueryResponse
genQueryResponse =
  QueryResponse
    <$> Gen.maybe (Gen.list defaultRange (genFieldMap genFieldValue))
    <*> Gen.maybe (genFieldMap genValue)
