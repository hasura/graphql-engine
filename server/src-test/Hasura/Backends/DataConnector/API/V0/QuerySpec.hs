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
import Hasura.Backends.DataConnector.API.V0.ExpressionSpec (genExpression)
import Hasura.Backends.DataConnector.API.V0.OrderBySpec (genOrderBy)
import Hasura.Backends.DataConnector.API.V0.RelationshipsSpec (genRelationshipName, genRelationships)
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType, genScalarValue)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableName)
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumText, genHashMap)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Range (linear)
import Test.Aeson.Utils (genValue, jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "Field" $ do
    describe "ColumnField"
      $ testToFromJSONToSchema
        (ColumnField (ColumnName "my_column_name") (ScalarType "string"))
        [aesonQQ|
          { "type": "column",
            "column": "my_column_name",
            "column_type": "string"
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
            { _qFields = Just $ HashMap.fromList [(FieldName "my_field_alias", ColumnField (ColumnName "my_field_name") (ScalarType "string"))],
              _qAggregates = Just $ HashMap.fromList [(FieldName "my_aggregate", StarCount)],
              _qAggregatesLimit = Just 5,
              _qLimit = Just 10,
              _qOffset = Just 20,
              _qWhere = Just $ And [],
              _qOrderBy = Just $ OrderBy [] (OrderByElement [] (OrderByColumn (ColumnName "my_column_name")) Ascending :| [])
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
          QRTable
            $ TableRequest
              { _trTable = TableName ["my_table"],
                _trRelationships = [],
                _trQuery = Query (Just mempty) Nothing Nothing Nothing Nothing Nothing Nothing,
                _trForeach = Just (HashMap.fromList [(ColumnName "my_id", ScalarValue (J.Number 666) (ScalarType "number"))] :| [])
              }
    testToFromJSONToSchema
      queryRequest
      [aesonQQ|
        { "type": "table",
          "table": ["my_table"],
          "table_relationships": [],
          "query": { "fields": {} },
          "foreach": [
            { "my_id": { "value": 666, "value_type": "number" } }
          ]
        }
      |]
    jsonOpenApiProperties genQueryRequest

  describe "FunctionRequest" $ do
    let queryRequest =
          QRFunction
            $ FunctionRequest
              { _frFunction = FunctionName ["my_function"],
                _frFunctionArguments = [],
                _frRelationships = [],
                _frQuery = Query (Just mempty) Nothing Nothing Nothing Nothing Nothing Nothing
              }
    testToFromJSONToSchema
      queryRequest
      [aesonQQ|
        { "type": "function",
          "function": ["my_function"],
          "function_arguments": [],
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
    [ColumnField <$> genColumnName <*> genScalarType]
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

genFunctionRequest :: Gen QueryRequest
genFunctionRequest =
  FunctionQueryRequest
    <$> genFunctionName
    <*> Gen.list defaultRange genFunctionArgument
    <*> Gen.set defaultRange genRelationships
    <*> genQuery

genFunctionName :: (MonadGen m) => m FunctionName
genFunctionName = FunctionName <$> Gen.nonEmpty (linear 1 3) (genArbitraryAlphaNumText defaultRange)

genFunctionArgument :: Gen FunctionArgument
genFunctionArgument =
  NamedArgument
    <$> genArbitraryAlphaNumText defaultRange
    <*> genArgumentValue

genArgumentValue :: Gen ArgumentValue
genArgumentValue =
  fmap ScalarArgumentValue
    $ ScalarValue
    <$> genValue
    <*> genScalarType

genTableRequest :: Gen QueryRequest
genTableRequest =
  TableQueryRequest
    <$> genTableName
    <*> Gen.set defaultRange genRelationships
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
