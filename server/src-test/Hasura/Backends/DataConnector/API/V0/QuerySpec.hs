{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.QuerySpec (spec) where

import Data.Aeson qualified as J
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.AggregateSpec (genAggregate)
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataConnector.API.V0.ExpressionSpec (genExpression)
import Hasura.Backends.DataConnector.API.V0.OrderBySpec (genOrderBy)
import Hasura.Backends.DataConnector.API.V0.RelationshipsSpec (genRelationshipName, genTableRelationships)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableName)
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumText)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils (genValue, jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "Field" $ do
    describe "ColumnField" $
      testToFromJSONToSchema
        (ColumnField $ ColumnName "my_column_name")
        [aesonQQ|
          { "type": "column",
            "column": "my_column_name"
          }
        |]
    describe "RelationshipField" $ do
      let query = Query (Just mempty) Nothing Nothing Nothing Nothing Nothing
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
            { _qFields = Just $ HashMap.fromList [(FieldName "my_field_alias", ColumnField $ ColumnName "my_field_name")],
              _qAggregates = Just $ HashMap.fromList [(FieldName "my_aggregate", StarCount)],
              _qLimit = Just 10,
              _qOffset = Just 20,
              _qWhere = Just $ And [],
              _qOrderBy = Just $ OrderBy [] (OrderByElement [] (OrderByColumn (ColumnName "my_column_name")) Ascending :| [])
            }
    testToFromJSONToSchema
      query
      [aesonQQ|
        { "fields": {"my_field_alias": {"type": "column", "column": "my_field_name"}},
          "aggregates": { "my_aggregate": { "type": "star_count" } },
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

  describe "QueryRequest" $ do
    let queryRequest =
          QueryRequest
            { _qrTable = TableName ["my_table"],
              _qrTableRelationships = [],
              _qrQuery = Query (Just mempty) Nothing Nothing Nothing Nothing Nothing
            }
    testToFromJSONToSchema
      queryRequest
      [aesonQQ|
        { "table": ["my_table"],
          "table_relationships": [],
          "query": { "fields": {} } }
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

genField :: MonadGen m => m Field
genField =
  Gen.recursive
    Gen.choice
    [ColumnField <$> genColumnName]
    [RelField <$> genRelationshipField]

genFieldName :: MonadGen m => m FieldName
genFieldName = FieldName <$> genArbitraryAlphaNumText defaultRange

genFieldMap :: MonadGen m => m value -> m (HashMap FieldName value)
genFieldMap genValue' =
  HashMap.fromList <$> Gen.list defaultRange ((,) <$> genFieldName <*> genValue')

genRelationshipField :: MonadGen m => m RelationshipField
genRelationshipField =
  RelationshipField
    <$> genRelationshipName
    <*> genQuery

genQuery :: MonadGen m => m Query
genQuery =
  Query
    <$> Gen.maybe (genFieldMap genField)
    <*> Gen.maybe (genFieldMap genAggregate)
    <*> Gen.maybe (Gen.int defaultRange)
    <*> Gen.maybe (Gen.int defaultRange)
    <*> Gen.maybe genExpression
    <*> Gen.maybe genOrderBy

genQueryRequest :: MonadGen m => m QueryRequest
genQueryRequest =
  QueryRequest
    <$> genTableName
    <*> Gen.list defaultRange genTableRelationships
    <*> genQuery

genFieldValue :: MonadGen m => m FieldValue
genFieldValue =
  Gen.recursive
    Gen.choice
    [mkColumnFieldValue <$> genValue]
    [mkRelationshipFieldValue <$> genQueryResponse]

genQueryResponse :: MonadGen m => m QueryResponse
genQueryResponse =
  QueryResponse
    <$> Gen.maybe (Gen.list defaultRange (genFieldMap genFieldValue))
    <*> Gen.maybe (genFieldMap genValue)
