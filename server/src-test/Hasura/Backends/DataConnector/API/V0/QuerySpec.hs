{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.QuerySpec (spec) where

import Autodocodec.Extended
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.AggregateSpec (genAggregate)
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataConnector.API.V0.ExpressionSpec (genExpression)
import Hasura.Backends.DataConnector.API.V0.OrderBySpec (genOrderBy)
import Hasura.Backends.DataConnector.API.V0.RelationshipsSpec (genRelationshipName, genTableRelationships)
import Hasura.Backends.DataConnector.API.V0.Scalar.ValueSpec (genValue)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableName)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import Test.Aeson.Utils (genKeyMap, jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "Field" $ do
    describe "ColumnField" $
      testToFromJSONToSchema
        (ColumnField (ValueWrapper $ ColumnName "my_column_name"))
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
            { _qFields = Just $ KM.fromList [("my_field_alias", ColumnField $ ValueWrapper $ ColumnName "my_field_name")],
              _qAggregates = Just $ KM.fromList [("my_aggregate", StarCount)],
              _qLimit = Just 10,
              _qOffset = Just 20,
              _qWhere = Just . And $ ValueWrapper [],
              _qOrderBy = Just [OrderBy (ColumnName "my_column_name") Ascending]
            }
    testToFromJSONToSchema
      query
      [aesonQQ|
        { "fields": {"my_field_alias": {"type": "column", "column": "my_field_name"}},
          "aggregates": { "my_aggregate": { "type": "star_count" } },
          "limit": 10,
          "offset": 20,
          "where": {"type": "and", "expressions": []},
          "order_by": [{"column": "my_column_name", "ordering": "asc"}]
        }
      |]
    jsonOpenApiProperties genQuery

  describe "QueryRequest" $ do
    let queryRequest =
          QueryRequest
            { _qrTable = TableName "my_table",
              _qrTableRelationships = [],
              _qrQuery = Query (Just mempty) Nothing Nothing Nothing Nothing Nothing
            }
    testToFromJSONToSchema
      queryRequest
      [aesonQQ|
        { "table": "my_table",
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

genField :: MonadGen m => m Field
genField =
  Gen.recursive
    Gen.choice
    [ColumnField . ValueWrapper <$> genColumnName]
    [RelField <$> genRelationshipField]

genRelationshipField :: MonadGen m => m RelationshipField
genRelationshipField =
  RelationshipField
    <$> genRelationshipName
    <*> genQuery

genQuery :: MonadGen m => m Query
genQuery =
  Query
    <$> Gen.maybe (genKeyMap genField)
    <*> Gen.maybe (genKeyMap genAggregate)
    <*> Gen.maybe (Gen.int (linear 0 5))
    <*> Gen.maybe (Gen.int (linear 0 5))
    <*> Gen.maybe genExpression
    <*> Gen.maybe (Gen.nonEmpty (linear 0 5) genOrderBy)

genQueryRequest :: MonadGen m => m QueryRequest
genQueryRequest =
  QueryRequest
    <$> genTableName
    <*> Gen.list (linear 0 5) genTableRelationships
    <*> genQuery

genFieldValue :: MonadGen m => m FieldValue
genFieldValue =
  Gen.recursive
    Gen.choice
    [ColumnFieldValue . ValueWrapper <$> genValue]
    [RelationshipFieldValue . ValueWrapper <$> genQueryResponse]

genQueryResponse :: MonadGen m => m QueryResponse
genQueryResponse =
  QueryResponse
    <$> Gen.maybe (Gen.list (linear 0 5) (genKeyMap genFieldValue))
    <*> Gen.maybe (genKeyMap genValue)
