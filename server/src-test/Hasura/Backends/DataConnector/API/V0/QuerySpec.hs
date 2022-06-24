{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.QuerySpec (spec) where

import Autodocodec.Extended
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range
import Test.Aeson.Utils
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
      let query = Query mempty Nothing Nothing Nothing Nothing
      testToFromJSONToSchema
        (RelField $ RelationshipField (RelationshipName "a_relationship") query)
        [aesonQQ|
        { "type": "relationship",
          "relationship": "a_relationship",
          "query": {"fields": {}}
        }
      |]
  describe "Query" $ do
    let query =
          Query
            { _qFields = KM.fromList [("my_field_alias", ColumnField $ ValueWrapper $ ColumnName "my_field_name")],
              _qLimit = Just 10,
              _qOffset = Just 20,
              _qWhere = Just . And $ ValueWrapper [],
              _qOrderBy = Just [OrderBy (ColumnName "my_column_name") Ascending]
            }
    testToFromJSONToSchema
      query
      [aesonQQ|
        { "fields": {"my_field_alias": {"type": "column", "column": "my_field_name"}},
          "limit": 10,
          "offset": 20,
          "where": {"type": "and", "expressions": []},
          "order_by": [{"column": "my_column_name", "ordering": "asc"}]
        }
      |]
  describe "QueryRequest" $ do
    let queryRequest =
          QueryRequest
            { _qrTable = TableName "my_table",
              _qrTableRelationships = [],
              _qrQuery = Query mempty Nothing Nothing Nothing Nothing
            }
    testToFromJSONToSchema
      queryRequest
      [aesonQQ|
        { "table": "my_table",
          "table_relationships": [],
          "query": { "fields": {} } }
      |]

  describe "QueryResponse" $ do
    testToFromJSONToSchema (QueryResponse []) [aesonQQ|[]|]
    jsonOpenApiProperties genQueryResponse

genQueryResponse :: MonadGen m => m QueryResponse
genQueryResponse =
  QueryResponse <$> Gen.list (linear 0 5) genObject
