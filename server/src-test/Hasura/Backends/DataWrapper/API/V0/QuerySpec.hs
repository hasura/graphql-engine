{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataWrapper.API.V0.QuerySpec (spec) where

import Autodocodec.Extended
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as Map
import Hasura.Backends.DataWrapper.API.V0.API
import Hasura.Backends.DataWrapper.API.V0.ColumnSpec (genColumnName)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "PrimaryKey" $ do
    testToFromJSONToSchema (PrimaryKey $ ColumnName "my_primary_key") [aesonQQ|"my_primary_key"|]
    jsonOpenApiProperties genPrimaryKey
  describe "ForeignKey" $
    testToFromJSONToSchema (ForeignKey $ ColumnName "my_foreign_key") [aesonQQ|"my_foreign_key"|]
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
      let fieldMapping = Map.fromList [(PrimaryKey $ ColumnName "id", ForeignKey $ ColumnName "my_foreign_id")]
          query = Query mempty (TableName "my_table_name") Nothing Nothing Nothing Nothing
      testToFromJSONToSchema
        (RelationshipField $ RelField fieldMapping query)
        [aesonQQ|
        { "type": "relationship",
          "column_mapping": {"id": "my_foreign_id"},
          "query": {"fields": {}, "from": "my_table_name"}
        }
      |]
  describe "Query" $ do
    let query =
          Query
            { fields = Map.fromList [("my_field_alias", ColumnField $ ValueWrapper $ ColumnName "my_field_name")],
              from = TableName "my_table_name",
              limit = Just 10,
              offset = Just 20,
              where_ = Just $ Literal $ ValueWrapper $ Boolean True,
              orderBy = Just [OrderBy (ColumnName "my_column_name") Ascending]
            }
    testToFromJSONToSchema
      query
      [aesonQQ|
        { "fields": {"my_field_alias": {"type": "column", "column": "my_field_name"}},
          "from": "my_table_name",
          "limit": 10,
          "offset": 20,
          "where": {"type": "literal", "value": true},
          "order_by": [{"column": "my_column_name", "ordering": "asc"}]
        }
      |]
  describe "QueryResponse" $ do
    testToFromJSONToSchema (QueryResponse []) [aesonQQ|[]|]
    jsonOpenApiProperties genQueryResponse

genPrimaryKey :: MonadGen m => m PrimaryKey
genPrimaryKey = PrimaryKey <$> genColumnName

genQueryResponse :: MonadGen m => m QueryResponse
genQueryResponse =
  QueryResponse <$> Gen.list (linear 0 5) genObject
