{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.OrderBySpec
  ( spec,
    genOrderBy,
    genOrderDirection,
  )
where

import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.AggregateSpec (genSingleColumnAggregate)
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataConnector.API.V0.ExpressionSpec (genExpression)
import Hasura.Backends.DataConnector.API.V0.RelationshipsSpec (genRelationshipName)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (linear)
import Test.Aeson.Utils (jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "OrderByTarget" $ do
    describe "OrderByColumn" $
      testToFromJSONToSchema
        (OrderByColumn (ColumnName "test_column"))
        [aesonQQ|
          { "type": "column",
            "column": "test_column"
          }
        |]
    describe "OrderByStarCountAggregate" $
      testToFromJSONToSchema
        (OrderByStarCountAggregate)
        [aesonQQ|
          { "type": "star_count_aggregate" }
        |]
    describe "OrderBySingleColumnAggregate" $
      testToFromJSONToSchema
        (OrderBySingleColumnAggregate (SingleColumnAggregate Sum (ColumnName "test_column")))
        [aesonQQ|
          { "type": "single_column_aggregate",
            "function": "sum",
            "column": "test_column"
          }
        |]
    jsonOpenApiProperties genOrderByTarget

  describe "OrderByElement" $ do
    testToFromJSONToSchema
      ( OrderByElement
          [RelationshipName "relation1", RelationshipName "relation2"]
          (OrderByColumn (ColumnName "my_column_name"))
          Ascending
      )
      [aesonQQ|
        { "target_path": ["relation1", "relation2"],
          "target": {
            "type": "column",
            "column": "my_column_name"
          },
          "order_direction": "asc"
        }
      |]
    jsonOpenApiProperties genOrderByElement

  describe "OrderByRelation" $ do
    testToFromJSONToSchema
      ( OrderByRelation
          (Just $ And [])
          (HashMap.fromList [(RelationshipName "relationship_name", (OrderByRelation Nothing mempty))])
      )
      [aesonQQ|
        { "where": {
            "type": "and",
            "expressions": []
          },
          "subrelations": {
            "relationship_name": {
              "subrelations": {}
            }
          }
        }
      |]
    jsonOpenApiProperties genOrderByRelation

  describe "OrderBy" $ do
    testToFromJSONToSchema
      ( OrderBy
          (HashMap.fromList [(RelationshipName "relationship_name", (OrderByRelation Nothing mempty))])
          (OrderByElement [] OrderByStarCountAggregate Ascending :| [])
      )
      [aesonQQ|
        { "relations": {
            "relationship_name": {
              "subrelations": {}
            }
          },
          "elements": [
            {
              "target_path": [],
              "target": {
                "type": "star_count_aggregate"
              },
              "order_direction": "asc"
            }
          ]
        }
      |]
    jsonOpenApiProperties genOrderBy

  describe "OrderDirection" $ do
    describe "Ascending" $
      testToFromJSONToSchema Ascending [aesonQQ|"asc"|]
    describe "Descending" $
      testToFromJSONToSchema Descending [aesonQQ|"desc"|]
    jsonOpenApiProperties genOrderDirection

genOrderBy :: MonadGen m => m OrderBy
genOrderBy =
  OrderBy
    <$> (HashMap.fromList <$> Gen.list (linear 0 5) ((,) <$> genRelationshipName <*> genOrderByRelation))
    <*> Gen.nonEmpty (linear 1 5) genOrderByElement

genOrderByRelation :: MonadGen m => m OrderByRelation
genOrderByRelation =
  OrderByRelation
    <$> Gen.maybe genExpression
    -- Gen.small ensures the recursion will terminate as the size will shrink with each recursion
    <*> Gen.small (HashMap.fromList <$> Gen.list (linear 0 5) ((,) <$> genRelationshipName <*> genOrderByRelation))

genOrderByElement :: MonadGen m => m OrderByElement
genOrderByElement =
  OrderByElement
    <$> Gen.list (linear 0 5) genRelationshipName
    <*> genOrderByTarget
    <*> genOrderDirection

genOrderByTarget :: MonadGen m => m OrderByTarget
genOrderByTarget =
  Gen.choice
    [ OrderByColumn <$> genColumnName,
      pure OrderByStarCountAggregate,
      OrderBySingleColumnAggregate <$> genSingleColumnAggregate
    ]

genOrderDirection :: MonadGen m => m OrderDirection
genOrderDirection = Gen.enumBounded
