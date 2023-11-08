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
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnSelector)
import Hasura.Backends.DataConnector.API.V0.ExpressionSpec (genExpression, genRedactionExpressionName)
import Hasura.Backends.DataConnector.API.V0.RelationshipsSpec (genRelationshipName)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.Aeson.Utils (jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "OrderByTarget" $ do
    describe "OrderByColumn"
      $ testToFromJSONToSchema
        (OrderByColumn (mkColumnSelector $ ColumnName "test_column") (Just $ RedactionExpressionName "RedactionExp2"))
        [aesonQQ|
          { "type": "column",
            "column": "test_column",
            "redaction_expression": "RedactionExp2"
          }
        |]
    describe "OrderByStarCountAggregate"
      $ testToFromJSONToSchema
        (OrderByStarCountAggregate)
        [aesonQQ|
          { "type": "star_count_aggregate" }
        |]
    describe "OrderBySingleColumnAggregate"
      $ testToFromJSONToSchema
        (OrderBySingleColumnAggregate (SingleColumnAggregate (SingleColumnAggregateFunction [G.name|sum|]) (ColumnName "test_column") (Just $ RedactionExpressionName "RedactionExp2") (ScalarType "number")))
        [aesonQQ|
          { "type": "single_column_aggregate",
            "function": "sum",
            "column": "test_column",
            "result_type": "number",
            "redaction_expression": "RedactionExp2"
          }
        |]
    jsonOpenApiProperties genOrderByTarget

  describe "OrderByElement" $ do
    testToFromJSONToSchema
      ( OrderByElement
          [RelationshipName "relation1", RelationshipName "relation2"]
          (OrderByColumn (mkColumnSelector $ ColumnName "my_column_name") (Just $ RedactionExpressionName "RedactionExp2"))
          Ascending
      )
      [aesonQQ|
        { "target_path": ["relation1", "relation2"],
          "target": {
            "type": "column",
            "column": "my_column_name",
            "redaction_expression": "RedactionExp2"
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
    describe "Ascending"
      $ testToFromJSONToSchema Ascending [aesonQQ|"asc"|]
    describe "Descending"
      $ testToFromJSONToSchema Descending [aesonQQ|"desc"|]
    jsonOpenApiProperties genOrderDirection

genOrderBy :: Gen OrderBy
genOrderBy =
  OrderBy
    <$> (HashMap.fromList <$> Gen.list defaultRange ((,) <$> genRelationshipName <*> genOrderByRelation))
    <*> Gen.nonEmpty defaultRange genOrderByElement

genOrderByRelation :: Gen OrderByRelation
genOrderByRelation =
  OrderByRelation
    <$> Gen.maybe genExpression
    -- Gen.small ensures the recursion will terminate as the size will shrink with each recursion
    <*> Gen.small (HashMap.fromList <$> Gen.list defaultRange ((,) <$> genRelationshipName <*> genOrderByRelation))

genOrderByElement :: Gen OrderByElement
genOrderByElement =
  OrderByElement
    <$> Gen.list defaultRange genRelationshipName
    <*> genOrderByTarget
    <*> genOrderDirection

genOrderByTarget :: Gen OrderByTarget
genOrderByTarget =
  Gen.choice
    [ OrderByColumn <$> genColumnSelector <*> Gen.maybe genRedactionExpressionName,
      pure OrderByStarCountAggregate,
      OrderBySingleColumnAggregate <$> genSingleColumnAggregate
    ]

genOrderDirection :: Gen OrderDirection
genOrderDirection = Gen.enumBounded
