{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.RelationshipsSpec
  ( spec,
    genRelationshipName,
    genRelationships,
    genTableRelationships,
  )
where

import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableName, genTableTarget)
import Hasura.Generator.Common (defaultRange, genArbitraryAlphaNumText)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "RelationshipName" $ do
    testToFromJSONToSchema (RelationshipName "relationship_name") [aesonQQ|"relationship_name"|]
    jsonOpenApiProperties genRelationshipName
  describe "RelationshipType" $ do
    describe "ObjectRelationship"
      $ testToFromJSONToSchema ObjectRelationship [aesonQQ|"object"|]
    describe "ArrayRelationship"
      $ testToFromJSONToSchema ArrayRelationship [aesonQQ|"array"|]
    jsonOpenApiProperties genRelationshipType
  describe "Relationship" $ do
    let relationship =
          Relationship
            { _rTarget = TTable (TargetTable (TableName ["target_table_name"])),
              _rRelationshipType = ObjectRelationship,
              _rColumnMapping = [(ColumnName "outer_column", ColumnName "inner_column")]
            }
    testToFromJSONToSchema
      relationship
      [aesonQQ|
        { "target": {"type": "table", "name": ["target_table_name"]},
          "relationship_type": "object",
          "column_mapping": {
            "outer_column": "inner_column"
          }
        }
      |]
    jsonOpenApiProperties genRelationship
  describe "TableRelationships" $ do
    let relationshipA =
          Relationship
            { _rTarget = TTable (TargetTable (TableName ["target_table_name_a"])),
              _rRelationshipType = ObjectRelationship,
              _rColumnMapping = [(ColumnName "outer_column_a", ColumnName "inner_column_a")]
            }
    let relationshipB =
          Relationship
            { _rTarget = TTable (TargetTable (TableName ["target_table_name_b"])),
              _rRelationshipType = ArrayRelationship,
              _rColumnMapping = [(ColumnName "outer_column_b", ColumnName "inner_column_b")]
            }
    let tableRelationships =
          TableRelationships
            { _trelSourceTable = TableName ["source_table_name"],
              _trelRelationships =
                [ (RelationshipName "relationship_a", relationshipA),
                  (RelationshipName "relationship_b", relationshipB)
                ]
            }
    testToFromJSONToSchema
      (RTable tableRelationships)
      [aesonQQ|
        { "source_table": ["source_table_name"],
          "type": "table",
          "relationships": {
            "relationship_a": {
              "target": {"type": "table", "name": ["target_table_name_a"]},
              "relationship_type": "object",
              "column_mapping": {
                "outer_column_a": "inner_column_a"
              }
            },
            "relationship_b": {
              "target": {"type": "table", "name":["target_table_name_b"]},
              "relationship_type": "array",
              "column_mapping": {
                "outer_column_b": "inner_column_b"
              }
            }
          }
        }
      |]
    jsonOpenApiProperties (RTable <$> genTableRelationships)

genRelationshipName :: (MonadGen m) => m RelationshipName
genRelationshipName =
  RelationshipName <$> genArbitraryAlphaNumText defaultRange

genRelationshipType :: (MonadGen m) => m RelationshipType
genRelationshipType = Gen.enumBounded

genRelationship :: (MonadGen m) => m Relationship
genRelationship =
  Relationship
    <$> genTableTarget
    <*> genRelationshipType
    <*> (HashMap.fromList <$> Gen.list defaultRange ((,) <$> genColumnName <*> genColumnName))

genRelationships :: Gen Relationships
genRelationships = (RTable <$> genTableRelationships) <|> (RFunction <$> genFunctionRelationships)

genTableRelationships :: (MonadGen m) => m TableRelationships
genTableRelationships =
  TableRelationships
    <$> genTableName
    <*> fmap HashMap.fromList (Gen.list defaultRange ((,) <$> genRelationshipName <*> genRelationship))

genFunctionRelationships :: (MonadGen m) => m FunctionRelationships
genFunctionRelationships =
  FunctionRelationships
    <$> genFunctionName
    <*> fmap HashMap.fromList (Gen.list defaultRange ((,) <$> genRelationshipName <*> genRelationship))

genFunctionName :: (MonadGen m) => m FunctionName
genFunctionName = FunctionName <$> Gen.nonEmpty (linear 1 3) (genArbitraryAlphaNumText defaultRange)
