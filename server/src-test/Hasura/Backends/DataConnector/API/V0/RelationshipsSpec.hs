{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.RelationshipsSpec
  ( spec,
    genRelationshipName,
    genTableRelationships,
  )
where

import Data.Aeson.QQ.Simple (aesonQQ)
import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableName)
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
    describe "ObjectRelationship" $
      testToFromJSONToSchema ObjectRelationship [aesonQQ|"object"|]
    describe "ArrayRelationship" $
      testToFromJSONToSchema ArrayRelationship [aesonQQ|"array"|]
    jsonOpenApiProperties genRelationshipType
  describe "Relationship" $ do
    let relationship =
          Relationship
            { _rTargetTable = TableName ["target_table_name"],
              _rRelationshipType = ObjectRelationship,
              _rColumnMapping = [(ColumnName "outer_column", ColumnName "inner_column")]
            }
    testToFromJSONToSchema
      relationship
      [aesonQQ|
        { "target_table": ["target_table_name"],
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
            { _rTargetTable = TableName ["target_table_name_a"],
              _rRelationshipType = ObjectRelationship,
              _rColumnMapping = [(ColumnName "outer_column_a", ColumnName "inner_column_a")]
            }
    let relationshipB =
          Relationship
            { _rTargetTable = TableName ["target_table_name_b"],
              _rRelationshipType = ArrayRelationship,
              _rColumnMapping = [(ColumnName "outer_column_b", ColumnName "inner_column_b")]
            }
    let tableRelationships =
          TableRelationships
            { _trSourceTable = TableName ["source_table_name"],
              _trRelationships =
                [ (RelationshipName "relationship_a", relationshipA),
                  (RelationshipName "relationship_b", relationshipB)
                ]
            }
    testToFromJSONToSchema
      tableRelationships
      [aesonQQ|
        { "source_table": ["source_table_name"],
          "relationships": {
            "relationship_a": {
              "target_table": ["target_table_name_a"],
              "relationship_type": "object",
              "column_mapping": {
                "outer_column_a": "inner_column_a"
              }
            },
            "relationship_b": {
              "target_table": ["target_table_name_b"],
              "relationship_type": "array",
              "column_mapping": {
                "outer_column_b": "inner_column_b"
              }
            }
          }
        }
      |]
    jsonOpenApiProperties genTableRelationships

genRelationshipName :: MonadGen m => m RelationshipName
genRelationshipName =
  RelationshipName <$> Gen.text (linear 0 10) Gen.unicode

genRelationshipType :: MonadGen m => m RelationshipType
genRelationshipType = Gen.enumBounded

genRelationship :: MonadGen m => m Relationship
genRelationship =
  Relationship
    <$> genTableName
    <*> genRelationshipType
    <*> (HashMap.fromList <$> Gen.list (linear 0 5) ((,) <$> genColumnName <*> genColumnName))

genTableRelationships :: MonadGen m => m TableRelationships
genTableRelationships =
  TableRelationships
    <$> genTableName
    <*> fmap HashMap.fromList (Gen.list (linear 0 5) ((,) <$> genRelationshipName <*> genRelationship))
