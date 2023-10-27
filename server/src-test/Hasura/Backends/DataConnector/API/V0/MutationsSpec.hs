{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.MutationsSpec
  ( spec,
  )
where

import Data.Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0
import Hasura.Backends.DataConnector.API.V0.CapabilitiesSpec (genUpdateColumnOperatorName)
import Hasura.Backends.DataConnector.API.V0.ColumnSpec (genColumnName, genColumnType, genColumnValueGenerationStrategy)
import Hasura.Backends.DataConnector.API.V0.ExpressionSpec (genExpression, genTargetRedactionExpressions)
import Hasura.Backends.DataConnector.API.V0.QuerySpec (genField, genFieldMap, genFieldValue)
import Hasura.Backends.DataConnector.API.V0.RelationshipsSpec (genRelationshipName, genTableRelationships)
import Hasura.Backends.DataConnector.API.V0.ScalarSpec (genScalarType)
import Hasura.Backends.DataConnector.API.V0.TableSpec (genTableName)
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.Aeson.Utils (genValue, jsonOpenApiProperties, testToFromJSONToSchema)
import Test.Hspec

spec :: Spec
spec = do
  describe "MutationRequest" $ do
    testToFromJSONToSchema
      (MutationRequest [] [] [] [])
      [aesonQQ|
        { "relationships": [],
          "insert_schema": [],
          "operations": [] }
      |]
    jsonOpenApiProperties genMutationRequest

  describe "TableInsertSchema" $ do
    describe "minimal" $ do
      testToFromJSONToSchema
        (TableInsertSchema (TableName ["my_table"]) Nothing [])
        [aesonQQ|
          { "table": ["my_table"],
            "fields": {} }
        |]
    describe "non-minimal" $ do
      testToFromJSONToSchema
        (TableInsertSchema (TableName ["my_table"]) (Just $ ColumnName "pk" :| []) [])
        [aesonQQ|
          { "table": ["my_table"],
            "primary_key": ["pk"],
            "fields": {} }
        |]
    jsonOpenApiProperties genTableInsertSchema

  describe "InsertFieldSchema" $ do
    describe "ColumnInsert" $ do
      describe "minimal" $ do
        testToFromJSONToSchema
          (ColumnInsert (ColumnInsertSchema (ColumnName "my_column") (ColumnTypeScalar $ ScalarType "number") True Nothing))
          [aesonQQ|
            { "type": "column",
              "column": "my_column",
              "column_type": "number",
              "nullable": true }
          |]
      describe "non-minimal" $ do
        testToFromJSONToSchema
          (ColumnInsert (ColumnInsertSchema (ColumnName "my_column") (ColumnTypeScalar $ ScalarType "number") True (Just UniqueIdentifier)))
          [aesonQQ|
            { "type": "column",
              "column": "my_column",
              "column_type": "number",
              "nullable": true,
              "value_generated": { "type": "unique_identifier" } }
          |]
    describe "ObjectRelationInsert" $ do
      testToFromJSONToSchema
        (ObjectRelationInsert (ObjectRelationInsertSchema (RelationshipName "my_relation") BeforeParent))
        [aesonQQ|
          { "type": "object_relation",
            "relationship": "my_relation",
            "insertion_order": "before_parent" }
        |]
    describe "ArrayRelationInsert" $ do
      testToFromJSONToSchema
        (ArrayRelationInsert (ArrayRelationInsertSchema (RelationshipName "my_relation")))
        [aesonQQ|
          { "type": "array_relation",
            "relationship": "my_relation" }
        |]
    jsonOpenApiProperties genInsertFieldSchema

  describe "MutationOperation" $ do
    let returningFields = [(FieldName "field", ColumnField (ColumnName "my_column") (ScalarType "string") (Just $ RedactionExpressionName "RedactionExp1"))]
    describe "InsertOperation" $ do
      testToFromJSONToSchema
        (InsertOperation (InsertMutationOperation (TableName ["my_table"]) [] (Just $ And []) returningFields))
        [aesonQQ|
          { "type": "insert",
            "table": ["my_table"],
            "rows": [],
            "post_insert_check": { "type": "and", "expressions": [] },
            "returning_fields": {
              "field": {
                "type": "column",
                "column": "my_column",
                "column_type": "string",
                "redaction_expression": "RedactionExp1"
              }
            }
          }
        |]
    describe "UpdateOperation" $ do
      testToFromJSONToSchema
        (UpdateOperation (UpdateMutationOperation (TableName ["my_table"]) (Just $ And []) [] (Just $ And []) returningFields))
        [aesonQQ|
          { "type": "update",
            "table": ["my_table"],
            "where": { "type": "and", "expressions": [] },
            "updates": [],
            "post_update_check": { "type": "and", "expressions": [] },
            "returning_fields": {
              "field": {
                "type": "column",
                "column": "my_column",
                "column_type": "string",
                "redaction_expression": "RedactionExp1"
              }
            }
          }
        |]
    describe "DeleteOperation" $ do
      testToFromJSONToSchema
        (DeleteOperation (DeleteMutationOperation (TableName ["my_table"]) (Just $ And []) returningFields))
        [aesonQQ|
          { "type": "delete",
            "table": ["my_table"],
            "where": { "type": "and", "expressions": [] },
            "returning_fields": {
              "field": {
                "type": "column",
                "column": "my_column",
                "column_type": "string",
                "redaction_expression": "RedactionExp1"
              }
            }
          }
        |]
    jsonOpenApiProperties genMutationOperation

  describe "RowObject" $ do
    testToFromJSONToSchema
      (RowObject [(FieldName "row_field", mkColumnInsertFieldValue $ String "row_field_value")])
      [aesonQQ|
        { "row_field": "row_field_value" }
      |]
    jsonOpenApiProperties genRowObject

  describe "InsertFieldValue" $ do
    describe "ColumnInsertFieldValue" $ do
      describe "Object"
        $ testToFromJSONToSchema
          (mkColumnInsertFieldValue $ Object [("property", "Wow")])
          [aesonQQ| { "property": "Wow" } |]
      describe "String" $ do
        testToFromJSONToSchema
          (mkColumnInsertFieldValue $ String "Test")
          [aesonQQ| "Test" |]
      describe "Number" $ do
        testToFromJSONToSchema
          (mkColumnInsertFieldValue $ Number 123)
          [aesonQQ| 123 |]
      describe "Bool" $ do
        testToFromJSONToSchema
          (mkColumnInsertFieldValue $ Bool True)
          [aesonQQ| true |]
      describe "Null" $ do
        testToFromJSONToSchema
          (mkColumnInsertFieldValue Null)
          [aesonQQ| null |]
    describe "ObjectRelationInsertFieldValue" $ do
      testToFromJSONToSchema
        (mkObjectRelationInsertFieldValue $ RowObject [(FieldName "row_field", mkColumnInsertFieldValue $ String "row_field_value")])
        [aesonQQ| { "row_field": "row_field_value" } |]
    describe "ArrayRelationInsertFieldValue" $ do
      testToFromJSONToSchema
        (mkArrayRelationInsertFieldValue $ [RowObject [(FieldName "row_field", mkColumnInsertFieldValue $ String "row_field_value")]])
        [aesonQQ| [{ "row_field": "row_field_value" }] |]
    jsonOpenApiProperties genInsertFieldValue

  describe "ObjectRelationInsertionOrder" $ do
    describe "BeforeParent"
      $ testToFromJSONToSchema BeforeParent [aesonQQ|"before_parent"|]
    describe "AfterParent"
      $ testToFromJSONToSchema AfterParent [aesonQQ|"after_parent"|]
    jsonOpenApiProperties genObjectRelationInsertionOrder

  describe "RowUpdate" $ do
    describe "SetColumnRowUpdate"
      $ testToFromJSONToSchema
        (SetColumn $ RowColumnOperatorValue (ColumnName "my_column") (Number 10) (ScalarType "number"))
        [aesonQQ|
            { "type": "set",
              "column": "my_column",
              "value": 10,
              "value_type": "number" }
          |]
    describe "CustomUpdateColumnOperator"
      $ testToFromJSONToSchema
        (CustomUpdateColumnOperator (UpdateColumnOperatorName [G.name|increment|]) (RowColumnOperatorValue (ColumnName "my_column") (Number 10) (ScalarType "number")))
        [aesonQQ|
            { "type": "custom_operator",
              "operator_name": "increment",
              "column": "my_column",
              "value": 10,
              "value_type": "number" }
          |]
    jsonOpenApiProperties genRowUpdate

  describe "RowColumnOperatorValue" $ do
    testToFromJSONToSchema
      (RowColumnOperatorValue (ColumnName "my_column") (String "a value") (ScalarType "string"))
      [aesonQQ|
        { "column": "my_column",
          "value": "a value",
          "value_type": "string" }
      |]
    jsonOpenApiProperties genRowColumnOperatorValue

  describe "MutationResponse" $ do
    testToFromJSONToSchema
      (MutationResponse [])
      [aesonQQ|
        { "operation_results": [] }
      |]
    jsonOpenApiProperties genMutationResponse

  describe "MutationOperationResults" $ do
    testToFromJSONToSchema
      (MutationOperationResults 64 (Just [[(FieldName "field", mkColumnFieldValue $ String "field_value")]]))
      [aesonQQ|
        { "affected_rows": 64,
          "returning": [
            { "field": "field_value" }
          ]
        }
      |]
    jsonOpenApiProperties genMutationOperationResults

genMutationRequest :: Gen MutationRequest
genMutationRequest =
  MutationRequest
    <$> Gen.set defaultRange (RTable <$> genTableRelationships)
    <*> Gen.set defaultRange genTargetRedactionExpressions
    <*> Gen.set defaultRange genTableInsertSchema
    <*> Gen.list defaultRange genMutationOperation

genTableInsertSchema :: Gen TableInsertSchema
genTableInsertSchema =
  TableInsertSchema
    <$> genTableName
    <*> Gen.maybe (Gen.nonEmpty defaultRange genColumnName)
    <*> genFieldMap genInsertFieldSchema

genInsertFieldSchema :: Gen InsertFieldSchema
genInsertFieldSchema =
  Gen.choice
    [ ColumnInsert <$> genColumnInsertSchema,
      ObjectRelationInsert <$> genObjectRelationInsertSchema,
      ArrayRelationInsert <$> genArrayRelationInsertSchema
    ]

genColumnInsertSchema :: Gen ColumnInsertSchema
genColumnInsertSchema =
  ColumnInsertSchema
    <$> genColumnName
    <*> genColumnType
    <*> Gen.bool
    <*> Gen.maybe genColumnValueGenerationStrategy

genObjectRelationInsertSchema :: (MonadGen m) => m ObjectRelationInsertSchema
genObjectRelationInsertSchema =
  ObjectRelationInsertSchema
    <$> genRelationshipName
    <*> genObjectRelationInsertionOrder

genObjectRelationInsertionOrder :: (MonadGen m) => m ObjectRelationInsertionOrder
genObjectRelationInsertionOrder = Gen.enumBounded

genArrayRelationInsertSchema :: (MonadGen m) => m ArrayRelationInsertSchema
genArrayRelationInsertSchema = ArrayRelationInsertSchema <$> genRelationshipName

genMutationOperation :: Gen MutationOperation
genMutationOperation =
  Gen.choice
    [ InsertOperation <$> genInsertMutationOperation,
      UpdateOperation <$> genUpdateMutationOperation,
      DeleteOperation <$> genDeleteMutationOperation
    ]

genInsertMutationOperation :: Gen InsertMutationOperation
genInsertMutationOperation =
  InsertMutationOperation
    <$> genTableName
    <*> Gen.list defaultRange genRowObject
    <*> Gen.maybe genExpression
    <*> genFieldMap genField

genRowObject :: Gen RowObject
genRowObject = RowObject <$> genFieldMap genInsertFieldValue

genInsertFieldValue :: Gen InsertFieldValue
genInsertFieldValue =
  Gen.recursive
    Gen.choice
    [mkColumnInsertFieldValue <$> genValue]
    [ mkObjectRelationInsertFieldValue <$> genRowObject,
      mkArrayRelationInsertFieldValue <$> Gen.list defaultRange genRowObject
    ]

genUpdateMutationOperation :: Gen UpdateMutationOperation
genUpdateMutationOperation =
  UpdateMutationOperation
    <$> genTableName
    <*> Gen.maybe genExpression
    <*> Gen.set defaultRange genRowUpdate
    <*> Gen.maybe genExpression
    <*> genFieldMap genField

genRowUpdate :: (MonadGen m, GenBase m ~ Identity) => m RowUpdate
genRowUpdate =
  Gen.choice
    [ SetColumn <$> genRowColumnOperatorValue,
      CustomUpdateColumnOperator <$> genUpdateColumnOperatorName <*> genRowColumnOperatorValue
    ]

genRowColumnOperatorValue :: (MonadGen m, GenBase m ~ Identity) => m RowColumnOperatorValue
genRowColumnOperatorValue =
  RowColumnOperatorValue
    <$> genColumnName
    <*> genValue
    <*> genScalarType

genDeleteMutationOperation :: Gen DeleteMutationOperation
genDeleteMutationOperation =
  DeleteMutationOperation
    <$> genTableName
    <*> Gen.maybe genExpression
    <*> genFieldMap genField

genMutationResponse :: Gen MutationResponse
genMutationResponse =
  MutationResponse
    <$> Gen.list defaultRange genMutationOperationResults

genMutationOperationResults :: Gen MutationOperationResults
genMutationOperationResults =
  MutationOperationResults
    <$> Gen.int defaultRange
    <*> Gen.maybe (Gen.list defaultRange (genFieldMap genFieldValue))
