{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.API.V0.Mutations
  ( MutationRequest (..),
    mrRelationships,
    mrRedactionExpressions,
    mrInsertSchema,
    mrOperations,
    TableInsertSchema (..),
    InsertFieldSchema (..),
    ColumnInsertSchema (..),
    ObjectRelationInsertSchema (..),
    ObjectRelationInsertionOrder (..),
    ArrayRelationInsertSchema (..),
    MutationOperation (..),
    InsertMutationOperation (..),
    imoTable,
    imoRows,
    imoPostInsertCheck,
    imoReturningFields,
    RowObject (..),
    InsertFieldValue,
    mkColumnInsertFieldValue,
    mkObjectRelationInsertFieldValue,
    mkArrayRelationInsertFieldValue,
    deserializeAsColumnInsertFieldValue,
    deserializeAsObjectRelationInsertFieldValue,
    deserializeAsArrayRelationInsertFieldValue,
    _ColumnInsertFieldValue,
    _ObjectRelationInsertFieldValue,
    _ArrayRelationInsertFieldValue,
    UpdateMutationOperation (..),
    umoTable,
    umoWhere,
    umoUpdates,
    umoPostUpdateCheck,
    umoReturningFields,
    RowUpdate (..),
    RowColumnOperatorValue (..),
    DeleteMutationOperation (..),
    dmoTable,
    dmoWhere,
    dmoReturningFields,
    MutationResponse (..),
    mrOperationResults,
    MutationOperationResults (..),
    morAffectedRows,
    morReturning,
  )
where

import Autodocodec.Extended
import Autodocodec.OpenAPI ()
import Control.Lens (Lens', lens, prism')
import Control.Lens.Combinators (Prism')
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty)
import Data.OpenApi (ToSchema)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Hasura.Backends.DataConnector.API.V0.Capabilities qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Column qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Expression qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Query qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Relationships qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Scalar qualified as API.V0
import Hasura.Backends.DataConnector.API.V0.Table qualified as API.V0
import Servant.API (HasStatus (..))
import Prelude

-- | A request to perform a batch of 'MutationOperation's.
--
-- The table relationships and insert schema represent metadata that will be
-- used by agents interpreting the operations, and are shared across all operations.
--
-- TODO: Does this need to be enhanced ala. QueryRequest to support FunctionRequests?
data MutationRequest = MutationRequest
  { _mrRelationships :: Set API.V0.Relationships,
    _mrRedactionExpressions :: Set API.V0.TargetRedactionExpressions,
    _mrInsertSchema :: Set TableInsertSchema,
    _mrOperations :: [MutationOperation]
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec MutationRequest

instance HasCodec MutationRequest where
  codec :: JSONCodec MutationRequest
  codec =
    object "MutationRequest" $
      MutationRequest
        <$> requiredField "relationships" "The relationships involved in the entire mutation request"
          .= _mrRelationships
        <*> optionalFieldWithOmittedDefault "redaction_expressions" mempty "Expressions that can be referenced by the query to redact fields/columns"
          .= _mrRedactionExpressions
        <*> requiredField "insert_schema" "The schema by which to interpret row data specified in any insert operations in this request"
          .= _mrInsertSchema
        <*> requiredField "operations" "The mutation operations to perform"
          .= _mrOperations

-- | Describes the fields that may be found in rows that are requested to be inserted
-- in an 'InsertMutationOperation' in the 'MutationRequest' (ie. a 'RowObject').
--
-- This metadata about rows to be inserted has been extracted out of the rows
-- themselves to reduce the amount of duplicated information in the serialized request.
-- Without doing this, every row inserted into the same table would repeat the same
-- schema information over and over.
data TableInsertSchema = TableInsertSchema
  { _tisTable :: API.V0.TableName,
    _tisPrimaryKey :: Maybe (NonEmpty API.V0.ColumnName),
    _tisFields :: HashMap API.V0.FieldName InsertFieldSchema
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec TableInsertSchema

instance HasCodec TableInsertSchema where
  codec :: JSONCodec TableInsertSchema
  codec =
    object "TableInsertSchema" $
      TableInsertSchema
        <$> requiredField "table" "The name of the table"
          .= _tisTable
        <*> optionalFieldOrNull "primary_key" "The names of the columns that make up the table's primary key"
          .= _tisPrimaryKey
        <*> requiredField "fields" "The fields that will be found in the insert row data for the table and the schema for each field"
          .= _tisFields

-- | The schema of a particular field in a row to be inserted (ie. a 'RowObject').
data InsertFieldSchema
  = -- | The field represents a particular column in the table
    ColumnInsert ColumnInsertSchema
  | -- | The field represents a row to be inserted in a separate table that
    -- is related to the current table by an object relationship
    ObjectRelationInsert ObjectRelationInsertSchema
  | -- | The field represents a collection of rows to be inserted in a
    -- separate table that is related to the current table by an array relationship
    ArrayRelationInsert ArrayRelationInsertSchema
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec InsertFieldSchema

instance HasCodec InsertFieldSchema where
  codec =
    named "InsertFieldSchema" $
      object "InsertFieldSchema" $
        discriminatedUnionCodec "type" enc dec
    where
      columnInsertSchemaObjectCodec :: ObjectCodec ColumnInsertSchema ColumnInsertSchema
      columnInsertSchemaObjectCodec =
        ColumnInsertSchema
          <$> requiredField "column" "The name of the column that this field should be inserted into"
            .= _cisColumn
          <*> requiredField "column_type" "The scalar type of the column"
            .= _cisColumnType
          <*> requiredField "nullable" "Is the column nullable"
            .= _cisNullable
          <*> optionalFieldOrNull "value_generated" "Whether or not and how the value of the column can be generated by the database"
            .= _cisValueGenerated

      objectRelationInsertSchemaObjectCodec :: ObjectCodec ObjectRelationInsertSchema ObjectRelationInsertSchema
      objectRelationInsertSchemaObjectCodec =
        ObjectRelationInsertSchema
          <$> requiredField "relationship" "The name of the object relationship over which the related row must be inserted"
            .= _orisRelationship
          <*> requiredField "insertion_order" "The order in which to insert the related row, relative to its parent"
            .= _orisInsertionOrder

      arrayRelationInsertSchemaObjectCodec :: ObjectCodec ArrayRelationInsertSchema ArrayRelationInsertSchema
      arrayRelationInsertSchemaObjectCodec =
        ArrayRelationInsertSchema
          <$> requiredField "relationship" "The name of the array relationship over which the related rows must be inserted"
            .= _arisRelationship

      enc = \case
        ColumnInsert insertOp -> ("column", mapToEncoder insertOp columnInsertSchemaObjectCodec)
        ObjectRelationInsert updateOp -> ("object_relation", mapToEncoder updateOp objectRelationInsertSchemaObjectCodec)
        ArrayRelationInsert deleteOp -> ("array_relation", mapToEncoder deleteOp arrayRelationInsertSchemaObjectCodec)
      dec =
        HashMap.fromList
          [ ("column", ("ColumnInsertSchema", mapToDecoder ColumnInsert columnInsertSchemaObjectCodec)),
            ("object_relation", ("ObjectRelationInsertSchema", mapToDecoder ObjectRelationInsert objectRelationInsertSchemaObjectCodec)),
            ("array_relation", ("ArrayRelationInsertSchema", mapToDecoder ArrayRelationInsert arrayRelationInsertSchemaObjectCodec))
          ]

-- | Describes a field in a row to be inserted that represents a column in the table
data ColumnInsertSchema = ColumnInsertSchema
  { _cisColumn :: API.V0.ColumnName,
    _cisColumnType :: API.V0.ColumnType,
    _cisNullable :: Bool,
    _cisValueGenerated :: Maybe API.V0.ColumnValueGenerationStrategy
  }
  deriving stock (Eq, Ord, Show)

-- | Describes a field in a row to be inserted that represents a row to be inserted in
-- a separate table tha is related to the current table by an object relationship
data ObjectRelationInsertSchema = ObjectRelationInsertSchema
  { _orisRelationship :: API.V0.RelationshipName,
    _orisInsertionOrder :: ObjectRelationInsertionOrder
  }
  deriving stock (Eq, Ord, Show)

-- | Describes whether the object-related row needs to be inserted before or after the
-- parent row. This is important in one-to-one object relationships where the order
-- of insertion needs to be clarified
data ObjectRelationInsertionOrder
  = BeforeParent
  | AfterParent
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec ObjectRelationInsertionOrder

instance HasCodec ObjectRelationInsertionOrder where
  codec =
    named "ObjectRelationInsertionOrder" $
      ( stringConstCodec
          [ (BeforeParent, "before_parent"),
            (AfterParent, "after_parent")
          ]
      )

-- | Describes a field in a row to be inserted that represents a collection of rows to
-- be inserted in a separate table tha is related to the current table by an array relationship
data ArrayRelationInsertSchema = ArrayRelationInsertSchema
  { _arisRelationship :: API.V0.RelationshipName
  }
  deriving stock (Eq, Ord, Show)

-- | Represents a particular mutation operation to perform against a table
data MutationOperation
  = InsertOperation InsertMutationOperation
  | UpdateOperation UpdateMutationOperation
  | DeleteOperation DeleteMutationOperation
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec MutationOperation

instance HasCodec MutationOperation where
  codec =
    named "MutationOperation" $
      object "MutationOperation" $
        discriminatedUnionCodec "type" enc dec
    where
      insertMutationOperationObjectCodec :: ObjectCodec InsertMutationOperation InsertMutationOperation
      insertMutationOperationObjectCodec =
        InsertMutationOperation
          <$> requiredField "table" "The name of the table to insert rows in"
            .= _imoTable
          <*> requiredField "rows" "The rows to insert into the table"
            .= _imoRows
          <*> optionalFieldOrNull "post_insert_check" "An expression that all inserted rows must match after they have been inserted, otherwise the changes must be reverted"
            .= _imoPostInsertCheck
          <*> optionalFieldOrNullWithOmittedDefault "returning_fields" mempty "The fields to return for the rows affected by this insert operation"
            .= _imoReturningFields

      updateMutationOperationObjectCodec :: ObjectCodec UpdateMutationOperation UpdateMutationOperation
      updateMutationOperationObjectCodec =
        UpdateMutationOperation
          <$> requiredField "table" "The name of the table to update rows in"
            .= _umoTable
          <*> optionalFieldOrNull "where" "The filter by which to select rows to update"
            .= _umoWhere
          <*> requiredField "updates" "The updates to make to the matched rows in the table"
            .= _umoUpdates
          <*> optionalFieldOrNull "post_update_check" "An expression that all updated rows must match after they have been updated, otherwise the changes must be reverted"
            .= _umoPostUpdateCheck
          <*> optionalFieldOrNullWithOmittedDefault "returning_fields" mempty "The fields to return for the rows affected by this update operation"
            .= _umoReturningFields

      deleteMutationOperationObjectCodec :: ObjectCodec DeleteMutationOperation DeleteMutationOperation
      deleteMutationOperationObjectCodec =
        DeleteMutationOperation
          <$> requiredField "table" "The name of the table to delete rows from"
            .= _dmoTable
          <*> optionalFieldOrNull "where" "The filter by which to select rows to delete"
            .= _dmoWhere
          <*> optionalFieldOrNullWithOmittedDefault "returning_fields" mempty "The fields to return for the rows affected by this delete operation"
            .= _dmoReturningFields

      enc = \case
        InsertOperation insertOp -> ("insert", mapToEncoder insertOp insertMutationOperationObjectCodec)
        UpdateOperation updateOp -> ("update", mapToEncoder updateOp updateMutationOperationObjectCodec)
        DeleteOperation deleteOp -> ("delete", mapToEncoder deleteOp deleteMutationOperationObjectCodec)
      dec =
        HashMap.fromList
          [ ("insert", ("InsertMutationOperation", mapToDecoder InsertOperation insertMutationOperationObjectCodec)),
            ("update", ("UpdateMutationOperation", mapToDecoder UpdateOperation updateMutationOperationObjectCodec)),
            ("delete", ("DeleteMutationOperation", mapToDecoder DeleteOperation deleteMutationOperationObjectCodec))
          ]

-- | Describes a collection of rows that should be inserted into a particular table.
--
-- The schema of the rows must be interpreted by looking at the insert schema on 'MutationRequest'
data InsertMutationOperation = InsertMutationOperation
  { -- | The table to insert into
    _imoTable :: API.V0.TableName,
    -- | The rows to insert into the table
    _imoRows :: [RowObject],
    -- | An expression that all inserted rows must match after they have been inserted,
    -- otherwise the changes must be reverted
    _imoPostInsertCheck :: Maybe API.V0.Expression,
    -- | The fields to return that represent a projection over the set of rows inserted
    -- after they are inserted (after insertion they include calculated columns, relations etc)
    _imoReturningFields :: HashMap API.V0.FieldName API.V0.Field
  }
  deriving stock (Eq, Ord, Show)

-- | A row to be inserted into a table. It is mapping from a 'API.V0.FieldName' to a 'InsertFieldValue'.
-- The field name must be looked up in the insert schema defined in the 'MutationRequest' in order to know
-- how to deserialize the 'InsertFieldValue' correctly.
--
-- Note that the field name is not the same as a table column name. The column name (if the field
-- represent a table column) can be found in the insert schema.
newtype RowObject = RowObject {unRowObject :: HashMap API.V0.FieldName InsertFieldValue}
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec RowObject

instance HasCodec RowObject where
  codec = named "RowObject" $ dimapCodec RowObject unRowObject codec

-- | Represents the value of a field in a row to be inserted. There are three diffferent types of
-- row field value types: column, object relation and array relation. ColumnInsertFieldValues represent
-- a table column in the row to be inserted. ObjectRelationInsertFieldValues represent a row in another
-- object-related table that needs to be inserted. ArrayRelationInsertFieldValues represent rows in
-- another array-related table that need to be inserted.
--
-- Unfortunately, the JSON representation of these three different types wholly overlap with each other.
-- We chose not to explicitly discriminate them (eg. wrapped in an object with a type property) because
-- we wanted to keep the JSON that represents row data to insert as terse as possible. This means in
-- order to know which type any particular 'InsertFieldValue' actually is, you must look at the
-- 'TableInsertSchema' and look up the field in question to see the matching 'InsertFieldSchema', which
-- will tell you what type it is. Then, you can explicitly deserialize the 'InsertFieldValue' as the type
-- it is supposed to be.
--
-- Explicit serialization functions:
-- * 'mkColumnInsertFieldValue'
-- * 'mkObjectRelationInsertFieldValue'
-- * 'mkArrayRelationInsertFieldValue'
--
-- Explicit deserialization functions:
-- * 'deserializeAsColumnInsertFieldValue'
-- * 'deserializeAsObjectRelationInsertFieldValue'
-- * 'deserializeAsArrayRelationInsertFieldValue'
newtype InsertFieldValue = InsertFieldValue J.Value
  deriving stock (Eq, Ord, Show)
  deriving (ToJSON, FromJSON, ToSchema) via Autodocodec InsertFieldValue

mkColumnInsertFieldValue :: J.Value -> InsertFieldValue
mkColumnInsertFieldValue = InsertFieldValue

mkObjectRelationInsertFieldValue :: RowObject -> InsertFieldValue
mkObjectRelationInsertFieldValue = InsertFieldValue . J.toJSON

mkArrayRelationInsertFieldValue :: [RowObject] -> InsertFieldValue
mkArrayRelationInsertFieldValue = InsertFieldValue . J.toJSON

deserializeAsColumnInsertFieldValue :: InsertFieldValue -> J.Value
deserializeAsColumnInsertFieldValue (InsertFieldValue value) = value

deserializeAsObjectRelationInsertFieldValue :: InsertFieldValue -> Either Text RowObject
deserializeAsObjectRelationInsertFieldValue (InsertFieldValue value) =
  case J.fromJSON value of
    J.Error s -> Left $ T.pack s
    J.Success rowObject -> Right rowObject

deserializeAsArrayRelationInsertFieldValue :: InsertFieldValue -> Either Text [RowObject]
deserializeAsArrayRelationInsertFieldValue (InsertFieldValue value) =
  case J.fromJSON value of
    J.Error s -> Left $ T.pack s
    J.Success rowObjects -> Right rowObjects

_ColumnInsertFieldValue :: Lens' InsertFieldValue J.Value
_ColumnInsertFieldValue = lens deserializeAsColumnInsertFieldValue (const mkColumnInsertFieldValue)

_ObjectRelationInsertFieldValue :: Prism' InsertFieldValue RowObject
_ObjectRelationInsertFieldValue = prism' mkObjectRelationInsertFieldValue (either (const Nothing) Just . deserializeAsObjectRelationInsertFieldValue)

_ArrayRelationInsertFieldValue :: Prism' InsertFieldValue [RowObject]
_ArrayRelationInsertFieldValue = prism' mkArrayRelationInsertFieldValue (either (const Nothing) Just . deserializeAsArrayRelationInsertFieldValue)

instance HasCodec InsertFieldValue where
  codec =
    dimapCodec encode decode $
      possiblyJointEitherCodec anyJsonValueCodec (possiblyJointEitherCodec (possiblyJointEitherCodec objectRelationInsertFieldValueCodec arrayRelationInsertFieldValueCodec) nullColumnFieldValue)
    where
      arrayRelationInsertFieldValueCodec :: JSONCodec [RowObject]
      arrayRelationInsertFieldValueCodec = named "ArrayRelationInsertFieldValue" $ codec

      objectRelationInsertFieldValueCodec :: JSONCodec RowObject
      objectRelationInsertFieldValueCodec = named "ObjectRelationInsertFieldValue" $ codec

      anyJsonValueCodec :: JSONCodec J.Value
      anyJsonValueCodec = named "ColumnInsertFieldValue" valueCodec

      -- We have to explicitly call out null as a separate named type in OpenAPI
      -- to get the typescript type-generator to recognise null as a valid value here
      nullColumnFieldValue :: JSONCodec ()
      nullColumnFieldValue = named "NullColumnInsertFieldValue" nullCodec

      encode :: Either J.Value (Either (Either RowObject [RowObject]) ()) -> InsertFieldValue
      encode = InsertFieldValue . either id (either (either J.toJSON J.toJSON) (const J.Null))

      decode :: InsertFieldValue -> Either J.Value (Either (Either RowObject [RowObject]) ())
      decode = Left . deserializeAsColumnInsertFieldValue

-- | Describes a update against a table that can modify a certain subset of rows
data UpdateMutationOperation = UpdateMutationOperation
  { -- | The table to update rows in
    _umoTable :: API.V0.TableName,
    -- | An expression to select which rows should be updated
    _umoWhere :: Maybe API.V0.Expression,
    -- | The updates to perform against each row
    _umoUpdates :: Set RowUpdate,
    -- | An expression that all updated rows must match after they have been updated,
    -- otherwise the changes must be reverted
    _umoPostUpdateCheck :: Maybe API.V0.Expression,
    -- | The fields to return that represent a projection over the set of rows updated
    -- after they are updated (ie. with their updated values)
    _umoReturningFields :: HashMap API.V0.FieldName API.V0.Field
  }
  deriving stock (Eq, Ord, Show)

-- | Describes an update to be performed on a row
data RowUpdate
  = -- | A particular column in the row should have its value overwritten
    SetColumn RowColumnOperatorValue
  | -- | A particular column in the row should have its value modified with the specified operator
    CustomUpdateColumnOperator API.V0.UpdateColumnOperatorName RowColumnOperatorValue
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec RowUpdate

instance HasCodec RowUpdate where
  codec =
    named "RowUpdate" $
      object "RowUpdate" $
        discriminatedUnionCodec "type" enc dec
    where
      customUpdateColumnOperatorObjectCodec :: ObjectCodec (API.V0.UpdateColumnOperatorName, RowColumnOperatorValue) (API.V0.UpdateColumnOperatorName, RowColumnOperatorValue)
      customUpdateColumnOperatorObjectCodec =
        (,)
          <$> requiredField "operator_name" "The name of the custom update operator"
            .= fst
          <*> rowColumnOperatorValueObjectCodec
            .= snd

      enc = \case
        SetColumn rowColumnValue -> ("set", mapToEncoder rowColumnValue rowColumnOperatorValueObjectCodec)
        CustomUpdateColumnOperator operatorName rowColumnValue -> ("custom_operator", mapToEncoder (operatorName, rowColumnValue) customUpdateColumnOperatorObjectCodec)
      dec =
        HashMap.fromList
          [ ("set", ("SetColumnRowUpdate", mapToDecoder SetColumn rowColumnOperatorValueObjectCodec)),
            ("custom_operator", ("CustomUpdateColumnOperatorRowUpdate", mapToDecoder (uncurry CustomUpdateColumnOperator) customUpdateColumnOperatorObjectCodec))
          ]

-- | The value applied to a column using an operator
data RowColumnOperatorValue = RowColumnOperatorValue
  { _rcovColumn :: API.V0.ColumnName,
    _rcovValue :: J.Value,
    _rcovValueType :: API.V0.ScalarType
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec RowColumnOperatorValue

instance HasCodec RowColumnOperatorValue where
  codec =
    object "RowColumnOperatorValue" rowColumnOperatorValueObjectCodec

rowColumnOperatorValueObjectCodec :: ObjectCodec RowColumnOperatorValue RowColumnOperatorValue
rowColumnOperatorValueObjectCodec =
  RowColumnOperatorValue
    <$> requiredField "column" "The name of the column in the row"
      .= _rcovColumn
    <*> requiredField "value" "The value to use with the column operator"
      .= _rcovValue
    <*> requiredField "value_type" "The scalar type of the value"
      .= _rcovValueType

-- | Describes a set of rows to be deleted from a table
data DeleteMutationOperation = DeleteMutationOperation
  { -- | The table to delete rows from
    _dmoTable :: API.V0.TableName,
    -- | An expression with which to select the rows to be deleted
    _dmoWhere :: Maybe API.V0.Expression,
    -- | The fields to return that represent a projection over the set of rows that will be deleted
    -- with their values before they are deleted
    _dmoReturningFields :: HashMap API.V0.FieldName API.V0.Field
  }
  deriving stock (Eq, Ord, Show)

-- | Represents the response from a 'MutationRequest'
data MutationResponse = MutationResponse
  { -- | A matching list of results per 'MutationOperation' in the request, in the same order
    _mrOperationResults :: [MutationOperationResults]
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec MutationResponse

instance HasCodec MutationResponse where
  codec :: JSONCodec MutationResponse
  codec =
    object "MutationResponse" $
      MutationResponse
        <$> requiredField "operation_results" "The results of each mutation operation, in the same order as they were received"
          .= _mrOperationResults

instance HasStatus MutationResponse where
  type StatusOf MutationResponse = 200

-- | The results of a particular 'MutationOperation'
data MutationOperationResults = MutationOperationResults
  { -- | The number of rows affected by the 'MutationOperation'
    _morAffectedRows :: Int,
    -- | A projection of the rows affected by the mutation, if requested by the operation
    -- via its returning fields definition
    _morReturning :: Maybe [HashMap API.V0.FieldName API.V0.FieldValue]
  }
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, ToSchema) via Autodocodec MutationOperationResults

instance HasCodec MutationOperationResults where
  codec :: JSONCodec MutationOperationResults
  codec =
    object "MutationOperationResults" $
      MutationOperationResults
        <$> requiredField "affected_rows" "The number of rows affected by the mutation operation"
          .= _morAffectedRows
        <*> optionalFieldOrNull "returning" "The rows affected by the mutation operation"
          .= _morReturning

$(makeLenses ''MutationRequest)
$(makeLenses ''InsertMutationOperation)
$(makeLenses ''UpdateMutationOperation)
$(makeLenses ''DeleteMutationOperation)
$(makeLenses ''MutationResponse)
$(makeLenses ''MutationOperationResults)
