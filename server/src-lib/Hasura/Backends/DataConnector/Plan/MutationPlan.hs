module Hasura.Backends.DataConnector.Plan.MutationPlan
  ( mkMutationPlan,
  )
where

import Control.Monad.Trans.Writer.CPS qualified as CPS
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.Has (Has, modifier)
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended (toTxt)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.DataConnector.Adapter.Types.Mutations
import Hasura.Backends.DataConnector.Plan.Common
import Hasura.Backends.DataConnector.Plan.QueryPlan (reshapeAnnFields, translateAnnFields)
import Hasura.Base.Error (Code (..), QErr, throw400, throw500)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (GBoolExp (..))
import Hasura.RQL.IR.Delete
import Hasura.RQL.IR.Insert hiding (Single)
import Hasura.RQL.IR.Returning
import Hasura.RQL.IR.Root
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Update
import Hasura.RQL.IR.Update.Batch
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend (BackendType (..))
import Hasura.Session (SessionVariables)
import Language.GraphQL.Draft.Syntax qualified as G
import Witch qualified

--------------------------------------------------------------------------------

newtype TableInsertSchemas = TableInsertSchemas
  {unTableInsertSchemas :: HashMap API.TableName (HashMap API.FieldName API.InsertFieldSchema)}
  deriving stock (Eq, Show)

instance Semigroup TableInsertSchemas where
  (TableInsertSchemas l) <> (TableInsertSchemas r) = TableInsertSchemas $ HashMap.unionWith HashMap.union l r

instance Monoid TableInsertSchemas where
  mempty = TableInsertSchemas mempty

recordTableInsertSchema ::
  ( Has TableInsertSchemas writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  API.TableName ->
  HashMap API.FieldName API.InsertFieldSchema ->
  CPS.WriterT writerOutput m ()
recordTableInsertSchema tableName fieldSchemas =
  let newTableSchema = TableInsertSchemas $ HashMap.singleton tableName fieldSchemas
   in CPS.tell $ modifier (const newTableSchema) mempty

--------------------------------------------------------------------------------

mkMutationPlan ::
  MonadError QErr m =>
  SessionVariables ->
  MutationDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m (Plan API.MutationRequest API.MutationResponse)
mkMutationPlan sessionVariables mutationDB = do
  request <- translateMutationDB sessionVariables mutationDB
  pure $ Plan request (reshapeResponseToMutationGqlShape mutationDB)

translateMutationDB ::
  MonadError QErr m =>
  SessionVariables ->
  MutationDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m API.MutationRequest
translateMutationDB sessionVariables = \case
  MDBInsert insert -> do
    (insertOperation, (tableRelationships, tableInsertSchemas)) <- CPS.runWriterT $ translateInsert sessionVariables insert
    let apiTableRelationships = uncurry API.TableRelationships <$> HashMap.toList (unTableRelationships tableRelationships)
    let apiTableInsertSchema = uncurry API.TableInsertSchema <$> HashMap.toList (unTableInsertSchemas tableInsertSchemas)
    pure $
      API.MutationRequest
        { _mrTableRelationships = apiTableRelationships,
          _mrInsertSchema = apiTableInsertSchema,
          _mrOperations = [API.InsertOperation insertOperation]
        }
  MDBUpdate update -> do
    (updateOperations, tableRelationships) <- CPS.runWriterT $ translateUpdate sessionVariables update
    let apiTableRelationships = uncurry API.TableRelationships <$> HashMap.toList (unTableRelationships tableRelationships)
    pure $
      API.MutationRequest
        { _mrTableRelationships = apiTableRelationships,
          _mrInsertSchema = [],
          _mrOperations = API.UpdateOperation <$> updateOperations
        }
  MDBDelete _delete ->
    throw400 NotSupported "translateMutationDB: delete mutations not implemented for the Data Connector backend."
  MDBFunction _returnsSet _select ->
    throw400 NotSupported "translateMutationDB: function mutations not implemented for the Data Connector backend."

translateInsert ::
  MonadError QErr m =>
  SessionVariables ->
  AnnotatedInsert 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT (TableRelationships, TableInsertSchemas) m API.InsertMutationOperation
translateInsert sessionVariables AnnotatedInsert {..} = do
  rows <- traverse (translateInsertRow sessionVariables tableName _aiTableColumns _aiPresetValues) _aiInsertObject
  postInsertCheck <- translateBoolExpToExpression sessionVariables tableName insertCheckCondition
  returningFields <- translateMutationOutputToReturningFields sessionVariables tableName _aiOutput
  pure $
    API.InsertMutationOperation
      { API._imoTable = tableName,
        API._imoRows = rows,
        API._imoPostInsertCheck = postInsertCheck,
        API._imoReturningFields = HashMap.mapKeys (API.FieldName . getFieldNameTxt) returningFields
      }
  where
    tableName = Witch.from _aiTableName
    insertCheckCondition = fst _aiCheckCondition
    AnnotatedInsertData {..} = _aiData

translateInsertRow ::
  ( Has TableInsertSchemas writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  API.TableName ->
  [ColumnInfo 'DataConnector] ->
  HashMap ColumnName (UnpreparedValue 'DataConnector) ->
  AnnotatedInsertRow 'DataConnector (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m API.RowObject
translateInsertRow sessionVariables tableName tableColumns defaultColumnValues insertRow = do
  columnSchemasAndValues <- lift $ forM (HashMap.toList columnUnpreparedValues) $ \(columnName, columnValue) -> do
    fieldName <-
      case find (\ColumnInfo {..} -> ciColumn == columnName) tableColumns of
        Just ColumnInfo {..} -> pure . API.FieldName $ G.unName ciName
        Nothing -> throw500 $ "Can't find column " <> toTxt columnName <> " in table schema for " <> API.tableNameToText tableName
    preparedLiteral <- prepareLiteral sessionVariables columnValue

    (scalarType, value) <-
      case preparedLiteral of
        ValueLiteral scalarType value -> pure (scalarType, value)
        ArrayLiteral _scalarType _values -> throw400 NotSupported "translateInsertRow: Array literals are not supported as column insert values"

    let columnInsertSchema = API.ColumnInsertSchema (Witch.from columnName) (Witch.from scalarType)
    pure (fieldName, columnInsertSchema, value)

  let fieldSchemas =
        columnSchemasAndValues
          & fmap (\(fieldName, columnInsertSchema, _) -> (fieldName, API.ColumnInsert columnInsertSchema))
          & HashMap.fromList
  recordTableInsertSchema tableName fieldSchemas

  let rowObject =
        columnSchemasAndValues
          & fmap (\(fieldName, _, value) -> (fieldName, API.mkColumnInsertFieldValue value))
          & HashMap.fromList
          & API.RowObject

  pure rowObject
  where
    columnUnpreparedValues :: HashMap ColumnName (UnpreparedValue 'DataConnector)
    columnUnpreparedValues = HashMap.union rowColumnValues defaultColumnValues

    rowColumnValues :: HashMap ColumnName (UnpreparedValue 'DataConnector)
    rowColumnValues =
      insertRow
        & fmap (\(AIColumn columnNameAndValue) -> columnNameAndValue)
        & HashMap.fromList

translateUpdate ::
  MonadError QErr m =>
  SessionVariables ->
  AnnotatedUpdateG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT TableRelationships m [API.UpdateMutationOperation]
translateUpdate sessionVariables annUpdate@AnnotatedUpdateG {..} = do
  case _auUpdateVariant of
    SingleBatch batch -> (: []) <$> translateUpdateBatch sessionVariables annUpdate batch
    MultipleBatches batches -> traverse (translateUpdateBatch sessionVariables annUpdate) batches

translateUpdateBatch ::
  MonadError QErr m =>
  SessionVariables ->
  AnnotatedUpdateG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  UpdateBatch 'DataConnector UpdateOperator (UnpreparedValue 'DataConnector) ->
  CPS.WriterT TableRelationships m API.UpdateMutationOperation
translateUpdateBatch sessionVariables AnnotatedUpdateG {..} UpdateBatch {..} = do
  updates <- lift $ translateUpdateOperations sessionVariables _ubOperations
  whereExp <- translateBoolExpToExpression sessionVariables tableName (BoolAnd [_auUpdatePermissions, _ubWhere])
  postUpdateCheck <- translateBoolExpToExpression sessionVariables tableName _auCheck
  returningFields <- translateMutationOutputToReturningFields sessionVariables tableName _auOutput

  pure $
    API.UpdateMutationOperation
      { API._umoTable = tableName,
        API._umoWhere = whereExp,
        API._umoUpdates = updates,
        API._umoPostUpdateCheck = postUpdateCheck,
        API._umoReturningFields = HashMap.mapKeys (API.FieldName . getFieldNameTxt) returningFields
      }
  where
    tableName = Witch.from _auTable

translateUpdateOperations ::
  forall m.
  MonadError QErr m =>
  SessionVariables ->
  HashMap ColumnName (UpdateOperator (UnpreparedValue 'DataConnector)) ->
  m [API.RowUpdate]
translateUpdateOperations sessionVariables columnUpdates =
  forM (HashMap.toList columnUpdates) $ \(columnName, updateOperator) -> do
    let (mkRowUpdate, value) =
          case updateOperator of
            UpdateSet value' -> (API.SetColumn, value')
            UpdateCustomOperator operatorName value' -> (API.CustomUpdateColumnOperator operatorName, value')
    (scalarType, literalValue) <- prepareAndExtractLiteralValue value
    let operatorValue = API.RowColumnOperatorValue (Witch.from columnName) literalValue (Witch.from scalarType)
    pure $ mkRowUpdate operatorValue
  where
    prepareAndExtractLiteralValue :: UnpreparedValue 'DataConnector -> m (ScalarType, J.Value)
    prepareAndExtractLiteralValue unpreparedValue = do
      preparedLiteral <- prepareLiteral sessionVariables unpreparedValue
      case preparedLiteral of
        ValueLiteral scalarType value -> pure (scalarType, value)
        ArrayLiteral _scalarType _values -> throw400 NotSupported "translateUpdateOperations: Array literals are not supported as column update values"

translateMutationOutputToReturningFields ::
  ( MonadError QErr m,
    Has TableRelationships writerOutput,
    Monoid writerOutput
  ) =>
  SessionVariables ->
  API.TableName ->
  MutationOutputG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m (HashMap FieldName API.Field)
translateMutationOutputToReturningFields sessionVariables tableName = \case
  MOutSinglerowObject annFields ->
    translateAnnFields sessionVariables noPrefix tableName annFields
  MOutMultirowFields mutFields ->
    HashMap.unions <$> traverse (uncurry $ translateMutField sessionVariables tableName) mutFields

translateMutField ::
  ( MonadError QErr m,
    Has TableRelationships writerOutput,
    Monoid writerOutput
  ) =>
  SessionVariables ->
  API.TableName ->
  FieldName ->
  MutFldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m (HashMap FieldName API.Field)
translateMutField sessionVariables tableName fieldName = \case
  MCount ->
    -- All mutation operations in a request return their affected rows count.
    -- The count can just be added to the response JSON during agent response reshaping
    pure mempty
  MExp _text ->
    -- We ignore literal text fields (we don't send them to the data connector agent)
    -- and add them back to the response JSON when we reshape what the agent returns
    -- to us
    pure mempty
  MRet annFields ->
    translateAnnFields sessionVariables (prefixWith fieldName) tableName annFields

--------------------------------------------------------------------------------

reshapeResponseToMutationGqlShape ::
  MonadError QErr m =>
  MutationDB 'DataConnector Void v ->
  API.MutationResponse ->
  m J.Encoding
reshapeResponseToMutationGqlShape mutationDb mutationResponse = do
  case mutationDb of
    MDBInsert AnnotatedInsert {..} ->
      reshapeOutputForSingleBatchOperation _aiOutput mutationResponse
    MDBUpdate AnnotatedUpdateG {..} ->
      case _auUpdateVariant of
        SingleBatch _batch ->
          reshapeOutputForSingleBatchOperation _auOutput mutationResponse
        MultipleBatches batches ->
          let outputs = replicate (length batches) _auOutput
           in reshapeOutputForMultipleBatchOperation outputs mutationResponse
    MDBDelete AnnDel {..} ->
      reshapeOutputForSingleBatchOperation _adOutput mutationResponse
    MDBFunction _returnsSet _select ->
      throw400 NotSupported "reshapeResponseToMutationGqlShape: function mutations not implemented for the Data Connector backend."

reshapeOutputForSingleBatchOperation ::
  MonadError QErr m =>
  MutationOutputG 'DataConnector Void v ->
  API.MutationResponse ->
  m J.Encoding
reshapeOutputForSingleBatchOperation mutationOutput API.MutationResponse {..} = do
  mutationOperationResult <-
    listToMaybe _mrOperationResults
      `onNothing` throw500 "Unable to find expected mutation operation results"
  reshapeMutationOutput mutationOutput mutationOperationResult

reshapeOutputForMultipleBatchOperation ::
  MonadError QErr m =>
  [MutationOutputG 'DataConnector Void v] ->
  API.MutationResponse ->
  m J.Encoding
reshapeOutputForMultipleBatchOperation mutationOutputs API.MutationResponse {..} = do
  unless (operationResultCount >= requiredResultCount) $
    throw500 ("Data Connector agent returned " <> tshow operationResultCount <> " mutation operation results where at least " <> tshow requiredResultCount <> " was expected")

  reshapedResults <-
    zip mutationOutputs _mrOperationResults
      & traverse (uncurry reshapeMutationOutput)

  pure $ JE.list id reshapedResults
  where
    requiredResultCount = length mutationOutputs
    operationResultCount = length _mrOperationResults

reshapeMutationOutput ::
  MonadError QErr m =>
  MutationOutputG 'DataConnector Void v ->
  API.MutationOperationResults ->
  m J.Encoding
reshapeMutationOutput mutationOutput mutationOperationResults =
  case mutationOutput of
    MOutSinglerowObject annFields -> reshapeReturningRows Single noPrefix annFields mutationOperationResults
    MOutMultirowFields mutFields -> reshapeMutFields mutFields mutationOperationResults

reshapeReturningRows ::
  MonadError QErr m =>
  Cardinality ->
  FieldPrefix ->
  AnnFieldsG 'DataConnector Void v ->
  API.MutationOperationResults ->
  m J.Encoding
reshapeReturningRows cardinality fieldNamePrefix annFields API.MutationOperationResults {..} =
  case cardinality of
    Single ->
      case rows of
        [] -> pure $ J.toEncoding J.Null
        [singleRow] -> reshapeAnnFields fieldNamePrefix annFields singleRow
        _multipleRows ->
          throw500 "Data Connector agent returned multiple rows in a mutation operation result when only one was expected"
    Many -> do
      reshapedRows <- traverse (reshapeAnnFields fieldNamePrefix annFields) rows
      pure $ JE.list id reshapedRows
  where
    rows = fromMaybe mempty _morReturning

reshapeMutFields ::
  MonadError QErr m =>
  MutFldsG 'DataConnector Void v ->
  API.MutationOperationResults ->
  m J.Encoding
reshapeMutFields mutFields mutationOperationResults@API.MutationOperationResults {..} = do
  reshapedMutFields <- forM mutFields $ \(fieldName@(FieldName fieldNameText), mutField) -> do
    reshapedFieldValue <-
      case mutField of
        MCount -> pure $ JE.int _morAffectedRows
        MExp literalText -> pure $ JE.text literalText
        MRet annFields -> reshapeReturningRows Many (prefixWith fieldName) annFields mutationOperationResults
    pure (fieldNameText, reshapedFieldValue)

  pure $ encodeAssocListAsObject reshapedMutFields
