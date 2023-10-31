module Hasura.Backends.DataConnector.Plan.MutationPlan
  ( mkMutationPlan,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.Has (Has)
import Data.HashMap.Strict qualified as HashMap
import Data.Semigroup.Foldable (toNonEmpty)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Extended (toTxt)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Backend
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.DataConnector.Adapter.Types.Mutations
import Hasura.Backends.DataConnector.Plan.Common
import Hasura.Backends.DataConnector.Plan.QueryPlan (reshapeAnnFields, translateAnnFields)
import Hasura.Base.Error (Code (..), QErr, throw400, throw500)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (GBoolExp (..))
import Hasura.RQL.IR.Delete
import Hasura.RQL.IR.Insert hiding (Single)
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.IR.ModelInformation.Types (ModelNameInfo (..))
import Hasura.RQL.IR.Returning
import Hasura.RQL.IR.Root
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Update
import Hasura.RQL.IR.Update.Batch
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.BackendType (BackendType (..))
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.Session (SessionVariables)
import Language.GraphQL.Draft.Syntax qualified as G
import Witch qualified

--------------------------------------------------------------------------------

newtype TableInsertSchemas = TableInsertSchemas
  {unTableInsertSchemas :: HashMap API.TableName TableInsertSchema}
  deriving stock (Eq, Show)

instance Semigroup TableInsertSchemas where
  (TableInsertSchemas l) <> (TableInsertSchemas r) = TableInsertSchemas $ HashMap.unionWith (<>) l r

instance Monoid TableInsertSchemas where
  mempty = TableInsertSchemas mempty

data TableInsertSchema = TableInsertSchema
  { _tisPrimaryKey :: Maybe (NonEmpty API.ColumnName),
    _tisFields :: HashMap API.FieldName API.InsertFieldSchema
  }
  deriving stock (Eq, Show)

instance Semigroup TableInsertSchema where
  l <> r =
    TableInsertSchema
      { _tisPrimaryKey = _tisPrimaryKey l,
        _tisFields = HashMap.union (_tisFields l) (_tisFields r)
      }

recordTableInsertSchema ::
  ( Has TableInsertSchemas state,
    MonadState state m
  ) =>
  API.TableName ->
  TableInsertSchema ->
  m ()
recordTableInsertSchema tableName tableInsertSchema =
  writeOutput . TableInsertSchemas $ HashMap.singleton tableName tableInsertSchema

--------------------------------------------------------------------------------

mkMutationPlan ::
  ( MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r,
    MonadIO m
  ) =>
  SourceName ->
  ModelSourceType ->
  MutationDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m (Plan API.MutationRequest API.MutationResponse, [ModelNameInfo])
mkMutationPlan sourceName modelSourceType mutationDB = do
  (request, modelInfo) <- translateMutationDB sourceName modelSourceType mutationDB
  pure $ (Plan request (reshapeResponseToMutationGqlShape mutationDB), modelInfo)

translateMutationDB ::
  ( MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r,
    MonadIO m
  ) =>
  SourceName ->
  ModelSourceType ->
  MutationDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m (API.MutationRequest, [ModelNameInfo])
translateMutationDB sourceName modelSourceType = \case
  MDBInsert insert -> do
    (insertOperation, (tableRelationships, redactionExpressionState, API.InterpolatedQueries interpolatedQueries, tableInsertSchemas)) <-
      flip runStateT (mempty, RedactionExpressionState mempty, mempty, mempty) $ translateInsert insert
    unless (null interpolatedQueries) do
      -- TODO: See if we can allow this in mutations
      throw400 NotSupported "translateMutationDB: Native Queries not supported in insert operations."
    let apiTableInsertSchema =
          unTableInsertSchemas tableInsertSchemas
            & HashMap.toList
            & fmap (\(tableName, TableInsertSchema {..}) -> API.TableInsertSchema tableName _tisPrimaryKey _tisFields)
    let apiTableRelationships = Set.fromList $ API.RTable . uncurry API.TableRelationships <$> mapMaybe tableKey (HashMap.toList (unTableRelationships tableRelationships))
    let (modelName, modelType) = (toTxt $ _aiTableName $ _aiData insert, ModelTypeTable)
    let outputInsertMut = _aiOutput insert
    argModels <- do
      (_, res') <- flip runStateT [] $ getMutationInsertArgumentModelNamesDC sourceName modelSourceType $ _aiData insert
      return res'
    let modelNames = [ModelNameInfo (modelName, modelType, sourceName, modelSourceType)] <> argModels <> getMutationOutputModelNamesGen sourceName modelSourceType outputInsertMut
    pure
      $ ( API.MutationRequest
            { _mrRelationships = apiTableRelationships,
              _mrRedactionExpressions = translateRedactionExpressions redactionExpressionState,
              _mrInsertSchema = Set.fromList apiTableInsertSchema,
              _mrOperations = [API.InsertOperation insertOperation]
            },
          modelNames
        )
  MDBUpdate update -> do
    (updateOperations, (tableRelationships, redactionExpressionState, API.InterpolatedQueries interpolatedQueries)) <-
      flip runStateT (mempty, RedactionExpressionState mempty, mempty) $ translateUpdate update

    unless (null interpolatedQueries) do
      -- TODO: See if we can allow this in mutations
      throw400 NotSupported "translateMutationDB: Native Queries not supported in update operations."

    let apiTableRelationships =
          Set.fromList
            $ API.RTable
            . uncurry API.TableRelationships
            <$> mapMaybe tableKey (HashMap.toList (unTableRelationships tableRelationships))
    let getWhereClauseModels updateBatch' = do
          (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType $ _ubWhere updateBatch'
          return res
    let (modelName, modelType) = (toTxt $ _auTable update, ModelTypeTable)
    let returnModels = getMutationOutputModelNamesGen sourceName modelSourceType (_auOutput update)
    let mutationUpdateVariant = _auUpdateVariant update
    argModelNames <- case mutationUpdateVariant of
      SingleBatch updateBatch -> getWhereClauseModels updateBatch
      MultipleBatches updateBatchList -> do
        whereModelsList <-
          forM updateBatchList $ \updateBatch -> getWhereClauseModels updateBatch
        pure $ concat whereModelsList
    let modelNames = [ModelNameInfo (modelName, modelType, sourceName, modelSourceType)] <> (argModelNames) <> (returnModels)
    pure
      $ ( API.MutationRequest
            { _mrRelationships = apiTableRelationships,
              _mrRedactionExpressions = translateRedactionExpressions redactionExpressionState,
              _mrInsertSchema = mempty,
              _mrOperations = API.UpdateOperation <$> updateOperations
            },
          modelNames
        )
  MDBDelete delete -> do
    (deleteOperation, (tableRelationships, redactionExpressionState, API.InterpolatedQueries interpolatedQueries)) <-
      flip runStateT (mempty, RedactionExpressionState mempty, mempty) $ translateDelete delete

    unless (null interpolatedQueries) do
      -- TODO: See if we can allow this in mutations
      throw400 NotSupported "translateMutationDB: Native Queries not supported in delete operations."

    let apiTableRelationships =
          Set.fromList
            $ API.RTable
            . uncurry API.TableRelationships
            <$> mapMaybe tableKey (HashMap.toList (unTableRelationships tableRelationships))
    let getWhereClauseModels boolExp = do
          (_, res) <- flip runStateT [] $ getArgumentModelNamesGen sourceName modelSourceType boolExp
          res
    let (modelName, modelType) = (toTxt $ _adTable delete, ModelTypeTable)
        returnModels = getMutationOutputModelNamesGen sourceName modelSourceType (_adOutput delete)
        argModelNames = getWhereClauseModels $ snd $ _adWhere delete
        modelNames = [ModelNameInfo (modelName, modelType, sourceName, modelSourceType)] <> (argModelNames) <> (returnModels)
    pure
      $ ( API.MutationRequest
            { _mrRelationships = apiTableRelationships,
              _mrRedactionExpressions = translateRedactionExpressions redactionExpressionState,
              _mrInsertSchema = mempty,
              _mrOperations = [API.DeleteOperation deleteOperation]
            },
          modelNames
        )
  MDBFunction _returnsSet _select ->
    throw400 NotSupported "translateMutationDB: function mutations not implemented for the Data Connector backend."

tableKey :: (API.TargetName, c) -> Maybe (API.TableName, c)
tableKey (API.TNTable t, x) = Just (t, x)
tableKey _ = Nothing

translateInsert ::
  ( MonadState state m,
    Has TableRelationships state,
    Has RedactionExpressionState state,
    Has TableInsertSchemas state,
    Has API.InterpolatedQueries state,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r,
    MonadIO m
  ) =>
  AnnotatedInsert 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m API.InsertMutationOperation
translateInsert AnnotatedInsert {_aiData = AnnotatedInsertData {..}, ..} = do
  captureTableInsertSchema tableName _aiTableColumns _aiPrimaryKey _aiExtraTableMetadata
  rows <- traverse (translateInsertRow tableName _aiTableColumns _aiPresetValues) _aiInsertObject
  postInsertCheck <- translateBoolExpToExpression (API.TNTable tableName) insertCheckCondition
  returningFields <- translateMutationOutputToReturningFields tableName _aiOutput
  pure
    $ API.InsertMutationOperation
      { API._imoTable = tableName,
        API._imoRows = rows,
        API._imoPostInsertCheck = postInsertCheck,
        API._imoReturningFields = HashMap.mapKeys (API.FieldName . getFieldNameTxt) returningFields
      }
  where
    tableName = Witch.from _aiTableName
    -- Update check condition must be used once upserts are supported
    (insertCheckCondition, _updateCheckCondition) = _aiCheckCondition

captureTableInsertSchema ::
  ( MonadState state m,
    Has TableInsertSchemas state
  ) =>
  API.TableName ->
  [ColumnInfo 'DataConnector] ->
  Maybe (NESeq ColumnName) ->
  ExtraTableMetadata ->
  m ()
captureTableInsertSchema tableName tableColumns primaryKey ExtraTableMetadata {..} = do
  let fieldSchemas =
        tableColumns
          & fmap
            ( \ColumnInfo {..} ->
                let extraColumnMetadata = HashMap.lookup ciColumn _etmExtraColumnMetadata
                    scalarType = columnTypeToScalarType ciType
                    valueGenerated = extraColumnMetadata >>= _ecmValueGenerated
                    fieldName = API.FieldName $ G.unName ciName
                    columnInsertSchema = API.ColumnInsert $ API.ColumnInsertSchema (Witch.from ciColumn) (API.ColumnTypeScalar $ Witch.from scalarType) ciIsNullable valueGenerated
                 in (fieldName, columnInsertSchema)
            )
          & HashMap.fromList
  let primaryKey' = fmap Witch.from . toNonEmpty <$> primaryKey
  recordTableInsertSchema tableName $ TableInsertSchema primaryKey' fieldSchemas

translateInsertRow ::
  ( MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r
  ) =>
  API.TableName ->
  [ColumnInfo 'DataConnector] ->
  HashMap ColumnName (UnpreparedValue 'DataConnector) ->
  AnnotatedInsertRow 'DataConnector (UnpreparedValue 'DataConnector) ->
  m API.RowObject
translateInsertRow tableName tableColumns defaultColumnValues insertRow = do
  columnSchemasAndValues <- forM (HashMap.toList columnUnpreparedValues) $ \(columnName, columnValue) -> do
    fieldName <-
      case find (\ColumnInfo {..} -> ciColumn == columnName) tableColumns of
        Just ColumnInfo {..} -> pure . API.FieldName $ G.unName ciName
        Nothing -> throw500 $ "Can't find column " <> toTxt columnName <> " in table schema for " <> API.tableNameToText tableName
    preparedLiteral <- prepareLiteral columnValue

    value <-
      case preparedLiteral of
        ValueLiteral _scalarType value -> pure value
        ArrayLiteral _scalarType _values -> throw400 NotSupported "translateInsertRow: Array literals are not supported as column insert values"

    pure (fieldName, value)

  let rowObject =
        columnSchemasAndValues
          & fmap (second API.mkColumnInsertFieldValue)
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
  ( MonadState state m,
    Has TableRelationships state,
    Has RedactionExpressionState state,
    Has API.InterpolatedQueries state,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r,
    MonadIO m
  ) =>
  AnnotatedUpdateG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m [API.UpdateMutationOperation]
translateUpdate annUpdate@AnnotatedUpdateG {..} = do
  case _auUpdateVariant of
    SingleBatch batch -> (: []) <$> translateUpdateBatch annUpdate batch
    MultipleBatches batches -> traverse (translateUpdateBatch annUpdate) batches

translateUpdateBatch ::
  ( MonadState state m,
    Has TableRelationships state,
    Has RedactionExpressionState state,
    Has API.InterpolatedQueries state,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    MonadIO m,
    Has SessionVariables r
  ) =>
  AnnotatedUpdateG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  UpdateBatch 'DataConnector UpdateOperator (UnpreparedValue 'DataConnector) ->
  m API.UpdateMutationOperation
translateUpdateBatch AnnotatedUpdateG {..} UpdateBatch {..} = do
  updates <- translateUpdateOperations _ubOperations
  whereExp <- translateBoolExpToExpression (API.TNTable tableName) (BoolAnd [_auUpdatePermissions, _ubWhere])
  postUpdateCheck <- translateBoolExpToExpression (API.TNTable tableName) _auCheck
  returningFields <- translateMutationOutputToReturningFields tableName _auOutput

  pure
    $ API.UpdateMutationOperation
      { API._umoTable = tableName,
        API._umoWhere = whereExp,
        API._umoUpdates = updates,
        API._umoPostUpdateCheck = postUpdateCheck,
        API._umoReturningFields = HashMap.mapKeys (API.FieldName . getFieldNameTxt) returningFields
      }
  where
    tableName :: API.TableName = Witch.from _auTable

translateUpdateOperations ::
  forall m r.
  ( MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r
  ) =>
  HashMap ColumnName (UpdateOperator (UnpreparedValue 'DataConnector)) ->
  m (Set API.RowUpdate)
translateUpdateOperations columnUpdates =
  fmap Set.fromList . forM (HashMap.toList columnUpdates) $ \(columnName, updateOperator) -> do
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
      preparedLiteral <- prepareLiteral unpreparedValue
      case preparedLiteral of
        ValueLiteral scalarType value -> pure (scalarType, value)
        ArrayLiteral _scalarType _values -> throw400 NotSupported "translateUpdateOperations: Array literals are not supported as column update values"

translateDelete ::
  ( MonadState state m,
    Has TableRelationships state,
    Has RedactionExpressionState state,
    Has API.InterpolatedQueries state,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    MonadIO m,
    Has SessionVariables r
  ) =>
  AnnDelG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m API.DeleteMutationOperation
translateDelete AnnDel {..} = do
  whereExp <- translateBoolExpToExpression (API.TNTable tableName) (BoolAnd [permissionFilter, whereClause])
  returningFields <- translateMutationOutputToReturningFields tableName _adOutput
  pure
    $ API.DeleteMutationOperation
      { API._dmoTable = tableName,
        API._dmoWhere = whereExp,
        API._dmoReturningFields = HashMap.mapKeys (API.FieldName . getFieldNameTxt) returningFields
      }
  where
    tableName :: API.TableName = Witch.from _adTable
    (permissionFilter, whereClause) = _adWhere

translateMutationOutputToReturningFields ::
  ( MonadState state m,
    Has TableRelationships state,
    Has RedactionExpressionState state,
    Has API.InterpolatedQueries state,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r,
    MonadIO m
  ) =>
  API.TableName ->
  MutationOutputG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m (HashMap FieldName API.Field)
translateMutationOutputToReturningFields tableName = \case
  MOutSinglerowObject annFields ->
    translateAnnFields noPrefix (API.TNTable tableName) annFields
  MOutMultirowFields mutFields ->
    HashMap.unions <$> traverse (uncurry $ translateMutField tableName) mutFields

translateMutField ::
  ( MonadState state m,
    Has TableRelationships state,
    Has RedactionExpressionState state,
    Has API.InterpolatedQueries state,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r,
    MonadIO m
  ) =>
  API.TableName ->
  FieldName ->
  MutFldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m (HashMap FieldName API.Field)
translateMutField tableName fieldName = \case
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
    translateAnnFields (prefixWith fieldName) (API.TNTable tableName) annFields

--------------------------------------------------------------------------------

reshapeResponseToMutationGqlShape ::
  (MonadError QErr m) =>
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
  (MonadError QErr m) =>
  MutationOutputG 'DataConnector Void v ->
  API.MutationResponse ->
  m J.Encoding
reshapeOutputForSingleBatchOperation mutationOutput API.MutationResponse {..} = do
  mutationOperationResult <-
    listToMaybe _mrOperationResults
      `onNothing` throw500 "Unable to find expected mutation operation results"
  reshapeMutationOutput mutationOutput mutationOperationResult

reshapeOutputForMultipleBatchOperation ::
  (MonadError QErr m) =>
  [MutationOutputG 'DataConnector Void v] ->
  API.MutationResponse ->
  m J.Encoding
reshapeOutputForMultipleBatchOperation mutationOutputs API.MutationResponse {..} = do
  unless (operationResultCount >= requiredResultCount)
    $ throw500 ("Data Connector agent returned " <> tshow operationResultCount <> " mutation operation results where at least " <> tshow requiredResultCount <> " was expected")

  reshapedResults <-
    zip mutationOutputs _mrOperationResults
      & traverse (uncurry reshapeMutationOutput)

  pure $ JE.list id reshapedResults
  where
    requiredResultCount = length mutationOutputs
    operationResultCount = length _mrOperationResults

reshapeMutationOutput ::
  (MonadError QErr m) =>
  MutationOutputG 'DataConnector Void v ->
  API.MutationOperationResults ->
  m J.Encoding
reshapeMutationOutput mutationOutput mutationOperationResults =
  case mutationOutput of
    MOutSinglerowObject annFields -> reshapeReturningRows Single noPrefix annFields mutationOperationResults
    MOutMultirowFields mutFields -> reshapeMutFields mutFields mutationOperationResults

reshapeReturningRows ::
  (MonadError QErr m) =>
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
  (MonadError QErr m) =>
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
