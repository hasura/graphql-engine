module Hasura.Backends.DataConnector.Plan.QueryPlan
  ( mkQueryPlan,
    queryHasRelations,
    translateAnnFields,
    reshapeAnnFields,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Trans.Writer.CPS qualified as CPS
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.Bifunctor (Bifunctor (bimap))
import Data.Has (Has)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Min (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Backend
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.DataConnector.Plan.Common
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Witch qualified

--------------------------------------------------------------------------------

data FieldsAndAggregates = FieldsAndAggregates
  { _faaFields :: HashMap FieldName API.Field,
    _faaAggregates :: HashMap FieldName API.Aggregate
  }
  deriving stock (Show, Eq)

instance Semigroup FieldsAndAggregates where
  left <> right =
    FieldsAndAggregates
      (_faaFields left <> _faaFields right)
      (_faaAggregates left <> _faaAggregates right)

instance Monoid FieldsAndAggregates where
  mempty = FieldsAndAggregates mempty mempty

--------------------------------------------------------------------------------

-- | Map a 'QueryDB 'DataConnector' term into a 'Plan'
mkQueryPlan ::
  forall m.
  MonadError QErr m =>
  SessionVariables ->
  SourceConfig ->
  QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m (Plan API.QueryRequest API.QueryResponse)
mkQueryPlan sessionVariables (SourceConfig {}) ir = do
  queryRequest <- translateQueryDB ir
  pure $ Plan queryRequest (reshapeResponseToQueryShape ir)
  where
    translateQueryDB ::
      QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m API.QueryRequest
    translateQueryDB =
      \case
        QDBMultipleRows annSelect -> translateAnnSelectToQueryRequest (translateAnnFieldsWithNoAggregates sessionVariables noPrefix) annSelect
        QDBSingleRow annSelect -> translateAnnSelectToQueryRequest (translateAnnFieldsWithNoAggregates sessionVariables noPrefix) annSelect
        QDBAggregation annSelect -> translateAnnSelectToQueryRequest (translateTableAggregateFields sessionVariables) annSelect

    translateAnnSelectToQueryRequest ::
      (API.TableName -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT TableRelationships m FieldsAndAggregates) ->
      AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector) ->
      m API.QueryRequest
    translateAnnSelectToQueryRequest translateFieldsAndAggregates selectG = do
      tableName <- extractTableName selectG
      (query, (TableRelationships tableRelationships)) <- CPS.runWriterT (translateAnnSelect sessionVariables translateFieldsAndAggregates tableName selectG)
      let apiTableRelationships = uncurry API.TableRelationships <$> HashMap.toList tableRelationships
      pure $
        API.QueryRequest
          { _qrTable = tableName,
            _qrTableRelationships = apiTableRelationships,
            _qrQuery = query
          }

extractTableName :: MonadError QErr m => AnnSelectG 'DataConnector fieldsType valueType -> m API.TableName
extractTableName selectG =
  case _asnFrom selectG of
    FromTable tn -> pure $ Witch.from tn
    FromIdentifier _ -> throw400 NotSupported "AnnSelectG: FromIdentifier not supported"
    FromFunction {} -> throw400 NotSupported "AnnSelectG: FromFunction not supported"

translateAnnSelect ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  (API.TableName -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT writerOutput m FieldsAndAggregates) ->
  API.TableName ->
  AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m API.Query
translateAnnSelect sessionVariables translateFieldsAndAggregates tableName selectG = do
  FieldsAndAggregates {..} <- translateFieldsAndAggregates tableName (_asnFields selectG)
  let whereClauseWithPermissions =
        case _saWhere (_asnArgs selectG) of
          Just expr -> BoolAnd [expr, _tpFilter (_asnPerm selectG)]
          Nothing -> _tpFilter (_asnPerm selectG)
  whereClause <- translateBoolExpToExpression sessionVariables tableName whereClauseWithPermissions
  orderBy <- traverse (translateOrderBy sessionVariables tableName) (_saOrderBy $ _asnArgs selectG)
  pure
    API.Query
      { _qFields = mapFieldNameHashMap _faaFields,
        _qAggregates = mapFieldNameHashMap _faaAggregates,
        _qLimit =
          fmap getMin $
            foldMap
              (fmap Min)
              [ _saLimit (_asnArgs selectG),
                _tpLimit (_asnPerm selectG)
              ],
        _qOffset = fmap fromIntegral (_saOffset (_asnArgs selectG)),
        _qWhere = whereClause,
        _qOrderBy = orderBy
      }

translateOrderBy ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  API.TableName ->
  NE.NonEmpty (AnnotatedOrderByItemG 'DataConnector (UnpreparedValue 'DataConnector)) ->
  CPS.WriterT writerOutput m API.OrderBy
translateOrderBy sessionVariables sourceTableName orderByItems = do
  orderByElementsAndRelations <- for orderByItems \OrderByItemG {..} -> do
    let orderDirection = maybe API.Ascending Witch.from obiType
    translateOrderByElement sessionVariables sourceTableName orderDirection [] obiColumn
  relations <- lift . mergeOrderByRelations $ snd <$> orderByElementsAndRelations
  pure
    API.OrderBy
      { _obRelations = relations,
        _obElements = fst <$> orderByElementsAndRelations
      }

translateOrderByElement ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  API.TableName ->
  API.OrderDirection ->
  [API.RelationshipName] ->
  AnnotatedOrderByElement 'DataConnector (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m (API.OrderByElement, HashMap API.RelationshipName API.OrderByRelation)
translateOrderByElement sessionVariables sourceTableName orderDirection targetReversePath = \case
  AOCColumn (ColumnInfo {..}) ->
    pure
      ( API.OrderByElement
          { _obeTargetPath = reverse targetReversePath,
            _obeTarget = API.OrderByColumn $ Witch.from ciColumn,
            _obeOrderDirection = orderDirection
          },
        mempty
      )
  AOCObjectRelation relationshipInfo filterExp orderByElement -> do
    (relationshipName, API.Relationship {..}) <- recordTableRelationshipFromRelInfo sourceTableName relationshipInfo
    (translatedOrderByElement, subOrderByRelations) <- translateOrderByElement sessionVariables _rTargetTable orderDirection (relationshipName : targetReversePath) orderByElement

    targetTableWhereExp <- translateBoolExpToExpression sessionVariables _rTargetTable filterExp
    let orderByRelations = HashMap.fromList [(relationshipName, API.OrderByRelation targetTableWhereExp subOrderByRelations)]

    pure (translatedOrderByElement, orderByRelations)
  AOCArrayAggregation relationshipInfo filterExp aggregateOrderByElement -> do
    (relationshipName, API.Relationship {..}) <- recordTableRelationshipFromRelInfo sourceTableName relationshipInfo
    orderByTarget <- case aggregateOrderByElement of
      AAOCount ->
        pure API.OrderByStarCountAggregate
      AAOOp aggFunctionTxt resultType ColumnInfo {..} -> do
        aggFunction <- lift $ translateSingleColumnAggregateFunction aggFunctionTxt
        let resultScalarType = Witch.from $ columnTypeToScalarType resultType
        pure . API.OrderBySingleColumnAggregate $ API.SingleColumnAggregate aggFunction (Witch.from ciColumn) resultScalarType

    let translatedOrderByElement =
          API.OrderByElement
            { _obeTargetPath = reverse (relationshipName : targetReversePath),
              _obeTarget = orderByTarget,
              _obeOrderDirection = orderDirection
            }

    targetTableWhereExp <- translateBoolExpToExpression sessionVariables _rTargetTable filterExp
    let orderByRelations = HashMap.fromList [(relationshipName, API.OrderByRelation targetTableWhereExp mempty)]
    pure (translatedOrderByElement, orderByRelations)

mergeOrderByRelations ::
  forall m f.
  (MonadError QErr m, Foldable f) =>
  f (HashMap API.RelationshipName API.OrderByRelation) ->
  m (HashMap API.RelationshipName API.OrderByRelation)
mergeOrderByRelations orderByRelationsList =
  foldM mergeMap mempty orderByRelationsList
  where
    mergeMap :: HashMap API.RelationshipName API.OrderByRelation -> HashMap API.RelationshipName API.OrderByRelation -> m (HashMap API.RelationshipName API.OrderByRelation)
    mergeMap left right = foldM (\targetMap (relName, orderByRel) -> HashMap.alterF (maybe (pure $ Just orderByRel) (fmap Just . mergeOrderByRelation orderByRel)) relName targetMap) left $ HashMap.toList right

    mergeOrderByRelation :: API.OrderByRelation -> API.OrderByRelation -> m API.OrderByRelation
    mergeOrderByRelation right left =
      if API._obrWhere left == API._obrWhere right
        then do
          mergedSubrelations <- mergeMap (API._obrSubrelations left) (API._obrSubrelations right)
          pure $ API.OrderByRelation (API._obrWhere left) mergedSubrelations
        else throw500 "mergeOrderByRelations: Differing filter expressions found for the same table"

translateAnnFieldsWithNoAggregates ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  FieldPrefix ->
  API.TableName ->
  AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m FieldsAndAggregates
translateAnnFieldsWithNoAggregates sessionVariables fieldNamePrefix sourceTableName fields =
  (\fields' -> FieldsAndAggregates fields' mempty) <$> translateAnnFields sessionVariables fieldNamePrefix sourceTableName fields

translateAnnFields ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  FieldPrefix ->
  API.TableName ->
  AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m (HashMap FieldName API.Field)
translateAnnFields sessionVariables fieldNamePrefix sourceTableName fields = do
  translatedFields <- traverse (traverse (translateAnnField sessionVariables sourceTableName)) fields
  pure $ HashMap.fromList (mapMaybe (\(fieldName, field) -> (applyPrefix fieldNamePrefix fieldName,) <$> field) translatedFields)

translateAnnField ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  API.TableName ->
  AnnFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m (Maybe API.Field)
translateAnnField sessionVariables sourceTableName = \case
  AFColumn colField ->
    -- TODO: make sure certain fields in colField are not in use, since we don't
    -- support them
    pure . Just $ API.ColumnField (Witch.from $ _acfColumn colField) (Witch.from . columnTypeToScalarType $ _acfType colField)
  AFObjectRelation objRel -> do
    let targetTable = Witch.from $ _aosTableFrom (_aarAnnSelect objRel)
    let relationshipName = mkRelationshipName $ _aarRelationshipName objRel
    fields <- translateAnnFields sessionVariables noPrefix targetTable (_aosFields (_aarAnnSelect objRel))
    whereClause <- translateBoolExpToExpression sessionVariables targetTable (_aosTableFilter (_aarAnnSelect objRel))

    recordTableRelationship
      sourceTableName
      relationshipName
      API.Relationship
        { _rTargetTable = targetTable,
          _rRelationshipType = API.ObjectRelationship,
          _rColumnMapping = HashMap.fromList $ bimap Witch.from Witch.from <$> HashMap.toList (_aarColumnMapping objRel)
        }

    pure . Just . API.RelField $
      API.RelationshipField
        relationshipName
        ( API.Query
            { _qFields = mapFieldNameHashMap fields,
              _qAggregates = mempty,
              _qWhere = whereClause,
              _qLimit = Nothing,
              _qOffset = Nothing,
              _qOrderBy = Nothing
            }
        )
  AFArrayRelation (ASSimple arrayRelationSelect) -> do
    Just <$> translateArrayRelationSelect sessionVariables sourceTableName (translateAnnFieldsWithNoAggregates sessionVariables noPrefix) arrayRelationSelect
  AFArrayRelation (ASAggregate arrayRelationSelect) ->
    Just <$> translateArrayRelationSelect sessionVariables sourceTableName (translateTableAggregateFields sessionVariables) arrayRelationSelect
  AFExpression _literal ->
    -- We ignore literal text fields (we don't send them to the data connector agent)
    -- and add them back to the response JSON when we reshape what the agent returns
    -- to us
    pure Nothing

translateArrayRelationSelect ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  API.TableName ->
  (API.TableName -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT writerOutput m FieldsAndAggregates) ->
  AnnRelationSelectG 'DataConnector (AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector)) ->
  CPS.WriterT writerOutput m API.Field
translateArrayRelationSelect sessionVariables sourceTableName translateFieldsAndAggregates arrRel = do
  targetTable <- lift $ extractTableName (_aarAnnSelect arrRel)
  query <- translateAnnSelect sessionVariables translateFieldsAndAggregates targetTable (_aarAnnSelect arrRel)
  let relationshipName = mkRelationshipName $ _aarRelationshipName arrRel

  recordTableRelationship
    sourceTableName
    relationshipName
    API.Relationship
      { _rTargetTable = targetTable,
        _rRelationshipType = API.ArrayRelationship,
        _rColumnMapping = HashMap.fromList $ bimap Witch.from Witch.from <$> HashMap.toList (_aarColumnMapping arrRel)
      }

  pure . API.RelField $
    API.RelationshipField
      relationshipName
      query

translateTableAggregateFields ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  API.TableName ->
  TableAggregateFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m FieldsAndAggregates
translateTableAggregateFields sessionVariables sourceTableName fields = do
  mconcat <$> traverse (uncurry (translateTableAggregateField sessionVariables sourceTableName)) fields

translateTableAggregateField ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  API.TableName ->
  FieldName ->
  TableAggregateFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m FieldsAndAggregates
translateTableAggregateField sessionVariables sourceTableName fieldName = \case
  TAFAgg aggregateFields -> do
    let fieldNamePrefix = prefixWith fieldName
    translatedAggregateFields <- lift $ mconcat <$> traverse (uncurry (translateAggregateField fieldNamePrefix)) aggregateFields
    pure $
      FieldsAndAggregates
        mempty
        translatedAggregateFields
  TAFNodes _ fields ->
    translateAnnFieldsWithNoAggregates sessionVariables (prefixWith fieldName) sourceTableName fields
  TAFExp _txt ->
    -- We ignore literal text fields (we don't send them to the data connector agent)
    -- and add them back to the response JSON when we reshape what the agent returns
    -- to us
    pure mempty

translateAggregateField ::
  MonadError QErr m =>
  FieldPrefix ->
  FieldName ->
  AggregateField 'DataConnector ->
  m (HashMap FieldName API.Aggregate)
translateAggregateField fieldPrefix fieldName = \case
  AFCount countAggregate ->
    let aggregate =
          case countAggregate of
            StarCount -> API.StarCount
            ColumnCount column -> API.ColumnCount $ API.ColumnCountAggregate {_ccaColumn = Witch.from column, _ccaDistinct = False}
            ColumnDistinctCount column -> API.ColumnCount $ API.ColumnCountAggregate {_ccaColumn = Witch.from column, _ccaDistinct = True}
     in pure $ HashMap.singleton (applyPrefix fieldPrefix fieldName) aggregate
  AFOp AggregateOp {..} -> do
    let fieldPrefix' = fieldPrefix <> prefixWith fieldName
    aggFunction <- translateSingleColumnAggregateFunction _aoOp

    fmap (HashMap.fromList . catMaybes) . forM _aoFields $ \(columnFieldName, columnField) ->
      case columnField of
        CFCol column resultType ->
          let resultScalarType = Witch.from $ columnTypeToScalarType resultType
           in pure . Just $ (applyPrefix fieldPrefix' columnFieldName, API.SingleColumn $ API.SingleColumnAggregate aggFunction (Witch.from column) resultScalarType)
        CFExp _txt ->
          -- We ignore literal text fields (we don't send them to the data connector agent)
          -- and add them back to the response JSON when we reshape what the agent returns
          -- to us
          pure Nothing
  AFExp _txt ->
    -- We ignore literal text fields (we don't send them to the data connector agent)
    -- and add them back to the response JSON when we reshape what the agent returns
    -- to us
    pure mempty

translateSingleColumnAggregateFunction :: MonadError QErr m => Text -> m API.SingleColumnAggregateFunction
translateSingleColumnAggregateFunction functionName =
  fmap API.SingleColumnAggregateFunction (G.mkName functionName)
    `onNothing` throw500 ("translateSingleColumnAggregateFunction: Invalid aggregate function encountered: " <> functionName)

--------------------------------------------------------------------------------

-- | Validate if a 'API.QueryRequest' contains any relationships.
queryHasRelations :: API.QueryRequest -> Bool
queryHasRelations API.QueryRequest {..} = _qrTableRelationships /= mempty

--------------------------------------------------------------------------------

reshapeResponseToQueryShape ::
  MonadError QErr m =>
  QueryDB 'DataConnector Void v ->
  API.QueryResponse ->
  m J.Encoding
reshapeResponseToQueryShape queryDb response =
  case queryDb of
    QDBMultipleRows simpleSelect -> reshapeSimpleSelectRows Many (_asnFields simpleSelect) response
    QDBSingleRow simpleSelect -> reshapeSimpleSelectRows Single (_asnFields simpleSelect) response
    QDBAggregation aggregateSelect -> reshapeTableAggregateFields (_asnFields aggregateSelect) response

reshapeSimpleSelectRows ::
  MonadError QErr m =>
  Cardinality ->
  AnnFieldsG 'DataConnector Void v ->
  API.QueryResponse ->
  m J.Encoding
reshapeSimpleSelectRows cardinality fields API.QueryResponse {..} =
  case cardinality of
    Single ->
      case rows of
        [] -> pure $ J.toEncoding J.Null
        [singleRow] -> reshapeAnnFields noPrefix fields singleRow
        _multipleRows ->
          throw500 "Data Connector agent returned multiple rows when only one was expected" -- TODO(dchambers): Add pathing information for error clarity
    Many -> do
      reshapedRows <- traverse (reshapeAnnFields noPrefix fields) rows
      pure $ JE.list id reshapedRows
  where
    rows = fromMaybe mempty _qrRows

reshapeTableAggregateFields ::
  MonadError QErr m =>
  TableAggregateFieldsG 'DataConnector Void v ->
  API.QueryResponse ->
  m J.Encoding
reshapeTableAggregateFields tableAggregateFields API.QueryResponse {..} = do
  reshapedFields <- forM tableAggregateFields $ \(fieldName@(FieldName fieldNameText), tableAggregateField) -> do
    case tableAggregateField of
      TAFAgg aggregateFields -> do
        reshapedAggregateFields <- reshapeAggregateFields (prefixWith fieldName) aggregateFields responseAggregates
        pure $ (fieldNameText, reshapedAggregateFields)
      TAFNodes _ annFields -> do
        reshapedRows <- traverse (reshapeAnnFields (prefixWith fieldName) annFields) responseRows
        pure $ (fieldNameText, JE.list id reshapedRows)
      TAFExp txt ->
        pure $ (fieldNameText, JE.text txt)
  pure $ encodeAssocListAsObject reshapedFields
  where
    responseRows = fromMaybe mempty _qrRows
    responseAggregates = fromMaybe mempty _qrAggregates

reshapeAggregateFields ::
  MonadError QErr m =>
  FieldPrefix ->
  AggregateFields 'DataConnector ->
  HashMap API.FieldName J.Value ->
  m J.Encoding
reshapeAggregateFields fieldPrefix aggregateFields responseAggregates = do
  reshapedFields <- forM aggregateFields $ \(fieldName@(FieldName fieldNameText), aggregateField) ->
    case aggregateField of
      AFCount _countAggregate -> do
        let fieldNameKey = API.FieldName . getFieldNameTxt $ applyPrefix fieldPrefix fieldName
        responseAggregateValue <-
          HashMap.lookup fieldNameKey responseAggregates
            `onNothing` throw500 ("Unable to find expected aggregate " <> API.unFieldName fieldNameKey <> " in aggregates returned by Data Connector agent") -- TODO(dchambers): Add pathing information for error clarity
        pure (fieldNameText, J.toEncoding responseAggregateValue)
      AFOp AggregateOp {..} -> do
        reshapedColumnFields <- forM _aoFields $ \(columnFieldName@(FieldName columnFieldNameText), columnField) ->
          case columnField of
            CFCol _column _columnType -> do
              let fieldPrefix' = fieldPrefix <> prefixWith fieldName
              let columnFieldNameKey = API.FieldName . getFieldNameTxt $ applyPrefix fieldPrefix' columnFieldName
              responseAggregateValue <-
                HashMap.lookup columnFieldNameKey responseAggregates
                  `onNothing` throw500 ("Unable to find expected aggregate " <> API.unFieldName columnFieldNameKey <> " in aggregates returned by Data Connector agent") -- TODO(dchambers): Add pathing information for error clarity
              pure (columnFieldNameText, J.toEncoding responseAggregateValue)
            CFExp txt ->
              pure (columnFieldNameText, JE.text txt)
        pure (fieldNameText, encodeAssocListAsObject reshapedColumnFields)
      AFExp txt ->
        pure (fieldNameText, JE.text txt)
  pure $ encodeAssocListAsObject reshapedFields

reshapeAnnFields ::
  MonadError QErr m =>
  FieldPrefix ->
  AnnFieldsG 'DataConnector Void v ->
  HashMap API.FieldName API.FieldValue ->
  m J.Encoding
reshapeAnnFields fieldNamePrefix fields responseRow = do
  reshapedFields <- forM fields $ \(fieldName@(FieldName fieldNameText), field) -> do
    let fieldNameKey = API.FieldName . getFieldNameTxt $ applyPrefix fieldNamePrefix fieldName
    let responseField =
          HashMap.lookup fieldNameKey responseRow
            `onNothing` throw500 ("Unable to find expected field " <> API.unFieldName fieldNameKey <> " in row returned by Data Connector agent") -- TODO(dchambers): Add pathing information for error clarity
    reshapedField <- reshapeField field responseField
    pure (fieldNameText, reshapedField)

  pure $ encodeAssocListAsObject reshapedFields

reshapeField ::
  MonadError QErr m =>
  AnnFieldG 'DataConnector Void v ->
  m API.FieldValue -> -- This lookup is lazy (behind the monad) so that we can _not_ do it when we've got an AFExpression
  m J.Encoding
reshapeField field responseFieldValue =
  case field of
    AFColumn _columnField -> do
      columnFieldValue <- API.deserializeAsColumnFieldValue <$> responseFieldValue
      pure $ J.toEncoding columnFieldValue
    AFObjectRelation objectRelationField -> do
      relationshipFieldValue <- API.deserializeAsRelationshipFieldValue <$> responseFieldValue
      case relationshipFieldValue of
        Left err -> throw500 $ "Found column field value where relationship field value was expected in field returned by Data Connector agent: " <> err -- TODO(dchambers): Add pathing information for error clarity
        Right subqueryResponse ->
          let fields = _aosFields $ _aarAnnSelect objectRelationField
           in reshapeSimpleSelectRows Single fields subqueryResponse
    AFArrayRelation (ASSimple simpleArrayRelationField) ->
      reshapeAnnRelationSelect (reshapeSimpleSelectRows Many) simpleArrayRelationField =<< responseFieldValue
    AFArrayRelation (ASAggregate aggregateArrayRelationField) ->
      reshapeAnnRelationSelect reshapeTableAggregateFields aggregateArrayRelationField =<< responseFieldValue
    AFExpression txt -> pure $ JE.text txt

reshapeAnnRelationSelect ::
  MonadError QErr m =>
  (Fields (fieldType v) -> API.QueryResponse -> m J.Encoding) ->
  AnnRelationSelectG 'DataConnector (AnnSelectG 'DataConnector fieldType v) ->
  API.FieldValue ->
  m J.Encoding
reshapeAnnRelationSelect reshapeFields annRelationSelect fieldValue =
  case API.deserializeAsRelationshipFieldValue fieldValue of
    Left err -> throw500 $ "Found column field value where relationship field value was expected in field returned by Data Connector agent: " <> err -- TODO(dchambers): Add pathing information for error clarity
    Right subqueryResponse ->
      let annSimpleSelect = _aarAnnSelect annRelationSelect
       in reshapeFields (_asnFields annSimpleSelect) subqueryResponse

--------------------------------------------------------------------------------

mapFieldNameHashMap :: Eq v => HashMap FieldName v -> Maybe (HashMap API.FieldName v)
mapFieldNameHashMap = memptyToNothing . HashMap.mapKeys (API.FieldName . getFieldNameTxt)

memptyToNothing :: (Monoid m, Eq m) => m -> Maybe m
memptyToNothing m = if m == mempty then Nothing else Just m
