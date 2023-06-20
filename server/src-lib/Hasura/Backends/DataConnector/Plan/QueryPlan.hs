module Hasura.Backends.DataConnector.Plan.QueryPlan
  ( -- Main external interface
    mkQueryPlan,
    -- Internals reused by other plan modules
    translateAnnSimpleSelectToQueryRequest,
    translateAnnAggregateSelectToQueryRequest,
    translateAnnFields,
    reshapeSimpleSelectRows,
    reshapeTableAggregateFields,
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
import Data.Set qualified as Set
import Data.Text.Extended (toTxt)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Backend
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.DataConnector.Plan.Common
import Hasura.Base.Error
import Hasura.Function.Cache qualified as Function
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Witch qualified

--------------------------------------------------------------------------------

data FieldsAndAggregates = FieldsAndAggregates
  { _faaFields :: Maybe (HashMap FieldName API.Field),
    _faaAggregates :: Maybe (HashMap FieldName API.Aggregate)
  }
  deriving stock (Show, Eq)

instance Semigroup FieldsAndAggregates where
  left <> right =
    FieldsAndAggregates
      (_faaFields left <> _faaFields right)
      (_faaAggregates left <> _faaAggregates right)

instance Monoid FieldsAndAggregates where
  mempty = FieldsAndAggregates Nothing Nothing

--------------------------------------------------------------------------------

-- | Map a 'QueryDB 'DataConnector' term into a 'Plan'
mkQueryPlan ::
  forall m.
  (MonadError QErr m) =>
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
        QDBMultipleRows simpleSelect -> translateAnnSimpleSelectToQueryRequest sessionVariables simpleSelect
        QDBSingleRow simpleSelect -> translateAnnSimpleSelectToQueryRequest sessionVariables simpleSelect
        QDBAggregation aggregateSelect -> translateAnnAggregateSelectToQueryRequest sessionVariables aggregateSelect

translateAnnSimpleSelectToQueryRequest ::
  forall m.
  (MonadError QErr m) =>
  SessionVariables ->
  AnnSimpleSelectG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m API.QueryRequest
translateAnnSimpleSelectToQueryRequest sessionVariables simpleSelect =
  translateAnnSelectToQueryRequest sessionVariables (translateAnnFieldsWithNoAggregates sessionVariables noPrefix) simpleSelect

translateAnnAggregateSelectToQueryRequest ::
  forall m.
  (MonadError QErr m) =>
  SessionVariables ->
  AnnAggregateSelectG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m API.QueryRequest
translateAnnAggregateSelectToQueryRequest sessionVariables aggregateSelect =
  translateAnnSelectToQueryRequest sessionVariables (translateTableAggregateFields sessionVariables) aggregateSelect

translateAnnSelectToQueryRequest ::
  forall m fieldType.
  (MonadError QErr m) =>
  SessionVariables ->
  (TableRelationshipsKey -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT TableRelationships m FieldsAndAggregates) ->
  AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector) ->
  m API.QueryRequest
translateAnnSelectToQueryRequest sessionVariables translateFieldsAndAggregates selectG = do
  case _asnFrom selectG of
    FromIdentifier _ -> throw400 NotSupported "AnnSelectG: FromIdentifier not supported"
    FromNativeQuery {} -> throw400 NotSupported "AnnSelectG: FromNativeQuery not supported"
    FromStoredProcedure {} -> throw400 NotSupported "AnnSelectG: FromStoredProcedure not supported"
    FromTable tableName -> do
      (query, TableRelationships tableRelationships) <-
        CPS.runWriterT (translateAnnSelect sessionVariables translateFieldsAndAggregates (TableNameKey (Witch.into tableName)) selectG)
      let relationships = mkRelationships <$> HashMap.toList tableRelationships
      pure
        $ API.QRTable
          API.TableRequest
            { _trTable = Witch.into tableName,
              _trRelationships = Set.fromList relationships,
              _trQuery = query,
              _trForeach = Nothing
            }
    FromFunction fn@(FunctionName functionName) argsExp _dListM -> do
      args <- mkArgs sessionVariables argsExp fn
      (query, TableRelationships tableRelationships) <-
        CPS.runWriterT (translateAnnSelect sessionVariables translateFieldsAndAggregates (FunctionNameKey (Witch.into functionName)) selectG)
      let relationships = mkRelationships <$> HashMap.toList tableRelationships
      pure
        $ API.QRFunction
          API.FunctionRequest
            { _frFunction = Witch.into functionName,
              _frRelationships = Set.fromList relationships,
              _frQuery = query,
              _frFunctionArguments = args
            }

mkRelationships :: (TableRelationshipsKey, (HashMap API.RelationshipName API.Relationship)) -> API.Relationships
mkRelationships (FunctionNameKey functionName, relationships) = API.RFunction (API.FunctionRelationships functionName relationships)
mkRelationships (TableNameKey tableName, relationships) = API.RTable (API.TableRelationships tableName relationships)

mkArgs ::
  ( MonadError QErr m
  ) =>
  SessionVariables ->
  Function.FunctionArgsExpG (ArgumentExp (UnpreparedValue 'DataConnector)) ->
  FunctionName ->
  m [API.FunctionArgument]
mkArgs sessionVariables (Function.FunctionArgsExp ps ns) functionName = do
  unless (null ps) $ throw400 NotSupported $ "Positional arguments not supported in function " <> toTxt functionName
  getNamed
  where
    getNamed = mapM mkArg (HashMap.toList ns)
    mkArg (n, input) = (API.NamedArgument n . API.ScalarArgumentValue) <$> getValue input

    getValue (AEInput x) = case x of
      UVLiteral _ -> throw400 NotSupported "Literal not supported in Data Connector function args."
      UVSessionVar _ _ -> throw400 NotSupported "SessionVar not supported in Data Connector function args."
      UVParameter _ (ColumnValue t v) -> pure (API.ScalarValue v (coerce (toTxt t)))
      UVSession -> pure (API.ScalarValue (J.toJSON sessionVariables) (API.ScalarType "json"))

translateAnnSelect ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  (TableRelationshipsKey -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT writerOutput m FieldsAndAggregates) ->
  TableRelationshipsKey ->
  AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m API.Query
translateAnnSelect sessionVariables translateFieldsAndAggregates entityName selectG = do
  FieldsAndAggregates {..} <- translateFieldsAndAggregates entityName (_asnFields selectG)
  let whereClauseWithPermissions =
        case _saWhere (_asnArgs selectG) of
          Just expr -> BoolAnd [expr, _tpFilter (_asnPerm selectG)]
          Nothing -> _tpFilter (_asnPerm selectG)
  whereClause <- translateBoolExpToExpression sessionVariables entityName whereClauseWithPermissions
  orderBy <- traverse (translateOrderBy sessionVariables entityName) (_saOrderBy $ _asnArgs selectG)
  pure
    API.Query
      { _qFields = mapFieldNameHashMap <$> _faaFields,
        _qAggregates = mapFieldNameHashMap <$> _faaAggregates,
        _qAggregatesLimit = _saLimit (_asnArgs selectG) <* _faaAggregates, -- Only include the aggregates limit if we actually have aggregrates
        _qLimit =
          fmap getMin
            $ foldMap
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
  TableRelationshipsKey ->
  NE.NonEmpty (AnnotatedOrderByItemG 'DataConnector (UnpreparedValue 'DataConnector)) ->
  CPS.WriterT writerOutput m API.OrderBy
translateOrderBy sessionVariables sourceName orderByItems = do
  orderByElementsAndRelations <- for orderByItems \OrderByItemG {..} -> do
    let orderDirection = maybe API.Ascending Witch.from obiType
    translateOrderByElement sessionVariables sourceName orderDirection [] obiColumn
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
  TableRelationshipsKey ->
  API.OrderDirection ->
  [API.RelationshipName] ->
  AnnotatedOrderByElement 'DataConnector (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m (API.OrderByElement, HashMap API.RelationshipName API.OrderByRelation)
translateOrderByElement sessionVariables sourceName orderDirection targetReversePath = \case
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
    (relationshipName, API.Relationship {..}) <- recordTableRelationshipFromRelInfo sourceName relationshipInfo
    (translatedOrderByElement, subOrderByRelations) <- translateOrderByElement sessionVariables (TableNameKey _rTargetTable) orderDirection (relationshipName : targetReversePath) orderByElement

    targetTableWhereExp <- translateBoolExpToExpression sessionVariables (TableNameKey _rTargetTable) filterExp
    let orderByRelations = HashMap.fromList [(relationshipName, API.OrderByRelation targetTableWhereExp subOrderByRelations)]

    pure (translatedOrderByElement, orderByRelations)
  AOCArrayAggregation relationshipInfo filterExp aggregateOrderByElement -> do
    (relationshipName, API.Relationship {..}) <- recordTableRelationshipFromRelInfo sourceName relationshipInfo
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

    targetTableWhereExp <- translateBoolExpToExpression sessionVariables (TableNameKey _rTargetTable) filterExp
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
  TableRelationshipsKey ->
  AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m FieldsAndAggregates
translateAnnFieldsWithNoAggregates sessionVariables fieldNamePrefix sourceName fields =
  (\fields' -> FieldsAndAggregates (Just fields') Nothing) <$> translateAnnFields sessionVariables fieldNamePrefix sourceName fields

translateAnnFields ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  FieldPrefix ->
  TableRelationshipsKey ->
  AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m (HashMap FieldName API.Field)
translateAnnFields sessionVariables fieldNamePrefix sourceName fields = do
  translatedFields <- traverse (traverse (translateAnnField sessionVariables sourceName)) fields
  pure $ HashMap.fromList (mapMaybe (\(fieldName, field) -> (applyPrefix fieldNamePrefix fieldName,) <$> field) translatedFields)

translateAnnField ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  TableRelationshipsKey ->
  AnnFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m (Maybe API.Field)
translateAnnField sessionVariables sourceTableName = \case
  AFNestedObject nestedObj ->
    Just
      . API.NestedObjField (Witch.from $ _anosColumn nestedObj)
      <$> translateNestedObjectSelect sessionVariables sourceTableName nestedObj
  AFNestedArray _ (ANASSimple field) ->
    fmap mkArrayField <$> translateAnnField sessionVariables sourceTableName field
    where
      mkArrayField nestedField =
        API.NestedArrayField (API.ArrayField nestedField Nothing Nothing Nothing Nothing)
  -- TODO(dmoverton): support limit, offset, where and order_by in ArrayField
  AFNestedArray _ (ANASAggregate _) ->
    pure Nothing -- TODO(dmoverton): support nested array aggregates
  AFColumn colField ->
    -- TODO: make sure certain fields in colField are not in use, since we don't support them
    pure . Just $ API.ColumnField (Witch.from $ _acfColumn colField) (Witch.from . columnTypeToScalarType $ _acfType colField)
  AFObjectRelation objRel ->
    case _aosTarget (_aarAnnSelect objRel) of
      FromTable tableName -> do
        let targetTable = Witch.from tableName
        let relationshipName = mkRelationshipName $ _aarRelationshipName objRel
        fields <- translateAnnFields sessionVariables noPrefix (TableNameKey targetTable) (_aosFields (_aarAnnSelect objRel))
        whereClause <- translateBoolExpToExpression sessionVariables (TableNameKey targetTable) (_aosTargetFilter (_aarAnnSelect objRel))

        recordTableRelationship
          sourceTableName
          relationshipName
          API.Relationship
            { _rTargetTable = targetTable,
              _rRelationshipType = API.ObjectRelationship,
              _rColumnMapping = HashMap.fromList $ bimap Witch.from Witch.from <$> HashMap.toList (_aarColumnMapping objRel)
            }

        pure
          . Just
          . API.RelField
          $ API.RelationshipField
            relationshipName
            ( API.Query
                { _qFields = Just $ mapFieldNameHashMap fields,
                  _qAggregates = mempty,
                  _qWhere = whereClause,
                  _qAggregatesLimit = Nothing,
                  _qLimit = Nothing,
                  _qOffset = Nothing,
                  _qOrderBy = Nothing
                }
            )
      other -> error $ "translateAnnField: " <> show other
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
  TableRelationshipsKey ->
  (TableRelationshipsKey -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT writerOutput m FieldsAndAggregates) ->
  AnnRelationSelectG 'DataConnector (AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector)) ->
  CPS.WriterT writerOutput m API.Field
translateArrayRelationSelect sessionVariables sourceName translateFieldsAndAggregates arrRel = do
  case _asnFrom (_aarAnnSelect arrRel) of
    FromIdentifier _ -> lift $ throw400 NotSupported "AnnSelectG: FromIdentifier not supported"
    FromNativeQuery {} -> lift $ throw400 NotSupported "AnnSelectG: FromNativeQuery not supported"
    FromStoredProcedure {} -> lift $ throw400 NotSupported "AnnSelectG: FromStoredProcedure not supported"
    FromFunction {} -> lift $ throw400 NotSupported "translateArrayRelationSelect: FromFunction not currently supported"
    FromTable targetTable -> do
      query <- translateAnnSelect sessionVariables translateFieldsAndAggregates (TableNameKey (Witch.into targetTable)) (_aarAnnSelect arrRel)
      let relationshipName = mkRelationshipName $ _aarRelationshipName arrRel

      recordTableRelationship
        sourceName
        relationshipName
        API.Relationship
          { _rTargetTable = Witch.into targetTable,
            _rRelationshipType = API.ArrayRelationship,
            _rColumnMapping = HashMap.fromList $ bimap Witch.from Witch.from <$> HashMap.toList (_aarColumnMapping arrRel)
          }

      pure
        . API.RelField
        $ API.RelationshipField
          relationshipName
          query

translateTableAggregateFields ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  TableRelationshipsKey ->
  TableAggregateFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m FieldsAndAggregates
translateTableAggregateFields sessionVariables sourceName fields = do
  mconcat <$> traverse (uncurry (translateTableAggregateField sessionVariables sourceName)) fields

translateTableAggregateField ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  TableRelationshipsKey ->
  FieldName ->
  TableAggregateFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m FieldsAndAggregates
translateTableAggregateField sessionVariables sourceName fieldName = \case
  TAFAgg aggregateFields -> do
    let fieldNamePrefix = prefixWith fieldName
    translatedAggregateFields <- lift $ mconcat <$> traverse (uncurry (translateAggregateField fieldNamePrefix)) aggregateFields
    pure
      $ FieldsAndAggregates
        Nothing
        (Just translatedAggregateFields)
  TAFNodes _ fields ->
    translateAnnFieldsWithNoAggregates sessionVariables (prefixWith fieldName) sourceName fields
  TAFExp _txt ->
    -- We ignore literal text fields (we don't send them to the data connector agent)
    -- and add them back to the response JSON when we reshape what the agent returns
    -- to us
    pure mempty

translateAggregateField ::
  (MonadError QErr m) =>
  FieldPrefix ->
  FieldName ->
  AggregateField 'DataConnector (UnpreparedValue 'DataConnector) ->
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
        SFCol column resultType ->
          let resultScalarType = Witch.from $ columnTypeToScalarType resultType
           in pure . Just $ (applyPrefix fieldPrefix' columnFieldName, API.SingleColumn $ API.SingleColumnAggregate aggFunction (Witch.from column) resultScalarType)
        SFExp _txt ->
          -- We ignore literal text fields (we don't send them to the data connector agent)
          -- and add them back to the response JSON when we reshape what the agent returns
          -- to us
          pure Nothing
        -- See Hasura.RQL.Types.Backend.supportsAggregateComputedFields
        SFComputedField _ _ -> error "Aggregate computed fields aren't currently supported for Data Connectors!"
  AFExp _txt ->
    -- We ignore literal text fields (we don't send them to the data connector agent)
    -- and add them back to the response JSON when we reshape what the agent returns
    -- to us
    pure mempty

translateSingleColumnAggregateFunction :: (MonadError QErr m) => Text -> m API.SingleColumnAggregateFunction
translateSingleColumnAggregateFunction functionName =
  fmap API.SingleColumnAggregateFunction (G.mkName functionName)
    `onNothing` throw500 ("translateSingleColumnAggregateFunction: Invalid aggregate function encountered: " <> functionName)

translateNestedObjectSelect ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  SessionVariables ->
  TableRelationshipsKey ->
  AnnNestedObjectSelectG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m API.Query
translateNestedObjectSelect sessionVariables relationshipKey selectG = do
  FieldsAndAggregates {..} <- translateAnnFieldsWithNoAggregates sessionVariables noPrefix relationshipKey $ _anosFields selectG
  pure
    API.Query
      { _qFields = mapFieldNameHashMap <$> _faaFields,
        _qAggregates = Nothing,
        _qAggregatesLimit = Nothing,
        _qLimit = Nothing,
        _qOffset = Nothing,
        _qWhere = Nothing,
        _qOrderBy = Nothing
      }

--------------------------------------------------------------------------------

reshapeResponseToQueryShape ::
  (MonadError QErr m) =>
  QueryDB 'DataConnector Void v ->
  API.QueryResponse ->
  m J.Encoding
reshapeResponseToQueryShape queryDb response =
  case queryDb of
    QDBMultipleRows simpleSelect -> reshapeSimpleSelectRows Many (_asnFields simpleSelect) response
    QDBSingleRow simpleSelect -> reshapeSimpleSelectRows Single (_asnFields simpleSelect) response
    QDBAggregation aggregateSelect -> reshapeTableAggregateFields (_asnFields aggregateSelect) response

reshapeSimpleSelectRows ::
  (MonadError QErr m) =>
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
  (MonadError QErr m) =>
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
  (MonadError QErr m) =>
  FieldPrefix ->
  AggregateFields 'DataConnector v ->
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
            SFCol _column _columnType -> do
              let fieldPrefix' = fieldPrefix <> prefixWith fieldName
              let columnFieldNameKey = API.FieldName . getFieldNameTxt $ applyPrefix fieldPrefix' columnFieldName
              responseAggregateValue <-
                HashMap.lookup columnFieldNameKey responseAggregates
                  `onNothing` throw500 ("Unable to find expected aggregate " <> API.unFieldName columnFieldNameKey <> " in aggregates returned by Data Connector agent") -- TODO(dchambers): Add pathing information for error clarity
              pure (columnFieldNameText, J.toEncoding responseAggregateValue)
            SFExp txt ->
              pure (columnFieldNameText, JE.text txt)
            -- See Hasura.RQL.Types.Backend.supportsAggregateComputedFields
            SFComputedField _ _ -> error "Aggregate computed fields aren't currently supported for Data Connectors!"

        pure (fieldNameText, encodeAssocListAsObject reshapedColumnFields)
      AFExp txt ->
        pure (fieldNameText, JE.text txt)
  pure $ encodeAssocListAsObject reshapedFields

reshapeAnnFields ::
  (MonadError QErr m) =>
  FieldPrefix ->
  AnnFieldsG 'DataConnector Void v ->
  HashMap API.FieldName API.FieldValue ->
  m J.Encoding
reshapeAnnFields fieldNamePrefix fields responseRow = do
  reshapedFields <- forM fields $ \(fieldName@(FieldName fieldNameText), field) -> do
    let fieldNameKey = API.FieldName . getFieldNameTxt $ applyPrefix fieldNamePrefix fieldName
    let responseField = fromMaybe API.nullFieldValue $ HashMap.lookup fieldNameKey responseRow
    reshapedField <- reshapeField field responseField
    pure (fieldNameText, reshapedField)

  pure $ encodeAssocListAsObject reshapedFields

reshapeField ::
  (MonadError QErr m) =>
  AnnFieldG 'DataConnector Void v ->
  API.FieldValue ->
  m J.Encoding
reshapeField field responseFieldValue =
  case field of
    AFNestedObject nestedObj ->
      handleNull responseFieldValue
        $ case API.deserializeAsNestedObjFieldValue responseFieldValue of
          Left err -> throw500 $ "Expected object in field returned by Data Connector agent: " <> err -- TODO(dmoverton): Add pathing information for error clarity
          Right nestedResponse ->
            reshapeAnnFields noPrefix (_anosFields nestedObj) nestedResponse
    AFNestedArray _ (ANASSimple arrayField) ->
      handleNull responseFieldValue
        $ case API.deserializeAsNestedArrayFieldValue responseFieldValue of
          Left err -> throw500 $ "Expected array in field returned by Data Connector agent: " <> err -- TODO(dmoverton): Add pathing information for error clarity
          Right arrayResponse ->
            JE.list id <$> traverse (reshapeField arrayField) arrayResponse
    AFNestedArray _ (ANASAggregate _) ->
      throw400 NotSupported "Nested array aggregate not supported"
    AFColumn _columnField -> do
      let columnFieldValue = API.deserializeAsColumnFieldValue responseFieldValue
      pure $ J.toEncoding columnFieldValue
    AFObjectRelation objectRelationField -> do
      case API.deserializeAsRelationshipFieldValue responseFieldValue of
        Left err -> throw500 $ "Found column field value where relationship field value was expected in field returned by Data Connector agent: " <> err -- TODO(dchambers): Add pathing information for error clarity
        Right subqueryResponse ->
          let fields = _aosFields $ _aarAnnSelect objectRelationField
           in reshapeSimpleSelectRows Single fields subqueryResponse
    AFArrayRelation (ASSimple simpleArrayRelationField) ->
      reshapeAnnRelationSelect (reshapeSimpleSelectRows Many) simpleArrayRelationField responseFieldValue
    AFArrayRelation (ASAggregate aggregateArrayRelationField) ->
      reshapeAnnRelationSelect reshapeTableAggregateFields aggregateArrayRelationField responseFieldValue
    AFExpression txt -> pure $ JE.text txt
  where
    handleNull v a =
      if API.isNullFieldValue v
        then pure JE.null_
        else a

reshapeAnnRelationSelect ::
  (MonadError QErr m) =>
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
