module Hasura.Backends.DataConnector.Plan
  ( QueryPlan (..),
    mkPlan,
    renderQuery,
    queryHasRelations,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Trans.Writer.CPS qualified as CPS
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Min (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Extended (toTxt, (<<>), (<>>))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Backend
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend (SessionVarType)
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.Local (RelInfo (..))
import Hasura.SQL.Backend
import Hasura.SQL.Types (CollectableType (..))
import Hasura.Session
import Witch qualified

--------------------------------------------------------------------------------

data QueryPlan = QueryPlan
  { _qpRequest :: API.QueryRequest,
    _qpResponseReshaper :: forall m. (MonadError QErr m) => API.QueryResponse -> m J.Encoding
  }

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

newtype FieldPrefix = FieldPrefix (Maybe FieldName)
  deriving stock (Show, Eq)

instance Semigroup FieldPrefix where
  (FieldPrefix Nothing) <> (FieldPrefix something) = FieldPrefix something
  (FieldPrefix something) <> (FieldPrefix Nothing) = FieldPrefix something
  (FieldPrefix (Just l)) <> (FieldPrefix (Just r)) = FieldPrefix . Just $ l <> "_" <> r

instance Monoid FieldPrefix where
  mempty = FieldPrefix Nothing

noPrefix :: FieldPrefix
noPrefix = FieldPrefix Nothing

prefixWith :: FieldName -> FieldPrefix
prefixWith = FieldPrefix . Just

applyPrefix :: FieldPrefix -> FieldName -> FieldName
applyPrefix (FieldPrefix fieldNamePrefix) fieldName = maybe fieldName (\prefix -> prefix <> "_" <> fieldName) fieldNamePrefix

newtype TableRelationships = TableRelationships
  {unTableRelationships :: HashMap API.TableName (HashMap API.RelationshipName API.Relationship)}
  deriving stock (Eq, Show)

instance Semigroup TableRelationships where
  (TableRelationships l) <> (TableRelationships r) = TableRelationships $ HashMap.unionWith HashMap.union l r

instance Monoid TableRelationships where
  mempty = TableRelationships mempty

-- | Render a 'API.QueryRequest' as 'Text'.
--
-- NOTE: This is for logging and debug purposes only.
renderQuery :: API.QueryRequest -> Text
renderQuery =
  TE.decodeUtf8 . BL.toStrict . J.encode

-- | Map a 'QueryDB 'DataConnector' term into a 'Plan'
mkPlan ::
  forall m.
  MonadError QErr m =>
  SessionVariables ->
  SourceConfig ->
  QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m QueryPlan
mkPlan session (SourceConfig {}) ir = do
  queryRequest <- translateQueryDB ir
  pure $ QueryPlan queryRequest (reshapeResponseToQueryShape ir)
  where
    translateQueryDB ::
      QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m API.QueryRequest
    translateQueryDB =
      \case
        QDBMultipleRows annSelect -> translateAnnSelectToQueryRequest (translateAnnFields noPrefix) annSelect
        QDBSingleRow annSelect -> translateAnnSelectToQueryRequest (translateAnnFields noPrefix) annSelect
        QDBAggregation annSelect -> translateAnnSelectToQueryRequest translateTableAggregateFields annSelect

    translateAnnSelectToQueryRequest ::
      (API.TableName -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT TableRelationships m FieldsAndAggregates) ->
      AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector) ->
      m API.QueryRequest
    translateAnnSelectToQueryRequest translateFieldsAndAggregates selectG = do
      tableName <- extractTableName selectG
      (query, (TableRelationships tableRelationships)) <- CPS.runWriterT (translateAnnSelect translateFieldsAndAggregates tableName selectG)
      let apiTableRelationships = uncurry API.TableRelationships <$> HashMap.toList tableRelationships
      pure $
        API.QueryRequest
          { _qrTable = tableName,
            _qrTableRelationships = apiTableRelationships,
            _qrQuery = query
          }

    extractTableName :: AnnSelectG 'DataConnector fieldsType valueType -> m API.TableName
    extractTableName selectG =
      case _asnFrom selectG of
        FromTable tn -> pure $ Witch.from tn
        FromIdentifier _ -> throw400 NotSupported "AnnSelectG: FromIdentifier not supported"
        FromFunction {} -> throw400 NotSupported "AnnSelectG: FromFunction not supported"

    recordTableRelationship :: API.TableName -> API.RelationshipName -> API.Relationship -> CPS.WriterT TableRelationships m ()
    recordTableRelationship sourceTableName relationshipName relationship =
      CPS.tell . TableRelationships $ HashMap.singleton sourceTableName (HashMap.singleton relationshipName relationship)

    translateAnnSelect ::
      (API.TableName -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT TableRelationships m FieldsAndAggregates) ->
      API.TableName ->
      AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector) ->
      CPS.WriterT TableRelationships m API.Query
    translateAnnSelect translateFieldsAndAggregates tableName selectG = do
      FieldsAndAggregates {..} <- translateFieldsAndAggregates tableName (_asnFields selectG)
      let whereClauseWithPermissions =
            case _saWhere (_asnArgs selectG) of
              Just expr -> BoolAnd [expr, _tpFilter (_asnPerm selectG)]
              Nothing -> _tpFilter (_asnPerm selectG)
      whereClause <- translateBoolExpToExpression tableName whereClauseWithPermissions
      orderBy <- traverse (translateOrderBy tableName) (_saOrderBy $ _asnArgs selectG)
      pure
        API.Query
          { _qFields = memptyToNothing . KM.fromList $ (first (K.fromText . getFieldNameTxt)) <$> HashMap.toList _faaFields,
            _qAggregates = memptyToNothing . KM.fromList $ (first (K.fromText . getFieldNameTxt)) <$> HashMap.toList _faaAggregates,
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
      API.TableName ->
      NE.NonEmpty (AnnotatedOrderByItemG 'DataConnector (UnpreparedValue 'DataConnector)) ->
      CPS.WriterT TableRelationships m API.OrderBy
    translateOrderBy sourceTableName orderByItems = do
      orderByElementsAndRelations <- for orderByItems \OrderByItemG {..} -> do
        let orderDirection = maybe API.Ascending Witch.from obiType
        translateOrderByElement sourceTableName orderDirection [] obiColumn
      relations <- lift . mergeOrderByRelations $ snd <$> orderByElementsAndRelations
      pure
        API.OrderBy
          { _obRelations = relations,
            _obElements = fst <$> orderByElementsAndRelations
          }

    translateOrderByElement ::
      API.TableName ->
      API.OrderDirection ->
      [API.RelationshipName] ->
      AnnotatedOrderByElement 'DataConnector (UnpreparedValue 'DataConnector) ->
      CPS.WriterT TableRelationships m (API.OrderByElement, HashMap API.RelationshipName API.OrderByRelation)
    translateOrderByElement sourceTableName orderDirection targetReversePath = \case
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
        (translatedOrderByElement, subOrderByRelations) <- translateOrderByElement _rTargetTable orderDirection (relationshipName : targetReversePath) orderByElement

        targetTableWhereExp <- translateBoolExpToExpression _rTargetTable filterExp
        let orderByRelations = HashMap.fromList [(relationshipName, API.OrderByRelation targetTableWhereExp subOrderByRelations)]

        pure (translatedOrderByElement, orderByRelations)
      AOCArrayAggregation relationshipInfo filterExp aggregateOrderByElement -> do
        (relationshipName, API.Relationship {..}) <- recordTableRelationshipFromRelInfo sourceTableName relationshipInfo
        orderByTarget <- case aggregateOrderByElement of
          AAOCount ->
            pure API.OrderByStarCountAggregate
          AAOOp aggFunctionTxt ColumnInfo {..} -> do
            aggFunction <- lift $ translateSingleColumnAggregateFunction aggFunctionTxt
            pure . API.OrderBySingleColumnAggregate $ API.SingleColumnAggregate aggFunction $ Witch.from ciColumn

        let translatedOrderByElement =
              API.OrderByElement
                { _obeTargetPath = reverse (relationshipName : targetReversePath),
                  _obeTarget = orderByTarget,
                  _obeOrderDirection = orderDirection
                }

        targetTableWhereExp <- translateBoolExpToExpression _rTargetTable filterExp
        let orderByRelations = HashMap.fromList [(relationshipName, API.OrderByRelation targetTableWhereExp mempty)]
        pure (translatedOrderByElement, orderByRelations)

    mergeOrderByRelations ::
      Foldable f =>
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

    recordTableRelationshipFromRelInfo ::
      API.TableName ->
      RelInfo 'DataConnector ->
      CPS.WriterT TableRelationships m (API.RelationshipName, API.Relationship)
    recordTableRelationshipFromRelInfo sourceTableName RelInfo {..} = do
      let relationshipName = mkRelationshipName riName
      let relationshipType = case riType of
            ObjRel -> API.ObjectRelationship
            ArrRel -> API.ArrayRelationship
      let relationship =
            API.Relationship
              { _rTargetTable = Witch.from riRTable,
                _rRelationshipType = relationshipType,
                _rColumnMapping = HashMap.fromList $ bimap Witch.from Witch.from <$> HashMap.toList riMapping
              }
      recordTableRelationship
        sourceTableName
        relationshipName
        relationship
      pure (relationshipName, relationship)

    translateAnnFields ::
      FieldPrefix ->
      API.TableName ->
      AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT TableRelationships m FieldsAndAggregates
    translateAnnFields fieldNamePrefix sourceTableName fields = do
      translatedFields <- traverse (traverse (translateAnnField sourceTableName)) fields
      let translatedFields' = HashMap.fromList . catMaybes $ (\(fieldName, field) -> (applyPrefix fieldNamePrefix fieldName,) <$> field) <$> translatedFields
      pure $
        FieldsAndAggregates
          translatedFields'
          mempty

    translateAnnField ::
      API.TableName ->
      AnnFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT TableRelationships m (Maybe API.Field)
    translateAnnField sourceTableName = \case
      AFColumn colField ->
        -- TODO: make sure certain fields in colField are not in use, since we don't
        -- support them
        pure . Just . API.ColumnField . Witch.from $ _acfColumn colField
      AFObjectRelation objRel -> do
        let targetTable = Witch.from $ _aosTableFrom (_aarAnnSelect objRel)
        let relationshipName = mkRelationshipName $ _aarRelationshipName objRel
        FieldsAndAggregates {..} <- translateAnnFields noPrefix targetTable (_aosFields (_aarAnnSelect objRel))
        whereClause <- translateBoolExpToExpression targetTable (_aosTableFilter (_aarAnnSelect objRel))

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
                { _qFields = memptyToNothing . KM.fromList $ (first (K.fromText . getFieldNameTxt)) <$> HashMap.toList _faaFields,
                  _qAggregates = memptyToNothing . KM.fromList $ (first (K.fromText . getFieldNameTxt)) <$> HashMap.toList _faaAggregates,
                  _qWhere = whereClause,
                  _qLimit = Nothing,
                  _qOffset = Nothing,
                  _qOrderBy = Nothing
                }
            )
      AFArrayRelation (ASSimple arrayRelationSelect) -> do
        Just <$> translateArrayRelationSelect sourceTableName (translateAnnFields noPrefix) arrayRelationSelect
      AFArrayRelation (ASAggregate arrayRelationSelect) ->
        Just <$> translateArrayRelationSelect sourceTableName translateTableAggregateFields arrayRelationSelect
      AFExpression _literal ->
        -- We ignore literal text fields (we don't send them to the data connector agent)
        -- and add them back to the response JSON when we reshape what the agent returns
        -- to us
        pure Nothing

    translateArrayRelationSelect ::
      API.TableName ->
      (API.TableName -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT TableRelationships m FieldsAndAggregates) ->
      AnnRelationSelectG 'DataConnector (AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector)) ->
      CPS.WriterT TableRelationships m API.Field
    translateArrayRelationSelect sourceTableName translateFieldsAndAggregates arrRel = do
      targetTable <- lift $ extractTableName (_aarAnnSelect arrRel)
      query <- translateAnnSelect translateFieldsAndAggregates targetTable (_aarAnnSelect arrRel)
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
      API.TableName ->
      TableAggregateFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT TableRelationships m FieldsAndAggregates
    translateTableAggregateFields sourceTableName fields = do
      mconcat <$> traverse (uncurry (translateTableAggregateField sourceTableName)) fields

    translateTableAggregateField ::
      API.TableName ->
      FieldName ->
      TableAggregateFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT TableRelationships m FieldsAndAggregates
    translateTableAggregateField sourceTableName fieldName = \case
      TAFAgg aggregateFields -> do
        let fieldNamePrefix = prefixWith fieldName
        translatedAggregateFields <- lift $ mconcat <$> traverse (uncurry (translateAggregateField fieldNamePrefix)) aggregateFields
        pure $
          FieldsAndAggregates
            mempty
            translatedAggregateFields
      TAFNodes _ fields ->
        translateAnnFields (prefixWith fieldName) sourceTableName fields
      TAFExp _txt ->
        -- We ignore literal text fields (we don't send them to the data connector agent)
        -- and add them back to the response JSON when we reshape what the agent returns
        -- to us
        pure mempty

    translateAggregateField ::
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
            CFCol column _columnType ->
              pure . Just $ (applyPrefix fieldPrefix' columnFieldName, API.SingleColumn . API.SingleColumnAggregate aggFunction $ Witch.from column)
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

    translateSingleColumnAggregateFunction :: Text -> m API.SingleColumnAggregateFunction
    translateSingleColumnAggregateFunction = \case
      "avg" -> pure API.Average
      "max" -> pure API.Max
      "min" -> pure API.Min
      "stddev_pop" -> pure API.StandardDeviationPopulation
      "stddev_samp" -> pure API.StandardDeviationSample
      "stddev" -> pure API.StandardDeviationSample
      "sum" -> pure API.Sum
      "var_pop" -> pure API.VariancePopulation
      "var_samp" -> pure API.VarianceSample
      "variance" -> pure API.VarianceSample
      unknownFunc -> throw500 $ "translateSingleColumnAggregateFunction: Unknown aggregate function encountered: " <> unknownFunc

    prepareLiterals ::
      UnpreparedValue 'DataConnector ->
      m Literal
    prepareLiterals (UVLiteral literal) = pure $ literal
    prepareLiterals (UVParameter _ e) = pure (ValueLiteral (cvValue e))
    prepareLiterals UVSession = throw400 NotSupported "prepareLiterals: UVSession"
    prepareLiterals (UVSessionVar sessionVarType sessionVar) = do
      textValue <-
        getSessionVariableValue sessionVar session
          `onNothing` throw400 NotSupported ("prepareLiterals: session var not found: " <>> sessionVar)
      parseSessionVariable sessionVar sessionVarType textValue

    parseSessionVariable :: SessionVariable -> SessionVarType 'DataConnector -> Text -> m Literal
    parseSessionVariable varName varType varValue = do
      case varType of
        CollectableTypeScalar scalarType ->
          case scalarType of
            StringTy -> pure . ValueLiteral $ J.String varValue
            NumberTy -> parseValue (ValueLiteral . J.Number) "number value"
            BoolTy -> parseValue (ValueLiteral . J.Bool) "boolean value"
            CustomTy customTypeName -> parseValue ValueLiteral (customTypeName <> " JSON value")
        CollectableTypeArray scalarType ->
          case scalarType of
            StringTy -> parseValue (ArrayLiteral . fmap J.String) "JSON array of strings"
            NumberTy -> parseValue (ArrayLiteral . fmap J.Number) "JSON array of numbers"
            BoolTy -> parseValue (ArrayLiteral . fmap J.Bool) "JSON array of booleans"
            CustomTy customTypeName -> parseValue ArrayLiteral ("JSON array of " <> customTypeName <> " JSON values")
      where
        parseValue :: J.FromJSON a => (a -> Literal) -> Text -> m Literal
        parseValue toLiteral description =
          toLiteral <$> J.eitherDecodeStrict' valValueBS
            `onLeft` (\err -> throw400 ParseFailed ("Expected " <> description <> " for session variable " <> varName <<> ". " <> T.pack err))

        valValueBS :: BS.ByteString
        valValueBS = TE.encodeUtf8 varValue

    translateBoolExpToExpression ::
      API.TableName ->
      AnnBoolExp 'DataConnector (UnpreparedValue 'DataConnector) ->
      CPS.WriterT TableRelationships m (Maybe API.Expression)
    translateBoolExpToExpression sourceTableName boolExp = do
      removeAlwaysTrueExpression <$> translateBoolExp sourceTableName boolExp

    translateBoolExp ::
      API.TableName ->
      AnnBoolExp 'DataConnector (UnpreparedValue 'DataConnector) ->
      CPS.WriterT TableRelationships m API.Expression
    translateBoolExp sourceTableName = \case
      BoolAnd xs ->
        mkIfZeroOrMany API.And . mapMaybe removeAlwaysTrueExpression <$> traverse (translateBoolExp sourceTableName) xs
      BoolOr xs ->
        mkIfZeroOrMany API.Or . mapMaybe removeAlwaysFalseExpression <$> traverse (translateBoolExp sourceTableName) xs
      BoolNot x ->
        API.Not <$> (translateBoolExp sourceTableName) x
      BoolField (AVColumn c xs) ->
        lift $ mkIfZeroOrMany API.And <$> traverse (translateOp (Witch.from $ ciColumn c)) xs
      BoolField (AVRelationship relationshipInfo boolExp) -> do
        (relationshipName, API.Relationship {..}) <- recordTableRelationshipFromRelInfo sourceTableName relationshipInfo
        API.Exists (API.RelatedTable relationshipName) <$> translateBoolExp _rTargetTable boolExp
      BoolExists GExists {..} ->
        let tableName = Witch.from _geTable
         in API.Exists (API.UnrelatedTable tableName) <$> translateBoolExp tableName _geWhere
      where
        -- Makes an 'API.Expression' like 'API.And' if there is zero or many input expressions otherwise
        -- just returns the singleton expression. This helps remove redundant 'API.And' etcs from the expression.
        mkIfZeroOrMany :: ([API.Expression] -> API.Expression) -> [API.Expression] -> API.Expression
        mkIfZeroOrMany mk = \case
          [singleExp] -> singleExp
          zeroOrManyExps -> mk zeroOrManyExps

    removeAlwaysTrueExpression :: API.Expression -> Maybe API.Expression
    removeAlwaysTrueExpression = \case
      API.And [] -> Nothing
      API.Not (API.Or []) -> Nothing
      other -> Just other

    removeAlwaysFalseExpression :: API.Expression -> Maybe API.Expression
    removeAlwaysFalseExpression = \case
      API.Or [] -> Nothing
      API.Not (API.And []) -> Nothing
      other -> Just other

    translateOp ::
      API.ColumnName ->
      OpExpG 'DataConnector (UnpreparedValue 'DataConnector) ->
      m API.Expression
    translateOp columnName opExp = do
      preparedOpExp <- traverse prepareLiterals $ opExp
      case preparedOpExp of
        AEQ _ (ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar API.Equal value
        AEQ _ (ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for AEQ operator"
        ANE _ (ValueLiteral value) ->
          pure . API.Not $ mkApplyBinaryComparisonOperatorToScalar API.Equal value
        ANE _ (ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for ANE operator"
        AGT (ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar API.GreaterThan value
        AGT (ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for AGT operator"
        ALT (ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar API.LessThan value
        ALT (ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for ALT operator"
        AGTE (ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar API.GreaterThanOrEqual value
        AGTE (ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for AGTE operator"
        ALTE (ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar API.LessThanOrEqual value
        ALTE (ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for ALTE operator"
        ANISNULL ->
          pure $ API.ApplyUnaryComparisonOperator API.IsNull currentComparisonColumn
        ANISNOTNULL ->
          pure $ API.Not (API.ApplyUnaryComparisonOperator API.IsNull currentComparisonColumn)
        AIN literal -> pure $ inOperator literal
        ANIN literal -> pure . API.Not $ inOperator literal
        CEQ rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.Equal rootOrCurrentColumn
        CNE rootOrCurrentColumn ->
          pure $ API.Not $ mkApplyBinaryComparisonOperatorToAnotherColumn API.Equal rootOrCurrentColumn
        CGT rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.GreaterThan rootOrCurrentColumn
        CLT rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.LessThan rootOrCurrentColumn
        CGTE rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.GreaterThanOrEqual rootOrCurrentColumn
        CLTE rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.LessThanOrEqual rootOrCurrentColumn
        ALIKE _literal ->
          throw400 NotSupported "The ALIKE operator is not supported by the Data Connector backend"
        ANLIKE _literal ->
          throw400 NotSupported "The ANLIKE operator is not supported by the Data Connector backend"
        ACast _literal ->
          throw400 NotSupported "The ACast operator is not supported by the Data Connector backend"
        ABackendSpecific CustomBooleanOperator {..} -> case _cboRHS of
          Nothing -> pure $ API.ApplyUnaryComparisonOperator (API.CustomUnaryComparisonOperator _cboName) currentComparisonColumn
          Just (Left rootOrCurrentColumn) ->
            pure $ mkApplyBinaryComparisonOperatorToAnotherColumn (API.CustomBinaryComparisonOperator _cboName) rootOrCurrentColumn
          Just (Right (ValueLiteral value)) ->
            pure $ mkApplyBinaryComparisonOperatorToScalar (API.CustomBinaryComparisonOperator _cboName) value
          Just (Right (ArrayLiteral array)) ->
            pure $ API.ApplyBinaryArrayComparisonOperator (API.CustomBinaryArrayComparisonOperator _cboName) currentComparisonColumn array
      where
        currentComparisonColumn :: API.ComparisonColumn
        currentComparisonColumn = API.ComparisonColumn API.CurrentTable columnName

        mkApplyBinaryComparisonOperatorToAnotherColumn :: API.BinaryComparisonOperator -> RootOrCurrentColumn 'DataConnector -> API.Expression
        mkApplyBinaryComparisonOperatorToAnotherColumn operator (RootOrCurrentColumn rootOrCurrent otherColumnName) =
          let columnPath = case rootOrCurrent of
                IsRoot -> API.QueryTable
                IsCurrent -> API.CurrentTable
           in API.ApplyBinaryComparisonOperator operator currentComparisonColumn (API.AnotherColumn . API.ComparisonColumn columnPath $ Witch.from otherColumnName)

        inOperator :: Literal -> API.Expression
        inOperator literal =
          let values = case literal of
                ArrayLiteral array -> array
                ValueLiteral value -> [value]
           in API.ApplyBinaryArrayComparisonOperator API.In currentComparisonColumn values

        mkApplyBinaryComparisonOperatorToScalar :: API.BinaryComparisonOperator -> J.Value -> API.Expression
        mkApplyBinaryComparisonOperatorToScalar operator value =
          API.ApplyBinaryComparisonOperator operator currentComparisonColumn (API.ScalarValue value)

-- | Validate if a 'API.QueryRequest' contains any relationships.
queryHasRelations :: API.QueryRequest -> Bool
queryHasRelations API.QueryRequest {..} = _qrTableRelationships /= mempty

data Cardinality
  = Single
  | Many

reshapeResponseToQueryShape ::
  MonadError QErr m =>
  QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
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
  AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
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
  TableAggregateFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
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
  KeyMap J.Value ->
  m J.Encoding
reshapeAggregateFields fieldPrefix aggregateFields responseAggregates = do
  reshapedFields <- forM aggregateFields $ \(fieldName@(FieldName fieldNameText), aggregateField) ->
    case aggregateField of
      AFCount _countAggregate -> do
        let fieldNameKey = K.fromText . getFieldNameTxt $ applyPrefix fieldPrefix fieldName
        responseAggregateValue <-
          KM.lookup fieldNameKey responseAggregates
            `onNothing` throw500 ("Unable to find expected aggregate " <> K.toText fieldNameKey <> " in aggregates returned by Data Connector agent") -- TODO(dchambers): Add pathing information for error clarity
        pure (fieldNameText, J.toEncoding responseAggregateValue)
      AFOp AggregateOp {..} -> do
        reshapedColumnFields <- forM _aoFields $ \(columnFieldName@(FieldName columnFieldNameText), columnField) ->
          case columnField of
            CFCol _column _columnType -> do
              let fieldPrefix' = fieldPrefix <> prefixWith fieldName
              let columnFieldNameKey = K.fromText . getFieldNameTxt $ applyPrefix fieldPrefix' columnFieldName
              responseAggregateValue <-
                KM.lookup columnFieldNameKey responseAggregates
                  `onNothing` throw500 ("Unable to find expected aggregate " <> K.toText columnFieldNameKey <> " in aggregates returned by Data Connector agent") -- TODO(dchambers): Add pathing information for error clarity
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
  AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  KeyMap API.FieldValue ->
  m J.Encoding
reshapeAnnFields fieldNamePrefix fields responseRow = do
  reshapedFields <- forM fields $ \(fieldName@(FieldName fieldNameText), field) -> do
    let fieldNameKey = K.fromText . getFieldNameTxt $ applyPrefix fieldNamePrefix fieldName
    let responseField =
          KM.lookup fieldNameKey responseRow
            `onNothing` throw500 ("Unable to find expected field " <> K.toText fieldNameKey <> " in row returned by Data Connector agent") -- TODO(dchambers): Add pathing information for error clarity
    reshapedField <- reshapeField field responseField
    pure (fieldNameText, reshapedField)

  pure $ encodeAssocListAsObject reshapedFields

encodeAssocListAsObject :: [(Text, J.Encoding)] -> J.Encoding
encodeAssocListAsObject =
  JE.dict
    JE.text
    id
    (\fn -> foldr (uncurry fn))

reshapeField ::
  MonadError QErr m =>
  AnnFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
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
  (Fields (fieldType (UnpreparedValue 'DataConnector)) -> API.QueryResponse -> m J.Encoding) ->
  AnnRelationSelectG 'DataConnector (AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector)) ->
  API.FieldValue ->
  m J.Encoding
reshapeAnnRelationSelect reshapeFields annRelationSelect fieldValue =
  case API.deserializeAsRelationshipFieldValue fieldValue of
    Left err -> throw500 $ "Found column field value where relationship field value was expected in field returned by Data Connector agent: " <> err -- TODO(dchambers): Add pathing information for error clarity
    Right subqueryResponse ->
      let annSimpleSelect = _aarAnnSelect annRelationSelect
       in reshapeFields (_asnFields annSimpleSelect) subqueryResponse

memptyToNothing :: (Monoid m, Eq m) => m -> Maybe m
memptyToNothing m = if m == mempty then Nothing else Just m

mkRelationshipName :: RelName -> API.RelationshipName
mkRelationshipName relName = API.RelationshipName $ toTxt relName
