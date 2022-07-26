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
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Min (..))
import Data.Text.Encoding qualified as TE
import Data.Text.Extended ((<>>))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Backend
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.DataConnector.IR.Aggregate qualified as IR.A
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Expression (UnaryComparisonOperator (CustomUnaryComparisonOperator))
import Hasura.Backends.DataConnector.IR.Expression qualified as IR.E
import Hasura.Backends.DataConnector.IR.OrderBy qualified as IR.O
import Hasura.Backends.DataConnector.IR.Query qualified as IR.Q
import Hasura.Backends.DataConnector.IR.Relationships qualified as IR.R
import Hasura.Backends.DataConnector.IR.Scalar.Value qualified as IR.S
import Hasura.Backends.DataConnector.IR.Table qualified as IR.T
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.Local (RelInfo (..))
import Hasura.SQL.Backend
import Hasura.Session
import Witch qualified

--------------------------------------------------------------------------------

data QueryPlan = QueryPlan
  { _qpRequest :: IR.Q.QueryRequest,
    _qpResponseReshaper :: forall m. (MonadError QErr m) => API.QueryResponse -> m J.Encoding
  }

data FieldsAndAggregates = FieldsAndAggregates
  { _faaFields :: HashMap FieldName IR.Q.Field,
    _faaAggregates :: HashMap FieldName IR.A.Aggregate
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

noPrefix :: FieldPrefix
noPrefix = FieldPrefix Nothing

prefixWith :: FieldName -> FieldPrefix
prefixWith = FieldPrefix . Just

applyPrefix :: FieldPrefix -> FieldName -> FieldName
applyPrefix (FieldPrefix fieldNamePrefix) fieldName = maybe fieldName (\prefix -> prefix <> "_" <> fieldName) fieldNamePrefix

-- | Extract the 'IR.Q' from a 'Plan' and render it as 'Text'.
--
-- NOTE: This is for logging and debug purposes only.
renderQuery :: IR.Q.QueryRequest -> Text
renderQuery =
  TE.decodeUtf8 . BL.toStrict . J.encode . Witch.into @API.QueryRequest

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
      m IR.Q.QueryRequest
    translateQueryDB =
      \case
        QDBMultipleRows annSelect -> translateAnnSelectToQueryRequest (translateAnnFields noPrefix) annSelect
        QDBSingleRow annSelect -> translateAnnSelectToQueryRequest (translateAnnFields noPrefix) annSelect
        QDBAggregation annSelect -> translateAnnSelectToQueryRequest translateTableAggregateFields annSelect

    translateAnnSelectToQueryRequest ::
      (IR.T.Name -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT IR.R.TableRelationships m FieldsAndAggregates) ->
      AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector) ->
      m IR.Q.QueryRequest
    translateAnnSelectToQueryRequest translateFieldsAndAggregates selectG = do
      tableName <- extractTableName selectG
      (query, tableRelationships) <- CPS.runWriterT (translateAnnSelect translateFieldsAndAggregates tableName selectG)
      pure $
        IR.Q.QueryRequest
          { _qrTable = tableName,
            _qrTableRelationships = tableRelationships,
            _qrQuery = query
          }

    extractTableName :: AnnSelectG 'DataConnector fieldsType valueType -> m IR.T.Name
    extractTableName selectG =
      case _asnFrom selectG of
        FromTable tn -> pure tn
        FromIdentifier _ -> throw400 NotSupported "AnnSelectG: FromIdentifier not supported"
        FromFunction {} -> throw400 NotSupported "AnnSelectG: FromFunction not supported"

    recordTableRelationship :: IR.T.Name -> IR.R.RelationshipName -> IR.R.Relationship -> CPS.WriterT IR.R.TableRelationships m ()
    recordTableRelationship sourceTableName relationshipName relationship =
      CPS.tell . IR.R.TableRelationships $ HashMap.singleton sourceTableName (HashMap.singleton relationshipName relationship)

    translateAnnSelect ::
      (IR.T.Name -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT IR.R.TableRelationships m FieldsAndAggregates) ->
      IR.T.Name ->
      AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector) ->
      CPS.WriterT IR.R.TableRelationships m IR.Q.Query
    translateAnnSelect translateFieldsAndAggregates tableName selectG = do
      FieldsAndAggregates {..} <- translateFieldsAndAggregates tableName (_asnFields selectG)
      let whereClauseWithPermissions =
            case _saWhere (_asnArgs selectG) of
              Just expr -> BoolAnd [expr, _tpFilter (_asnPerm selectG)]
              Nothing -> _tpFilter (_asnPerm selectG)
      whereClause <- translateBoolExp [] tableName whereClauseWithPermissions
      orderBy <- lift $ translateOrderBy (_saOrderBy $ _asnArgs selectG)
      pure
        IR.Q.Query
          { _qFields = _faaFields,
            _qAggregates = _faaAggregates,
            _qLimit =
              fmap getMin $
                foldMap
                  (fmap Min)
                  [ _saLimit (_asnArgs selectG),
                    _tpLimit (_asnPerm selectG)
                  ],
            _qOffset = fmap fromIntegral (_saOffset (_asnArgs selectG)),
            _qWhere = Just whereClause,
            _qOrderBy = orderBy
          }

    translateOrderBy ::
      Maybe (NE.NonEmpty (AnnotatedOrderByItemG 'DataConnector (UnpreparedValue 'DataConnector))) ->
      m [IR.O.OrderBy]
    translateOrderBy = \case
      Nothing -> pure []
      Just orderBys ->
        do
          NE.toList
          <$> for orderBys \OrderByItemG {..} -> case obiColumn of
            AOCColumn (ColumnInfo {ciColumn = dynColumnName}) ->
              pure
                IR.O.OrderBy
                  { column = dynColumnName,
                    -- NOTE: Picking a default ordering.
                    ordering = fromMaybe IR.O.Ascending obiType
                  }
            AOCObjectRelation {} ->
              throw400 NotSupported "translateOrderBy: AOCObjectRelation unsupported in the Data Connector backend"
            AOCArrayAggregation {} ->
              throw400 NotSupported "translateOrderBy: AOCArrayAggregation unsupported in the Data Connector backend"

    translateAnnFields ::
      FieldPrefix ->
      IR.T.Name ->
      AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT IR.R.TableRelationships m FieldsAndAggregates
    translateAnnFields fieldNamePrefix sourceTableName fields = do
      translatedFields <- traverse (traverse (translateAnnField sourceTableName)) fields
      let translatedFields' = HashMap.fromList . catMaybes $ (\(fieldName, field) -> (applyPrefix fieldNamePrefix fieldName,) <$> field) <$> translatedFields
      pure $
        FieldsAndAggregates
          translatedFields'
          mempty

    translateAnnField ::
      IR.T.Name ->
      AnnFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT IR.R.TableRelationships m (Maybe IR.Q.Field)
    translateAnnField sourceTableName = \case
      AFColumn colField ->
        -- TODO: make sure certain fields in colField are not in use, since we don't
        -- support them
        pure . Just . IR.Q.ColumnField $ _acfColumn colField
      AFObjectRelation objRel -> do
        let targetTable = _aosTableFrom (_aarAnnSelect objRel)
        let relationshipName = IR.R.mkRelationshipName $ _aarRelationshipName objRel
        FieldsAndAggregates {..} <- translateAnnFields noPrefix targetTable (_aosFields (_aarAnnSelect objRel))
        whereClause <- translateBoolExp [] targetTable (_aosTableFilter (_aarAnnSelect objRel))

        recordTableRelationship
          sourceTableName
          relationshipName
          IR.R.Relationship
            { _rTargetTable = targetTable,
              _rRelationshipType = IR.R.ObjectRelationship,
              _rColumnMapping = _aarColumnMapping objRel
            }

        pure . Just . IR.Q.RelField $
          IR.Q.RelationshipField
            relationshipName
            ( IR.Q.Query
                { _qFields = _faaFields,
                  _qAggregates = _faaAggregates,
                  _qWhere = Just whereClause,
                  _qLimit = Nothing,
                  _qOffset = Nothing,
                  _qOrderBy = []
                }
            )
      AFArrayRelation (ASSimple arrayRelationSelect) -> do
        Just <$> translateArrayRelationSelect sourceTableName (translateAnnFields noPrefix) arrayRelationSelect
      AFArrayRelation (ASAggregate arrayRelationSelect) ->
        Just <$> translateArrayRelationSelect sourceTableName translateTableAggregateFields arrayRelationSelect
      AFExpression _literal ->
        pure Nothing

    translateArrayRelationSelect ::
      IR.T.Name ->
      (IR.T.Name -> Fields (fieldType (UnpreparedValue 'DataConnector)) -> CPS.WriterT IR.R.TableRelationships m FieldsAndAggregates) ->
      AnnRelationSelectG 'DataConnector (AnnSelectG 'DataConnector fieldType (UnpreparedValue 'DataConnector)) ->
      CPS.WriterT IR.R.TableRelationships m IR.Q.Field
    translateArrayRelationSelect sourceTableName translateFieldsAndAggregates arrRel = do
      targetTable <- lift $ extractTableName (_aarAnnSelect arrRel)
      query <- translateAnnSelect translateFieldsAndAggregates targetTable (_aarAnnSelect arrRel)
      let relationshipName = IR.R.mkRelationshipName $ _aarRelationshipName arrRel

      recordTableRelationship
        sourceTableName
        relationshipName
        IR.R.Relationship
          { _rTargetTable = targetTable,
            _rRelationshipType = IR.R.ArrayRelationship,
            _rColumnMapping = _aarColumnMapping arrRel
          }

      pure . IR.Q.RelField $
        IR.Q.RelationshipField
          relationshipName
          query

    translateTableAggregateFields ::
      IR.T.Name ->
      TableAggregateFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT IR.R.TableRelationships m FieldsAndAggregates
    translateTableAggregateFields sourceTableName fields = do
      mconcat <$> traverse (uncurry (translateTableAggregateField sourceTableName)) fields

    translateTableAggregateField ::
      IR.T.Name ->
      FieldName ->
      TableAggregateFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT IR.R.TableRelationships m FieldsAndAggregates
    translateTableAggregateField sourceTableName fieldName = \case
      TAFAgg _aggregateFields ->
        pure mempty --TODO(dchambers) implement this
      TAFNodes _ fields ->
        translateAnnFields (prefixWith fieldName) sourceTableName fields
      TAFExp _txt ->
        -- We ignore literal text fields (we don't send them to the data connector agent)
        -- and add them back to the response JSON when we reshape what the agent returns
        -- to us
        pure mempty

    prepareLiterals ::
      UnpreparedValue 'DataConnector ->
      m IR.S.Literal
    prepareLiterals (UVLiteral literal) = pure $ literal
    prepareLiterals (UVParameter _ e) = pure (IR.S.ValueLiteral (cvValue e))
    prepareLiterals UVSession = throw400 NotSupported "prepareLiterals: UVSession"
    prepareLiterals (UVSessionVar _ v) =
      case getSessionVariableValue v session of
        Nothing -> throw400 NotSupported ("prepareLiterals: session var not found: " <>> v)
        Just s -> pure (IR.S.ValueLiteral (IR.S.String s))

    translateBoolExp ::
      [IR.R.RelationshipName] ->
      IR.T.Name ->
      AnnBoolExp 'DataConnector (UnpreparedValue 'DataConnector) ->
      CPS.WriterT IR.R.TableRelationships m IR.E.Expression
    translateBoolExp columnRelationshipReversePath sourceTableName = \case
      BoolAnd xs ->
        mkIfZeroOrMany IR.E.And <$> traverse (translateBoolExp columnRelationshipReversePath sourceTableName) xs
      BoolOr xs ->
        mkIfZeroOrMany IR.E.Or <$> traverse (translateBoolExp columnRelationshipReversePath sourceTableName) xs
      BoolNot x ->
        IR.E.Not <$> (translateBoolExp columnRelationshipReversePath sourceTableName) x
      BoolField (AVColumn c xs) ->
        lift $ mkIfZeroOrMany IR.E.And <$> traverse (translateOp columnRelationshipReversePath (ciColumn c)) xs
      BoolField (AVRelationship relationshipInfo boolExp) -> do
        let relationshipName = IR.R.mkRelationshipName $ riName relationshipInfo
        let targetTable = riRTable relationshipInfo
        let relationshipType = case riType relationshipInfo of
              ObjRel -> IR.R.ObjectRelationship
              ArrRel -> IR.R.ArrayRelationship
        recordTableRelationship
          sourceTableName
          relationshipName
          IR.R.Relationship
            { _rTargetTable = targetTable,
              _rRelationshipType = relationshipType,
              _rColumnMapping = riMapping relationshipInfo
            }
        translateBoolExp (relationshipName : columnRelationshipReversePath) targetTable boolExp
      BoolExists _ ->
        lift $ throw400 NotSupported "The BoolExists expression type is not supported by the Data Connector backend"
      where
        -- Makes an 'IR.E.Expression' like 'IR.E.And' if there is zero or many input expressions otherwise
        -- just returns the singleton expression. This helps remove redundant 'IE.E.And' etcs from the expression.
        mkIfZeroOrMany :: ([IR.E.Expression] -> IR.E.Expression) -> [IR.E.Expression] -> IR.E.Expression
        mkIfZeroOrMany mk = \case
          [singleExp] -> singleExp
          zeroOrManyExps -> mk zeroOrManyExps

    translateOp ::
      [IR.R.RelationshipName] ->
      IR.C.Name ->
      OpExpG 'DataConnector (UnpreparedValue 'DataConnector) ->
      m IR.E.Expression
    translateOp columnRelationshipReversePath columnName opExp = do
      preparedOpExp <- traverse prepareLiterals $ opExp
      case preparedOpExp of
        AEQ _ (IR.S.ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar IR.E.Equal value
        AEQ _ (IR.S.ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for AEQ operator"
        ANE _ (IR.S.ValueLiteral value) ->
          pure . IR.E.Not $ mkApplyBinaryComparisonOperatorToScalar IR.E.Equal value
        ANE _ (IR.S.ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for ANE operator"
        AGT (IR.S.ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar IR.E.GreaterThan value
        AGT (IR.S.ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for AGT operator"
        ALT (IR.S.ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar IR.E.LessThan value
        ALT (IR.S.ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for ALT operator"
        AGTE (IR.S.ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar IR.E.GreaterThanOrEqual value
        AGTE (IR.S.ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for AGTE operator"
        ALTE (IR.S.ValueLiteral value) ->
          pure $ mkApplyBinaryComparisonOperatorToScalar IR.E.LessThanOrEqual value
        ALTE (IR.S.ArrayLiteral _array) ->
          throw400 NotSupported "Array literals not supported for ALTE operator"
        ANISNULL ->
          pure $ IR.E.ApplyUnaryComparisonOperator IR.E.IsNull currentComparisonColumn
        ANISNOTNULL ->
          pure $ IR.E.Not (IR.E.ApplyUnaryComparisonOperator IR.E.IsNull currentComparisonColumn)
        AIN literal -> pure $ inOperator literal
        ANIN literal -> pure . IR.E.Not $ inOperator literal
        CEQ rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.Equal rootOrCurrentColumn
        CNE rootOrCurrentColumn ->
          pure $ IR.E.Not $ mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.Equal rootOrCurrentColumn
        CGT rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.GreaterThan rootOrCurrentColumn
        CLT rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.LessThan rootOrCurrentColumn
        CGTE rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.GreaterThanOrEqual rootOrCurrentColumn
        CLTE rootOrCurrentColumn ->
          pure $ mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.LessThanOrEqual rootOrCurrentColumn
        ALIKE _literal ->
          throw400 NotSupported "The ALIKE operator is not supported by the Data Connector backend"
        ANLIKE _literal ->
          throw400 NotSupported "The ANLIKE operator is not supported by the Data Connector backend"
        ACast _literal ->
          throw400 NotSupported "The ACast operator is not supported by the Data Connector backend"
        ABackendSpecific CustomBooleanOperator {..} -> case _cboRHS of
          Nothing -> pure $ IR.E.ApplyUnaryComparisonOperator (CustomUnaryComparisonOperator _cboName) currentComparisonColumn
          Just (Left rootOrCurrentColumn) ->
            pure $ mkApplyBinaryComparisonOperatorToAnotherColumn (IR.E.CustomBinaryComparisonOperator _cboName) rootOrCurrentColumn
          Just (Right (IR.S.ValueLiteral value)) ->
            pure $ mkApplyBinaryComparisonOperatorToScalar (IR.E.CustomBinaryComparisonOperator _cboName) value
          Just (Right (IR.S.ArrayLiteral array)) ->
            pure $ IR.E.ApplyBinaryArrayComparisonOperator (IR.E.CustomBinaryArrayComparisonOperator _cboName) currentComparisonColumn array
      where
        currentComparisonColumn :: IR.E.ComparisonColumn
        currentComparisonColumn = IR.E.ComparisonColumn (reverse columnRelationshipReversePath) columnName

        mkApplyBinaryComparisonOperatorToAnotherColumn :: IR.E.BinaryComparisonOperator -> RootOrCurrentColumn 'DataConnector -> IR.E.Expression
        mkApplyBinaryComparisonOperatorToAnotherColumn operator (RootOrCurrentColumn rootOrCurrent otherColumnName) =
          let columnPath = case rootOrCurrent of
                IsRoot -> []
                IsCurrent -> (reverse columnRelationshipReversePath)
           in IR.E.ApplyBinaryComparisonOperator operator currentComparisonColumn (IR.E.AnotherColumn $ IR.E.ComparisonColumn columnPath otherColumnName)

        inOperator :: IR.S.Literal -> IR.E.Expression
        inOperator literal =
          let values = case literal of
                IR.S.ArrayLiteral array -> array
                IR.S.ValueLiteral value -> [value]
           in IR.E.ApplyBinaryArrayComparisonOperator IR.E.In currentComparisonColumn values

        mkApplyBinaryComparisonOperatorToScalar :: IR.E.BinaryComparisonOperator -> IR.S.Value -> IR.E.Expression
        mkApplyBinaryComparisonOperatorToScalar operator value =
          IR.E.ApplyBinaryComparisonOperator operator currentComparisonColumn (IR.E.ScalarValue value)

-- | Validate if a 'IR.Q' contains any relationships.
queryHasRelations :: IR.Q.QueryRequest -> Bool
queryHasRelations IR.Q.QueryRequest {..} = _qrTableRelationships /= mempty

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
      TAFAgg _aggregateFields ->
        --TODO(dchambers) implement this
        throw500 "reshapeTableAggregateFields: TAFAgg not supported"
      TAFNodes _ annFields -> do
        reshapedRows <- traverse (reshapeAnnFields (prefixWith fieldName) annFields) rows
        pure $ (fieldNameText, JE.list id reshapedRows)
      TAFExp txt ->
        pure $ (fieldNameText, JE.text txt)
  pure $ encodeAssocListAsObject reshapedFields
  where
    rows = fromMaybe mempty _qrRows

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
