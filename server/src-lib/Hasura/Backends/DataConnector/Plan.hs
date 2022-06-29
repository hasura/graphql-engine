module Hasura.Backends.DataConnector.Plan
  ( SourceConfig (..),
    mkPlan,
    renderQuery,
    queryHasRelations,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Trans.Writer.CPS qualified as CPS
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Min (..))
import Data.Text.Encoding qualified as TE
import Data.Text.Extended ((<>>))
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Export qualified as IR
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

--------------------------------------------------------------------------------

-- | Error type for the postProcessor continuation. Failure can occur if the Agent
-- returns bad data.
data ResponseError
  = RequiredFieldMissing
  | UnexpectedFields
  | ExpectedObject
  | ExpectedArray
  deriving (Show, Eq)

-- | Extract the 'IR.Q' from a 'Plan' and render it as 'Text'.
--
-- NOTE: This is for logging and debug purposes only.
renderQuery :: IR.Q.QueryRequest -> Text
renderQuery =
  TE.decodeUtf8 . BL.toStrict . J.encode . IR.queryRequestToAPI

-- | Map a 'QueryDB 'DataConnector' term into a 'Plan'
mkPlan ::
  forall m.
  MonadError QErr m =>
  SessionVariables ->
  SourceConfig ->
  QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m IR.Q.QueryRequest
mkPlan session (SourceConfig {}) ir = translateQueryDB ir
  where
    translateQueryDB ::
      QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m IR.Q.QueryRequest
    translateQueryDB =
      \case
        QDBMultipleRows s -> translateAnnSelectToQueryRequest s
        QDBSingleRow s -> translateAnnSelectToQueryRequest s
        QDBAggregation {} -> throw400 NotSupported "QDBAggregation: not supported"

    translateAnnSelectToQueryRequest ::
      AnnSimpleSelectG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m IR.Q.QueryRequest
    translateAnnSelectToQueryRequest selectG = do
      tableName <- extractTableName selectG
      (query, tableRelationships) <- CPS.runWriterT (translateAnnSelect tableName selectG)
      pure $
        IR.Q.QueryRequest
          { _qrTable = tableName,
            _qrTableRelationships = tableRelationships,
            _qrQuery = query
          }

    extractTableName :: AnnSimpleSelectG 'DataConnector Void v -> m IR.T.Name
    extractTableName selectG =
      case _asnFrom selectG of
        FromTable tn -> pure tn
        FromIdentifier _ -> throw400 NotSupported "AnnSelectG: FromIdentifier not supported"
        FromFunction {} -> throw400 NotSupported "AnnSelectG: FromFunction not supported"

    recordTableRelationship :: IR.T.Name -> IR.R.RelationshipName -> IR.R.Relationship -> CPS.WriterT IR.R.TableRelationships m ()
    recordTableRelationship sourceTableName relationshipName relationship =
      CPS.tell . IR.R.TableRelationships $ HashMap.singleton sourceTableName (HashMap.singleton relationshipName relationship)

    translateAnnSelect ::
      IR.T.Name ->
      AnnSimpleSelectG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT IR.R.TableRelationships m IR.Q.Query
    translateAnnSelect tableName selectG = do
      fields <- translateFields tableName (_asnFields selectG)
      let whereClauseWithPermissions =
            case _saWhere (_asnArgs selectG) of
              Just expr -> BoolAnd [expr, _tpFilter (_asnPerm selectG)]
              Nothing -> _tpFilter (_asnPerm selectG)
      whereClause <- translateBoolExp [] tableName whereClauseWithPermissions
      orderBy <- lift $ translateOrderBy (_saOrderBy $ _asnArgs selectG)
      pure
        IR.Q.Query
          { _qFields = fields,
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

    translateFields ::
      IR.T.Name ->
      AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT IR.R.TableRelationships m (HashMap Text IR.Q.Field)
    translateFields sourceTableName fields = do
      translatedFields <- traverse (traverse (translateField sourceTableName)) fields
      pure $
        HashMap.fromList $
          mapMaybe
            sequence
            [(getFieldNameTxt f, field) | (f, field) <- translatedFields]

    translateField ::
      IR.T.Name ->
      AnnFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      CPS.WriterT IR.R.TableRelationships m (Maybe IR.Q.Field)
    translateField sourceTableName = \case
      AFColumn colField ->
        -- TODO: make sure certain fields in colField are not in use, since we don't
        -- support them
        pure . Just . IR.Q.ColumnField $ _acfColumn colField
      AFObjectRelation objRel -> do
        let targetTable = _aosTableFrom (_aarAnnSelect objRel)
        let relationshipName = IR.R.mkRelationshipName $ _aarRelationshipName objRel
        fields <- translateFields targetTable (_aosFields (_aarAnnSelect objRel))
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
                { _qFields = fields,
                  _qWhere = Just whereClause,
                  _qLimit = Nothing,
                  _qOffset = Nothing,
                  _qOrderBy = []
                }
            )
      AFArrayRelation (ASSimple arrRel) -> do
        targetTable <- lift $ extractTableName (_aarAnnSelect arrRel)
        query <- translateAnnSelect targetTable (_aarAnnSelect arrRel)
        let relationshipName = IR.R.mkRelationshipName $ _aarRelationshipName arrRel

        recordTableRelationship
          sourceTableName
          relationshipName
          IR.R.Relationship
            { _rTargetTable = targetTable,
              _rRelationshipType = IR.R.ArrayRelationship,
              _rColumnMapping = _aarColumnMapping arrRel
            }

        pure . Just . IR.Q.RelField $
          IR.Q.RelationshipField
            relationshipName
            query
      AFArrayRelation (ASAggregate _) ->
        lift $ throw400 NotSupported "translateField: AFArrayRelation ASAggregate not supported"
      AFExpression _literal ->
        pure Nothing

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
      BoolFld (AVColumn c xs) ->
        lift $ mkIfZeroOrMany IR.E.And <$> traverse (translateOp columnRelationshipReversePath (ciColumn c)) xs
      BoolFld (AVRelationship relationshipInfo boolExp) -> do
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
          mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.Equal rootOrCurrentColumn
        CNE rootOrCurrentColumn ->
          IR.E.Not <$> mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.Equal rootOrCurrentColumn
        CGT rootOrCurrentColumn ->
          mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.GreaterThan rootOrCurrentColumn
        CLT rootOrCurrentColumn ->
          mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.LessThan rootOrCurrentColumn
        CGTE rootOrCurrentColumn ->
          mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.GreaterThanOrEqual rootOrCurrentColumn
        CLTE rootOrCurrentColumn ->
          mkApplyBinaryComparisonOperatorToAnotherColumn IR.E.LessThanOrEqual rootOrCurrentColumn
        ALIKE _literal ->
          throw400 NotSupported "The ALIKE operator is not supported by the Data Connector backend"
        ANLIKE _literal ->
          throw400 NotSupported "The ANLIKE operator is not supported by the Data Connector backend"
        ACast _literal ->
          throw400 NotSupported "The ACast operator is not supported by the Data Connector backend"
      where
        currentComparisonColumn :: IR.E.ComparisonColumn
        currentComparisonColumn = IR.E.ComparisonColumn (reverse columnRelationshipReversePath) columnName

        mkApplyBinaryComparisonOperatorToAnotherColumn :: IR.E.BinaryComparisonOperator -> RootOrCurrentColumn 'DataConnector -> m IR.E.Expression
        mkApplyBinaryComparisonOperatorToAnotherColumn operator (RootOrCurrentColumn rootOrCurrent otherColumnName) = do
          let columnPath = case rootOrCurrent of
                IsRoot -> []
                IsCurrent -> (reverse columnRelationshipReversePath)
          pure $ IR.E.ApplyBinaryComparisonOperator operator currentComparisonColumn (IR.E.AnotherColumn $ IR.E.ComparisonColumn columnPath otherColumnName)

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
