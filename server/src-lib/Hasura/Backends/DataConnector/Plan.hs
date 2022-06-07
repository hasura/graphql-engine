module Hasura.Backends.DataConnector.Plan
  ( SourceConfig (..),
    mkPlan,
    renderQuery,
    queryHasRelations,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Any (..), Min (..))
import Data.Text as T
import Data.Text.Encoding qualified as TE
import Data.Text.Extended ((<>>))
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.Export qualified as IR
import Hasura.Backends.DataConnector.IR.Expression qualified as IR.E
import Hasura.Backends.DataConnector.IR.OrderBy qualified as IR.O
import Hasura.Backends.DataConnector.IR.Query qualified as IR.Q
import Hasura.Backends.DataConnector.IR.Scalar.Value qualified as IR.S
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

--------------------------------------------------------------------------------

-- | Error type for the postProcessor continuation. Failure can occur if the Agent
-- returns bad data.
data ResponseError
  = RequiredFieldMissing
  | UnexpectedFields
  | ExpectedObject
  | ExpectedArray
  | UnexpectedResponseCardinality
  deriving (Show, Eq)

-- | Extract the 'IR.Q' from a 'Plan' and render it as 'Text'.
--
-- NOTE: This is for logging and debug purposes only.
renderQuery :: IR.Q.Query -> Text
renderQuery =
  TE.decodeUtf8 . BL.toStrict . J.encode . IR.queryToAPI

-- | Map a 'QueryDB 'DataConnector' term into a 'Plan'
mkPlan ::
  forall m.
  MonadError QErr m =>
  SessionVariables ->
  SourceConfig ->
  QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m IR.Q.Query
mkPlan session (SourceConfig {}) ir = translateQueryDB ir
  where
    translateQueryDB ::
      QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m IR.Q.Query
    translateQueryDB =
      \case
        QDBMultipleRows s -> translateAnnSelect IR.Q.Many s
        QDBSingleRow s -> translateAnnSelect IR.Q.OneOrZero s
        QDBAggregation {} -> throw400 NotSupported "QDBAggregation: not supported"

    translateAnnSelect ::
      IR.Q.Cardinality ->
      AnnSimpleSelectG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m IR.Q.Query
    translateAnnSelect card selectG = do
      tableName <- case _asnFrom selectG of
        FromTable tn -> pure tn
        FromIdentifier _ -> throw400 NotSupported "AnnSelectG: FromIdentifier not supported"
        FromFunction {} -> throw400 NotSupported "AnnSelectG: FromFunction not supported"
      fields <- translateFields card (_asnFields selectG)
      let whereClauseWithPermissions =
            case _saWhere (_asnArgs selectG) of
              Just expr -> BoolAnd [expr, _tpFilter (_asnPerm selectG)]
              Nothing -> _tpFilter (_asnPerm selectG)
      whereClause <- translateBoolExp whereClauseWithPermissions
      orderBy <- translateOrderBy (_saOrderBy $ _asnArgs selectG)
      pure
        IR.Q.Query
          { from = tableName,
            fields = fields,
            limit =
              fmap getMin $
                foldMap
                  (fmap Min)
                  [ _saLimit (_asnArgs selectG),
                    _tpLimit (_asnPerm selectG)
                  ],
            offset = fmap fromIntegral (_saOffset (_asnArgs selectG)),
            where_ = Just whereClause,
            orderBy = orderBy,
            cardinality = card
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
      IR.Q.Cardinality ->
      AnnFieldsG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m (HashMap Text IR.Q.Field)
    translateFields cardinality fields = do
      translatedFields <- traverse (traverse (translateField cardinality)) fields
      pure $
        HashMap.fromList $
          mapMaybe
            sequence
            [(getFieldNameTxt f, field) | (f, field) <- translatedFields]

    translateField ::
      IR.Q.Cardinality ->
      AnnFieldG 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m (Maybe IR.Q.Field)
    translateField cardinality = \case
      AFColumn colField ->
        -- TODO: make sure certain fields in colField are not in use, since we don't
        -- support them
        pure $ Just $ IR.Q.Column (IR.Q.ColumnContents $ _acfColumn colField)
      AFObjectRelation objRel -> do
        fields <- translateFields cardinality (_aosFields (_aarAnnSelect objRel))
        whereClause <- translateBoolExp (_aosTableFilter (_aarAnnSelect objRel))
        pure . Just . IR.Q.Relationship $
          IR.Q.RelationshipContents
            (HashMap.mapKeys IR.Q.PrimaryKey . fmap IR.Q.ForeignKey $ _aarColumnMapping objRel)
            IR.Q.ObjectRelationship
            ( IR.Q.Query
                { fields = fields,
                  from = _aosTableFrom (_aarAnnSelect objRel),
                  where_ = Just whereClause,
                  limit = Nothing,
                  offset = Nothing,
                  orderBy = [],
                  cardinality = cardinality
                }
            )
      AFArrayRelation (ASSimple arrRel) -> do
        query <- translateAnnSelect IR.Q.Many (_aarAnnSelect arrRel)
        pure . Just . IR.Q.Relationship $
          IR.Q.RelationshipContents
            (HashMap.mapKeys IR.Q.PrimaryKey $ fmap IR.Q.ForeignKey $ _aarColumnMapping arrRel)
            IR.Q.ArrayRelationship
            query
      AFArrayRelation (ASAggregate _) ->
        throw400 NotSupported "translateField: AFArrayRelation ASAggregate not supported"
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
      AnnBoolExp 'DataConnector (UnpreparedValue 'DataConnector) ->
      m IR.E.Expression
    translateBoolExp = \case
      BoolAnd xs ->
        IR.E.And <$> traverse (translateBoolExp) xs
      BoolOr xs ->
        IR.E.Or <$> traverse (translateBoolExp) xs
      BoolNot x ->
        IR.E.Not <$> (translateBoolExp) x
      BoolFld (AVColumn c xs) ->
        IR.E.And
          <$> sequence
            [translateOp (ciColumn c) x | x <- xs]
      BoolFld (AVRelationship _ _) ->
        throw400 NotSupported "The BoolFld AVRelationship expression type is not supported by the Data Connector backend"
      BoolExists _ ->
        throw400 NotSupported "The BoolExists expression type is not supported by the Data Connector backend"

    translateOp ::
      IR.C.Name ->
      OpExpG 'DataConnector (UnpreparedValue 'DataConnector) ->
      m IR.E.Expression
    translateOp columnName opExp = do
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
          pure $ IR.E.ApplyUnaryComparisonOperator IR.E.IsNull columnName
        ANISNOTNULL ->
          pure $ IR.E.Not (IR.E.ApplyUnaryComparisonOperator IR.E.IsNull columnName)
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
        mkApplyBinaryComparisonOperatorToAnotherColumn :: IR.E.BinaryComparisonOperator -> RootOrCurrentColumn 'DataConnector -> m IR.E.Expression
        mkApplyBinaryComparisonOperatorToAnotherColumn operator (RootOrCurrentColumn rootOrCurrent otherColumnName) = do
          case rootOrCurrent of
            IsRoot -> throw400 NotSupported "Comparing columns on the root table in a BoolExp is not supported by the Data Connector backend"
            IsCurrent -> pure $ IR.E.ApplyBinaryComparisonOperator operator columnName (IR.E.AnotherColumn otherColumnName)

        inOperator :: IR.S.Literal -> IR.E.Expression
        inOperator literal =
          let values = case literal of
                IR.S.ArrayLiteral array -> IR.E.ScalarValue <$> array
                IR.S.ValueLiteral value -> [IR.E.ScalarValue value]
           in IR.E.ApplyBinaryArrayComparisonOperator IR.E.In columnName values

        mkApplyBinaryComparisonOperatorToScalar :: IR.E.BinaryComparisonOperator -> IR.S.Value -> IR.E.Expression
        mkApplyBinaryComparisonOperatorToScalar operator value =
          IR.E.ApplyBinaryComparisonOperator operator columnName (IR.E.ScalarValue value)

-- | Validate if a 'IR.Q' contains any relationships.
queryHasRelations :: IR.Q.Query -> Bool
queryHasRelations IR.Q.Query {fields} = getAny $ flip foldMap fields \case
  IR.Q.Relationship _ -> Any True
  _ -> Any False
