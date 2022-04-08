{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hasura.Backends.DataWrapper.Plan
  ( SourceConfig (..),
    Plan (..),
    mkPlan,
    queryHasRelations,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.Align
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Any (..), Min (..))
import Data.These
import Data.Vector qualified as Vector
import Hasura.Backends.DataWrapper.API (Capabilities (..), QueryResponse (..), SchemaResponse (..))
import Hasura.Backends.DataWrapper.Adapter.Types
import Hasura.Backends.DataWrapper.IR.Expression qualified as IR
import Hasura.Backends.DataWrapper.IR.Expression qualified as IR.Expression
import Hasura.Backends.DataWrapper.IR.OrderBy qualified as IR
import Hasura.Backends.DataWrapper.IR.OrderBy qualified as IR.OrderBy
import Hasura.Backends.DataWrapper.IR.Query qualified as IR (Field (..), ForeignKey (..), PrimaryKey (..), Query (..))
import Hasura.Backends.DataWrapper.IR.Query qualified as IR.Query
import Hasura.Backends.DataWrapper.IR.Scalar.Value qualified as IR.Scalar
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Column
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend
import Hasura.Session

--------------------------------------------------------------------------------

-- | A 'Plan' consists of an 'IR.Query' describing the query to be
-- performed by the Agent and a continuation for post processing the
-- response. See the 'postProcessResponseRow' haddock for more
-- information on why we need a post-processor.
data Plan = Plan
  { query :: IR.Query,
    postProcessor :: (QueryResponse -> Either ResponseError QueryResponse)
  }

-- | Error type for the postProcessor continuation. Failure can occur if the Agent
-- returns bad data.
data ResponseError
  = RequiredFieldMissing
  | UnexpectedFields
  | ExpectedObject
  | ExpectedArray
  | UnexpectedResponseCardinality
  deriving (Show, Eq)

-- | Map a 'QueryDB 'DataWrapper' term into a 'Plan'
mkPlan ::
  forall m.
  MonadError QErr m =>
  SessionVariables ->
  SourceConfig ->
  QueryDB 'DataWrapper Void (UnpreparedValue 'DataWrapper) ->
  m Plan
mkPlan session (SourceConfig _ SchemaResponse {srCapabilities} _) ir = translateQueryDB ir
  where
    translateQueryDB ::
      QueryDB 'DataWrapper Void (UnpreparedValue 'DataWrapper) ->
      m Plan
    translateQueryDB =
      traverse replaceSessionVariables >=> \case
        QDBMultipleRows s -> do
          query <- translateAnnSelect IR.Query.Many s
          pure $
            Plan query $ \QueryResponse {getQueryResponse = response} ->
              fmap QueryResponse $ traverse (postProcessResponseRow srCapabilities query) response
        QDBSingleRow s -> do
          query <- translateAnnSelect IR.Query.OneOrZero s
          pure $
            Plan query $ \QueryResponse {getQueryResponse = response} ->
              fmap QueryResponse $ traverse (postProcessResponseRow srCapabilities query) response
        QDBAggregation {} -> throw400 NotSupported "QDBAggregation: not supported"

    translateAnnSelect ::
      IR.Query.Cardinality ->
      AnnSimpleSelectG 'DataWrapper Void IR.Expression ->
      m IR.Query
    translateAnnSelect card s = do
      tableName <- case _asnFrom s of
        FromTable tn -> pure tn
        _ -> throw400 NotSupported "SelectFromG: not supported"
      fields <- translateFields card (_asnFields s)
      let whereClauseWithPermissions =
            case _saWhere (_asnArgs s) of
              Just expr -> BoolAnd [expr, _tpFilter (_asnPerm s)]
              Nothing -> _tpFilter (_asnPerm s)
      whereClause <- translateBoolExp whereClauseWithPermissions
      orderBy <- translateOrderBy (_saOrderBy $ _asnArgs s)
      pure
        IR.Query
          { from = tableName,
            fields = fields,
            limit =
              fmap getMin $
                foldMap
                  (fmap Min)
                  [ _saLimit (_asnArgs s),
                    _tpLimit (_asnPerm s)
                  ],
            offset = fmap fromIntegral (_saOffset (_asnArgs s)),
            where_ = Just whereClause,
            orderBy = orderBy,
            cardinality = card
          }

    translateOrderBy ::
      Maybe (NE.NonEmpty (AnnotatedOrderByItemG 'DataWrapper IR.Expression)) ->
      m [IR.OrderBy]
    translateOrderBy = \case
      Nothing -> pure []
      Just orderBys ->
        do
          NE.toList
          <$> for orderBys \OrderByItemG {..} -> case obiColumn of
            AOCColumn (ColumnInfo {ciColumn = dynColumnName}) ->
              pure
                IR.OrderBy
                  { column = dynColumnName,
                    -- NOTE: Picking a default ordering.
                    ordering = fromMaybe IR.OrderBy.Ascending obiType
                  }
            _ -> throw400 NotSupported "translateOrderBy: references unsupported in dynamic backends"

    translateFields ::
      IR.Query.Cardinality ->
      AnnFieldsG 'DataWrapper Void IR.Expression ->
      m (HashMap Text IR.Query.Field)
    translateFields card xs = do
      fields <- traverse (traverse (translateField card)) xs
      pure $
        HashMap.fromList $
          catMaybes $
            fmap sequence $
              [ (getFieldNameTxt f, field)
                | (f, field) <- fields
              ]

    translateField ::
      IR.Query.Cardinality ->
      AnnFieldG 'DataWrapper Void IR.Expression ->
      m (Maybe IR.Field)
    translateField _card (AFColumn colField) =
      -- TODO: make sure certain fields in colField are not in use, since we don't
      -- support them
      pure $ Just $ IR.Query.Column (IR.Query.ColumnContents $ _acfColumn colField)
    translateField card (AFObjectRelation objRel) = do
      fields <- translateFields card (_aosFields (_aarAnnSelect objRel))
      whereClause <- translateBoolExp (_aosTableFilter (_aarAnnSelect objRel))
      pure $
        Just $
          IR.Query.Relationship $
            IR.Query.RelationshipContents
              (HashMap.mapKeys IR.PrimaryKey $ fmap IR.ForeignKey $ _aarColumnMapping objRel)
              ( IR.Query
                  { fields = fields,
                    from = _aosTableFrom (_aarAnnSelect objRel),
                    where_ = Just whereClause,
                    limit = Nothing,
                    offset = Nothing,
                    orderBy = [],
                    cardinality = card
                  }
              )
    translateField _card (AFArrayRelation (ASSimple arrRel)) = do
      query <- translateAnnSelect IR.Query.Many (_aarAnnSelect arrRel)
      pure $ Just $ IR.Query.Relationship $ IR.Query.RelationshipContents (HashMap.mapKeys IR.PrimaryKey $ fmap IR.ForeignKey $ _aarColumnMapping arrRel) query
    translateField _card (AFExpression _literal) =
      pure Nothing
    translateField _ _ = throw400 NotSupported "translateField: field type not supported"

    replaceSessionVariables ::
      UnpreparedValue 'DataWrapper ->
      m IR.Expression
    replaceSessionVariables (UVLiteral e) = pure e
    replaceSessionVariables (UVParameter _ e) = pure (IR.Expression.Literal (cvValue e))
    replaceSessionVariables UVSession = throw400 NotSupported "replaceSessionVariables: UVSession"
    replaceSessionVariables (UVSessionVar _ v) =
      case getSessionVariableValue v session of
        Nothing -> throw400 NotSupported "session var not found"
        Just s -> pure (IR.Expression.Literal (IR.Scalar.String s))

    translateBoolExp ::
      AnnBoolExp 'DataWrapper IR.Expression ->
      m IR.Expression
    translateBoolExp (BoolAnd xs) =
      IR.And <$> traverse translateBoolExp xs
    translateBoolExp (BoolOr xs) =
      IR.Or <$> traverse translateBoolExp xs
    translateBoolExp (BoolNot x) =
      IR.Not <$> translateBoolExp x
    translateBoolExp (BoolFld (AVColumn c xs)) =
      IR.And
        <$> sequence
          [translateOp (IR.Expression.Column (ciColumn c)) x | x <- xs]
    translateBoolExp _ =
      throw400 NotSupported "An expression type is not supported by the dynamic backend"

    translateOp ::
      IR.Expression ->
      OpExpG 'DataWrapper IR.Expression ->
      m IR.Expression
    translateOp lhs = \case
      AEQ _ rhs ->
        pure $ IR.Expression.Equal lhs rhs
      ANE _ rhs ->
        pure $ IR.Expression.NotEqual lhs rhs
      AGT rhs ->
        pure $ IR.Expression.ApplyOperator IR.Expression.GreaterThan lhs rhs
      ALT rhs ->
        pure $ IR.Expression.ApplyOperator IR.Expression.LessThan lhs rhs
      AGTE rhs ->
        pure $ IR.Expression.ApplyOperator IR.Expression.GreaterThanOrEqual lhs rhs
      ALTE rhs ->
        pure $ IR.Expression.ApplyOperator IR.Expression.LessThanOrEqual lhs rhs
      ANISNULL ->
        pure $ IR.Expression.IsNull lhs
      ANISNOTNULL ->
        pure $ IR.Expression.IsNotNull lhs
      _ ->
        throw400 NotSupported "An operator is not supported by the dynamic backend"

-- | We need to modify any JSON substructures which appear as a result
-- of fetching object relationships, to peel off the outer array sent
-- by the backend.
--
-- This function takes a response object, and the 'Plan' used to
-- fetch it, and modifies any such arrays accordingly.
postProcessResponseRow ::
  Capabilities ->
  IR.Query ->
  J.Object ->
  Either ResponseError J.Object
postProcessResponseRow capabilities IR.Query {fields} row =
  sequenceA $ alignWith go fields row
  where
    go :: These IR.Query.Field J.Value -> Either ResponseError J.Value
    go (This field) =
      case field of
        IR.Query.Literal literal ->
          pure (J.String literal)
        _ ->
          Left RequiredFieldMissing
    go That {} =
      Left UnexpectedFields
    go (These field value) =
      case field of
        IR.Query.Literal {} ->
          Left UnexpectedFields
        IR.Query.Column {} ->
          pure value
        IR.Query.Relationship (IR.Query.RelationshipContents _ subquery@IR.Query {cardinality}) ->
          case value of
            J.Array rows -> do
              processed <- traverse (postProcessResponseRow capabilities subquery <=< parseObject) (toList rows)
              applyCardinalityToResponse cardinality processed
            other
              | dcRelationships capabilities ->
                Left ExpectedArray
              | otherwise ->
                pure other

parseObject :: J.Value -> Either ResponseError J.Object
parseObject = \case
  J.Object obj -> pure obj
  _ -> Left ExpectedObject

-- | If a fk-to-pk lookup comes from an object relationship then we
-- expect 0 or 1 items in the response and we should return it as an object.
-- if it's an array, we have to send back all of the results
applyCardinalityToResponse :: IR.Query.Cardinality -> [J.Object] -> Either ResponseError J.Value
applyCardinalityToResponse IR.Query.OneOrZero = \case
  [] -> pure J.Null
  [x] -> pure $ J.Object x
  _ -> Left UnexpectedResponseCardinality
applyCardinalityToResponse IR.Query.Many =
  pure . J.Array . Vector.fromList . fmap J.Object

-- | Validate if a 'IR.Query' contains any relationships.
queryHasRelations :: IR.Query -> Bool
queryHasRelations IR.Query {fields} = getAny $ flip foldMap fields \case
  IR.Query.Relationship _ -> Any True
  _ -> Any False
