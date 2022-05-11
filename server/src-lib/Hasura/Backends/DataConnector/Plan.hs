{-# LANGUAGE NamedFieldPuns #-}

module Hasura.Backends.DataConnector.Plan
  ( SourceConfig (..),
    Plan (..),
    mkPlan,
    renderPlan,
    queryHasRelations,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as J
import Data.Align
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Any (..), Min (..))
import Data.Text as T
import Data.Text.Encoding qualified as TE
import Data.These
import Data.Vector qualified as V
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.DataConnector.IR.Export qualified as IR
import Hasura.Backends.DataConnector.IR.Expression qualified as IR.E
import Hasura.Backends.DataConnector.IR.OrderBy qualified as IR.O
import Hasura.Backends.DataConnector.IR.Query qualified as IR.Q
import Hasura.Backends.DataConnector.IR.Scalar.Value qualified as IR.S
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

-- | A 'Plan' consists of an 'IR.Q' describing the query to be
-- performed by the Agent and a continuation for post processing the
-- response. See the 'postProcessResponseRow' haddock for more
-- information on why we need a post-processor.
data Plan = Plan
  { query :: IR.Q.Query,
    postProcessor :: (API.QueryResponse -> Either ResponseError API.QueryResponse)
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

-- | Extract the 'IR.Q' from a 'Plan' and render it as 'Text'.
--
-- NOTE: This is for logging and debug purposes only.
renderPlan :: Plan -> Text
renderPlan =
  TE.decodeUtf8 . BL.toStrict . J.encode . IR.queryToAPI . query

-- | Map a 'QueryDB 'DataConnector' term into a 'Plan'
mkPlan ::
  forall m.
  MonadError QErr m =>
  SessionVariables ->
  SourceConfig ->
  QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
  m Plan
mkPlan session (SourceConfig {_scSchema = API.SchemaResponse {..}}) ir = translateQueryDB ir
  where
    translateQueryDB ::
      QueryDB 'DataConnector Void (UnpreparedValue 'DataConnector) ->
      m Plan
    translateQueryDB =
      traverse replaceSessionVariables >=> \case
        QDBMultipleRows s -> do
          query <- translateAnnSelect IR.Q.Many s
          pure $
            Plan query $ \API.QueryResponse {getQueryResponse = response} ->
              fmap API.QueryResponse $ traverse (postProcessResponseRow srCapabilities query) response
        QDBSingleRow s -> do
          query <- translateAnnSelect IR.Q.OneOrZero s
          pure $
            Plan query $ \API.QueryResponse {getQueryResponse = response} ->
              fmap API.QueryResponse $ traverse (postProcessResponseRow srCapabilities query) response
        QDBAggregation {} -> throw400 NotSupported "QDBAggregation: not supported"

    translateAnnSelect ::
      IR.Q.Cardinality ->
      AnnSimpleSelectG 'DataConnector Void IR.E.Expression ->
      m IR.Q.Query
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
        IR.Q.Query
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
      Maybe (NE.NonEmpty (AnnotatedOrderByItemG 'DataConnector IR.E.Expression)) ->
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
            _ -> throw400 NotSupported "translateOrderBy: references unsupported in the Data Connector backend"

    translateFields ::
      IR.Q.Cardinality ->
      AnnFieldsG 'DataConnector Void IR.E.Expression ->
      m (HashMap Text IR.Q.Field)
    translateFields card xs = do
      fields <- traverse (traverse (translateField card)) xs
      pure $
        HashMap.fromList $
          mapMaybe
            sequence
            ( [ (getFieldNameTxt f, field)
                | (f, field) <- fields
              ]
            )

    translateField ::
      IR.Q.Cardinality ->
      AnnFieldG 'DataConnector Void IR.E.Expression ->
      m (Maybe IR.Q.Field)
    translateField _card (AFColumn colField) =
      -- TODO: make sure certain fields in colField are not in use, since we don't
      -- support them
      pure $ Just $ IR.Q.Column (IR.Q.ColumnContents $ _acfColumn colField)
    translateField card (AFObjectRelation objRel) = do
      fields <- translateFields card (_aosFields (_aarAnnSelect objRel))
      whereClause <- translateBoolExp (_aosTableFilter (_aarAnnSelect objRel))
      pure $
        Just $
          IR.Q.Relationship $
            IR.Q.RelationshipContents
              (HashMap.mapKeys IR.Q.PrimaryKey $ fmap IR.Q.ForeignKey $ _aarColumnMapping objRel)
              ( IR.Q.Query
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
      query <- translateAnnSelect IR.Q.Many (_aarAnnSelect arrRel)
      pure $ Just $ IR.Q.Relationship $ IR.Q.RelationshipContents (HashMap.mapKeys IR.Q.PrimaryKey $ fmap IR.Q.ForeignKey $ _aarColumnMapping arrRel) query
    translateField _card (AFExpression _literal) =
      pure Nothing
    translateField _ _ = throw400 NotSupported "translateField: field type not supported"

    replaceSessionVariables ::
      UnpreparedValue 'DataConnector ->
      m IR.E.Expression
    replaceSessionVariables (UVLiteral e) = pure e
    replaceSessionVariables (UVParameter _ e) = pure (IR.E.Literal (cvValue e))
    replaceSessionVariables UVSession = throw400 NotSupported "replaceSessionVariables: UVSession"
    replaceSessionVariables (UVSessionVar _ v) =
      case getSessionVariableValue v session of
        Nothing -> throw400 NotSupported "session var not found"
        Just s -> pure (IR.E.Literal (IR.S.String s))

    translateBoolExp ::
      AnnBoolExp 'DataConnector IR.E.Expression ->
      m IR.E.Expression
    translateBoolExp (BoolAnd xs) =
      IR.E.And <$> traverse translateBoolExp xs
    translateBoolExp (BoolOr xs) =
      IR.E.Or <$> traverse translateBoolExp xs
    translateBoolExp (BoolNot x) =
      IR.E.Not <$> translateBoolExp x
    translateBoolExp (BoolFld (AVColumn c xs)) =
      IR.E.And
        <$> sequence
          [translateOp (IR.E.Column (ciColumn c)) x | x <- xs]
    translateBoolExp _ =
      throw400 NotSupported "An expression type is not supported by the Data Connector backend"

    translateOp ::
      IR.E.Expression ->
      OpExpG 'DataConnector IR.E.Expression ->
      m IR.E.Expression
    translateOp lhs = \case
      AEQ _ rhs ->
        pure $ IR.E.ApplyOperator IR.E.Equal lhs rhs
      ANE _ rhs ->
        pure $ IR.E.Not (IR.E.ApplyOperator IR.E.Equal lhs rhs)
      AGT rhs ->
        pure $ IR.E.ApplyOperator IR.E.GreaterThan lhs rhs
      ALT rhs ->
        pure $ IR.E.ApplyOperator IR.E.LessThan lhs rhs
      AGTE rhs ->
        pure $ IR.E.ApplyOperator IR.E.GreaterThanOrEqual lhs rhs
      ALTE rhs ->
        pure $ IR.E.ApplyOperator IR.E.LessThanOrEqual lhs rhs
      ANISNULL ->
        pure $ IR.E.IsNull lhs
      ANISNOTNULL ->
        pure $ IR.E.Not (IR.E.IsNull lhs)
      AIN (IR.E.Array rhs) -> pure $ IR.E.In lhs rhs
      ANIN (IR.E.Array rhs) -> pure $ IR.E.Not $ IR.E.In lhs rhs
      _ ->
        throw400 NotSupported "An operator is not supported by the Data Connector backend"

-- | We need to modify any JSON substructures which appear as a result
-- of fetching object relationships, to peel off the outer array sent
-- by the backend.
--
-- This function takes a response object, and the 'Plan' used to
-- fetch it, and modifies any such arrays accordingly.
postProcessResponseRow ::
  API.Capabilities ->
  IR.Q.Query ->
  J.Object ->
  Either ResponseError J.Object
postProcessResponseRow capabilities IR.Q.Query {fields} row =
  sequenceA $ alignWith go fields row
  where
    go :: These IR.Q.Field J.Value -> Either ResponseError J.Value
    go (This field) =
      case field of
        IR.Q.Literal literal ->
          pure (J.String literal)
        _ ->
          Left RequiredFieldMissing
    go That {} =
      Left UnexpectedFields
    go (These field value) =
      case field of
        IR.Q.Literal {} ->
          Left UnexpectedFields
        IR.Q.Column {} ->
          pure value
        IR.Q.Relationship (IR.Q.RelationshipContents _ subquery@IR.Q.Query {cardinality}) ->
          case value of
            J.Array rows -> do
              processed <- traverse (postProcessResponseRow capabilities subquery <=< parseObject) (toList rows)
              applyCardinalityToResponse cardinality processed
            other
              | API.dcRelationships capabilities ->
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
applyCardinalityToResponse :: IR.Q.Cardinality -> [J.Object] -> Either ResponseError J.Value
applyCardinalityToResponse IR.Q.OneOrZero = \case
  [] -> pure J.Null
  [x] -> pure $ J.Object x
  _ -> Left UnexpectedResponseCardinality
applyCardinalityToResponse IR.Q.Many =
  pure . J.Array . V.fromList . fmap J.Object

-- | Validate if a 'IR.Q' contains any relationships.
queryHasRelations :: IR.Q.Query -> Bool
queryHasRelations IR.Q.Query {fields} = getAny $ flip foldMap fields \case
  IR.Q.Relationship _ -> Any True
  _ -> Any False
