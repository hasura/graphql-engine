{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.Execute
  ( PreparedSql (..),
  )
where

import Control.Monad.Trans.Control qualified as MT
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.IntMap qualified as IntMap
import Data.Sequence qualified as Seq
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection (runTx)
import Hasura.Backends.Postgres.Execute.Insert (convertToSQLTransaction)
import Hasura.Backends.Postgres.Execute.LiveQuery qualified as PGL
import Hasura.Backends.Postgres.Execute.Mutation qualified as PGE
import Hasura.Backends.Postgres.Execute.Prepare
  ( PlanningSt (..),
    PrepArgMap,
    initPlanningSt,
    prepareWithPlan,
    prepareWithoutPlan,
    resolveUnpreparedValue,
    withUserVars,
  )
import Hasura.Backends.Postgres.Execute.Types (PGSourceConfig (..))
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Backends.Postgres.SQL.Value qualified as PG
import Hasura.Backends.Postgres.Translate.Select (PostgresAnnotatedFieldJSON)
import Hasura.Backends.Postgres.Translate.Select qualified as DS
import Hasura.Base.Error (QErr)
import Hasura.EncJSON (EncJSON, encJFromJValue)
import Hasura.GraphQL.Execute.Backend
  ( BackendExecute (..),
    DBStepInfo (..),
    ExplainPlan (..),
    convertRemoteSourceRelationship,
  )
import Hasura.GraphQL.Execute.LiveQuery.Plan
  ( LiveQueryPlan (..),
    LiveQueryPlanExplanation (..),
    ParameterizedLiveQueryPlan (..),
    mkCohortVariables,
    newCohortId,
  )
import Hasura.GraphQL.Namespace
  ( RootFieldAlias (..),
    RootFieldMap,
  )
import Hasura.GraphQL.Parser (UnpreparedValue (..))
import Hasura.Prelude
import Hasura.QueryTags
  ( QueryTagsComment (..),
    emptyQueryTagsComment,
  )
import Hasura.RQL.DML.Internal (dmlTxErrorHandler)
import Hasura.RQL.IR (MutationDB (..), QueryDB (..))
import Hasura.RQL.IR.Delete qualified as IR
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Returning qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Update qualified as IR
import Hasura.RQL.Types
  ( Backend (..),
    BackendType (Postgres),
    FieldName,
    JsonAggSelect (..),
    SourceName,
    getFieldNameTxt,
    liftTx,
  )
import Hasura.RQL.Types.Column (ColumnType (..), ColumnValue (..))
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session (UserInfo (..))
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G

data PreparedSql = PreparedSql
  { _psQuery :: !Q.Query,
    _psPrepArgs :: !PrepArgMap
  }

instance
  ( Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  BackendExecute ('Postgres pgKind)
  where
  type PreparedQuery ('Postgres pgKind) = PreparedSql
  type MultiplexedQuery ('Postgres pgKind) = PGL.MultiplexedQuery
  type ExecutionMonad ('Postgres pgKind) = Tracing.TraceT (Q.TxET QErr IO)

  mkDBQueryPlan = pgDBQueryPlan
  mkDBMutationPlan = pgDBMutationPlan
  mkDBSubscriptionPlan = pgDBSubscriptionPlan
  mkDBQueryExplain = pgDBQueryExplain
  mkLiveQueryExplain = pgDBLiveQueryExplain
  mkDBRemoteRelationshipPlan = pgDBRemoteRelationshipPlan

-- query

pgDBQueryPlan ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  QueryDB ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind)) ->
  m (DBStepInfo ('Postgres pgKind))
pgDBQueryPlan userInfo sourceName sourceConfig qrf = do
  (preparedQuery, PlanningSt _ _ planVals) <-
    flip runStateT initPlanningSt $ traverse (prepareWithPlan userInfo) qrf
  queryTagsComment <- ask
  let preparedSQLWithQueryTags = appendPreparedSQLWithQueryTags (irToRootFieldPlan planVals preparedQuery) queryTagsComment
  let (action, preparedSQL) = mkCurPlanTx userInfo preparedSQLWithQueryTags
  pure $ DBStepInfo @('Postgres pgKind) sourceName sourceConfig preparedSQL action

pgDBQueryExplain ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  RootFieldAlias ->
  UserInfo ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  QueryDB ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind)) ->
  m (AB.AnyBackend DBStepInfo)
pgDBQueryExplain fieldName userInfo sourceName sourceConfig rootSelection = do
  preparedQuery <- traverse (resolveUnpreparedValue userInfo) rootSelection
  let PreparedSql querySQL _ = irToRootFieldPlan mempty preparedQuery
      textSQL = Q.getQueryText querySQL
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      withExplain = "EXPLAIN (FORMAT TEXT) " <> textSQL
  let action =
        liftTx $
          Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True <&> \planList ->
            encJFromJValue $ ExplainPlan fieldName (Just textSQL) (Just $ map runIdentity planList)
  pure $
    AB.mkAnyBackend $
      DBStepInfo @('Postgres pgKind) sourceName sourceConfig Nothing action

pgDBLiveQueryExplain ::
  ( MonadError QErr m,
    MonadIO m,
    MT.MonadBaseControl IO m
  ) =>
  LiveQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)) ->
  m LiveQueryPlanExplanation
pgDBLiveQueryExplain plan = do
  let parameterizedPlan = _lqpParameterizedPlan plan
      pgExecCtx = _pscExecCtx $ _lqpSourceConfig plan
      queryText = Q.getQueryText . PGL.unMultiplexedQuery $ _plqpQuery parameterizedPlan
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      explainQuery = Q.fromText $ "EXPLAIN (FORMAT TEXT) " <> queryText
  cohortId <- newCohortId
  explanationLines <-
    liftEitherM $
      runExceptT $
        runTx pgExecCtx Q.ReadOnly $
          map runIdentity <$> PGL.executeQuery explainQuery [(cohortId, _lqpVariables plan)]
  pure $ LiveQueryPlanExplanation queryText explanationLines $ _lqpVariables plan

-- mutation

convertDelete ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  UserInfo ->
  IR.AnnDelG ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind)) ->
  Bool ->
  QueryTagsComment ->
  m (Tracing.TraceT (Q.TxET QErr IO) EncJSON)
convertDelete userInfo deleteOperation stringifyNum queryTags = do
  preparedDelete <- traverse (prepareWithoutPlan userInfo) deleteOperation
  pure $ flip runReaderT queryTags $ PGE.execDeleteQuery stringifyNum userInfo (preparedDelete, Seq.empty)

convertUpdate ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  UserInfo ->
  IR.AnnUpdG ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind)) ->
  Bool ->
  QueryTagsComment ->
  m (Tracing.TraceT (Q.TxET QErr IO) EncJSON)
convertUpdate userInfo updateOperation stringifyNum queryTags = do
  preparedUpdate <- traverse (prepareWithoutPlan userInfo) updateOperation
  if null $ IR.uqp1OpExps updateOperation
    then pure $ pure $ IR.buildEmptyMutResp $ IR.uqp1Output preparedUpdate
    else
      pure $
        flip runReaderT queryTags $
          PGE.execUpdateQuery stringifyNum userInfo (preparedUpdate, Seq.empty)

convertInsert ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  UserInfo ->
  IR.AnnInsert ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind)) ->
  Bool ->
  QueryTagsComment ->
  m (Tracing.TraceT (Q.TxET QErr IO) EncJSON)
convertInsert userInfo insertOperation stringifyNum queryTags = do
  preparedInsert <- traverse (prepareWithoutPlan userInfo) insertOperation
  pure $ flip runReaderT queryTags $ convertToSQLTransaction preparedInsert userInfo Seq.empty stringifyNum

-- | A pared-down version of 'Query.convertQuerySelSet', for use in execution of
-- special case of SQL function mutations (see 'MDBFunction').
convertFunction ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  UserInfo ->
  JsonAggSelect ->
  -- | VOLATILE function as 'SelectExp'
  IR.AnnSimpleSelectG ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind)) ->
  -- | Query Tags
  QueryTagsComment ->
  m (Tracing.TraceT (Q.TxET QErr IO) EncJSON)
convertFunction userInfo jsonAggSelect unpreparedQuery queryTags = do
  -- Transform the RQL AST into a prepared SQL query
  (preparedQuery, PlanningSt _ _ planVals) <-
    flip runStateT initPlanningSt $
      traverse (prepareWithPlan userInfo) unpreparedQuery
  let queryResultFn =
        case jsonAggSelect of
          JASMultipleRows -> QDBMultipleRows
          JASSingleObject -> QDBSingleRow
  let preparedSQLWithQueryTags = appendPreparedSQLWithQueryTags (irToRootFieldPlan planVals $ queryResultFn preparedQuery) queryTags
  pure
    $! fst
    $ mkCurPlanTx userInfo preparedSQLWithQueryTags -- forget (Maybe PreparedSql)

pgDBMutationPlan ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  Bool ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  MutationDB ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind)) ->
  m (DBStepInfo ('Postgres pgKind))
pgDBMutationPlan userInfo stringifyNum sourceName sourceConfig mrf = do
  mutationQueryTagsComment <- ask
  go <$> case mrf of
    MDBInsert s -> convertInsert userInfo s stringifyNum mutationQueryTagsComment
    MDBUpdate s -> convertUpdate userInfo s stringifyNum mutationQueryTagsComment
    MDBDelete s -> convertDelete userInfo s stringifyNum mutationQueryTagsComment
    MDBFunction returnsSet s -> convertFunction userInfo returnsSet s mutationQueryTagsComment
  where
    go v = DBStepInfo @('Postgres pgKind) sourceName sourceConfig Nothing v

-- subscription

pgDBSubscriptionPlan ::
  forall pgKind m.
  ( MonadError QErr m,
    MonadIO m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  Maybe G.Name ->
  RootFieldMap (QueryDB ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind))) ->
  m (LiveQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)))
pgDBSubscriptionPlan userInfo _sourceName sourceConfig namespace unpreparedAST = do
  (preparedAST, PGL.QueryParametersInfo {..}) <-
    flip runStateT mempty $
      for unpreparedAST $ traverse (PGL.resolveMultiplexedValue $ _uiSession userInfo)
  mutationQueryTagsComment <- ask
  let multiplexedQuery = PGL.mkMultiplexedQuery $ OMap.mapKeys _rfaAlias preparedAST
      multiplexedQueryWithQueryTags =
        multiplexedQuery {PGL.unMultiplexedQuery = appendSQLWithQueryTags (PGL.unMultiplexedQuery multiplexedQuery) mutationQueryTagsComment}
      roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedLiveQueryPlan roleName multiplexedQueryWithQueryTags

  -- We need to ensure that the values provided for variables are correct according to Postgres.
  -- Without this check an invalid value for a variable for one instance of the subscription will
  -- take down the entire multiplexed query.
  validatedQueryVars <- PGL.validateVariables (_pscExecCtx sourceConfig) _qpiReusableVariableValues
  validatedSyntheticVars <- PGL.validateVariables (_pscExecCtx sourceConfig) $ toList _qpiSyntheticVariableValues

  -- TODO validatedQueryVars validatedSyntheticVars
  let cohortVariables =
        mkCohortVariables
          _qpiReferencedSessionVariables
          (_uiSession userInfo)
          validatedQueryVars
          validatedSyntheticVars

  pure $ LiveQueryPlan parameterizedPlan sourceConfig cohortVariables namespace

-- turn the current plan into a transaction
mkCurPlanTx ::
  UserInfo ->
  PreparedSql ->
  (Tracing.TraceT (Q.TxET QErr IO) EncJSON, Maybe PreparedSql)
mkCurPlanTx userInfo ps@(PreparedSql q prepMap) =
  -- generate the SQL and prepared vars or the bytestring
  let args = withUserVars (_uiSession userInfo) prepMap
      -- WARNING: this quietly assumes the intmap keys are contiguous
      prepArgs = fst <$> IntMap.elems args
   in (,Just ps) $
        Tracing.trace "Postgres" $ liftTx $ DS.asSingleRowJsonResp q prepArgs

-- convert a query from an intermediate representation to... another
irToRootFieldPlan ::
  ( Backend ('Postgres pgKind),
    DS.PostgresAnnotatedFieldJSON pgKind
  ) =>
  PrepArgMap ->
  QueryDB ('Postgres pgKind) (Const Void) S.SQLExp ->
  PreparedSql
irToRootFieldPlan prepped = \case
  QDBMultipleRows s -> mkPreparedSql (DS.selectQuerySQL JASMultipleRows) s
  QDBSingleRow s -> mkPreparedSql (DS.selectQuerySQL JASSingleObject) s
  QDBAggregation s -> mkPreparedSql DS.selectAggregateQuerySQL s
  QDBConnection s -> mkPreparedSql DS.connectionSelectQuerySQL s
  where
    mkPreparedSql :: (t -> Q.Query) -> t -> PreparedSql
    mkPreparedSql f simpleSel =
      PreparedSql (f simpleSel) prepped

-- Append Query Tags to the Prepared SQL
appendPreparedSQLWithQueryTags :: PreparedSql -> QueryTagsComment -> PreparedSql
appendPreparedSQLWithQueryTags preparedSQL queryTags =
  preparedSQL {_psQuery = appendSQLWithQueryTags query queryTags}
  where
    query = _psQuery preparedSQL

appendSQLWithQueryTags :: Q.Query -> QueryTagsComment -> Q.Query
appendSQLWithQueryTags query queryTags = query {Q.getQueryText = queryText <> _unQueryTagsComment queryTags}
  where
    queryText = Q.getQueryText query

--------------------------------------------------------------------------------
-- Remote Relationships (e.g. DB-to-DB Joins, remote schema joins, etc.)
--------------------------------------------------------------------------------

-- | Construct an action (i.e. 'DBStepInfo') which can marshal some remote
-- relationship information into a form that Postgres can query against.
pgDBRemoteRelationshipPlan ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind
  ) =>
  UserInfo ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  -- | List of json objects, each of which becomes a row of the table.
  NonEmpty J.Object ->
  -- | The above objects have this schema
  --
  -- XXX: What is this for/what does this mean?
  HashMap FieldName (Column ('Postgres pgKind), ScalarType ('Postgres pgKind)) ->
  -- | This is a field name from the lhs that *has* to be selected in the
  -- response along with the relationship.
  FieldName ->
  (FieldName, IR.SourceRelationshipSelection ('Postgres pgKind) (Const Void) UnpreparedValue) ->
  m (DBStepInfo ('Postgres pgKind))
pgDBRemoteRelationshipPlan userInfo sourceName sourceConfig lhs lhsSchema argumentId relationship = do
  -- NOTE: 'QueryTags' currently cannot support remote relationship queries.
  --
  -- In the future if we want to add support we'll need to add a new type of
  -- metadata (e.g. 'ParameterizedQueryHash' doesn't make sense here) and find
  -- a root field name that makes sense to attach to it.
  flip runReaderT emptyQueryTagsComment $ pgDBQueryPlan userInfo sourceName sourceConfig rootSelection
  where
    coerceToColumn = PG.unsafePGCol . getFieldNameTxt
    joinColumnMapping = mapKeys coerceToColumn lhsSchema

    rowsArgument :: UnpreparedValue ('Postgres pgKind)
    rowsArgument =
      UVParameter Nothing $
        ColumnValue (ColumnScalar PG.PGJSONB) $
          PG.PGValJSONB $ Q.JSONB $ J.toJSON lhs
    jsonToRecordSet :: IR.SelectFromG ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))

    recordSetDefinitionList =
      (coerceToColumn argumentId, PG.PGBigInt) : Map.toList (fmap snd joinColumnMapping)
    jsonToRecordSet =
      IR.FromFunction
        (PG.QualifiedObject "pg_catalog" $ PG.FunctionName "jsonb_to_recordset")
        (IR.FunctionArgsExp [IR.AEInput rowsArgument] mempty)
        (Just recordSetDefinitionList)

    rootSelection =
      convertRemoteSourceRelationship
        (fst <$> joinColumnMapping)
        jsonToRecordSet
        (PG.unsafePGCol $ getFieldNameTxt argumentId)
        (ColumnScalar PG.PGBigInt)
        relationship
