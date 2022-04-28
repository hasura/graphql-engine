{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Instances Execute
--
-- This module implements the needed functionality for implementing a 'BackendExecute'
-- instance for Postgres, which defines an interface for translating a root field into
-- an execution plan and interacting with a database.
--
-- This module includes the Postgres implementation of queries, mutations, and more.
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
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.Execute.Insert (convertToSQLTransaction)
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
import Hasura.Backends.Postgres.Execute.Subscription qualified as PGL
import Hasura.Backends.Postgres.Execute.Types (PGSourceConfig (..), dmlTxErrorHandler)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Backends.Postgres.SQL.Value qualified as PG
import Hasura.Backends.Postgres.Translate.Select (PostgresAnnotatedFieldJSON)
import Hasura.Backends.Postgres.Translate.Select qualified as DS
import Hasura.Backends.Postgres.Types.Update
import Hasura.Base.Error (QErr)
import Hasura.EncJSON (EncJSON, encJFromBS, encJFromJValue)
import Hasura.GraphQL.Execute.Backend
  ( BackendExecute (..),
    DBStepInfo (..),
    ExplainPlan (..),
    convertRemoteSourceRelationship,
  )
import Hasura.GraphQL.Execute.Subscription.Plan
  ( ParameterizedSubscriptionQueryPlan (..),
    SubscriptionQueryPlan (..),
    SubscriptionQueryPlanExplanation (..),
    mkCohortVariables,
    newCohortId,
  )
import Hasura.GraphQL.Namespace
  ( RootFieldAlias (..),
    RootFieldMap,
  )
import Hasura.GraphQL.Namespace qualified as G
import Hasura.GraphQL.Parser
  ( UnpreparedValue (..),
  )
import Hasura.Prelude
import Hasura.QueryTags
  ( QueryTagsComment (..),
    emptyQueryTagsComment,
  )
import Hasura.RQL.IR (MutationDB (..), QueryDB (..))
import Hasura.RQL.IR.Delete qualified as IR
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Returning qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Update qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
  ( ColumnType (..),
    ColumnValue (..),
    ciName,
  )
import Hasura.RQL.Types.Common
  ( FieldName (..),
    JsonAggSelect (..),
    SourceName,
    StringifyNumbers,
  )
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
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
  mkLiveQuerySubscriptionPlan = pgDBLiveQuerySubscriptionPlan
  mkDBStreamingSubscriptionPlan = pgDBStreamingSubscriptionPlan
  mkDBQueryExplain = pgDBQueryExplain
  mkSubscriptionExplain = pgDBSubscriptionExplain
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
  QueryDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
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
  QueryDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
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

pgDBSubscriptionExplain ::
  ( MonadError QErr m,
    MonadIO m,
    MT.MonadBaseControl IO m
  ) =>
  SubscriptionQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)) ->
  m SubscriptionQueryPlanExplanation
pgDBSubscriptionExplain plan = do
  let parameterizedPlan = _sqpParameterizedPlan plan
      pgExecCtx = _pscExecCtx $ _sqpSourceConfig plan
      queryText = Q.getQueryText . PGL.unMultiplexedQuery $ _plqpQuery parameterizedPlan
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      explainQuery = Q.fromText $ "EXPLAIN (FORMAT TEXT) " <> queryText
  cohortId <- newCohortId
  explanationLines <-
    liftEitherM $
      runExceptT $
        runTx pgExecCtx Q.ReadOnly $
          map runIdentity <$> PGL.executeQuery explainQuery [(cohortId, _sqpVariables plan)]
  pure $ SubscriptionQueryPlanExplanation queryText explanationLines $ _sqpVariables plan

-- mutation

convertDelete ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  IR.AnnDelG ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  StringifyNumbers ->
  m (Tracing.TraceT (Q.TxET QErr IO) EncJSON)
convertDelete userInfo deleteOperation stringifyNum = do
  queryTags <- ask
  preparedDelete <- traverse (prepareWithoutPlan userInfo) deleteOperation
  pure $ flip runReaderT queryTags $ PGE.execDeleteQuery stringifyNum userInfo (preparedDelete, Seq.empty)

convertUpdate ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  IR.AnnotatedUpdateG ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  StringifyNumbers ->
  m (Tracing.TraceT (Q.TxET QErr IO) EncJSON)
convertUpdate userInfo updateOperation stringifyNum = do
  queryTags <- ask
  preparedUpdate <- traverse (prepareWithoutPlan userInfo) updateOperation
  if null $ updateOperations . IR._auBackend $ updateOperation
    then pure $ pure $ IR.buildEmptyMutResp $ IR._auOutput preparedUpdate
    else
      pure $
        flip runReaderT queryTags $
          PGE.execUpdateQuery stringifyNum userInfo (preparedUpdate, Seq.empty)

convertInsert ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  IR.AnnotatedInsert ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  StringifyNumbers ->
  m (Tracing.TraceT (Q.TxET QErr IO) EncJSON)
convertInsert userInfo insertOperation stringifyNum = do
  queryTags <- ask
  preparedInsert <- traverse (prepareWithoutPlan userInfo) insertOperation
  pure $ flip runReaderT queryTags $ convertToSQLTransaction preparedInsert userInfo Seq.empty stringifyNum

-- | A pared-down version of 'Query.convertQuerySelSet', for use in execution of
-- special case of SQL function mutations (see 'MDBFunction').
convertFunction ::
  forall pgKind m.
  ( MonadError QErr m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  UserInfo ->
  JsonAggSelect ->
  -- | VOLATILE function as 'SelectExp'
  IR.AnnSimpleSelectG ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  m (Tracing.TraceT (Q.TxET QErr IO) EncJSON)
convertFunction userInfo jsonAggSelect unpreparedQuery = do
  queryTags <- ask
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
  StringifyNumbers ->
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  MutationDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)) ->
  m (DBStepInfo ('Postgres pgKind))
pgDBMutationPlan userInfo stringifyNum sourceName sourceConfig mrf = do
  go <$> case mrf of
    MDBInsert s -> convertInsert userInfo s stringifyNum
    MDBUpdate s -> convertUpdate userInfo s stringifyNum
    MDBDelete s -> convertDelete userInfo s stringifyNum
    MDBFunction returnsSet s -> convertFunction userInfo returnsSet s
  where
    go v = DBStepInfo @('Postgres pgKind) sourceName sourceConfig Nothing v

-- subscription

pgDBLiveQuerySubscriptionPlan ::
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
  RootFieldMap (QueryDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind))) ->
  m (SubscriptionQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)))
pgDBLiveQuerySubscriptionPlan userInfo _sourceName sourceConfig namespace unpreparedAST = do
  (preparedAST, PGL.QueryParametersInfo {..}) <-
    flip runStateT mempty $
      for unpreparedAST $ traverse (PGL.resolveMultiplexedValue (_uiSession userInfo))
  subscriptionQueryTagsComment <- ask
  let multiplexedQuery = PGL.mkMultiplexedQuery $ OMap.mapKeys _rfaAlias preparedAST
      multiplexedQueryWithQueryTags =
        multiplexedQuery {PGL.unMultiplexedQuery = appendSQLWithQueryTags (PGL.unMultiplexedQuery multiplexedQuery) subscriptionQueryTagsComment}
      roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedSubscriptionQueryPlan roleName multiplexedQueryWithQueryTags

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
          mempty -- live query subscriptions don't use the streaming cursor variables
  pure $ SubscriptionQueryPlan parameterizedPlan sourceConfig cohortVariables namespace

pgDBStreamingSubscriptionPlan ::
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
  (RootFieldAlias, (QueryDB ('Postgres pgKind) Void (UnpreparedValue ('Postgres pgKind)))) ->
  m (SubscriptionQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)))
pgDBStreamingSubscriptionPlan userInfo _sourceName sourceConfig (rootFieldAlias, unpreparedAST) = do
  (preparedAST, PGL.QueryParametersInfo {..}) <-
    flip runStateT mempty $
      traverse (PGL.resolveMultiplexedValue (_uiSession userInfo)) unpreparedAST
  subscriptionQueryTagsComment <- ask
  let multiplexedQuery = PGL.mkStreamingMultiplexedQuery (G._rfaAlias rootFieldAlias, preparedAST)
      multiplexedQueryWithQueryTags =
        multiplexedQuery {PGL.unMultiplexedQuery = appendSQLWithQueryTags (PGL.unMultiplexedQuery multiplexedQuery) subscriptionQueryTagsComment}
      roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedSubscriptionQueryPlan roleName multiplexedQueryWithQueryTags

  -- We need to ensure that the values provided for variables are correct according to Postgres.
  -- Without this check an invalid value for a variable for one instance of the subscription will
  -- take down the entire multiplexed query.
  validatedQueryVars <- PGL.validateVariables (_pscExecCtx sourceConfig) _qpiReusableVariableValues
  validatedSyntheticVars <- PGL.validateVariables (_pscExecCtx sourceConfig) $ toList _qpiSyntheticVariableValues
  validatedCursorVars <- PGL.validateVariables (_pscExecCtx sourceConfig) $ getCursorVars unpreparedAST

  let cohortVariables =
        mkCohortVariables
          _qpiReferencedSessionVariables
          (_uiSession userInfo)
          validatedQueryVars
          validatedSyntheticVars
          validatedCursorVars

  pure $ SubscriptionQueryPlan parameterizedPlan sourceConfig cohortVariables $ _rfaNamespace rootFieldAlias
  where
    getCursorVars qdb =
      case qdb of
        QDBStreamMultipleRows (IR.AnnSelectStreamG () _ _ _ args _) ->
          let cursorArg = IR._ssaCursorArg args
              colInfo = IR._sciColInfo cursorArg
           in Map.singleton (ciName colInfo) (IR._sciInitialValue cursorArg)
        _ -> mempty

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
        Tracing.trace "Postgres" $ liftTx $ asSingleRowJsonResp q prepArgs

-- | This function is generally used on the result of 'selectQuerySQL',
-- 'selectAggregateQuerySQL' or 'connectionSelectSQL' to run said query and get
-- back the resulting JSON.
asSingleRowJsonResp ::
  Q.Query ->
  [Q.PrepArg] ->
  Q.TxE QErr EncJSON
asSingleRowJsonResp query args =
  encJFromBS . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler query args True

-- convert a query from an intermediate representation to... another
irToRootFieldPlan ::
  ( Backend ('Postgres pgKind),
    DS.PostgresAnnotatedFieldJSON pgKind
  ) =>
  PrepArgMap ->
  QueryDB ('Postgres pgKind) Void S.SQLExp ->
  PreparedSql
irToRootFieldPlan prepped = \case
  QDBMultipleRows s -> mkPreparedSql (DS.selectQuerySQL JASMultipleRows) s
  QDBSingleRow s -> mkPreparedSql (DS.selectQuerySQL JASSingleObject) s
  QDBAggregation s -> mkPreparedSql DS.selectAggregateQuerySQL s
  QDBConnection s -> mkPreparedSql DS.connectionSelectQuerySQL s
  QDBStreamMultipleRows s -> mkPreparedSql DS.selectStreamQuerySQL s
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
  (FieldName, IR.SourceRelationshipSelection ('Postgres pgKind) Void UnpreparedValue) ->
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
