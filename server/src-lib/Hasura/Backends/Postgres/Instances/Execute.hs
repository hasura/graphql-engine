{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Instances.Execute
  ( PreparedSql(..)
  , runPGMutationTransaction
  ) where

import           Hasura.Prelude

import           Control.Monad.Morph                        (hoist)
import qualified Control.Monad.Trans.Control                as MT
import qualified Data.ByteString                            as B
import qualified Data.HashMap.Strict.InsOrd                 as OMap
import qualified Data.HashSet                               as Set
import qualified Data.IntMap                                as IntMap
import qualified Data.Sequence                              as Seq
import qualified Database.PG.Query                          as Q
import qualified Hasura.Logging                             as L
import qualified Language.GraphQL.Draft.Syntax              as G

import qualified Hasura.Backends.Postgres.Execute.LiveQuery as PGL
import qualified Hasura.Backends.Postgres.Execute.Mutation  as PGE
import qualified Hasura.Backends.Postgres.SQL.DML           as S
import qualified Hasura.Backends.Postgres.Translate.Select  as DS
import qualified Hasura.RQL.IR.Delete                       as IR
import qualified Hasura.RQL.IR.Insert                       as IR
import qualified Hasura.RQL.IR.Returning                    as IR
import qualified Hasura.RQL.IR.Select                       as IR
import qualified Hasura.RQL.IR.Update                       as IR
import qualified Hasura.Tracing                             as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.Execute.Insert
import           Hasura.Backends.Postgres.Execute.Prepare
import           Hasura.Backends.Postgres.Translate.Select  (PostgresAnnotatedFieldJSON)
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Parser
import           Hasura.RQL.DML.Internal                    (dmlTxErrorHandler)
import           Hasura.RQL.IR
import           Hasura.RQL.Types
import           Hasura.Server.Types                        (RequestId)
import           Hasura.Server.Version                      (HasVersion)
import           Hasura.Session
import           Hasura.Tracing

data PreparedSql
  = PreparedSql
  { _psQuery    :: !Q.Query
  , _psPrepArgs :: !PrepArgMap
  }


instance
  ( Backend ('Postgres pgKind)
  ,  PostgresAnnotatedFieldJSON pgKind
  ) => BackendExecute ('Postgres pgKind) where

  type MultiplexedQuery ('Postgres pgKind) = PGL.MultiplexedQuery

  executeQueryField = pgDBQueryPlan
  executeMutationField = pgDBMutationPlan
  makeLiveQueryPlan = pgDBSubscriptionPlan
  explainQueryField = pgDBQueryExplain
  explainLiveQuery = pgDBLiveQueryExplain
  executeMultiplexedQuery = runPGSubscription


-- query

pgDBQueryPlan
  :: forall pgKind m
   . ( MonadError QErr m
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     , MonadTrace m
     , MonadIO m
     )
  => RequestId
  -> L.Logger L.Hasura
  -> UserInfo
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> QueryDB ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> m EncJSON
pgDBQueryPlan requestId logger userInfo sourceName sourceConfig qrf = do
  (preparedQuery, PlanningSt _ _ planVals expectedVariables) <- flip runStateT initPlanningSt $ traverseQueryDB @('Postgres pgKind) prepareWithPlan qrf
  validateSessionVariables expectedVariables $ _uiSession userInfo
  let (action, preparedSQL) = mkCurPlanTx userInfo $ irToRootFieldPlan planVals preparedQuery
  Tracing.interpTraceT id $ hoist (runQueryTx $ _pscExecCtx sourceConfig) action

pgDBQueryExplain
  :: forall pgKind m
   . ( MonadError QErr m
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     , MonadIO m
     )
  => G.Name
  -> UserInfo
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> QueryDB ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> m EncJSON
pgDBQueryExplain fieldName userInfo sourceName sourceConfig qrf = do
  preparedQuery <- traverseQueryDB (resolveUnpreparedValue userInfo) qrf
  let PreparedSql querySQL _ = irToRootFieldPlan mempty preparedQuery
      textSQL = Q.getQueryText querySQL
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      withExplain = "EXPLAIN (FORMAT TEXT) " <> textSQL
  let action = liftTx $
        Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True <&> \planList ->
          encJFromJValue $ ExplainPlan fieldName (Just textSQL) (Just $ map runIdentity planList)
  runQueryTx (_pscExecCtx sourceConfig) action

runPGSubscription
  :: ( MonadIO m
     )
  => SourceConfig ('Postgres pgKind)
  -> MultiplexedQuery ('Postgres pgKind)
  -> [(CohortId, CohortVariables)]
  -> m (DiffTime, Either QErr [(CohortId, B.ByteString)])
runPGSubscription sourceConfig query variables = withElapsedTime
  $ runExceptT
  $ runQueryTx (_pscExecCtx sourceConfig)
  $ PGL.executeMultiplexedQuery query variables

pgDBLiveQueryExplain
  :: ( MonadError QErr m
     , MonadIO m
     , MT.MonadBaseControl IO m
     )
  => LiveQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind))
  -> m LiveQueryPlanExplanation
pgDBLiveQueryExplain plan = do
  let parameterizedPlan = _lqpParameterizedPlan plan
      pgExecCtx = _pscExecCtx $ _lqpSourceConfig plan
      queryText = Q.getQueryText . PGL.unMultiplexedQuery $ _plqpQuery parameterizedPlan
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      explainQuery = Q.fromText $ "EXPLAIN (FORMAT TEXT) " <> queryText
  cohortId <- newCohortId
  explanationLines <- liftEitherM $ runExceptT $ runLazyTx pgExecCtx Q.ReadOnly $
                      map runIdentity <$> PGL.executeQuery explainQuery [(cohortId, _lqpVariables plan)]
  pure $ LiveQueryPlanExplanation queryText explanationLines $ _lqpVariables plan


-- mutation

convertDelete
  :: forall pgKind m
   . ( MonadError QErr m
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => UserInfo
  -> IR.AnnDelG ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertDelete userInfo deleteOperation stringifyNum = do
  let (preparedDelete, expectedVariables) =
        flip runState Set.empty $ IR.traverseAnnDel @('Postgres pgKind) prepareWithoutPlan deleteOperation
  validateSessionVariables expectedVariables $ _uiSession userInfo
  pure $ PGE.execDeleteQuery stringifyNum userInfo (preparedDelete, Seq.empty)

convertUpdate
  :: forall pgKind m
   . ( MonadError QErr m
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => UserInfo
  -> IR.AnnUpdG ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertUpdate userInfo updateOperation stringifyNum = do
  let (preparedUpdate, expectedVariables) = flip runState Set.empty $ IR.traverseAnnUpd prepareWithoutPlan updateOperation
  if null $ IR.uqp1OpExps updateOperation
  then pure $ pure $ IR.buildEmptyMutResp $ IR.uqp1Output preparedUpdate
  else do
    validateSessionVariables expectedVariables $ _uiSession userInfo
    pure $ PGE.execUpdateQuery stringifyNum userInfo (preparedUpdate, Seq.empty)

convertInsert
  :: forall pgKind m
   . ( MonadError QErr m
     , HasVersion
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => UserInfo
  -> IR.AnnInsert ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertInsert userInfo insertOperation stringifyNum = do
  let (preparedInsert, expectedVariables) = flip runState Set.empty $ traverseAnnInsert prepareWithoutPlan insertOperation
  validateSessionVariables expectedVariables $ _uiSession userInfo
  pure $ convertToSQLTransaction preparedInsert userInfo Seq.empty stringifyNum

-- | A pared-down version of 'Query.convertQuerySelSet', for use in execution of
-- special case of SQL function mutations (see 'MDBFunction').
convertFunction
  :: forall pgKind m
   . ( MonadError QErr m
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => UserInfo
  -> JsonAggSelect
  -> IR.AnnSimpleSelG ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -- ^ VOLATILE function as 'SelectExp'
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertFunction userInfo jsonAggSelect unpreparedQuery = do
  -- Transform the RQL AST into a prepared SQL query
  (preparedQuery, PlanningSt _ _ planVals expectedVariables)
    <- flip runStateT initPlanningSt
       $ IR.traverseAnnSimpleSelect prepareWithPlan unpreparedQuery
  validateSessionVariables expectedVariables $ _uiSession userInfo
  let queryResultFn =
        case jsonAggSelect of
          JASMultipleRows -> QDBMultipleRows
          JASSingleObject -> QDBSingleRow
  pure $!
    fst $ -- forget (Maybe PreparedSql)
      mkCurPlanTx userInfo $
        irToRootFieldPlan planVals $ queryResultFn preparedQuery

pgDBMutationPlan
  :: forall pgKind m
   . ( MonadError QErr m
     , HasVersion
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     , MonadTrace m
     , MonadIO m
     )
  => RequestId
  -> L.Logger L.Hasura
  -> UserInfo
  -> Bool
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> MutationDB ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> m EncJSON
pgDBMutationPlan requestId logger userInfo stringifyNum sourceName sourceConfig mrf = do
  tx <- pgMutationRootFieldTransaction userInfo stringifyNum mrf
  runMutationTx userInfo sourceConfig tx

runMutationTx
  :: (MonadTrace m, MonadError QErr m, MonadIO m)
  => UserInfo -> PGSourceConfig -> TraceT (LazyTxT QErr IO) b -> m b
runMutationTx userInfo sourceConfig tx = do
  ctx <- Tracing.currentContext
  Tracing.interpTraceT (
    liftEitherM . liftIO . runExceptT
    . runLazyTx (_pscExecCtx sourceConfig) Q.ReadWrite
    . withTraceContext ctx
    . withUserInfo userInfo
    ) tx

pgMutationRootFieldTransaction
  :: forall pgKind m
   . ( MonadError QErr m
     , HasVersion
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => UserInfo
  -> Bool
  -> MutationDB ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> m (TraceT (LazyTxT QErr IO) EncJSON)
pgMutationRootFieldTransaction userInfo stringifyNum mrf = do
    case mrf of
      MDBInsert s              -> convertInsert userInfo s stringifyNum
      MDBUpdate s              -> convertUpdate userInfo s stringifyNum
      MDBDelete s              -> convertDelete userInfo s stringifyNum
      MDBFunction returnsSet s -> convertFunction userInfo returnsSet s

pgDBSubscriptionPlan
  :: forall pgKind m
   . ( MonadError QErr m
     , MonadIO m
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => UserInfo
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> InsOrdHashMap G.Name (QueryDB ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind)))
  -> m (LiveQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)))
pgDBSubscriptionPlan userInfo _sourceName sourceConfig unpreparedAST = do
  (preparedAST, PGL.QueryParametersInfo{..}) <- flip runStateT mempty $
    for unpreparedAST $ traverseQueryDB (PGL.resolveMultiplexedValue $ _uiSession userInfo)
  let multiplexedQuery = PGL.mkMultiplexedQuery preparedAST
      roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedLiveQueryPlan roleName multiplexedQuery

  -- We need to ensure that the values provided for variables are correct according to Postgres.
  -- Without this check an invalid value for a variable for one instance of the subscription will
  -- take down the entire multiplexed query.
  validatedQueryVars     <- PGL.validateVariables @pgKind (_pscExecCtx sourceConfig) _qpiReusableVariableValues
  validatedSyntheticVars <- PGL.validateVariables @pgKind (_pscExecCtx sourceConfig) $ toList _qpiSyntheticVariableValues

  -- TODO validatedQueryVars validatedSyntheticVars
  let cohortVariables = mkCohortVariables
        _qpiReferencedSessionVariables
        (_uiSession userInfo)
        validatedQueryVars
        validatedSyntheticVars

  pure $ LiveQueryPlan parameterizedPlan sourceConfig cohortVariables

-- turn the current plan into a transaction
mkCurPlanTx
  :: UserInfo
  -> PreparedSql
  -> (Tracing.TraceT (LazyTxT QErr IO) EncJSON, Maybe PreparedSql)
mkCurPlanTx userInfo ps@(PreparedSql q prepMap) =
  -- generate the SQL and prepared vars or the bytestring
  let args = withUserVars (_uiSession userInfo) prepMap
      -- WARNING: this quietly assumes the intmap keys are contiguous
      prepArgs = fst <$> IntMap.elems args
  in (, Just ps) $
      Tracing.trace "Postgres" $ liftTx $ DS.asSingleRowJsonResp q prepArgs

-- convert a query from an intermediate representation to... another
irToRootFieldPlan
  :: ( Backend ('Postgres pgKind)
     , DS.PostgresAnnotatedFieldJSON pgKind
     )
  => PrepArgMap
  -> QueryDB ('Postgres pgKind) S.SQLExp
  -> PreparedSql
irToRootFieldPlan prepped = \case
  QDBMultipleRows s -> mkPreparedSql (DS.selectQuerySQL JASMultipleRows) s
  QDBSingleRow s    -> mkPreparedSql (DS.selectQuerySQL JASSingleObject) s
  QDBAggregation s  -> mkPreparedSql DS.selectAggregateQuerySQL s
  QDBConnection s   -> mkPreparedSql DS.connectionSelectQuerySQL s
  where
    mkPreparedSql ::  (t -> Q.Query) -> t -> PreparedSql
    mkPreparedSql f simpleSel =
      PreparedSql (f simpleSel) prepped

-- ad-hoc transaction optimisation
-- see Note [Backwards-compatible transaction optimisation]

runPGMutationTransaction
  :: ( MonadIO m
     , MonadError QErr m
     , HasVersion
     , MonadTrace m
     )
  => RequestId
  -> L.Logger L.Hasura
  -> UserInfo
  -> SourceConfig ('Postgres pgKind)
  -> Bool
  -> InsOrdHashMap G.Name (MutationDBRoot UnpreparedValue ('Postgres 'Vanilla))
  -> m EncJSON
runPGMutationTransaction reqId logger userInfo sourceConfig stringifyNum mutations = do
  -- logQueryLog logger $ mkQueryLog query $$(G.litName "transaction") Nothing reqId
  txs <- for mutations \(MDBR rf) ->
            -- trace ("Postgres Mutation for root field " <>> fieldName) $
    pgMutationRootFieldTransaction userInfo stringifyNum rf

  encJFromInsOrdHashMap . OMap.mapKeys G.unName <$>
    runMutationTx userInfo sourceConfig (sequence txs)
