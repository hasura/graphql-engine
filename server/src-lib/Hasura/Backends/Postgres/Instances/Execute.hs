{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Instances.Execute () where

import           Hasura.Prelude

import qualified Control.Monad.Trans.Control                as MT
import qualified Data.Environment                           as Env
import qualified Data.HashSet                               as Set
import qualified Data.Sequence                              as Seq
import qualified Database.PG.Query                          as Q
import qualified Language.GraphQL.Draft.Syntax              as G
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Types                         as HTTP

import qualified Hasura.Backends.Postgres.Execute.LiveQuery as PGL
import qualified Hasura.Backends.Postgres.Execute.Mutation  as PGE
import qualified Hasura.RQL.IR.Delete                       as IR
import qualified Hasura.RQL.IR.Insert                       as IR
import qualified Hasura.RQL.IR.Returning                    as IR
import qualified Hasura.RQL.IR.Select                       as IR
import qualified Hasura.RQL.IR.Update                       as IR
import qualified Hasura.SQL.AnyBackend                      as AB
import qualified Hasura.Tracing                             as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.Common
import           Hasura.GraphQL.Execute.Insert
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser
import           Hasura.RQL.DML.Internal                    (dmlTxErrorHandler)
import           Hasura.RQL.IR.RemoteJoin
import           Hasura.RQL.Types
import           Hasura.Server.Version                      (HasVersion)
import           Hasura.Session


instance Backend ('Postgres pgKind) => BackendExecute ('Postgres pgKind) where
  type PreparedQuery    ('Postgres pgKind) = PreparedSql pgKind
  type MultiplexedQuery ('Postgres pgKind) = PGL.MultiplexedQuery
  type ExecutionMonad   ('Postgres pgKind) = Tracing.TraceT (LazyTxT QErr IO)

  getRemoteJoins = pgGetRemoteJoins
  mkDBQueryPlan = pgDBQueryPlan
  mkDBMutationPlan = pgDBMutationPlan
  mkDBSubscriptionPlan = pgDBSubscriptionPlan
  mkDBQueryExplain = pgDBQueryExplain
  mkLiveQueryExplain = pgDBLiveQueryExplain


-- query

pgDBQueryPlan
  :: forall pgKind m
   . ( MonadError QErr m
     , HasVersion
     , Backend ('Postgres pgKind)
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> [G.Directive G.Name]
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> QueryDB ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> m ExecutionStep
pgDBQueryPlan env manager reqHeaders userInfo _directives sourceName sourceConfig qrf = do
  (preparedQuery, PlanningSt _ _ planVals expectedVariables) <- flip runStateT initPlanningSt $ traverseQueryDB @('Postgres pgKind) prepareWithPlan qrf
  validateSessionVariables expectedVariables $ _uiSession userInfo
  let (action, preparedSQL) = mkCurPlanTx env manager reqHeaders userInfo $ irToRootFieldPlan planVals preparedQuery
  pure
    $ ExecStepDB []
    . AB.mkAnyBackend
    $ DBStepInfo @('Postgres pgKind) sourceName sourceConfig preparedSQL action

pgDBQueryExplain
  :: forall pgKind m
   . ( MonadError QErr m
     , Backend ('Postgres pgKind)
     )
  => G.Name
  -> UserInfo
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> QueryDB ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> m (AB.AnyBackend DBStepInfo)
pgDBQueryExplain fieldName userInfo sourceName sourceConfig qrf = do
  preparedQuery <- traverseQueryDB (resolveUnpreparedValue userInfo) qrf
  let PreparedSql querySQL _ remoteJoins = irToRootFieldPlan mempty preparedQuery
      textSQL = Q.getQueryText querySQL
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      withExplain = "EXPLAIN (FORMAT TEXT) " <> textSQL
  -- Reject if query contains any remote joins
  when (remoteJoins /= mempty) $
    throw400 NotSupported "Remote relationships are not allowed in explain query"
  let action = liftTx $
        Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True <&> \planList ->
          encJFromJValue $ ExplainPlan fieldName (Just textSQL) (Just $ map runIdentity planList)
  pure
    $ AB.mkAnyBackend
    $ DBStepInfo @('Postgres pgKind) sourceName sourceConfig Nothing action

pgDBLiveQueryExplain
  :: ( MonadError QErr m
     , MonadIO m
     , MT.MonadBaseControl IO m
     )
  => LiveQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)) -> m LiveQueryPlanExplanation
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
     , HasVersion
     , Backend ('Postgres pgKind)
     )
  => Env.Environment
  -> SessionVariables
  -> PGE.MutationRemoteJoinCtx
  -> IR.AnnDelG ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertDelete env usrVars remoteJoinCtx deleteOperation stringifyNum = do
  let (preparedDelete, expectedVariables) =
        flip runState Set.empty $ IR.traverseAnnDel @('Postgres pgKind) prepareWithoutPlan deleteOperation
  validateSessionVariables expectedVariables usrVars
  pure $ PGE.execDeleteQuery env stringifyNum (Just remoteJoinCtx) (preparedDelete, Seq.empty)

convertUpdate
  :: forall pgKind m
   . ( MonadError QErr m
     , HasVersion
     , Backend ('Postgres pgKind)
     )
  => Env.Environment
  -> SessionVariables
  -> PGE.MutationRemoteJoinCtx
  -> IR.AnnUpdG ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertUpdate env usrVars remoteJoinCtx updateOperation stringifyNum = do
  let (preparedUpdate, expectedVariables) = flip runState Set.empty $ IR.traverseAnnUpd prepareWithoutPlan updateOperation
  if null $ IR.uqp1OpExps updateOperation
  then pure $ pure $ IR.buildEmptyMutResp $ IR.uqp1Output preparedUpdate
  else do
    validateSessionVariables expectedVariables usrVars
    pure $ PGE.execUpdateQuery env stringifyNum (Just remoteJoinCtx) (preparedUpdate, Seq.empty)

convertInsert
  :: forall pgKind m
   . ( MonadError QErr m
     , HasVersion
     , Backend ('Postgres pgKind)
     )
  => Env.Environment
  -> SessionVariables
  -> PGE.MutationRemoteJoinCtx
  -> IR.AnnInsert ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertInsert env usrVars remoteJoinCtx insertOperation stringifyNum = do
  let (preparedInsert, expectedVariables) = flip runState Set.empty $ traverseAnnInsert prepareWithoutPlan insertOperation
  validateSessionVariables expectedVariables usrVars
  pure $ convertToSQLTransaction env preparedInsert remoteJoinCtx Seq.empty stringifyNum

-- | A pared-down version of 'Query.convertQuerySelSet', for use in execution of
-- special case of SQL function mutations (see 'MDBFunction').
convertFunction
  :: forall pgKind m
   . ( MonadError QErr m
     , HasVersion
     , Backend ('Postgres pgKind)
     )
  => Env.Environment
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> JsonAggSelect
  -> IR.AnnSimpleSelG ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -- ^ VOLATILE function as 'SelectExp'
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertFunction env userInfo manager reqHeaders jsonAggSelect unpreparedQuery = do
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
      mkCurPlanTx env manager reqHeaders userInfo $
        irToRootFieldPlan planVals $ queryResultFn preparedQuery

pgDBMutationPlan
  :: forall pgKind m
   . ( MonadError QErr m
     , HasVersion
     , Backend ('Postgres pgKind)
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> Bool
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> MutationDB ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind))
  -> m ExecutionStep
pgDBMutationPlan env manager reqHeaders userInfo stringifyNum sourceName sourceConfig mrf =
    go <$> case mrf of
    MDBInsert s              -> convertInsert env userSession remoteJoinCtx s stringifyNum
    MDBUpdate s              -> convertUpdate env userSession remoteJoinCtx s stringifyNum
    MDBDelete s              -> convertDelete env userSession remoteJoinCtx s stringifyNum
    MDBFunction returnsSet s -> convertFunction env userInfo manager reqHeaders returnsSet s
  where
    userSession = _uiSession userInfo
    remoteJoinCtx = (manager, reqHeaders, userInfo)
    go = ExecStepDB [] . AB.mkAnyBackend . DBStepInfo @('Postgres pgKind) sourceName sourceConfig Nothing



-- subscription

pgDBSubscriptionPlan
  :: forall pgKind m
   . ( MonadError QErr m
     , MonadIO m
     , Backend ('Postgres pgKind)
     )
  => UserInfo
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> InsOrdHashMap G.Name (QueryDB ('Postgres pgKind) (UnpreparedValue ('Postgres pgKind)))
  -> m (LiveQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)))
pgDBSubscriptionPlan userInfo _sourceName sourceConfig unpreparedAST = do
  (preparedAST, PGL.QueryParametersInfo{..}) <- flip runStateT mempty $
    for unpreparedAST $ traverseQueryDB PGL.resolveMultiplexedValue
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



-- helpers

pgGetRemoteJoins :: PreparedSql pgKind -> [RemoteJoin ('Postgres pgKind)]
pgGetRemoteJoins = concatMap (toList . snd) . maybe [] toList . _psRemoteJoins
