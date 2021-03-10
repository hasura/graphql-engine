{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.Execute () where

import           Hasura.Prelude

import qualified Data.Environment                           as Env
import qualified Data.HashSet                               as Set
import qualified Data.Sequence                              as Seq
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
import           Hasura.RQL.Types
import           Hasura.Server.Version                      (HasVersion)
import           Hasura.Session


instance BackendExecute 'Postgres where
  type PreparedQuery    'Postgres = PreparedSql
  type MultiplexedQuery 'Postgres = PGL.MultiplexedQuery
  type ExecutionMonad   'Postgres = Tracing.TraceT (LazyTxT QErr IO)
  getRemoteJoins = concatMap (toList . snd) . maybe [] toList . _psRemoteJoins

  mkDBQueryPlan = pgDBQueryPlan
  mkDBMutationPlan = pgDBMutationPlan
  mkDBSubscriptionPlan = pgDBSubscriptionPlan


-- query

pgDBQueryPlan
  :: forall m .
     ( MonadError QErr m
     , HasVersion
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> [G.Directive G.Name]
  -> SourceConfig 'Postgres
  -> QueryDB 'Postgres (UnpreparedValue 'Postgres)
  -> m ExecutionStep
pgDBQueryPlan env manager reqHeaders userInfo _directives sourceConfig qrf = do
  (preparedQuery, PlanningSt _ _ planVals expectedVariables) <- flip runStateT initPlanningSt $ traverseQueryDB prepareWithPlan qrf
  validateSessionVariables expectedVariables $ _uiSession userInfo
  let (action, preparedSQL) = mkCurPlanTx env manager reqHeaders userInfo $ irToRootFieldPlan planVals preparedQuery
  pure $ ExecStepDB sourceConfig preparedSQL [] action


-- mutation

convertDelete
  :: forall m .
     ( MonadError QErr m
     , HasVersion
     )
  => Env.Environment
  -> SessionVariables
  -> PGE.MutationRemoteJoinCtx
  -> IR.AnnDelG 'Postgres (UnpreparedValue 'Postgres)
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertDelete env usrVars remoteJoinCtx deleteOperation stringifyNum = do
  let (preparedDelete, expectedVariables) = flip runState Set.empty $ IR.traverseAnnDel prepareWithoutPlan deleteOperation
  validateSessionVariables expectedVariables usrVars
  pure $ PGE.execDeleteQuery env stringifyNum (Just remoteJoinCtx) (preparedDelete, Seq.empty)

convertUpdate
  :: forall m.
     ( MonadError QErr m
     , HasVersion
     )
  => Env.Environment
  -> SessionVariables
  -> PGE.MutationRemoteJoinCtx
  -> IR.AnnUpdG 'Postgres (UnpreparedValue 'Postgres)
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
  :: forall m.
     ( MonadError QErr m
     , HasVersion
     )
  => Env.Environment
  -> SessionVariables
  -> PGE.MutationRemoteJoinCtx
  -> IR.AnnInsert 'Postgres (UnpreparedValue 'Postgres)
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertInsert env usrVars remoteJoinCtx insertOperation stringifyNum = do
  let (preparedInsert, expectedVariables) = flip runState Set.empty $ traverseAnnInsert prepareWithoutPlan insertOperation
  validateSessionVariables expectedVariables usrVars
  pure $ convertToSQLTransaction env preparedInsert remoteJoinCtx Seq.empty stringifyNum

-- | A pared-down version of 'Query.convertQuerySelSet', for use in execution of
-- special case of SQL function mutations (see 'MDBFunction').
convertFunction
  :: forall m.
     ( MonadError QErr m
     , HasVersion
     )
  => Env.Environment
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> JsonAggSelect
  -> IR.AnnSimpleSelG 'Postgres (UnpreparedValue 'Postgres)
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
  :: forall m.
     ( MonadError QErr m
     , HasVersion
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> Bool
  -> SourceConfig 'Postgres
  -> MutationDB 'Postgres (UnpreparedValue 'Postgres)
  -> m ExecutionStep
pgDBMutationPlan env manager reqHeaders userInfo stringifyNum sourceConfig mrf =
  ExecStepDB sourceConfig Nothing [] <$> case mrf of
    MDBInsert s              -> convertInsert env userSession remoteJoinCtx s stringifyNum
    MDBUpdate s              -> convertUpdate env userSession remoteJoinCtx s stringifyNum
    MDBDelete s              -> convertDelete env userSession remoteJoinCtx s stringifyNum
    MDBFunction returnsSet s -> convertFunction env userInfo manager reqHeaders returnsSet s
  where
    userSession = _uiSession userInfo
    remoteJoinCtx = (manager, reqHeaders, userInfo)


-- subscription

pgDBSubscriptionPlan
  :: forall m.
     ( MonadError QErr m
     , MonadIO m
     )
  => UserInfo
  -> SourceConfig 'Postgres
  -> InsOrdHashMap G.Name (QueryDB 'Postgres (UnpreparedValue 'Postgres))
  -> m (LiveQueryPlan 'Postgres (MultiplexedQuery 'Postgres))
pgDBSubscriptionPlan userInfo sourceConfig unpreparedAST = do
  (preparedAST, PGL.QueryParametersInfo{..}) <- flip runStateT mempty $
    for unpreparedAST $ traverseQueryDB PGL.resolveMultiplexedValue
  let multiplexedQuery = PGL.mkMultiplexedQuery preparedAST
      roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedLiveQueryPlan roleName multiplexedQuery

  -- We need to ensure that the values provided for variables are correct according to Postgres.
  -- Without this check an invalid value for a variable for one instance of the subscription will
  -- take down the entire multiplexed query.
  validatedQueryVars     <- PGL.validateVariables (_pscExecCtx sourceConfig) _qpiReusableVariableValues
  validatedSyntheticVars <- PGL.validateVariables (_pscExecCtx sourceConfig) $ toList _qpiSyntheticVariableValues

  -- TODO validatedQueryVars validatedSyntheticVars
  let cohortVariables = mkCohortVariables
        _qpiReferencedSessionVariables
        (_uiSession userInfo)
        validatedQueryVars
        validatedSyntheticVars

  pure $ LiveQueryPlan parameterizedPlan sourceConfig cohortVariables
