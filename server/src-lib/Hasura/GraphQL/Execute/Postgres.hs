{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.GraphQL.Execute.Postgres () where

import           Hasura.Prelude

import qualified Data.Environment                          as Env
import qualified Data.HashSet                              as Set
import qualified Data.Sequence                             as Seq
import qualified Language.GraphQL.Draft.Syntax             as G
import qualified Network.HTTP.Client                       as HTTP
import qualified Network.HTTP.Types                        as HTTP

import qualified Hasura.Backends.Postgres.Execute.Mutation as PGE
import qualified Hasura.RQL.IR.Delete                      as IR
import qualified Hasura.RQL.IR.Insert                      as IR
import qualified Hasura.RQL.IR.Returning                   as IR
import qualified Hasura.RQL.IR.Select                      as IR
import qualified Hasura.RQL.IR.Update                      as IR
import qualified Hasura.Tracing                            as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.Common
import           Hasura.GraphQL.Execute.Insert
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser
import           Hasura.RQL.Types
import           Hasura.Server.Version                     (HasVersion)
import           Hasura.Session


instance
  ( MonadIO tx
  , MonadTx tx
  , Tracing.MonadTrace tx
  ) => BackendExecute 'Postgres tx where
  type PreparedQuery 'Postgres = PreparedSql
  getRemoteJoins = concatMap (toList . snd) . maybe [] toList . _psRemoteJoins

  mkDBQueryPlan = pgDBQueryPlan
  mkDBMutationPlan = pgDBMutationPlan


-- query

pgDBQueryPlan
  :: forall m tx .
     ( MonadError QErr m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     , HasVersion
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> [G.Directive G.Name]
  -> SourceConfig 'Postgres
  -> QueryDB 'Postgres (UnpreparedValue 'Postgres)
  -> m (ExecutionStep tx)
pgDBQueryPlan env manager reqHeaders userInfo _directives sourceConfig qrf = do
  (preparedQuery, PlanningSt _ _ planVals expectedVariables) <- flip runStateT initPlanningSt $ traverseQueryDB prepareWithPlan qrf
  validateSessionVariables expectedVariables $ _uiSession userInfo
  let (action, preparedSQL) = mkCurPlanTx env manager reqHeaders userInfo $ irToRootFieldPlan planVals preparedQuery
  pure $ ExecStepDB sourceConfig preparedSQL [] action


-- mutation

convertDelete
  :: forall m tx .
     ( MonadError QErr m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     , HasVersion
     )
  => Env.Environment
  -> SessionVariables
  -> PGE.MutationRemoteJoinCtx
  -> IR.AnnDelG 'Postgres (UnpreparedValue 'Postgres)
  -> Bool
  -> m (tx EncJSON)
convertDelete env usrVars remoteJoinCtx deleteOperation stringifyNum = do
  let (preparedDelete, expectedVariables) = flip runState Set.empty $ IR.traverseAnnDel prepareWithoutPlan deleteOperation
  validateSessionVariables expectedVariables usrVars
  pure $ PGE.execDeleteQuery env stringifyNum (Just remoteJoinCtx) (preparedDelete, Seq.empty)

convertUpdate
  :: forall m tx .
     ( MonadError QErr m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     , HasVersion
     )
  => Env.Environment
  -> SessionVariables
  -> PGE.MutationRemoteJoinCtx
  -> IR.AnnUpdG 'Postgres (UnpreparedValue 'Postgres)
  -> Bool
  -> m (tx EncJSON)
convertUpdate env usrVars remoteJoinCtx updateOperation stringifyNum = do
  let (preparedUpdate, expectedVariables) = flip runState Set.empty $ IR.traverseAnnUpd prepareWithoutPlan updateOperation
  if null $ IR.uqp1OpExps updateOperation
  then pure $ pure $ IR.buildEmptyMutResp $ IR.uqp1Output preparedUpdate
  else do
    validateSessionVariables expectedVariables usrVars
    pure $ PGE.execUpdateQuery env stringifyNum (Just remoteJoinCtx) (preparedUpdate, Seq.empty)

convertInsert
  :: forall m tx .
     ( MonadError QErr m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     , HasVersion
     )
  => Env.Environment
  -> SessionVariables
  -> PGE.MutationRemoteJoinCtx
  -> IR.AnnInsert 'Postgres (UnpreparedValue 'Postgres)
  -> Bool
  -> m (tx EncJSON)
convertInsert env usrVars remoteJoinCtx insertOperation stringifyNum = do
  let (preparedInsert, expectedVariables) = flip runState Set.empty $ traverseAnnInsert prepareWithoutPlan insertOperation
  validateSessionVariables expectedVariables usrVars
  pure $ convertToSQLTransaction env preparedInsert remoteJoinCtx Seq.empty stringifyNum

-- | A pared-down version of 'Query.convertQuerySelSet', for use in execution of
-- special case of SQL function mutations (see 'MDBFunction').
convertFunction
  :: forall m tx .
     ( MonadError QErr m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     , HasVersion
     )
  => Env.Environment
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> JsonAggSelect
  -> IR.AnnSimpleSelG 'Postgres (UnpreparedValue 'Postgres)
  -- ^ VOLATILE function as 'SelectExp'
  -> m (tx EncJSON)
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
  :: forall m tx .
     ( MonadError QErr m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     , HasVersion
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> Bool
  -> SourceConfig 'Postgres
  -> MutationDB 'Postgres (UnpreparedValue 'Postgres)
  -> m (ExecutionStep tx)
pgDBMutationPlan env manager reqHeaders userInfo stringifyNum sourceConfig mrf =
  ExecStepDB sourceConfig Nothing [] <$> case mrf of
    MDBInsert s              -> convertInsert env userSession remoteJoinCtx s stringifyNum
    MDBUpdate s              -> convertUpdate env userSession remoteJoinCtx s stringifyNum
    MDBDelete s              -> convertDelete env userSession remoteJoinCtx s stringifyNum
    MDBFunction returnsSet s -> convertFunction env userInfo manager reqHeaders returnsSet s
  where
    userSession = _uiSession userInfo
    remoteJoinCtx = (manager, reqHeaders, userInfo)
