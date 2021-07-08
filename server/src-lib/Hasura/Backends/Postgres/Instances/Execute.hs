{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Instances.Execute
  ( PreparedSql(..)
  ) where

import           Hasura.Prelude

import qualified Control.Monad.Trans.Control                as MT
import qualified Data.HashSet                               as Set
import qualified Data.IntMap                                as IntMap
import qualified Data.Sequence                              as Seq
import qualified Database.PG.Query                          as Q
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
import qualified Hasura.SQL.AnyBackend                      as AB
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
import           Hasura.Server.Version                      (HasVersion)
import           Hasura.Session


data PreparedSql
  = PreparedSql
  { _psQuery    :: !Q.Query
  , _psPrepArgs :: !PrepArgMap
  }

instance
  ( Backend ('Postgres pgKind)
  ,  PostgresAnnotatedFieldJSON pgKind
  ) => BackendExecute ('Postgres pgKind) where

  type PreparedQuery    ('Postgres pgKind) = PreparedSql
  type MultiplexedQuery ('Postgres pgKind) = PGL.MultiplexedQuery
  type ExecutionMonad   ('Postgres pgKind) = Tracing.TraceT (LazyTxT QErr IO)

  mkDBQueryPlan = pgDBQueryPlan
  mkDBMutationPlan = pgDBMutationPlan
  mkDBSubscriptionPlan = pgDBSubscriptionPlan
  mkDBQueryExplain = pgDBQueryExplain
  mkLiveQueryExplain = pgDBLiveQueryExplain


-- query

pgDBQueryPlan
  :: forall pgKind m
   . ( MonadError QErr m
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => UserInfo
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> QueryDB ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind))
  -> m (DBStepInfo ('Postgres pgKind))
pgDBQueryPlan userInfo sourceName sourceConfig qrf = do
  (preparedQuery, PlanningSt _ _ planVals expectedVariables) <-
    flip runStateT initPlanningSt $ traverse prepareWithPlan qrf
  validateSessionVariables expectedVariables $ _uiSession userInfo
  let (action, preparedSQL) = mkCurPlanTx userInfo $ irToRootFieldPlan planVals preparedQuery
  pure $ DBStepInfo @('Postgres pgKind) sourceName sourceConfig preparedSQL action

pgDBQueryExplain
  :: forall pgKind m
   . ( MonadError QErr m
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => G.Name
  -> UserInfo
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> QueryDB ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind))
  -> m (AB.AnyBackend DBStepInfo)
pgDBQueryExplain fieldName userInfo sourceName sourceConfig qrf = do
  preparedQuery <- traverse (resolveUnpreparedValue userInfo) qrf
  let PreparedSql querySQL _ = irToRootFieldPlan mempty preparedQuery
      textSQL = Q.getQueryText querySQL
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      withExplain = "EXPLAIN (FORMAT TEXT) " <> textSQL
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
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => UserInfo
  -> IR.AnnDelG ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind))
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertDelete userInfo deleteOperation stringifyNum = do
  let (preparedDelete, expectedVariables) =
        flip runState Set.empty $ traverse prepareWithoutPlan deleteOperation
  validateSessionVariables expectedVariables $ _uiSession userInfo
  pure $ PGE.execDeleteQuery stringifyNum userInfo (preparedDelete, Seq.empty)

convertUpdate
  :: forall pgKind m
   . ( MonadError QErr m
     , Backend ('Postgres pgKind)
     , PostgresAnnotatedFieldJSON pgKind
     )
  => UserInfo
  -> IR.AnnUpdG ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind))
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertUpdate userInfo updateOperation stringifyNum = do
  let (preparedUpdate, expectedVariables) = flip runState Set.empty $ traverse prepareWithoutPlan updateOperation
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
  -> IR.AnnInsert ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind))
  -> Bool
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertInsert userInfo insertOperation stringifyNum = do
  let (preparedInsert, expectedVariables) = flip runState Set.empty $ traverse prepareWithoutPlan insertOperation
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
  -> IR.AnnSimpleSelectG ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind))
  -- ^ VOLATILE function as 'SelectExp'
  -> m (Tracing.TraceT (LazyTxT QErr IO) EncJSON)
convertFunction userInfo jsonAggSelect unpreparedQuery = do
  -- Transform the RQL AST into a prepared SQL query
  (preparedQuery, PlanningSt _ _ planVals expectedVariables)
    <- flip runStateT initPlanningSt
       $ traverse prepareWithPlan unpreparedQuery
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
     )
  => UserInfo
  -> Bool
  -> SourceName
  -> SourceConfig ('Postgres pgKind)
  -> MutationDB ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind))
  -> m (DBStepInfo ('Postgres pgKind))
pgDBMutationPlan userInfo stringifyNum sourceName sourceConfig mrf =
  go <$> case mrf of
    MDBInsert s              -> convertInsert userInfo s stringifyNum
    MDBUpdate s              -> convertUpdate userInfo s stringifyNum
    MDBDelete s              -> convertDelete userInfo s stringifyNum
    MDBFunction returnsSet s -> convertFunction userInfo returnsSet s
  where
    go v = DBStepInfo @('Postgres pgKind) sourceName sourceConfig Nothing v


-- subscription

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
  -> InsOrdHashMap G.Name (QueryDB ('Postgres pgKind) (Const Void) (UnpreparedValue ('Postgres pgKind)))
  -> m (LiveQueryPlan ('Postgres pgKind) (MultiplexedQuery ('Postgres pgKind)))
pgDBSubscriptionPlan userInfo _sourceName sourceConfig unpreparedAST = do
  (preparedAST, PGL.QueryParametersInfo{..}) <- flip runStateT mempty $
    for unpreparedAST $ traverse (PGL.resolveMultiplexedValue $ _uiSession userInfo)
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
  -> QueryDB ('Postgres pgKind) (Const Void) S.SQLExp
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
