module Hasura.GraphQL.Execute.Mutation
  ( convertMutationSelectionSet
  ) where

import           Hasura.Prelude

import qualified Data.Environment                          as Env
import qualified Data.HashMap.Strict                       as Map
import qualified Data.HashMap.Strict.InsOrd                as OMap
import qualified Data.HashSet                              as Set
import qualified Data.Sequence                             as Seq
import qualified Data.Sequence.NonEmpty                    as NE
import qualified Language.GraphQL.Draft.Syntax             as G
import qualified Network.HTTP.Client                       as HTTP
import qualified Network.HTTP.Types                        as HTTP

import qualified Hasura.Backends.Postgres.Execute.Mutation as PGE
import qualified Hasura.GraphQL.Transport.HTTP.Protocol    as GH
import qualified Hasura.Logging                            as L
import qualified Hasura.RQL.IR.Delete                      as IR
import qualified Hasura.RQL.IR.Insert                      as IR
import qualified Hasura.RQL.IR.Returning                   as IR
import qualified Hasura.RQL.IR.Select                      as IR
import qualified Hasura.RQL.IR.Update                      as IR
import qualified Hasura.Tracing                            as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Common
import           Hasura.GraphQL.Execute.Insert
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Version                     (HasVersion)
import           Hasura.Session


convertDelete
  :: ( HasVersion
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx)
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
  :: ( HasVersion
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx
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
  :: ( HasVersion
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx)
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


noResponseHeaders :: tx EncJSON -> (tx EncJSON, HTTP.ResponseHeaders)
noResponseHeaders rTx = (rTx, [])

convertMutationAction
  ::( HasVersion
    , MonadIO m
    , MonadError QErr m
    , MonadMetadataStorage (MetadataStorageT m)
    , Tracing.MonadTrace m
    )
  => Env.Environment
  -> L.Logger L.Hasura
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> ActionMutation 'Postgres (UnpreparedValue 'Postgres)
  -> m (ActionExecutionPlan, HTTP.ResponseHeaders)
convertMutationAction env logger userInfo manager reqHeaders = \case
  AMSync s  -> ((AEPSync . _aerExecution) &&& _aerHeaders) <$>
    resolveActionExecution env logger userInfo s actionExecContext
  AMAsync s -> do
    result <- liftEitherM (runMetadataStorageT $ resolveActionMutationAsync s reqHeaders userSession)
    pure (AEPAsyncMutation result, [])
  where
    userSession = _uiSession userInfo
    actionExecContext = ActionExecContext manager reqHeaders $ _uiSession userInfo

convertMutationSelectionSet
  :: forall m tx
   . ( HasVersion
     , Tracing.MonadTrace m
     , MonadIO m
     , MonadError QErr m
     , MonadMetadataStorage (MetadataStorageT m)
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> GQLContext
  -> SQLGenCtx
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m (ExecutionPlan (ActionExecutionPlan, HTTP.ResponseHeaders) (tx EncJSON, HTTP.ResponseHeaders))
convertMutationSelectionSet env logger gqlContext SQLGenCtx{stringifyNum} userInfo manager reqHeaders fields varDefs varValsM = do
  mutationParser <- onNothing (gqlMutationParser gqlContext) $
    throw400 ValidationFailed "no mutations exist"
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability)
    :: (OMap.InsOrdHashMap G.Name (MutationRootField (UnpreparedValue 'Postgres)), QueryReusability)
    <-  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
    >>= (mutationParser >>> (`onLeft` reportParseErrors))

  -- Transform the RQL AST into a prepared SQL query
  let userSession = _uiSession userInfo
      remoteJoinCtx = (manager, reqHeaders, userInfo)
  txs <- for unpreparedQueries \case
    RFDB _ execCtx db -> ExecStepDB execCtx . noResponseHeaders <$> case db of
      MDBInsert s   -> convertInsert env userSession remoteJoinCtx s stringifyNum
      MDBUpdate s   -> convertUpdate env userSession remoteJoinCtx s stringifyNum
      MDBDelete s   -> convertDelete env userSession remoteJoinCtx s stringifyNum
      MDBFunction s -> convertFunction env userInfo manager reqHeaders s

    RFRemote remoteField -> do
      RemoteFieldG remoteSchemaInfo resolvedRemoteField <- resolveRemoteField userInfo remoteField
      pure $ buildExecStepRemote
        remoteSchemaInfo
        G.OperationTypeMutation
        $ [G.SelectionField resolvedRemoteField]
    RFAction action     -> ExecStepAction <$> convertMutationAction env logger userInfo manager reqHeaders action
    RFRaw s             -> pure $ ExecStepRaw s

  return txs
  where
    reportParseErrors errs = case NE.head errs of
      -- TODO: Our error reporting machinery doesn’t currently support reporting
      -- multiple errors at once, so we’re throwing away all but the first one
      -- here. It would be nice to report all of them!
      ParseError{ pePath, peMessage, peCode } ->
        throwError (err400 peCode peMessage){ qePath = pePath }

-- | A pared-down version of 'Query.convertQuerySelSet', for use in execution of
-- special case of SQL function mutations (see 'MDBFunction').
convertFunction
  :: forall m tx .
     ( MonadError QErr m
     , HasVersion
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> IR.AnnSimpleSelG 'Postgres (UnpreparedValue 'Postgres)
  -- ^ VOLATILE function as 'SelectExp'
  -> m (tx EncJSON)
convertFunction env userInfo manager reqHeaders unpreparedQuery = do
  -- Transform the RQL AST into a prepared SQL query
  (preparedQuery, PlanningSt _ _ planVals expectedVariables)
    <- flip runStateT initPlanningSt
       $ IR.traverseAnnSimpleSelect prepareWithPlan unpreparedQuery
  validateSessionVariables expectedVariables $ _uiSession userInfo

  pure $!
    fst $ -- forget (Maybe PreparedSql)
      mkCurPlanTx env manager reqHeaders userInfo id noProfile $
        RFPPostgres $ irToRootFieldPlan planVals $ QDBSimple preparedQuery
