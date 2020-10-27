module Hasura.GraphQL.Execute.Mutation
  ( convertMutationSelectionSet
  ) where

import           Hasura.Prelude

import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as Set
import qualified Data.Sequence                          as Seq
import qualified Data.Sequence.NonEmpty                 as NE
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.RQL.DML.Delete                  as RQL
import qualified Hasura.RQL.DML.Mutation                as RQL
import qualified Hasura.RQL.DML.Returning.Types         as RQL
import qualified Hasura.RQL.DML.Update                  as RQL
import qualified Hasura.Tracing                         as Tracing
import qualified Hasura.Logging                         as L


import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Insert
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Insert
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session

convertDelete
  :: ( HasVersion
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx)
  => Env.Environment
  -> SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> RQL.AnnDelG 'Postgres UnpreparedValue
  -> Bool
  -> m (tx EncJSON)
convertDelete env usrVars remoteJoinCtx deleteOperation stringifyNum = do
  let (preparedDelete, expectedVariables) = flip runState Set.empty $ RQL.traverseAnnDel prepareWithoutPlan deleteOperation
  validateSessionVariables expectedVariables usrVars
  pure $ RQL.execDeleteQuery env stringifyNum (Just remoteJoinCtx) (preparedDelete, Seq.empty)

convertUpdate
  :: ( HasVersion
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx
     )
  => Env.Environment
  -> SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> RQL.AnnUpdG 'Postgres UnpreparedValue
  -> Bool
  -> m (tx EncJSON)
convertUpdate env usrVars remoteJoinCtx updateOperation stringifyNum = do
  let (preparedUpdate, expectedVariables) = flip runState Set.empty $ RQL.traverseAnnUpd prepareWithoutPlan updateOperation
  if null $ RQL.uqp1OpExps updateOperation
  then pure $ pure $ RQL.buildEmptyMutResp $ RQL.uqp1Output preparedUpdate
  else do
    validateSessionVariables expectedVariables usrVars
    pure $ RQL.execUpdateQuery env stringifyNum (Just remoteJoinCtx) (preparedUpdate, Seq.empty)

convertInsert
  :: ( HasVersion
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx)
  => Env.Environment
  -> SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> AnnInsert 'Postgres UnpreparedValue
  -> Bool
  -> m (tx EncJSON)
convertInsert env usrVars remoteJoinCtx insertOperation stringifyNum = do
  let (preparedInsert, expectedVariables) = flip runState Set.empty $ traverseAnnInsert prepareWithoutPlan insertOperation
  validateSessionVariables expectedVariables usrVars
  pure $ convertToSQLTransaction env preparedInsert remoteJoinCtx Seq.empty stringifyNum

convertMutationDB
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , Tracing.MonadTrace tx
     , MonadIO tx
     , MonadTx tx
     )
  => Env.Environment
  -> SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> Bool
  -> MutationDB 'Postgres UnpreparedValue
  -> m (tx EncJSON, HTTP.ResponseHeaders)
convertMutationDB env userSession remoteJoinCtx stringifyNum = \case
  MDBInsert s -> noResponseHeaders <$> convertInsert env userSession remoteJoinCtx s stringifyNum
  MDBUpdate s -> noResponseHeaders <$> convertUpdate env userSession remoteJoinCtx s stringifyNum
  MDBDelete s -> noResponseHeaders <$> convertDelete env userSession remoteJoinCtx s stringifyNum

noResponseHeaders :: tx EncJSON -> (tx EncJSON, HTTP.ResponseHeaders)
noResponseHeaders rTx = (rTx, [])

convertMutationAction
  ::( HasVersion
    , MonadIO m
    , MonadError QErr m
    , Tracing.MonadTrace m
    , Tracing.MonadTrace tx
    , MonadIO tx
    , MonadTx tx
    )
  => Env.Environment
  -> L.Logger L.Hasura
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> ActionMutation 'Postgres UnpreparedValue
  -> m (tx EncJSON, HTTP.ResponseHeaders)
convertMutationAction env logger userInfo manager reqHeaders = \case
  AMSync s  -> (_aerTransaction &&& _aerHeaders) <$>
    resolveActionExecution env logger userInfo s actionExecContext
  AMAsync s -> noResponseHeaders <$> resolveActionMutationAsync s reqHeaders userSession
  where
    userSession = _uiSession userInfo
    actionExecContext = ActionExecContext manager reqHeaders $ _uiSession userInfo

convertMutationSelectionSet
  :: forall m tx
   . ( HasVersion
     , Tracing.MonadTrace m
     , MonadIO m
     , MonadError QErr m
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
  -> m (ExecutionPlan (tx EncJSON, HTTP.ResponseHeaders))
convertMutationSelectionSet env logger gqlContext sqlGenCtx userInfo manager reqHeaders fields varDefs varValsM = do
  mutationParser <- onNothing (gqlMutationParser gqlContext) $
    throw400 ValidationFailed "no mutations exist"
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability)
    :: (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue), QueryReusability)
    <-  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
    >>= (mutationParser >>> (`onLeft` reportParseErrors))

  -- Transform the RQL AST into a prepared SQL query
  let userSession = _uiSession userInfo
      remoteJoinCtx = (manager, reqHeaders, userInfo)
  txs <- for unpreparedQueries \case
    RFDB db             -> ExecStepDB <$> convertMutationDB env userSession remoteJoinCtx (stringifyNum sqlGenCtx) db
    RFRemote (remoteSchemaInfo, remoteField) ->
      pure $ buildExecStepRemote
             remoteSchemaInfo
             G.OperationTypeMutation
             varDefs
             [G.SelectionField remoteField]
             varValsM
    RFAction action     -> ExecStepDB <$> convertMutationAction env logger userInfo manager reqHeaders action
    RFRaw s             -> pure $ ExecStepRaw s

  return txs
  where
    reportParseErrors errs = case NE.head errs of
      -- TODO: Our error reporting machinery doesn’t currently support reporting
      -- multiple errors at once, so we’re throwing away all but the first one
      -- here. It would be nice to report all of them!
      ParseError{ pePath, peMessage, peCode } ->
        throwError (err400 peCode peMessage){ qePath = pePath }
