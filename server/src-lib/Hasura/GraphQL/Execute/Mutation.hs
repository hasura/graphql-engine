module Hasura.GraphQL.Execute.Mutation
  ( convertMutationSelectionSet
  ) where

import           Hasura.Prelude

import qualified Data.Environment                           as Env
import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashMap.Strict.InsOrd                 as OMap
import qualified Data.Sequence.NonEmpty                     as NE
import qualified Language.GraphQL.Draft.Syntax              as G
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Types                         as HTTP

import qualified Hasura.GraphQL.Transport.HTTP.Protocol     as GH
import qualified Hasura.Logging                             as L
import qualified Hasura.Tracing                             as Tracing

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Version                      (HasVersion)
import           Hasura.Session

-- backend instances
import           Hasura.Backends.MSSQL.Instances.Execute    ()
import           Hasura.Backends.Postgres.Instances.Execute ()


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
convertMutationAction env logger userInfo manager reqHeaders  = \case
  AMSync s  -> ((AEPSync . _aerExecution) &&& _aerHeaders) <$>
    resolveActionExecution env logger userInfo s actionExecContext
  AMAsync s -> do
    result <-
      liftEitherM (runMetadataStorageT $ resolveActionMutationAsync s reqHeaders userSession)
    pure (AEPAsyncMutation result, [])
  where
    userSession = _uiSession userInfo
    actionExecContext = ActionExecContext manager reqHeaders $ _uiSession userInfo

convertMutationSelectionSet
  :: forall m
   . ( HasVersion
     , Tracing.MonadTrace m
     , MonadIO m
     , MonadError QErr m
     , MonadMetadataStorage (MetadataStorageT m)
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
  -> m ExecutionPlan
convertMutationSelectionSet env logger gqlContext SQLGenCtx{stringifyNum} userInfo manager reqHeaders fields varDefs varValsM = do
  mutationParser <- onNothing (gqlMutationParser gqlContext) $
    throw400 ValidationFailed "no mutations exist"
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability)
    :: (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue), QueryReusability)
    <-  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
    >>= (mutationParser >>> (`onLeft` reportParseErrors))

  -- Transform the RQL AST into a prepared SQL query
  txs <- for unpreparedQueries \case
    RFDB _ (sourceConfig :: SourceConfig b) (MDBR db) -> case backendTag @b of
      PostgresTag -> mkDBMutationPlan env manager reqHeaders userInfo stringifyNum sourceConfig db
      MSSQLTag    -> mkDBMutationPlan env manager reqHeaders userInfo stringifyNum sourceConfig db
    RFRemote remoteField -> do
      RemoteFieldG remoteSchemaInfo resolvedRemoteField <- resolveRemoteField userInfo remoteField
      pure $ buildExecStepRemote remoteSchemaInfo G.OperationTypeMutation $ [G.SelectionField resolvedRemoteField]
    RFAction action ->
      uncurry ExecStepAction <$> convertMutationAction env logger userInfo manager reqHeaders action
    RFRaw s ->
      pure $ ExecStepRaw s

  return txs
  where
    reportParseErrors errs = case NE.head errs of
      -- TODO: Our error reporting machinery doesn’t currently support reporting
      -- multiple errors at once, so we’re throwing away all but the first one
      -- here. It would be nice to report all of them!
      ParseError{ pePath, peMessage, peCode } ->
        throwError (err400 peCode peMessage){ qePath = pePath }
