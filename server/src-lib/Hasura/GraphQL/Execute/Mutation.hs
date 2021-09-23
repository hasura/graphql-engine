module Hasura.GraphQL.Execute.Mutation
  ( convertMutationSelectionSet
  ) where

import           Hasura.Prelude

import qualified Data.Environment                          as Env
import qualified Data.HashMap.Strict                       as Map
import qualified Data.HashMap.Strict.InsOrd                as OMap
import qualified Data.Tagged                               as Tagged
import qualified Language.GraphQL.Draft.Syntax             as G
import qualified Network.HTTP.Client                       as HTTP
import qualified Network.HTTP.Types                        as HTTP

import qualified Hasura.GraphQL.Execute.RemoteJoin.Collect as RJ
import qualified Hasura.GraphQL.Transport.HTTP.Protocol    as GH
import qualified Hasura.Logging                            as L
import qualified Hasura.SQL.AnyBackend                     as AB
import qualified Hasura.Tracing                            as Tracing

import           Hasura.Base.Error
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.Common
import           Hasura.GraphQL.Execute.Instances          ()
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.ParameterizedQueryHash
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Parser.Directives
import           Hasura.Metadata.Class
import           Hasura.RQL.IR
import           Hasura.RQL.Types
import           Hasura.Server.Version                     (HasVersion)
import           Hasura.Session

import           Hasura.QueryTags
import           Hasura.Server.Types                       (RequestId (..))

convertMutationAction
  ::( MonadIO m
    , MonadError QErr m
    , MonadMetadataStorage (MetadataStorageT m)
    )
  => Env.Environment
  -> L.Logger L.Hasura
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> Maybe GH.GQLQueryText
  -> ActionMutation ('Postgres 'Vanilla) (Const Void) (UnpreparedValue ('Postgres 'Vanilla))
  -> m ActionExecutionPlan
convertMutationAction env logger userInfo manager reqHeaders gqlQueryText = \case
  AMSync s  -> pure $ AEPSync $ resolveActionExecution env logger userInfo s actionExecContext gqlQueryText
  AMAsync s -> AEPAsyncMutation <$>
    liftEitherM (runMetadataStorageT $ resolveActionMutationAsync s reqHeaders userSession)
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
     , MonadGQLExecutionCheck m
     , MonadQueryTags m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> GQLContext
  -> SQLGenCtx
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> GH.GQLReqUnparsed
  -> SetGraphqlIntrospectionOptions
  -> RequestId
  -> Maybe G.Name
  -- ^ Graphql Operation Name
  -> m (ExecutionPlan, ParameterizedQueryHash)
convertMutationSelectionSet env logger gqlContext SQLGenCtx{stringifyNum} userInfo manager reqHeaders
  directives fields varDefs gqlUnparsed introspectionDisabledRoles reqId maybeOperationName = do
  mutationParser <- onNothing (gqlMutationParser gqlContext) $
    throw400 ValidationFailed "no mutations exist"

  (resolvedDirectives, resolvedSelSet) <- resolveVariables varDefs (fromMaybe Map.empty (GH._grVariables gqlUnparsed)) directives fields
  -- Parse the GraphQL query into the RQL AST
  unpreparedQueries
    :: OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue)
    <-(mutationParser >>> (`onLeft` reportParseErrors)) resolvedSelSet

  -- Process directives on the mutation
  _dirMap <-  (`onLeft` reportParseErrors) =<<
    runParseT (parseDirectives customDirectives (G.DLExecutable G.EDLMUTATION) resolvedDirectives)

  let parameterizedQueryHash = calculateParameterizedQueryHash resolvedSelSet

  -- Transform the RQL AST into a prepared SQL query
  txs <- flip OMap.traverseWithKey unpreparedQueries $ \rootFieldName rootFieldUnpreparedValue -> do
    case rootFieldUnpreparedValue of
      RFDB sourceName exists ->
        AB.dispatchAnyBackend @BackendExecute exists
          \(SourceConfigWith (sourceConfig :: SourceConfig b) queryTagsConfig (MDBR db)) -> do

            let mutationQueryTagsAttributes = encodeQueryTags $ QTMutation $ MutationMetadata reqId maybeOperationName rootFieldName parameterizedQueryHash
            let queryTagsComment = Tagged.untag $ createQueryTags @m mutationQueryTagsAttributes queryTagsConfig
            let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsMutationDB db
            dbStepInfo <- flip runReaderT queryTagsComment $ mkDBMutationPlan @b userInfo stringifyNum sourceName sourceConfig noRelsDBAST
            pure $ ExecStepDB [] (AB.mkAnyBackend dbStepInfo) remoteJoins
      RFRemote remoteField -> do
        RemoteFieldG remoteSchemaInfo resultCustomizer resolvedRemoteField <- runVariableCache $ resolveRemoteField userInfo remoteField
        pure $ buildExecStepRemote remoteSchemaInfo resultCustomizer G.OperationTypeMutation $ getRemoteFieldSelectionSet resolvedRemoteField
      RFAction action -> do
        let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionMutation action
        (actionName, _fch) <- pure $ case noRelsDBAST of
          AMSync s  -> (_aaeName s, _aaeForwardClientHeaders s)
          AMAsync s -> (_aamaName s, _aamaForwardClientHeaders s)
        plan <- convertMutationAction env logger userInfo manager reqHeaders (Just (GH._grQuery gqlUnparsed)) noRelsDBAST
        pure $ ExecStepAction plan (ActionsInfo actionName _fch) remoteJoins -- `_fch` represents the `forward_client_headers` option from the action
                                                                  -- definition which is currently being ignored for actions that are mutations
      RFRaw s -> flip onLeft throwError =<< executeIntrospection userInfo s introspectionDisabledRoles
  return (txs, parameterizedQueryHash)
