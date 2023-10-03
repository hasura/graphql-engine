module Hasura.GraphQL.Execute.Mutation
  ( convertMutationSelectionSet,
  )
where

import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Tagged qualified as Tagged
import Data.Text.Extended (toTxt, (<>>))
import Hasura.Base.Error
import Hasura.GraphQL.Context
import Hasura.GraphQL.Execute.Action
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Common
import Hasura.GraphQL.Execute.Instances ()
import Hasura.GraphQL.Execute.Remote
import Hasura.GraphQL.Execute.RemoteJoin.Collect qualified as RJ
import Hasura.GraphQL.Execute.Resolve
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Parser.Directives
import Hasura.GraphQL.Parser.Variable qualified as G
import Hasura.GraphQL.Schema.Parser (runParse, toQErr)
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.QueryTags.Types
import Hasura.RQL.IR
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RemoteSchema.Metadata.Base (RemoteSchemaName (..))
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Prometheus (PrometheusMetrics (..))
import Hasura.Server.Types
import Hasura.Services
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

convertMutationAction ::
  ( MonadIO m,
    MonadError QErr m,
    MonadMetadataStorage m,
    ProvidesNetwork m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  Tracing.HttpPropagator ->
  PrometheusMetrics ->
  UserInfo ->
  HTTP.RequestHeaders ->
  Maybe GH.GQLQueryText ->
  ActionMutation Void ->
  m ActionExecutionPlan
convertMutationAction env logger tracesPropagator prometheusMetrics userInfo reqHeaders gqlQueryText action = do
  httpManager <- askHTTPManager
  case action of
    AMSync s ->
      pure $ AEPSync $ resolveActionExecution httpManager env logger tracesPropagator prometheusMetrics s actionExecContext gqlQueryText
    AMAsync s ->
      AEPAsyncMutation <$> resolveActionMutationAsync s reqHeaders userSession
  where
    userSession = _uiSession userInfo
    actionExecContext = ActionExecContext reqHeaders (_uiSession userInfo)

convertMutationSelectionSet ::
  forall m.
  ( Tracing.MonadTrace m,
    MonadIO m,
    MonadError QErr m,
    MonadMetadataStorage m,
    MonadGQLExecutionCheck m,
    MonadQueryTags m,
    ProvidesNetwork m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  Tracing.HttpPropagator ->
  PrometheusMetrics ->
  GQLContext ->
  SQLGenCtx ->
  UserInfo ->
  HTTP.RequestHeaders ->
  [G.Directive G.Name] ->
  G.SelectionSet G.NoFragments G.Name ->
  [G.VariableDefinition] ->
  GH.GQLReqUnparsed ->
  SetGraphqlIntrospectionOptions ->
  RequestId ->
  -- | Graphql Operation Name
  Maybe G.Name ->
  m (ExecutionPlan, ParameterizedQueryHash, [ModelInfoPart])
convertMutationSelectionSet
  env
  logger
  tracesPropagator
  prometheusMetrics
  gqlContext
  SQLGenCtx {stringifyNum}
  userInfo
  reqHeaders
  directives
  fields
  varDefs
  gqlUnparsed
  introspectionDisabledRoles
  reqId
  maybeOperationName = do
    mutationParser <-
      onNothing (gqlMutationParser gqlContext)
        $ throw400 ValidationFailed "no mutations exist"

    (resolvedDirectives, resolvedSelSet) <- resolveVariables varDefs (fromMaybe HashMap.empty (GH._grVariables gqlUnparsed)) directives fields
    -- Parse the GraphQL query into the RQL AST
    (unpreparedQueries :: RootFieldMap (MutationRootField UnpreparedValue)) <-
      Tracing.newSpan "Parse mutation IR" $ liftEither $ mutationParser resolvedSelSet

    -- Process directives on the mutation
    _dirMap <- toQErr $ runParse (parseDirectives customDirectives (G.DLExecutable G.EDLMUTATION) resolvedDirectives)
    let parameterizedQueryHash = calculateParameterizedQueryHash resolvedSelSet

        resolveExecutionSteps rootFieldName rootFieldUnpreparedValue = Tracing.newSpan ("Resolve execution step for " <>> rootFieldName) do
          case rootFieldUnpreparedValue of
            RFDB sourceName exists ->
              AB.dispatchAnyBackend @BackendExecute
                exists
                \(SourceConfigWith (sourceConfig :: SourceConfig b) queryTagsConfig (MDBR db)) -> do
                  let mReqId =
                        case _qtcOmitRequestId <$> queryTagsConfig of
                          -- we include the request id only if a user explicitly wishes for it to be included.
                          Just False -> Just reqId
                          _ -> Nothing
                      mutationQueryTagsAttributes = encodeQueryTags $ QTMutation $ MutationMetadata mReqId maybeOperationName rootFieldName parameterizedQueryHash
                      queryTagsComment = Tagged.untag $ createQueryTags @m mutationQueryTagsAttributes queryTagsConfig
                      (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsMutationDB db

                  httpManager <- askHTTPManager
                  let selSetArguments = getSelSetArgsFromRootField resolvedSelSet rootFieldName
                  (dbStepInfo, dbModelInfoList) <- flip runReaderT queryTagsComment $ mkDBMutationPlan @b env httpManager logger userInfo stringifyNum sourceName sourceConfig noRelsDBAST reqHeaders maybeOperationName selSetArguments
                  pure $ (ExecStepDB [] (AB.mkAnyBackend dbStepInfo) remoteJoins, dbModelInfoList)
            RFRemote (RemoteSchemaName rName) remoteField -> do
              RemoteSchemaRootField remoteSchemaInfo resultCustomizer resolvedRemoteField <- runVariableCache $ resolveRemoteField userInfo remoteField
              let (noRelsRemoteField, remoteJoins) = RJ.getRemoteJoinsGraphQLField resolvedRemoteField
                  rsModel = ModelInfoPart (toTxt rName) ModelTypeRemoteSchema Nothing Nothing (ModelOperationType G.OperationTypeMutation)
              pure
                $ (buildExecStepRemote remoteSchemaInfo resultCustomizer G.OperationTypeMutation noRelsRemoteField remoteJoins (GH._grOperationName gqlUnparsed), [rsModel])
            RFAction action -> do
              let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionMutation action
              (actionName, _fch) <- pure $ case noRelsDBAST of
                AMSync s -> (_aaeName s, _aaeForwardClientHeaders s)
                AMAsync s -> (_aamaName s, _aamaForwardClientHeaders s)
              plan <- convertMutationAction env logger tracesPropagator prometheusMetrics userInfo reqHeaders (Just (GH._grQuery gqlUnparsed)) noRelsDBAST
              let actionsModel = ModelInfoPart (toTxt actionName) ModelTypeAction Nothing Nothing (ModelOperationType G.OperationTypeMutation)
              pure $ (ExecStepAction plan (ActionsInfo actionName _fch) remoteJoins, [actionsModel]) -- `_fch` represents the `forward_client_headers` option from the action
              -- definition which is currently being ignored for actions that are mutations
            RFRaw customFieldVal -> fmap (,[]) $ flip onLeft throwError =<< executeIntrospection userInfo customFieldVal introspectionDisabledRoles
            RFMulti lst -> do
              allSteps <- traverse (resolveExecutionSteps rootFieldName) lst
              let executionStepsList = map fst allSteps
                  modelInfoList = map snd allSteps
              pure $ (ExecStepMulti executionStepsList, concat modelInfoList)

    -- Transform the RQL AST into a prepared SQL query
    txs <- flip InsOrdHashMap.traverseWithKey unpreparedQueries $ resolveExecutionSteps
    let executionPlan = InsOrdHashMap.map fst txs
        modelInfoHashMap = InsOrdHashMap.map snd txs

    let modelInfoList = concat $ InsOrdHashMap.elems modelInfoHashMap
    return (executionPlan, parameterizedQueryHash, modelInfoList)

-- | Extract the arguments from the selection set for a root field
-- This is used to validate the arguments of a mutation.
getSelSetArgsFromRootField :: G.SelectionSet G.NoFragments G.Variable -> RootFieldAlias -> Maybe (HashMap G.Name (G.Value G.Variable))
getSelSetArgsFromRootField selSet rootFieldName = do
  let maybeSelSet =
        case rootFieldName of
          RootFieldAlias Nothing alias -> getSelSet alias selSet
          RootFieldAlias (Just namespace) alias -> do
            let namespaceSelSet = getSelSet namespace selSet
            case namespaceSelSet of
              Just (G.SelectionField fld) -> getSelSet alias (G._fSelectionSet fld)
              _ -> Nothing
  case maybeSelSet of
    Just (G.SelectionField fld) -> Just $ (G._fArguments fld)
    _ -> Nothing
  where
    getSelSet alias set = flip find set $ \case
      G.SelectionField fld ->
        case G._fAlias fld of
          Nothing -> G.unName alias == G.unName (G._fName fld)
          Just aliasName -> G.unName alias == G.unName aliasName
      _ -> False
