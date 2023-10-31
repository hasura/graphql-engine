module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet,
    parseGraphQLQuery,
  )
where

import Data.Aeson qualified as J
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
import Hasura.GraphQL.Schema.Parser
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.QueryTags.Types
import Hasura.RQL.IR
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RemoteSchema.Metadata.Base (RemoteSchemaName (..))
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Prometheus (PrometheusMetrics (..))
import Hasura.Server.Types (MonadGetPolicies, RequestId (..))
import Hasura.Services.Network
import Hasura.Session
import Hasura.Tracing (MonadTrace)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

parseGraphQLQuery ::
  (MonadError QErr m) =>
  GQLContext ->
  [G.VariableDefinition] ->
  Maybe (HashMap G.Name J.Value) ->
  [G.Directive G.Name] ->
  G.SelectionSet G.NoFragments G.Name ->
  m
    ( RootFieldMap (QueryRootField UnpreparedValue),
      [G.Directive Variable],
      G.SelectionSet G.NoFragments Variable
    )
parseGraphQLQuery gqlContext varDefs varValsM directives fields = do
  (resolvedDirectives, resolvedSelSet) <- resolveVariables varDefs (fromMaybe HashMap.empty varValsM) directives fields
  parsedQuery <- liftEither $ gqlQueryParser gqlContext resolvedSelSet
  pure (parsedQuery, resolvedDirectives, resolvedSelSet)

-- | Construct an 'ExecutionPlan' from a 'G.SelectionSet'.
convertQuerySelSet ::
  forall m.
  ( MonadError QErr m,
    MonadTrace m,
    MonadIO m,
    MonadGQLExecutionCheck m,
    MonadQueryTags m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  Tracing.HttpPropagator ->
  PrometheusMetrics ->
  GQLContext ->
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
  m (ExecutionPlan, [QueryRootField UnpreparedValue], DirectiveMap, ParameterizedQueryHash, [ModelInfoPart])
convertQuerySelSet
  env
  logger
  tracingPropagator
  prometheusMetrics
  gqlContext
  userInfo
  reqHeaders
  directives
  fields
  varDefs
  gqlUnparsed
  introspectionDisabledRoles
  reqId
  maybeOperationName = do
    -- 1. Parse the GraphQL query into the 'RootFieldMap' and a 'SelectionSet'
    (unpreparedQueries, normalizedDirectives, normalizedSelectionSet) <-
      Tracing.newSpan "Parse query IR" $ parseGraphQLQuery gqlContext varDefs (GH._grVariables gqlUnparsed) directives fields

    -- 2. Parse directives on the query
    dirMap <- toQErr $ runParse (parseDirectives customDirectives (G.DLExecutable G.EDLQUERY) normalizedDirectives)

    let parameterizedQueryHash = calculateParameterizedQueryHash normalizedSelectionSet

        resolveExecutionSteps rootFieldName rootFieldUnpreparedValue = Tracing.newSpan ("Resolve execution step for " <>> rootFieldName) do
          case rootFieldUnpreparedValue of
            RFMulti lst -> do
              allSteps <- traverse (resolveExecutionSteps rootFieldName) lst
              let executionStepsList = map fst allSteps
                  modelInfoList = map snd allSteps
              pure $ (ExecStepMulti executionStepsList, concat modelInfoList)
            RFDB sourceName exists ->
              AB.dispatchAnyBackend @BackendExecute
                exists
                \(SourceConfigWith (sourceConfig :: (SourceConfig b)) queryTagsConfig (QDBR db)) -> do
                  Tracing.attachSourceConfigAttributes @b sourceConfig
                  let mReqId =
                        case _qtcOmitRequestId <$> queryTagsConfig of
                          -- we include the request id only if a user explicitly wishes for it to be included.
                          Just False -> Just reqId
                          _ -> Nothing
                      queryTagsAttributes = encodeQueryTags $ QTQuery $ QueryMetadata mReqId maybeOperationName rootFieldName parameterizedQueryHash
                      queryTagsComment = Tagged.untag $ createQueryTags @m queryTagsAttributes queryTagsConfig
                      (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsQueryDB db
                  (dbStepInfo, dbModelInfoList) <- flip runReaderT queryTagsComment $ mkDBQueryPlan @b userInfo sourceName sourceConfig noRelsDBAST reqHeaders maybeOperationName
                  pure $ (ExecStepDB [] (AB.mkAnyBackend dbStepInfo) remoteJoins, dbModelInfoList)
            RFRemote (RemoteSchemaName rName) rf -> do
              RemoteSchemaRootField remoteSchemaInfo resultCustomizer remoteField <- runVariableCache $ for rf $ resolveRemoteVariable userInfo
              let (noRelsRemoteField, remoteJoins) = RJ.getRemoteJoinsGraphQLField remoteField
              let rsModel = ModelInfoPart (toTxt rName) ModelTypeRemoteSchema Nothing Nothing (ModelOperationType G.OperationTypeQuery)
              pure $ (buildExecStepRemote remoteSchemaInfo resultCustomizer G.OperationTypeQuery noRelsRemoteField remoteJoins (GH._grOperationName gqlUnparsed), [rsModel])
            RFAction action -> do
              httpManager <- askHTTPManager
              let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionQuery action
              (actionExecution, actionName, fch) <- pure $ case noRelsDBAST of
                AQQuery s ->
                  ( AEPSync
                      $ resolveActionExecution
                        httpManager
                        env
                        logger
                        tracingPropagator
                        prometheusMetrics
                        s
                        (ActionExecContext reqHeaders (_uiSession userInfo))
                        (Just (GH._grQuery gqlUnparsed)),
                    _aaeName s,
                    _aaeForwardClientHeaders s
                  )
                AQAsync s -> (AEPAsyncQuery $ AsyncActionQueryExecutionPlan (_aaaqActionId s) $ resolveAsyncActionQuery userInfo s, _aaaqName s, _aaaqForwardClientHeaders s)
              let actionsModel = ModelInfoPart (toTxt actionName) ModelTypeAction Nothing Nothing (ModelOperationType G.OperationTypeQuery)
              pure $ (ExecStepAction actionExecution (ActionsInfo actionName fch) remoteJoins, [actionsModel])
            RFRaw r -> fmap (,[]) $ flip onLeft throwError =<< executeIntrospection userInfo r introspectionDisabledRoles
    -- 3. Transform the 'RootFieldMap' into an execution plan
    executionPlan <- flip InsOrdHashMap.traverseWithKey unpreparedQueries $ resolveExecutionSteps
    let executionPlan' = InsOrdHashMap.map fst executionPlan
        modelInfoHashMap = InsOrdHashMap.map snd executionPlan

    let modelInfoList = concat $ InsOrdHashMap.elems modelInfoHashMap
    pure (executionPlan', InsOrdHashMap.elems unpreparedQueries, dirMap, parameterizedQueryHash, modelInfoList)
