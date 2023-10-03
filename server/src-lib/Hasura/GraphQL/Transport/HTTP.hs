-- | Execution of GraphQL queries over HTTP transport
module Hasura.GraphQL.Transport.HTTP
  ( MonadExecuteQuery (..),
    CacheResult (..),
    CachedDirective (..),
    ResponseCacher (..),
    runGQ,
    runGQBatched,
    coalescePostgresMutations,
    extractFieldFromResponse,
    buildRaw,
    encodeAnnotatedResponseParts,
    encodeEncJSONResults,

    -- * imported from HTTP.Protocol; required by pro
    GQLReq (..),
    GQLReqUnparsed,
    GQLReqParsed,
    GQLExecDoc (..),
    OperationName (..),
    GQLQueryText (..),
    AnnotatedResponsePart (..),
    CacheStoreResponse (..),
    SessVarPred,
    filterVariablesFromQuery,
    runSessVarPred,
  )
where

import Control.Lens (Traversal', foldOf, to)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Ordered qualified as JO
import Data.Bifoldable
import Data.ByteString.Lazy qualified as LBS
import Data.Dependent.Map qualified as DM
import Data.Environment qualified as Env
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Monoid (Any (..))
import Data.Text qualified as T
import Data.Text.Extended (toTxt, (<>>))
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Backends.Postgres.Instances.Transport (runPGMutationTransaction)
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Action qualified as EA
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.RemoteJoin qualified as RJ
import Hasura.GraphQL.Logging
  ( MonadExecutionLog,
    MonadQueryLog (logQueryLog),
    QueryLog (..),
    QueryLogKind (..),
    statsToAnyBackend,
  )
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Parser.Directives hiding (cachedDirective)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.GraphQL.Transport.Instances ()
import Hasura.HTTP
  ( HttpResponse (HttpResponse, _hrBody),
    addHttpResponseHeaders,
  )
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.OpenTelemetry (getOtelTracesPropagator)
import Hasura.RQL.Types.ResultCustomization
import Hasura.RQL.Types.SchemaCache
import Hasura.RemoteSchema.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Init qualified as Init
import Hasura.Server.Init.Config
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Logging qualified as L
import Hasura.Server.Prometheus
  ( GraphQLRequestMetrics (..),
    PrometheusMetrics (..),
  )
import Hasura.Server.Telemetry.Counters qualified as Telem
import Hasura.Server.Types (ModelInfoLogState (..), MonadGetPolicies (..), ReadOnlyMode (..), RequestId (..))
import Hasura.Services
import Hasura.Session (SessionVariable, SessionVariableValue, SessionVariables, UserInfo (..), filterSessionVariables)
import Hasura.Tracing (MonadTrace, attachMetadata)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Extended qualified as Wai
import System.Metrics.Prometheus.Counter qualified as Prometheus.Counter
import System.Metrics.Prometheus.Histogram qualified as Prometheus.Histogram

-- | Encapsulates a function that stores a query response in the cache.
-- `cacheLookup` decides when such an invitation to store is generated.
newtype ResponseCacher = ResponseCacher {runStoreResponse :: forall m. (MonadTrace m, MonadIO m) => EncJSON -> m (Either QErr CacheStoreResponse)}

data CacheStoreResponse
  = -- | Cache storage is unconditional, just
    -- not always available.
    CacheStoreSuccess
  | CacheStoreLimitReached
  | CacheStoreNotEnoughCapacity
  | CacheStoreBackendError String

data CacheResult
  = -- | We have a cached response for this query
    ResponseCached EncJSON
  | -- | We don't have a cached response.  The `ResponseCacher` can be used to
    -- store the response in the cache after a fresh execution.
    ResponseUncached (Maybe ResponseCacher)

class (Monad m) => MonadExecuteQuery m where
  -- | This method does two things: it looks up a query result in the
  -- server-side cache, if a cache is used, and it additionally returns HTTP
  -- headers that can instruct a client how long a response can be cached
  -- locally (i.e. client-side).
  cacheLookup ::
    -- | How we _would've_ executed the query.  Ideally we'd use this as a
    -- caching key, but it's not serializable... [cont'd]
    EB.ExecutionPlan ->
    -- | Somewhat less processed plan of how we _would've_ executed the query.
    [QueryRootField UnpreparedValue] ->
    -- | `@cached` directive from the query AST
    Maybe CachedDirective ->
    -- | [cont'd] ... which is why we additionally pass serializable structures
    -- from earlier in the query processing pipeline.  This includes the query
    -- AST, which additionally specifies the `@cached` directive with TTL info...
    GQLReqParsed ->
    -- | ... and the `UserInfo`
    UserInfo ->
    -- | Used for remote schemas and actions
    [HTTP.Header] ->
    -- | Non-empty response headers instruct the client to store the response
    -- locally.
    m (Either QErr (HTTP.ResponseHeaders, CacheResult))
  default cacheLookup ::
    (m ~ t n, MonadTrans t, MonadExecuteQuery n) =>
    EB.ExecutionPlan ->
    [QueryRootField UnpreparedValue] ->
    Maybe CachedDirective ->
    GQLReqParsed ->
    UserInfo ->
    [HTTP.Header] ->
    m (Either QErr (HTTP.ResponseHeaders, CacheResult))
  cacheLookup a b c d e f = lift $ cacheLookup a b c d e f

instance (MonadExecuteQuery m) => MonadExecuteQuery (ReaderT r m)

instance (MonadExecuteQuery m) => MonadExecuteQuery (ExceptT e m)

-- | A partial response, e.g. from a remote schema call or postgres
-- postgres query, which we'll assemble into the final response for
-- the client. It is annotated with timing metadata.
data AnnotatedResponsePart = AnnotatedResponsePart
  { arpTimeIO :: DiffTime,
    arpLocality :: Telem.Locality,
    arpResponse :: EncJSON,
    arpHeaders :: HTTP.ResponseHeaders
  }

-- | A full response, annotated with timing metadata.
data AnnotatedResponse = AnnotatedResponse
  { arQueryType :: Telem.QueryType,
    arTimeIO :: DiffTime,
    arLocality :: Telem.Locality,
    arResponse :: HttpResponse (Maybe GQResponse, EncJSON)
  }

-- | Merge response parts into a full response.
buildResponseFromParts ::
  (MonadError QErr m) =>
  Telem.QueryType ->
  Either (Either GQExecError QErr) (RootFieldMap (AnnotatedResponsePart, [ModelInfoPart])) ->
  m (AnnotatedResponse, [ModelInfoPart])
buildResponseFromParts telemType partsErr =
  buildResponse telemType partsErr \parts ->
    let (key, (compositeValue')) = unzip $ InsOrdHashMap.toList parts
        (annotatedResp, model) = unzip compositeValue'
        parts' = InsOrdHashMap.fromList $ zip key annotatedResp
        modelInfoList = concat model
        responseData = Right $ encJToLBS $ encodeAnnotatedResponseParts parts'
     in ( AnnotatedResponse
            { arQueryType = telemType,
              arTimeIO = sum (fmap arpTimeIO parts'),
              arLocality = foldMap arpLocality parts',
              arResponse =
                HttpResponse
                  (Just responseData, encodeGQResp responseData)
                  (foldMap arpHeaders parts')
            },
          modelInfoList
        )

buildResponse ::
  (MonadError QErr m) =>
  Telem.QueryType ->
  Either (Either GQExecError QErr) a ->
  (a -> (AnnotatedResponse, [ModelInfoPart])) ->
  m (AnnotatedResponse, [ModelInfoPart])
buildResponse telemType res f = case res of
  Right a -> pure $ f a
  Left (Right err) -> throwError err
  Left (Left err) ->
    pure
      $ ( AnnotatedResponse
            { arQueryType = telemType,
              arTimeIO = 0,
              arLocality = Telem.Remote,
              arResponse =
                HttpResponse
                  (Just (Left err), encodeGQResp $ Left err)
                  []
            },
          []
        )

-- | A predicate on session variables. The 'Monoid' instance makes it simple
-- to combine several predicates disjunctively.
-- | The definition includes `Maybe` which allows us to short-circuit calls like @mempty <> m@ and @m <> mempty@, which
-- otherwise might build up long repeated chains of calls to @\_ _ -> False@.
newtype SessVarPred = SessVarPred {unSessVarPred :: Maybe (SessionVariable -> SessionVariableValue -> Bool)}
  deriving (Semigroup, Monoid) via (Maybe (SessionVariable -> SessionVariableValue -> Any))

keepAllSessionVariables :: SessVarPred
keepAllSessionVariables = SessVarPred $ Just $ \_ _ -> True

runSessVarPred :: SessVarPred -> SessionVariables -> SessionVariables
runSessVarPred = filterSessionVariables . fromMaybe (\_ _ -> False) . unSessVarPred

-- | Filter out only those session variables used by the query AST provided
filterVariablesFromQuery ::
  [ RootField
      (QueryDBRoot (RemoteRelationshipField UnpreparedValue) UnpreparedValue)
      (RemoteSchemaRootField (RemoteRelationshipField UnpreparedValue) RemoteSchemaVariable)
      (ActionQuery (RemoteRelationshipField UnpreparedValue))
      d
  ] ->
  SessVarPred
filterVariablesFromQuery = foldMap \case
  RFDB _ exists ->
    AB.dispatchAnyBackend @Backend exists \case
      SourceConfigWith _ _ (QDBR db) -> bifoldMap remoteFieldPred toPred db
  RFRemote _ remote -> foldOf (traverse . _SessionPresetVariable . to match) remote
  RFAction actionQ -> foldMap remoteFieldPred actionQ
  RFRaw {} -> mempty
  RFMulti {} -> mempty
  where
    _SessionPresetVariable :: Traversal' RemoteSchemaVariable SessionVariable
    _SessionPresetVariable f (SessionPresetVariable a b c) =
      (\a' -> SessionPresetVariable a' b c) <$> f a
    _SessionPresetVariable _ x = pure x

    toPred :: UnpreparedValue bet -> SessVarPred
    -- if we see a reference to the whole session variables object,
    -- then we need to keep everything:
    toPred UVSession = keepAllSessionVariables
    -- if we only see a specific session variable, we only need to keep that one:
    toPred (UVSessionVar _type sv) = match sv
    toPred _ = mempty

    match :: SessionVariable -> SessVarPred
    match sv = SessVarPred $ Just $ \sv' _ -> sv == sv'

    remoteFieldPred :: RemoteRelationshipField UnpreparedValue -> SessVarPred
    remoteFieldPred = \case
      RemoteSchemaField RemoteSchemaSelect {..} ->
        foldOf (traverse . _SessionPresetVariable . to match) _rselSelection
      RemoteSourceField exists ->
        AB.dispatchAnyBackend @Backend exists \RemoteSourceSelect {..} ->
          case _rssSelection of
            SourceRelationshipObject obj -> foldMap toPred obj
            SourceRelationshipArray arr -> foldMap toPred arr
            SourceRelationshipArrayAggregate agg -> foldMap toPred agg

-- | Run (execute) a single GraphQL query
runGQ ::
  forall m.
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadTrace m,
    MonadExecuteQuery m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  -- TODO: almost all of those arguments come from `AppEnv` and `HandlerCtx`
  -- (including `AppContext`). We could refactor this function to make use of
  -- `HasAppEnv` and `MonadReader HandlerCtx` if the direct dependency is ok.
  -- In turn, cleaning this list of arguments would allow for a cleanup of
  -- `runGQBatched` and `runCustomEndpoint`.
  Env.Environment ->
  SQLGenCtx ->
  SchemaCache ->
  Init.AllowListStatus ->
  ReadOnlyMode ->
  PrometheusMetrics ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  RequestId ->
  UserInfo ->
  Wai.IpAddress ->
  [HTTP.Header] ->
  E.GraphQLQueryType ->
  GQLReqUnparsed ->
  m (GQLQueryOperationSuccessLog, HttpResponse (Maybe GQResponse, EncJSON))
runGQ env sqlGenCtx sc enableAL readOnlyMode prometheusMetrics logger agentLicenseKey reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  getModelInfoLogStatus' <- runGetModelInfoLogStatus
  modelInfoLogStatus <- liftIO getModelInfoLogStatus'
  let gqlMetrics = pmGraphQLRequestMetrics prometheusMetrics

  (totalTime, (response, parameterizedQueryHash, gqlOpType, modelInfoListForLogging, queryCachedStatus)) <- withElapsedTime $ do
    (reqParsed, runLimits, queryParts) <- Tracing.newSpan "Parse GraphQL" $ observeGQLQueryError gqlMetrics Nothing $ do
      -- 1. Run system authorization on the 'reqUnparsed :: GQLReqUnparsed' query.
      reqParsed <-
        E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed reqId
          >>= flip onLeft throwError

      operationLimit <- askGraphqlOperationLimit reqId userInfo (scApiLimits sc)
      let runLimits = runResourceLimits operationLimit

      -- 2. Construct the first step of the execution plan from 'reqParsed :: GQLParsed'.
      queryParts <- getSingleOperation reqParsed
      return (reqParsed, runLimits, queryParts)

    let gqlOpType = G._todType queryParts
    observeGQLQueryError gqlMetrics (Just gqlOpType) $ do
      -- 3. Construct the remainder of the execution plan.
      let maybeOperationName = _unOperationName <$> getOpNameFromParsedReq reqParsed
      for_ maybeOperationName $ \nm ->
        -- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/instrumentation/graphql/
        attachMetadata [("graphql.operation.name", G.unName nm)]
      (parameterizedQueryHash, execPlan, modelInfoList) <-
        E.getResolvedExecPlan
          env
          logger
          prometheusMetrics
          userInfo
          sqlGenCtx
          readOnlyMode
          sc
          queryType
          reqHeaders
          reqUnparsed
          queryParts
          maybeOperationName
          reqId

      -- 4. Execute the execution plan producing a 'AnnotatedResponse'.
      (response, queryCachedStatus, modelInfoFromExecution) <- executePlan reqParsed runLimits execPlan
      return (response, parameterizedQueryHash, gqlOpType, ((modelInfoList <> (modelInfoFromExecution))), queryCachedStatus)

  -- 5. Record telemetry
  recordTimings totalTime response

  -- 6. Record Prometheus metrics (query successes)
  liftIO $ recordGQLQuerySuccess gqlMetrics totalTime gqlOpType

  -- 7. Return the response along with logging metadata.
  let requestSize = LBS.length $ J.encode reqUnparsed
      responseSize = LBS.length $ encJToLBS $ snd $ _hrBody $ arResponse $ response
  when (modelInfoLogStatus == ModelInfoLogOn) $ do
    for_ (modelInfoListForLogging) $ \(ModelInfoPart modelName modelType modelSourceName modelSourceType modelQueryType) -> do
      L.unLogger logger $ ModelInfoLog L.LevelInfo $ ModelInfo modelName (toTxt modelType) modelSourceName (toTxt <$> modelSourceType) (toTxt modelQueryType) queryCachedStatus
  return
    ( GQLQueryOperationSuccessLog reqUnparsed totalTime responseSize requestSize parameterizedQueryHash,
      arResponse response
    )
  where
    doQErr :: ExceptT QErr m a -> ExceptT (Either GQExecError QErr) m a
    doQErr = withExceptT Right

    forWithKey = flip InsOrdHashMap.traverseWithKey

    tracesPropagator = getOtelTracesPropagator $ scOpenTelemetryConfig sc

    executePlan ::
      GQLReqParsed ->
      (m (AnnotatedResponse, Bool, [ModelInfoPart]) -> m (AnnotatedResponse, Bool, [ModelInfoPart])) ->
      E.ResolvedExecutionPlan ->
      m (AnnotatedResponse, Bool, [ModelInfoPart])
    executePlan reqParsed runLimits execPlan = case execPlan of
      E.QueryExecutionPlan queryPlans asts dirMap -> do
        let cachedDirective = runIdentity <$> DM.lookup cached dirMap
        -- Attempt to lookup a cached response in the query cache.
        (cachingHeaders, cachedValue) <- liftEitherM $ cacheLookup queryPlans asts cachedDirective reqParsed userInfo reqHeaders
        case cachedValue of
          -- If we get a cache hit, annotate the response with metadata and return it.
          ResponseCached cachedResponseData -> do
            logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindCached
            pure
              $ ( AnnotatedResponse
                    { arQueryType = Telem.Query,
                      arTimeIO = 0,
                      arLocality = Telem.Local,
                      arResponse = HttpResponse (decodeGQResp cachedResponseData) cachingHeaders
                    },
                  True,
                  []
                )
          -- If we get a cache miss, we must run the query against the graphql engine.
          ResponseUncached storeResponseM -> runLimits $ do
            -- 1. 'traverse' the 'ExecutionPlan' executing every step.
            -- TODO: can this be a `catch` rather than a `runExceptT`?
            (conclusion) <- runExceptT $ forWithKey queryPlans executeQueryStep
            -- 2. Construct an 'AnnotatedResponse' from the results of all steps in the 'ExecutionPlan'.
            (result, modelInfoList) <- buildResponseFromParts Telem.Query conclusion
            let response@(HttpResponse responseData _) = arResponse result
            -- 3. Cache the 'AnnotatedResponse'.
            case storeResponseM of
              -- No caching intended
              Nothing ->
                -- TODO: we probably don't want to use `cachingHeaders` here.
                -- If no caching was intended, then we shouldn't instruct the
                -- client to cache, either.  The only reason we're passing
                -- headers here is to avoid breaking changes.
                pure $ (result {arResponse = addHttpResponseHeaders cachingHeaders response}, False, modelInfoList)
              -- Caching intended; store result and instruct client through HTTP headers
              Just ResponseCacher {..} -> do
                cacheStoreRes <- liftEitherM $ runStoreResponse (snd responseData)
                let headers = case cacheStoreRes of
                      -- Note: Warning header format: "Warning: <warn-code> <warn-agent> <warn-text> [warn-date]"
                      -- See: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Warning
                      CacheStoreSuccess -> cachingHeaders
                      CacheStoreLimitReached -> [("warning", "199 - cache-store-size-limit-exceeded")]
                      CacheStoreNotEnoughCapacity -> [("warning", "199 - cache-store-capacity-exceeded")]
                      CacheStoreBackendError _ -> [("warning", "199 - cache-store-error")]
                 in -- 4. Return the response.
                    pure $ (result {arResponse = addHttpResponseHeaders headers response}, False, modelInfoList)
      E.MutationExecutionPlan mutationPlans -> runLimits $ do
        {- Note [Backwards-compatible transaction optimisation]

           For backwards compatibility, we perform the following optimisation: if all mutation steps
           are going to the same source, and that source is Postgres, we group all mutations as a
           transaction. This is a somewhat dangerous beaviour, and we would prefer, in the future,
           to make transactionality explicit rather than implicit and context-dependent.
        -}
        case coalescePostgresMutations mutationPlans of
          -- we are in the aforementioned case; we circumvent the normal process
          Just (sourceConfig, resolvedConnectionTemplate, pgMutations) -> do
            res <-
              -- TODO: can this be a `catch` rather than a `runExceptT`?
              runExceptT
                $ doQErr
                $ runPGMutationTransaction reqId reqUnparsed userInfo logger sourceConfig resolvedConnectionTemplate pgMutations
            -- we do not construct response parts since we have only one part
            (annotatedResponse, modelInfo) <- buildResponse Telem.Mutation res \(telemTimeIO_DT, parts) ->
              let responseData = Right $ encJToLBS $ encodeEncJSONResults parts
               in ( ( AnnotatedResponse
                        { arQueryType = Telem.Mutation,
                          arTimeIO = telemTimeIO_DT,
                          arLocality = Telem.Local,
                          arResponse =
                            HttpResponse
                              (Just responseData, encodeGQResp responseData)
                              []
                        }
                    ),
                    []
                  )
            pure $ (annotatedResponse, False, modelInfo)

          -- we are not in the transaction case; proceeding normally
          Nothing -> do
            -- TODO: can this be a `catch` rather than a `runExceptT`?
            conclusion <- runExceptT $ forWithKey mutationPlans executeMutationStep
            (response, modelInfo) <- buildResponseFromParts Telem.Mutation conclusion
            pure $ (response, False, modelInfo)
      E.SubscriptionExecutionPlan _sub ->
        throw400 UnexpectedPayload "subscriptions are not supported over HTTP, use websockets instead"

    executeQueryStep ::
      RootFieldAlias ->
      EB.ExecutionStep ->
      ExceptT (Either GQExecError QErr) m (AnnotatedResponsePart, [ModelInfoPart])
    executeQueryStep fieldName = \case
      E.ExecStepDB _headers exists remoteJoins -> doQErr $ do
        (telemTimeIO_DT, resp) <-
          AB.dispatchAnyBackend @BackendTransport
            exists
            \(EB.DBStepInfo _ sourceConfig genSql tx resolvedConnectionTemplate :: EB.DBStepInfo b) ->
              runDBQuery @b reqId reqUnparsed fieldName userInfo logger agentLicenseKey sourceConfig (fmap (statsToAnyBackend @b) tx) genSql resolvedConnectionTemplate
        (finalResponse, modelInfo) <-
          RJ.processRemoteJoins reqId logger agentLicenseKey env reqHeaders userInfo resp remoteJoins reqUnparsed tracesPropagator
        pure $ (AnnotatedResponsePart telemTimeIO_DT Telem.Local finalResponse [], modelInfo)
      E.ExecStepRemote rsi resultCustomizer gqlReq remoteJoins -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindRemoteSchema
        runRemoteGQ fieldName rsi resultCustomizer gqlReq remoteJoins
      E.ExecStepAction aep _ remoteJoins -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindAction
        (time, resp, modelInfo) <- doQErr $ do
          (time, (resp, _)) <- EA.runActionExecution userInfo aep
          (finalResponse, modelInfo) <-
            RJ.processRemoteJoins reqId logger agentLicenseKey env reqHeaders userInfo resp remoteJoins reqUnparsed tracesPropagator
          pure (time, finalResponse, modelInfo)
        pure $ (AnnotatedResponsePart time Telem.Empty resp [], modelInfo)
      E.ExecStepRaw json -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindIntrospection
        (,[]) <$> buildRaw json
      -- For `ExecStepMulti`, execute all steps and then concat them in a list
      E.ExecStepMulti lst -> do
        _all <- traverse (executeQueryStep fieldName) lst
        let (allResponses, allModelInfo) = unzip _all
        pure $ (AnnotatedResponsePart 0 Telem.Local (encJFromList (map arpResponse allResponses)) [], concat allModelInfo)

    executeMutationStep ::
      RootFieldAlias ->
      EB.ExecutionStep ->
      ExceptT (Either GQExecError QErr) m (AnnotatedResponsePart, [ModelInfoPart])
    executeMutationStep fieldName = \case
      E.ExecStepDB responseHeaders exists remoteJoins -> doQErr $ do
        (telemTimeIO_DT, resp) <-
          AB.dispatchAnyBackend @BackendTransport
            exists
            \(EB.DBStepInfo _ sourceConfig genSql tx resolvedConnectionTemplate :: EB.DBStepInfo b) ->
              runDBMutation @b reqId reqUnparsed fieldName userInfo logger agentLicenseKey sourceConfig (fmap EB.arResult tx) genSql resolvedConnectionTemplate
        (finalResponse, modelInfo) <-
          RJ.processRemoteJoins reqId logger agentLicenseKey env reqHeaders userInfo resp remoteJoins reqUnparsed tracesPropagator
        pure $ (AnnotatedResponsePart telemTimeIO_DT Telem.Local finalResponse responseHeaders, modelInfo)
      E.ExecStepRemote rsi resultCustomizer gqlReq remoteJoins -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindRemoteSchema
        runRemoteGQ fieldName rsi resultCustomizer gqlReq remoteJoins
      E.ExecStepAction aep _ remoteJoins -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindAction
        (time, (resp, hdrs), modelInfo) <- doQErr $ do
          (time, (resp, hdrs)) <- EA.runActionExecution userInfo aep
          (finalResponse, modelInfo) <-
            RJ.processRemoteJoins reqId logger agentLicenseKey env reqHeaders userInfo resp remoteJoins reqUnparsed tracesPropagator
          pure (time, (finalResponse, hdrs), modelInfo)
        pure $ (AnnotatedResponsePart time Telem.Empty resp $ fromMaybe [] hdrs, modelInfo)
      E.ExecStepRaw json -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindIntrospection
        (,[]) <$> buildRaw json
      -- For `ExecStepMulti`, execute all steps and then concat them in a list
      E.ExecStepMulti lst -> do
        _all <- traverse (executeQueryStep fieldName) lst
        let (allResponses, allModelInfo) = unzip _all
        pure $ (AnnotatedResponsePart 0 Telem.Local (encJFromList (map arpResponse allResponses)) [], concat allModelInfo)

    runRemoteGQ fieldName rsi resultCustomizer gqlReq remoteJoins = Tracing.newSpan ("Remote schema query for root field " <>> fieldName) $ do
      (telemTimeIO_DT, remoteResponseHeaders, resp) <-
        doQErr $ E.execRemoteGQ env tracesPropagator userInfo reqHeaders (rsDef rsi) gqlReq
      value <- extractFieldFromResponse fieldName resultCustomizer resp
      (finalResponse, modelInfo) <-
        doQErr
          $ RJ.processRemoteJoins
            reqId
            logger
            agentLicenseKey
            env
            reqHeaders
            userInfo
            -- TODO: avoid encode and decode here
            (encJFromOrderedValue value)
            remoteJoins
            reqUnparsed
            tracesPropagator
      let filteredHeaders = filter ((== "Set-Cookie") . fst) remoteResponseHeaders
      pure $ (AnnotatedResponsePart telemTimeIO_DT Telem.Remote finalResponse filteredHeaders, modelInfo)

    recordTimings :: DiffTime -> AnnotatedResponse -> m ()
    recordTimings totalTime result = do
      Telem.recordTimingMetric
        Telem.RequestDimensions
          { telemTransport = Telem.HTTP,
            telemQueryType = arQueryType result,
            telemLocality = arLocality result
          }
        Telem.RequestTimings
          { telemTimeIO = convertDuration $ arTimeIO result,
            telemTimeTot = convertDuration totalTime
          }

    -- Catch, record, and re-throw errors.
    observeGQLQueryError ::
      forall n e a.
      ( MonadIO n,
        MonadError e n
      ) =>
      GraphQLRequestMetrics ->
      Maybe G.OperationType ->
      n a ->
      n a
    observeGQLQueryError gqlMetrics mOpType action =
      catchError (fmap Right action) (pure . Left) >>= \case
        Right result ->
          pure result
        Left err -> do
          case mOpType of
            Nothing ->
              liftIO $ Prometheus.Counter.inc (gqlRequestsUnknownFailure gqlMetrics)
            Just opType -> case opType of
              G.OperationTypeQuery ->
                liftIO $ Prometheus.Counter.inc (gqlRequestsQueryFailure gqlMetrics)
              G.OperationTypeMutation ->
                liftIO $ Prometheus.Counter.inc (gqlRequestsMutationFailure gqlMetrics)
              G.OperationTypeSubscription ->
                -- We do not collect metrics for subscriptions at the request level.
                pure ()
          throwError err

    -- Tally and record execution times for successful GraphQL requests.
    recordGQLQuerySuccess ::
      GraphQLRequestMetrics -> DiffTime -> G.OperationType -> IO ()
    recordGQLQuerySuccess gqlMetrics totalTime = \case
      G.OperationTypeQuery -> liftIO $ do
        Prometheus.Counter.inc (gqlRequestsQuerySuccess gqlMetrics)
        Prometheus.Histogram.observe (gqlExecutionTimeSecondsQuery gqlMetrics) (realToFrac totalTime)
      G.OperationTypeMutation -> liftIO $ do
        Prometheus.Counter.inc (gqlRequestsMutationSuccess gqlMetrics)
        Prometheus.Histogram.observe (gqlExecutionTimeSecondsMutation gqlMetrics) (realToFrac totalTime)
      G.OperationTypeSubscription ->
        -- We do not collect metrics for subscriptions at the request level.
        -- Furthermore, we do not serve GraphQL subscriptions over HTTP.
        pure ()

coalescePostgresMutations ::
  EB.ExecutionPlan ->
  Maybe
    ( SourceConfig ('Postgres 'Vanilla),
      ResolvedConnectionTemplate ('Postgres 'Vanilla),
      InsOrdHashMap RootFieldAlias (EB.DBStepInfo ('Postgres 'Vanilla))
    )
coalescePostgresMutations plan = do
  -- we extract the name and config of the first mutation root, if any
  (oneSourceName, oneResolvedConnectionTemplate, oneSourceConfig) <- case toList plan of
    (E.ExecStepDB _ exists _remoteJoins : _) ->
      AB.unpackAnyBackend @('Postgres 'Vanilla) exists <&> \dbsi ->
        ( EB.dbsiSourceName dbsi,
          EB.dbsiResolvedConnectionTemplate dbsi,
          EB.dbsiSourceConfig dbsi
        )
    _ -> Nothing
  -- we then test whether all mutations are going to that same first source
  -- and that it is Postgres
  mutations <- for plan \case
    E.ExecStepDB _ exists remoteJoins -> do
      dbStepInfo <- AB.unpackAnyBackend @('Postgres 'Vanilla) exists
      guard
        $ oneSourceName
        == EB.dbsiSourceName dbStepInfo
        && isNothing remoteJoins
        && oneResolvedConnectionTemplate
        == EB.dbsiResolvedConnectionTemplate dbStepInfo
      Just dbStepInfo
    _ -> Nothing
  Just (oneSourceConfig, oneResolvedConnectionTemplate, mutations)

data GraphQLResponse
  = GraphQLResponseErrors [J.Value]
  | GraphQLResponseData JO.Value

decodeGraphQLResponse :: LBS.ByteString -> Either Text GraphQLResponse
decodeGraphQLResponse bs = do
  val <- mapLeft T.pack $ JO.eitherDecode bs
  valObj <- JO.asObject val
  case JO.lookup "errors" valObj of
    Just (JO.Array errs) -> Right $ GraphQLResponseErrors (toList $ JO.fromOrdered <$> errs)
    Just _ -> Left "Invalid \"errors\" field in response from remote"
    Nothing -> do
      dataVal <- JO.lookup "data" valObj `onNothing` Left "Missing \"data\" field in response from remote"
      Right $ GraphQLResponseData dataVal

extractFieldFromResponse ::
  forall m.
  (Monad m) =>
  RootFieldAlias ->
  ResultCustomizer ->
  LBS.ByteString ->
  ExceptT (Either GQExecError QErr) m JO.Value
extractFieldFromResponse fieldName resultCustomizer resp = do
  let fieldName' = G.unName $ _rfaAlias fieldName
  dataVal <-
    applyResultCustomizer resultCustomizer
      <$> do
        graphQLResponse <- decodeGraphQLResponse resp `onLeft` do400
        case graphQLResponse of
          GraphQLResponseErrors errs -> doGQExecError errs
          GraphQLResponseData d -> pure d
  dataObj <- onLeft (JO.asObject dataVal) do400
  fieldVal <-
    onNothing (JO.lookup fieldName' dataObj)
      $ do400
      $ "expecting key "
      <> fieldName'
  return fieldVal
  where
    do400 = withExceptT Right . throw400 RemoteSchemaError
    doGQExecError = withExceptT Left . throwError . GQExecError . fmap J.toEncoding

buildRaw :: (Applicative m) => JO.Value -> m AnnotatedResponsePart
buildRaw json = do
  let obj = encJFromOrderedValue json
      telemTimeIO_DT = 0
  pure $ AnnotatedResponsePart telemTimeIO_DT Telem.Local obj []

encodeAnnotatedResponseParts :: RootFieldMap AnnotatedResponsePart -> EncJSON
encodeAnnotatedResponseParts = encodeEncJSONResults . fmap arpResponse

encodeEncJSONResults :: RootFieldMap EncJSON -> EncJSON
encodeEncJSONResults =
  encNameMap . fmap (namespacedField id encNameMap) . unflattenNamespaces
  where
    encNameMap = encJFromInsOrdHashMap . InsOrdHashMap.mapKeys G.unName

-- | Run (execute) a batched GraphQL query (see 'GQLBatchedReqs').
runGQBatched ::
  forall m.
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadTrace m,
    MonadExecuteQuery m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  Env.Environment ->
  SQLGenCtx ->
  SchemaCache ->
  Init.AllowListStatus ->
  ReadOnlyMode ->
  PrometheusMetrics ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  RequestId ->
  ResponseInternalErrorsConfig ->
  UserInfo ->
  Wai.IpAddress ->
  [HTTP.Header] ->
  E.GraphQLQueryType ->
  -- | the batched request with unparsed GraphQL query
  GQLBatchedReqs (GQLReq GQLQueryText) ->
  m (HttpLogGraphQLInfo, HttpResponse EncJSON)
runGQBatched env sqlGenCtx sc enableAL readOnlyMode prometheusMetrics logger agentLicenseKey reqId responseErrorsConfig userInfo ipAddress reqHdrs queryType query =
  case query of
    GQLSingleRequest req -> do
      (gqlQueryOperationLog, httpResp) <- runGQ env sqlGenCtx sc enableAL readOnlyMode prometheusMetrics logger agentLicenseKey reqId userInfo ipAddress reqHdrs queryType req
      let httpLoggingGQInfo = (CommonHttpLogMetadata L.RequestModeSingle (Just (GQLSingleRequest (GQLQueryOperationSuccess gqlQueryOperationLog))), (PQHSetSingleton (gqolParameterizedQueryHash gqlQueryOperationLog)))
      pure (httpLoggingGQInfo, snd <$> httpResp)
    GQLBatchedReqs reqs -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      E.checkGQLBatchedReqs userInfo reqId reqs sc >>= flip onLeft throwError
      let includeInternal = shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig
          removeHeaders =
            flip HttpResponse []
              . encJFromList
              . map (either (encJFromJEncoding . encodeGQErr includeInternal) _hrBody)
      responses <- for reqs \req -> fmap (req,) $ try $ (fmap . fmap . fmap) snd $ runGQ env sqlGenCtx sc enableAL readOnlyMode prometheusMetrics logger agentLicenseKey reqId userInfo ipAddress reqHdrs queryType req
      let requestsOperationLogs = map fst $ rights $ map snd responses
          batchOperationLogs =
            map
              ( \(req, resp) ->
                  case resp of
                    Left err -> GQLQueryOperationError $ GQLQueryOperationErrorLog req err
                    Right (successOpLog, _) -> GQLQueryOperationSuccess successOpLog
              )
              responses
          parameterizedQueryHashes = map gqolParameterizedQueryHash requestsOperationLogs
          httpLoggingGQInfo = (CommonHttpLogMetadata L.RequestModeBatched ((Just (GQLBatchedReqs batchOperationLogs))), PQHSetBatched parameterizedQueryHashes)
      pure (httpLoggingGQInfo, removeHeaders (map ((fmap snd) . snd) responses))
  where
    try = flip catchError (pure . Left) . fmap Right
