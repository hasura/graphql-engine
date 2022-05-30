-- | Execution of GraphQL queries over HTTP transport
module Hasura.GraphQL.Transport.HTTP
  ( QueryCacheKey (..),
    MonadExecuteQuery (..),
    CachedDirective (..),
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
    CacheStoreSuccess (..),
    CacheStoreFailure (..),
    SessVarPred,
    filterVariablesFromQuery,
    runSessVarPred,
  )
where

import Control.Lens (Traversal', foldOf, to)
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Ordered qualified as JO
import Data.Bifoldable
import Data.ByteString.Lazy qualified as LBS
import Data.Dependent.Map qualified as DM
import Data.Environment qualified as Env
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Monoid (Any (..))
import Data.Text qualified as T
import Hasura.Backends.Postgres.Instances.Transport (runPGMutationTransaction)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Action qualified as EA
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.RemoteJoin qualified as RJ
import Hasura.GraphQL.Logging
  ( MonadQueryLog (logQueryLog),
    QueryLog (..),
    QueryLogKind (..),
  )
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Parser.Directives (CachedDirective (..), DirectiveMap, cached)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.GraphQL.Transport.Instances ()
import Hasura.HTTP
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.ResultCustomization
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.Init.Config
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Logging qualified as L
import Hasura.Server.Telemetry.Counters qualified as Telem
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing (MonadTrace, TraceT, trace)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Extended qualified as Wai

data QueryCacheKey = QueryCacheKey
  { qckQueryString :: !GQLReqParsed,
    qckUserRole :: !RoleName,
    qckSession :: !SessionVariables
  }

instance J.ToJSON QueryCacheKey where
  toJSON (QueryCacheKey qs ur sess) =
    J.object ["query_string" J..= qs, "user_role" J..= ur, "session" J..= sess]

type CacheStoreResponse = Either CacheStoreFailure CacheStoreSuccess

data CacheStoreSuccess
  = CacheStoreSkipped
  | CacheStoreHit
  deriving (Eq, Show)

data CacheStoreFailure
  = CacheStoreLimitReached
  | CacheStoreNotEnoughCapacity
  | CacheStoreBackendError String
  deriving (Eq, Show)

class Monad m => MonadExecuteQuery m where
  -- | This method does two things: it looks up a query result in the
  -- server-side cache, if a cache is used, and it additionally returns HTTP
  -- headers that can instruct a client how long a response can be cached
  -- locally (i.e. client-side).
  cacheLookup ::
    -- | Used to check if the elaborated query supports caching
    [RemoteSchemaInfo] ->
    -- | Used to check if actions query supports caching (unsupported if `forward_client_headers` is set)
    [ActionsInfo] ->
    -- | Key that uniquely identifies the result of a query execution
    QueryCacheKey ->
    -- | Cached Directive from GraphQL query AST
    Maybe CachedDirective ->
    -- | HTTP headers to be sent back to the caller for this GraphQL request,
    -- containing e.g. time-to-live information, and a cached value if found and
    -- within time-to-live.  So a return value (non-empty-ttl-headers, Nothing)
    -- represents that we don't have a server-side cache of the query, but that
    -- the client should store it locally.  The value ([], Just json) represents
    -- that the client should not store the response locally, but we do have a
    -- server-side cache value that can be used to avoid query execution.
    TraceT (ExceptT QErr m) (HTTP.ResponseHeaders, Maybe EncJSON)

  -- | Store a json response for a query that we've executed in the cache.  Note
  -- that, as part of this, 'cacheStore' has to decide whether the response is
  -- cacheable.  A very similar decision is also made in 'cacheLookup', since it
  -- has to construct corresponding cache-enabling headers that are sent to the
  -- client.  But note that the HTTP headers influence client-side caching,
  -- whereas 'cacheStore' changes the server-side cache.
  cacheStore ::
    -- | Key under which to store the result of a query execution
    QueryCacheKey ->
    -- | Cached Directive from GraphQL query AST
    Maybe CachedDirective ->
    -- | Result of a query execution
    EncJSON ->
    -- | Always succeeds
    TraceT (ExceptT QErr m) CacheStoreResponse

  default cacheLookup ::
    (m ~ t n, MonadTrans t, MonadExecuteQuery n) =>
    [RemoteSchemaInfo] ->
    [ActionsInfo] ->
    QueryCacheKey ->
    Maybe CachedDirective ->
    TraceT (ExceptT QErr m) (HTTP.ResponseHeaders, Maybe EncJSON)
  cacheLookup a b c d = hoist (hoist lift) $ cacheLookup a b c d

  default cacheStore ::
    (m ~ t n, MonadTrans t, MonadExecuteQuery n) =>
    QueryCacheKey ->
    Maybe CachedDirective ->
    EncJSON ->
    TraceT (ExceptT QErr m) CacheStoreResponse
  cacheStore a b c = hoist (hoist lift) $ cacheStore a b c

instance MonadExecuteQuery m => MonadExecuteQuery (ReaderT r m)

instance MonadExecuteQuery m => MonadExecuteQuery (ExceptT r m)

instance MonadExecuteQuery m => MonadExecuteQuery (TraceT m)

instance MonadExecuteQuery m => MonadExecuteQuery (MetadataStorageT m)

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
  Either (Either GQExecError QErr) (RootFieldMap AnnotatedResponsePart) ->
  HTTP.ResponseHeaders ->
  m AnnotatedResponse
buildResponseFromParts telemType partsErr cacheHeaders =
  buildResponse telemType partsErr \parts ->
    let responseData = Right $ encJToLBS $ encodeAnnotatedResponseParts parts
     in AnnotatedResponse
          { arQueryType = telemType,
            arTimeIO = sum (fmap arpTimeIO parts),
            arLocality = foldMap arpLocality parts,
            arResponse =
              HttpResponse
                (Just responseData, encodeGQResp responseData)
                (cacheHeaders <> foldMap arpHeaders parts)
          }

buildResponse ::
  (MonadError QErr m) =>
  Telem.QueryType ->
  Either (Either GQExecError QErr) a ->
  (a -> AnnotatedResponse) ->
  m AnnotatedResponse
buildResponse telemType res f = case res of
  Right a -> pure $ f a
  Left (Right err) -> throwError err
  Left (Left err) ->
    pure $
      AnnotatedResponse
        { arQueryType = telemType,
          arTimeIO = 0,
          arLocality = Telem.Remote,
          arResponse =
            HttpResponse
              (Just (Left err), encodeGQResp $ Left err)
              []
        }

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
  RFRemote remote -> foldOf (traverse . _SessionPresetVariable . to match) remote
  RFAction actionQ -> foldMap remoteFieldPred actionQ
  RFRaw {} -> mempty
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
    MonadReader E.ExecutionCtx m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadTrace m,
    MonadExecuteQuery m,
    MonadMetadataStorage (MetadataStorageT m),
    EB.MonadQueryTags m,
    HasResourceLimits m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  RequestId ->
  UserInfo ->
  Wai.IpAddress ->
  [HTTP.Header] ->
  E.GraphQLQueryType ->
  GQLReqUnparsed ->
  m (GQLQueryOperationSuccessLog, HttpResponse (Maybe GQResponse, EncJSON))
runGQ env logger reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  (totalTime, (response, parameterizedQueryHash)) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx sc scVer httpManager enableAL readOnlyMode <- ask

    -- 1. Run system authorization on the 'reqUnparsed :: GQLReqUnparsed' query.
    reqParsed <-
      E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed reqId
        >>= flip onLeft throwError

    operationLimit <- askGraphqlOperationLimit
    let runLimits = runResourceLimits $ operationLimit userInfo (scApiLimits sc)

    -- 2. Construct an execution plan from 'reqParsed :: GQLParsed'.
    (parameterizedQueryHash, execPlan) <-
      E.getResolvedExecPlan
        env
        logger
        userInfo
        sqlGenCtx
        readOnlyMode
        sc
        scVer
        queryType
        httpManager
        reqHeaders
        (reqUnparsed, reqParsed)
        reqId

    -- 3. Execute the execution plan producing a 'AnnotatedResponse'.
    response <- executePlan httpManager reqParsed runLimits execPlan
    return (response, parameterizedQueryHash)

  recordTimings totalTime response
  let requestSize = LBS.length $ J.encode reqUnparsed
      responseSize = LBS.length $ encJToLBS $ snd $ _hrBody $ arResponse $ response
  -- 4. Return the response along with logging metadata.
  return
    ( GQLQueryOperationSuccessLog reqUnparsed totalTime responseSize requestSize parameterizedQueryHash,
      arResponse response
    )
  where
    doQErr :: ExceptT QErr m a -> ExceptT (Either GQExecError QErr) m a
    doQErr = withExceptT Right

    forWithKey = flip OMap.traverseWithKey

    executePlan ::
      HTTP.Manager ->
      GQLReqParsed ->
      (m AnnotatedResponse -> m AnnotatedResponse) ->
      E.ResolvedExecutionPlan ->
      m AnnotatedResponse
    executePlan httpManager reqParsed runLimits execPlan = case execPlan of
      E.QueryExecutionPlan queryPlans asts dirMap -> trace "Query" $ do
        -- Attempt to lookup a cached response in the query cache.
        -- 'keyedLookup' is a monadic action possibly returning a cache hit.
        -- 'keyedStore' is a function to write a new response to the cache.
        let (keyedLookup, keyedStore) = cacheAccess reqParsed queryPlans asts dirMap
        (cachingHeaders, cachedValue) <- keyedLookup
        case fmap decodeGQResp cachedValue of
          -- If we get a cache hit, annotate the response with metadata and return it.
          Just cachedResponseData -> do
            logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindCached
            pure $
              AnnotatedResponse
                { arQueryType = Telem.Query,
                  arTimeIO = 0,
                  arLocality = Telem.Local,
                  arResponse = HttpResponse cachedResponseData cachingHeaders
                }
          -- If we get a cache miss, we must run the query against the graphql engine.
          Nothing -> runLimits $ do
            -- 1. 'traverse' the 'ExecutionPlan' executing every step.
            conclusion <- runExceptT $ forWithKey queryPlans $ executeQueryStep httpManager
            -- 2. Construct an 'AnnotatedResponse' from the results of all steps in the 'ExecutionPlan'.
            result <- buildResponseFromParts Telem.Query conclusion cachingHeaders
            let response@(HttpResponse responseData _) = arResponse result
            -- 3. Cache the 'AnnotatedResponse'.
            cacheStoreRes <- keyedStore (snd responseData)
            let headers = case cacheStoreRes of
                  -- Note: Warning header format: "Warning: <warn-code> <warn-agent> <warn-text> [warn-date]"
                  -- See: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Warning
                  Right _ -> []
                  (Left CacheStoreLimitReached) -> [("warning", "199 - cache-store-size-limit-exceeded")]
                  (Left CacheStoreNotEnoughCapacity) -> [("warning", "199 - cache-store-capacity-exceeded")]
                  (Left (CacheStoreBackendError _)) -> [("warning", "199 - cache-store-error")]
             in -- 4. Return the response.
                pure $ result {arResponse = addHttpResponseHeaders headers response}
      E.MutationExecutionPlan mutationPlans -> runLimits $ do
        {- Note [Backwards-compatible transaction optimisation]

           For backwards compatibility, we perform the following optimisation: if all mutation steps
           are going to the same source, and that source is Postgres, we group all mutations as a
           transaction. This is a somewhat dangerous beaviour, and we would prefer, in the future,
           to make transactionality explicit rather than implicit and context-dependent.
        -}
        case coalescePostgresMutations mutationPlans of
          -- we are in the aforementioned case; we circumvent the normal process
          Just (sourceConfig, pgMutations) -> do
            res <-
              runExceptT $
                doQErr $
                  runPGMutationTransaction reqId reqUnparsed userInfo logger sourceConfig pgMutations
            -- we do not construct response parts since we have only one part
            buildResponse Telem.Mutation res \(telemTimeIO_DT, parts) ->
              let responseData = Right $ encJToLBS $ encodeEncJSONResults parts
               in AnnotatedResponse
                    { arQueryType = Telem.Mutation,
                      arTimeIO = telemTimeIO_DT,
                      arLocality = Telem.Local,
                      arResponse =
                        HttpResponse
                          (Just responseData, encodeGQResp responseData)
                          []
                    }

          -- we are not in the transaction case; proceeding normally
          Nothing -> do
            conclusion <- runExceptT $ forWithKey mutationPlans $ executeMutationStep httpManager
            buildResponseFromParts Telem.Mutation conclusion []
      E.SubscriptionExecutionPlan _sub ->
        throw400 UnexpectedPayload "subscriptions are not supported over HTTP, use websockets instead"

    executeQueryStep ::
      HTTP.Manager ->
      RootFieldAlias ->
      EB.ExecutionStep ->
      ExceptT (Either GQExecError QErr) m AnnotatedResponsePart
    executeQueryStep httpManager fieldName = \case
      E.ExecStepDB _headers exists remoteJoins -> doQErr $ do
        (telemTimeIO_DT, resp) <-
          AB.dispatchAnyBackend @BackendTransport
            exists
            \(EB.DBStepInfo _ sourceConfig genSql tx :: EB.DBStepInfo b) ->
              runDBQuery @b reqId reqUnparsed fieldName userInfo logger sourceConfig tx genSql
        finalResponse <-
          RJ.processRemoteJoins reqId logger env httpManager reqHeaders userInfo resp remoteJoins reqUnparsed
        pure $ AnnotatedResponsePart telemTimeIO_DT Telem.Local finalResponse []
      E.ExecStepRemote rsi resultCustomizer gqlReq remoteJoins -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindRemoteSchema
        runRemoteGQ httpManager fieldName rsi resultCustomizer gqlReq remoteJoins
      E.ExecStepAction aep _ remoteJoins -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindAction
        (time, resp) <- doQErr $ do
          (time, (resp, _)) <- EA.runActionExecution userInfo aep
          finalResponse <-
            RJ.processRemoteJoins reqId logger env httpManager reqHeaders userInfo resp remoteJoins reqUnparsed
          pure (time, finalResponse)
        pure $ AnnotatedResponsePart time Telem.Empty resp []
      E.ExecStepRaw json -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindIntrospection
        buildRaw json

    executeMutationStep ::
      HTTP.Manager ->
      RootFieldAlias ->
      EB.ExecutionStep ->
      ExceptT (Either GQExecError QErr) m AnnotatedResponsePart
    executeMutationStep httpManager fieldName = \case
      E.ExecStepDB responseHeaders exists remoteJoins -> doQErr $ do
        (telemTimeIO_DT, resp) <-
          AB.dispatchAnyBackend @BackendTransport
            exists
            \(EB.DBStepInfo _ sourceConfig genSql tx :: EB.DBStepInfo b) ->
              runDBMutation @b reqId reqUnparsed fieldName userInfo logger sourceConfig tx genSql
        finalResponse <-
          RJ.processRemoteJoins reqId logger env httpManager reqHeaders userInfo resp remoteJoins reqUnparsed
        pure $ AnnotatedResponsePart telemTimeIO_DT Telem.Local finalResponse responseHeaders
      E.ExecStepRemote rsi resultCustomizer gqlReq remoteJoins -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindRemoteSchema
        runRemoteGQ httpManager fieldName rsi resultCustomizer gqlReq remoteJoins
      E.ExecStepAction aep _ remoteJoins -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindAction
        (time, (resp, hdrs)) <- doQErr $ do
          (time, (resp, hdrs)) <- EA.runActionExecution userInfo aep
          finalResponse <-
            RJ.processRemoteJoins reqId logger env httpManager reqHeaders userInfo resp remoteJoins reqUnparsed
          pure (time, (finalResponse, hdrs))
        pure $ AnnotatedResponsePart time Telem.Empty resp $ fromMaybe [] hdrs
      E.ExecStepRaw json -> do
        logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindIntrospection
        buildRaw json

    runRemoteGQ httpManager fieldName rsi resultCustomizer gqlReq remoteJoins = do
      (telemTimeIO_DT, remoteResponseHeaders, resp) <-
        doQErr $ E.execRemoteGQ env httpManager userInfo reqHeaders (rsDef rsi) gqlReq
      value <- extractFieldFromResponse fieldName resultCustomizer resp
      finalResponse <-
        doQErr $
          RJ.processRemoteJoins
            reqId
            logger
            env
            httpManager
            reqHeaders
            userInfo
            -- TODO: avoid encode and decode here
            (encJFromOrderedValue value)
            remoteJoins
            reqUnparsed
      let filteredHeaders = filter ((== "Set-Cookie") . fst) remoteResponseHeaders
      pure $ AnnotatedResponsePart telemTimeIO_DT Telem.Remote finalResponse filteredHeaders

    cacheAccess ::
      GQLReqParsed ->
      EB.ExecutionPlan ->
      [QueryRootField UnpreparedValue] ->
      DirectiveMap ->
      ( m (HTTP.ResponseHeaders, Maybe EncJSON),
        EncJSON -> m CacheStoreResponse
      )
    cacheAccess reqParsed queryPlans asts dirMap =
      let filteredSessionVars = runSessVarPred (filterVariablesFromQuery asts) (_uiSession userInfo)
          remoteSchemas =
            OMap.elems queryPlans >>= \case
              E.ExecStepDB _headers _dbAST remoteJoins -> do
                maybe [] (map RJ._rsjRemoteSchema . RJ.getRemoteSchemaJoins) remoteJoins
              _ -> []
          getExecStepActionWithActionInfo acc execStep = case execStep of
            EB.ExecStepAction _ actionInfo _remoteJoins -> (actionInfo : acc)
            _ -> acc
          actionsInfo =
            foldl getExecStepActionWithActionInfo [] $
              OMap.elems $
                OMap.filter
                  ( \case
                      E.ExecStepAction _ _ _remoteJoins -> True
                      _ -> False
                  )
                  queryPlans
          cacheKey = QueryCacheKey reqParsed (_uiRole userInfo) filteredSessionVars
          cachedDirective = runIdentity <$> DM.lookup cached dirMap
       in ( Tracing.interpTraceT (liftEitherM . runExceptT) $
              cacheLookup remoteSchemas actionsInfo cacheKey cachedDirective,
            Tracing.interpTraceT (liftEitherM . runExceptT)
              . cacheStore cacheKey cachedDirective
          )

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

coalescePostgresMutations ::
  EB.ExecutionPlan ->
  Maybe
    ( SourceConfig ('Postgres 'Vanilla),
      InsOrdHashMap RootFieldAlias (EB.DBStepInfo ('Postgres 'Vanilla))
    )
coalescePostgresMutations plan = do
  -- we extract the name and config of the first mutation root, if any
  (oneSourceName, oneSourceConfig) <- case toList plan of
    (E.ExecStepDB _ exists _remoteJoins : _) ->
      AB.unpackAnyBackend @('Postgres 'Vanilla) exists <&> \dbsi ->
        ( EB.dbsiSourceName dbsi,
          EB.dbsiSourceConfig dbsi
        )
    _ -> Nothing
  -- we then test whether all mutations are going to that same first source
  -- and that it is Postgres
  mutations <- for plan \case
    E.ExecStepDB _ exists remoteJoins -> do
      dbStepInfo <- AB.unpackAnyBackend @('Postgres 'Vanilla) exists
      guard $ oneSourceName == EB.dbsiSourceName dbStepInfo && isNothing remoteJoins
      Just dbStepInfo
    _ -> Nothing
  Just (oneSourceConfig, mutations)

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
  Monad m =>
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
    onNothing (JO.lookup fieldName' dataObj) $
      do400 $ "expecting key " <> fieldName'
  return fieldVal
  where
    do400 = withExceptT Right . throw400 RemoteSchemaError
    doGQExecError = withExceptT Left . throwError . GQExecError

buildRaw :: Applicative m => JO.Value -> m AnnotatedResponsePart
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
    encNameMap = encJFromInsOrdHashMap . OMap.mapKeys G.unName

-- | Run (execute) a batched GraphQL query (see 'GQLBatchedReqs').
runGQBatched ::
  forall m.
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadReader E.ExecutionCtx m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadTrace m,
    MonadExecuteQuery m,
    HttpLog m,
    MonadMetadataStorage (MetadataStorageT m),
    EB.MonadQueryTags m,
    HasResourceLimits m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  RequestId ->
  ResponseInternalErrorsConfig ->
  UserInfo ->
  Wai.IpAddress ->
  [HTTP.Header] ->
  E.GraphQLQueryType ->
  -- | the batched request with unparsed GraphQL query
  GQLBatchedReqs (GQLReq GQLQueryText) ->
  m (HttpLogMetadata m, HttpResponse EncJSON)
runGQBatched env logger reqId responseErrorsConfig userInfo ipAddress reqHdrs queryType query =
  case query of
    GQLSingleRequest req -> do
      (gqlQueryOperationLog, httpResp) <- runGQ env logger reqId userInfo ipAddress reqHdrs queryType req
      let httpLoggingMetadata = buildHttpLogMetadata @m (PQHSetSingleton (gqolParameterizedQueryHash gqlQueryOperationLog)) L.RequestModeSingle (Just (GQLSingleRequest (GQLQueryOperationSuccess gqlQueryOperationLog)))
      pure (httpLoggingMetadata, snd <$> httpResp)
    GQLBatchedReqs reqs -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let includeInternal = shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig
          removeHeaders =
            flip HttpResponse []
              . encJFromList
              . map (either (encJFromJValue . encodeGQErr includeInternal) _hrBody)
      responses <- traverse (\req -> fmap (req,) . try . (fmap . fmap . fmap) snd . runGQ env logger reqId userInfo ipAddress reqHdrs queryType $ req) reqs
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
          httpLoggingMetadata = buildHttpLogMetadata @m (PQHSetBatched parameterizedQueryHashes) L.RequestModeBatched (Just (GQLBatchedReqs batchOperationLogs))
      pure (httpLoggingMetadata, removeHeaders (map ((fmap snd) . snd) responses))
  where
    try = flip catchError (pure . Left) . fmap Right
