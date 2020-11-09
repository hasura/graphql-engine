-- | Execution of GraphQL queries over HTTP transport
{-# LANGUAGE RecordWildCards #-}
module Hasura.GraphQL.Transport.HTTP
  ( QueryCacheKey(..)
  , MonadExecuteQuery(..)
  , runGQ
  , runGQBatched
  , extractFieldFromResponse
  , buildRaw
  -- * imported from HTTP.Protocol; required by pro
  , GQLReq(..)
  , GQLReqUnparsed
  , GQLReqParsed
  , GQLExecDoc(..)
  , OperationName(..)
  , GQLQueryText(..)
  , ResultsFragment(..)
  ) where

import           Control.Monad.Morph                    (hoist)

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Logging                 (MonadQueryLog (..))
import           Hasura.GraphQL.Parser.Column           (UnpreparedValue)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Init.Config
import           Hasura.Server.Utils                    (RequestId)
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import           Hasura.Tracing                         (MonadTrace, TraceT, trace)

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Ordered                     as JO
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai

data QueryCacheKey = QueryCacheKey
  { qckQueryString :: !GQLReqParsed
  , qckUserRole    :: !RoleName
  }

instance J.ToJSON QueryCacheKey where
  toJSON (QueryCacheKey qs ur ) =
    J.object ["query_string" J..= qs, "user_role" J..= ur]


class Monad m => MonadExecuteQuery m where
  -- | This method does two things: it looks up a query result in the
  -- server-side cache, if a cache is used, and it additionally returns HTTP
  -- headers that can instruct a client how long a response can be cached
  -- locally (i.e. client-side).
  cacheLookup
    :: [QueryRootField UnpreparedValue]
    -- ^ Used to check that the query is cacheable
    -> QueryCacheKey
    -- ^ Key that uniquely identifies the result of a query execution
    -> TraceT m (HTTP.ResponseHeaders, Maybe EncJSON)
    -- ^ HTTP headers to be sent back to the caller for this GraphQL request,
    -- containing e.g. time-to-live information, and a cached value if found and
    -- within time-to-live.  So a return value (non-empty-ttl-headers, Nothing)
    -- represents that we don't have a server-side cache of the query, but that
    -- the client should store it locally.  The value ([], Just json) represents
    -- that the client should not store the response locally, but we do have a
    -- server-side cache value that can be used to avoid query execution.

  -- | Store a json response for a query that we've executed in the cache.  Note
  -- that, as part of this, 'cacheStore' has to decide whether the response is
  -- cacheable.  A very similar decision is also made in 'cacheLookup', since it
  -- has to construct corresponding cache-enabling headers that are sent to the
  -- client.  But note that the HTTP headers influence client-side caching,
  -- whereas 'cacheStore' changes the server-side cache.
  cacheStore
    :: QueryCacheKey
    -- ^ Key under which to store the result of a query execution
    -> EncJSON
    -- ^ Result of a query execution
    -> TraceT m ()
    -- ^ Always succeeds

instance MonadExecuteQuery m => MonadExecuteQuery (ReaderT r m) where
  cacheLookup a b = hoist lift $ cacheLookup a b
  cacheStore  a b = hoist lift $ cacheStore  a b

instance MonadExecuteQuery m => MonadExecuteQuery (ExceptT r m) where
  cacheLookup a b = hoist lift $ cacheLookup a b
  cacheStore  a b = hoist lift $ cacheStore  a b

instance MonadExecuteQuery m => MonadExecuteQuery (TraceT m) where
  cacheLookup a b = hoist lift $ cacheLookup a b
  cacheStore  a b = hoist lift $ cacheStore  a b

data ResultsFragment = ResultsFragment
  { rfTimeIO   :: DiffTime
  , rfLocality :: Telem.Locality
  , rfResponse :: EncJSON
  , rfHeaders  :: HTTP.ResponseHeaders
  }

-- | Run (execute) a single GraphQL query
runGQ
  :: forall m
   . ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , MonadTrace m
     , MonadExecuteQuery m
     , EQ.MonadQueryInstrumentation m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> RequestId
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLReqUnparsed
  -> m (HttpResponse EncJSON)
runGQ env logger reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  (telemTimeTot_DT, (telemCacheHit, (telemQueryType, telemTimeIO_DT, telemLocality, resp))) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx pgExecCtx {- planCache -} sc scVer httpManager enableAL <- ask

    -- run system authorization on the GraphQL API
    reqParsed <- E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed
                 >>= flip onLeft throwError

    (telemCacheHit, execPlan) <- E.getResolvedExecPlan env logger pgExecCtx {- planCache -}
                                 userInfo sqlGenCtx sc scVer queryType
                                 httpManager reqHeaders (reqUnparsed, reqParsed)
    (telemCacheHit,) <$> case execPlan of
      E.QueryExecutionPlan queryPlans asts -> trace "Query" $ do
        let cacheKey = QueryCacheKey reqParsed $ _uiRole userInfo
        (responseHeaders, cachedValue) <- Tracing.interpTraceT id $ cacheLookup asts cacheKey
        case cachedValue of
          Just cachedResponseData ->
            pure (Telem.Query, 0, Telem.Local, HttpResponse cachedResponseData responseHeaders)
          Nothing -> do
            conclusion <- runExceptT $ forWithKey queryPlans $ \fieldName -> \case
              E.ExecStepDB (tx, genSql) -> doQErr $ do
                (telemTimeIO_DT, resp) <-
                  runQueryDB reqId reqUnparsed fieldName tx genSql
                return $ ResultsFragment telemTimeIO_DT Telem.Local resp []
              E.ExecStepRemote (rsi, opDef, varValsM) ->
                runRemoteGQ fieldName rsi opDef varValsM
              E.ExecStepRaw json ->
                buildRaw json
            out@(_, _, _, HttpResponse responseData _) <- buildResult Telem.Query conclusion responseHeaders
            Tracing.interpTraceT id $ cacheStore cacheKey responseData
            pure out

      E.MutationExecutionPlan mutationPlans -> do
        conclusion <- runExceptT $ forWithKey mutationPlans $ \fieldName -> \case
          E.ExecStepDB (tx, responseHeaders) -> doQErr $ do
            (telemTimeIO_DT, resp) <- runMutationDB reqId reqUnparsed userInfo tx
            return $ ResultsFragment telemTimeIO_DT Telem.Local resp responseHeaders
          E.ExecStepRemote (rsi, opDef, varValsM) ->
            runRemoteGQ fieldName rsi opDef varValsM
          E.ExecStepRaw json ->
            buildRaw json
        buildResult Telem.Mutation conclusion []

      E.SubscriptionExecutionPlan _sub ->
        throw400 UnexpectedPayload "subscriptions are not supported over HTTP, use websockets instead"
  -- The response and misc telemetry data:
  let telemTimeIO = convertDuration telemTimeIO_DT
      telemTimeTot = convertDuration telemTimeTot_DT
      telemTransport = Telem.HTTP
  Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return resp
  where
    doQErr = withExceptT Right

    forWithKey = flip OMap.traverseWithKey

    runRemoteGQ fieldName rsi opDef varValsM = do
      (telemTimeIO_DT, HttpResponse resp remoteResponseHeaders) <-
        doQErr $ E.execRemoteGQ env reqId userInfo reqHeaders rsi opDef varValsM
      value <- extractFieldFromResponse (G.unName fieldName) $ encJToLBS resp
      let filteredHeaders = filter ((== "Set-Cookie") . fst) remoteResponseHeaders
      pure $ ResultsFragment telemTimeIO_DT Telem.Remote (JO.toEncJSON value) filteredHeaders

    buildResult telemType (Left (Left err)) _ = pure
      ( telemType
      , 0
      , Telem.Remote
      , HttpResponse (encodeGQResp $ throwError err) []
      )
    buildResult _telemType (Left (Right err)) _ = throwError err
    buildResult telemType (Right results) cacheHeaders = do
      let responseData = encodeGQResp $ pure $ encJToLBS $ encJFromInsOrdHashMap $ rfResponse <$> OMap.mapKeys G.unName results
      pure
        ( telemType
        , sum (fmap rfTimeIO results)
        , foldMap rfLocality results
        , HttpResponse
          responseData
          (cacheHeaders <> foldMap rfHeaders results)
        )

extractFieldFromResponse
  :: Monad m => Text -> LBS.ByteString -> ExceptT (Either GQExecError QErr) m JO.Value
extractFieldFromResponse fieldName bs = do
  val <- onLeft (JO.eitherDecode bs) $ do400 . T.pack
  valObj <- onLeft (JO.asObject val) do400
  dataVal <- case JO.toList valObj of
    [("data", v)] -> pure v
    _ -> case JO.lookup "errors" valObj of
      Just (JO.Array err) -> doGQExecError $ toList $ fmap JO.fromOrdered err
      _                   -> do400 "Received invalid JSON value from remote"
  dataObj <- onLeft (JO.asObject dataVal) do400
  fieldVal <- onNothing (JO.lookup fieldName dataObj) $
    do400 $ "expecting key " <> fieldName
  return fieldVal
  where
    do400 = withExceptT Right . throw400 RemoteSchemaError
    doGQExecError = withExceptT Left . throwError . GQExecError

buildRaw :: Applicative m => J.Value -> m ResultsFragment
buildRaw json = do
  let obj = encJFromJValue json
      telemTimeIO_DT = 0
  pure $ ResultsFragment telemTimeIO_DT Telem.Local obj []

-- | Run (execute) a batched GraphQL query (see 'GQLBatchedReqs')
runGQBatched
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , MonadTrace m
     , MonadExecuteQuery m
     , EQ.MonadQueryInstrumentation m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> RequestId
  -> ResponseInternalErrorsConfig
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLBatchedReqs GQLQueryText
  -- ^ the batched request with unparsed GraphQL query
  -> m (HttpResponse EncJSON)
runGQBatched env logger reqId responseErrorsConfig userInfo ipAddress reqHdrs queryType query =
  case query of
    GQLSingleRequest req ->
      runGQ env logger reqId userInfo ipAddress reqHdrs queryType req
    GQLBatchedReqs reqs -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let includeInternal = shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig
          removeHeaders =
            flip HttpResponse []
            . encJFromList
            . map (either (encJFromJValue . encodeGQErr includeInternal) _hrBody)

      removeHeaders <$> traverse (try . runGQ env logger reqId userInfo ipAddress reqHdrs queryType) reqs
  where
    try = flip catchError (pure . Left) . fmap Right


runQueryDB
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , MonadQueryLog m
     , MonadTrace m
     )
  => RequestId
  -> GQLReqUnparsed
  -> G.Name -- ^ name of the root field we're fetching
  -> Tracing.TraceT (LazyTxT QErr IO) EncJSON
  -> Maybe EQ.PreparedSql
  -> m (DiffTime, EncJSON)
  -- ^ Also return the time spent in the PG query; for telemetry.
runQueryDB reqId query fieldName tx genSql =  do
  -- log the generated SQL and the graphql query
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ <- ask
  logQueryLog logger query ((fieldName,) <$> genSql) reqId
  withElapsedTime $ trace ("Postgres Query for root field " <> G.unName fieldName) $
    Tracing.interpTraceT id $ hoist (runQueryTx pgExecCtx) tx

runMutationDB
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , MonadQueryLog m
     , MonadTrace m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> Tracing.TraceT (LazyTxT QErr IO) EncJSON
  -> m (DiffTime, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runMutationDB reqId query userInfo tx =  do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ <- ask
  -- log the graphql query
  logQueryLog logger query Nothing reqId
  ctx <- Tracing.currentContext
  withElapsedTime $ trace "Mutation" $
    Tracing.interpTraceT (
      liftEitherM . liftIO . runExceptT
      . runLazyTx pgExecCtx Q.ReadWrite
      . withTraceContext ctx
      . withUserInfo userInfo
      )  tx
