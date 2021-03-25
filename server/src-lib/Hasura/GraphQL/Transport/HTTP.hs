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

  , SessVarPred
  , filterVariablesFromQuery
  , runSessVarPred
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Ordered                     as JO
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.Text                              as T
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai

import           Control.Lens                           (toListOf)
import           Control.Monad.Morph                    (hoist)
import           Control.Monad.Trans.Control            (MonadBaseControl)

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.Action          as EA
import qualified Hasura.GraphQL.Execute.Backend         as EB
import qualified Hasura.Logging                         as L
import qualified Hasura.SQL.AnyBackend                  as AB
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Hasura.Tracing                         as Tracing

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Logging                 (MonadQueryLog)
import           Hasura.GraphQL.Parser.Column           (UnpreparedValue (..))
import           Hasura.GraphQL.Transport.Backend
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.Instances     ()
import           Hasura.HTTP
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Init.Config
import           Hasura.Server.Types                    (RequestId)
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import           Hasura.Tracing                         (MonadTrace, TraceT, trace)


data QueryCacheKey = QueryCacheKey
  { qckQueryString :: !GQLReqParsed
  , qckUserRole    :: !RoleName
  , qckSession     :: !SessionVariables
  }

instance J.ToJSON QueryCacheKey where
  toJSON (QueryCacheKey qs ur sess) =
    J.object ["query_string" J..= qs, "user_role" J..= ur, "session" J..= sess]


class Monad m => MonadExecuteQuery m where
  -- | This method does two things: it looks up a query result in the
  -- server-side cache, if a cache is used, and it additionally returns HTTP
  -- headers that can instruct a client how long a response can be cached
  -- locally (i.e. client-side).
  cacheLookup
    :: [RemoteSchemaInfo]
    -- ^ Used to check if the elaborated query supports caching
    -> QueryCacheKey
    -- ^ Key that uniquely identifies the result of a query execution
    -> TraceT (ExceptT QErr m) (HTTP.ResponseHeaders, Maybe EncJSON)
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
    -> TraceT (ExceptT QErr m) ()
    -- ^ Always succeeds

instance MonadExecuteQuery m => MonadExecuteQuery (ReaderT r m) where
  cacheLookup a b = hoist (hoist lift) $ cacheLookup a b
  cacheStore  a b = hoist (hoist lift) $ cacheStore  a b

instance MonadExecuteQuery m => MonadExecuteQuery (ExceptT r m) where
  cacheLookup a b = hoist (hoist lift) $ cacheLookup a b
  cacheStore  a b = hoist (hoist lift) $ cacheStore  a b

instance MonadExecuteQuery m => MonadExecuteQuery (TraceT m) where
  cacheLookup a b = hoist (hoist lift) $ cacheLookup a b
  cacheStore  a b = hoist (hoist lift) $ cacheStore  a b

instance MonadExecuteQuery m => MonadExecuteQuery (MetadataStorageT m) where
  cacheLookup a b = hoist (hoist lift) $ cacheLookup a b
  cacheStore  a b = hoist (hoist lift) $ cacheStore  a b

-- | A partial result, e.g. from a remote schema or postgres, which we'll
-- assemble into the final result for the client.
--
-- Nothing to do with graphql fragments...
data ResultsFragment = ResultsFragment
  { rfTimeIO   :: DiffTime
  , rfLocality :: Telem.Locality
  , rfResponse :: EncJSON
  , rfHeaders  :: HTTP.ResponseHeaders
  }

-- | A predicate on session variables. The 'Monoid' instance makes it simple
-- to combine several predicates disjunctively.
newtype SessVarPred = SessVarPred { unSessVarPred :: SessionVariable -> SessionVariableValue -> Bool }

keepAllSessionVariables :: SessVarPred
keepAllSessionVariables = SessVarPred $ \_ _ -> True

instance Semigroup SessVarPred where
  SessVarPred p1 <> SessVarPred p2 = SessVarPred $ \sv svv ->
    p1 sv svv || p2 sv svv

instance Monoid SessVarPred where
  mempty = SessVarPred $ \_ _ -> False

runSessVarPred :: SessVarPred -> SessionVariables -> SessionVariables
runSessVarPred = filterSessionVariables . unSessVarPred

-- | Filter out only those session variables used by the query AST provided
filterVariablesFromQuery
  :: Backend backend
  => [RootField (QueryDBRoot UnpreparedValue) c (ActionQuery backend (UnpreparedValue bet)) d]
  -> SessVarPred
filterVariablesFromQuery query = fold $ rootToSessVarPreds =<< query
  where
    rootToSessVarPreds = \case
      RFDB _ exists ->
        AB.dispatchAnyBackend @Backend exists \case
          SourceConfigWith _ (QDBR db) -> toPred <$> toListOf traverseQueryDB db
      RFAction actionQ -> toPred <$> toListOf traverseActionQuery actionQ
      _ -> []

    toPred :: UnpreparedValue bet -> SessVarPred
    -- if we see a reference to the whole session variables object,
    -- then we need to keep everything:
    toPred UVSession               = keepAllSessionVariables
    -- if we only see a specific session variable, we only need to keep that one:
    toPred (UVSessionVar _type sv) = SessVarPred $ \sv' _ -> sv == sv'
    toPred _                       = mempty

-- | Run (execute) a single GraphQL query
runGQ
  :: forall m
   . ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , MonadTrace m
     , MonadExecuteQuery m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> RequestId
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLReqUnparsed
  -> m (HttpResponse (Maybe GQResponse, EncJSON))
runGQ env logger reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  (telemTimeTot_DT, (telemCacheHit, (telemQueryType, telemTimeIO_DT, telemLocality, resp))) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx {- planCache -} sc scVer httpManager enableAL <- ask

    -- run system authorization on the GraphQL API
    reqParsed <- E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed
                 >>= flip onLeft throwError

    (telemCacheHit, execPlan) <-
      E.getResolvedExecPlan
        env logger {- planCache -}
        userInfo sqlGenCtx sc scVer queryType
        httpManager reqHeaders (reqUnparsed, reqParsed)

    (telemCacheHit,) <$> case execPlan of
      E.QueryExecutionPlan queryPlans asts -> trace "Query" $ do
        let filteredSessionVars = runSessVarPred (filterVariablesFromQuery asts) (_uiSession userInfo)
            cacheKey = QueryCacheKey reqParsed (_uiRole userInfo) filteredSessionVars
            remoteJoins = OMap.elems queryPlans >>= \case
              E.ExecStepDB _headers exists ->
                AB.dispatchAnyBackend @BackendTransport exists EB.getRemoteSchemaInfo
              _ -> []
        (responseHeaders, cachedValue) <- Tracing.interpTraceT (liftEitherM . runExceptT) $ cacheLookup remoteJoins cacheKey
        case fmap decodeGQResp cachedValue of
          Just cachedResponseData ->
            pure (Telem.Query, 0, Telem.Local, HttpResponse cachedResponseData responseHeaders)
          Nothing -> do
            conclusion <- runExceptT $ forWithKey queryPlans $ \fieldName -> \case
              E.ExecStepDB _headers exists -> doQErr $ do
                (telemTimeIO_DT, resp) <-
                  AB.dispatchAnyBackend @BackendTransport exists
                    \(EB.DBStepInfo sourceConfig genSql tx) ->
                        runDBQuery
                          reqId
                          reqUnparsed
                          fieldName
                          userInfo
                          logger
                          sourceConfig
                          tx
                          genSql
                return $ ResultsFragment telemTimeIO_DT Telem.Local resp []
              E.ExecStepRemote rsi gqlReq ->
                runRemoteGQ httpManager fieldName rsi gqlReq
              E.ExecStepAction aep _ -> do
                (time, r) <- doQErr $ EA.runActionExecution aep
                pure $ ResultsFragment time Telem.Empty r []
              E.ExecStepRaw json ->
                buildRaw json
            out@(_, _, _, HttpResponse responseData _) <- buildResult Telem.Query conclusion responseHeaders
            Tracing.interpTraceT (liftEitherM . runExceptT) $ cacheStore cacheKey $ snd responseData
            pure out

      E.MutationExecutionPlan mutationPlans -> do
        conclusion <- runExceptT $ forWithKey mutationPlans $ \fieldName -> \case
          E.ExecStepDB responseHeaders exists -> doQErr $ do
            (telemTimeIO_DT, resp) <-
              AB.dispatchAnyBackend @BackendTransport exists
                \(EB.DBStepInfo sourceConfig genSql tx) ->
                    runDBMutation
                      reqId
                      reqUnparsed
                      fieldName
                      userInfo
                      logger
                      sourceConfig
                      tx
                      genSql
            return $ ResultsFragment telemTimeIO_DT Telem.Local resp responseHeaders
          E.ExecStepRemote rsi gqlReq ->
            runRemoteGQ httpManager fieldName rsi gqlReq
          E.ExecStepAction aep hdrs -> do
            (time, r) <- doQErr $ EA.runActionExecution aep
            pure $ ResultsFragment time Telem.Empty r hdrs
          E.ExecStepRaw json ->
            buildRaw json
        buildResult Telem.Mutation conclusion []

      E.SubscriptionExecutionPlan _sourceName _sub ->
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

    runRemoteGQ httpManager fieldName rsi gqlReq = do
      (telemTimeIO_DT, remoteResponseHeaders, resp) <-
        doQErr $ E.execRemoteGQ env httpManager userInfo reqHeaders rsi gqlReq
      value <- extractFieldFromResponse (G.unName fieldName) resp
      let filteredHeaders = filter ((== "Set-Cookie") . fst) remoteResponseHeaders
      pure $ ResultsFragment telemTimeIO_DT Telem.Remote (JO.toEncJSON value) filteredHeaders

    buildResult
      :: Telem.QueryType
      -> Either (Either GQExecError QErr) (InsOrdHashMap G.Name ResultsFragment)
      -> HTTP.ResponseHeaders
      -> m (Telem.QueryType, DiffTime, Telem.Locality, HttpResponse (Maybe GQResponse, EncJSON))
    buildResult telemType (Left (Left err)) _ = pure
      ( telemType , 0 , Telem.Remote , HttpResponse (Just (Left err), encodeGQResp $ Left err) [])
    buildResult _telemType (Left (Right err)) _ = throwError err
    buildResult telemType (Right results) cacheHeaders = do
      let responseData = pure $ encJToLBS $ encJFromInsOrdHashMap $ rfResponse <$> OMap.mapKeys G.unName results
      pure
        ( telemType
        , sum (fmap rfTimeIO results)
        , foldMap rfLocality results
        , HttpResponse
          (Just responseData, encodeGQResp responseData)
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
     , MonadBaseControl IO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , MonadTrace m
     , MonadExecuteQuery m
     , MonadMetadataStorage (MetadataStorageT m)
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
      (fmap . fmap) snd (runGQ env logger reqId userInfo ipAddress reqHdrs queryType req)
    GQLBatchedReqs reqs -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let includeInternal = shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig
          removeHeaders =
            flip HttpResponse []
            . encJFromList
            . map (either (encJFromJValue . encodeGQErr includeInternal) _hrBody)

      removeHeaders <$> traverse (try . (fmap . fmap) snd . runGQ env logger reqId userInfo ipAddress reqHdrs queryType) reqs
  where
    try = flip catchError (pure . Left) . fmap Right

