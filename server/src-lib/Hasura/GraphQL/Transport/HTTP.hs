-- | Execution of GraphQL queries over HTTP transport

module Hasura.GraphQL.Transport.HTTP
  ( QueryCacheKey(..)
  , MonadExecuteQuery(..)
  , CachedDirective(..)
  , runGQ
  , runGQBatched
  , coalescePostgresMutations
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

import qualified Data.Aeson                                   as J
import qualified Data.Aeson.Ordered                           as JO
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.Dependent.Map                           as DM
import qualified Data.Environment                             as Env
import qualified Data.HashMap.Strict.InsOrd                   as OMap
import qualified Data.Text                                    as T
import qualified Language.GraphQL.Draft.Syntax                as G
import qualified Network.HTTP.Types                           as HTTP
import qualified Network.Wai.Extended                         as Wai

import           Control.Lens                                 (Traversal', toListOf)
import           Control.Monad.Morph                          (hoist)
import           Control.Monad.Trans.Control                  (MonadBaseControl)

import qualified Hasura.GraphQL.Execute                       as E
import qualified Hasura.GraphQL.Execute.Action                as EA
import qualified Hasura.GraphQL.Execute.Backend               as EB
import qualified Hasura.GraphQL.Execute.RemoteJoin            as RJ
import qualified Hasura.Logging                               as L
import qualified Hasura.SQL.AnyBackend                        as AB
import qualified Hasura.Server.Logging                        as L
import qualified Hasura.Server.Telemetry.Counters             as Telem
import qualified Hasura.Tracing                               as Tracing

import           Hasura.Backends.Postgres.Instances.Transport (runPGMutationTransaction)
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Logging                       (MonadQueryLog (logQueryLog),
                                                               QueryLog (..), QueryLogKind (..))
import           Hasura.GraphQL.ParameterizedQueryHash
import           Hasura.GraphQL.Parser.Column                 (UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Directives             (CachedDirective (..), cached)
import           Hasura.GraphQL.Transport.Backend
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.Instances           ()
import           Hasura.HTTP
import           Hasura.Metadata.Class
import           Hasura.RQL.IR
import           Hasura.RQL.Types
import           Hasura.Server.Init.Config
import           Hasura.Server.Logging
import           Hasura.Server.Types                          (RequestId)
import           Hasura.Server.Version                        (HasVersion)
import           Hasura.Session
import           Hasura.Tracing                               (MonadTrace, TraceT, trace)


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
    -> [ActionsInfo]
    -- ^ Used to check if actions query supports caching (unsupported if `forward_client_headers` is set)
    -> QueryCacheKey
    -- ^ Key that uniquely identifies the result of a query execution
    -> Maybe CachedDirective
    -- ^ Cached Directive from GraphQL query AST
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
    -> Maybe CachedDirective
    -- ^ Cached Directive from GraphQL query AST
    -> EncJSON
    -- ^ Result of a query execution
    -> TraceT (ExceptT QErr m) ()
    -- ^ Always succeeds

  default cacheLookup :: (m ~ t n, MonadTrans t, MonadExecuteQuery n) =>
    [RemoteSchemaInfo] -> [ActionsInfo] -> QueryCacheKey -> Maybe CachedDirective -> TraceT (ExceptT QErr m) (HTTP.ResponseHeaders, Maybe EncJSON)
  cacheLookup a b c d = hoist (hoist lift) $ cacheLookup a b c d

  default cacheStore :: (m ~ t n, MonadTrans t, MonadExecuteQuery n) =>
    QueryCacheKey -> Maybe CachedDirective -> EncJSON -> TraceT (ExceptT QErr m) ()
  cacheStore a b c = hoist (hoist lift) $ cacheStore  a b c


instance MonadExecuteQuery m => MonadExecuteQuery (ReaderT r m)
instance MonadExecuteQuery m => MonadExecuteQuery (ExceptT r m)
instance MonadExecuteQuery m => MonadExecuteQuery (TraceT m)
instance MonadExecuteQuery m => MonadExecuteQuery (MetadataStorageT m)

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
  => [RootField (QueryDBRoot UnpreparedValue UnpreparedValue) RemoteField (ActionQuery backend UnpreparedValue (UnpreparedValue backend)) d]
  -> SessVarPred
filterVariablesFromQuery query = fold $ rootToSessVarPreds =<< query
  where
    rootToSessVarPreds = \case
      RFDB _ exists ->
        AB.dispatchAnyBackend @Backend exists \case
          SourceConfigWith _ (QDBR db) -> toPred <$> toListOf traverse db
      RFRemote remote -> match <$> toListOf (traverse . _SessionPresetVariable) remote
      RFAction actionQ -> toPred <$> toListOf traverse actionQ
      _ -> []

    _SessionPresetVariable :: Traversal' RemoteSchemaVariable SessionVariable
    _SessionPresetVariable f (SessionPresetVariable a b c) =
      (\a' -> SessionPresetVariable a' b c) <$> f a
    _SessionPresetVariable _ x = pure x

    toPred :: UnpreparedValue bet -> SessVarPred
    -- if we see a reference to the whole session variables object,
    -- then we need to keep everything:
    toPred UVSession               = keepAllSessionVariables
    -- if we only see a specific session variable, we only need to keep that one:
    toPred (UVSessionVar _type sv) = match sv
    toPred _                       = mempty

    match :: SessionVariable -> SessVarPred
    match sv = SessVarPred $ \sv' _ -> sv == sv'

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
  -> m (ParameterizedQueryHash, HttpResponse (Maybe GQResponse, EncJSON))
runGQ env logger reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  (telemTimeTot_DT, (telemCacheHit, (telemQueryType, telemTimeIO_DT, telemLocality, resp, parameterizedQueryHash))) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx {- planCache -} sc scVer httpManager enableAL <- ask

    -- run system authorization on the GraphQL API
    reqParsed <- E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed
                 >>= flip onLeft throwError

    (telemCacheHit, (parameterizedQueryHash, execPlan)) <-
      E.getResolvedExecPlan
        env logger {- planCache -}
        userInfo sqlGenCtx sc scVer queryType
        httpManager reqHeaders (reqUnparsed, reqParsed)

    (telemCacheHit,) <$> case execPlan of
      E.QueryExecutionPlan queryPlans asts dirMap -> trace "Query" $ do
        let filteredSessionVars = runSessVarPred (filterVariablesFromQuery asts) (_uiSession userInfo)
            cacheKey = QueryCacheKey reqParsed (_uiRole userInfo) filteredSessionVars
            remoteSchemas = OMap.elems queryPlans >>= \case
              E.ExecStepDB _headers _dbAST remoteJoins -> do
                concatMap (map RJ._rjRemoteSchema . toList . snd) $
                  maybe [] toList remoteJoins
              _ -> []
            actionsInfo = foldl getExecStepActionWithActionInfo [] $ OMap.elems $ OMap.filter (\case
              E.ExecStepAction _ _ _remoteJoins -> True
              _                                 -> False
              ) queryPlans
            cachedDirective = runIdentity <$> DM.lookup cached dirMap

        (responseHeaders, cachedValue) <- Tracing.interpTraceT (liftEitherM . runExceptT) $ cacheLookup remoteSchemas actionsInfo cacheKey cachedDirective
        case fmap decodeGQResp cachedValue of
          Just cachedResponseData -> do
            logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindCached
            pure (Telem.Query, 0, Telem.Local, HttpResponse cachedResponseData responseHeaders, parameterizedQueryHash)

          Nothing -> do
            conclusion <- runExceptT $ forWithKey queryPlans $ \fieldName -> \case
              E.ExecStepDB _headers exists remoteJoins -> doQErr $ do
                (telemTimeIO_DT, resp) <-
                  AB.dispatchAnyBackend @BackendTransport exists
                    \(EB.DBStepInfo _ sourceConfig genSql tx :: EB.DBStepInfo b) ->
                        runDBQuery @b
                          reqId
                          reqUnparsed
                          fieldName
                          userInfo
                          logger
                          sourceConfig
                          tx
                          genSql
                finalResponse <-
                  maybe
                  (pure resp)
                  (RJ.processRemoteJoins env httpManager reqHeaders userInfo $ encJToLBS resp)
                  remoteJoins
                return $ ResultsFragment telemTimeIO_DT Telem.Local finalResponse []
              E.ExecStepRemote rsi gqlReq -> do
                logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindRemoteSchema
                runRemoteGQ httpManager fieldName rsi gqlReq
              E.ExecStepAction aep _ remoteJoins -> do
                logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindAction

                (time, (resp, _)) <- doQErr $ do
                  (time, (resp, hdrs)) <- EA.runActionExecution aep
                  finalResponse <-
                    maybe
                    (pure resp)
                    (RJ.processRemoteJoins env httpManager reqHeaders userInfo $ encJToLBS resp)
                    remoteJoins
                  pure (time, (finalResponse, hdrs))
                pure $ ResultsFragment time Telem.Empty resp []
              E.ExecStepRaw json -> do
                logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindIntrospection
                buildRaw json
            out@(_, _, _, HttpResponse responseData _, _) <-
              buildResultFromFragments Telem.Query conclusion responseHeaders parameterizedQueryHash
            Tracing.interpTraceT (liftEitherM . runExceptT) $ cacheStore cacheKey cachedDirective $ snd responseData
            pure out

      E.MutationExecutionPlan mutationPlans -> do
        {- Note [Backwards-compatible transaction optimisation]

           For backwards compatibility, we perform the following optimisation: if all mutation steps
           are going to the same source, and that source is Postgres, we group all mutations as a
           transaction. This is a somewhat dangerous beaviour, and we would prefer, in the future,
           to make transactionality explicit rather than implicit and context-dependent.
        -}
        case coalescePostgresMutations mutationPlans of
          -- we are in the aforementioned case; we circumvent the normal process
          Just (sourceConfig, pgMutations) -> do
            resp <- runExceptT $ doQErr $
              runPGMutationTransaction reqId reqUnparsed userInfo logger sourceConfig pgMutations
            -- we do not construct result fragments since we have only one result
            buildResult Telem.Mutation parameterizedQueryHash resp \(telemTimeIO_DT, results) ->
              let responseData = Right $ encJToLBS $ encJFromInsOrdHashMap $ OMap.mapKeys G.unName results
              in  ( Telem.Mutation
                  , telemTimeIO_DT
                  , Telem.Local
                  , HttpResponse
                    (Just responseData, encodeGQResp responseData)
                    []
                  , parameterizedQueryHash
                  )

          -- we are not in the transaction case; proceeding normally
          Nothing -> do
            conclusion <- runExceptT $ forWithKey mutationPlans $ \fieldName -> \case
              E.ExecStepDB responseHeaders exists remoteJoins -> doQErr $ do
                (telemTimeIO_DT, resp) <-
                  AB.dispatchAnyBackend @BackendTransport exists
                    \(EB.DBStepInfo _ sourceConfig genSql tx :: EB.DBStepInfo b) ->
                        runDBMutation @b
                          reqId
                          reqUnparsed
                          fieldName
                          userInfo
                          logger
                          sourceConfig
                          tx
                          genSql

                finalResponse <-
                  maybe
                  (pure resp)
                  (RJ.processRemoteJoins env httpManager reqHeaders userInfo $ encJToLBS resp)
                  remoteJoins
                return $ ResultsFragment telemTimeIO_DT Telem.Local finalResponse responseHeaders
              E.ExecStepRemote rsi gqlReq -> do
                logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindRemoteSchema
                runRemoteGQ httpManager fieldName rsi gqlReq
              E.ExecStepAction aep _ remoteJoins -> do
                logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindAction
                (time, (resp, hdrs)) <- doQErr $ do
                  (time, (resp, hdrs)) <- EA.runActionExecution aep
                  finalResponse <-
                    maybe
                    (pure resp)
                    (RJ.processRemoteJoins env httpManager reqHeaders userInfo $ encJToLBS resp)
                    remoteJoins
                  pure (time, (finalResponse, hdrs))
                pure $ ResultsFragment time Telem.Empty resp $ fromMaybe [] hdrs
              E.ExecStepRaw json -> do
                logQueryLog logger $ QueryLog reqUnparsed Nothing reqId QueryLogKindIntrospection
                buildRaw json
            buildResultFromFragments Telem.Mutation conclusion [] parameterizedQueryHash

      E.SubscriptionExecutionPlan _sub ->
        throw400 UnexpectedPayload "subscriptions are not supported over HTTP, use websockets instead"
  -- The response and misc telemetry data:
  let telemTimeIO = convertDuration telemTimeIO_DT
      telemTimeTot = convertDuration telemTimeTot_DT
      telemTransport = Telem.HTTP
  Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return (parameterizedQueryHash, resp)
  where
    getExecStepActionWithActionInfo acc execStep = case execStep of
       EB.ExecStepAction _ actionInfo _remoteJoins -> (actionInfo:acc)
       _                                           -> acc

    doQErr = withExceptT Right

    forWithKey = flip OMap.traverseWithKey

    runRemoteGQ httpManager fieldName rsi gqlReq = do
      (telemTimeIO_DT, remoteResponseHeaders, resp) <-
        doQErr $ E.execRemoteGQ env httpManager userInfo reqHeaders rsi gqlReq
      value <- extractFieldFromResponse (G.unName fieldName) resp
      let filteredHeaders = filter ((== "Set-Cookie") . fst) remoteResponseHeaders
      pure $ ResultsFragment telemTimeIO_DT Telem.Remote (JO.toEncJSON value) filteredHeaders

    buildResultFromFragments
       :: Telem.QueryType
       -> Either (Either GQExecError QErr) (InsOrdHashMap G.Name ResultsFragment)
       -> HTTP.ResponseHeaders
       -> ParameterizedQueryHash
       -> m ( Telem.QueryType
            , DiffTime
            , Telem.Locality
            , HttpResponse (Maybe GQResponse, EncJSON)
            , ParameterizedQueryHash
            )
    buildResultFromFragments telemType fragments cacheHeaders parameterizedQueryHash =
      buildResult telemType parameterizedQueryHash fragments \results ->
        let responseData = Right $ encJToLBS $ encJFromInsOrdHashMap $ rfResponse <$> OMap.mapKeys G.unName results
        in  ( telemType
            , sum (fmap rfTimeIO results)
            , foldMap rfLocality results
            , HttpResponse
              (Just responseData, encodeGQResp responseData)
              (cacheHeaders <> foldMap rfHeaders results)
            , parameterizedQueryHash
            )

    buildResult
      :: Telem.QueryType
      -> ParameterizedQueryHash
      -> Either (Either GQExecError QErr) a
      -> (a ->
           ( Telem.QueryType
           , DiffTime
           , Telem.Locality
           , HttpResponse (Maybe GQResponse, EncJSON)
           , ParameterizedQueryHash
           )
         )
      -> m ( Telem.QueryType
           , DiffTime
           , Telem.Locality
           , HttpResponse (Maybe GQResponse, EncJSON)
           , ParameterizedQueryHash
           )
    buildResult telemType parameterizedQueryHash result f = case result of
      Right a          -> pure $ f a
      Left (Right err) -> throwError err
      Left (Left  err) -> pure ( telemType
                               , 0
                               , Telem.Remote
                               , HttpResponse
                                   (Just (Left err), encodeGQResp $ Left err)
                                   []
                               , parameterizedQueryHash
                               )

coalescePostgresMutations
  :: EB.ExecutionPlan
  -> Maybe ( SourceConfig ('Postgres 'Vanilla)
           , InsOrdHashMap G.Name (EB.DBStepInfo ('Postgres 'Vanilla))
           )
coalescePostgresMutations plan = do
  -- we extract the name and config of the first mutation root, if any
  (oneSourceName, oneSourceConfig) <- case toList plan of
    (E.ExecStepDB _ exists _remoteJoins:_) -> AB.unpackAnyBackend @('Postgres 'Vanilla) exists <&> \dbsi ->
      ( EB.dbsiSourceName   dbsi
      , EB.dbsiSourceConfig dbsi
      )
    _                         -> Nothing
  -- we then test whether all mutations are going to that same first source
  -- and that it is Postgres
  mutations <- for plan \case
    E.ExecStepDB _ exists remoteJoins -> do
      dbStepInfo <- AB.unpackAnyBackend @('Postgres 'Vanilla) exists
      guard $ oneSourceName == EB.dbsiSourceName dbStepInfo && isNothing remoteJoins
      Just dbStepInfo
    _ -> Nothing
  Just (oneSourceConfig, mutations)

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

buildRaw :: Applicative m => JO.Value -> m ResultsFragment
buildRaw json = do
  let obj = JO.toEncJSON json
      telemTimeIO_DT = 0
  pure $ ResultsFragment telemTimeIO_DT Telem.Local obj []

-- | Run (execute) a batched GraphQL query (see 'GQLBatchedReqs').
runGQBatched
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
     , HttpLog m
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
  -> m (HttpLogMetadata m, HttpResponse EncJSON)
runGQBatched env logger reqId responseErrorsConfig userInfo ipAddress reqHdrs queryType query =
  case query of
    GQLSingleRequest req -> do
      (parameterizedQueryHash, httpResp) <- runGQ env logger reqId userInfo ipAddress reqHdrs queryType req
      let httpLoggingMetadata = buildHttpLogMetadata @m [parameterizedQueryHash] $ Just L.RequestSingle
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
      responses <- traverse (try . (fmap . fmap . fmap) snd . runGQ env logger reqId userInfo ipAddress reqHdrs queryType) reqs
      let httpLoggingMetadata = buildHttpLogMetadata @m (rights $ map (fmap fst) responses) $ Just L.RequestBatched
      pure (httpLoggingMetadata, removeHeaders (map (fmap snd) responses))
  where
    try = flip catchError (pure . Left) . fmap Right
