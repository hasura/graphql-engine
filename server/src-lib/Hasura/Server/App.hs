{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Server.App
  ( APIResp (JSONResp, RawResp),
    CEConsoleType (..),
    ConsoleRenderer (..),
    MonadVersionAPIWithExtraData (..),
    Handler,
    HandlerCtx (hcReqHeaders, hcAppContext, hcSchemaCache, hcUser),
    HasuraApp (HasuraApp),
    MonadConfigApiHandler (..),
    MonadGQLApiHandler (..),
    MonadMetadataApiAuthorization (..),
    AppContext (..),
    boolToText,
    ceConsoleTypeIdentifier,
    configApiGetHandler,
    isAdminSecretSet,
    mkGetHandler,
    mkSpockAction,
    mkWaiApp,
    onlyAdmin,
    renderHtmlTemplate,
    onlyWhenApiEnabled,
    v1GQHandler,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Exception (IOException, throwIO, try)
import Control.Exception.Lifted (ErrorCall (..), catch)
import Control.Monad.Morph (hoist)
import Control.Monad.Stateless
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson hiding (json)
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as J
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as S
import Data.Kind (Type)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Conversions (convertText)
import Data.Text.Extended
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Stats.Extended qualified as RTS
import Hasura.App.State
import Hasura.Backends.DataConnector.API (openApiSchema)
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Subscription.State qualified as ES
import Hasura.GraphQL.Explain qualified as GE
import Hasura.GraphQL.Logging (MonadExecutionLog, MonadQueryLog)
import Hasura.GraphQL.Transport.HTTP qualified as GH
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.GraphQL.Transport.WSServerApp qualified as WS
import Hasura.GraphQL.Transport.WebSocket qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Types qualified as WS
import Hasura.HTTP
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude hiding (get, put)
import Hasura.QueryTags
import Hasura.RQL.DDL.EventTrigger (MonadEventLogCleanup)
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Endpoint as EP
import Hasura.RQL.Types.OpenTelemetry (getOtelTracesPropagator)
import Hasura.RQL.Types.Roles (adminRoleName, roleNameToTxt)
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.Server.API.Config (runGetConfig)
import Hasura.Server.API.Metadata
import Hasura.Server.API.PGDump qualified as PGD
import Hasura.Server.API.Query
import Hasura.Server.API.V2Query qualified as V2Q
import Hasura.Server.AppStateRef
  ( AppStateRef,
    getAppContext,
    getRebuildableSchemaCacheWithVersion,
    getSchemaCache,
    withSchemaCacheReadUpdate,
  )
import Hasura.Server.Auth (AuthMode (..), UserAuthentication (..))
import Hasura.Server.Compression
import Hasura.Server.Init
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Middleware (corsMiddleware)
import Hasura.Server.OpenAPI (buildOpenAPI)
import Hasura.Server.Rest
import Hasura.Server.Types
import Hasura.Server.Utils
import Hasura.Server.Version
import Hasura.Services
import Hasura.Session (ExtraUserInfo (..), UserInfo (..), UserInfoM, askUserInfo)
import Hasura.Tracing (MonadTrace)
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Types qualified as HTTP
import Network.Mime (defaultMimeLookup)
import Network.Wai.Extended qualified as Wai
import Network.Wai.Handler.WebSockets.Custom qualified as WSC
import System.FilePath (isRelative, joinPath, splitExtension, takeFileName)
import System.Mem (performMajorGC)
import System.Metrics qualified as EKG
import System.Metrics.Json qualified as EKG
import Text.Mustache qualified as M
import Web.Spock.Action qualified as Spock
import Web.Spock.Core ((<//>))
import Web.Spock.Core qualified as Spock

data HandlerCtx = HandlerCtx
  { hcAppContext :: AppContext,
    hcSchemaCache :: RebuildableSchemaCache,
    hcUser :: UserInfo,
    hcReqHeaders :: [HTTP.Header],
    hcRequestId :: RequestId,
    hcSourceIpAddress :: Wai.IpAddress,
    hcLicenseKeyCache :: Maybe (CredentialCache AgentLicenseKey)
  }

newtype Handler m a = Handler (ReaderT HandlerCtx (ExceptT QErr m) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadBase b,
      MonadBaseControl b,
      MonadReader HandlerCtx,
      MonadError QErr,
      Tracing.MonadTraceContext,
      MonadTrace,
      HasAppEnv,
      HasCacheStaticConfig,
      HasFeatureFlagChecker,
      HasResourceLimits,
      MonadResolveSource,
      E.MonadGQLExecutionCheck,
      MonadEventLogCleanup,
      MonadQueryLog,
      MonadExecutionLog,
      MonadQueryTags,
      GH.MonadExecuteQuery,
      MonadMetadataApiAuthorization,
      MonadMetadataStorage,
      ProvidesNetwork,
      MonadGetPolicies
    )

instance MonadTrans Handler where
  lift = Handler . lift . lift

instance (Monad m) => UserInfoM (Handler m) where
  askUserInfo = asks hcUser

runHandler :: (MonadIO m, Tracing.MonadTraceContext m, HasResourceLimits m, MonadBaseControl IO m) => L.Logger L.Hasura -> HandlerCtx -> Handler m a -> m (Either QErr a)
runHandler logger ctx (Handler r) = do
  handlerLimit <- askHTTPHandlerLimit
  runExceptT (runReaderT (runResourceLimits handlerLimit r) ctx)
    `catch` \errorCallWithLoc@(ErrorCallWithLocation txt _) -> do
      L.unLoggerTracing logger $ L.UnhandledInternalErrorLog errorCallWithLoc
      pure
        $ throw500WithDetail "Internal Server Error"
        $ object [("error", fromString txt)]

data APIResp
  = JSONResp !(HttpResponse EncJSON)
  | RawResp !(HttpResponse BL.ByteString)

-- | API request handlers for different endpoints
data APIHandler m a where
  -- | A simple GET request
  AHGet :: !(Handler m (HttpLogGraphQLInfo, APIResp)) -> APIHandler m void
  -- | A simple POST request that expects a request body from which an 'a' can be extracted
  AHPost :: !(a -> Handler m (HttpLogGraphQLInfo, APIResp)) -> APIHandler m a
  -- | A general GraphQL request (query or mutation) for which the content of the query
  -- is made available to the handler for authentication.
  -- This is a more specific version of the 'AHPost' constructor.
  AHGraphQLRequest :: !(GH.ReqsText -> Handler m (HttpLogGraphQLInfo, APIResp)) -> APIHandler m GH.ReqsText
  AHPersistedGraphQLRequest :: !(ExtQueryReqs -> Handler m (HttpLogGraphQLInfo, APIResp)) -> APIHandler m ExtQueryReqs

boolToText :: Bool -> Text
boolToText = bool "false" "true"

isAdminSecretSet :: AuthMode -> Text
isAdminSecretSet AMNoAuth = boolToText False
isAdminSecretSet _ = boolToText True

mkGetHandler :: Handler m (HttpLogGraphQLInfo, APIResp) -> APIHandler m ()
mkGetHandler = AHGet

mkPostHandler :: (a -> Handler m (HttpLogGraphQLInfo, APIResp)) -> APIHandler m a
mkPostHandler = AHPost

mkGQLRequestHandler :: (GH.ReqsText -> Handler m (HttpLogGraphQLInfo, APIResp)) -> APIHandler m GH.ReqsText
mkGQLRequestHandler = AHGraphQLRequest

mkPersistedGQLRequestHandler :: (ExtQueryReqs -> Handler m (HttpLogGraphQLInfo, APIResp)) -> APIHandler m ExtQueryReqs
mkPersistedGQLRequestHandler = AHPersistedGraphQLRequest

mkAPIRespHandler :: (Functor m) => (a -> Handler m (HttpResponse EncJSON)) -> (a -> Handler m APIResp)
mkAPIRespHandler = (fmap . fmap) JSONResp

mkGQLAPIRespHandler ::
  (Functor m) =>
  (a -> Handler m (b, (HttpResponse EncJSON))) ->
  (a -> Handler m (b, APIResp))
mkGQLAPIRespHandler = (fmap . fmap . fmap) JSONResp

isMetadataEnabled :: AppContext -> Bool
isMetadataEnabled ac = S.member METADATA $ acEnabledAPIs ac

isGraphQLEnabled :: AppContext -> Bool
isGraphQLEnabled ac = S.member GRAPHQL $ acEnabledAPIs ac

isPGDumpEnabled :: AppContext -> Bool
isPGDumpEnabled ac = S.member PGDUMP $ acEnabledAPIs ac

isConfigEnabled :: AppContext -> Bool
isConfigEnabled ac = S.member CONFIG $ acEnabledAPIs ac

isDeveloperAPIEnabled :: AppContext -> Bool
isDeveloperAPIEnabled ac = S.member DEVELOPER $ acEnabledAPIs ac

-- {-# SCC parseBody #-}
parseBody :: (FromJSON a, MonadError QErr m) => BL.ByteString -> m (Value, a)
parseBody reqBody =
  case eitherDecode' reqBody of
    Left e -> throw400 InvalidJSON (T.pack e)
    Right jVal -> (jVal,) <$> decodeValue jVal

onlyAdmin :: (MonadError QErr m, MonadReader HandlerCtx m) => m ()
onlyAdmin = do
  uRole <- asks (_uiRole . hcUser)
  unless (uRole == adminRoleName)
    $ throw400 AccessDenied "You have to be an admin to access this endpoint"

setHeader :: (MonadIO m) => HTTP.Header -> Spock.ActionCtxT ctx m ()
setHeader (headerName, headerValue) =
  Spock.setHeader (bsToTxt $ CI.original headerName) (bsToTxt headerValue)

-- | Typeclass representing the metadata API authorization effect
class (Monad m) => MonadMetadataApiAuthorization m where
  authorizeV1QueryApi ::
    RQLQuery -> HandlerCtx -> m (Either QErr ())

  authorizeV1MetadataApi ::
    RQLMetadata -> HandlerCtx -> m (Either QErr ())

  authorizeV2QueryApi ::
    V2Q.RQLQuery -> HandlerCtx -> m (Either QErr ())

instance (MonadMetadataApiAuthorization m) => MonadMetadataApiAuthorization (ReaderT r m) where
  authorizeV1QueryApi q hc = lift $ authorizeV1QueryApi q hc
  authorizeV1MetadataApi q hc = lift $ authorizeV1MetadataApi q hc
  authorizeV2QueryApi q hc = lift $ authorizeV2QueryApi q hc

instance (MonadMetadataApiAuthorization m) => MonadMetadataApiAuthorization (ExceptT e m) where
  authorizeV1QueryApi q hc = lift $ authorizeV1QueryApi q hc
  authorizeV1MetadataApi q hc = lift $ authorizeV1MetadataApi q hc
  authorizeV2QueryApi q hc = lift $ authorizeV2QueryApi q hc

instance (MonadMetadataApiAuthorization m) => MonadMetadataApiAuthorization (Tracing.TraceT m) where
  authorizeV1QueryApi q hc = lift $ authorizeV1QueryApi q hc
  authorizeV1MetadataApi q hc = lift $ authorizeV1MetadataApi q hc
  authorizeV2QueryApi q hc = lift $ authorizeV2QueryApi q hc

-- | The config API (/v1alpha1/config) handler
class (Monad m) => MonadConfigApiHandler m where
  runConfigApiHandler ::
    AppStateRef impl ->
    Spock.SpockCtxT () m ()

-- The graphql API (/graphql/v1) handler. It handles both persisted queries as well as graphql queries.
class (Monad m) => MonadGQLApiHandler m where
  -- the GET handler handles only persisted queries (containing the hash of the query to be fetched from the cache store)
  runPersistedQueriesGetHandler ::
    PersistedQueriesState -> Int -> [(Text, Text)] -> Handler m (HttpLogGraphQLInfo, APIResp)

  -- the POST handler handler handles both persisted queries as well as graphql requests (without a hash associated with
  -- them).
  runPersistedQueriesPostHandler ::
    PersistedQueriesState -> Int -> ExtQueryReqs -> Handler m (HttpLogGraphQLInfo, HttpResponse EncJSON)

mkSpockAction ::
  forall m a impl.
  ( MonadIO m,
    MonadBaseControl IO m,
    HasAppEnv m,
    FromJSON a,
    UserAuthentication m,
    HttpLog m,
    HasResourceLimits m,
    MonadTrace m
  ) =>
  AppStateRef impl ->
  -- | `QErr` JSON encoder function
  (Bool -> QErr -> Encoding) ->
  -- | `QErr` modifier
  (QErr -> QErr) ->
  APIHandler m a ->
  Spock.ActionT m ()
mkSpockAction appStateRef qErrEncoder qErrModifier apiHandler = do
  AppEnv {..} <- lift askAppEnv
  AppContext {..} <- liftIO $ getAppContext appStateRef
  SchemaCache {..} <- liftIO $ getSchemaCache appStateRef
  req <- Spock.request
  let origHeaders = Wai.requestHeaders req
      ipAddress = Wai.getSourceFromFallback req
      pathInfo = Wai.rawPathInfo req
      propagators = getOtelTracesPropagator scOpenTelemetryConfig

  -- Bytes are actually read from the socket here. Time this.
  (ioWaitTime, reqBody) <- withElapsedTime $ liftIO $ Wai.strictRequestBody req

  (requestId, headers) <- getRequestId origHeaders

  tracingCtx <- liftIO $ Tracing.extract propagators headers

  let runTrace ::
        forall m1 a1.
        (MonadTrace m1) =>
        m1 a1 ->
        m1 a1
      runTrace =
        Tracing.newTraceWith tracingCtx appEnvTraceSamplingPolicy (fromString (B8.unpack pathInfo))

  let getInfo parsedRequest = do
        authenticationResp <- lift (resolveUserInfo (_lsLogger appEnvLoggers) appEnvManager headers acAuthMode parsedRequest)
        authInfo <- onLeft authenticationResp (logErrorAndResp Nothing requestId req (reqBody, Nothing) False origHeaders (ExtraUserInfo Nothing) . qErrModifier)
        let (userInfo, _, authHeaders, extraUserInfo) = authInfo
        appContext <- liftIO $ getAppContext appStateRef
        schemaCache <- liftIO $ getRebuildableSchemaCacheWithVersion appStateRef
        pure
          ( userInfo,
            authHeaders,
            HandlerCtx appContext schemaCache userInfo headers requestId ipAddress appEnvLicenseKeyCache,
            shouldIncludeInternal (_uiRole userInfo) acResponseInternalErrorsConfig,
            extraUserInfo
          )

  hoist runTrace do
    -- Add the request ID to the tracing metadata so that we
    -- can correlate requests and traces
    lift $ Tracing.attachMetadata [("request_id", unRequestId requestId)]

    (serviceTime, (result, userInfo, authHeaders, includeInternal, queryJSON, extraUserInfo)) <- withElapsedTime $ case apiHandler of
      -- in the case of a simple get/post we don't have to send the webhook anything
      AHGet handler -> do
        (userInfo, authHeaders, handlerState, includeInternal, extraUserInfo) <- getInfo Nothing
        res <- lift $ runHandler (_lsLogger appEnvLoggers) handlerState handler
        pure (res, userInfo, authHeaders, includeInternal, Nothing, extraUserInfo)
      AHPost handler -> do
        (userInfo, authHeaders, handlerState, includeInternal, extraUserInfo) <- getInfo Nothing
        (queryJSON, parsedReq) <-
          runExcept (parseBody reqBody) `onLeft` \e -> do
            logErrorAndResp (Just userInfo) requestId req (reqBody, Nothing) includeInternal origHeaders extraUserInfo (qErrModifier e)
        res <- lift $ runHandler (_lsLogger appEnvLoggers) handlerState $ handler parsedReq
        pure (res, userInfo, authHeaders, includeInternal, Just queryJSON, extraUserInfo)
      -- in this case we parse the request _first_ and then send the request to the webhook for auth
      AHGraphQLRequest handler -> do
        (queryJSON, parsedReq) <-
          runExcept (parseBody reqBody) `onLeft` \e -> do
            -- if the request fails to parse, call the webhook without a request body
            -- TODO should we signal this to the webhook somehow?
            (userInfo, _, _, _, extraUserInfo) <- getInfo Nothing
            logErrorAndResp (Just userInfo) requestId req (reqBody, Nothing) False origHeaders extraUserInfo (qErrModifier e)
        (userInfo, authHeaders, handlerState, includeInternal, extraUserInfo) <- getInfo (Just parsedReq)

        res <- lift $ runHandler (_lsLogger appEnvLoggers) handlerState $ handler parsedReq
        pure (res, userInfo, authHeaders, includeInternal, Just queryJSON, extraUserInfo)
      AHPersistedGraphQLRequest handler -> do
        (queryJSON, parsedReq) <-
          runExcept (parseBody reqBody) `onLeft` \e -> do
            -- if the request fails to parse, call the webhook without a request body
            -- TODO should we signal this to the webhook somehow?
            (userInfo, _, _, _, extraUserInfo) <- getInfo Nothing
            logErrorAndResp (Just userInfo) requestId req (reqBody, Nothing) False origHeaders extraUserInfo (qErrModifier e)
        let newReq = case parsedReq of
              EqrGQLReq reqText -> Just reqText
              -- Note: We send only `ReqsText` to the webhook in case of `ExtPersistedQueryRequest` (persisted queries),
              -- which does not contain the `extensions` field.
              EqrAPQReq persistedQueryReq -> do
                q <- _extQuery persistedQueryReq
                Just $ GH.GQLSingleRequest $ GH.GQLReq (_extOperationName persistedQueryReq) q (_extVariables persistedQueryReq)
        (userInfo, authHeaders, handlerState, includeInternal, extraUserInfo) <- getInfo newReq

        res <- lift $ runHandler (_lsLogger appEnvLoggers) handlerState $ handler parsedReq
        pure (res, userInfo, authHeaders, includeInternal, Just queryJSON, extraUserInfo)

    -- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/span-general/#general-identity-attributes
    lift $ Tracing.attachMetadata [("enduser.role", roleNameToTxt $ _uiRole userInfo)]

    -- apply the error modifier
    let modResult = fmapL qErrModifier result

    -- log and return result
    case modResult of
      Left err ->
        logErrorAndResp (Just userInfo) requestId req (reqBody, queryJSON) includeInternal headers extraUserInfo err
      Right (httpLogGraphQLInfo, res) -> do
        let httpLogMetadata = buildHttpLogMetadata @m httpLogGraphQLInfo extraUserInfo
        logSuccessAndResp (Just userInfo) requestId req (reqBody, queryJSON) res (Just (ioWaitTime, serviceTime)) origHeaders authHeaders httpLogMetadata
  where
    logErrorAndResp ::
      forall any ctx.
      Maybe UserInfo ->
      RequestId ->
      Wai.Request ->
      (BL.ByteString, Maybe Value) ->
      Bool ->
      [HTTP.Header] ->
      ExtraUserInfo ->
      QErr ->
      Spock.ActionCtxT ctx m any
    logErrorAndResp userInfo reqId waiReq req includeInternal headers extraUserInfo qErr = do
      AppEnv {..} <- lift askAppEnv
      let httpLogMetadata = buildHttpLogMetadata @m emptyHttpLogGraphQLInfo extraUserInfo
          jsonResponse = J.encodingToLazyByteString $ qErrEncoder includeInternal qErr
          contentLength = ("Content-Length", B8.toStrict $ BB.toLazyByteString $ BB.int64Dec $ BL.length jsonResponse)
          allHeaders = [contentLength, jsonHeader]
      -- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/http/#common-attributes
      lift $ Tracing.attachMetadata [("http.response_content_length", bsToTxt $ snd contentLength)]
      lift $ logHttpError (_lsLogger appEnvLoggers) appEnvLoggingSettings userInfo reqId waiReq req qErr headers httpLogMetadata True
      mapM_ setHeader allHeaders
      Spock.setStatus $ qeStatus qErr
      Spock.lazyBytes jsonResponse

    logSuccessAndResp userInfo reqId waiReq req result qTime reqHeaders authHdrs httpLoggingMetadata = do
      AppEnv {..} <- lift askAppEnv
      let (respBytes, respHeaders) = case result of
            JSONResp (HttpResponse encJson h) -> (encJToLBS encJson, pure jsonHeader <> h)
            RawResp (HttpResponse rawBytes h) -> (rawBytes, h)
          (compressedResp, encodingType) = compressResponse (Wai.requestHeaders waiReq) respBytes
          encodingHeader = maybeToList (contentEncodingHeader <$> encodingType)
          reqIdHeader = (requestIdHeader, txtToBs $ unRequestId reqId)
          contentLength = ("Content-Length", B8.toStrict $ BB.toLazyByteString $ BB.int64Dec $ BL.length compressedResp)
          allRespHeaders = [reqIdHeader, contentLength] <> encodingHeader <> respHeaders <> authHdrs
      -- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/http/#common-attributes
      lift $ Tracing.attachMetadata [("http.response_content_length", bsToTxt $ snd contentLength)]
      lift $ logHttpSuccess (_lsLogger appEnvLoggers) appEnvLoggingSettings userInfo reqId waiReq req respBytes compressedResp qTime encodingType reqHeaders httpLoggingMetadata True
      mapM_ setHeader allRespHeaders
      Spock.lazyBytes compressedResp

v1QueryHandler ::
  ( MonadIO m,
    MonadError QErr m,
    MonadBaseControl IO m,
    MonadMetadataApiAuthorization m,
    MonadTrace m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesNetwork m,
    MonadGetPolicies m,
    UserInfoM m
  ) =>
  ((RebuildableSchemaCache -> m (EncJSON, RebuildableSchemaCache)) -> m EncJSON) ->
  RQLQuery ->
  m (HttpResponse EncJSON)
v1QueryHandler schemaCacheRefUpdater query = do
  (liftEitherM . authorizeV1QueryApi query) =<< ask
  schemaCache <- asks hcSchemaCache
  res <- bool (fst <$> action schemaCache) (schemaCacheRefUpdater action) $ queryModifiesSchemaCache query
  return $ HttpResponse res []
  where
    action schemaCache = do
      appContext <- asks hcAppContext
      runQuery
        appContext
        schemaCache
        query

-- | See Note [Explicitly passing AppStateRef]
v1MetadataHandler ::
  ( MonadIO m,
    MonadError QErr m,
    MonadBaseControl IO m,
    MonadReader HandlerCtx m,
    MonadTrace m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadMetadataApiAuthorization m,
    MonadEventLogCleanup m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    ProvidesNetwork m,
    MonadGetPolicies m,
    UserInfoM m
  ) =>
  ((RebuildableSchemaCache -> m (EncJSON, RebuildableSchemaCache)) -> m EncJSON) ->
  WS.WebsocketCloseOnMetadataChangeAction ->
  RQLMetadata ->
  m (HttpResponse EncJSON)
v1MetadataHandler schemaCacheRefUpdater closeWebsocketsOnMetadataChangeAction query = Tracing.newSpan "Metadata" $ do
  (liftEitherM . authorizeV1MetadataApi query) =<< ask
  appContext <- asks hcAppContext
  r <-
    schemaCacheRefUpdater $ \schemaCache ->
      runMetadataQuery
        appContext
        schemaCache
        closeWebsocketsOnMetadataChangeAction
        query
  pure $ HttpResponse r []

v2QueryHandler ::
  ( MonadIO m,
    MonadError QErr m,
    MonadBaseControl IO m,
    MonadMetadataApiAuthorization m,
    MonadTrace m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    MonadQueryTags m,
    ProvidesNetwork m,
    UserInfoM m
  ) =>
  ((RebuildableSchemaCache -> m (EncJSON, RebuildableSchemaCache)) -> m EncJSON) ->
  V2Q.RQLQuery ->
  m (HttpResponse EncJSON)
v2QueryHandler schemaCacheRefUpdater query = Tracing.newSpan "v2 Query" $ do
  schemaCache <- asks hcSchemaCache
  (liftEitherM . authorizeV2QueryApi query) =<< ask
  res <-
    bool (fst <$> dbAction schemaCache) (schemaCacheRefUpdater dbAction)
      $ V2Q.queryModifiesSchema query
  return $ HttpResponse res []
  where
    -- Hit postgres
    dbAction schemaCache = do
      appContext <- asks hcAppContext
      V2Q.runQuery
        appContext
        schemaCache
        query

v1Alpha1GQHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadTrace m,
    HasAppEnv m,
    GH.MonadExecuteQuery m,
    MonadError QErr m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  E.GraphQLQueryType ->
  GH.GQLBatchedReqs (GH.GQLReq GH.GQLQueryText) ->
  m (HttpLogGraphQLInfo, HttpResponse EncJSON)
v1Alpha1GQHandler queryType query = do
  AppEnv {..} <- askAppEnv
  AppContext {..} <- asks hcAppContext
  userInfo <- asks hcUser
  schemaCache <- lastBuiltSchemaCache <$> asks hcSchemaCache
  reqHeaders <- asks hcReqHeaders
  ipAddress <- asks hcSourceIpAddress
  requestId <- asks hcRequestId
  GH.runGQBatched acEnvironment acSQLGenCtx schemaCache acEnableAllowlist appEnvEnableReadOnlyMode appEnvPrometheusMetrics (_lsLogger appEnvLoggers) appEnvLicenseKeyCache requestId acResponseInternalErrorsConfig userInfo ipAddress reqHeaders queryType query

v1GQHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadTrace m,
    HasAppEnv m,
    GH.MonadExecuteQuery m,
    MonadError QErr m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  GH.GQLBatchedReqs (GH.GQLReq GH.GQLQueryText) ->
  m (HttpLogGraphQLInfo, HttpResponse EncJSON)
v1GQHandler = v1Alpha1GQHandler E.QueryHasura

v1GQRelayHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadTrace m,
    HasAppEnv m,
    GH.MonadExecuteQuery m,
    MonadError QErr m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  GH.GQLBatchedReqs (GH.GQLReq GH.GQLQueryText) ->
  m (HttpLogGraphQLInfo, HttpResponse EncJSON)
v1GQRelayHandler = v1Alpha1GQHandler E.QueryRelay

gqlExplainHandler ::
  forall m.
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    MonadTrace m
  ) =>
  GE.GQLExplain ->
  m (HttpResponse EncJSON)
gqlExplainHandler query = do
  onlyAdmin
  schemaCache <- asks hcSchemaCache
  reqHeaders <- asks hcReqHeaders
  licenseKeyCache <- asks hcLicenseKeyCache
  res <- GE.explainGQLQuery (lastBuiltSchemaCache schemaCache) licenseKeyCache reqHeaders query
  return $ HttpResponse res []

v1Alpha1PGDumpHandler :: (MonadIO m, MonadError QErr m, MonadReader HandlerCtx m) => PGD.PGDumpReqBody -> m APIResp
v1Alpha1PGDumpHandler b = do
  onlyAdmin
  schemaCache <- asks hcSchemaCache
  let sources = scSources (lastBuiltSchemaCache schemaCache)
      sourceName = PGD.prbSource b
      sourceConfig = unsafeSourceConfiguration @('Postgres 'Vanilla) =<< HashMap.lookup sourceName sources
  ci <-
    fmap _pscConnInfo sourceConfig
      `onNothing` throw400 NotFound ("source " <> sourceName <<> " not found")
  output <- PGD.execPGDump b ci
  return $ RawResp $ HttpResponse output [sqlHeader]

consoleAssetsHandler ::
  (MonadIO m, HttpLog m) =>
  L.Logger L.Hasura ->
  LoggingSettings ->
  Text ->
  Text ->
  Spock.ActionT m ()
consoleAssetsHandler logger loggingSettings dir path = do
  req <- Spock.request
  let reqHeaders = Wai.requestHeaders req
  -- '..' in paths need not be handed as it is resolved in the url by
  -- spock's routing. we get the expanded path.
  eFileContents <-
    liftIO
      $ try @IOException do
        unless validFilename $ throwIO $ userError "invalid asset filename"
        BL.readFile
          $ joinPath [T.unpack dir, pathStr]
  either (onError reqHeaders) onSuccess eFileContents
  where
    pathStr = T.unpack path
    validFilename = isRelative pathStr && not (".." `T.isInfixOf` path)
    onSuccess c = do
      mapM_ setHeader headers
      Spock.lazyBytes c
    onError :: (MonadIO m, HttpLog m) => [HTTP.Header] -> a -> Spock.ActionT m ()
    onError hdrs _ = raiseGenericApiError logger loggingSettings hdrs $ err404 NotFound $ "Couldn't find console asset " <> path
    -- set gzip header if the filename ends with .gz
    (fileName, encHeader) = case splitExtension (takeFileName pathStr) of
      (v, ".gz") -> (T.pack v, [gzipHeader])
      _ -> (path, [])
    mimeType = defaultMimeLookup fileName
    headers = ("Content-Type", mimeType) : encHeader

class (Monad m) => ConsoleRenderer m where
  type ConsoleType m :: Type
  renderConsole ::
    Text ->
    AuthMode ->
    TelemetryStatus ->
    Maybe Text ->
    Maybe Text ->
    ConsoleType m ->
    m (Either String Text)

-- TODO(awjchen): This is a kludge that will be removed when the entitlement service is fully implemented.
data CEConsoleType
  = OSSConsole
  | ProLiteConsole

ceConsoleTypeIdentifier :: CEConsoleType -> String
ceConsoleTypeIdentifier = \case
  OSSConsole -> "oss"
  ProLiteConsole -> "pro-lite"

instance (ConsoleRenderer m) => ConsoleRenderer (Tracing.TraceT m) where
  type ConsoleType (Tracing.TraceT m) = ConsoleType m
  renderConsole a b c d e f = lift $ renderConsole a b c d e f

-- Type class to get any extra [Pair] for the version API
class (Monad m) => MonadVersionAPIWithExtraData m where
  getExtraDataForVersionAPI :: m [J.Pair]

renderHtmlTemplate :: M.Template -> Value -> Either String Text
renderHtmlTemplate template jVal =
  bool (Left errMsg) (Right res) $ null errs
  where
    errMsg = "template rendering failed: " ++ show errs
    (errs, res) = M.checkedSubstitute template jVal

-- | Default implementation of the 'MonadConfigApiHandler'
configApiGetHandler ::
  forall m impl.
  ( MonadIO m,
    MonadBaseControl IO m,
    HasAppEnv m,
    UserAuthentication m,
    HttpLog m,
    HasResourceLimits m,
    MonadTrace m
  ) =>
  AppStateRef impl ->
  Spock.SpockCtxT () m ()
configApiGetHandler appStateRef = do
  Spock.get "v1alpha1/config"
    $ onlyWhenApiEnabled isConfigEnabled appStateRef
    $ do
      AppEnv {..} <- lift askAppEnv
      AppContext {..} <- liftIO $ getAppContext appStateRef
      featureFlagSettings <-
        traverse
          (\(ff, desc) -> (ff,desc,) <$> liftIO (runCheckFeatureFlag appEnvCheckFeatureFlag ff))
          (listKnownFeatureFlags appEnvCheckFeatureFlag)
      mkSpockAction appStateRef encodeQErr id
        $ mkGetHandler
        $ do
          onlyAdmin
          let res =
                runGetConfig
                  acFunctionPermsCtx
                  acRemoteSchemaPermsCtx
                  acAuthMode
                  acEnableAllowlist
                  acLiveQueryOptions
                  acStreamQueryOptions
                  appEnvConsoleAssetsDir
                  acExperimentalFeatures
                  acEnabledAPIs
                  acDefaultNamingConvention
                  featureFlagSettings
                  acApolloFederationStatus
          return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue res) [])

data HasuraApp = HasuraApp
  { _hapApplication :: !Wai.Application,
    _hapAsyncActionSubscriptionState :: !ES.AsyncActionSubscriptionState,
    _hapShutdownWsServer :: !(IO ())
  }

mkWaiApp ::
  forall m impl.
  ( MonadIO m,
    MonadFail m, -- only due to https://gitlab.haskell.org/ghc/ghc/-/issues/15681
    MonadFix m,
    MonadStateless IO m,
    LA.Forall (LA.Pure m),
    ConsoleRenderer m,
    MonadVersionAPIWithExtraData m,
    HttpLog m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    UserAuthentication m,
    MonadMetadataApiAuthorization m,
    E.MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    MonadExecutionLog m,
    WS.MonadWSLog m,
    MonadTrace m,
    GH.MonadExecuteQuery m,
    HasResourceLimits m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesNetwork m,
    MonadGetPolicies m,
    MonadGQLApiHandler m
  ) =>
  (AppStateRef impl -> Spock.SpockT m ()) ->
  AppStateRef impl ->
  ConsoleType m ->
  EKG.Store EKG.EmptyMetrics ->
  WS.WSServerEnv impl ->
  m HasuraApp
mkWaiApp setupHook appStateRef consoleType ekgStore wsServerEnv = do
  appEnv@AppEnv {..} <- askAppEnv
  spockApp <- liftWithStateless $ \lowerIO ->
    Spock.spockAsApp
      $ Spock.spockT lowerIO
      $ httpApp setupHook appStateRef appEnv consoleType ekgStore
      $ WS.mkCloseWebsocketsOnMetadataChangeAction (WS._wseServer wsServerEnv)

  let wsServerApp = WS.createWSServerApp (_lsEnabledLogTypes appEnvLoggingSettings) wsServerEnv appEnvWebSocketConnectionInitTimeout appEnvLicenseKeyCache
      stopWSServer = WS.stopWSServerApp wsServerEnv

  waiApp <- liftWithStateless $ \lowerIO ->
    pure $ WSC.websocketsOr appEnvConnectionOptions (\ip conn -> lowerIO $ wsServerApp ip conn) spockApp

  pure $ HasuraApp waiApp (ES._ssAsyncActions appEnvSubscriptionState) stopWSServer

httpApp ::
  forall m impl.
  ( MonadIO m,
    MonadFix m,
    MonadBaseControl IO m,
    ConsoleRenderer m,
    MonadVersionAPIWithExtraData m,
    HttpLog m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    UserAuthentication m,
    MonadMetadataApiAuthorization m,
    E.MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadTrace m,
    GH.MonadExecuteQuery m,
    MonadMetadataStorage m,
    HasResourceLimits m,
    MonadResolveSource m,
    MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesNetwork m,
    MonadGetPolicies m,
    MonadGQLApiHandler m
  ) =>
  (AppStateRef impl -> Spock.SpockT m ()) ->
  AppStateRef impl ->
  AppEnv ->
  ConsoleType m ->
  EKG.Store EKG.EmptyMetrics ->
  WS.WebsocketCloseOnMetadataChangeAction ->
  Spock.SpockT m ()
httpApp setupHook appStateRef AppEnv {..} consoleType ekgStore closeWebsocketsOnMetadataChangeAction = do
  -- Additional spock action to run
  setupHook appStateRef

  -- cors middleware
  Spock.middleware
    $ corsMiddleware (acCorsPolicy <$> getAppContext appStateRef)

  -- API Console and Root Dir
  serveApiConsole

  -- Local console assets for server and CLI consoles
  serveApiConsoleAssets

  -- Health check endpoint with logs
  let healthzAction = do
        let errorMsg = "ERROR"
        isStrict <- fromMaybe False <$> Spock.param "strict"
        lift checkMetadataStorageHealth >>= \case
          Left err -> do
            -- error running the health check
            logError err
            Spock.setStatus HTTP.status500 >> Spock.text errorMsg
          Right _ -> do
            -- metadata storage is healthy
            sc <- liftIO $ getSchemaCache appStateRef
            let isInconsistent = not $ null $ scInconsistentObjs sc
                inconsistenciesMessage = "inconsistent objects in schema"
            (status, responseText) <-
              if
                | (isInconsistent && isStrict) -> do
                    -- Inconsistencies exist and strict mode enabled. Report inconsistencies as ERROR with status 500.
                    let message = "ERROR: " <> inconsistenciesMessage
                    logError $ err500 InvalidConfiguration message
                    pure (HTTP.status500, message)
                | (isInconsistent && not isStrict) -> do
                    -- Inconsistencies exist and strict mode disabled. Warn inconsistencies with status 200.
                    let message = "WARN: " <> inconsistenciesMessage
                    logSuccess $ LT.fromStrict message
                    pure (HTTP.status200, message)
                | otherwise -> do
                    -- No inconsistencies in schema cache, report OK
                    let message = "OK"
                    logSuccess $ LT.fromStrict message
                    pure (HTTP.status200, message)

            Spock.setStatus status >> Spock.text responseText

  Spock.get "healthz" healthzAction

  -- This is an alternative to `healthz` (See issue #6958)
  Spock.get "hasura/healthz" healthzAction

  Spock.get "v1/version" $ do
    logSuccess $ "version: " <> convertText currentVersion
    extraData <- lift $ getExtraDataForVersionAPI
    setHeader jsonHeader
    Spock.lazyBytes $ encode $ object $ ["version" .= currentVersion] <> extraData

  let customEndpointHandler ::
        RestRequest Spock.SpockMethod ->
        Handler m (HttpLogGraphQLInfo, APIResp)
      customEndpointHandler restReq = do
        AppContext {..} <- liftIO $ getAppContext appStateRef
        endpoints <- liftIO $ scEndpoints <$> getSchemaCache appStateRef
        schemaCache <- lastBuiltSchemaCache <$> asks hcSchemaCache
        requestId <- asks hcRequestId
        userInfo <- asks hcUser
        reqHeaders <- asks hcReqHeaders
        ipAddress <- asks hcSourceIpAddress

        req <-
          restReq & traverse \case
            Spock.MethodStandard (Spock.HttpMethod m) -> case m of
              Spock.GET -> pure EP.GET
              Spock.POST -> pure EP.POST
              Spock.PUT -> pure EP.PUT
              Spock.DELETE -> pure EP.DELETE
              Spock.PATCH -> pure EP.PATCH
              other -> throw400 BadRequest $ "Method " <> tshow other <> " not supported."
            _ -> throw400 BadRequest $ "Nonstandard method not allowed for REST endpoints"
        fmap JSONResp <$> runCustomEndpoint acEnvironment acSQLGenCtx schemaCache acEnableAllowlist appEnvEnableReadOnlyMode appEnvPrometheusMetrics (_lsLogger appEnvLoggers) appEnvLicenseKeyCache requestId userInfo reqHeaders ipAddress req endpoints

  -- See Issue #291 for discussion around restified feature
  Spock.hookRouteAll ("api" <//> "rest" <//> Spock.wildcard) $ \wildcard -> do
    queryParams <- Spock.params
    body <- Spock.body
    method <- Spock.reqMethod

    -- This is where we decode the json encoded body args. They
    -- are treated as if they came from query arguments, but allow
    -- us to pass non-scalar values.
    let bodyParams = case J.decodeStrict body of
          Just (J.Object o) -> map (first K.toText) $ KM.toList o
          _ -> []
        allParams = fmap Left <$> queryParams <|> fmap Right <$> bodyParams

    spockAction encodeQErr id $ do
      -- TODO: Are we actually able to use mkGetHandler in this situation? POST handler seems to do some work that we might want to avoid.
      mkGetHandler $ customEndpointHandler (RestRequest wildcard method allParams)

  -- Note: we create a schema cache updater function, to restrict the access
  -- to 'AppStateRef' inside the request handlers
  let schemaCacheUpdater = withSchemaCacheReadUpdate appStateRef logger Nothing

  Spock.post "v1/graphql/explain" $ do
    onlyWhenApiEnabled isMetadataEnabled appStateRef gqlExplainAction

  Spock.post "v1alpha1/graphql/explain" $ do
    onlyWhenApiEnabled isMetadataEnabled appStateRef gqlExplainAction

  Spock.post "v1/query" $ do
    onlyWhenApiEnabled isMetadataEnabled appStateRef
      $ spockAction encodeQErr id
      $ do
        mkPostHandler $ fmap (emptyHttpLogGraphQLInfo,) <$> mkAPIRespHandler (v1QueryHandler schemaCacheUpdater)

  Spock.post "v1/metadata" $ do
    onlyWhenApiEnabled isMetadataEnabled appStateRef
      $ spockAction encodeQErr id
      $ mkPostHandler
      $ fmap (emptyHttpLogGraphQLInfo,)
      <$> mkAPIRespHandler (v1MetadataHandler schemaCacheUpdater closeWebsocketsOnMetadataChangeAction)

  Spock.post "v2/query" $ do
    onlyWhenApiEnabled isMetadataEnabled appStateRef
      $ spockAction encodeQErr id
      $ mkPostHandler
      $ fmap (emptyHttpLogGraphQLInfo,)
      <$> mkAPIRespHandler (v2QueryHandler schemaCacheUpdater)

  Spock.post "v1alpha1/pg_dump" $ do
    onlyWhenApiEnabled isPGDumpEnabled appStateRef
      $ spockAction encodeQErr id
      $ mkPostHandler
      $ fmap (emptyHttpLogGraphQLInfo,)
      <$> v1Alpha1PGDumpHandler

  runConfigApiHandler appStateRef

  Spock.post "v1alpha1/graphql" $ do
    onlyWhenApiEnabled isGraphQLEnabled appStateRef
      $ spockAction GH.encodeGQErr id
      $ mkGQLRequestHandler
      $ mkGQLAPIRespHandler
      $ v1Alpha1GQHandler E.QueryHasura

  Spock.post "v1/graphql" $ do
    let persistedQueriesState = appEnvPersistedQueries
        persistedQueriesTtl = appEnvPersistedQueriesTtl
    let apiHandler = runPersistedQueriesPostHandler persistedQueriesState persistedQueriesTtl
    onlyWhenApiEnabled isGraphQLEnabled appStateRef
      $ spockAction GH.encodeGQErr allMod200
      $ mkPersistedGQLRequestHandler
      $ mkGQLAPIRespHandler
      $ apiHandler

  Spock.get "v1/graphql" $ do
    let persistedQueriesState = appEnvPersistedQueries
        persistedQueriesTtl = appEnvPersistedQueriesTtl
    params <- Spock.paramsGet
    let apiHandler = runPersistedQueriesGetHandler persistedQueriesState persistedQueriesTtl params
    onlyWhenApiEnabled isGraphQLEnabled appStateRef
      $ spockAction GH.encodeGQErr allMod200
      $ mkGetHandler
      $ apiHandler

  Spock.post "v1beta1/relay" $ do
    onlyWhenApiEnabled isGraphQLEnabled appStateRef
      $ spockAction GH.encodeGQErr allMod200
      $ mkGQLRequestHandler
      $ mkGQLAPIRespHandler
      $ v1GQRelayHandler

  -- This exposes some simple RTS stats when we run with `+RTS -T`. We want
  -- this to be available even when developer APIs are not compiled in, to
  -- support benchmarking.
  -- See: https://hackage.haskell.org/package/base/docs/GHC-Stats.html
  exposeRtsStats <- liftIO RTS.getRTSStatsEnabled
  when exposeRtsStats $ do
    Spock.get "dev/rts_stats" $ do
      -- This ensures the live_bytes and other counters from GCDetails are fresh:
      liftIO performMajorGC
      stats <- liftIO RTS.getRTSStats
      Spock.json stats

  Spock.get "dev/ekg" $ do
    onlyWhenApiEnabled isDeveloperAPIEnabled appStateRef
      $ spockAction encodeQErr id
      $ mkGetHandler
      $ do
        onlyAdmin
        respJ <- liftIO $ EKG.sampleAll ekgStore
        return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue $ EKG.sampleToJson respJ) [])

  -- This deprecated endpoint used to show the query plan cache pre-PDV.
  -- Eventually this endpoint can be removed.
  Spock.get "dev/plan_cache" $ do
    onlyWhenApiEnabled isDeveloperAPIEnabled appStateRef
      $ spockAction encodeQErr id
      $ mkGetHandler
      $ do
        onlyAdmin
        return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue J.Null) [])

  Spock.get "dev/subscriptions" $ do
    onlyWhenApiEnabled isDeveloperAPIEnabled appStateRef
      $ spockAction encodeQErr id
      $ mkGetHandler
      $ do
        onlyAdmin
        appCtx <- liftIO $ getAppContext appStateRef
        respJ <- liftIO $ ES.dumpSubscriptionsState False (acLiveQueryOptions appCtx) (acStreamQueryOptions appCtx) appEnvSubscriptionState
        return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue respJ) [])

  Spock.get "dev/subscriptions/extended" $ do
    onlyWhenApiEnabled isDeveloperAPIEnabled appStateRef
      $ spockAction encodeQErr id
      $ mkGetHandler
      $ do
        onlyAdmin
        appCtx <- liftIO $ getAppContext appStateRef
        respJ <- liftIO $ ES.dumpSubscriptionsState True (acLiveQueryOptions appCtx) (acStreamQueryOptions appCtx) appEnvSubscriptionState
        return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue respJ) [])

  Spock.get "dev/dataconnector/schema" $ do
    onlyWhenApiEnabled isDeveloperAPIEnabled appStateRef
      $ spockAction encodeQErr id
      $ mkGetHandler
      $ do
        onlyAdmin
        return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue openApiSchema) [])

  Spock.get "api/swagger/json"
    $ spockAction encodeQErr id
    $ mkGetHandler
    $ do
      onlyAdmin
      sc <- liftIO $ getSchemaCache appStateRef
      json <- buildOpenAPI sc
      return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue json) [])

  forM_ [Spock.GET, Spock.POST] $ \m -> Spock.hookAny m $ \_ -> do
    req <- Spock.request
    let headers = Wai.requestHeaders req
        qErr = err404 NotFound "resource does not exist"
    raiseGenericApiError logger appEnvLoggingSettings headers qErr
  where
    logger = _lsLogger appEnvLoggers

    logSuccess msg = do
      req <- Spock.request
      reqBody <- liftIO $ Wai.strictRequestBody req
      let headers = Wai.requestHeaders req
          blMsg = TL.encodeUtf8 msg
      (reqId, _newHeaders) <- getRequestId headers
      -- setting the bool flag countDataTransferBytes to False here since we don't want to count the data
      -- transfer bytes for requests to `/heatlhz` and `/v1/version` endpoints
      lift $ logHttpSuccess logger appEnvLoggingSettings Nothing reqId req (reqBody, Nothing) blMsg blMsg Nothing Nothing headers (emptyHttpLogMetadata @m) False

    logError err = do
      req <- Spock.request
      reqBody <- liftIO $ Wai.strictRequestBody req
      let headers = Wai.requestHeaders req
      (reqId, _newHeaders) <- getRequestId headers
      -- setting the bool flag countDataTransferBytes to False here since we don't want to count the data
      -- transfer bytes for requests to `/heatlhz` and `/v1/version` endpoints
      lift $ logHttpError logger appEnvLoggingSettings Nothing reqId req (reqBody, Nothing) err headers (emptyHttpLogMetadata @m) False

    spockAction ::
      forall a.
      (FromJSON a) =>
      (Bool -> QErr -> Encoding) ->
      (QErr -> QErr) ->
      APIHandler m a ->
      Spock.ActionT m ()
    spockAction qErrEncoder qErrModifier apiHandler = mkSpockAction appStateRef qErrEncoder qErrModifier apiHandler

    -- all graphql errors should be of type 200
    allMod200 qe = qe {qeStatus = HTTP.status200}
    gqlExplainAction = do
      spockAction encodeQErr id
        $ mkPostHandler
        $ fmap (emptyHttpLogGraphQLInfo,)
        <$> mkAPIRespHandler gqlExplainHandler

    serveApiConsole = do
      -- redirect / to /console
      Spock.get Spock.root $ do
        onlyWhenApiEnabled (\appCtx -> isConsoleEnabled (acConsoleStatus appCtx) && isMetadataEnabled appCtx) appStateRef
          $ Spock.redirect "console"

      -- serve console html
      Spock.get ("console" <//> Spock.wildcard) $ \path -> do
        onlyWhenApiEnabled (\appCtx -> isConsoleEnabled (acConsoleStatus appCtx) && isMetadataEnabled appCtx) appStateRef $ do
          AppContext {..} <- liftIO $ getAppContext appStateRef
          req <- Spock.request
          let headers = Wai.requestHeaders req
          consoleHtml <- lift $ renderConsole path acAuthMode acEnableTelemetry appEnvConsoleAssetsDir appEnvConsoleSentryDsn consoleType
          either (raiseGenericApiError logger appEnvLoggingSettings headers . internalError . T.pack) Spock.html consoleHtml

    serveApiConsoleAssets = do
      -- serve static files if consoleAssetsDir is set
      for_ appEnvConsoleAssetsDir $ \dir ->
        Spock.get ("console/assets" <//> Spock.wildcard) $ \path -> do
          consoleAssetsHandler logger appEnvLoggingSettings dir path

-- an endpoint can be switched ON/OFF dynamically, hence serve the endpoint only
-- when it is enabled else throw HTTP Error 404
onlyWhenApiEnabled ::
  (MonadIO m) =>
  (AppContext -> Bool) ->
  AppStateRef impl ->
  Spock.ActionCtxT ctx m b ->
  Spock.ActionCtxT ctx m b
onlyWhenApiEnabled isEnabled appStateRef endpointAction = do
  appContext <- liftIO $ getAppContext appStateRef
  if (isEnabled appContext)
    then do endpointAction
    else do
      let qErr = err404 NotFound "resource does not exist"
      Spock.setStatus $ qeStatus qErr
      setHeader jsonHeader
      Spock.lazyBytes . J.encodingToLazyByteString $ encodeQErr False qErr

raiseGenericApiError ::
  forall m.
  (MonadIO m, HttpLog m) =>
  L.Logger L.Hasura ->
  LoggingSettings ->
  [HTTP.Header] ->
  QErr ->
  Spock.ActionT m ()
raiseGenericApiError logger loggingSetting headers qErr = do
  req <- Spock.request
  reqBody <- liftIO $ Wai.strictRequestBody req
  (reqId, _newHeaders) <- getRequestId $ Wai.requestHeaders req
  -- setting the bool flag countDataTransferBytes to False here since we don't want to count the data
  -- transfer bytes for requests to undefined resources
  lift $ logHttpError logger loggingSetting Nothing reqId req (reqBody, Nothing) qErr headers (emptyHttpLogMetadata @m) False
  setHeader jsonHeader
  Spock.setStatus $ qeStatus qErr
  Spock.lazyBytes $ encode qErr
