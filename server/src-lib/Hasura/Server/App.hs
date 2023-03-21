{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Server.App
  ( APIResp (JSONResp, RawResp),
    ConsoleRenderer (..),
    MonadVersionAPIWithExtraData (..),
    Handler,
    HandlerCtx (hcReqHeaders, hcAppContext, hcSchemaCache, hcUser),
    HasuraApp (HasuraApp),
    Loggers (..),
    MonadConfigApiHandler (..),
    MonadMetadataApiAuthorization (..),
    AppContext (..),
    boolToText,
    configApiGetHandler,
    isAdminSecretSet,
    mkGetHandler,
    mkSpockAction,
    mkWaiApp,
    onlyAdmin,
    renderHtmlTemplate,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Exception (IOException, try)
import Control.Monad.Morph (hoist)
import Control.Monad.Stateless
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson hiding (json)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as J
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Conversions (convertText)
import Data.Text.Extended
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Stats.Extended qualified as RTS
import Hasura.App.State
import Hasura.Backends.DataConnector.API (openApiSchema)
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.Subscription.State qualified as ES
import Hasura.GraphQL.Explain qualified as GE
import Hasura.GraphQL.Logging (MonadExecutionLog, MonadQueryLog)
import Hasura.GraphQL.Transport.HTTP qualified as GH
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.GraphQL.Transport.WSServerApp qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Types qualified as WS
import Hasura.HTTP
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude hiding (get, put)
import Hasura.RQL.DDL.ApiLimit (MonadGetApiTimeLimit)
import Hasura.RQL.DDL.EventTrigger (MonadEventLogCleanup)
import Hasura.RQL.DDL.Schema
import Hasura.RQL.Types.Endpoint as EP
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.SQL.Backend
import Hasura.Server.API.Config (runGetConfig)
import Hasura.Server.API.Metadata
import Hasura.Server.API.PGDump qualified as PGD
import Hasura.Server.API.Query
import Hasura.Server.API.V2Query qualified as V2Q
import Hasura.Server.AppStateRef
  ( AppStateRef,
    getAppContext,
    getSchemaCache,
    readSchemaCacheRef,
    withSchemaCacheUpdate,
  )
import Hasura.Server.Auth (AuthMode (..), UserAuthentication (..))
import Hasura.Server.Compression
import Hasura.Server.Init hiding (checkFeatureFlag)
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Middleware (corsMiddleware)
import Hasura.Server.OpenAPI (buildOpenAPI)
import Hasura.Server.Rest
import Hasura.Server.Types
import Hasura.Server.Utils
import Hasura.Server.Version
import Hasura.Services
import Hasura.Session
import Hasura.Tracing (MonadTrace)
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Types qualified as HTTP
import Network.Mime (defaultMimeLookup)
import Network.Wai.Extended qualified as Wai
import Network.Wai.Handler.WebSockets.Custom qualified as WSC
import System.FilePath (joinPath, takeFileName)
import System.Mem (performMajorGC)
import System.Metrics qualified as EKG
import System.Metrics.Json qualified as EKG
import Text.Mustache qualified as M
import Web.Spock.Core ((<//>))
import Web.Spock.Core qualified as Spock

data HandlerCtx = HandlerCtx
  { hcAppContext :: AppContext,
    hcSchemaCache :: RebuildableSchemaCache,
    hcSchemaCacheVersion :: SchemaCacheVer,
    hcUser :: UserInfo,
    hcReqHeaders :: [HTTP.Header],
    hcRequestId :: RequestId,
    hcSourceIpAddress :: Wai.IpAddress
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
      MonadTrace,
      HasAppEnv,
      HasResourceLimits,
      MonadResolveSource,
      E.MonadGQLExecutionCheck,
      MonadEventLogCleanup,
      MonadQueryLog,
      MonadExecutionLog,
      EB.MonadQueryTags,
      GH.MonadExecuteQuery,
      MonadMetadataApiAuthorization,
      MonadMetadataStorage,
      MonadMetadataStorageQueryAPI,
      ProvidesNetwork,
      MonadGetApiTimeLimit
    )

instance MonadTrans Handler where
  lift = Handler . lift . lift

instance Monad m => UserInfoM (Handler m) where
  askUserInfo = asks hcUser

instance (HasAppEnv m) => HasServerConfigCtx (Handler m) where
  askServerConfigCtx = Handler do
    AppEnv {..} <- askAppEnv
    AppContext {..} <- asks hcAppContext
    pure
      ServerConfigCtx
        { _sccFunctionPermsCtx = acFunctionPermsCtx,
          _sccRemoteSchemaPermsCtx = acRemoteSchemaPermsCtx,
          _sccSQLGenCtx = acSQLGenCtx,
          _sccMaintenanceMode = appEnvEnableMaintenanceMode,
          _sccExperimentalFeatures = acExperimentalFeatures,
          _sccEventingMode = appEnvEventingMode,
          _sccReadOnlyMode = appEnvEnableReadOnlyMode,
          _sccDefaultNamingConvention = acDefaultNamingConvention,
          _sccMetadataDefaults = acMetadataDefaults,
          _sccCheckFeatureFlag = appEnvCheckFeatureFlag,
          _sccApolloFederationStatus = acApolloFederationStatus
        }

runHandler :: (HasResourceLimits m, MonadBaseControl IO m) => HandlerCtx -> Handler m a -> m (Either QErr a)
runHandler ctx (Handler r) = do
  handlerLimit <- askHTTPHandlerLimit
  runExceptT $ flip runReaderT ctx $ runResourceLimits handlerLimit r

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
  unless (uRole == adminRoleName) $
    throw400 AccessDenied "You have to be an admin to access this endpoint"

setHeader :: MonadIO m => HTTP.Header -> Spock.ActionCtxT ctx m ()
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

instance MonadMetadataApiAuthorization m => MonadMetadataApiAuthorization (ReaderT r m) where
  authorizeV1QueryApi q hc = lift $ authorizeV1QueryApi q hc
  authorizeV1MetadataApi q hc = lift $ authorizeV1MetadataApi q hc
  authorizeV2QueryApi q hc = lift $ authorizeV2QueryApi q hc

instance MonadMetadataApiAuthorization m => MonadMetadataApiAuthorization (ExceptT e m) where
  authorizeV1QueryApi q hc = lift $ authorizeV1QueryApi q hc
  authorizeV1MetadataApi q hc = lift $ authorizeV1MetadataApi q hc
  authorizeV2QueryApi q hc = lift $ authorizeV2QueryApi q hc

instance MonadMetadataApiAuthorization m => MonadMetadataApiAuthorization (Tracing.TraceT m) where
  authorizeV1QueryApi q hc = lift $ authorizeV1QueryApi q hc
  authorizeV1MetadataApi q hc = lift $ authorizeV1MetadataApi q hc
  authorizeV2QueryApi q hc = lift $ authorizeV2QueryApi q hc

-- | The config API (/v1alpha1/config) handler
class Monad m => MonadConfigApiHandler m where
  runConfigApiHandler ::
    AppStateRef impl ->
    AppEnv ->
    Spock.SpockCtxT () m ()

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
  (Bool -> QErr -> Value) ->
  -- | `QErr` modifier
  (QErr -> QErr) ->
  APIHandler m a ->
  Spock.ActionT m ()
mkSpockAction appStateRef qErrEncoder qErrModifier apiHandler = do
  AppEnv {..} <- lift askAppEnv
  AppContext {..} <- liftIO $ getAppContext appStateRef
  req <- Spock.request
  let origHeaders = Wai.requestHeaders req
      ipAddress = Wai.getSourceFromFallback req
      pathInfo = Wai.rawPathInfo req

  -- Bytes are actually read from the socket here. Time this.
  (ioWaitTime, reqBody) <- withElapsedTime $ liftIO $ Wai.strictRequestBody req

  (requestId, headers) <- getRequestId origHeaders
  tracingCtx <- liftIO do
    -- B3 TraceIds can have a length of either 64 bits (16 hex chars) or 128 bits
    -- (32 hex chars). For 64-bit TraceIds, we pad them with zeros on the left to
    -- make them 128 bits long.
    let traceIdMaybe =
          lookup "X-B3-TraceId" headers >>= \rawTraceId ->
            if
                | Char8.length rawTraceId == 32 ->
                    Tracing.traceIdFromHex rawTraceId
                | Char8.length rawTraceId == 16 ->
                    Tracing.traceIdFromHex $ Char8.replicate 16 '0' <> rawTraceId
                | otherwise ->
                    Nothing
    for traceIdMaybe $ \traceId -> do
      freshSpanId <- Tracing.randomSpanId
      let parentSpanId = Tracing.spanIdFromHex =<< lookup "X-B3-SpanId" headers
          samplingState = Tracing.samplingStateFromHeader $ lookup "X-B3-Sampled" headers
      pure $ Tracing.TraceContext traceId freshSpanId parentSpanId samplingState

  let runTrace ::
        forall m1 a1.
        (MonadIO m1, MonadTrace m1) =>
        m1 a1 ->
        m1 a1
      runTrace = case tracingCtx of
        Nothing -> Tracing.newTrace appEnvTraceSamplingPolicy (fromString (B8.unpack pathInfo))
        Just ctx -> Tracing.newTraceWith ctx appEnvTraceSamplingPolicy (fromString (B8.unpack pathInfo))

  let getInfo parsedRequest = do
        authenticationResp <- lift (resolveUserInfo (_lsLogger appEnvLoggers) appEnvManager headers acAuthMode parsedRequest)
        authInfo <- onLeft authenticationResp (logErrorAndResp Nothing requestId req (reqBody, Nothing) False origHeaders (ExtraUserInfo Nothing) . qErrModifier)
        let (userInfo, _, authHeaders, extraUserInfo) = authInfo
        appContext <- liftIO $ getAppContext appStateRef
        (schemaCache, schemaCacheVer) <- liftIO $ readSchemaCacheRef appStateRef
        pure
          ( userInfo,
            authHeaders,
            HandlerCtx appContext schemaCache schemaCacheVer userInfo headers requestId ipAddress,
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
        res <- lift $ runHandler handlerState handler
        pure (res, userInfo, authHeaders, includeInternal, Nothing, extraUserInfo)
      AHPost handler -> do
        (userInfo, authHeaders, handlerState, includeInternal, extraUserInfo) <- getInfo Nothing
        (queryJSON, parsedReq) <-
          runExcept (parseBody reqBody) `onLeft` \e -> do
            logErrorAndResp (Just userInfo) requestId req (reqBody, Nothing) includeInternal origHeaders extraUserInfo (qErrModifier e)
        res <- lift $ runHandler handlerState $ handler parsedReq
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

        res <- lift $ runHandler handlerState $ handler parsedReq
        pure (res, userInfo, authHeaders, includeInternal, Just queryJSON, extraUserInfo)

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
          jsonResponse = J.encode $ qErrEncoder includeInternal qErr
          contentLength = ("Content-Length", B8.toStrict $ BB.toLazyByteString $ BB.int64Dec $ BL.length jsonResponse)
          allHeaders = [contentLength, jsonHeader]
      lift $ logHttpError (_lsLogger appEnvLoggers) appEnvLoggingSettings userInfo reqId waiReq req qErr headers httpLogMetadata
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
      lift $ logHttpSuccess (_lsLogger appEnvLoggers) appEnvLoggingSettings userInfo reqId waiReq req respBytes compressedResp qTime encodingType reqHeaders httpLoggingMetadata
      mapM_ setHeader allRespHeaders
      Spock.lazyBytes compressedResp

{- Note [Explicitly passing AppStateRef]
~~~~~~~~~~~~~~~~~~~~~~~
The AppStateRef is passed explicitly to `v1QueryHandler` and `v1MetadataHandler`
functions, so that they can update the schema cache in the ref.
They don't use it to read the latest AppContext or SchemaCache.
The AppContext or SchemaCache is read from the HandlerCtx.
This is to avoid any race conditions that can occur by reading AppContext/SchemaCache
one after the other.
-}

v1QueryHandler ::
  ( MonadIO m,
    MonadError QErr m,
    MonadBaseControl IO m,
    MonadMetadataApiAuthorization m,
    MonadTrace m,
    MonadReader HandlerCtx m,
    MonadMetadataStorageQueryAPI m,
    MonadResolveSource m,
    HasAppEnv m,
    EB.MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesNetwork m,
    MonadGetApiTimeLimit m,
    UserInfoM m,
    HasServerConfigCtx m
  ) =>
  AppStateRef impl ->
  RQLQuery ->
  m (HttpResponse EncJSON)
v1QueryHandler appStateRef query = do
  (liftEitherM . authorizeV1QueryApi query) =<< ask
  logger <- _lsLogger . appEnvLoggers <$> askAppEnv
  res <- bool (fst <$> action) (withSchemaCacheUpdate appStateRef logger Nothing action) $ queryModifiesSchemaCache query
  return $ HttpResponse res []
  where
    action = do
      appContext <- asks hcAppContext
      schemaCache <- asks hcSchemaCache
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
    MonadMetadataStorageQueryAPI m,
    MonadResolveSource m,
    MonadMetadataApiAuthorization m,
    MonadEventLogCleanup m,
    HasAppEnv m,
    ProvidesNetwork m,
    MonadGetApiTimeLimit m,
    UserInfoM m,
    HasServerConfigCtx m
  ) =>
  AppStateRef impl ->
  RQLMetadata ->
  m (HttpResponse EncJSON)
v1MetadataHandler appStateRef query = Tracing.newSpan "Metadata" $ do
  (liftEitherM . authorizeV1MetadataApi query) =<< ask
  logger <- _lsLogger . appEnvLoggers <$> askAppEnv
  appContext <- asks hcAppContext
  schemaCache <- asks hcSchemaCache
  r <-
    withSchemaCacheUpdate
      appStateRef
      logger
      Nothing
      $ runMetadataQuery
        appContext
        schemaCache
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
    EB.MonadQueryTags m,
    ProvidesNetwork m,
    UserInfoM m,
    HasServerConfigCtx m
  ) =>
  AppStateRef impl ->
  V2Q.RQLQuery ->
  m (HttpResponse EncJSON)
v2QueryHandler appStateRef query = Tracing.newSpan "v2 Query" $ do
  (liftEitherM . authorizeV2QueryApi query) =<< ask
  logger <- _lsLogger . appEnvLoggers <$> askAppEnv
  res <-
    bool (fst <$> dbAction) (withSchemaCacheUpdate appStateRef logger Nothing dbAction) $
      V2Q.queryModifiesSchema query
  return $ HttpResponse res []
  where
    -- Hit postgres
    dbAction = do
      schemaCache <- asks hcSchemaCache
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
    EB.MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m
  ) =>
  E.GraphQLQueryType ->
  GH.GQLBatchedReqs (GH.GQLReq GH.GQLQueryText) ->
  m (HttpLogGraphQLInfo, HttpResponse EncJSON)
v1Alpha1GQHandler queryType query = do
  AppEnv {..} <- askAppEnv
  AppContext {..} <- asks hcAppContext
  userInfo <- asks hcUser
  schemaCache <- lastBuiltSchemaCache <$> asks hcSchemaCache
  schemaCacheVer <- asks hcSchemaCacheVersion
  reqHeaders <- asks hcReqHeaders
  ipAddress <- asks hcSourceIpAddress
  requestId <- asks hcRequestId
  GH.runGQBatched acEnvironment acSQLGenCtx schemaCache schemaCacheVer acEnableAllowlist appEnvEnableReadOnlyMode appEnvPrometheusMetrics (_lsLogger appEnvLoggers) requestId acResponseInternalErrorsConfig userInfo ipAddress reqHeaders queryType query

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
    EB.MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m
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
    EB.MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m
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
    EB.MonadQueryTags m,
    MonadTrace m
  ) =>
  GE.GQLExplain ->
  m (HttpResponse EncJSON)
gqlExplainHandler query = do
  onlyAdmin
  schemaCache <- asks hcSchemaCache
  reqHeaders <- asks hcReqHeaders
  res <- GE.explainGQLQuery (lastBuiltSchemaCache schemaCache) reqHeaders query
  return $ HttpResponse res []

v1Alpha1PGDumpHandler :: (MonadIO m, MonadError QErr m, MonadReader HandlerCtx m) => PGD.PGDumpReqBody -> m APIResp
v1Alpha1PGDumpHandler b = do
  onlyAdmin
  schemaCache <- asks hcSchemaCache
  let sources = scSources (lastBuiltSchemaCache schemaCache)
      sourceName = PGD.prbSource b
      sourceConfig = unsafeSourceConfiguration @('Postgres 'Vanilla) =<< M.lookup sourceName sources
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
  FilePath ->
  Spock.ActionT m ()
consoleAssetsHandler logger loggingSettings dir path = do
  req <- Spock.request
  let reqHeaders = Wai.requestHeaders req
  -- '..' in paths need not be handed as it is resolved in the url by
  -- spock's routing. we get the expanded path.
  eFileContents <-
    liftIO $
      try $
        BL.readFile $
          joinPath [T.unpack dir, path]
  either (onError reqHeaders) onSuccess eFileContents
  where
    onSuccess c = do
      mapM_ setHeader headers
      Spock.lazyBytes c
    onError :: (MonadIO m, HttpLog m) => [HTTP.Header] -> IOException -> Spock.ActionT m ()
    onError hdrs = raiseGenericApiError logger loggingSettings hdrs . err404 NotFound . tshow
    fn = T.pack $ takeFileName path
    -- set gzip header if the filename ends with .gz
    (fileName, encHeader) = case T.stripSuffix ".gz" fn of
      Just v -> (v, [gzipHeader])
      Nothing -> (fn, [])
    mimeType = defaultMimeLookup fileName
    headers = ("Content-Type", mimeType) : encHeader

class (Monad m) => ConsoleRenderer m where
  renderConsole :: Text -> AuthMode -> TelemetryStatus -> Maybe Text -> Maybe Text -> m (Either String Text)

instance ConsoleRenderer m => ConsoleRenderer (Tracing.TraceT m) where
  renderConsole a b c d e = lift $ renderConsole a b c d e

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
  AppEnv ->
  Spock.SpockCtxT () m ()
configApiGetHandler appStateRef AppEnv {..} = do
  AppContext {..} <- liftIO $ getAppContext appStateRef
  Spock.get "v1alpha1/config" $
    mkSpockAction appStateRef encodeQErr id $
      mkGetHandler $ do
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
        return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue res) [])

data HasuraApp = HasuraApp
  { _hapApplication :: !Wai.Application,
    _hapAsyncActionSubscriptionState :: !ES.AsyncActionSubscriptionState,
    _hapShutdownWsServer :: !(IO ())
  }

mkWaiApp ::
  forall m impl.
  ( MonadIO m,
    MonadFix m,
    MonadStateless IO m,
    LA.Forall (LA.Pure m),
    ConsoleRenderer m,
    MonadVersionAPIWithExtraData m,
    HttpLog m,
    HasAppEnv m,
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
    MonadMetadataStorageQueryAPI m,
    MonadResolveSource m,
    EB.MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesNetwork m,
    MonadGetApiTimeLimit m
  ) =>
  (AppStateRef impl -> AppEnv -> Spock.SpockT m ()) ->
  AppStateRef impl ->
  AppEnv ->
  EKG.Store EKG.EmptyMetrics ->
  WS.WSServerEnv impl ->
  m HasuraApp
mkWaiApp
  setupHook
  appStateRef
  appEnv@AppEnv {..}
  ekgStore
  wsServerEnv = do
    AppContext {..} <- liftIO $ getAppContext appStateRef
    spockApp <- liftWithStateless $ \lowerIO ->
      Spock.spockAsApp $
        Spock.spockT lowerIO $
          httpApp setupHook appStateRef appEnv ekgStore

    let wsServerApp = WS.createWSServerApp acEnvironment (_lsEnabledLogTypes appEnvLoggingSettings) acAuthMode wsServerEnv appEnvWebSocketConnectionInitTimeout -- TODO: Lyndon: Can we pass environment through wsServerEnv?
        stopWSServer = WS.stopWSServerApp wsServerEnv

    waiApp <- liftWithStateless $ \lowerIO ->
      pure $ WSC.websocketsOr appEnvConnectionOptions (\ip conn -> lowerIO $ wsServerApp ip conn) spockApp

    return $ HasuraApp waiApp (ES._ssAsyncActions appEnvSubscriptionState) stopWSServer

httpApp ::
  forall m impl.
  ( MonadIO m,
    MonadFix m,
    MonadBaseControl IO m,
    ConsoleRenderer m,
    MonadVersionAPIWithExtraData m,
    HttpLog m,
    HasAppEnv m,
    UserAuthentication m,
    MonadMetadataApiAuthorization m,
    E.MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadTrace m,
    GH.MonadExecuteQuery m,
    MonadMetadataStorageQueryAPI m,
    HasResourceLimits m,
    MonadResolveSource m,
    EB.MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesNetwork m,
    MonadGetApiTimeLimit m
  ) =>
  (AppStateRef impl -> AppEnv -> Spock.SpockT m ()) ->
  AppStateRef impl ->
  AppEnv ->
  EKG.Store EKG.EmptyMetrics ->
  Spock.SpockT m ()
httpApp setupHook appStateRef appEnv@AppEnv {..} ekgStore = do
  -- Additional spock action to run
  setupHook appStateRef appEnv

  -- cors middleware
  -- todo: puru: create middleware dynamically based on the corsPolicy change
  Spock.middleware $
    corsMiddleware (acCorsPolicy <$> getAppContext appStateRef)

  appCtx@AppContext {..} <- liftIO $ getAppContext appStateRef
  -- API Console and Root Dir
  when (isConsoleEnabled acConsoleStatus && isMetadataEnabled appCtx) serveApiConsole

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
        endpoints <- liftIO $ scEndpoints <$> getSchemaCache appStateRef
        schemaCache <- lastBuiltSchemaCache <$> asks hcSchemaCache
        schemaCacheVer <- asks hcSchemaCacheVersion
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
        fmap JSONResp <$> runCustomEndpoint acEnvironment acSQLGenCtx schemaCache schemaCacheVer acEnableAllowlist appEnvEnableReadOnlyMode appEnvPrometheusMetrics (_lsLogger appEnvLoggers) requestId userInfo reqHeaders ipAddress req endpoints

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

  when (isMetadataEnabled appCtx) $ do
    Spock.post "v1/graphql/explain" gqlExplainAction

    Spock.post "v1alpha1/graphql/explain" gqlExplainAction

    Spock.post "v1/query" $
      spockAction encodeQErr id $ do
        mkPostHandler $ fmap (emptyHttpLogGraphQLInfo,) <$> mkAPIRespHandler (v1QueryHandler appStateRef)

    Spock.post "v1/metadata" $
      spockAction encodeQErr id $
        mkPostHandler $
          fmap (emptyHttpLogGraphQLInfo,) <$> mkAPIRespHandler (v1MetadataHandler appStateRef)

    Spock.post "v2/query" $
      spockAction encodeQErr id $
        mkPostHandler $
          fmap (emptyHttpLogGraphQLInfo,) <$> mkAPIRespHandler (v2QueryHandler appStateRef)

  when (isPGDumpEnabled appCtx) $
    Spock.post "v1alpha1/pg_dump" $
      spockAction encodeQErr id $
        mkPostHandler $
          fmap (emptyHttpLogGraphQLInfo,) <$> v1Alpha1PGDumpHandler

  when (isConfigEnabled appCtx) $ runConfigApiHandler appStateRef appEnv

  when (isGraphQLEnabled appCtx) $ do
    Spock.post "v1alpha1/graphql" $
      spockAction GH.encodeGQErr id $
        mkGQLRequestHandler $
          mkGQLAPIRespHandler $
            v1Alpha1GQHandler E.QueryHasura

    Spock.post "v1/graphql" $
      spockAction GH.encodeGQErr allMod200 $
        mkGQLRequestHandler $
          mkGQLAPIRespHandler $
            v1GQHandler

    Spock.post "v1beta1/relay" $
      spockAction GH.encodeGQErr allMod200 $
        mkGQLRequestHandler $
          mkGQLAPIRespHandler $
            v1GQRelayHandler

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

  when (isDeveloperAPIEnabled appCtx) $ do
    Spock.get "dev/ekg" $
      spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EKG.sampleAll ekgStore
          return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue $ EKG.sampleToJson respJ) [])
    -- This deprecated endpoint used to show the query plan cache pre-PDV.
    -- Eventually this endpoint can be removed.
    Spock.get "dev/plan_cache" $
      spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue J.Null) [])
    Spock.get "dev/subscriptions" $
      spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ ES.dumpSubscriptionsState False acLiveQueryOptions acStreamQueryOptions appEnvSubscriptionState
          return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue respJ) [])
    Spock.get "dev/subscriptions/extended" $
      spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ ES.dumpSubscriptionsState True acLiveQueryOptions acStreamQueryOptions appEnvSubscriptionState
          return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue respJ) [])
    Spock.get "dev/dataconnector/schema" $
      spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue openApiSchema) [])
  Spock.get "api/swagger/json" $
    spockAction encodeQErr id $
      mkGetHandler $ do
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
      lift $
        logHttpSuccess logger appEnvLoggingSettings Nothing reqId req (reqBody, Nothing) blMsg blMsg Nothing Nothing headers (emptyHttpLogMetadata @m)

    logError err = do
      req <- Spock.request
      reqBody <- liftIO $ Wai.strictRequestBody req
      let headers = Wai.requestHeaders req
      (reqId, _newHeaders) <- getRequestId headers
      lift $
        logHttpError logger appEnvLoggingSettings Nothing reqId req (reqBody, Nothing) err headers (emptyHttpLogMetadata @m)

    spockAction ::
      forall a.
      (FromJSON a) =>
      (Bool -> QErr -> Value) ->
      (QErr -> QErr) ->
      APIHandler m a ->
      Spock.ActionT m ()
    spockAction qErrEncoder qErrModifier apiHandler = mkSpockAction appStateRef qErrEncoder qErrModifier apiHandler

    -- all graphql errors should be of type 200
    allMod200 qe = qe {qeStatus = HTTP.status200}
    gqlExplainAction = do
      spockAction encodeQErr id $
        mkPostHandler $
          fmap (emptyHttpLogGraphQLInfo,) <$> mkAPIRespHandler gqlExplainHandler

    serveApiConsole = do
      -- redirect / to /console
      Spock.get Spock.root $ Spock.redirect "console"

      -- serve console html
      Spock.get ("console" <//> Spock.wildcard) $ \path -> do
        AppContext {..} <- liftIO $ getAppContext appStateRef
        req <- Spock.request
        let headers = Wai.requestHeaders req
        consoleHtml <- lift $ renderConsole path acAuthMode acEnableTelemetry appEnvConsoleAssetsDir appEnvConsoleSentryDsn
        either (raiseGenericApiError logger appEnvLoggingSettings headers . internalError . T.pack) Spock.html consoleHtml

    serveApiConsoleAssets = do
      -- serve static files if consoleAssetsDir is set
      for_ appEnvConsoleAssetsDir $ \dir ->
        Spock.get ("console/assets" <//> Spock.wildcard) $ \path -> do
          consoleAssetsHandler logger appEnvLoggingSettings dir (T.unpack path)

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
  lift $ logHttpError logger loggingSetting Nothing reqId req (reqBody, Nothing) qErr headers (emptyHttpLogMetadata @m)
  setHeader jsonHeader
  Spock.setStatus $ qeStatus qErr
  Spock.lazyBytes $ encode qErr
