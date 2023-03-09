{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Server.App
  ( APIResp (JSONResp, RawResp),
    ConsoleRenderer (..),
    MonadVersionAPIWithExtraData (..),
    Handler,
    HandlerCtx (hcReqHeaders, hcAppContext, hcAppEnv, hcUser),
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
import Control.Exception (IOException, throwIO, try)
import Control.Monad.Stateless
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Control qualified as MTC
import Data.Aeson hiding (json)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as J
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as B8
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
import Hasura.GraphQL.Logging (MonadQueryLog)
import Hasura.GraphQL.Transport.HTTP qualified as GH
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.GraphQL.Transport.WSServerApp qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.HTTP
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude hiding (get, put)
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
import Hasura.Server.Auth (AuthMode (..), UserAuthentication (..))
import Hasura.Server.Compression
import Hasura.Server.Cors
import Hasura.Server.Init hiding (checkFeatureFlag)
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Middleware (corsMiddleware)
import Hasura.Server.OpenAPI (buildOpenAPI)
import Hasura.Server.Rest
import Hasura.Server.SchemaCacheRef
  ( SchemaCacheRef,
    getSchemaCache,
    readSchemaCacheRef,
    withSchemaCacheUpdate,
  )
import Hasura.Server.Types
import Hasura.Server.Utils
import Hasura.Server.Version
import Hasura.Services
import Hasura.Session
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
import Web.Spock.Core ((<//>))
import Web.Spock.Core qualified as Spock

data HandlerCtx = HandlerCtx
  { hcAppContext :: !AppContext,
    hcAppEnv :: !AppEnv,
    hcUser :: !UserInfo,
    hcReqHeaders :: ![HTTP.Header],
    hcRequestId :: !RequestId,
    hcSourceIpAddress :: !Wai.IpAddress
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
      -- Tracing.HasReporter,
      Tracing.MonadTrace,
      HasResourceLimits,
      MonadResolveSource,
      HasServerConfigCtx,
      E.MonadGQLExecutionCheck,
      MonadEventLogCleanup,
      MonadQueryLog,
      EB.MonadQueryTags,
      GH.MonadExecuteQuery,
      MonadMetadataApiAuthorization,
      MonadMetadataStorage,
      MonadMetadataStorageQueryAPI,
      ProvidesNetwork
    )

instance MonadTrans Handler where
  lift = Handler . lift . lift

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
    AppContext ->
    AppEnv ->
    Spock.SpockCtxT () m ()

mapActionT ::
  (Monad m, Monad n) =>
  (m (MTC.StT (Spock.ActionCtxT ()) a) -> n (MTC.StT (Spock.ActionCtxT ()) a)) ->
  Spock.ActionT m a ->
  Spock.ActionT n a
mapActionT f tma = MTC.restoreT . pure =<< MTC.liftWith (\run -> f (run tma))

mkSpockAction ::
  forall m a.
  ( MonadIO m,
    MonadBaseControl IO m,
    FromJSON a,
    UserAuthentication (Tracing.TraceT m),
    HttpLog m,
    Tracing.HasReporter m,
    HasResourceLimits m
  ) =>
  AppContext ->
  AppEnv ->
  -- | `QErr` JSON encoder function
  (Bool -> QErr -> Value) ->
  -- | `QErr` modifier
  (QErr -> QErr) ->
  APIHandler (Tracing.TraceT m) a ->
  Spock.ActionT m ()
mkSpockAction appCtx@AppContext {..} appEnv@AppEnv {..} qErrEncoder qErrModifier apiHandler = do
  req <- Spock.request
  let origHeaders = Wai.requestHeaders req
      ipAddress = Wai.getSourceFromFallback req
      pathInfo = Wai.rawPathInfo req

  -- Bytes are actually read from the socket here. Time this.
  (ioWaitTime, reqBody) <- withElapsedTime $ liftIO $ Wai.strictRequestBody req

  (requestId, headers) <- getRequestId origHeaders
  tracingCtx <- liftIO $ Tracing.extractB3HttpContext headers

  let runTraceT ::
        forall m1 a1.
        (MonadIO m1, MonadBaseControl IO m1, Tracing.HasReporter m1) =>
        Tracing.TraceT m1 a1 ->
        m1 a1
      runTraceT = do
        (maybe Tracing.runTraceT Tracing.runTraceTInContext tracingCtx)
          appEnvTraceSamplingPolicy
          (fromString (B8.unpack pathInfo))

      getInfo parsedRequest = do
        authenticationResp <- lift (resolveUserInfo (_lsLogger appEnvLoggers) appEnvManager headers acAuthMode parsedRequest)
        authInfo <- onLeft authenticationResp (logErrorAndResp Nothing requestId req (reqBody, Nothing) False origHeaders (ExtraUserInfo Nothing) . qErrModifier)
        let (userInfo, _, authHeaders, extraUserInfo) = authInfo
        pure
          ( userInfo,
            authHeaders,
            HandlerCtx appCtx appEnv userInfo headers requestId ipAddress,
            shouldIncludeInternal (_uiRole userInfo) acResponseInternalErrorsConfig,
            extraUserInfo
          )

  mapActionT runTraceT $ do
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
      forall m3 a3 ctx.
      (MonadIO m3, HttpLog m3) =>
      Maybe UserInfo ->
      RequestId ->
      Wai.Request ->
      (BL.ByteString, Maybe Value) ->
      Bool ->
      [HTTP.Header] ->
      ExtraUserInfo ->
      QErr ->
      Spock.ActionCtxT ctx m3 a3
    logErrorAndResp userInfo reqId waiReq req includeInternal headers extraUserInfo qErr = do
      let httpLogMetadata = buildHttpLogMetadata @m3 emptyHttpLogGraphQLInfo extraUserInfo
          jsonResponse = J.encode $ qErrEncoder includeInternal qErr
          contentLength = ("Content-Length", B8.toStrict $ BB.toLazyByteString $ BB.int64Dec $ BL.length jsonResponse)
          allHeaders = [contentLength, jsonHeader]
      lift $ logHttpError (_lsLogger appEnvLoggers) appEnvLoggingSettings userInfo reqId waiReq req qErr headers httpLogMetadata
      mapM_ setHeader allHeaders
      Spock.setStatus $ qeStatus qErr
      Spock.lazyBytes jsonResponse

    logSuccessAndResp userInfo reqId waiReq req result qTime reqHeaders authHdrs httpLoggingMetadata = do
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

v1QueryHandler ::
  ( MonadIO m,
    MonadError QErr m,
    MonadBaseControl IO m,
    MonadMetadataApiAuthorization m,
    Tracing.MonadTrace m,
    MonadReader HandlerCtx m,
    MonadMetadataStorageQueryAPI m,
    MonadResolveSource m,
    EB.MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesNetwork m
  ) =>
  RQLQuery ->
  m (HttpResponse EncJSON)
v1QueryHandler query = do
  (liftEitherM . authorizeV1QueryApi query) =<< ask
  scRef <- asks (acCacheRef . hcAppContext)
  logger <- asks (_lsLogger . appEnvLoggers . hcAppEnv)
  res <- bool (fst <$> (action logger)) (withSchemaCacheUpdate scRef logger Nothing (action logger)) $ queryModifiesSchemaCache query
  return $ HttpResponse res []
  where
    action logger = do
      userInfo <- asks hcUser
      AppContext {..} <- asks hcAppContext
      schemaCache <- liftIO $ fst <$> readSchemaCacheRef acCacheRef
      instanceId <- asks (appEnvInstanceId . hcAppEnv)
      maintenanceMode <- asks (appEnvEnableMaintenanceMode . hcAppEnv)
      eventingMode <- asks (appEnvEventingMode . hcAppEnv)
      readOnlyMode <- asks (appEnvEnableReadOnlyMode . hcAppEnv)
      checkFeatureFlag <- asks (appEnvCheckFeatureFlag . hcAppEnv)
      let serverConfigCtx =
            ServerConfigCtx
              acFunctionPermsCtx
              acRemoteSchemaPermsCtx
              acSQLGenCtx
              maintenanceMode
              acExperimentalFeatures
              eventingMode
              readOnlyMode
              acDefaultNamingConvention
              acMetadataDefaults
              checkFeatureFlag
      runQuery
        acEnvironment
        logger
        instanceId
        userInfo
        schemaCache
        serverConfigCtx
        query

v1MetadataHandler ::
  ( MonadIO m,
    MonadError QErr m,
    MonadBaseControl IO m,
    MonadReader HandlerCtx m,
    Tracing.MonadTrace m,
    MonadMetadataStorageQueryAPI m,
    MonadResolveSource m,
    MonadMetadataApiAuthorization m,
    MonadEventLogCleanup m,
    ProvidesNetwork m
  ) =>
  RQLMetadata ->
  m (HttpResponse EncJSON)
v1MetadataHandler query = Tracing.trace "Metadata" $ do
  (liftEitherM . authorizeV1MetadataApi query) =<< ask
  userInfo <- asks hcUser
  AppContext {..} <- asks hcAppContext
  instanceId <- asks (appEnvInstanceId . hcAppEnv)
  logger <- asks (_lsLogger . appEnvLoggers . hcAppEnv)
  maintenanceMode <- asks (appEnvEnableMaintenanceMode . hcAppEnv)
  eventingMode <- asks (appEnvEventingMode . hcAppEnv)
  readOnlyMode <- asks (appEnvEnableReadOnlyMode . hcAppEnv)
  checkFeatureFlag <- asks (appEnvCheckFeatureFlag . hcAppEnv)
  let serverConfigCtx =
        ServerConfigCtx
          acFunctionPermsCtx
          acRemoteSchemaPermsCtx
          acSQLGenCtx
          maintenanceMode
          acExperimentalFeatures
          eventingMode
          readOnlyMode
          acDefaultNamingConvention
          acMetadataDefaults
          checkFeatureFlag
  r <-
    withSchemaCacheUpdate
      acCacheRef
      logger
      Nothing
      $ runMetadataQuery
        acEnvironment
        logger
        instanceId
        userInfo
        serverConfigCtx
        acCacheRef
        query
  pure $ HttpResponse r []

v2QueryHandler ::
  ( MonadIO m,
    MonadError QErr m,
    MonadBaseControl IO m,
    MonadMetadataApiAuthorization m,
    Tracing.MonadTrace m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    EB.MonadQueryTags m,
    ProvidesNetwork m
  ) =>
  V2Q.RQLQuery ->
  m (HttpResponse EncJSON)
v2QueryHandler query = Tracing.trace "v2 Query" $ do
  (liftEitherM . authorizeV2QueryApi query) =<< ask
  scRef <- asks (acCacheRef . hcAppContext)
  logger <- asks (_lsLogger . appEnvLoggers . hcAppEnv)
  res <-
    bool (fst <$> dbAction) (withSchemaCacheUpdate scRef logger Nothing dbAction) $
      V2Q.queryModifiesSchema query
  return $ HttpResponse res []
  where
    -- Hit postgres
    dbAction = do
      userInfo <- asks hcUser
      AppContext {..} <- asks hcAppContext
      schemaCache <- liftIO $ fst <$> readSchemaCacheRef acCacheRef
      instanceId <- asks (appEnvInstanceId . hcAppEnv)
      maintenanceMode <- asks (appEnvEnableMaintenanceMode . hcAppEnv)
      eventingMode <- asks (appEnvEventingMode . hcAppEnv)
      readOnlyMode <- asks (appEnvEnableReadOnlyMode . hcAppEnv)
      checkFeatureFlag <- asks (appEnvCheckFeatureFlag . hcAppEnv)
      let serverConfigCtx =
            ServerConfigCtx
              acFunctionPermsCtx
              acRemoteSchemaPermsCtx
              acSQLGenCtx
              maintenanceMode
              acExperimentalFeatures
              eventingMode
              readOnlyMode
              acDefaultNamingConvention
              acMetadataDefaults
              checkFeatureFlag

      V2Q.runQuery acEnvironment instanceId userInfo schemaCache serverConfigCtx query

v1Alpha1GQHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    Tracing.MonadTrace m,
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
  userInfo <- asks hcUser
  AppContext {..} <- asks hcAppContext
  reqHeaders <- asks hcReqHeaders
  ipAddress <- asks hcSourceIpAddress
  requestId <- asks hcRequestId
  logger <- asks (_lsLogger . appEnvLoggers . hcAppEnv)
  execCtx <- mkExecutionContext

  flip runReaderT execCtx $
    GH.runGQBatched acEnvironment logger requestId acResponseInternalErrorsConfig userInfo ipAddress reqHeaders queryType query

mkExecutionContext ::
  ( MonadIO m,
    MonadReader HandlerCtx m
  ) =>
  m E.ExecutionCtx
mkExecutionContext = do
  AppContext {..} <- asks hcAppContext
  (sc, scVer) <- liftIO $ readSchemaCacheRef acCacheRef
  logger <- asks (_lsLogger . appEnvLoggers . hcAppEnv)
  readOnlyMode <- asks (appEnvEnableReadOnlyMode . hcAppEnv)
  prometheusMetrics <- asks (appEnvPrometheusMetrics . hcAppEnv)
  pure $ E.ExecutionCtx logger acSQLGenCtx (lastBuiltSchemaCache sc) scVer acEnableAllowlist readOnlyMode prometheusMetrics

v1GQHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    Tracing.MonadTrace m,
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
    Tracing.MonadTrace m,
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
    Tracing.MonadTrace m
  ) =>
  GE.GQLExplain ->
  m (HttpResponse EncJSON)
gqlExplainHandler query = do
  onlyAdmin
  scRef <- asks (acCacheRef . hcAppContext)
  sc <- liftIO $ getSchemaCache scRef
  reqHeaders <- asks hcReqHeaders
  res <- GE.explainGQLQuery sc reqHeaders query
  return $ HttpResponse res []

v1Alpha1PGDumpHandler :: (MonadIO m, MonadError QErr m, MonadReader HandlerCtx m) => PGD.PGDumpReqBody -> m APIResp
v1Alpha1PGDumpHandler b = do
  onlyAdmin
  scRef <- asks (acCacheRef . hcAppContext)
  sc <- liftIO $ getSchemaCache scRef
  let sources = scSources sc
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
  Text ->
  Spock.ActionT m ()
consoleAssetsHandler logger loggingSettings dir path = do
  req <- Spock.request
  let reqHeaders = Wai.requestHeaders req
  -- '..' in paths need not be handed as it is resolved in the url by
  -- spock's routing. we get the expanded path.
  eFileContents <-
    liftIO $
      try @IOException do
        unless validFilename $ throwIO $ userError "invalid asset filename"
        BL.readFile $
          joinPath [T.unpack dir, pathStr]
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
  forall m.
  (MonadIO m, MonadBaseControl IO m, UserAuthentication (Tracing.TraceT m), HttpLog m, Tracing.HasReporter m, HasResourceLimits m) =>
  AppContext ->
  AppEnv ->
  Spock.SpockCtxT () m ()
configApiGetHandler appCtx@AppContext {..} appEnv =
  Spock.get "v1alpha1/config" $
    mkSpockAction appCtx appEnv encodeQErr id $
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
                (appEnvConsoleAssetsDir appEnv)
                acExperimentalFeatures
                acEnabledAPIs
                acDefaultNamingConvention
        return (emptyHttpLogGraphQLInfo, JSONResp $ HttpResponse (encJFromJValue res) [])

data HasuraApp = HasuraApp
  { _hapApplication :: !Wai.Application,
    _hapSchemaRef :: !SchemaCacheRef,
    _hapAsyncActionSubscriptionState :: !ES.AsyncActionSubscriptionState,
    _hapShutdownWsServer :: !(IO ())
  }

mkWaiApp ::
  forall m.
  ( MonadIO m,
    MonadFix m,
    MonadStateless IO m,
    LA.Forall (LA.Pure m),
    ConsoleRenderer m,
    MonadVersionAPIWithExtraData m,
    HttpLog m,
    UserAuthentication (Tracing.TraceT m),
    MonadMetadataApiAuthorization m,
    E.MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    WS.MonadWSLog m,
    Tracing.HasReporter m,
    GH.MonadExecuteQuery m,
    HasResourceLimits m,
    MonadMetadataStorageQueryAPI m,
    MonadResolveSource m,
    EB.MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesNetwork m
  ) =>
  (AppContext -> Spock.SpockT m ()) ->
  AppContext ->
  AppEnv ->
  EKG.Store EKG.EmptyMetrics ->
  m HasuraApp
mkWaiApp
  setupHook
  appCtx@AppContext {..}
  appEnv@AppEnv {..}
  ekgStore = do
    let getSchemaCache' = first lastBuiltSchemaCache <$> readSchemaCacheRef acCacheRef
    let corsPolicy = mkDefaultCorsPolicy acCorsConfig

    wsServerEnv <-
      WS.createWSServerEnv
        (_lsLogger appEnvLoggers)
        appEnvSubscriptionState
        acLiveQueryOptions
        acStreamQueryOptions
        getSchemaCache'
        appEnvManager
        corsPolicy
        acSQLGenCtx
        appEnvEnableReadOnlyMode
        acEnableAllowlist
        appEnvWebSocketKeepAlive
        appEnvServerMetrics
        appEnvPrometheusMetrics
        appEnvTraceSamplingPolicy

    spockApp <- liftWithStateless $ \lowerIO ->
      Spock.spockAsApp $
        Spock.spockT lowerIO $
          httpApp setupHook appCtx appEnv ekgStore

    let wsServerApp = WS.createWSServerApp acEnvironment (_lsEnabledLogTypes appEnvLoggingSettings) acAuthMode wsServerEnv appEnvWebSocketConnectionInitTimeout -- TODO: Lyndon: Can we pass environment through wsServerEnv?
        stopWSServer = WS.stopWSServerApp wsServerEnv

    waiApp <- liftWithStateless $ \lowerIO ->
      pure $ WSC.websocketsOr appEnvConnectionOptions (\ip conn -> lowerIO $ wsServerApp ip conn) spockApp

    return $ HasuraApp waiApp acCacheRef (ES._ssAsyncActions appEnvSubscriptionState) stopWSServer

httpApp ::
  forall m.
  ( MonadIO m,
    MonadFix m,
    MonadBaseControl IO m,
    ConsoleRenderer m,
    MonadVersionAPIWithExtraData m,
    HttpLog m,
    UserAuthentication (Tracing.TraceT m),
    MonadMetadataApiAuthorization m,
    E.MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    Tracing.HasReporter m,
    GH.MonadExecuteQuery m,
    MonadMetadataStorageQueryAPI m,
    HasResourceLimits m,
    MonadResolveSource m,
    EB.MonadQueryTags m,
    MonadEventLogCleanup m,
    ProvidesNetwork m
  ) =>
  (AppContext -> Spock.SpockT m ()) ->
  AppContext ->
  AppEnv ->
  EKG.Store EKG.EmptyMetrics ->
  Spock.SpockT m ()
httpApp setupHook appCtx@AppContext {..} appEnv@AppEnv {..} ekgStore = do
  -- Additional spock action to run
  setupHook appCtx

  -- cors middleware
  unless (isCorsDisabled acCorsConfig) $
    Spock.middleware $
      corsMiddleware (mkDefaultCorsPolicy acCorsConfig)

  -- API Console and Root Dir
  when (isConsoleEnabled acConsoleStatus && enableMetadata) serveApiConsole

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
            sc <- liftIO $ getSchemaCache acCacheRef
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
        forall n.
        ( MonadIO n,
          MonadBaseControl IO n,
          E.MonadGQLExecutionCheck n,
          MonadQueryLog n,
          GH.MonadExecuteQuery n,
          MonadMetadataStorage n,
          EB.MonadQueryTags n,
          HasResourceLimits n,
          ProvidesNetwork n
        ) =>
        RestRequest Spock.SpockMethod ->
        Handler (Tracing.TraceT n) (HttpLogGraphQLInfo, APIResp)
      customEndpointHandler restReq = do
        endpoints <- liftIO $ scEndpoints <$> getSchemaCache acCacheRef
        execCtx <- mkExecutionContext
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
        fmap JSONResp <$> runCustomEndpoint acEnvironment execCtx requestId userInfo reqHeaders ipAddress req endpoints

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

  when enableMetadata $ do
    Spock.post "v1/graphql/explain" gqlExplainAction

    Spock.post "v1alpha1/graphql/explain" gqlExplainAction

    Spock.post "v1/query" $
      spockAction encodeQErr id $ do
        mkPostHandler $ fmap (emptyHttpLogGraphQLInfo,) <$> mkAPIRespHandler v1QueryHandler

    Spock.post "v1/metadata" $
      spockAction encodeQErr id $
        mkPostHandler $
          fmap (emptyHttpLogGraphQLInfo,) <$> mkAPIRespHandler v1MetadataHandler

    Spock.post "v2/query" $
      spockAction encodeQErr id $
        mkPostHandler $
          fmap (emptyHttpLogGraphQLInfo,) <$> mkAPIRespHandler v2QueryHandler

  when enablePGDump $
    Spock.post "v1alpha1/pg_dump" $
      spockAction encodeQErr id $
        mkPostHandler $
          fmap (emptyHttpLogGraphQLInfo,) <$> v1Alpha1PGDumpHandler

  when enableConfig $ runConfigApiHandler appCtx appEnv

  when enableGraphQL $ do
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
        sc <- liftIO $ getSchemaCache acCacheRef
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
      forall a n.
      ( FromJSON a,
        MonadIO n,
        MonadBaseControl IO n,
        UserAuthentication (Tracing.TraceT n),
        HttpLog n,
        Tracing.HasReporter n,
        HasResourceLimits n
      ) =>
      (Bool -> QErr -> Value) ->
      (QErr -> QErr) ->
      APIHandler (Tracing.TraceT n) a ->
      Spock.ActionT n ()
    spockAction qErrEncoder qErrModifier apiHandler = mkSpockAction appCtx appEnv qErrEncoder qErrModifier apiHandler

    -- all graphql errors should be of type 200
    allMod200 qe = qe {qeStatus = HTTP.status200}
    gqlExplainAction = do
      spockAction encodeQErr id $
        mkPostHandler $
          fmap (emptyHttpLogGraphQLInfo,) <$> mkAPIRespHandler gqlExplainHandler
    enableGraphQL = isGraphQLEnabled appCtx
    enableMetadata = isMetadataEnabled appCtx
    enablePGDump = isPGDumpEnabled appCtx
    enableConfig = isConfigEnabled appCtx

    serveApiConsole = do
      -- redirect / to /console
      Spock.get Spock.root $ Spock.redirect "console"

      -- serve console html
      Spock.get ("console" <//> Spock.wildcard) $ \path -> do
        req <- Spock.request
        let headers = Wai.requestHeaders req
        consoleHtml <- lift $ renderConsole path acAuthMode acEnableTelemetry appEnvConsoleAssetsDir appEnvConsoleSentryDsn
        either (raiseGenericApiError logger appEnvLoggingSettings headers . internalError . T.pack) Spock.html consoleHtml

    serveApiConsoleAssets = do
      -- serve static files if consoleAssetsDir is set
      for_ appEnvConsoleAssetsDir $ \dir ->
        Spock.get ("console/assets" <//> Spock.wildcard) $ \path -> do
          consoleAssetsHandler logger appEnvLoggingSettings dir path

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
