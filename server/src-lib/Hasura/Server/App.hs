{-# LANGUAGE CPP #-}

module Hasura.Server.App
  ( APIResp (JSONResp),
    ConsoleRenderer (..),
    Handler,
    HandlerCtx (hcReqHeaders, hcServerCtx, hcUser),
    HasuraApp (HasuraApp),
    MonadConfigApiHandler (..),
    MonadMetadataApiAuthorization (..),
    SchemaCacheRef (..),
    ServerCtx (scManager),
    boolToText,
    configApiGetHandler,
    getSCFromRef,
    initialiseCache,
    isAdminSecretSet,
    logInconsObjs,
    mkGetHandler,
    mkSpockAction,
    mkWaiApp,
    onlyAdmin,
    renderHtmlTemplate,
    withSCUpdate,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.STM qualified as STM
import Control.Exception (IOException, try)
import Control.Monad.Stateless
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Control qualified as MTC
import Data.Aeson hiding (json)
import Data.Aeson qualified as J
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Either (fromRight)
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.IORef
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Conversions (convertText)
import Data.Text.Encoding qualified as T
import Data.Text.Extended
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Stats.Extended qualified as RTS
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.LiveQuery.Options qualified as EL
import Hasura.GraphQL.Execute.LiveQuery.Poll qualified as EL
import Hasura.GraphQL.Execute.LiveQuery.State qualified as EL
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
import Hasura.RQL.DDL.Schema
import Hasura.RQL.Types
import Hasura.Server.API.Config (runGetConfig)
import Hasura.Server.API.Metadata
import Hasura.Server.API.PGDump qualified as PGD
import Hasura.Server.API.Query
import Hasura.Server.API.V2Query qualified as V2Q
import Hasura.Server.Auth (AuthMode (..), UserAuthentication (..))
import Hasura.Server.Compression
import Hasura.Server.Cors
import Hasura.Server.Init
import Hasura.Server.Limits
import Hasura.Server.Logging
import Hasura.Server.Metrics (ServerMetrics)
import Hasura.Server.Middleware (corsMiddleware)
import Hasura.Server.OpenAPI (serveJSON)
import Hasura.Server.Rest
import Hasura.Server.Types
import Hasura.Server.Utils
import Hasura.Server.Version
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Mime (defaultMimeLookup)
import Network.Wai.Extended qualified as Wai
import Network.Wai.Handler.WebSockets.Custom qualified as WSC
import Network.WebSockets qualified as WS
import System.FilePath (joinPath, takeFileName)
import System.Mem (performMajorGC)
import System.Metrics qualified as EKG
import System.Metrics.Json qualified as EKG
import Text.Mustache qualified as M
import Web.Spock.Core ((<//>))
import Web.Spock.Core qualified as Spock

data SchemaCacheRef = SchemaCacheRef
  { -- | The idea behind explicit locking here is to
    --
    --   1. Allow maximum throughput for serving requests (/v1/graphql) (as each
    --      request reads the current schemacache)
    --   2. We don't want to process more than one request at any point of time
    --      which would modify the schema cache as such queries are expensive.
    --
    -- Another option is to consider removing this lock in place of `_scrCache ::
    -- MVar ...` if it's okay or in fact correct to block during schema update in
    -- e.g.  _wseGCtxMap. Vamshi says: It is theoretically possible to have a
    -- situation (in between building new schemacache and before writing it to
    -- the IORef) where we serve a request with a stale schemacache but I guess
    -- it is an okay trade-off to pay for a higher throughput (I remember doing a
    -- bunch of benchmarks to test this hypothesis).
    _scrLock :: MVar (),
    _scrCache :: IORef (RebuildableSchemaCache, SchemaCacheVer)
  }

data ServerCtx = ServerCtx
  { scLogger :: !(L.Logger L.Hasura),
    scCacheRef :: !SchemaCacheRef,
    scAuthMode :: !AuthMode,
    scManager :: !HTTP.Manager,
    scSQLGenCtx :: !SQLGenCtx,
    scEnabledAPIs :: !(S.HashSet API),
    scInstanceId :: !InstanceId,
    scLQState :: !EL.LiveQueriesState,
    scEnableAllowlist :: !Bool,
    scEkgStore :: !(EKG.Store EKG.EmptyMetrics),
    scResponseInternalErrorsConfig :: !ResponseInternalErrorsConfig,
    scEnvironment :: !Env.Environment,
    scRemoteSchemaPermsCtx :: !RemoteSchemaPermsCtx,
    scFunctionPermsCtx :: !FunctionPermissionsCtx,
    scEnableMaintenanceMode :: !MaintenanceMode,
    scExperimentalFeatures :: !(S.HashSet ExperimentalFeature),
    -- | this is only required for the short-term fix in https://github.com/hasura/graphql-engine-mono/issues/1770
    scEnabledLogTypes :: !(S.HashSet (L.EngineLogType L.Hasura))
  }

data HandlerCtx = HandlerCtx
  { hcServerCtx :: !ServerCtx,
    hcUser :: !UserInfo,
    hcReqHeaders :: ![HTTP.Header],
    hcRequestId :: !RequestId,
    hcSourceIpAddress :: !Wai.IpAddress
  }

type Handler m = ReaderT HandlerCtx (MetadataStorageT m)

data APIResp
  = JSONResp !(HttpResponse EncJSON)
  | RawResp !(HttpResponse BL.ByteString)

-- | API request handlers for different endpoints
data APIHandler m a where
  -- | A simple GET request
  AHGet :: !(Handler m (HttpLogMetadata m, APIResp)) -> APIHandler m void
  -- | A simple POST request that expects a request body from which an 'a' can be extracted
  AHPost :: !(a -> Handler m (HttpLogMetadata m, APIResp)) -> APIHandler m a
  -- | A general GraphQL request (query or mutation) for which the content of the query
  -- is made available to the handler for authentication.
  -- This is a more specific version of the 'AHPost' constructor.
  AHGraphQLRequest :: !(GH.ReqsText -> Handler m (HttpLogMetadata m, APIResp)) -> APIHandler m GH.ReqsText

boolToText :: Bool -> Text
boolToText = bool "false" "true"

isAdminSecretSet :: AuthMode -> Text
isAdminSecretSet AMNoAuth = boolToText False
isAdminSecretSet _ = boolToText True

getSCFromRef :: (MonadIO m) => SchemaCacheRef -> m SchemaCache
getSCFromRef scRef = lastBuiltSchemaCache . fst <$> liftIO (readIORef $ _scrCache scRef)

logInconsObjs :: L.Logger L.Hasura -> [InconsistentMetadata] -> IO ()
logInconsObjs logger objs =
  unless (null objs) $
    L.unLogger logger $ mkInconsMetadataLog objs

withSCUpdate ::
  (MonadIO m, MonadBaseControl IO m) =>
  SchemaCacheRef ->
  L.Logger L.Hasura ->
  Maybe (STM.TVar Bool) ->
  m (a, RebuildableSchemaCache) ->
  m a
withSCUpdate scr logger mLogCheckerTVar action =
  withMVarMasked lk $ \() -> do
    (!res, !newSC) <- action
    liftIO $ do
      -- update schemacache in IO reference
      modifyIORef' cacheRef $ \(_, prevVer) ->
        let !newVer = incSchemaCacheVer prevVer
         in (newSC, newVer)

      let inconsistentObjectsList = scInconsistentObjs $ lastBuiltSchemaCache newSC
          logInconsistentMetadata = logInconsObjs logger inconsistentObjectsList
      -- log any inconsistent objects only once and not everytime this method is called
      case mLogCheckerTVar of
        Nothing -> do logInconsistentMetadata
        Just logCheckerTVar -> do
          logCheck <- liftIO $ STM.readTVarIO logCheckerTVar
          if null inconsistentObjectsList && logCheck
            then do
              STM.atomically $ STM.writeTVar logCheckerTVar False
            else do
              when (not logCheck && not (null inconsistentObjectsList)) $ do
                STM.atomically $ STM.writeTVar logCheckerTVar True
                logInconsistentMetadata

    return res
  where
    SchemaCacheRef lk cacheRef = scr

mkGetHandler :: Handler m (HttpLogMetadata m, APIResp) -> APIHandler m ()
mkGetHandler = AHGet

mkPostHandler :: (a -> Handler m (HttpLogMetadata m, APIResp)) -> APIHandler m a
mkPostHandler = AHPost

mkGQLRequestHandler :: (GH.ReqsText -> Handler m (HttpLogMetadata m, APIResp)) -> APIHandler m GH.ReqsText
mkGQLRequestHandler = AHGraphQLRequest

mkAPIRespHandler :: (Functor m) => (a -> Handler m (HttpResponse EncJSON)) -> (a -> Handler m APIResp)
mkAPIRespHandler = (fmap . fmap) JSONResp

mkGQLAPIRespHandler ::
  (Functor m) =>
  (a -> Handler m (b, (HttpResponse EncJSON))) ->
  (a -> Handler m (b, APIResp))
mkGQLAPIRespHandler = (fmap . fmap . fmap) JSONResp

isMetadataEnabled :: ServerCtx -> Bool
isMetadataEnabled sc = S.member METADATA $ scEnabledAPIs sc

isGraphQLEnabled :: ServerCtx -> Bool
isGraphQLEnabled sc = S.member GRAPHQL $ scEnabledAPIs sc

isPGDumpEnabled :: ServerCtx -> Bool
isPGDumpEnabled sc = S.member PGDUMP $ scEnabledAPIs sc

isConfigEnabled :: ServerCtx -> Bool
isConfigEnabled sc = S.member CONFIG $ scEnabledAPIs sc

isDeveloperAPIEnabled :: ServerCtx -> Bool
isDeveloperAPIEnabled sc = S.member DEVELOPER $ scEnabledAPIs sc

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

setHeader :: MonadIO m => HTTP.Header -> Spock.ActionT m ()
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

instance MonadMetadataApiAuthorization m => MonadMetadataApiAuthorization (MetadataStorageT m) where
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
    ServerCtx ->
    -- | console assets directory
    Maybe Text ->
    Spock.SpockCtxT () m ()

-- instance (MonadIO m, UserAuthentication m, HttpLog m, Tracing.HasReporter m) => MonadConfigApiHandler (Tracing.TraceT m) where
--   runConfigApiHandler = configApiGetHandler

mapActionT ::
  (Monad m, Monad n) =>
  (m (MTC.StT (Spock.ActionCtxT ()) a) -> n (MTC.StT (Spock.ActionCtxT ()) a)) ->
  Spock.ActionT m a ->
  Spock.ActionT n a
mapActionT f tma = MTC.restoreT . pure =<< MTC.liftWith (\run -> f (run tma))

mkSpockAction ::
  (MonadIO m, MonadBaseControl IO m, FromJSON a, UserAuthentication (Tracing.TraceT m), HttpLog m, Tracing.HasReporter m, HasResourceLimits m) =>
  ServerCtx ->
  -- | `QErr` JSON encoder function
  (Bool -> QErr -> Value) ->
  -- | `QErr` modifier
  (QErr -> QErr) ->
  APIHandler (Tracing.TraceT m) a ->
  Spock.ActionT m ()
mkSpockAction serverCtx@ServerCtx {..} qErrEncoder qErrModifier apiHandler = do
  req <- Spock.request
  let origHeaders = Wai.requestHeaders req
      ipAddress = Wai.getSourceFromFallback req
      pathInfo = Wai.rawPathInfo req

  -- Bytes are actually read from the socket here. Time this.
  (ioWaitTime, reqBody) <- withElapsedTime $ liftIO $ Wai.strictRequestBody req

  (requestId, headers) <- getRequestId origHeaders
  tracingCtx <- liftIO $ Tracing.extractHttpContext headers
  handlerLimit <- lift askHTTPHandlerLimit

  let runTraceT ::
        forall m a.
        (MonadIO m, Tracing.HasReporter m) =>
        Tracing.TraceT m a ->
        m a
      runTraceT =
        maybe
          Tracing.runTraceT
          Tracing.runTraceTInContext
          tracingCtx
          (fromString (B8.unpack pathInfo))

      runHandler ::
        MonadBaseControl IO m =>
        HandlerCtx ->
        ReaderT HandlerCtx (MetadataStorageT m) a ->
        m (Either QErr a)
      runHandler handlerCtx handler =
        runMetadataStorageT $ flip runReaderT handlerCtx $ runResourceLimits handlerLimit $ handler

      getInfo parsedRequest = do
        authenticationResp <- lift (resolveUserInfo scLogger scManager headers scAuthMode parsedRequest)
        authInfo <- onLeft authenticationResp (logErrorAndResp Nothing requestId req (reqBody, Nothing) False origHeaders . qErrModifier)
        let (userInfo, _, authHeaders) = authInfo
        pure
          ( userInfo,
            authHeaders,
            HandlerCtx serverCtx userInfo headers requestId ipAddress,
            shouldIncludeInternal (_uiRole userInfo) scResponseInternalErrorsConfig
          )

  mapActionT runTraceT $ do
    -- Add the request ID to the tracing metadata so that we
    -- can correlate requests and traces
    lift $ Tracing.attachMetadata [("request_id", unRequestId requestId)]

    (serviceTime, (result, userInfo, authHeaders, includeInternal, queryJSON)) <- withElapsedTime $ case apiHandler of
      -- in the case of a simple get/post we don't have to send the webhook anything
      AHGet handler -> do
        (userInfo, authHeaders, handlerState, includeInternal) <- getInfo Nothing
        res <- lift $ runHandler handlerState handler
        pure (res, userInfo, authHeaders, includeInternal, Nothing)
      AHPost handler -> do
        (userInfo, authHeaders, handlerState, includeInternal) <- getInfo Nothing
        (queryJSON, parsedReq) <-
          runExcept (parseBody reqBody) `onLeft` \e ->
            logErrorAndResp (Just userInfo) requestId req (reqBody, Nothing) includeInternal origHeaders $ qErrModifier e
        res <- lift $ runHandler handlerState $ handler parsedReq
        pure (res, userInfo, authHeaders, includeInternal, Just queryJSON)
      -- in this case we parse the request _first_ and then send the request to the webhook for auth
      AHGraphQLRequest handler -> do
        (queryJSON, parsedReq) <-
          runExcept (parseBody reqBody) `onLeft` \e -> do
            -- if the request fails to parse, call the webhook without a request body
            -- TODO should we signal this to the webhook somehow?
            (userInfo, _, _, _) <- getInfo Nothing
            logErrorAndResp (Just userInfo) requestId req (reqBody, Nothing) False origHeaders $ qErrModifier e
        (userInfo, authHeaders, handlerState, includeInternal) <- getInfo (Just parsedReq)

        res <- lift $ runHandler handlerState $ handler parsedReq
        pure (res, userInfo, authHeaders, includeInternal, Just queryJSON)

    -- apply the error modifier
    let modResult = fmapL qErrModifier result

    -- log and return result
    case modResult of
      Left err ->
        logErrorAndResp (Just userInfo) requestId req (reqBody, queryJSON) includeInternal headers err
      Right (httpLoggingMetadata, res) ->
        logSuccessAndResp (Just userInfo) requestId req (reqBody, queryJSON) res (Just (ioWaitTime, serviceTime)) origHeaders authHeaders httpLoggingMetadata
  where
    logErrorAndResp ::
      (MonadIO m, HttpLog m) =>
      Maybe UserInfo ->
      RequestId ->
      Wai.Request ->
      (BL.ByteString, Maybe Value) ->
      Bool ->
      [HTTP.Header] ->
      QErr ->
      Spock.ActionCtxT ctx m a
    logErrorAndResp userInfo reqId waiReq req includeInternal headers qErr = do
      lift $ logHttpError scLogger scEnabledLogTypes userInfo reqId waiReq req qErr headers
      Spock.setStatus $ qeStatus qErr
      Spock.json $ qErrEncoder includeInternal qErr

    logSuccessAndResp userInfo reqId waiReq req result qTime reqHeaders authHdrs httpLoggingMetadata = do
      let (respBytes, respHeaders) = case result of
            JSONResp (HttpResponse encJson h) -> (encJToLBS encJson, pure jsonHeader <> h)
            RawResp (HttpResponse rawBytes h) -> (rawBytes, h)
          (compressedResp, mEncodingHeader, mCompressionType) = compressResponse (Wai.requestHeaders waiReq) respBytes
          encodingHeader = onNothing mEncodingHeader []
          reqIdHeader = (requestIdHeader, txtToBs $ unRequestId reqId)
          allRespHeaders = pure reqIdHeader <> encodingHeader <> respHeaders <> authHdrs
      lift $ logHttpSuccess scLogger scEnabledLogTypes userInfo reqId waiReq req respBytes compressedResp qTime mCompressionType reqHeaders httpLoggingMetadata
      mapM_ setHeader allRespHeaders
      Spock.lazyBytes compressedResp

v1QueryHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadMetadataApiAuthorization m,
    Tracing.MonadTrace m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    EB.MonadQueryTags m
  ) =>
  RQLQuery ->
  m (HttpResponse EncJSON)
v1QueryHandler query = do
  (liftEitherM . authorizeV1QueryApi query) =<< ask
  scRef <- asks (scCacheRef . hcServerCtx)
  logger <- asks (scLogger . hcServerCtx)
  res <- bool (fst <$> (action logger)) (withSCUpdate scRef logger Nothing (action logger)) $ queryModifiesSchemaCache query
  return $ HttpResponse res []
  where
    action logger = do
      userInfo <- asks hcUser
      scRef <- asks (scCacheRef . hcServerCtx)
      schemaCache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
      httpMgr <- asks (scManager . hcServerCtx)
      sqlGenCtx <- asks (scSQLGenCtx . hcServerCtx)
      instanceId <- asks (scInstanceId . hcServerCtx)
      env <- asks (scEnvironment . hcServerCtx)
      remoteSchemaPermsCtx <- asks (scRemoteSchemaPermsCtx . hcServerCtx)
      functionPermsCtx <- asks (scFunctionPermsCtx . hcServerCtx)
      maintenanceMode <- asks (scEnableMaintenanceMode . hcServerCtx)
      experimentalFeatures <- asks (scExperimentalFeatures . hcServerCtx)
      let serverConfigCtx = ServerConfigCtx functionPermsCtx remoteSchemaPermsCtx sqlGenCtx maintenanceMode experimentalFeatures
      runQuery
        env
        logger
        instanceId
        userInfo
        schemaCache
        httpMgr
        serverConfigCtx
        query

v1MetadataHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadReader HandlerCtx m,
    Tracing.MonadTrace m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    MonadMetadataApiAuthorization m
  ) =>
  RQLMetadata ->
  m (HttpResponse EncJSON)
v1MetadataHandler query = do
  (liftEitherM . authorizeV1MetadataApi query) =<< ask
  userInfo <- asks hcUser
  scRef <- asks (scCacheRef . hcServerCtx)
  schemaCache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
  httpMgr <- asks (scManager . hcServerCtx)
  sqlGenCtx <- asks (scSQLGenCtx . hcServerCtx)
  env <- asks (scEnvironment . hcServerCtx)
  instanceId <- asks (scInstanceId . hcServerCtx)
  logger <- asks (scLogger . hcServerCtx)
  remoteSchemaPermsCtx <- asks (scRemoteSchemaPermsCtx . hcServerCtx)
  functionPermsCtx <- asks (scFunctionPermsCtx . hcServerCtx)
  experimentalFeatures <- asks (scExperimentalFeatures . hcServerCtx)
  maintenanceMode <- asks (scEnableMaintenanceMode . hcServerCtx)
  let serverConfigCtx = ServerConfigCtx functionPermsCtx remoteSchemaPermsCtx sqlGenCtx maintenanceMode experimentalFeatures
  r <-
    withSCUpdate
      scRef
      logger
      Nothing
      $ runMetadataQuery
        env
        logger
        instanceId
        userInfo
        httpMgr
        serverConfigCtx
        schemaCache
        query
  pure $ HttpResponse r []

v2QueryHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadMetadataApiAuthorization m,
    Tracing.MonadTrace m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    EB.MonadQueryTags m
  ) =>
  V2Q.RQLQuery ->
  m (HttpResponse EncJSON)
v2QueryHandler query = do
  (liftEitherM . authorizeV2QueryApi query) =<< ask
  scRef <- asks (scCacheRef . hcServerCtx)
  logger <- asks (scLogger . hcServerCtx)
  res <-
    bool (fst <$> dbAction) (withSCUpdate scRef logger Nothing dbAction) $
      V2Q.queryModifiesSchema query
  return $ HttpResponse res []
  where
    -- Hit postgres
    dbAction = do
      userInfo <- asks hcUser
      scRef <- asks (scCacheRef . hcServerCtx)
      schemaCache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
      httpMgr <- asks (scManager . hcServerCtx)
      sqlGenCtx <- asks (scSQLGenCtx . hcServerCtx)
      instanceId <- asks (scInstanceId . hcServerCtx)
      env <- asks (scEnvironment . hcServerCtx)
      remoteSchemaPermsCtx <- asks (scRemoteSchemaPermsCtx . hcServerCtx)
      experimentalFeatures <- asks (scExperimentalFeatures . hcServerCtx)
      functionPermsCtx <- asks (scFunctionPermsCtx . hcServerCtx)
      maintenanceMode <- asks (scEnableMaintenanceMode . hcServerCtx)
      let serverConfigCtx = ServerConfigCtx functionPermsCtx remoteSchemaPermsCtx sqlGenCtx maintenanceMode experimentalFeatures
      V2Q.runQuery env instanceId userInfo schemaCache httpMgr serverConfigCtx query

v1Alpha1GQHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    Tracing.MonadTrace m,
    GH.MonadExecuteQuery m,
    MonadError QErr m,
    MonadReader HandlerCtx m,
    HttpLog m,
    MonadMetadataStorage (MetadataStorageT m),
    EB.MonadQueryTags m,
    HasResourceLimits m
  ) =>
  E.GraphQLQueryType ->
  GH.GQLBatchedReqs (GH.GQLReq GH.GQLQueryText) ->
  m (HttpLogMetadata m, HttpResponse EncJSON)
v1Alpha1GQHandler queryType query = do
  userInfo <- asks hcUser
  reqHeaders <- asks hcReqHeaders
  ipAddress <- asks hcSourceIpAddress
  requestId <- asks hcRequestId
  manager <- asks (scManager . hcServerCtx)
  scRef <- asks (scCacheRef . hcServerCtx)
  (sc, scVer) <- liftIO $ readIORef $ _scrCache scRef
  sqlGenCtx <- asks (scSQLGenCtx . hcServerCtx)
  enableAL <- asks (scEnableAllowlist . hcServerCtx)
  logger <- asks (scLogger . hcServerCtx)
  responseErrorsConfig <- asks (scResponseInternalErrorsConfig . hcServerCtx)
  env <- asks (scEnvironment . hcServerCtx)

  let execCtx =
        E.ExecutionCtx
          logger
          sqlGenCtx
          (lastBuiltSchemaCache sc)
          scVer
          manager
          enableAL

  flip runReaderT execCtx $
    GH.runGQBatched env logger requestId responseErrorsConfig userInfo ipAddress reqHeaders queryType query

mkExecutionContext ::
  ( MonadIO m,
    MonadReader HandlerCtx m
  ) =>
  m E.ExecutionCtx
mkExecutionContext = do
  manager <- asks (scManager . hcServerCtx)
  scRef <- asks (scCacheRef . hcServerCtx)
  (sc, scVer) <- liftIO $ readIORef $ _scrCache scRef
  sqlGenCtx <- asks (scSQLGenCtx . hcServerCtx)
  enableAL <- asks (scEnableAllowlist . hcServerCtx)
  logger <- asks (scLogger . hcServerCtx)
  pure $ E.ExecutionCtx logger sqlGenCtx (lastBuiltSchemaCache sc) scVer manager enableAL

v1GQHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    Tracing.MonadTrace m,
    GH.MonadExecuteQuery m,
    HttpLog m,
    MonadError QErr m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage (MetadataStorageT m),
    EB.MonadQueryTags m,
    HasResourceLimits m
  ) =>
  GH.GQLBatchedReqs (GH.GQLReq GH.GQLQueryText) ->
  m (HttpLogMetadata m, HttpResponse EncJSON)
v1GQHandler = v1Alpha1GQHandler E.QueryHasura

v1GQRelayHandler ::
  ( MonadIO m,
    MonadBaseControl IO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    Tracing.MonadTrace m,
    HttpLog m,
    GH.MonadExecuteQuery m,
    MonadError QErr m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage (MetadataStorageT m),
    EB.MonadQueryTags m,
    HasResourceLimits m
  ) =>
  GH.GQLBatchedReqs (GH.GQLReq GH.GQLQueryText) ->
  m (HttpLogMetadata m, HttpResponse EncJSON)
v1GQRelayHandler = v1Alpha1GQHandler E.QueryRelay

gqlExplainHandler ::
  forall m.
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadReader HandlerCtx m,
    MonadMetadataStorage (MetadataStorageT m),
    EB.MonadQueryTags m
  ) =>
  GE.GQLExplain ->
  m (HttpResponse EncJSON)
gqlExplainHandler query = do
  onlyAdmin
  scRef <- asks (scCacheRef . hcServerCtx)
  sc <- getSCFromRef scRef
  res <- GE.explainGQLQuery sc query
  return $ HttpResponse res []

v1Alpha1PGDumpHandler :: (MonadIO m, MonadError QErr m, MonadReader HandlerCtx m) => PGD.PGDumpReqBody -> m APIResp
v1Alpha1PGDumpHandler b = do
  onlyAdmin
  scRef <- asks (scCacheRef . hcServerCtx)
  sc <- getSCFromRef scRef
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
  HashSet (L.EngineLogType L.Hasura) ->
  Text ->
  FilePath ->
  Spock.ActionT m ()
consoleAssetsHandler logger enabledLogTypes dir path = do
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
    onError hdrs = raiseGenericApiError logger enabledLogTypes hdrs . err404 NotFound . tshow
    fn = T.pack $ takeFileName path
    -- set gzip header if the filename ends with .gz
    (fileName, encHeader) = case T.stripSuffix ".gz" fn of
      Just v -> (v, [gzipHeader])
      Nothing -> (fn, [])
    mimeType = defaultMimeLookup fileName
    headers = ("Content-Type", mimeType) : encHeader

class (Monad m) => ConsoleRenderer m where
  renderConsole :: Text -> AuthMode -> Bool -> Maybe Text -> m (Either String Text)

instance ConsoleRenderer m => ConsoleRenderer (Tracing.TraceT m) where
  renderConsole a b c d = lift $ renderConsole a b c d

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
  ServerCtx ->
  Maybe Text ->
  Spock.SpockCtxT () m ()
configApiGetHandler serverCtx@ServerCtx {..} consoleAssetsDir =
  Spock.get "v1alpha1/config" $
    mkSpockAction serverCtx encodeQErr id $
      mkGetHandler $ do
        onlyAdmin
        let res =
              runGetConfig
                scFunctionPermsCtx
                scRemoteSchemaPermsCtx
                scAuthMode
                scEnableAllowlist
                (EL._lqsOptions $ scLQState)
                consoleAssetsDir
                scExperimentalFeatures
        return (emptyHttpLogMetadata @m, JSONResp $ HttpResponse (encJFromJValue res) [])

data HasuraApp = HasuraApp
  { _hapApplication :: !Wai.Application,
    _hapSchemaRef :: !SchemaCacheRef,
    _hapAsyncActionSubscriptionState :: !EL.AsyncActionSubscriptionState,
    _hapShutdownWsServer :: !(IO ())
  }

-- TODO: Put Env into ServerCtx?

mkWaiApp ::
  forall m.
  ( MonadIO m,
    --     , MonadUnique m
    MonadStateless IO m,
    LA.Forall (LA.Pure m),
    ConsoleRenderer m,
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
    MonadMetadataStorage (MetadataStorageT m),
    MonadResolveSource m,
    EB.MonadQueryTags m
  ) =>
  (ServerCtx -> Spock.SpockT m ()) ->
  -- | Set of environment variables for reference in UIs
  Env.Environment ->
  -- | a 'L.Hasura' specific logger
  L.Logger L.Hasura ->
  SQLGenCtx ->
  -- | is AllowList enabled - TODO: change this boolean to sumtype
  Bool ->
  -- | HTTP manager so that we can re-use sessions
  HTTP.Manager ->
  -- | 'AuthMode' in which the application should operate in
  AuthMode ->
  CorsConfig ->
  -- | is console enabled - TODO: better type
  Bool ->
  -- | filepath to the console static assets directory - TODO: better type
  Maybe Text ->
  -- | is telemetry enabled
  Bool ->
  -- | each application, when run, gets an 'InstanceId'. this is used at various places including
  -- schema syncing and telemetry
  InstanceId ->
  -- | set of the enabled 'API's
  S.HashSet API ->
  EL.LiveQueriesOptions ->
  ResponseInternalErrorsConfig ->
  Maybe EL.LiveQueryPostPollHook ->
  SchemaCacheRef ->
  EKG.Store EKG.EmptyMetrics ->
  ServerMetrics ->
  RemoteSchemaPermsCtx ->
  FunctionPermissionsCtx ->
  WS.ConnectionOptions ->
  KeepAliveDelay ->
  MaintenanceMode ->
  -- | Set of the enabled experimental features
  S.HashSet ExperimentalFeature ->
  S.HashSet (L.EngineLogType L.Hasura) ->
  WSConnectionInitTimeout ->
  m HasuraApp
mkWaiApp
  setupHook
  env
  logger
  sqlGenCtx
  enableAL
  httpManager
  mode
  corsCfg
  enableConsole
  consoleAssetsDir
  enableTelemetry
  instanceId
  apis
  lqOpts
  responseErrorsConfig
  liveQueryHook
  schemaCacheRef
  ekgStore
  serverMetrics
  enableRSPermsCtx
  functionPermsCtx
  connectionOptions
  keepAliveDelay
  maintenanceMode
  experimentalFeatures
  enabledLogTypes
  wsConnInitTimeout = do
    let getSchemaCache = first lastBuiltSchemaCache <$> readIORef (_scrCache schemaCacheRef)

    let corsPolicy = mkDefaultCorsPolicy corsCfg
        postPollHook = fromMaybe (EL.defaultLiveQueryPostPollHook logger) liveQueryHook

    lqState <- liftIO $ EL.initLiveQueriesState lqOpts postPollHook
    wsServerEnv <-
      WS.createWSServerEnv
        logger
        lqState
        getSchemaCache
        httpManager
        corsPolicy
        sqlGenCtx
        enableAL
        keepAliveDelay
        serverMetrics

    let serverCtx =
          ServerCtx
            { scLogger = logger,
              scCacheRef = schemaCacheRef,
              scAuthMode = mode,
              scManager = httpManager,
              scSQLGenCtx = sqlGenCtx,
              scEnabledAPIs = apis,
              scInstanceId = instanceId,
              scLQState = lqState,
              scEnableAllowlist = enableAL,
              scEkgStore = ekgStore,
              scEnvironment = env,
              scResponseInternalErrorsConfig = responseErrorsConfig,
              scRemoteSchemaPermsCtx = enableRSPermsCtx,
              scFunctionPermsCtx = functionPermsCtx,
              scEnableMaintenanceMode = maintenanceMode,
              scExperimentalFeatures = experimentalFeatures,
              scEnabledLogTypes = enabledLogTypes
            }

    spockApp <- liftWithStateless $ \lowerIO ->
      Spock.spockAsApp $
        Spock.spockT lowerIO $
          httpApp setupHook corsCfg serverCtx enableConsole consoleAssetsDir enableTelemetry

    let wsServerApp = WS.createWSServerApp env enabledLogTypes mode wsServerEnv wsConnInitTimeout -- TODO: Lyndon: Can we pass environment through wsServerEnv?
        stopWSServer = WS.stopWSServerApp wsServerEnv

    waiApp <- liftWithStateless $ \lowerIO ->
      pure $ WSC.websocketsOr connectionOptions (\ip conn -> lowerIO $ wsServerApp ip conn) spockApp

    return $ HasuraApp waiApp schemaCacheRef (EL._lqsAsyncActions lqState) stopWSServer

initialiseCache :: MonadIO m => RebuildableSchemaCache -> m SchemaCacheRef
initialiseCache schemaCache = do
  cacheLock <- liftIO $ newMVar ()
  cacheCell <- liftIO $ newIORef (schemaCache, initSchemaCacheVer)
  let cacheRef = SchemaCacheRef cacheLock cacheCell
  pure cacheRef

httpApp ::
  forall m.
  ( MonadIO m,
    --     , MonadUnique m
    MonadBaseControl IO m,
    ConsoleRenderer m,
    HttpLog m,
    -- , UserAuthentication m
    UserAuthentication (Tracing.TraceT m),
    MonadMetadataApiAuthorization m,
    E.MonadGQLExecutionCheck m,
    MonadConfigApiHandler m,
    MonadQueryLog m,
    Tracing.HasReporter m,
    GH.MonadExecuteQuery m,
    MonadMetadataStorage (MetadataStorageT m),
    HasResourceLimits m,
    MonadResolveSource m,
    EB.MonadQueryTags m
  ) =>
  (ServerCtx -> Spock.SpockT m ()) ->
  CorsConfig ->
  ServerCtx ->
  Bool ->
  Maybe Text ->
  Bool ->
  Spock.SpockT m ()
httpApp setupHook corsCfg serverCtx enableConsole consoleAssetsDir enableTelemetry = do
  -- Additional spock action to run
  setupHook serverCtx

  -- cors middleware
  unless (isCorsDisabled corsCfg) $
    Spock.middleware $ corsMiddleware (mkDefaultCorsPolicy corsCfg)

  -- API Console and Root Dir
  when (enableConsole && enableMetadata) serveApiConsole

  -- Health check endpoint with logs
  let healthzAction = do
        sc <- getSCFromRef $ scCacheRef serverCtx
        eitherHealth <- runMetadataStorageT checkMetadataStorageHealth
        let dbOk = fromRight False eitherHealth
            okText = "OK"
            warnText = "WARN: inconsistent objects in schema"
            errorText = "ERROR"
            responseText =
              if null (scInconsistentObjs sc)
                then okText
                else warnText
        if dbOk
          then do
            logSuccess responseText
            Spock.setStatus HTTP.status200 >> Spock.text (LT.toStrict responseText)
          else do
            logError errorText
            Spock.setStatus HTTP.status500 >> Spock.text errorText

  Spock.get "healthz" healthzAction

  -- This is an alternative to `healthz` (See issue #6958)
  Spock.get "hasura/healthz" healthzAction

  Spock.get "v1/version" $ do
    logSuccess $ "version: " <> convertText currentVersion
    setHeader jsonHeader
    Spock.lazyBytes $ encode $ object ["version" .= currentVersion]

  let customEndpointHandler ::
        forall n.
        ( MonadIO n,
          MonadBaseControl IO n,
          E.MonadGQLExecutionCheck n,
          MonadQueryLog n,
          GH.MonadExecuteQuery n,
          MonadMetadataStorage (MetadataStorageT n),
          HttpLog n,
          EB.MonadQueryTags n,
          HasResourceLimits n
        ) =>
        RestRequest Spock.SpockMethod ->
        Handler (Tracing.TraceT n) (HttpLogMetadata n, APIResp)
      customEndpointHandler restReq = do
        scRef <- asks (scCacheRef . hcServerCtx)
        endpoints <- scEndpoints <$> getSCFromRef scRef
        execCtx <- mkExecutionContext
        env <- asks (scEnvironment . hcServerCtx)
        requestId <- asks hcRequestId
        userInfo <- asks hcUser
        reqHeaders <- asks hcReqHeaders
        ipAddress <- asks hcSourceIpAddress

        req <-
          restReq & traverse \case
            Spock.MethodStandard (Spock.HttpMethod m) ->
              pure $ EndpointMethod $ T.decodeUtf8 $ HTTP.renderStdMethod m
            _ -> throw400 BadRequest $ "Nonstandard method not allowed for REST endpoints"
        fmap JSONResp <$> runCustomEndpoint env execCtx requestId userInfo reqHeaders ipAddress req endpoints

  -- See Issue #291 for discussion around restified feature
  Spock.hookRouteAll ("api" <//> "rest" <//> Spock.wildcard) $ \wildcard -> do
    queryParams <- Spock.params
    body <- Spock.body
    method <- Spock.reqMethod

    -- This is where we decode the json encoded body args. They
    -- are treated as if they came from query arguments, but allow
    -- us to pass non-scalar values.
    let bodyParams = case J.decodeStrict body of
          Just (J.Object o) -> M.toList o
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
        mkPostHandler $ fmap (emptyHttpLogMetadata @m,) <$> mkAPIRespHandler v1QueryHandler

    Spock.post "v1/metadata" $
      spockAction encodeQErr id $
        mkPostHandler $ fmap (emptyHttpLogMetadata @m,) <$> mkAPIRespHandler v1MetadataHandler

    Spock.post "v2/query" $
      spockAction encodeQErr id $
        mkPostHandler $ fmap (emptyHttpLogMetadata @m,) <$> mkAPIRespHandler v2QueryHandler

  when enablePGDump $
    Spock.post "v1alpha1/pg_dump" $
      spockAction encodeQErr id $
        mkPostHandler $ fmap (emptyHttpLogMetadata @m,) <$> v1Alpha1PGDumpHandler

  when enableConfig $ runConfigApiHandler serverCtx consoleAssetsDir

  when enableGraphQL $ do
    Spock.post "v1alpha1/graphql" $
      spockAction GH.encodeGQErr id $
        mkGQLRequestHandler $ mkGQLAPIRespHandler $ v1Alpha1GQHandler E.QueryHasura

    Spock.post "v1/graphql" $
      spockAction GH.encodeGQErr allMod200 $
        mkGQLRequestHandler $ mkGQLAPIRespHandler v1GQHandler

    Spock.post "v1beta1/relay" $
      spockAction GH.encodeGQErr allMod200 $
        mkGQLRequestHandler $ mkGQLAPIRespHandler $ v1GQRelayHandler

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

  when (isDeveloperAPIEnabled serverCtx) $ do
    Spock.get "dev/ekg" $
      spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EKG.sampleAll $ scEkgStore serverCtx
          return (emptyHttpLogMetadata @m, JSONResp $ HttpResponse (encJFromJValue $ EKG.sampleToJson respJ) [])
    -- This deprecated endpoint used to show the query plan cache pre-PDV.
    -- Eventually this endpoint can be removed.
    Spock.get "dev/plan_cache" $
      spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          return (emptyHttpLogMetadata @m, JSONResp $ HttpResponse (encJFromJValue J.Null) [])
    Spock.get "dev/subscriptions" $
      spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState False $ scLQState serverCtx
          return (emptyHttpLogMetadata @m, JSONResp $ HttpResponse (encJFromJValue respJ) [])
    Spock.get "dev/subscriptions/extended" $
      spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState True $ scLQState serverCtx
          return (emptyHttpLogMetadata @m, JSONResp $ HttpResponse (encJFromJValue respJ) [])
  Spock.get "api/swagger/json" $
    spockAction encodeQErr id $
      mkGetHandler $ do
        onlyAdmin
        sc <- getSCFromRef $ scCacheRef serverCtx
        let json = serveJSON sc
        return (emptyHttpLogMetadata @m, JSONResp $ HttpResponse (encJFromJValue json) [])

  forM_ [Spock.GET, Spock.POST] $ \m -> Spock.hookAny m $ \_ -> do
    req <- Spock.request
    let headers = Wai.requestHeaders req
        qErr = err404 NotFound "resource does not exist"
    raiseGenericApiError logger (scEnabledLogTypes serverCtx) headers qErr
  where
    logger = scLogger serverCtx

    logSuccess msg = do
      let enabledLogTypes = scEnabledLogTypes serverCtx
      req <- Spock.request
      reqBody <- liftIO $ Wai.strictRequestBody req
      let headers = Wai.requestHeaders req
          blMsg = TL.encodeUtf8 msg
      (reqId, _newHeaders) <- getRequestId headers
      lift $
        logHttpSuccess logger enabledLogTypes Nothing reqId req (reqBody, Nothing) blMsg blMsg Nothing Nothing headers (emptyHttpLogMetadata @m)

    logError errmsg = do
      let enabledLogTypes = scEnabledLogTypes serverCtx
      req <- Spock.request
      reqBody <- liftIO $ Wai.strictRequestBody req
      let headers = Wai.requestHeaders req
      (reqId, _newHeaders) <- getRequestId headers
      lift $
        logHttpError logger enabledLogTypes Nothing reqId req (reqBody, Nothing) (internalError errmsg) headers

    spockAction ::
      forall a n.
      (FromJSON a, MonadIO n, MonadBaseControl IO n, UserAuthentication (Tracing.TraceT n), HttpLog n, Tracing.HasReporter n, HasResourceLimits n) =>
      (Bool -> QErr -> Value) ->
      (QErr -> QErr) ->
      APIHandler (Tracing.TraceT n) a ->
      Spock.ActionT n ()
    spockAction = mkSpockAction serverCtx

    -- all graphql errors should be of type 200
    allMod200 qe = qe {qeStatus = HTTP.status200}
    gqlExplainAction = do
      spockAction encodeQErr id $
        mkPostHandler $
          fmap (emptyHttpLogMetadata @m,) <$> mkAPIRespHandler gqlExplainHandler
    enableGraphQL = isGraphQLEnabled serverCtx
    enableMetadata = isMetadataEnabled serverCtx
    enablePGDump = isPGDumpEnabled serverCtx
    enableConfig = isConfigEnabled serverCtx

    serveApiConsole = do
      -- redirect / to /console
      Spock.get Spock.root $ Spock.redirect "console"

      -- serve static files if consoleAssetsDir is set
      onJust consoleAssetsDir $ \dir ->
        Spock.get ("console/assets" <//> Spock.wildcard) $ \path -> do
          consoleAssetsHandler logger (scEnabledLogTypes serverCtx) dir (T.unpack path)

      -- serve console html
      Spock.get ("console" <//> Spock.wildcard) $ \path -> do
        req <- Spock.request
        let headers = Wai.requestHeaders req
            authMode = scAuthMode serverCtx
            enabledLogTypes = scEnabledLogTypes serverCtx
        consoleHtml <- lift $ renderConsole path authMode enableTelemetry consoleAssetsDir
        either (raiseGenericApiError logger enabledLogTypes headers . internalError . T.pack) Spock.html consoleHtml

raiseGenericApiError ::
  (MonadIO m, HttpLog m) =>
  L.Logger L.Hasura ->
  HashSet (L.EngineLogType L.Hasura) ->
  [HTTP.Header] ->
  QErr ->
  Spock.ActionT m ()
raiseGenericApiError logger enabledLogTypes headers qErr = do
  req <- Spock.request
  reqBody <- liftIO $ Wai.strictRequestBody req
  (reqId, _newHeaders) <- getRequestId $ Wai.requestHeaders req
  lift $ logHttpError logger enabledLogTypes Nothing reqId req (reqBody, Nothing) qErr headers
  setHeader jsonHeader
  Spock.setStatus $ qeStatus qErr
  Spock.lazyBytes $ encode qErr
