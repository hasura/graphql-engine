{-# LANGUAGE CPP #-}

module Hasura.Server.App where

import           Hasura.Prelude                            hiding (get, put)

import qualified Control.Concurrent.Async.Lifted.Safe      as LA
import qualified Control.Monad.Trans.Control               as MTC
import qualified Data.Aeson                                as J
import qualified Data.ByteString.Char8                     as B8
import qualified Data.ByteString.Lazy                      as BL
import qualified Data.CaseInsensitive                      as CI
import qualified Data.Environment                          as Env
import qualified Data.HashMap.Strict                       as M
import qualified Data.HashSet                              as S
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as T
import qualified GHC.Stats.Extended                        as RTS
import qualified Network.HTTP.Client                       as HTTP
import qualified Network.HTTP.Types                        as HTTP
import qualified Network.Wai.Extended                      as Wai
import qualified Network.Wai.Handler.WebSockets.Custom     as WSC
import qualified Network.WebSockets                        as WS
import qualified System.Metrics                            as EKG
import qualified System.Metrics.Json                       as EKG
import qualified Text.Mustache                             as M
import qualified Web.Spock.Core                            as Spock

import           Control.Concurrent.MVar.Lifted
import           Control.Exception                         (IOException, try)
import           Control.Monad.Stateless
import           Control.Monad.Trans.Control               (MonadBaseControl)
import           Data.Aeson                                hiding (json)
import           Data.IORef
import           Data.String                               (fromString)
import           Data.Text.Extended
import           Network.Mime                              (defaultMimeLookup)
import           System.FilePath                           (joinPath, takeFileName)
import           Web.Spock.Core                            ((<//>))

import qualified Hasura.Backends.Postgres.SQL.Types        as PG
import qualified Hasura.GraphQL.Execute                    as E
import qualified Hasura.GraphQL.Execute.LiveQuery.Options  as EL
import qualified Hasura.GraphQL.Execute.LiveQuery.Poll     as EL
import qualified Hasura.GraphQL.Execute.LiveQuery.State    as EL
import qualified Hasura.GraphQL.Execute.Plan               as E
import qualified Hasura.GraphQL.Explain                    as GE
import qualified Hasura.GraphQL.Transport.HTTP             as GH
import qualified Hasura.GraphQL.Transport.HTTP.Protocol    as GH
import qualified Hasura.GraphQL.Transport.WebSocket        as WS
import qualified Hasura.GraphQL.Transport.WebSocket.Server as WS
import qualified Hasura.Logging                            as L
import qualified Hasura.Server.API.PGDump                  as PGD
import qualified Hasura.Server.API.V2Query                 as V2Q
import qualified Hasura.Tracing                            as Tracing

import           Hasura.Backends.Postgres.Execute.Types
import           Hasura.EncJSON
import           Hasura.GraphQL.Logging                    (MonadQueryLog)
import           Hasura.HTTP
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.API.Config                  (runGetConfig)
import           Hasura.Server.API.Metadata
import           Hasura.Server.API.Query
import           Hasura.Server.Auth                        (AuthMode (..), UserAuthentication (..))
import           Hasura.Server.Compression
import           Hasura.Server.Cors
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Middleware                  (corsMiddleware)
import           Hasura.Server.Rest
import           Hasura.Server.Types
import           Hasura.Server.Utils
import           Hasura.Server.Version
import           Hasura.Session

data SchemaCacheRef
  = SchemaCacheRef
  { _scrLock     :: MVar ()
  -- ^ The idea behind explicit locking here is to
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
  , _scrCache    :: IORef (RebuildableSchemaCache, SchemaCacheVer)
  , _scrOnChange :: IO ()
  -- ^ an action to run when schemacache changes
  }

data ServerCtx
  = ServerCtx
  { scLogger                       :: !(L.Logger L.Hasura)
  , scCacheRef                     :: !SchemaCacheRef
  , scAuthMode                     :: !AuthMode
  , scManager                      :: !HTTP.Manager
  , scSQLGenCtx                    :: !SQLGenCtx
  , scEnabledAPIs                  :: !(S.HashSet API)
  , scInstanceId                   :: !InstanceId
  -- , scPlanCache                    :: !E.PlanCache -- See Note [Temporarily disabling query plan caching]
  , scLQState                      :: !EL.LiveQueriesState
  , scEnableAllowlist              :: !Bool
  , scEkgStore                     :: !EKG.Store
  , scResponseInternalErrorsConfig :: !ResponseInternalErrorsConfig
  , scEnvironment                  :: !Env.Environment
  , scRemoteSchemaPermsCtx         :: !RemoteSchemaPermsCtx
  , scFunctionPermsCtx             :: !FunctionPermissionsCtx
  , scEnableMaintenanceMode        :: !MaintenanceMode
  , scExperimentalFeatures         :: !(S.HashSet ExperimentalFeature)
  }

data HandlerCtx
  = HandlerCtx
  { hcServerCtx       :: !ServerCtx
  , hcUser            :: !UserInfo
  , hcReqHeaders      :: ![HTTP.Header]
  , hcRequestId       :: !RequestId
  , hcSourceIpAddress :: !Wai.IpAddress
  }

type Handler m = ReaderT HandlerCtx (MetadataStorageT m)

data APIResp
  = JSONResp !(HttpResponse EncJSON)
  | RawResp  !(HttpResponse BL.ByteString)

type ReqsText = GH.GQLBatchedReqs GH.GQLQueryText

-- | API request handlers for different endpoints
data APIHandler m a where
  -- | A simple GET request
  AHGet :: !(Handler m (HTTPLoggingMetadata m, APIResp)) -> APIHandler m void
  -- | A simple POST request that expects a request body from which an 'a' can be extracted
  AHPost :: !(a -> Handler m (HTTPLoggingMetadata m, APIResp)) -> APIHandler m a
  -- | A general GraphQL request (query or mutation) for which the content of the query
  -- is made available to the handler for authentication.
  -- This is a more specific version of the 'AHPost' constructor.
  AHGraphQLRequest :: !(ReqsText -> Handler m (HTTPLoggingMetadata m, APIResp)) -> APIHandler m ReqsText

boolToText :: Bool -> Text
boolToText = bool "false" "true"

isAdminSecretSet :: AuthMode -> Text
isAdminSecretSet AMNoAuth = boolToText False
isAdminSecretSet _        = boolToText True

getSCFromRef :: (MonadIO m) => SchemaCacheRef -> m SchemaCache
getSCFromRef scRef = lastBuiltSchemaCache . fst <$> liftIO (readIORef $ _scrCache scRef)

logInconsObjs :: L.Logger L.Hasura -> [InconsistentMetadata] -> IO ()
logInconsObjs logger objs =
  unless (null objs) $ L.unLogger logger $ mkInconsMetadataLog objs

withSCUpdate
  :: (MonadIO m, MonadBaseControl IO m)
  => SchemaCacheRef -> L.Logger L.Hasura -> m (a, RebuildableSchemaCache) -> m a
withSCUpdate scr logger action =
  withMVarMasked lk $ \() -> do
    (!res, !newSC) <- action
    liftIO $ do
      -- update schemacache in IO reference



      modifyIORef' cacheRef $ \(_, prevVer) ->
        let !newVer = incSchemaCacheVer prevVer
          in (newSC, newVer)
      -- log any inconsistent objects
      logInconsObjs logger $ scInconsistentObjs $ lastBuiltSchemaCache newSC
      onChange
    return res
  where
    SchemaCacheRef lk cacheRef onChange = scr

mkGetHandler :: Handler m (HTTPLoggingMetadata m, APIResp) -> APIHandler m ()
mkGetHandler = AHGet

mkPostHandler :: (a -> Handler m (HTTPLoggingMetadata m, APIResp)) -> APIHandler m a
mkPostHandler = AHPost

mkGQLRequestHandler :: (ReqsText -> Handler m (HTTPLoggingMetadata m, APIResp)) -> APIHandler m ReqsText
mkGQLRequestHandler = AHGraphQLRequest

mkAPIRespHandler :: (Functor m) => (a -> Handler m (HttpResponse EncJSON)) -> (a -> Handler m APIResp)
mkAPIRespHandler = (fmap . fmap) JSONResp

mkGQLAPIRespHandler
  :: (Functor m)
  => (a -> Handler m (b, (HttpResponse EncJSON)))
  -> (a -> Handler m (b, APIResp))
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
parseBody :: (FromJSON a, MonadError QErr m) => BL.ByteString -> m a
parseBody reqBody =
  case eitherDecode' reqBody of
    Left e     -> throw400 InvalidJSON (T.pack e)
    Right jVal -> decodeValue jVal

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
  authorizeV1QueryApi
    :: HasVersion => RQLQuery -> HandlerCtx -> m (Either QErr ())

  authorizeV1MetadataApi
    :: HasVersion => RQLMetadata -> HandlerCtx -> m (Either QErr ())

  authorizeV2QueryApi
    :: HasVersion => V2Q.RQLQuery -> HandlerCtx -> m (Either QErr ())

instance MonadMetadataApiAuthorization m => MonadMetadataApiAuthorization (ReaderT r m) where
  authorizeV1QueryApi q hc    = lift $ authorizeV1QueryApi q hc
  authorizeV1MetadataApi q hc = lift $ authorizeV1MetadataApi q hc
  authorizeV2QueryApi q hc    = lift $ authorizeV2QueryApi q hc

instance MonadMetadataApiAuthorization m => MonadMetadataApiAuthorization (MetadataStorageT m) where
  authorizeV1QueryApi q hc    = lift $ authorizeV1QueryApi q hc
  authorizeV1MetadataApi q hc = lift $ authorizeV1MetadataApi q hc
  authorizeV2QueryApi q hc    = lift $ authorizeV2QueryApi q hc

instance MonadMetadataApiAuthorization m => MonadMetadataApiAuthorization (Tracing.TraceT m) where
  authorizeV1QueryApi q hc    = lift $ authorizeV1QueryApi q hc
  authorizeV1MetadataApi q hc = lift $ authorizeV1MetadataApi q hc
  authorizeV2QueryApi q hc    = lift $ authorizeV2QueryApi q hc

-- | The config API (/v1alpha1/config) handler
class Monad m => MonadConfigApiHandler m where
  runConfigApiHandler
    :: HasVersion
    => ServerCtx
    -> Maybe Text
    -- ^ console assets directory
    -> Spock.SpockCtxT () m ()

-- instance (MonadIO m, UserAuthentication m, HttpLog m, Tracing.HasReporter m) => MonadConfigApiHandler (Tracing.TraceT m) where
--   runConfigApiHandler = configApiGetHandler

mapActionT
  :: (Monad m, Monad n)
  => (m (MTC.StT (Spock.ActionCtxT ()) a) -> n (MTC.StT (Spock.ActionCtxT ()) a))
  -> Spock.ActionT m a
  -> Spock.ActionT n a
mapActionT f tma = MTC.restoreT . pure =<< MTC.liftWith (\run -> f (run tma))

-- | Resource limits, represented by a function which modifies IO actions to
-- enforce those limits by throwing errors using 'MonadError' in the case
-- where they are exceeded.

newtype ResourceLimits = ResourceLimits
  { runResourceLimits :: forall m a. (MonadBaseControl IO m, MonadError QErr m) => m a -> m a
  }

-- | Monads which support resource (memory, CPU time, etc.) limiting
class Monad m => HasResourceLimits m where
  askResourceLimits :: m ResourceLimits

  -- A default for monad transformer instances
  default askResourceLimits
    :: (m ~ t n, MonadTrans t, HasResourceLimits n)
    => m ResourceLimits
  askResourceLimits = lift askResourceLimits

instance HasResourceLimits m => HasResourceLimits (ReaderT r m)
instance HasResourceLimits m => HasResourceLimits (ExceptT e m)
instance HasResourceLimits m => HasResourceLimits (Tracing.TraceT m)

mkSpockAction
  :: (HasVersion, MonadIO m, MonadBaseControl IO m, FromJSON a, ToJSON a, UserAuthentication (Tracing.TraceT m), HttpLog m, Tracing.HasReporter m, HasResourceLimits m)
  => ServerCtx
  -> (Bool -> QErr -> Value)
  -- ^ `QErr` JSON encoder function
  -> (QErr -> QErr)
  -- ^ `QErr` modifier
  -> APIHandler (Tracing.TraceT m) a
  -> Spock.ActionT m ()
mkSpockAction serverCtx qErrEncoder qErrModifier apiHandler = do
    req <- Spock.request
    -- Bytes are actually read from the socket here. Time this.
    (ioWaitTime, reqBody) <- withElapsedTime $ liftIO $ Wai.strictRequestBody req
    let headers = Wai.requestHeaders req
        authMode = scAuthMode serverCtx
        manager = scManager serverCtx
        ipAddress = Wai.getSourceFromFallback req
        pathInfo = Wai.rawPathInfo req

    tracingCtx <- liftIO $ Tracing.extractHttpContext headers

    let runTraceT
          :: forall m a
           . (MonadIO m, Tracing.HasReporter m)
          => Tracing.TraceT m a
          -> m a
        runTraceT = maybe
          Tracing.runTraceT
          Tracing.runTraceTInContext
          tracingCtx
          (fromString (B8.unpack pathInfo))

    requestId <- getRequestId headers

    mapActionT runTraceT $ do
      -- Add the request ID to the tracing metadata so that we
      -- can correlate requests and traces
      lift $ Tracing.attachMetadata [("request_id", unRequestId requestId)]

      let getInfo parsedRequest = do
            userInfoE <- fmap fst <$> lift (resolveUserInfo logger manager headers authMode parsedRequest)
            userInfo  <- onLeft userInfoE (logErrorAndResp Nothing requestId req (reqBody, Nothing) False headers . qErrModifier)
            let handlerState = HandlerCtx serverCtx userInfo headers requestId ipAddress
                includeInternal = shouldIncludeInternal (_uiRole userInfo) $
                                  scResponseInternalErrorsConfig serverCtx
            pure (userInfo, handlerState, includeInternal)
      limits <- lift askResourceLimits
      let runHandler
            :: MonadBaseControl IO m
            => HandlerCtx
            -> ReaderT HandlerCtx (MetadataStorageT m) a
            -> m (Either QErr a)
          runHandler st = runMetadataStorageT . flip runReaderT st . runResourceLimits limits

      (serviceTime, (result, userInfo, includeInternal, query)) <- withElapsedTime $ case apiHandler of
        -- in the case of a simple get/post we don't have to send the webhook anything
        AHGet handler -> do
          (userInfo, handlerState, includeInternal) <- getInfo Nothing
          res <- lift $ runHandler handlerState handler
          return (res , userInfo, includeInternal, Nothing)
        AHPost handler -> do
          (userInfo, handlerState, includeInternal) <- getInfo Nothing
          parsedReqE <- runExceptT $ parseBody reqBody
          parsedReq  <- onLeft parsedReqE (logErrorAndResp (Just userInfo) requestId req (reqBody, Nothing) includeInternal headers . qErrModifier)
          res <- lift $ runHandler handlerState $ handler parsedReq
          return (res, userInfo, includeInternal, Just parsedReq)
        -- in this case we parse the request _first_ and then send the request to the webhook for auth
        AHGraphQLRequest handler -> do
          parsedReqE <- runExceptT $ parseBody reqBody
          parsedReq  <- onLeft parsedReqE (logErrorAndResp Nothing requestId req (reqBody, Nothing) False headers . qErrModifier)
          (userInfo, handlerState, includeInternal) <- getInfo (Just parsedReq)
          res <- lift $ runHandler handlerState $ handler parsedReq
          return (res, userInfo, includeInternal, Just parsedReq)

      -- apply the error modifier
      let modResult = fmapL qErrModifier result

      -- log and return result
      case modResult of
        Left err  -> logErrorAndResp (Just userInfo) requestId req (reqBody, toJSON <$> query) includeInternal headers err
        Right (httpLoggingMetadata, res) ->
          logSuccessAndResp (Just userInfo) requestId req (reqBody, toJSON <$> query) res (Just (ioWaitTime, serviceTime)) headers httpLoggingMetadata

    where
      logger = scLogger serverCtx

      logErrorAndResp
        :: (MonadIO m, HttpLog m)
        => Maybe UserInfo
        -> RequestId
        -> Wai.Request
        -> (BL.ByteString, Maybe Value)
        -> Bool
        -> [HTTP.Header]
        -> QErr
        -> Spock.ActionCtxT ctx m a
      logErrorAndResp userInfo reqId waiReq req includeInternal headers qErr = do
        lift $ logHttpError logger userInfo reqId waiReq req qErr headers
        Spock.setStatus $ qeStatus qErr
        Spock.json $ qErrEncoder includeInternal qErr

      logSuccessAndResp userInfo reqId waiReq reqBody result qTime reqHeaders httpLoggingMetadata =
        case result of
          JSONResp (HttpResponse encJson h) ->
            possiblyCompressedLazyBytes userInfo reqId waiReq reqBody qTime (encJToLBS encJson)
              (pure jsonHeader <> h) reqHeaders httpLoggingMetadata
          RawResp (HttpResponse rawBytes h) ->
            possiblyCompressedLazyBytes userInfo reqId waiReq reqBody qTime rawBytes h reqHeaders httpLoggingMetadata

      possiblyCompressedLazyBytes userInfo reqId waiReq req qTime respBytes respHeaders reqHeaders httpLoggingMetadata = do
        let (compressedResp, mEncodingHeader, mCompressionType) =
              compressResponse (Wai.requestHeaders waiReq) respBytes
            encodingHeader = onNothing mEncodingHeader []
            reqIdHeader = (requestIdHeader, txtToBs $ unRequestId reqId)
            allRespHeaders = pure reqIdHeader <> encodingHeader <> respHeaders
        lift $ logHttpSuccess logger userInfo reqId waiReq
               req respBytes compressedResp qTime mCompressionType reqHeaders httpLoggingMetadata
        mapM_ setHeader allRespHeaders
        Spock.lazyBytes compressedResp

v1QueryHandler
  :: ( HasVersion, MonadIO m, MonadBaseControl IO m, MonadMetadataApiAuthorization m, Tracing.MonadTrace m
     , MonadReader HandlerCtx m, MonadMetadataStorage m, MonadResolveSource m
     )
  => RQLQuery
  -> m (HttpResponse EncJSON)
v1QueryHandler query = do
  (liftEitherM . authorizeV1QueryApi query) =<< ask
  scRef  <- asks (scCacheRef . hcServerCtx)
  logger <- asks (scLogger . hcServerCtx)
  res    <- bool (fst <$> action) (withSCUpdate scRef logger action) $ queryModifiesSchemaCache query
  return $ HttpResponse res []
  where
    action = do
      userInfo             <- asks hcUser
      scRef                <- asks (scCacheRef . hcServerCtx)
      schemaCache          <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
      httpMgr              <- asks (scManager . hcServerCtx)
      sqlGenCtx            <- asks (scSQLGenCtx . hcServerCtx)
      instanceId           <- asks (scInstanceId . hcServerCtx)
      env                  <- asks (scEnvironment . hcServerCtx)
      remoteSchemaPermsCtx <- asks (scRemoteSchemaPermsCtx . hcServerCtx)
      functionPermsCtx     <- asks (scFunctionPermsCtx . hcServerCtx)
      maintenanceMode      <- asks (scEnableMaintenanceMode . hcServerCtx)
      experimentalFeatures <- asks (scExperimentalFeatures . hcServerCtx)
      let serverConfigCtx = ServerConfigCtx functionPermsCtx remoteSchemaPermsCtx sqlGenCtx maintenanceMode experimentalFeatures
      runQuery env instanceId userInfo schemaCache httpMgr
               serverConfigCtx query

v1MetadataHandler
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , MonadReader HandlerCtx m
     , Tracing.MonadTrace m
     , MonadMetadataStorage m
     , MonadResolveSource m
     , MonadMetadataApiAuthorization m
     )
  => RQLMetadata -> m (HttpResponse EncJSON)
v1MetadataHandler query = do
  (liftEitherM . authorizeV1MetadataApi query) =<< ask
  userInfo             <- asks hcUser
  scRef                <- asks (scCacheRef . hcServerCtx)
  schemaCache          <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
  httpMgr              <- asks (scManager . hcServerCtx)
  sqlGenCtx            <- asks (scSQLGenCtx . hcServerCtx)
  env                  <- asks (scEnvironment . hcServerCtx)
  instanceId           <- asks (scInstanceId . hcServerCtx)
  logger               <- asks (scLogger . hcServerCtx)
  remoteSchemaPermsCtx <- asks (scRemoteSchemaPermsCtx . hcServerCtx)
  functionPermsCtx     <- asks (scFunctionPermsCtx . hcServerCtx)
  experimentalFeatures <- asks (scExperimentalFeatures . hcServerCtx)
  maintenanceMode      <- asks (scEnableMaintenanceMode . hcServerCtx)
  let serverConfigCtx = ServerConfigCtx functionPermsCtx remoteSchemaPermsCtx sqlGenCtx maintenanceMode experimentalFeatures
  r <- withSCUpdate scRef logger $
       runMetadataQuery env instanceId userInfo httpMgr serverConfigCtx
                        schemaCache query
  pure $ HttpResponse r []

v2QueryHandler
  :: ( HasVersion, MonadIO m, MonadBaseControl IO m, MonadMetadataApiAuthorization m, Tracing.MonadTrace m
     , MonadReader HandlerCtx m
     , MonadMetadataStorage m
     , MonadResolveSource m
     )
  => V2Q.RQLQuery
  -> m (HttpResponse EncJSON)
v2QueryHandler query = do
  (liftEitherM . authorizeV2QueryApi query) =<< ask
  scRef  <- asks (scCacheRef . hcServerCtx)
  logger <- asks (scLogger . hcServerCtx)
  res    <- bool (fst <$> dbAction) (withSCUpdate scRef logger dbAction) $
            V2Q.queryModifiesSchema query
  return $ HttpResponse res []
  where
    -- Hit postgres
    dbAction = do
      userInfo    <- asks hcUser
      scRef       <- asks (scCacheRef . hcServerCtx)
      schemaCache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
      httpMgr     <- asks (scManager . hcServerCtx)
      sqlGenCtx   <- asks (scSQLGenCtx . hcServerCtx)
      instanceId  <- asks (scInstanceId . hcServerCtx)
      env         <- asks (scEnvironment . hcServerCtx)
      remoteSchemaPermsCtx <- asks (scRemoteSchemaPermsCtx . hcServerCtx)
      experimentalFeatures <- asks (scExperimentalFeatures . hcServerCtx)
      functionPermsCtx     <- asks (scFunctionPermsCtx . hcServerCtx)
      maintenanceMode      <- asks (scEnableMaintenanceMode . hcServerCtx)
      let serverConfigCtx = ServerConfigCtx functionPermsCtx remoteSchemaPermsCtx sqlGenCtx maintenanceMode experimentalFeatures
      V2Q.runQuery env instanceId userInfo schemaCache httpMgr serverConfigCtx query

v1Alpha1GQHandler
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , Tracing.MonadTrace m
     , GH.MonadExecuteQuery m
     , MonadError QErr m
     , MonadReader HandlerCtx m
     , HttpLog m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => E.GraphQLQueryType -> GH.GQLBatchedReqs GH.GQLQueryText
  -> m (HTTPLoggingMetadata m, HttpResponse EncJSON)
v1Alpha1GQHandler queryType query = do
  userInfo             <- asks hcUser
  reqHeaders           <- asks hcReqHeaders
  ipAddress            <- asks hcSourceIpAddress
  requestId            <- asks hcRequestId
  manager              <- asks (scManager . hcServerCtx)
  scRef                <- asks (scCacheRef . hcServerCtx)
  (sc, scVer)          <- liftIO $ readIORef $ _scrCache scRef
  sqlGenCtx            <- asks (scSQLGenCtx . hcServerCtx)
  -- planCache            <- asks (scPlanCache . hcServerCtx)
  enableAL             <- asks (scEnableAllowlist . hcServerCtx)
  logger               <- asks (scLogger . hcServerCtx)
  responseErrorsConfig <- asks (scResponseInternalErrorsConfig . hcServerCtx)
  env                  <- asks (scEnvironment . hcServerCtx)

  let execCtx = E.ExecutionCtx logger sqlGenCtx {- planCache -}
                (lastBuiltSchemaCache sc) scVer manager enableAL

  flip runReaderT execCtx $
    GH.runGQBatched env logger requestId responseErrorsConfig userInfo ipAddress reqHeaders queryType query

mkExecutionContext
  :: ( MonadIO m
    , MonadReader HandlerCtx m
    )
  =>  m E.ExecutionCtx
mkExecutionContext = do
  manager              <- asks (scManager . hcServerCtx)
  scRef                <- asks (scCacheRef . hcServerCtx)
  (sc, scVer)          <- liftIO $ readIORef $ _scrCache scRef
  sqlGenCtx            <- asks (scSQLGenCtx . hcServerCtx)
  enableAL             <- asks (scEnableAllowlist . hcServerCtx)
  logger               <- asks (scLogger . hcServerCtx)
  pure $ E.ExecutionCtx logger sqlGenCtx (lastBuiltSchemaCache sc) scVer manager enableAL

v1GQHandler
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , Tracing.MonadTrace m
     , GH.MonadExecuteQuery m
     , HttpLog m
     , MonadError QErr m
     , MonadReader HandlerCtx m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => GH.GQLBatchedReqs GH.GQLQueryText
  -> m (HTTPLoggingMetadata m, HttpResponse EncJSON)
v1GQHandler = v1Alpha1GQHandler E.QueryHasura

v1GQRelayHandler
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , Tracing.MonadTrace m
     , HttpLog m
     , GH.MonadExecuteQuery m
     , MonadError QErr m
     , MonadReader HandlerCtx m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => GH.GQLBatchedReqs GH.GQLQueryText
  -> m (HTTPLoggingMetadata m, HttpResponse EncJSON)
v1GQRelayHandler = v1Alpha1GQHandler E.QueryRelay

gqlExplainHandler
  :: forall m. ( MonadIO m
               , MonadBaseControl IO m
               , MonadError QErr m
               , MonadReader HandlerCtx m
               , MonadMetadataStorage (MetadataStorageT m)
               )
  => GE.GQLExplain
  -> m (HttpResponse EncJSON)
gqlExplainHandler query = do
  onlyAdmin
  scRef     <- asks (scCacheRef . hcServerCtx)
  sc        <- getSCFromRef scRef
--  sqlGenCtx <- asks (scSQLGenCtx . hcServerCtx)
--  env       <- asks (scEnvironment . hcServerCtx)
--  logger    <- asks (scLogger . hcServerCtx)


  -- let runTx :: ReaderT HandlerCtx (Tracing.TraceT (Tracing.NoReporter (LazyTx QErr))) a
  --           -> ExceptT QErr (ReaderT HandlerCtx (Tracing.TraceT m)) a
  -- let runTx rttx = ExceptT . ReaderT $ \ctx -> do
  --       runExceptT (Tracing.interpTraceT (runLazyTx pgExecCtx Q.ReadOnly) (runReaderT rttx ctx))

  res <- GE.explainGQLQuery sc query
  return $ HttpResponse res []

v1Alpha1PGDumpHandler :: (MonadIO m, MonadError QErr m, MonadReader HandlerCtx m) => PGD.PGDumpReqBody -> m APIResp
v1Alpha1PGDumpHandler b = do
  onlyAdmin
  scRef <- asks (scCacheRef . hcServerCtx)
  sc    <- getSCFromRef scRef
  let sources      = scSources sc
      sourceName   = PGD.prbSource b
      sourceConfig = unsafeSourceConfiguration @'Postgres =<< M.lookup sourceName sources
  ci <- fmap _pscConnInfo sourceConfig
        `onNothing` throw400 NotFound ("source " <> sourceName <<> " not found")
  output <- PGD.execPGDump b ci
  return $ RawResp $ HttpResponse output [sqlHeader]

consoleAssetsHandler
  :: (MonadIO m, HttpLog m)
  => L.Logger L.Hasura
  -> Text
  -> FilePath
  -> Spock.ActionT m ()
consoleAssetsHandler logger dir path = do
  req <- Spock.request
  let reqHeaders = Wai.requestHeaders req
  -- '..' in paths need not be handed as it is resolved in the url by
  -- spock's routing. we get the expanded path.
  eFileContents <- liftIO $ try $ BL.readFile $
    joinPath [T.unpack dir, path]
  either (onError reqHeaders) onSuccess eFileContents
  where
    onSuccess c = do
      mapM_ setHeader headers
      Spock.lazyBytes c
    onError :: (MonadIO m, HttpLog m) => [HTTP.Header] -> IOException -> Spock.ActionT m ()
    onError hdrs = raiseGenericApiError logger hdrs . err404 NotFound . tshow
    fn = T.pack $ takeFileName path
    -- set gzip header if the filename ends with .gz
    (fileName, encHeader) = case T.stripSuffix ".gz" fn of
      Just v  -> (v, [gzipHeader])
      Nothing -> (fn, [])
    mimeType = defaultMimeLookup fileName
    headers = ("Content-Type", mimeType) : encHeader

class (Monad m) => ConsoleRenderer m where
  renderConsole :: HasVersion => Text -> AuthMode -> Bool -> Maybe Text -> m (Either String Text)

instance ConsoleRenderer m => ConsoleRenderer (Tracing.TraceT m) where
  renderConsole a b c d = lift $ renderConsole a b c d

renderHtmlTemplate :: M.Template -> Value -> Either String Text
renderHtmlTemplate template jVal =
  bool (Left errMsg) (Right res) $ null errs
  where
    errMsg = "template rendering failed: " ++ show errs
    (errs, res) = M.checkedSubstitute template jVal

newtype LegacyQueryParser m
  = LegacyQueryParser
  { getLegacyQueryParser :: PG.QualifiedTable -> Object -> m RQLQueryV1 }

queryParsers :: (MonadError QErr m) => M.HashMap Text (LegacyQueryParser m)
queryParsers =
  M.fromList
  [ ("select", mkLegacyQueryParser RQSelect)
  , ("insert", mkLegacyQueryParser RQInsert)
  , ("update", mkLegacyQueryParser RQUpdate)
  , ("delete", mkLegacyQueryParser RQDelete)
  , ("count", mkLegacyQueryParser RQCount)
  ]
  where
    mkLegacyQueryParser f =
      LegacyQueryParser $ \qt obj -> do
      let val = Object $ M.insert "table" (toJSON qt) obj
      q <- decodeValue val
      return $ f q

legacyQueryHandler
  :: ( HasVersion, MonadIO m, MonadBaseControl IO m, MonadMetadataApiAuthorization m, Tracing.MonadTrace m
     , MonadReader HandlerCtx m
     , MonadMetadataStorage m
     , MonadResolveSource m
     )
  => PG.TableName -> Text -> Object
  -> m (HttpResponse EncJSON)
legacyQueryHandler tn queryType req =
  case M.lookup queryType queryParsers of
    Just queryParser -> getLegacyQueryParser queryParser qt req >>= v1QueryHandler . RQV1
    Nothing          -> throw404 "No such resource exists"
  where
    qt = PG.QualifiedObject PG.publicSchema tn

-- | Default implementation of the 'MonadConfigApiHandler'
configApiGetHandler
  :: (HasVersion, MonadIO m, MonadBaseControl IO m, UserAuthentication (Tracing.TraceT m), HttpLog m, Tracing.HasReporter m, HasResourceLimits m)
  => ServerCtx -> Maybe Text -> Spock.SpockCtxT () m ()
configApiGetHandler serverCtx@ServerCtx{..} consoleAssetsDir =
  Spock.get "v1alpha1/config" $ mkSpockAction serverCtx encodeQErr id $
    mkGetHandler $ do
      onlyAdmin
      let res = runGetConfig scFunctionPermsCtx scRemoteSchemaPermsCtx scAuthMode scEnableAllowlist
                (EL._lqsOptions $ scLQState) consoleAssetsDir scExperimentalFeatures
      return (mempty, JSONResp $ HttpResponse (encJFromJValue res) [])

data HasuraApp
  = HasuraApp
  { _hapApplication                  :: !Wai.Application
  , _hapSchemaRef                    :: !SchemaCacheRef
  , _hapAsyncActionSubscriptionState :: !EL.AsyncActionSubscriptionState
  , _hapShutdownWsServer             :: !(IO ())
  }

-- TODO: Put Env into ServerCtx?

mkWaiApp
  :: forall m.
     ( HasVersion
     , MonadIO m
--     , MonadUnique m
     , MonadStateless IO m
     , LA.Forall (LA.Pure m)
     , ConsoleRenderer m
     , HttpLog m
     , UserAuthentication (Tracing.TraceT m)
     , MonadMetadataApiAuthorization m
     , E.MonadGQLExecutionCheck m
     , MonadConfigApiHandler m
     , MonadQueryLog m
     , WS.MonadWSLog m
     , Tracing.HasReporter m
     , GH.MonadExecuteQuery m
     , HasResourceLimits m
     , MonadMetadataStorage (MetadataStorageT m)
     , MonadResolveSource m
     )
  => (ServerCtx -> Spock.SpockT m ())
  -> Env.Environment
  -- ^ Set of environment variables for reference in UIs
  -> L.Logger L.Hasura
  -- ^ a 'L.Hasura' specific logger
  -> SQLGenCtx
  -> Bool
  -- ^ is AllowList enabled - TODO: change this boolean to sumtype
  -> HTTP.Manager
  -- ^ HTTP manager so that we can re-use sessions
  -> AuthMode
  -- ^ 'AuthMode' in which the application should operate in
  -> CorsConfig
  -> Bool
  -- ^ is console enabled - TODO: better type
  -> Maybe Text
  -- ^ filepath to the console static assets directory - TODO: better type
  -> Bool
  -- ^ is telemetry enabled
  -> InstanceId
  -- ^ each application, when run, gets an 'InstanceId'. this is used at various places including
  -- schema syncing and telemetry
  -> S.HashSet API
  -- ^ set of the enabled 'API's
  -> EL.LiveQueriesOptions
  -> E.PlanCacheOptions
  -> ResponseInternalErrorsConfig
  -> Maybe EL.LiveQueryPostPollHook
  -> SchemaCacheRef
  -> EKG.Store
  -> RemoteSchemaPermsCtx
  -> FunctionPermissionsCtx
  -> WS.ConnectionOptions
  -> KeepAliveDelay
  -- ^ Metadata storage connection pool
  -> MaintenanceMode
  -> S.HashSet ExperimentalFeature
  -- ^ Set of the enabled experimental features
  -> m HasuraApp
mkWaiApp setupHook env logger sqlGenCtx enableAL httpManager mode corsCfg enableConsole consoleAssetsDir
         enableTelemetry instanceId apis lqOpts _ {- planCacheOptions -} responseErrorsConfig
         liveQueryHook schemaCacheRef ekgStore enableRSPermsCtx functionPermsCtx connectionOptions keepAliveDelay
         maintenanceMode experimentalFeatures = do

    let getSchemaCache = first lastBuiltSchemaCache <$> readIORef (_scrCache schemaCacheRef)

    let corsPolicy = mkDefaultCorsPolicy corsCfg
        postPollHook = fromMaybe (EL.defaultLiveQueryPostPollHook logger) liveQueryHook

    lqState <- liftIO $ EL.initLiveQueriesState lqOpts postPollHook
    wsServerEnv <- WS.createWSServerEnv logger lqState getSchemaCache httpManager
                                        corsPolicy sqlGenCtx enableAL keepAliveDelay {- planCache -}

    let serverCtx = ServerCtx
                    { scLogger                       =  logger
                    , scCacheRef                     =  schemaCacheRef
                    , scAuthMode                     =  mode
                    , scManager                      =  httpManager
                    , scSQLGenCtx                    =  sqlGenCtx
                    , scEnabledAPIs                  =  apis
                    , scInstanceId                   =  instanceId
                    -- , scPlanCache                    =  planCache
                    , scLQState                      =  lqState
                    , scEnableAllowlist              =  enableAL
                    , scEkgStore                     =  ekgStore
                    , scEnvironment                  =  env
                    , scResponseInternalErrorsConfig = responseErrorsConfig
                    , scRemoteSchemaPermsCtx         = enableRSPermsCtx
                    , scFunctionPermsCtx             = functionPermsCtx
                    , scEnableMaintenanceMode        = maintenanceMode
                    , scExperimentalFeatures         = experimentalFeatures
                    }

    spockApp <- liftWithStateless $ \lowerIO ->
      Spock.spockAsApp $ Spock.spockT lowerIO $
        httpApp setupHook corsCfg serverCtx enableConsole consoleAssetsDir enableTelemetry

    let wsServerApp  = WS.createWSServerApp env mode wsServerEnv -- TODO: Lyndon: Can we pass environment through wsServerEnv?
        stopWSServer = WS.stopWSServerApp wsServerEnv

    waiApp <- liftWithStateless $ \lowerIO ->
      pure $ WSC.websocketsOr connectionOptions (\ip conn -> lowerIO $ wsServerApp ip conn) spockApp

    return $ HasuraApp waiApp schemaCacheRef (EL._lqsAsyncActions lqState) stopWSServer

-- initialiseCache :: m (E.PlanCache, SchemaCacheRef)
initialiseCache :: MonadIO m => RebuildableSchemaCache -> m SchemaCacheRef
initialiseCache schemaCache = do
  cacheLock <- liftIO $ newMVar ()
  cacheCell <- liftIO $ newIORef (schemaCache, initSchemaCacheVer)
  -- planCache <- liftIO $ E.initPlanCache planCacheOptions
  let cacheRef = SchemaCacheRef cacheLock cacheCell E.clearPlanCache
  -- pure (planCache, cacheRef)
  pure cacheRef


httpApp
  :: ( HasVersion
     , MonadIO m
--     , MonadUnique m
     , MonadBaseControl IO m
     , ConsoleRenderer m
     , HttpLog m
     -- , UserAuthentication m
     , UserAuthentication (Tracing.TraceT m)
     , MonadMetadataApiAuthorization m
     , E.MonadGQLExecutionCheck m
     , MonadConfigApiHandler m
     , MonadQueryLog m
     , Tracing.HasReporter m
     , GH.MonadExecuteQuery m
     , MonadMetadataStorage (MetadataStorageT m)
     , HasResourceLimits m
     , MonadResolveSource m
     )
  => (ServerCtx -> Spock.SpockT m ())
  ->  CorsConfig
  -> ServerCtx
  -> Bool
  -> Maybe Text
  -> Bool
  -> Spock.SpockT m ()
httpApp setupHook corsCfg serverCtx enableConsole consoleAssetsDir enableTelemetry = do

    -- Additional spock action to run
    setupHook serverCtx

    -- cors middleware
    unless (isCorsDisabled corsCfg) $
      Spock.middleware $ corsMiddleware (mkDefaultCorsPolicy corsCfg)

    -- API Console and Root Dir
    when (enableConsole && enableMetadata) serveApiConsole

    -- Health check endpoint
    Spock.get "healthz" $ do
      sc <- getSCFromRef $ scCacheRef serverCtx
      eitherHealth <- runMetadataStorageT checkMetadataStorageHealth
      let dbOk = either (const False) id eitherHealth
      if dbOk
        then Spock.setStatus HTTP.status200 >> Spock.text (if null (scInconsistentObjs sc)
                                               then "OK"
                                               else "WARN: inconsistent objects in schema")
        else Spock.setStatus HTTP.status500 >> Spock.text "ERROR"

    Spock.get "v1/version" $ do
      setHeader jsonHeader
      Spock.lazyBytes $ encode $ object [ "version" .= currentVersion ]

    let
      customEndpointHandler
          :: forall m
           . ( HasVersion
             , MonadIO m
             , MonadBaseControl IO m
             , E.MonadGQLExecutionCheck m
             , MonadQueryLog m
             , GH.MonadExecuteQuery m
             , MonadMetadataStorage (MetadataStorageT m)
             , HttpLog m
             )
          => RestRequest Spock.SpockMethod
          -> Handler (Tracing.TraceT m) (HTTPLoggingMetadata m, APIResp)
      customEndpointHandler restReq = do
        scRef <- asks (scCacheRef . hcServerCtx)
        endpoints <- scEndpoints <$> getSCFromRef scRef
        execCtx <- mkExecutionContext
        env <- asks (scEnvironment . hcServerCtx)
        requestId <- asks hcRequestId
        userInfo <- asks hcUser
        reqHeaders <- asks hcReqHeaders
        ipAddress <- asks hcSourceIpAddress

        req <- restReq & traverse \case
          Spock.MethodStandard (Spock.HttpMethod m) ->
            pure $ EndpointMethod $ T.decodeUtf8 $ HTTP.renderStdMethod m
          _ -> throw400 BadRequest $ "Nonstandard method not allowed for REST endpoints"
        fmap JSONResp <$> runCustomEndpoint env execCtx requestId userInfo reqHeaders ipAddress req endpoints

    -- See Issue #291 for discussion around restified feature
    Spock.hookRouteAll ("api" <//> "rest" <//> Spock.wildcard) $ \wildcard -> do
      queryParams <- Spock.params
      body        <- Spock.body
      method      <- Spock.reqMethod

      -- This is where we decode the json encoded body args. They
      -- are treated as if they came from query arguments, but allow
      -- us to pass non-scalar values.
      let bodyParams = case J.decodeStrict body of
            Just (J.Object o) -> M.toList o
            _                 -> []

          allParams = fmap Left <$> queryParams <|> fmap Right <$> bodyParams


      spockAction encodeQErr id $ do
        -- TODO: Are we actually able to use mkGetHandler in this situation? POST handler seems to do some work that we might want to avoid.
        mkGetHandler $ customEndpointHandler (RestRequest wildcard method allParams)

    when enableMetadata $ do

      Spock.post "v1/graphql/explain" gqlExplainAction

      Spock.post "v1alpha1/graphql/explain" gqlExplainAction

      Spock.post "v1/query" $ spockAction encodeQErr id $ do
        mkPostHandler $ fmap (mempty, ) <$> mkAPIRespHandler v1QueryHandler

      Spock.post "v1/metadata" $ spockAction encodeQErr id $
        mkPostHandler $ fmap (mempty, ) <$> mkAPIRespHandler v1MetadataHandler

      Spock.post "v2/query" $ spockAction encodeQErr id $
        mkPostHandler $ fmap (mempty, ) <$> mkAPIRespHandler v2QueryHandler

      Spock.post ("api/1/table" <//> Spock.var <//> Spock.var) $ \tableName queryType ->
        mkSpockAction serverCtx encodeQErr id $
          mkPostHandler $
          fmap (mempty, )
          <$> (mkAPIRespHandler $ legacyQueryHandler (PG.TableName tableName) queryType)

    when enablePGDump $
      Spock.post "v1alpha1/pg_dump" $ spockAction encodeQErr id $
        mkPostHandler $ fmap (mempty,) <$> v1Alpha1PGDumpHandler

    when enableConfig $ runConfigApiHandler serverCtx consoleAssetsDir

    when enableGraphQL $ do
      Spock.post "v1alpha1/graphql" $ spockAction GH.encodeGQErr id $
        mkGQLRequestHandler $ mkGQLAPIRespHandler $ v1Alpha1GQHandler E.QueryHasura

      Spock.post "v1/graphql" $ spockAction GH.encodeGQErr allMod200 $
        mkGQLRequestHandler $ mkGQLAPIRespHandler v1GQHandler

      Spock.post "v1beta1/relay" $ spockAction GH.encodeGQErr allMod200 $
        mkGQLRequestHandler $ mkGQLAPIRespHandler $ v1GQRelayHandler

    -- This exposes some simple RTS stats when we run with `+RTS -T`. We want
    -- this to be available even when developer APIs are not compiled in, to
    -- support benchmarking.
    -- See: https://hackage.haskell.org/package/base/docs/GHC-Stats.html
    exposeRtsStats <- liftIO RTS.getRTSStatsEnabled
    when exposeRtsStats $ do
      Spock.get "dev/rts_stats" $ do
        stats <- liftIO RTS.getRTSStats
        Spock.json stats

    when (isDeveloperAPIEnabled serverCtx) $ do
      Spock.get "dev/ekg" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EKG.sampleAll $ scEkgStore serverCtx
          return (mempty, JSONResp $ HttpResponse (encJFromJValue $ EKG.sampleToJson respJ) [])
      Spock.get "dev/plan_cache" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ E.dumpPlanCache {- scPlanCache serverCtx -}
          return (mempty, JSONResp $ HttpResponse (encJFromJValue respJ) [])
      Spock.get "dev/subscriptions" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState False $ scLQState serverCtx
          return (mempty, JSONResp $ HttpResponse (encJFromJValue respJ) [])
      Spock.get "dev/subscriptions/extended" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState True $ scLQState serverCtx
          return (mempty, JSONResp $ HttpResponse (encJFromJValue respJ) [])

    forM_ [Spock.GET, Spock.POST] $ \m -> Spock.hookAny m $ \_ -> do
      req <- Spock.request
      let headers = Wai.requestHeaders req
          qErr = err404 NotFound "resource does not exist"
      raiseGenericApiError logger headers qErr

  where
    logger = scLogger serverCtx

    spockAction
      :: (FromJSON a, ToJSON a, MonadIO m, MonadBaseControl IO m, UserAuthentication (Tracing.TraceT m), HttpLog m, Tracing.HasReporter m, HasResourceLimits m)
      => (Bool -> QErr -> Value)
      -> (QErr -> QErr) -> APIHandler (Tracing.TraceT m) a -> Spock.ActionT m ()
    spockAction = mkSpockAction serverCtx

    -- all graphql errors should be of type 200
    allMod200 qe     = qe { qeStatus = HTTP.status200 }
    gqlExplainAction = do
      spockAction encodeQErr id $
        mkPostHandler $
        fmap (mempty, ) <$> mkAPIRespHandler gqlExplainHandler
    enableGraphQL    = isGraphQLEnabled serverCtx
    enableMetadata   = isMetadataEnabled serverCtx
    enablePGDump     = isPGDumpEnabled serverCtx
    enableConfig     = isConfigEnabled serverCtx

    serveApiConsole = do
      -- redirect / to /console
      Spock.get Spock.root $ Spock.redirect "console"

      -- serve static files if consoleAssetsDir is set
      onJust consoleAssetsDir $ \dir ->
        Spock.get ("console/assets" <//> Spock.wildcard) $ \path -> do
          consoleAssetsHandler logger dir (T.unpack path)

      -- serve console html
      Spock.get ("console" <//> Spock.wildcard) $ \path -> do
        req <- Spock.request
        let headers = Wai.requestHeaders req
            authMode = scAuthMode serverCtx
        consoleHtml <- lift $ renderConsole path authMode enableTelemetry consoleAssetsDir
        either (raiseGenericApiError logger headers . err500 Unexpected . T.pack) Spock.html consoleHtml

raiseGenericApiError
  :: (MonadIO m, HttpLog m)
  => L.Logger L.Hasura
  -> [HTTP.Header]
  -> QErr
  -> Spock.ActionT m ()
raiseGenericApiError logger headers qErr = do
  req <- Spock.request
  reqBody <- liftIO $ Wai.strictRequestBody req
  reqId <- getRequestId $ Wai.requestHeaders req
  lift $ logHttpError logger Nothing reqId req (reqBody, Nothing) qErr headers
  setHeader jsonHeader
  Spock.setStatus $ qeStatus qErr
  Spock.lazyBytes $ encode qErr
