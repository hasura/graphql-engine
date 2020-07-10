{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}

module Hasura.Server.App where

import           Control.Concurrent.MVar.Lifted
import           Control.Exception                         (IOException, try)
import           Control.Monad.Morph                       (hoist)
import           Control.Monad.Trans.Control               (MonadBaseControl)
import           Data.String                               (fromString)

import           Control.Monad.Stateless
import           Data.Aeson                                hiding (json)
import           Data.Int                                  (Int64)
import           Data.IORef
import           Data.Time.Clock                           (UTCTime)
import           Data.Time.Clock.POSIX                     (getPOSIXTime)
import           Network.Mime                              (defaultMimeLookup)
import           System.FilePath                           (joinPath, takeFileName)
import           Web.Spock.Core                            ((<//>))

import qualified Control.Concurrent.Async.Lifted.Safe      as LA
import qualified Control.Monad.Trans.Control               as MTC
import qualified Data.ByteString.Char8                     as B8
import qualified Data.ByteString.Lazy                      as BL
import qualified Data.CaseInsensitive                      as CI
import qualified Data.Environment                          as Env
import qualified Data.HashMap.Strict                       as M
import qualified Data.HashSet                              as S
import qualified Data.Text                                 as T
import qualified Database.PG.Query                         as Q
import qualified Network.HTTP.Client                       as HTTP
import qualified Network.HTTP.Types                        as HTTP
import qualified Network.Wai.Extended                      as Wai
import qualified Network.WebSockets                        as WS
import qualified System.Metrics                            as EKG
import qualified System.Metrics.Json                       as EKG
import qualified Text.Mustache                             as M
import qualified Web.Spock.Core                            as Spock

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Logging                    (MonadQueryLog (..))
import           Hasura.GraphQL.Resolve.Action
import           Hasura.HTTP
import           Hasura.Prelude                            hiding (get, put)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Run
import           Hasura.Server.API.Config                  (runGetConfig)
import           Hasura.Server.API.Query
import           Hasura.Server.Auth                        (AuthMode (..), UserAuthentication (..))
import           Hasura.Server.Compression
import           Hasura.Server.Cors
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Middleware                  (corsMiddleware)
import           Hasura.Server.Utils
import           Hasura.Server.Version
import           Hasura.Session
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Execute                    as E
import qualified Hasura.GraphQL.Execute.LiveQuery          as EL
import qualified Hasura.GraphQL.Explain                    as GE
import qualified Hasura.GraphQL.Transport.HTTP             as GH
import qualified Hasura.GraphQL.Transport.HTTP.Protocol    as GH
import qualified Hasura.GraphQL.Transport.WebSocket        as WS
import qualified Hasura.GraphQL.Transport.WebSocket.Server as WS
import qualified Hasura.Logging                            as L
import qualified Hasura.Server.API.PGDump                  as PGD
import qualified Hasura.Tracing                            as Tracing
import qualified Network.Wai.Handler.WebSockets.Custom     as WSC


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
  , _scrCache    :: IORef (RebuildableSchemaCache Run, SchemaCacheVer)
  , _scrOnChange :: IO ()
  -- ^ an action to run when schemacache changes
  }

data ServerCtx
  = ServerCtx
  { scPGExecCtx                    :: !PGExecCtx
  , scConnInfo                     :: !Q.ConnInfo
  , scLogger                       :: !(L.Logger L.Hasura)
  , scCacheRef                     :: !SchemaCacheRef
  , scAuthMode                     :: !AuthMode
  , scManager                      :: !HTTP.Manager
  , scSQLGenCtx                    :: !SQLGenCtx
  , scEnabledAPIs                  :: !(S.HashSet API)
  , scInstanceId                   :: !InstanceId
  , scPlanCache                    :: !E.PlanCache
  , scLQState                      :: !EL.LiveQueriesState
  , scEnableAllowlist              :: !Bool
  , scEkgStore                     :: !EKG.Store
  , scResponseInternalErrorsConfig :: !ResponseInternalErrorsConfig
  , scEnvironment                  :: !Env.Environment
  }

data HandlerCtx
  = HandlerCtx
  { hcServerCtx       :: !ServerCtx
  , hcUser            :: !UserInfo
  , hcReqHeaders      :: ![HTTP.Header]
  , hcRequestId       :: !RequestId
  , hcSourceIpAddress :: !Wai.IpAddress
  }

type Handler m = ExceptT QErr (ReaderT HandlerCtx m)

data APIResp
  = JSONResp !(HttpResponse EncJSON)
  | RawResp  !(HttpResponse BL.ByteString)

data APIHandler m a
  = AHGet !(Handler m APIResp)
  | AHPost !(a -> Handler m APIResp)


boolToText :: Bool -> T.Text
boolToText = bool "false" "true"

isAdminSecretSet :: AuthMode -> T.Text
isAdminSecretSet AMNoAuth = boolToText False
isAdminSecretSet _        = boolToText True

getSCFromRef :: (MonadIO m) => SchemaCacheRef -> m SchemaCache
getSCFromRef scRef = lastBuiltSchemaCache . fst <$> liftIO (readIORef $ _scrCache scRef)

logInconsObjs :: L.Logger L.Hasura -> [InconsistentMetadata] -> IO ()
logInconsObjs logger objs =
  unless (null objs) $ L.unLogger logger $ mkInconsMetadataLog objs

withSCUpdate
  :: (MonadIO m, MonadBaseControl IO m)
  => SchemaCacheRef -> L.Logger L.Hasura -> m (a, RebuildableSchemaCache Run) -> m a
withSCUpdate scr logger action = do
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

mkGetHandler :: Handler m APIResp -> APIHandler m ()
mkGetHandler = AHGet

mkPostHandler :: (a -> Handler m APIResp) -> APIHandler m a
mkPostHandler = AHPost

mkAPIRespHandler :: (Functor m) => (a -> Handler m (HttpResponse EncJSON)) -> (a -> Handler m APIResp)
mkAPIRespHandler = (fmap . fmap) JSONResp

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

onlyAdmin :: (Monad m) => Handler m ()
onlyAdmin = do
  uRole <- asks (_uiRole . hcUser)
  when (uRole /= adminRoleName) $
    throw400 AccessDenied "You have to be an admin to access this endpoint"

buildQCtx :: (MonadIO m) => Handler m QCtx
buildQCtx = do
  scRef     <- asks (scCacheRef . hcServerCtx)
  userInfo  <- asks hcUser
  cache     <- getSCFromRef scRef
  sqlGenCtx <- asks (scSQLGenCtx . hcServerCtx)
  return $ QCtx userInfo cache sqlGenCtx

setHeader :: MonadIO m => HTTP.Header -> Spock.ActionT m ()
setHeader (headerName, headerValue) =
  Spock.setHeader (bsToTxt $ CI.original headerName) (bsToTxt headerValue)

-- | Typeclass representing the metadata API authorization effect
class Monad m => MetadataApiAuthorization m where
  authorizeMetadataApi :: HasVersion => RQLQuery -> UserInfo -> Handler m ()

instance MetadataApiAuthorization m => MetadataApiAuthorization (Tracing.TraceT m) where
  authorizeMetadataApi q ui = hoist (hoist lift) $ authorizeMetadataApi q ui

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
mapActionT f tma = MTC.restoreT . pure =<< (MTC.liftWith $ \run -> f (run tma))


mkSpockAction
  :: (HasVersion, MonadIO m, FromJSON a, ToJSON a, UserAuthentication (Tracing.TraceT m), HttpLog m, Tracing.HasReporter m)
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
          Tracing.runTraceTWith
          tracingCtx
          (fromString (B8.unpack pathInfo))

    requestId <- getRequestId headers

    mapActionT runTraceT $ do
      userInfoE <- fmap fst <$> lift (resolveUserInfo logger manager headers authMode)
      userInfo  <- either (logErrorAndResp Nothing requestId req (Left reqBody) False headers . qErrModifier)
                  return userInfoE

      let handlerState = HandlerCtx serverCtx userInfo headers requestId ipAddress
          includeInternal = shouldIncludeInternal (_uiRole userInfo) $
                            scResponseInternalErrorsConfig serverCtx

      (serviceTime, (result, q)) <- withElapsedTime $ case apiHandler of
        AHGet handler -> do
          res <- lift $ runReaderT (runExceptT handler) handlerState
          return (res, Nothing)
        AHPost handler -> do
          parsedReqE <- runExceptT $ parseBody reqBody
          parsedReq  <- either (logErrorAndResp (Just userInfo) requestId req (Left reqBody) includeInternal headers . qErrModifier)
                        return parsedReqE
          res <- lift $ runReaderT (runExceptT $ handler parsedReq) handlerState
          return (res, Just parsedReq)

      -- apply the error modifier
      let modResult = fmapL qErrModifier result

      -- log and return result
      case modResult of
        Left err  -> let jErr = maybe (Left reqBody) (Right . toJSON) q
                    in logErrorAndResp (Just userInfo) requestId req jErr includeInternal headers err
        Right res -> logSuccessAndResp (Just userInfo) requestId req (fmap toJSON q) res (Just (ioWaitTime, serviceTime)) headers

    where
      logger = scLogger serverCtx

      logErrorAndResp
        :: (MonadIO m, HttpLog m)
        => Maybe UserInfo
        -> RequestId
        -> Wai.Request
        -> Either BL.ByteString Value
        -> Bool
        -> [HTTP.Header]
        -> QErr
        -> Spock.ActionCtxT ctx m a
      logErrorAndResp userInfo reqId req reqBody includeInternal headers qErr = do
        lift $ logHttpError logger userInfo reqId req reqBody qErr headers
        Spock.setStatus $ qeStatus qErr
        Spock.json $ qErrEncoder includeInternal qErr

      logSuccessAndResp userInfo reqId req reqBody result qTime reqHeaders =
        case result of
          JSONResp (HttpResponse encJson h) ->
            possiblyCompressedLazyBytes userInfo reqId req reqBody qTime (encJToLBS encJson)
              (pure jsonHeader <> h) reqHeaders
          RawResp (HttpResponse rawBytes h) ->
            possiblyCompressedLazyBytes userInfo reqId req reqBody qTime rawBytes h reqHeaders

      possiblyCompressedLazyBytes userInfo reqId req reqBody qTime respBytes respHeaders reqHeaders = do
        let (compressedResp, mEncodingHeader, mCompressionType) =
              compressResponse (Wai.requestHeaders req) respBytes
            encodingHeader = maybe [] pure mEncodingHeader
            reqIdHeader = (requestIdHeader, txtToBs $ unRequestId reqId)
            allRespHeaders = pure reqIdHeader <> encodingHeader <> respHeaders
        lift $ logHttpSuccess logger userInfo reqId req reqBody respBytes compressedResp qTime mCompressionType reqHeaders
        mapM_ setHeader allRespHeaders
        Spock.lazyBytes compressedResp


v1QueryHandler
  :: (HasVersion, MonadIO m, MonadBaseControl IO m, MetadataApiAuthorization m)
  => RQLQuery
  -> Handler m (HttpResponse EncJSON)
v1QueryHandler query = do
  userInfo <- asks hcUser
  authorizeMetadataApi query userInfo
  scRef  <- asks (scCacheRef . hcServerCtx)
  logger <- asks (scLogger . hcServerCtx)
  res    <- bool (fst <$> dbAction) (withSCUpdate scRef logger dbAction) $ queryModifiesSchemaCache query
  return $ HttpResponse res []
  where
    -- Hit postgres
    dbAction = do
      userInfo    <- asks hcUser
      scRef       <- asks (scCacheRef . hcServerCtx)
      schemaCache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
      httpMgr     <- asks (scManager . hcServerCtx)
      sqlGenCtx   <- asks (scSQLGenCtx . hcServerCtx)
      pgExecCtx   <- asks (scPGExecCtx . hcServerCtx)
      instanceId  <- asks (scInstanceId . hcServerCtx)
      env         <- asks (scEnvironment . hcServerCtx)
      runQuery env pgExecCtx instanceId userInfo schemaCache httpMgr sqlGenCtx (SystemDefined False) query

v1Alpha1GQHandler
  :: (HasVersion, MonadIO m, E.MonadGQLExecutionCheck m, MonadQueryLog m, Tracing.MonadTrace m, GH.MonadExecuteQuery m)
  => E.GraphQLQueryType -> GH.GQLBatchedReqs GH.GQLQueryText -> Handler m (HttpResponse EncJSON)
v1Alpha1GQHandler queryType query = do
  userInfo             <- asks hcUser
  reqHeaders           <- asks hcReqHeaders
  ipAddress            <- asks hcSourceIpAddress
  requestId            <- asks hcRequestId
  manager              <- asks (scManager . hcServerCtx)
  scRef                <- asks (scCacheRef . hcServerCtx)
  (sc, scVer)          <- liftIO $ readIORef $ _scrCache scRef
  pgExecCtx            <- asks (scPGExecCtx . hcServerCtx)
  sqlGenCtx            <- asks (scSQLGenCtx . hcServerCtx)
  planCache            <- asks (scPlanCache . hcServerCtx)
  enableAL             <- asks (scEnableAllowlist . hcServerCtx)
  logger               <- asks (scLogger . hcServerCtx)
  responseErrorsConfig <- asks (scResponseInternalErrorsConfig . hcServerCtx)
  env                  <- asks (scEnvironment . hcServerCtx)

  let execCtx = E.ExecutionCtx logger sqlGenCtx pgExecCtx planCache
                (lastBuiltSchemaCache sc) scVer manager enableAL

  flip runReaderT execCtx $
    GH.runGQBatched env requestId responseErrorsConfig userInfo ipAddress reqHeaders queryType query

v1GQHandler
  :: (HasVersion, MonadIO m, E.MonadGQLExecutionCheck m, MonadQueryLog m, Tracing.MonadTrace m, GH.MonadExecuteQuery m)
  => GH.GQLBatchedReqs GH.GQLQueryText
  -> Handler m (HttpResponse EncJSON)
v1GQHandler = v1Alpha1GQHandler E.QueryHasura

v1GQRelayHandler
  :: (HasVersion, MonadIO m, E.MonadGQLExecutionCheck m, MonadQueryLog m, Tracing.MonadTrace m, GH.MonadExecuteQuery m)
  => GH.GQLBatchedReqs GH.GQLQueryText
  -> Handler m (HttpResponse EncJSON)
v1GQRelayHandler = v1Alpha1GQHandler E.QueryRelay

gqlExplainHandler
  :: forall m
   . ( HasVersion
     , MonadIO m
     , Tracing.HasReporter m
     )
  => GE.GQLExplain
  -> Handler (Tracing.TraceT m) (HttpResponse EncJSON)
gqlExplainHandler query = do
  onlyAdmin
  scRef     <- asks (scCacheRef . hcServerCtx)
  sc        <- getSCFromRef scRef
  pgExecCtx <- asks (scPGExecCtx . hcServerCtx)
  sqlGenCtx <- asks (scSQLGenCtx . hcServerCtx)
  env       <- asks (scEnvironment . hcServerCtx)

  -- let runTx :: ReaderT HandlerCtx (Tracing.TraceT (Tracing.NoReporter (LazyTx QErr))) a
  --           -> ExceptT QErr (ReaderT HandlerCtx (Tracing.TraceT m)) a
  let runTx rttx = ExceptT . ReaderT $ \ctx -> do
        runExceptT (Tracing.interpTraceT (runLazyTx pgExecCtx Q.ReadOnly . Tracing.runNoReporter) (runReaderT rttx ctx))

  res <- GE.explainGQLQuery env pgExecCtx runTx sc sqlGenCtx
         (restrictActionExecuter "query actions cannot be explained") query
  return $ HttpResponse res []

v1Alpha1PGDumpHandler :: (MonadIO m) => PGD.PGDumpReqBody -> Handler m APIResp
v1Alpha1PGDumpHandler b = do
  onlyAdmin
  ci     <- asks (scConnInfo . hcServerCtx)
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
    onError hdrs = raiseGenericApiError logger hdrs . err404 NotFound . T.pack . show
    fn = T.pack $ takeFileName path
    -- set gzip header if the filename ends with .gz
    (fileName, encHeader) = case T.stripSuffix ".gz" fn of
      Just v  -> (v, [gzipHeader])
      Nothing -> (fn, [])
    mimeType = defaultMimeLookup fileName
    headers = ("Content-Type", mimeType) : encHeader

class (Monad m) => ConsoleRenderer m where
  renderConsole :: HasVersion => T.Text -> AuthMode -> Bool -> Maybe Text -> m (Either String Text)

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
  { getLegacyQueryParser :: QualifiedTable -> Object -> Handler m RQLQueryV1 }

queryParsers :: (Monad m) => M.HashMap T.Text (LegacyQueryParser m)
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
  :: (HasVersion, MonadIO m, MonadBaseControl IO m, MetadataApiAuthorization m)
  => TableName -> T.Text -> Object
  -> Handler m (HttpResponse EncJSON)
legacyQueryHandler tn queryType req =
  case M.lookup queryType queryParsers of
    Just queryParser -> getLegacyQueryParser queryParser qt req >>= v1QueryHandler . RQV1
    Nothing          -> throw404 "No such resource exists"
  where
    qt = QualifiedObject publicSchema tn

-- | Default implementation of the 'MonadConfigApiHandler'
configApiGetHandler
  :: (HasVersion, MonadIO m, UserAuthentication (Tracing.TraceT m), HttpLog m, Tracing.HasReporter m)
  => ServerCtx -> Maybe Text -> Spock.SpockCtxT () m ()
configApiGetHandler serverCtx@ServerCtx{..} consoleAssetsDir =
  Spock.get "v1alpha1/config" $ mkSpockAction serverCtx encodeQErr id $
    mkGetHandler $ do
      onlyAdmin
      let res = runGetConfig scAuthMode scEnableAllowlist
                (EL._lqsOptions $ scLQState) consoleAssetsDir
      return $ JSONResp $ HttpResponse (encJFromJValue res) []

data HasuraApp
  = HasuraApp
  { _hapApplication      :: !Wai.Application
  , _hapSchemaRef        :: !SchemaCacheRef
  , _hapCacheBuildTime   :: !(Maybe UTCTime)
  , _hapShutdownWsServer :: !(IO ())
  }

-- TODO: Put Env into ServerCtx?

mkWaiApp
  :: forall m.
     ( HasVersion
     , MonadIO m
     , MonadStateless IO m
     , LA.Forall (LA.Pure m)
     , ConsoleRenderer m
     , HttpLog m
     -- , UserAuthentication m
     , UserAuthentication (Tracing.TraceT m)
     , MetadataApiAuthorization m
     , E.MonadGQLExecutionCheck m
     , MonadConfigApiHandler m
     , MonadQueryLog m
     , WS.MonadWSLog m
     , Tracing.HasReporter m
     , GH.MonadExecuteQuery m
     )
  => Env.Environment
  -- ^ Set of environment variables for reference in UIs
  -> Q.TxIsolation
  -- ^ postgres transaction isolation to be used in the entire app
  -> L.Logger L.Hasura
  -- ^ a 'L.Hasura' specific logger
  -> SQLGenCtx
  -> Bool
  -- ^ is AllowList enabled - TODO: change this boolean to sumtype
  -> Q.PGPool
  -> Maybe PGExecCtx
  -> Q.ConnInfo
  -- ^ postgres connection parameters
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
  -> (RebuildableSchemaCache Run, Maybe UTCTime)
  -> m HasuraApp
mkWaiApp env isoLevel logger sqlGenCtx enableAL pool pgExecCtxCustom ci httpManager mode corsCfg enableConsole consoleAssetsDir
         enableTelemetry instanceId apis lqOpts planCacheOptions responseErrorsConfig (schemaCache, cacheBuiltTime) = do

    (planCache, schemaCacheRef) <- initialiseCache
    let getSchemaCache = first lastBuiltSchemaCache <$> readIORef (_scrCache schemaCacheRef)

    let corsPolicy = mkDefaultCorsPolicy corsCfg
        pgExecCtx = fromMaybe (mkPGExecCtx isoLevel pool) pgExecCtxCustom

    lqState <- liftIO $ EL.initLiveQueriesState lqOpts pgExecCtx
    wsServerEnv <- WS.createWSServerEnv logger pgExecCtx lqState getSchemaCache httpManager
                                        corsPolicy sqlGenCtx enableAL planCache

    ekgStore <- liftIO EKG.newStore

    let serverCtx = ServerCtx
                    { scPGExecCtx       =  pgExecCtx
                    , scConnInfo        =  ci
                    , scLogger          =  logger
                    , scCacheRef        =  schemaCacheRef
                    , scAuthMode        =  mode
                    , scManager         =  httpManager
                    , scSQLGenCtx       =  sqlGenCtx
                    , scEnabledAPIs     =  apis
                    , scInstanceId      =  instanceId
                    , scPlanCache       =  planCache
                    , scLQState         =  lqState
                    , scEnableAllowlist =  enableAL
                    , scEkgStore        =  ekgStore
                    , scEnvironment     =  env
                    , scResponseInternalErrorsConfig = responseErrorsConfig
                    }

    when (isDeveloperAPIEnabled serverCtx) $ do
      liftIO $ EKG.registerGcMetrics ekgStore
      liftIO $ EKG.registerCounter "ekg.server_timestamp_ms" getTimeMs ekgStore

    spockApp <- liftWithStateless $ \lowerIO ->
      Spock.spockAsApp $ Spock.spockT lowerIO $
        httpApp corsCfg serverCtx enableConsole consoleAssetsDir enableTelemetry

    let wsServerApp  = WS.createWSServerApp env mode wsServerEnv -- TODO: Lyndon: Can we pass environment through wsServerEnv?
        stopWSServer = WS.stopWSServerApp wsServerEnv

    waiApp <- liftWithStateless $ \lowerIO ->
      pure $ WSC.websocketsOr WS.defaultConnectionOptions (\ip conn -> lowerIO $ wsServerApp ip conn) spockApp

    return $ HasuraApp waiApp schemaCacheRef cacheBuiltTime stopWSServer
  where
    getTimeMs :: IO Int64
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

    initialiseCache :: m (E.PlanCache, SchemaCacheRef)
    initialiseCache = do
      cacheLock <- liftIO $ newMVar ()
      cacheCell <- liftIO $ newIORef (schemaCache, initSchemaCacheVer)
      planCache <- liftIO $ E.initPlanCache planCacheOptions
      let cacheRef = SchemaCacheRef cacheLock cacheCell (E.clearPlanCache planCache)
      pure (planCache, cacheRef)


httpApp
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , ConsoleRenderer m
     , HttpLog m
     -- , UserAuthentication m
     , UserAuthentication (Tracing.TraceT m)
     , MetadataApiAuthorization m
     , E.MonadGQLExecutionCheck m
     , MonadConfigApiHandler m
     , MonadQueryLog m
     , Tracing.HasReporter m
     , GH.MonadExecuteQuery m
     )
  => CorsConfig
  -> ServerCtx
  -> Bool
  -> Maybe Text
  -> Bool
  -> Spock.SpockT m ()
httpApp corsCfg serverCtx enableConsole consoleAssetsDir enableTelemetry = do

    -- cors middleware
    unless (isCorsDisabled corsCfg) $
      Spock.middleware $ corsMiddleware (mkDefaultCorsPolicy corsCfg)

    -- API Console and Root Dir
    when (enableConsole && enableMetadata) serveApiConsole

    -- Health check endpoint
    Spock.get "healthz" $ do
      sc <- getSCFromRef $ scCacheRef serverCtx
      dbOk <- liftIO $ _pecCheckHealth $ scPGExecCtx serverCtx
      if dbOk
        then Spock.setStatus HTTP.status200 >> (Spock.text $ if null (scInconsistentObjs sc)
                                                 then "OK"
                                                 else "WARN: inconsistent objects in schema")
        else Spock.setStatus HTTP.status500 >> Spock.text "ERROR"

    Spock.get "v1/version" $ do
      setHeader jsonHeader
      Spock.lazyBytes $ encode $ object [ "version" .= currentVersion ]

    when enableMetadata $ do

      Spock.post "v1/graphql/explain" gqlExplainAction

      Spock.post "v1alpha1/graphql/explain" gqlExplainAction

      Spock.post "v1/query" $ spockAction encodeQErr id $
        mkPostHandler $ mkAPIRespHandler v1QueryHandler

      Spock.post ("api/1/table" <//> Spock.var <//> Spock.var) $ \tableName queryType ->
        mkSpockAction serverCtx encodeQErr id $ mkPostHandler $
          mkAPIRespHandler $ legacyQueryHandler (TableName tableName) queryType

    when enablePGDump $
      Spock.post "v1alpha1/pg_dump" $ spockAction encodeQErr id $
        mkPostHandler v1Alpha1PGDumpHandler

    when enableConfig $ runConfigApiHandler serverCtx consoleAssetsDir

    when enableGraphQL $ do
      Spock.post "v1alpha1/graphql" $ spockAction GH.encodeGQErr id $
        mkPostHandler $ mkAPIRespHandler $ v1Alpha1GQHandler E.QueryHasura

      Spock.post "v1/graphql" $ spockAction GH.encodeGQErr allMod200 $
        mkPostHandler $ mkAPIRespHandler v1GQHandler

      Spock.post "v1beta1/relay" $ spockAction GH.encodeGQErr allMod200 $
        mkPostHandler $ mkAPIRespHandler $ v1GQRelayHandler

    when (isDeveloperAPIEnabled serverCtx) $ do
      Spock.get "dev/ekg" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EKG.sampleAll $ scEkgStore serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue $ EKG.sampleToJson respJ) []
      Spock.get "dev/plan_cache" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ E.dumpPlanCache $ scPlanCache serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue respJ) []
      Spock.get "dev/subscriptions" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState False $ scLQState serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue respJ) []
      Spock.get "dev/subscriptions/extended" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState True $ scLQState serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue respJ) []

    forM_ [Spock.GET, Spock.POST] $ \m -> Spock.hookAny m $ \_ -> do
      req <- Spock.request
      let headers = Wai.requestHeaders req
      let qErr = err404 NotFound "resource does not exist"
      raiseGenericApiError logger headers qErr

  where
    logger = scLogger serverCtx

    spockAction
      :: (FromJSON a, ToJSON a, MonadIO m, UserAuthentication (Tracing.TraceT m), HttpLog m, Tracing.HasReporter m)
      => (Bool -> QErr -> Value)
      -> (QErr -> QErr) -> APIHandler (Tracing.TraceT m) a -> Spock.ActionT m ()
    spockAction = mkSpockAction serverCtx


    -- all graphql errors should be of type 200
    allMod200 qe     = qe { qeStatus = HTTP.status200 }
    gqlExplainAction = spockAction encodeQErr id $ mkPostHandler $ mkAPIRespHandler gqlExplainHandler
    enableGraphQL    = isGraphQLEnabled serverCtx
    enableMetadata   = isMetadataEnabled serverCtx
    enablePGDump     = isPGDumpEnabled serverCtx
    enableConfig     = isConfigEnabled serverCtx

    serveApiConsole = do
      -- redirect / to /console
      Spock.get Spock.root $ Spock.redirect "console"

      -- serve static files if consoleAssetsDir is set
      onJust consoleAssetsDir $ \dir ->
        Spock.get ("console/assets" <//> Spock.wildcard) $ \path ->
          consoleAssetsHandler logger dir (T.unpack path)

      -- serve console html
      Spock.get ("console" <//> Spock.wildcard) $ \path -> do
        req <- Spock.request
        let headers = Wai.requestHeaders req
        let authMode = scAuthMode serverCtx
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
  lift $ logHttpError logger Nothing reqId req (Left reqBody) qErr headers
  setHeader jsonHeader
  Spock.setStatus $ qeStatus qErr
  Spock.lazyBytes $ encode qErr
