{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}

module Hasura.Server.App where

import           Control.Concurrent.MVar
import           Control.Exception                      (IOException, try)
import           Control.Monad.Stateless
import           Data.Aeson                             hiding (json)
import           Data.Either                            (isRight)
import           Data.Int                               (Int64)
import           Data.IORef
import           Data.Time.Clock                        (UTCTime)
import           Data.Time.Clock.POSIX                  (getPOSIXTime)
import           Network.Mime                           (defaultMimeLookup)
import           System.Exit                            (exitFailure)
import           System.FilePath                        (joinPath, takeFileName)
import           Web.Spock.Core                         ((<//>))

import qualified Control.Concurrent.Async.Lifted.Safe   as LA
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.HashMap.Strict                    as M
import qualified Data.HashSet                           as S
import qualified Data.Text                              as T
import qualified Data.Time.Clock                        as Clock
import qualified Database.PG.Query                      as Q
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai                            as Wai
import qualified Network.Wai.Handler.WebSockets         as WS
import qualified Network.WebSockets                     as WS
import qualified System.Metrics                         as EKG
import qualified System.Metrics.Json                    as EKG
import qualified Text.Mustache                          as M
import qualified Web.Spock.Core                         as Spock

import           Hasura.EncJSON
import           Hasura.Prelude                         hiding (get, put)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.Auth                     (AuthMode (..), UserAuthentication (..))
import           Hasura.Server.Compression
import           Hasura.Server.Config                   (runGetConfig)
import           Hasura.Server.Context
import           Hasura.Server.Cors
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Middleware               (corsMiddleware)
import           Hasura.Server.Query
import           Hasura.Server.Utils
import           Hasura.Server.Version
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Explain                 as GE
import qualified Hasura.GraphQL.Transport.HTTP          as GH
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Transport.WebSocket     as WS
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.PGDump                   as PGD


data SchemaCacheRef
  = SchemaCacheRef
  { _scrLock     :: MVar ()
  , _scrCache    :: IORef (SchemaCache, SchemaCacheVer)
  -- an action to run when schemacache changes
  , _scrOnChange :: IO ()
  }

data ServerCtx
  = ServerCtx
  { scPGExecCtx       :: !PGExecCtx
  , scConnInfo        :: !Q.ConnInfo
  , scLogger          :: !(L.Logger L.Hasura)
  , scCacheRef        :: !SchemaCacheRef
  , scAuthMode        :: !AuthMode
  , scManager         :: !HTTP.Manager
  , scSQLGenCtx       :: !SQLGenCtx
  , scEnabledAPIs     :: !(S.HashSet API)
  , scInstanceId      :: !InstanceId
  , scPlanCache       :: !E.PlanCache
  , scLQState         :: !EL.LiveQueriesState
  , scEnableAllowlist :: !Bool
  , scEkgStore        :: !EKG.Store
  }

data HandlerCtx
  = HandlerCtx
  { hcServerCtx  :: !ServerCtx
  , hcUser       :: !UserInfo
  , hcReqHeaders :: ![HTTP.Header]
  , hcRequestId  :: !RequestId
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
getSCFromRef scRef = liftIO (readIORef (_scrCache scRef)) >>= return . fst

logInconsObjs :: (MonadIO m) => L.Logger L.Hasura -> [InconsistentMetadataObj] -> m ()
logInconsObjs logger objs =
  unless (null objs) $ L.unLogger logger $ mkInconsMetadataLog objs

withSCUpdate
  :: (MonadIO m, MonadError e m)
  => SchemaCacheRef -> L.Logger L.Hasura -> m (a, SchemaCache) -> m a
withSCUpdate scr logger action = do
  acquireLock
  (res, newSC) <- action `catchError` onError
  liftIO $ do
    -- update schemacache in IO reference
    modifyIORef' cacheRef $
      \(_, prevVer) -> (newSC, incSchemaCacheVer prevVer)
    -- log any inconsistent objects
    logInconsObjs logger $ scInconsistentObjs newSC
    onChange
  releaseLock
  return res
  where
    SchemaCacheRef lk cacheRef onChange = scr
    onError e   = releaseLock >> throwError e
    acquireLock = liftIO $ takeMVar lk
    releaseLock = liftIO $ putMVar lk ()

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
  uRole <- asks (userRole . hcUser)
  when (uRole /= adminRole) $
    throw400 AccessDenied "You have to be an admin to access this endpoint"

buildQCtx :: (MonadIO m) => Handler m QCtx
buildQCtx = do
  scRef    <- scCacheRef . hcServerCtx <$> ask
  userInfo <- asks hcUser
  cache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
  sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
  return $ QCtx userInfo cache sqlGenCtx

-- | Typeclass representing the metadata API authorization effect
class MetadataApiAuthorization m where
  authorizeMetadataApi :: RQLQuery -> UserInfo -> Handler m ()

mkSpockAction
  :: (MonadIO m, FromJSON a, ToJSON a, UserAuthentication m, HttpLog m)
  => ServerCtx
  -> (Bool -> QErr -> Value)
  -- ^ `QErr` JSON encoder function
  -> (QErr -> QErr)
  -- ^ `QErr` modifier
  -> APIHandler m a
  -> Spock.ActionT m ()
mkSpockAction serverCtx qErrEncoder qErrModifier apiHandler = do
    req <- Spock.request
    reqBody <- liftIO $ Wai.strictRequestBody req
    let headers = Wai.requestHeaders req
        authMode = scAuthMode serverCtx
        manager = scManager serverCtx

    requestId <- getRequestId headers

    userInfoE <- fmap fst <$> lift (resolveUserInfo logger manager headers authMode)
    userInfo  <- either (logErrorAndResp Nothing requestId req (Left reqBody) False headers . qErrModifier)
                 return userInfoE

    let handlerState = HandlerCtx serverCtx userInfo headers requestId
        curRole = userRole userInfo

    t1 <- liftIO Clock.getCurrentTime -- for measuring response time purposes

    (result, q) <- case apiHandler of
      AHGet handler -> do
        res <- lift $ runReaderT (runExceptT handler) handlerState
        return (res, Nothing)
      AHPost handler -> do
        parsedReqE <- runExceptT $ parseBody reqBody
        parsedReq  <- either (logErrorAndResp (Just userInfo) requestId req (Left reqBody) (isAdmin curRole) headers . qErrModifier)
                      return parsedReqE
        res <- lift $ runReaderT (runExceptT $ handler parsedReq) handlerState
        return (res, Just parsedReq)

    t2 <- liftIO Clock.getCurrentTime -- for measuring response time purposes

    -- apply the error modifier
    let modResult = fmapL qErrModifier result

    -- log and return result
    case modResult of
      Left err  -> let jErr = maybe (Left reqBody) (Right . toJSON) q
                   in logErrorAndResp (Just userInfo) requestId req jErr (isAdmin curRole) headers err
      Right res -> logSuccessAndResp (Just userInfo) requestId req (fmap toJSON q) res (Just (t1, t2)) headers

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
              (pure jsonHeader <> mkHeaders h) reqHeaders
          RawResp (HttpResponse rawBytes h) ->
            possiblyCompressedLazyBytes userInfo reqId req reqBody qTime rawBytes (mkHeaders h) reqHeaders

      possiblyCompressedLazyBytes userInfo reqId req reqBody qTime respBytes respHeaders reqHeaders = do
        let (compressedResp, mEncodingHeader, mCompressionType) =
              compressResponse (Wai.requestHeaders req) respBytes
            encodingHeader = maybe [] pure mEncodingHeader
            reqIdHeader = (requestIdHeader, unRequestId reqId)
            allRespHeaders = pure reqIdHeader <> encodingHeader <> respHeaders
        lift $ logHttpSuccess logger userInfo reqId req reqBody respBytes compressedResp qTime mCompressionType reqHeaders
        mapM_ (uncurry Spock.setHeader) allRespHeaders
        Spock.lazyBytes compressedResp

      mkHeaders = maybe [] (map unHeader)

v1QueryHandler :: (MonadIO m, MetadataApiAuthorization m) => RQLQuery -> Handler m (HttpResponse EncJSON)
v1QueryHandler query = do
  userInfo <- asks hcUser
  authorizeMetadataApi query userInfo
  scRef <- scCacheRef . hcServerCtx <$> ask
  logger <- scLogger . hcServerCtx <$> ask
  res <- bool (fst <$> dbAction) (withSCUpdate scRef logger dbAction) $
         queryNeedsReload query
  return $ HttpResponse res Nothing
  where
    -- Hit postgres
    dbAction = do
      userInfo <- asks hcUser
      scRef <- scCacheRef . hcServerCtx <$> ask
      schemaCache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
      httpMgr <- scManager . hcServerCtx <$> ask
      sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
      pgExecCtx <- scPGExecCtx . hcServerCtx <$> ask
      instanceId <- scInstanceId . hcServerCtx <$> ask
      runQuery pgExecCtx instanceId userInfo schemaCache httpMgr sqlGenCtx (SystemDefined False) query

v1Alpha1GQHandler :: (MonadIO m) => GH.GQLBatchedReqs GH.GQLQueryText -> Handler m (HttpResponse EncJSON)
v1Alpha1GQHandler query = do
  userInfo <- asks hcUser
  reqHeaders <- asks hcReqHeaders
  manager <- scManager . hcServerCtx <$> ask
  scRef <- scCacheRef . hcServerCtx <$> ask
  (sc, scVer) <- liftIO $ readIORef $ _scrCache scRef
  pgExecCtx <- scPGExecCtx . hcServerCtx <$> ask
  sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
  planCache <- scPlanCache . hcServerCtx <$> ask
  enableAL  <- scEnableAllowlist . hcServerCtx <$> ask
  logger    <- scLogger . hcServerCtx <$> ask
  requestId <- asks hcRequestId
  let execCtx = E.ExecutionCtx logger sqlGenCtx pgExecCtx planCache
                sc scVer manager enableAL
  flip runReaderT execCtx $ GH.runGQBatched requestId userInfo reqHeaders query

v1GQHandler
  :: (MonadIO m)
  => GH.GQLBatchedReqs GH.GQLQueryText
  -> Handler m (HttpResponse EncJSON)
v1GQHandler = v1Alpha1GQHandler

gqlExplainHandler :: (MonadIO m) => GE.GQLExplain -> Handler m (HttpResponse EncJSON)
gqlExplainHandler query = do
  onlyAdmin
  scRef <- scCacheRef . hcServerCtx <$> ask
  sc <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
  pgExecCtx <- scPGExecCtx . hcServerCtx <$> ask
  sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
  enableAL <- scEnableAllowlist . hcServerCtx <$> ask
  res <- GE.explainGQLQuery pgExecCtx sc sqlGenCtx enableAL query
  return $ HttpResponse res Nothing

v1Alpha1PGDumpHandler :: (MonadIO m) => PGD.PGDumpReqBody -> Handler m APIResp
v1Alpha1PGDumpHandler b = do
  onlyAdmin
  ci <- scConnInfo . hcServerCtx <$> ask
  output <- PGD.execPGDump b ci
  return $ RawResp $ HttpResponse output (Just [Header sqlHeader])

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
      mapM_ (uncurry Spock.setHeader) headers
      Spock.lazyBytes c
    onError :: (MonadIO m, HttpLog m) => [HTTP.Header] -> IOException -> Spock.ActionT m ()
    onError hdrs = raiseGenericApiError logger hdrs . err404 NotFound . T.pack . show
    fn = T.pack $ takeFileName path
    -- set gzip header if the filename ends with .gz
    (fileName, encHeader) = case T.stripSuffix ".gz" fn of
      Just v  -> (v, [gzipHeader])
      Nothing -> (fn, [])
    mimeType = bsToTxt $ defaultMimeLookup fileName
    headers = ("Content-Type", mimeType) : encHeader

class (Monad m) => ConsoleRenderer m where
  renderConsole :: T.Text -> AuthMode -> Bool -> Maybe Text -> m (Either String Text)

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
  :: (MonadIO m, MetadataApiAuthorization m)
  => TableName -> T.Text -> Object
  -> Handler m (HttpResponse EncJSON)
legacyQueryHandler tn queryType req =
  case M.lookup queryType queryParsers of
    Just queryParser -> getLegacyQueryParser queryParser qt req >>= v1QueryHandler . RQV1
    Nothing          -> throw404 "No such resource exists"
  where
    qt = QualifiedObject publicSchema tn

initErrExit :: QErr -> IO a
initErrExit e = do
  putStrLn $
    "failed to build schema-cache because of inconsistent metadata: "
    <> T.unpack (qeError e)
  exitFailure

data HasuraApp
  = HasuraApp
  { _hapApplication    :: !Wai.Application
  , _hapSchemaRef      :: !SchemaCacheRef
  , _hapCacheBuildTime :: !(Maybe UTCTime)
  , _hapShutdown       :: !(IO ())
  }

mkWaiApp
  :: forall m.
     ( MonadIO m
     , MonadStateless IO m
     , ConsoleRenderer m
     , HttpLog m
     , UserAuthentication m
     , MetadataApiAuthorization m
     , LA.Forall (LA.Pure m)
     )
  => Q.TxIsolation
  -> L.Logger L.Hasura
  -> SQLGenCtx
  -> Bool
  -> Q.PGPool
  -> Q.ConnInfo
  -> HTTP.Manager
  -> AuthMode
  -> CorsConfig
  -> Bool
  -> Maybe Text
  -> Bool
  -> InstanceId
  -> S.HashSet API
  -> EL.LiveQueriesOptions
  -> E.PlanCacheOptions
  -> m HasuraApp
mkWaiApp isoLevel logger sqlGenCtx enableAL pool ci httpManager mode corsCfg enableConsole consoleAssetsDir
         enableTelemetry instanceId apis lqOpts planCacheOptions = do

    let pgExecCtx = PGExecCtx pool isoLevel
        pgExecCtxSer = PGExecCtx pool Q.Serializable
        runCtx = RunCtx adminUserInfo httpManager sqlGenCtx

    (cacheRef, cacheBuiltTime) <- do
      pgResp <- runExceptT $ peelRun emptySchemaCache runCtx pgExecCtxSer Q.ReadWrite $ do
        buildSchemaCache
        liftTx fetchLastUpdate
      (time, sc) <- either (liftIO . initErrExit) return pgResp
      scRef <- liftIO $  newIORef (sc, initSchemaCacheVer)
      return (scRef, snd <$> time)

    cacheLock <- liftIO $ newMVar ()
    planCache <- liftIO $ E.initPlanCache planCacheOptions

    let corsPolicy = mkDefaultCorsPolicy corsCfg

    lqState <- liftIO $ EL.initLiveQueriesState lqOpts pgExecCtx
    wsServerEnv <- WS.createWSServerEnv logger pgExecCtx lqState cacheRef httpManager corsPolicy
                   sqlGenCtx enableAL planCache

    ekgStore <- liftIO EKG.newStore

    let schemaCacheRef = SchemaCacheRef cacheLock cacheRef (E.clearPlanCache planCache)
        serverCtx = ServerCtx
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
                    }

    when (isDeveloperAPIEnabled serverCtx) $ do
      liftIO $ EKG.registerGcMetrics ekgStore
      liftIO $ EKG.registerCounter "ekg.server_timestamp_ms" getTimeMs ekgStore

    spockApp <- liftWithStateless $ \lowerIO ->
      Spock.spockAsApp $ Spock.spockT lowerIO $ httpApp corsCfg serverCtx enableConsole consoleAssetsDir enableTelemetry

    let wsServerApp = WS.createWSServerApp mode wsServerEnv
        stopWSServer = WS.stopWSServerApp wsServerEnv

    waiApp <- liftWithStateless $ \lowerIO ->
      pure $ WS.websocketsOr WS.defaultConnectionOptions (lowerIO . wsServerApp) spockApp

    return $ HasuraApp waiApp schemaCacheRef cacheBuiltTime stopWSServer
  where
    getTimeMs :: IO Int64
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime


httpApp
  :: (MonadIO m, ConsoleRenderer m, HttpLog m, UserAuthentication m, MetadataApiAuthorization m)
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
      sc <- liftIO $ getSCFromRef $ scCacheRef serverCtx
      dbOk <- checkDbConnection
      if dbOk
        then Spock.setStatus HTTP.status200 >> (Spock.text $ if null (scInconsistentObjs sc)
                                                 then "OK"
                                                 else "WARN: inconsistent objects in schema")
        else Spock.setStatus HTTP.status500 >> Spock.text "ERROR"

    Spock.get "v1/version" $ do
      uncurry Spock.setHeader jsonHeader
      Spock.lazyBytes $ encode $ object [ "version" .= currentVersion ]

    when enableMetadata $ do

      Spock.post "v1/query" $ spockAction encodeQErr id $
        mkPostHandler $ mkAPIRespHandler v1QueryHandler

      Spock.post ("api/1/table" <//> Spock.var <//> Spock.var) $ \tableName queryType ->
        mkSpockAction serverCtx encodeQErr id $ mkPostHandler $
          mkAPIRespHandler $ legacyQueryHandler (TableName tableName) queryType

    when enablePGDump $
      Spock.post "v1alpha1/pg_dump" $ spockAction encodeQErr id $
        mkPostHandler v1Alpha1PGDumpHandler

    when enableConfig $
      Spock.get "v1alpha1/config" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          let res = encJFromJValue $ runGetConfig (scAuthMode serverCtx)
          return $ JSONResp $ HttpResponse res Nothing

    when enableGraphQL $ do
      Spock.post "v1alpha1/graphql/explain" gqlExplainAction

      Spock.post "v1alpha1/graphql" $ spockAction GH.encodeGQErr id $
        mkPostHandler $ mkAPIRespHandler v1Alpha1GQHandler

      Spock.post "v1/graphql/explain" gqlExplainAction

      Spock.post "v1/graphql" $ spockAction GH.encodeGQErr allMod200 $
        mkPostHandler $ mkAPIRespHandler v1GQHandler

    when (isDeveloperAPIEnabled serverCtx) $ do
      Spock.get "dev/ekg" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EKG.sampleAll $ scEkgStore serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue $ EKG.sampleToJson respJ) Nothing
      Spock.get "dev/plan_cache" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ E.dumpPlanCache $ scPlanCache serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue respJ) Nothing
      Spock.get "dev/subscriptions" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState False $ scLQState serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue respJ) Nothing
      Spock.get "dev/subscriptions/extended" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState True $ scLQState serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue respJ) Nothing

    forM_ [Spock.GET, Spock.POST] $ \m -> Spock.hookAny m $ \_ -> do
      req <- Spock.request
      let headers = Wai.requestHeaders req
      let qErr = err404 NotFound "resource does not exist"
      raiseGenericApiError logger headers qErr

  where
    logger = scLogger serverCtx

    spockAction
      :: (FromJSON a, ToJSON a, MonadIO m, UserAuthentication m, HttpLog m)
      => (Bool -> QErr -> Value) -> (QErr -> QErr) -> APIHandler m a -> Spock.ActionT m ()
    spockAction = mkSpockAction serverCtx


    -- all graphql errors should be of type 200
    allMod200 qe = qe { qeStatus = HTTP.status200 }

    gqlExplainAction =
      spockAction encodeQErr id $ mkPostHandler $
        mkAPIRespHandler gqlExplainHandler

    enableGraphQL = isGraphQLEnabled serverCtx
    enableMetadata = isMetadataEnabled serverCtx
    enablePGDump = isPGDumpEnabled serverCtx
    enableConfig = isConfigEnabled serverCtx

    checkDbConnection = do
      e <- liftIO $ runExceptT $ runLazyTx' (scPGExecCtx serverCtx) select1Query
      pure $ isRight e
      where
        select1Query :: (MonadTx m) => m Int
        select1Query =   liftTx $ runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
                         [Q.sql| SELECT 1 |] () False

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
  uncurry Spock.setHeader jsonHeader
  Spock.setStatus $ qeStatus qErr
  Spock.lazyBytes $ encode qErr
