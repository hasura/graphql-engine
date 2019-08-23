{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Hasura.Server.App where

import           Control.Concurrent.MVar
import           Control.Exception                      (IOException, try)
import           Data.Aeson                             hiding (json)
import           Data.Int                               (Int64)
import           Data.IORef
import           Data.Time.Clock                        (UTCTime,
                                                         getCurrentTime)
import           Data.Time.Clock.POSIX                  (getPOSIXTime)
import           Network.Mime                           (defaultMimeLookup)
import           Network.Wai                            (requestHeaders,
                                                         strictRequestBody)
import           System.Exit                            (exitFailure)
import           System.FilePath                        (joinPath, takeFileName)
import           Web.Spock.Core

import qualified Data.ByteString.Lazy                   as BL
import qualified Data.HashMap.Strict                    as M
import qualified Data.HashSet                           as S
import qualified Data.Text                              as T
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wai                            as Wai
import qualified Network.Wai.Handler.WebSockets         as WS
import qualified Network.WebSockets                     as WS
import qualified System.Metrics                         as EKG
import qualified System.Metrics.Json                    as EKG
import qualified Text.Mustache                          as M
import qualified Text.Mustache.Compile                  as M

import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Explain                 as GE
import qualified Hasura.GraphQL.Transport.HTTP          as GH
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Transport.WebSocket     as WS
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.PGDump                   as PGD

import           Hasura.EncJSON
import           Hasura.Prelude                         hiding (get, put)
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.Server.Auth                     (AuthMode (..),
                                                         getUserInfo)
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

consoleTmplt :: M.Template
consoleTmplt = $(M.embedSingleTemplate "src-rsr/console.html")

boolToText :: Bool -> T.Text
boolToText = bool "false" "true"

isAdminSecretSet :: AuthMode -> T.Text
isAdminSecretSet AMNoAuth = boolToText False
isAdminSecretSet _        = boolToText True

data SchemaCacheRef
  = SchemaCacheRef
  { _scrLock     :: MVar ()
  , _scrCache    :: IORef (SchemaCache, SchemaCacheVer)
  -- an action to run when schemacache changes
  , _scrOnChange :: IO ()
  }

getSCFromRef :: SchemaCacheRef -> IO SchemaCache
getSCFromRef scRef = fst <$> readIORef (_scrCache scRef)

logInconsObjs :: L.Logger -> [InconsistentMetadataObj] -> IO ()
logInconsObjs logger objs =
  unless (null objs) $ L.unLogger logger $ mkInconsMetadataLog objs

withSCUpdate
  :: (MonadIO m, MonadError e m)
  => SchemaCacheRef -> L.Logger -> m (a, SchemaCache) -> m a
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

data ServerCtx
  = ServerCtx
  { scPGExecCtx       :: !PGExecCtx
  , scConnInfo        :: !Q.ConnInfo
  , scLogger          :: !L.Logger
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
  , hcReqHeaders :: ![N.Header]
  , hcRequestId  :: !RequestId
  }

type Handler = ExceptT QErr (ReaderT HandlerCtx IO)

type HasuraMiddleware a = a -> Handler ()

type UserAuthMiddleware =
  L.Logger -> HTTP.Manager -> [N.Header] -> AuthMode -> ExceptT QErr IO UserInfo

data APIResp
  = JSONResp !(HttpResponse EncJSON)
  | RawResp  !(HttpResponse BL.ByteString)

apiRespToLBS :: APIResp -> BL.ByteString
apiRespToLBS = \case
  JSONResp (HttpResponse j _) -> encJToLBS j
  RawResp (HttpResponse b _)  -> b

data APIHandler a
  = AHGet !(Handler APIResp)
  | AHPost !(a -> Handler APIResp)


postGql :: (Text, Text)
postGql = ("/v1/graphql", "POST")

postV1q :: (Text, Text)
postV1q = ("/v1/query", "POST")

mkGetHandler :: Handler APIResp -> APIHandler ()
mkGetHandler = AHGet

mkPostHandler :: (a -> Handler APIResp) -> APIHandler a
mkPostHandler = AHPost

mkAPIRespHandler :: (a -> Handler (HttpResponse EncJSON)) -> (a -> Handler APIResp)
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


onlyAdmin :: Handler ()
onlyAdmin = do
  uRole <- asks (userRole . hcUser)
  when (uRole /= adminRole) $
    throw400 AccessDenied "You have to be an admin to access this endpoint"

buildQCtx ::  Handler QCtx
buildQCtx = do
  scRef    <- scCacheRef . hcServerCtx <$> ask
  userInfo <- asks hcUser
  cache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
  sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
  return $ QCtx userInfo cache sqlGenCtx

logResult
  :: (MonadIO m)
  => L.Logger
  -> Maybe UserInfo
  -> RequestId
  -> Wai.Request
  -> Maybe Value
  -> Either QErr BL.ByteString
  -> Maybe (UTCTime, UTCTime)
  -> m ()
logResult logger userInfoM reqId httpReq req res qTime = do
  let logline = case res of
        Right res' -> mkHttpAccessLog userInfoM reqId httpReq res' qTime
        Left e     -> mkHttpErrorLog userInfoM reqId httpReq e req qTime
  liftIO $ L.unLogger logger logline

logSuccess
  :: (MonadIO m)
  => L.Logger
  -> Maybe UserInfo
  -> RequestId
  -> Wai.Request
  -> BL.ByteString
  -> Maybe (UTCTime, UTCTime)
  -> m ()
logSuccess logger userInfoM reqId httpReq res qTime =
  liftIO $ L.unLogger logger $ mkHttpAccessLog userInfoM reqId httpReq res qTime

logError
  :: (MonadIO m)
  => L.Logger
  -> Maybe UserInfo
  -> RequestId
  -> Wai.Request
  -> Maybe Value
  -> QErr -> m ()
logError logger userInfoM reqId httpReq req qErr =
  liftIO $ L.unLogger logger $ mkHttpErrorLog userInfoM reqId httpReq qErr req Nothing

mkSpockAction
  :: (MonadIO m, FromJSON a, ToJSON a)
  => ServerCtx
  -> Maybe UserAuthMiddleware
  -- -> Maybe (HasuraMiddleware RQLQuery)
  -> (Bool -> QErr -> Value)
  -- ^ `QErr` JSON encoder function
  -> (QErr -> QErr)
  -- ^ `QErr` modifier
  -> APIHandler a
  -> ActionT m ()
mkSpockAction serverCtx userAuthMiddleware qErrEncoder qErrModifier apiHandler = do
  req <- request
  reqBody <- liftIO $ strictRequestBody req
  let headers = requestHeaders req
      authMode = scAuthMode serverCtx
      manager = scManager serverCtx
      -- convert ByteString to Maybe Value for logging
      reqTxt = Just $ String $ bsToTxt $ BL.toStrict reqBody

  requestId <- getRequestId headers
  -- default to @getUserInfo@ if no user-auth middleware is passed
  let resolveUserInfo = fromMaybe getUserInfo userAuthMiddleware
  userInfoE <- liftIO $ runExceptT $ resolveUserInfo logger manager headers authMode
  userInfo  <- either (logErrorAndResp Nothing requestId req reqTxt False . qErrModifier)
               return userInfoE

  let handlerState = HandlerCtx serverCtx userInfo headers requestId
      curRole = userRole userInfo

  t1 <- liftIO getCurrentTime -- for measuring response time purposes

  (result, q) <- case apiHandler of
    AHGet handler -> do
      res <- liftIO $ runReaderT (runExceptT handler) handlerState
      return (res, Nothing)
    AHPost handler -> do
      parsedReqE <- runExceptT $ parseBody reqBody
      parsedReq  <- either (logErrorAndResp (Just userInfo) requestId req reqTxt (isAdmin curRole) . qErrModifier) return parsedReqE
      res <- liftIO $ runReaderT (runExceptT $ handler parsedReq) handlerState
      return (res, Just parsedReq)

  t2 <- liftIO getCurrentTime -- for measuring response time purposes

  -- apply the error modifier
  let modResult = fmapL qErrModifier result

  -- log and return result
  case modResult of
    Left err  -> logErrorAndResp (Just userInfo) requestId req (toJSON <$> q) (isAdmin curRole) err
    Right res -> logSuccessAndResp (Just userInfo) requestId req res (Just (t1, t2))

  where
    logger = scLogger serverCtx

    logErrorAndResp
      :: (MonadIO m)
      => Maybe UserInfo -> RequestId -> Wai.Request -> Maybe Value -> Bool -> QErr -> ActionCtxT ctx m a
    logErrorAndResp userInfo reqId req reqBody includeInternal qErr = do
      logError logger userInfo reqId req reqBody qErr
      setStatus $ qeStatus qErr
      json $ qErrEncoder includeInternal qErr

    logSuccessAndResp userInfo reqId req result qTime = do
      logSuccess logger userInfo reqId req (apiRespToLBS result) qTime
      case result of
        JSONResp (HttpResponse j h) -> do
          uncurry setHeader jsonHeader
          uncurry setHeader (requestIdHeader, unRequestId reqId)
          mapM_ (mapM_ (uncurry setHeader . unHeader)) h
          lazyBytes $ encJToLBS j
        RawResp (HttpResponse b h) -> do
          uncurry setHeader (requestIdHeader, unRequestId reqId)
          mapM_ (mapM_ (uncurry setHeader . unHeader)) h
          lazyBytes b

v1QueryHandler :: Maybe (HasuraMiddleware RQLQuery) -> RQLQuery -> Handler (HttpResponse EncJSON)
v1QueryHandler metadataMiddleware query = do
  -- run any given metadata query middleware
  maybe (return ()) (\m -> m query) metadataMiddleware
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
      runQuery pgExecCtx instanceId userInfo schemaCache httpMgr sqlGenCtx query

v1Alpha1GQHandler :: GH.GQLReqUnparsed -> Handler (HttpResponse EncJSON)
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
  flip runReaderT execCtx $ GH.runGQ requestId userInfo reqHeaders query

v1GQHandler
  :: GH.GQLReqUnparsed
  -> Handler (HttpResponse EncJSON)
v1GQHandler = v1Alpha1GQHandler

gqlExplainHandler :: GE.GQLExplain -> Handler (HttpResponse EncJSON)
gqlExplainHandler query = do
  onlyAdmin
  scRef <- scCacheRef . hcServerCtx <$> ask
  sc <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
  pgExecCtx <- scPGExecCtx . hcServerCtx <$> ask
  sqlGenCtx <- scSQLGenCtx . hcServerCtx <$> ask
  enableAL <- scEnableAllowlist . hcServerCtx <$> ask
  res <- GE.explainGQLQuery pgExecCtx sc sqlGenCtx enableAL query
  return $ HttpResponse res Nothing

v1Alpha1PGDumpHandler :: PGD.PGDumpReqBody -> Handler APIResp
v1Alpha1PGDumpHandler b = do
  onlyAdmin
  ci <- scConnInfo . hcServerCtx <$> ask
  output <- PGD.execPGDump b ci
  return $ RawResp $ HttpResponse output (Just [Header sqlHeader])

consoleAssetsHandler :: L.Logger -> Text -> FilePath -> ActionT IO ()
consoleAssetsHandler logger dir path = do
  -- '..' in paths need not be handed as it is resolved in the url by
  -- spock's routing. we get the expanded path.
  eFileContents <- liftIO $ try $ BL.readFile $
    joinPath [T.unpack dir, path]
  either onError onSuccess eFileContents
  where
    onSuccess c = do
      mapM_ (uncurry setHeader) headers
      lazyBytes c
    onError :: IOException -> ActionT IO ()
    onError = raiseGenericApiError logger . err404 NotFound . T.pack . show
    fn = T.pack $ takeFileName path
    -- set gzip header if the filename ends with .gz
    (fileName, encHeader) = case T.stripSuffix ".gz" fn of
      Just v  -> (v, [gzipHeader])
      Nothing -> (fn, [])
    mimeType = bsToTxt $ defaultMimeLookup fileName
    headers = ("Content-Type", mimeType) : encHeader

mkConsoleHTML :: T.Text -> AuthMode -> Bool -> Maybe Text -> Either String T.Text
mkConsoleHTML path authMode enableTelemetry consoleAssetsDir =
  renderConsoleHtml consoleTmplt $
    -- variables required to render the template
    object [ "isAdminSecretSet" .= isAdminSecretSet authMode
           , "consolePath" .= consolePath
           , "enableTelemetry" .= boolToText enableTelemetry
           , "cdnAssets" .= boolToText (isNothing consoleAssetsDir)
           , "assetsVersion" .= consoleVersion
           , "serverVersion" .= currentVersion
           ]
   where
    consolePath = case path of
      "" -> "/console"
      r  -> "/console/" <> r

renderConsoleHtml :: M.Template -> Value -> Either String Text
renderConsoleHtml consoleTemplate jVal =
  bool (Left errMsg) (Right res) $ null errs
  where
    errMsg = "console template rendering failed: " ++ show errs
    (errs, res) = M.checkedSubstitute consoleTemplate jVal

newtype ConsoleRenderer
  = ConsoleRenderer
  { unConsoleRenderer :: T.Text -> AuthMode -> Bool -> Maybe Text -> Either String Text
  }

newtype QueryParser
  = QueryParser
  { getQueryParser :: QualifiedTable -> Object -> Handler RQLQuery }

queryParsers :: M.HashMap T.Text QueryParser
queryParsers =
  M.fromList
  [ ("select", mkQueryParser RQSelect)
  , ("insert", mkQueryParser RQInsert)
  , ("update", mkQueryParser RQUpdate)
  , ("delete", mkQueryParser RQDelete)
  , ("count", mkQueryParser RQCount)
  ]
  where
    mkQueryParser f =
      QueryParser $ \qt obj -> do
      let val = Object $ M.insert "table" (toJSON qt) obj
      q <- decodeValue val
      return $ f q

legacyQueryHandler
  :: Maybe (HasuraMiddleware RQLQuery)
  -> TableName -> T.Text -> Object
  -> Handler (HttpResponse EncJSON)
legacyQueryHandler metadataMiddleware tn queryType req =
  case M.lookup queryType queryParsers of
    Just queryParser -> getQueryParser queryParser qt req >>= v1QueryHandler metadataMiddleware
    Nothing          -> throw404 "No such resource exists"
  where
    qt = QualifiedObject publicSchema tn

initErrExit :: QErr -> IO a
initErrExit e = do
  putStrLn $
    "failed to build schema-cache because of inconsistent metadata: "
    <> T.unpack (qeError e)
  exitFailure

mkWaiApp
  :: Q.TxIsolation
  -> L.LoggerCtx
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
  -> EL.LQOpts
  -> Maybe UserAuthMiddleware
  -> Maybe (HasuraMiddleware RQLQuery)
  -> Maybe ConsoleRenderer
  -> IO (Wai.Application, SchemaCacheRef, Maybe UTCTime)
mkWaiApp isoLevel loggerCtx sqlGenCtx enableAL pool ci httpManager mode corsCfg
         enableConsole consoleAssetsDir enableTelemetry instanceId apis
         lqOpts authMiddleware metadataMiddleware renderConsole = do
    let pgExecCtx = PGExecCtx pool isoLevel
        pgExecCtxSer = PGExecCtx pool Q.Serializable
    (cacheRef, cacheBuiltTime) <- do
      pgResp <- runExceptT $ peelRun emptySchemaCache adminUserInfo
                httpManager sqlGenCtx pgExecCtxSer $ do
                  buildSchemaCache
                  liftTx fetchLastUpdate
      (time, sc) <- either initErrExit return pgResp
      scRef <- newIORef (sc, initSchemaCacheVer)
      return (scRef, snd <$> time)

    cacheLock <- newMVar ()
    planCache <- E.initPlanCache

    let corsPolicy = mkDefaultCorsPolicy corsCfg
        logger = L.mkLogger loggerCtx

    lqState <- EL.initLiveQueriesState lqOpts pgExecCtx
    wsServerEnv <- WS.createWSServerEnv logger pgExecCtx lqState cacheRef
                   httpManager corsPolicy sqlGenCtx enableAL planCache

    ekgStore <- EKG.newStore

    let schemaCacheRef =
          SchemaCacheRef cacheLock cacheRef (E.clearPlanCache planCache)
        serverCtx = ServerCtx pgExecCtx ci logger
                    schemaCacheRef mode httpManager
                    sqlGenCtx apis instanceId planCache
                    lqState enableAL ekgStore

    when (isDeveloperAPIEnabled serverCtx) $ do
      EKG.registerGcMetrics ekgStore
      EKG.registerCounter "ekg.server_timestamp_ms" getTimeMs ekgStore

    spockApp <- spockAsApp $ spockT id $
                httpApp corsCfg serverCtx enableConsole
                  consoleAssetsDir enableTelemetry authMiddleware metadataMiddleware renderConsole

    let wsServerApp = WS.createWSServerApp mode wsServerEnv
    return ( WS.websocketsOr WS.defaultConnectionOptions wsServerApp spockApp
           , schemaCacheRef
           , cacheBuiltTime
           )
  where
    getTimeMs :: IO Int64
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

httpApp
  :: CorsConfig
  -> ServerCtx
  -> Bool
  -> Maybe Text
  -> Bool
  -> Maybe UserAuthMiddleware
  -> Maybe (HasuraMiddleware RQLQuery)
  -- ^ The current middleware is only for RQLQuery (metadata) queries, in future
  -- this can be extended to take a `HashMap (HttpMethod, Path) (HasuraMiddleware a)`
  -> Maybe ConsoleRenderer
  -> SpockT IO ()
httpApp corsCfg serverCtx enableConsole consoleAssetsDir enableTelemetry
        authMiddleware metadataMiddleware consoleRenderer = do

    -- cors middleware
    unless (isCorsDisabled corsCfg) $
      middleware $ corsMiddleware (mkDefaultCorsPolicy corsCfg)

    -- API Console and Root Dir
    when (enableConsole && enableMetadata) serveApiConsole

    -- Health check endpoint
    get "healthz" $ do
      sc <- liftIO $ getSCFromRef $ scCacheRef serverCtx
      if null $ scInconsistentObjs sc
        then setStatus N.status200 >> lazyBytes "OK"
        else setStatus N.status500 >> lazyBytes "ERROR"

    get "v1/version" $ do
      uncurry setHeader jsonHeader
      lazyBytes $ encode $ object [ "version" .= currentVersion ]

    when enableMetadata $ do

      post "v1/query" $ spockAction encodeQErr id $
        mkPostHandler $ mkAPIRespHandler (v1QueryHandler metadataMiddleware)

      post ("api/1/table" <//> var <//> var) $ \tableName queryType ->
        mkSpockAction serverCtx authMiddleware encodeQErr id $ mkPostHandler $
          mkAPIRespHandler $ legacyQueryHandler metadataMiddleware (TableName tableName) queryType

    when enablePGDump $
      post "v1alpha1/pg_dump" $ spockAction encodeQErr id $
        mkPostHandler v1Alpha1PGDumpHandler

    when enableConfig $
      get "v1alpha1/config" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          let res = encJFromJValue $ runGetConfig (scAuthMode serverCtx)
          return $ JSONResp $ HttpResponse res Nothing

    when enableGraphQL $ do
      post "v1alpha1/graphql/explain" gqlExplainAction

      post "v1alpha1/graphql" $ spockAction GH.encodeGQErr id $
        mkPostHandler $ mkAPIRespHandler v1Alpha1GQHandler

      post "v1/graphql/explain" gqlExplainAction

      post "v1/graphql" $ spockAction GH.encodeGQErr allMod200 $
        mkPostHandler $ mkAPIRespHandler v1GQHandler

    when (isDeveloperAPIEnabled serverCtx) $ do
      get "dev/ekg" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EKG.sampleAll $ scEkgStore serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue $ EKG.sampleToJson respJ) Nothing
      get "dev/plan_cache" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ E.dumpPlanCache $ scPlanCache serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue respJ) Nothing
      get "dev/subscriptions" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState False $ scLQState serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue respJ) Nothing
      get "dev/subscriptions/extended" $ spockAction encodeQErr id $
        mkGetHandler $ do
          onlyAdmin
          respJ <- liftIO $ EL.dumpLiveQueriesState True $ scLQState serverCtx
          return $ JSONResp $ HttpResponse (encJFromJValue respJ) Nothing

    forM_ [GET,POST] $ \m -> hookAny m $ \_ -> do
      let qErr = err404 NotFound "resource does not exist"
      raiseGenericApiError logger qErr

  where
    logger = scLogger serverCtx

    spockAction :: (FromJSON a, ToJSON a) => (Bool -> QErr -> Value) -> (QErr -> QErr) -> APIHandler a -> ActionT IO ()
    spockAction = mkSpockAction serverCtx authMiddleware

    -- all graphql errors should be of type 200
    allMod200 qe = qe { qeStatus = N.status200 }

    gqlExplainAction =
      spockAction encodeQErr id $ mkPostHandler $
        mkAPIRespHandler gqlExplainHandler

    enableGraphQL = isGraphQLEnabled serverCtx
    enableMetadata = isMetadataEnabled serverCtx
    enablePGDump = isPGDumpEnabled serverCtx
    enableConfig = isConfigEnabled serverCtx

    serveApiConsole = do
      -- redirect / to /console
      get root $ redirect "console"

      -- serve static files if consoleAssetsDir is set
      onJust consoleAssetsDir $ \dir ->
        get ("console/assets" <//> wildcard) $ \path ->
          consoleAssetsHandler logger dir (T.unpack path)

      -- serve console html
      get ("console" <//> wildcard) $ \path -> do
        let authMode = scAuthMode serverCtx
        let consoleHtml = maybe (mkConsoleHTML path authMode enableTelemetry consoleAssetsDir)
                                (\cr -> unConsoleRenderer cr path authMode enableTelemetry consoleAssetsDir)
                                consoleRenderer
        either (raiseGenericApiError logger . err500 Unexpected . T.pack) html consoleHtml

raiseGenericApiError :: L.Logger -> QErr -> ActionT IO ()
raiseGenericApiError logger qErr = do
  req <- request
  reqBody <- liftIO $ strictRequestBody req
  let reqTxt = toJSON $ String $ bsToTxt $ BL.toStrict reqBody
  reqId <- getRequestId $ requestHeaders req
  logError logger Nothing reqId req (Just reqTxt) qErr
  uncurry setHeader jsonHeader
  setStatus $ qeStatus qErr
  lazyBytes $ encode qErr
