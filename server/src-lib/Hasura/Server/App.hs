{-# LANGUAGE CPP        #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Hasura.Server.App where

import           Control.Arrow                          ((***))
import           Control.Concurrent.MVar
import           Data.Aeson                             hiding (json)
import           Data.IORef
import           Data.Time.Clock                        (UTCTime,
                                                         getCurrentTime)
import           Network.Wai                            (requestHeaders,
                                                         strictRequestBody)
import           System.Exit                            (exitFailure)
import           Web.Spock.Core

import qualified Data.ByteString.Lazy                   as BL
#ifdef LocalConsole
import qualified Data.FileEmbed                         as FE
#endif
import qualified Data.HashMap.Strict                    as M
import qualified Data.HashSet                           as S
import qualified Data.Text                              as T
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wai                            as Wai
import qualified Network.Wai.Handler.WebSockets         as WS
import qualified Network.WebSockets                     as WS
import qualified Text.Mustache                          as M
import qualified Text.Mustache.Compile                  as M

import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Explain                 as GE
import qualified Hasura.GraphQL.Schema                  as GS
import qualified Hasura.GraphQL.Transport.HTTP          as GH
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Transport.WebSocket     as WS
import qualified Hasura.Logging                         as L

import           Hasura.EncJSON
import           Hasura.GraphQL.RemoteServer
import           Hasura.Prelude                         hiding (get, put)
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DML.QueryTemplate
import           Hasura.RQL.Types
import           Hasura.Server.Auth                     (AuthMode (..),
                                                         getUserInfo)
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

#ifdef LocalConsole
consoleAssetsLoc :: Text
consoleAssetsLoc = "/static"
#else
consoleAssetsLoc :: Text
consoleAssetsLoc =
  "https://storage.googleapis.com/hasura-graphql-engine/console/" <> consoleVersion
#endif

mkConsoleHTML :: T.Text -> AuthMode -> Bool -> Either String T.Text
mkConsoleHTML path authMode enableTelemetry =
  bool (Left errMsg) (Right res) $ null errs
  where
    (errs, res) = M.checkedSubstitute consoleTmplt $
                  object [ "consoleAssetsLoc" .= consoleAssetsLoc
                         , "isAdminSecretSet" .= isAdminSecretSet authMode
                         , "consolePath" .= consolePath
                         , "enableTelemetry" .= boolToText enableTelemetry
                         ]
    consolePath = case path of
      "" -> "/console"
      r  -> "/console/" <> r

    errMsg = "console template rendering failed: " ++ show errs

data SchemaCacheRef
  = SchemaCacheRef
  { _scrLock      :: MVar ()
  , _scrCache     :: IORef (SchemaCache, SchemaCacheVer)
  , _scrPlanCache :: E.PlanCache
  }

withSCUpdate
  :: (MonadIO m, MonadError e m)
  => SchemaCacheRef -> m (a, SchemaCache) -> m a
withSCUpdate scr action = do
  acquireLock
  (res, newSC) <- action `catchError` onError
  -- update schemacache in IO reference
  liftIO $ modifyIORef' cacheRef $
    \(_, prevVer) -> (newSC, incSchemaCacheVer prevVer)
  liftIO $ E.clearPlanCache planCache
  releaseLock
  return res
  where
    SchemaCacheRef lk cacheRef planCache = scr
    onError e   = releaseLock >> throwError e
    acquireLock = liftIO $ takeMVar lk
    releaseLock = liftIO $ putMVar lk ()

data ServerCtx
  = ServerCtx
  { scIsolation    :: Q.TxIsolation
  , scPGPool       :: Q.PGPool
  , scLogger       :: L.Logger
  , scCacheRef     :: SchemaCacheRef
  , scAuthMode     :: AuthMode
  , scManager      :: HTTP.Manager
  , scStringifyNum :: Bool
  , scEnabledAPIs  :: S.HashSet API
  , scInstanceId   :: InstanceId
  }

data HandlerCtx
  = HandlerCtx
  { hcServerCtx  :: ServerCtx
  , hcReqBody    :: BL.ByteString
  , hcUser       :: UserInfo
  , hcReqHeaders :: [N.Header]
  }

type Handler = ExceptT QErr (ReaderT HandlerCtx IO)

isMetadataEnabled :: ServerCtx -> Bool
isMetadataEnabled sc = S.member METADATA $ scEnabledAPIs sc

isGraphQLEnabled :: ServerCtx -> Bool
isGraphQLEnabled sc = S.member GRAPHQL $ scEnabledAPIs sc

-- {-# SCC parseBody #-}
parseBody :: (FromJSON a) => Handler a
parseBody = do
  reqBody <- hcReqBody <$> ask
  case decode' reqBody of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"

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
  strfyNum <- scStringifyNum . hcServerCtx <$> ask
  return $ QCtx userInfo cache $ SQLGenCtx strfyNum

logResult
  :: (MonadIO m)
  => Maybe UserInfo -> Wai.Request -> BL.ByteString -> ServerCtx
  -> Either QErr BL.ByteString -> Maybe (UTCTime, UTCTime)
  -> m ()
logResult userInfoM req reqBody sc res qTime =
  liftIO $ logger $ mkAccessLog userInfoM req (reqBody, res) qTime
  where
    logger = L.unLogger $ scLogger sc

logError
  :: MonadIO m
  => Maybe UserInfo -> Wai.Request
  -> BL.ByteString -> ServerCtx -> QErr -> m ()
logError userInfoM req reqBody sc qErr =
  logResult userInfoM req reqBody sc (Left qErr) Nothing

mkSpockAction
  :: (MonadIO m)
  => (Bool -> QErr -> Value)
  -> ServerCtx
  -> Handler EncJSON
  -> ActionT m ()
mkSpockAction qErrEncoder serverCtx handler = do
  req <- request
  reqBody <- liftIO $ strictRequestBody req
  let headers  = requestHeaders req
      authMode = scAuthMode serverCtx
      manager = scManager serverCtx

  userInfoE <- liftIO $ runExceptT $ getUserInfo logger manager headers authMode
  userInfo <- either (logAndThrow req reqBody False) return userInfoE

  let handlerState = HandlerCtx serverCtx reqBody userInfo headers

  t1 <- liftIO getCurrentTime -- for measuring response time purposes
  result <- liftIO $ runReaderT (runExceptT handler) handlerState
  t2 <- liftIO getCurrentTime -- for measuring response time purposes

  let resLBS = fmap encJToLBS result

  -- log result
  logResult (Just userInfo) req reqBody serverCtx resLBS $ Just (t1, t2)
  either (qErrToResp $ userRole userInfo == adminRole) resToResp resLBS

  where
    logger = scLogger serverCtx
    -- encode error response
    qErrToResp :: (MonadIO m) => Bool -> QErr -> ActionCtxT ctx m b
    qErrToResp includeInternal qErr = do
      setStatus $ qeStatus qErr
      json $ qErrEncoder includeInternal qErr

    logAndThrow req reqBody includeInternal qErr = do
      logError Nothing req reqBody serverCtx qErr
      qErrToResp includeInternal qErr

    resToResp resp = do
      uncurry setHeader jsonHeader
      lazyBytes resp

v1QueryHandler :: RQLQuery -> Handler EncJSON
v1QueryHandler query = do
  scRef <- scCacheRef . hcServerCtx <$> ask
  bool (fst <$> dbAction) (withSCUpdate scRef dbActionReload) $
    queryNeedsReload query
  where
    -- Hit postgres
    dbAction = do
      userInfo <- asks hcUser
      scRef <- scCacheRef . hcServerCtx <$> ask
      schemaCache <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
      httpMgr <- scManager . hcServerCtx <$> ask
      strfyNum <- scStringifyNum . hcServerCtx <$> ask
      pool <- scPGPool . hcServerCtx <$> ask
      isoL <- scIsolation . hcServerCtx <$> ask
      instanceId <- scInstanceId . hcServerCtx <$> ask
      runQuery pool isoL instanceId userInfo schemaCache httpMgr strfyNum query

    -- Also update the schema cache
    dbActionReload = do
      (resp, newSc) <- dbAction
      httpMgr <- scManager . hcServerCtx <$> ask
      --FIXME: should we be fetching the remote schema again? if not how do we get the remote schema?
      newGCtxMap <- GS.mkGCtxMap (scTables newSc) (scFunctions newSc)
      (mergedGCtxMap, defGCtx) <-
        mergeSchemas (scRemoteResolvers newSc) newGCtxMap httpMgr
      let newSc' =
            newSc { scGCtxMap = mergedGCtxMap, scDefaultRemoteGCtx = defGCtx }
      return (resp, newSc')

v1Alpha1GQHandler :: GH.GQLReqUnparsed -> Handler EncJSON
v1Alpha1GQHandler query = do
  userInfo <- asks hcUser
  reqBody <- asks hcReqBody
  reqHeaders <- asks hcReqHeaders
  manager <- scManager . hcServerCtx <$> ask
  scRef <- scCacheRef . hcServerCtx <$> ask
  (sc, scVer) <- liftIO $ readIORef $ _scrCache scRef
  pool <- scPGPool . hcServerCtx <$> ask
  isoL <- scIsolation . hcServerCtx <$> ask
  strfyNum <- scStringifyNum . hcServerCtx <$> ask
  planCache <- _scrPlanCache . scCacheRef . hcServerCtx <$> ask
  GH.runGQ pool isoL userInfo (SQLGenCtx strfyNum) planCache
    sc scVer manager reqHeaders query reqBody

gqlExplainHandler :: GE.GQLExplain -> Handler EncJSON
gqlExplainHandler query = do
  onlyAdmin
  scRef <- scCacheRef . hcServerCtx <$> ask
  sc <- fmap fst $ liftIO $ readIORef $ _scrCache scRef
  pool <- scPGPool . hcServerCtx <$> ask
  isoL <- scIsolation . hcServerCtx <$> ask
  strfyNum <- scStringifyNum . hcServerCtx <$> ask
  GE.explainGQLQuery pool isoL sc (SQLGenCtx strfyNum) query

newtype QueryParser
  = QueryParser { getQueryParser :: QualifiedTable -> Handler RQLQuery }

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
      QueryParser $ \qt -> do
      obj <- parseBody
      let val = Object $ M.insert "table" (toJSON qt) obj
      q <- decodeValue val
      return $ f q

legacyQueryHandler :: TableName -> T.Text -> Handler EncJSON
legacyQueryHandler tn queryType =
  case M.lookup queryType queryParsers of
    Just queryParser -> getQueryParser queryParser qt >>= v1QueryHandler
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
  :: Q.TxIsolation -> L.LoggerCtx -> Bool
  -> Q.PGPool -> HTTP.Manager -> AuthMode
  -> CorsConfig -> Bool -> Bool
  -> InstanceId -> S.HashSet API
  -> IO (Wai.Application, SchemaCacheRef, Maybe UTCTime)
mkWaiApp isoLevel loggerCtx strfyNum pool httpManager mode corsCfg
         enableConsole enableTelemetry instanceId apis = do
    (cacheRef, cacheBuiltTime) <- do
      pgResp <- runExceptT $ peelRun emptySchemaCache adminUserInfo
                httpManager strfyNum pool Q.Serializable $ do
                  buildSchemaCache
                  liftTx fetchLastUpdate
      (time, sc) <- either initErrExit return pgResp
      scRef <- newIORef (sc, initSchemaCacheVer)
      return (scRef, snd <$> time)

    cacheLock <- newMVar ()
    planCache <- E.initPlanCache


    let schemaCacheRef = SchemaCacheRef cacheLock cacheRef planCache
        serverCtx = ServerCtx isoLevel pool (L.mkLogger loggerCtx)
                    schemaCacheRef mode httpManager
                    strfyNum apis instanceId

    spockApp <- spockAsApp $ spockT id $
                httpApp corsCfg serverCtx enableConsole enableTelemetry

    let corsPolicy = mkDefaultCorsPolicy corsCfg
        sqlGenCtx = SQLGenCtx strfyNum

    wsServerEnv <- WS.createWSServerEnv (scLogger serverCtx) httpManager
                   sqlGenCtx cacheRef
                   (runExceptT . runLazyTx pool isoLevel)
                   corsPolicy planCache

    let wsServerApp = WS.createWSServerApp mode wsServerEnv
    return ( WS.websocketsOr WS.defaultConnectionOptions wsServerApp spockApp
           , schemaCacheRef
           , cacheBuiltTime
           )

httpApp :: CorsConfig -> ServerCtx -> Bool -> Bool -> SpockT IO ()
httpApp corsCfg serverCtx enableConsole enableTelemetry = do
    -- cors middleware
    unless (isCorsDisabled corsCfg) $
      middleware $ corsMiddleware (mkDefaultCorsPolicy corsCfg)

    -- API Console and Root Dir
    when (enableConsole && enableMetadata) serveApiConsole

    get "v1/version" $ do
      uncurry setHeader jsonHeader
      lazyBytes $ encode $ object [ "version" .= currentVersion ]

    when enableMetadata $ do
      get    ("v1/template" <//> var) tmpltGetOrDeleteH
      post   ("v1/template" <//> var) tmpltPutOrPostH
      put    ("v1/template" <//> var) tmpltPutOrPostH
      delete ("v1/template" <//> var) tmpltGetOrDeleteH

      post "v1/query" $ mkSpockAction encodeQErr serverCtx $ do
        query <- parseBody
        v1QueryHandler query

      post ("api/1/table" <//> var <//> var) $ \tableName queryType ->
        mkSpockAction encodeQErr serverCtx $
        legacyQueryHandler (TableName tableName) queryType

    when enableGraphQL $ do
      post "v1alpha1/graphql/explain" $ mkSpockAction encodeQErr serverCtx $ do
        expQuery <- parseBody
        gqlExplainHandler expQuery

      post "v1alpha1/graphql" $ mkSpockAction GH.encodeGQErr serverCtx $ do
        query <- parseBody
        v1Alpha1GQHandler query

    get "internal/plan_cache" $ do
      respJ <- liftIO $ E.dumpPlanCache $ _scrPlanCache $ scCacheRef serverCtx
      json respJ

    forM_ [GET,POST] $ \m -> hookAny m $ \_ -> do
      let qErr = err404 NotFound "resource does not exist"
      raiseGenericApiError qErr

  where
    enableGraphQL = isGraphQLEnabled serverCtx
    enableMetadata = isMetadataEnabled serverCtx
    tmpltGetOrDeleteH tmpltName = do
      tmpltArgs <- tmpltArgsFromQueryParams
      mkSpockAction encodeQErr serverCtx $ mkQTemplateAction tmpltName tmpltArgs

    tmpltPutOrPostH tmpltName = do
      tmpltArgs <- tmpltArgsFromQueryParams
      mkSpockAction encodeQErr serverCtx $ do
        bodyTmpltArgs <- parseBody
        mkQTemplateAction tmpltName $ M.union bodyTmpltArgs tmpltArgs

    tmpltArgsFromQueryParams = do
      qparams <- params
      return $ M.fromList $ flip map qparams $
        TemplateParam *** String

    mkQTemplateAction tmpltName tmpltArgs =
      v1QueryHandler $ RQExecuteQueryTemplate $
      ExecQueryTemplate (TQueryName tmpltName) tmpltArgs

    raiseGenericApiError qErr = do
      req <- request
      reqBody <- liftIO $ strictRequestBody req
      logError Nothing req reqBody serverCtx qErr
      uncurry setHeader jsonHeader
      setStatus $ qeStatus qErr
      lazyBytes $ encode qErr

    serveApiConsole = do
      get root $ redirect "console"
      get ("console" <//> wildcard) $ \path ->
        either (raiseGenericApiError . err500 Unexpected . T.pack) html $
          mkConsoleHTML path (scAuthMode serverCtx) enableTelemetry

#ifdef LocalConsole
      get "static/main.js" $ do
        setHeader "Content-Type" "text/javascript;charset=UTF-8"
        bytes $(FE.embedFile "../console/static/dist/main.js")
      get "static/main.css" $ do
        setHeader "Content-Type" "text/css;charset=UTF-8"
        bytes $(FE.embedFile "../console/static/dist/main.css")
      get "static/vendor.js" $ do
        setHeader "Content-Type" "text/javascript;charset=UTF-8"
        bytes $(FE.embedFile "../console/static/dist/vendor.js")
#endif
