{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hasura.Server.App where

import           Control.Arrow                          ((***))
import           Control.Concurrent.MVar
import           Data.Aeson                             hiding (json)
import           Data.IORef
import           Data.Time.Clock                        (UTCTime,
                                                         getCurrentTime)
import           Network.Wai                            (requestHeaders,
                                                         strictRequestBody)
import           Web.Spock.Core

import qualified Data.ByteString.Lazy                   as BL
import qualified Data.HashMap.Strict                    as M
import qualified Data.Text                              as T
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.Wai                            as Wai
import qualified Network.Wai.Handler.WebSockets         as WS
import qualified Network.Wai.Middleware.Static          as MS
import qualified Network.WebSockets                     as WS
import qualified Text.Mustache                          as M
import qualified Text.Mustache.Compile                  as M

import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Explain                 as GE
import qualified Hasura.GraphQL.Schema                  as GS
import qualified Hasura.GraphQL.Transport.HTTP          as GH
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Transport.WebSocket     as WS
import qualified Hasura.Logging                         as L

import           Hasura.Prelude                         hiding (get, put)
import           Hasura.RQL.DDL.Schema.Table
--import           Hasura.RQL.DML.Explain
import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.DML.QueryTemplate
import           Hasura.RQL.Types
import           Hasura.Server.Auth                     (AuthMode (..),
                                                         getUserInfo)
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Middleware               (corsMiddleware,
                                                         mkDefaultCorsPolicy)
import           Hasura.Server.Query
import           Hasura.Server.Utils
import           Hasura.Server.Version
import           Hasura.SQL.Types



consoleTmplt :: M.Template
consoleTmplt = $(M.embedSingleTemplate "src-rsr/console.html")

isAccessKeySet :: AuthMode -> T.Text
isAccessKeySet AMNoAuth = "false"
isAccessKeySet _        = "true"

mkConsoleHTML :: AuthMode -> IO T.Text
mkConsoleHTML authMode =
  bool (initErrExit errMsg) (return res) (null errs)
  where
    (errs, res) = M.checkedSubstitute consoleTmplt $
                   object [ "version" .= consoleVersion
                          , "isAccessKeySet" .= isAccessKeySet authMode
                          ]
    errMsg = "Fatal Error : console template rendering failed"
             ++ show errs

data ServerCtx
  = ServerCtx
  { scIsolation :: Q.TxIsolation
  , scPGPool    :: Q.PGPool
  , scLogger    :: L.Logger
  , scCacheRef  :: IORef (SchemaCache, GS.GCtxMap)
  , scCacheLock :: MVar ()
  , scAuthMode  :: AuthMode
  , scManager   :: HTTP.Manager
  }

data HandlerCtx
  = HandlerCtx
  { hcServerCtx :: ServerCtx
  , hcReqBody   :: BL.ByteString
  , hcUser      :: UserInfo
  }

type Handler = ExceptT QErr (ReaderT HandlerCtx IO)

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
  cache <- liftIO $ readIORef scRef
  return $ QCtx userInfo $ fst cache

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
  -> Handler BL.ByteString
  -> ActionT m ()
mkSpockAction qErrEncoder serverCtx handler = do
  req <- request
  reqBody <- liftIO $ strictRequestBody req
  let headers  = requestHeaders req
      authMode = scAuthMode serverCtx
      manager = scManager serverCtx

  userInfoE <- liftIO $ runExceptT $ getUserInfo logger manager headers authMode
  userInfo <- either (logAndThrow req reqBody False) return userInfoE

  let handlerState = HandlerCtx serverCtx reqBody userInfo

  t1 <- liftIO getCurrentTime -- for measuring response time purposes
  result <- liftIO $ runReaderT (runExceptT handler) handlerState
  t2 <- liftIO getCurrentTime -- for measuring response time purposes

  -- log result
  logResult (Just userInfo) req reqBody serverCtx result $ Just (t1, t2)
  either (qErrToResp $ userRole userInfo == adminRole) resToResp result

  where
    logger = scLogger serverCtx
    -- encode error response
    qErrToResp includeInternal qErr = do
      setStatus $ qeStatus qErr
      json $ qErrEncoder includeInternal qErr

    logAndThrow req reqBody includeInternal qErr = do
      logError Nothing req reqBody serverCtx qErr
      qErrToResp includeInternal qErr

    resToResp resp = do
      uncurry setHeader jsonHeader
      lazyBytes resp

withLock :: (MonadIO m, MonadError e m)
         => MVar () -> m a -> m a
withLock lk action = do
  acquireLock
  res <- action `catchError` onError
  releaseLock
  return res
  where
    onError e   = releaseLock >> throwError e
    acquireLock = liftIO $ takeMVar lk
    releaseLock = liftIO $ putMVar lk ()

v1QueryHandler :: RQLQuery -> Handler BL.ByteString
v1QueryHandler query = do
  lk <- scCacheLock . hcServerCtx <$> ask
  bool (fst <$> dbAction) (withLock lk dbActionReload) $
    queryNeedsReload query
  where
    -- Hit postgres
    dbAction = do
      userInfo <- asks hcUser
      scRef <- scCacheRef . hcServerCtx <$> ask
      schemaCache <- liftIO $ readIORef scRef
      httpMgr <- scManager . hcServerCtx <$> ask
      pool <- scPGPool . hcServerCtx <$> ask
      isoL <- scIsolation . hcServerCtx <$> ask
      runQuery pool isoL userInfo (fst schemaCache) httpMgr query

    -- Also update the schema cache
    dbActionReload = do
      (resp, newSc) <- dbAction
      scRef <- scCacheRef . hcServerCtx <$> ask
      httpMgr <- scManager . hcServerCtx <$> ask
      -- FIXME: should we be fetching the remote schema again? if not how do we get the remote schema?
      newGCtxMap <- GS.mkGCtxMap (scTables newSc)
      mergedGCtxMap <-
        mergeSchemas (scRemoteResolvers newSc) newGCtxMap httpMgr
      liftIO $ writeIORef scRef (newSc, mergedGCtxMap)
      return resp

v1Alpha1GQHandler :: GH.GraphQLRequest -> Handler BL.ByteString
v1Alpha1GQHandler query = do
  userInfo <- asks hcUser
  reqBody <- asks hcReqBody
  manager <- scManager . hcServerCtx <$> ask
  scRef <- scCacheRef . hcServerCtx <$> ask
  (_, gCtxMap) <- liftIO $ readIORef scRef
  pool <- scPGPool . hcServerCtx <$> ask
  isoL <- scIsolation . hcServerCtx <$> ask
  GH.runGQ pool isoL userInfo gCtxMap manager query reqBody

gqlExplainHandler :: GE.GQLExplain -> Handler BL.ByteString
gqlExplainHandler query = do
  onlyAdmin
  scRef <- scCacheRef . hcServerCtx <$> ask
  cache <- liftIO $ readIORef scRef
  pool <- scPGPool . hcServerCtx <$> ask
  isoL <- scIsolation . hcServerCtx <$> ask
  GE.explainGQLQuery pool isoL (snd cache) query

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

legacyQueryHandler :: TableName -> T.Text -> Handler BL.ByteString
legacyQueryHandler tn queryType =
  case M.lookup queryType queryParsers of
    Just queryParser -> getQueryParser queryParser qt >>= v1QueryHandler
    Nothing          -> throw404 "No such resource exists"
  where
    qt = QualifiedTable publicSchema tn


mkWaiApp
  :: Q.TxIsolation
  -> Maybe String
  -> L.LoggerCtx
  -> Q.PGPool
  -> HTTP.Manager
  -> AuthMode
  -> CorsConfig
  -> Bool
  -> IO (Wai.Application, IORef (SchemaCache, GS.GCtxMap))
mkWaiApp isoLevel mRootDir loggerCtx pool httpManager mode corsCfg enableConsole = do
    cacheRef <- do
      pgResp <- liftIO $ runExceptT $ Q.runTx pool (Q.Serializable, Nothing) $ do
        Q.catchE defaultTxErrorHandler initStateTx
        sc <- buildSchemaCache httpManager
        gCtxMap <- GS.mkGCtxMap (scTables sc)
        mergedGCtxMap <-
          mergeSchemas (scRemoteResolvers sc) gCtxMap httpManager
        return $ (,) sc mergedGCtxMap
      either initErrExit return pgResp >>= newIORef

    cacheLock <- newMVar ()

    let serverCtx =
          ServerCtx isoLevel pool (L.mkLogger loggerCtx) cacheRef
          cacheLock mode httpManager

    spockApp <- spockAsApp $ spockT id $
                httpApp mRootDir corsCfg serverCtx enableConsole

    let runTx tx = runExceptT $ Q.runTx pool (isoLevel, Nothing) tx

    wsServerEnv <- WS.createWSServerEnv (scLogger serverCtx) httpManager cacheRef runTx
    let wsServerApp = WS.createWSServerApp mode wsServerEnv
    return (WS.websocketsOr WS.defaultConnectionOptions wsServerApp spockApp, cacheRef)

httpApp :: Maybe String -> CorsConfig -> ServerCtx -> Bool -> SpockT IO ()
httpApp mRootDir corsCfg serverCtx enableConsole = do
    liftIO $ putStrLn "HasuraDB is now waiting for connections"

    -- cors middleware
    unless (ccDisabled corsCfg) $
      middleware $ corsMiddleware (mkDefaultCorsPolicy $ ccDomain corsCfg)

    -- API Console and Root Dir
    if enableConsole then do
      consoleHTML <- lift $ mkConsoleHTML $ scAuthMode serverCtx
      serveApiConsole consoleHTML
    else maybe (return ()) (middleware . MS.staticPolicy . MS.addBase) mRootDir

    get "v1/version" $ do
      uncurry setHeader jsonHeader
      lazyBytes $ encode $ object [ "version" .= currentVersion ]

    get    ("v1/template" <//> var) tmpltGetOrDeleteH
    post   ("v1/template" <//> var) tmpltPutOrPostH
    put    ("v1/template" <//> var) tmpltPutOrPostH
    delete ("v1/template" <//> var) tmpltGetOrDeleteH

    post "v1/query" $ mkSpockAction encodeQErr serverCtx $ do
      query <- parseBody
      v1QueryHandler query

    post "v1alpha1/graphql/explain" $ mkSpockAction encodeQErr serverCtx $ do
      expQuery <- parseBody
      gqlExplainHandler expQuery

    post "v1alpha1/graphql" $ mkSpockAction GH.encodeGQErr serverCtx $ do
      query <- parseBody
      v1Alpha1GQHandler query

    -- get "v1alpha1/graphql/schema" $
    --   mkSpockAction encodeQErr serverCtx v1Alpha1GQSchemaHandler

    post ("api/1/table" <//> var <//> var) $ \tableName queryType ->
      mkSpockAction encodeQErr serverCtx $
      legacyQueryHandler (TableName tableName) queryType

    hookAny GET $ \_ -> do
      let qErr = err404 NotFound "resource does not exist"
      req <- request
      reqBody <- liftIO $ strictRequestBody req
      logError Nothing req reqBody serverCtx qErr
      uncurry setHeader jsonHeader
      lazyBytes $ encode qErr

  where
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

    serveApiConsole htmlFile = do
      get root $ redirect "/console"
      get ("console" <//> wildcard) $ const $ html htmlFile
