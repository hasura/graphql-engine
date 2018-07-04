{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hasura.Server.App where

import           Control.Concurrent.MVar
import           Control.Exception             (try)
import           Control.Lens                  hiding ((.=))
import           Data.Char                     (isSpace)
import           Data.IORef

import           Crypto.Hash                   (Digest, SHA1, hash)
import           Data.Aeson                    hiding (json)
import qualified Data.ByteString.Lazy          as BL
import           Data.CaseInsensitive          (CI (..), original)
import qualified Data.HashMap.Strict           as M
import qualified Data.String.Conversions       as CS
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           Data.Time.Clock               (getCurrentTime)
import qualified Network.Connection            as NC
import qualified Network.HTTP.Client           as H
import qualified Network.HTTP.Client.TLS       as HT
import           Network.Wai                   (strictRequestBody)
import qualified Network.Wreq                  as Wq
import qualified Network.Wreq.Types            as WqT
import qualified Text.Mustache                 as M
import qualified Text.Mustache.Compile         as M

import           Web.Spock.Core

import qualified Network.HTTP.Types            as N
import qualified Network.Wai.Internal          as WI
import qualified Network.Wai.Middleware.Static as MS

import qualified Data.Text.Encoding.Error      as TE
import qualified Database.PG.Query             as Q
import qualified Hasura.GraphQL.Execute        as GE
import qualified Hasura.GraphQL.Execute.Result as GE
import qualified Hasura.GraphQL.Schema         as GS

import           Hasura.Prelude                hiding (get, put)
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DML.Explain
import           Hasura.RQL.DML.QueryTemplate
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Logging
import           Hasura.Server.Middleware      (corsMiddleware,
                                                mkDefaultCorsPolicy)
import           Hasura.Server.Query
import           Hasura.Server.Utils
import           Hasura.Server.Version
import           Hasura.SQL.Types

type RavenLogger = ServerLogger (BL.ByteString, Either QErr BL.ByteString)

consoleTmplt :: M.Template
consoleTmplt = $(M.embedSingleTemplate "src-rsr/console.html")

mkConsoleHTML :: IO T.Text
mkConsoleHTML =
  bool (initErrExit errMsg) (return res) (null errs)
  where
    (errs, res) = M.checkedSubstitute consoleTmplt $
                   object ["version" .= consoleVersion]
    errMsg = "Fatal Error : console template rendering failed"
             ++ show errs

ravenLogGen :: LogDetailG (BL.ByteString, Either QErr BL.ByteString)
ravenLogGen _ (reqBody, res) =

  (status, toJSON <$> logDetail, Just qh, Just size)
  where
    status = either qeStatus (const N.status200) res
    logDetail = either (Just . qErrToLogDetail) (const Nothing) res
    reqBodyTxt = TL.filter (not . isSpace) $ decodeLBS reqBody
    qErrToLogDetail qErr =
      LogDetail reqBodyTxt $ toJSON qErr
    size = BL.length $ either encode id res
    qh = T.pack . show $ sha1 reqBody
    sha1 :: BL.ByteString -> Digest SHA1
    sha1 = hash . BL.toStrict

decodeLBS :: BL.ByteString -> TL.Text
decodeLBS = TLE.decodeUtf8With TE.lenientDecode

data AuthMode
  = AMNoAuth
  | AMAccessKey !T.Text
  | AMAccessKeyAndHook !T.Text !T.Text
  deriving (Show, Eq)

data ServerCtx
  = ServerCtx
  { scIsolation  :: Q.TxIsolation
  , scPGPool     :: Q.PGPool
  , scLogger     :: RavenLogger
  , scCacheRef   :: IORef (SchemaCache, GS.GCtxMap)
  , scCacheLock  :: MVar ()
  , scServerMode :: AuthMode
  }

data HandlerCtx
  = HandlerCtx
  { hcServerCtx :: ServerCtx
  , hcReqBody   :: BL.ByteString
  , hcHeaders   :: [(T.Text, T.Text)]
  }

type Handler = ExceptT QErr (ReaderT HandlerCtx IO)

{-# SCC parseBody #-}
parseBody :: (FromJSON a) => Handler a
parseBody = do
  reqBody <- hcReqBody <$> ask
  case decode' reqBody of
    Just jVal -> decodeValue jVal
    Nothing   -> throw400 InvalidJSON "invalid json"

filterHeaders :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
filterHeaders hdrs = flip filter hdrs $ \(h, _) ->
  isXHasuraTxt h && (T.toLower h /= userRoleHeader)
  && (T.toLower h /= accessKeyHeader)

parseUserInfo :: Handler UserInfo
parseUserInfo = do
  headers <- hcHeaders <$> ask
  let mUserRoleTuple = flip find headers $ \hdr ->
        userRoleHeader == T.toLower (fst hdr)
      mUserRoleV = snd <$> mUserRoleTuple
      userRoleV = fromMaybe "admin" mUserRoleV
  return $ UserInfo (RoleName userRoleV) $ filterHeaders headers

onlyAdmin :: Handler ()
onlyAdmin = do
  (UserInfo uRole _) <- parseUserInfo
  when (uRole /= adminRole) $
    throw400 AccessDenied "You have to be an admin to access this endpoint"

buildQCtx ::  Handler QCtx
buildQCtx = do
  scRef    <- scCacheRef . hcServerCtx <$> ask
  userInfo <- parseUserInfo
  cache <- liftIO $ readIORef scRef
  return $ QCtx userInfo $ fst cache

httpToQErr :: H.HttpException -> QErr
httpToQErr e = case e of
  H.InvalidUrlException _ _ -> err500 Unexpected "Invalid Webhook Url"
  H.HttpExceptionRequest _ H.ConnectionTimeout -> err500 Unexpected
    "Webhook : Connection timeout"
  H.HttpExceptionRequest _ H.ResponseTimeout -> err500 Unexpected
    "Webhook : Response timeout"
  _ -> err500 Unexpected "HTTP Exception from Webhook"

fromWebHook
  :: (MonadIO m)
  => T.Text
  -> [N.Header]
  -> ExceptT QErr m [(T.Text, T.Text)]
fromWebHook urlT reqHeaders = do
  manager <- liftIO $
    H.newManager $ HT.mkManagerSettings tlsSimple Nothing
  let options = Wq.defaults
        { WqT.headers = filteredHeaders
        , WqT.checkResponse = Just (\_ _ -> return ())
        , WqT.manager = Right manager
        }
  respWithExcept <- liftIO $ try $ Wq.getWith options $ T.unpack urlT
  resp <- either (throwError . httpToQErr) return respWithExcept
  let status = resp ^. Wq.responseStatus
  validateStatus status
  webHookResp <- decodeBS $ resp ^. Wq.responseBody
  return $ M.toList webHookResp

  where
    filteredHeaders = flip filter reqHeaders $ \(n, _) ->
      n /= "Content-Length" && n /= "User-Agent" && n /= "Host"
      && n /= "Origin" && n /= "Referer"
    tlsSimple = NC.TLSSettingsSimple True False False

    validateStatus statusCode
      | statusCode == N.status200 = return ()
      | statusCode == N.status401 = throw401
         "Authentication hook unauthorized this request"
      | otherwise = throw500
        "Invalid response from authorization hook"

    decodeBS bs = case eitherDecode bs of
      Left e -> throw500 $
        "Invalid response from authorization hook; " <> T.pack e
      Right a -> return a

fetchHeaders
  :: (MonadIO m)
  => WI.Request
  -> Maybe T.Text
  -> AuthMode
  ->  ExceptT QErr m [(T.Text, T.Text)]
fetchHeaders req mReqAccessKey authMode =
  case authMode of
    AMNoAuth -> return headers

    AMAccessKey accKey -> do
      reqAccessKey <- maybe accessKeyAuthErr return mReqAccessKey
      validateKeyAndReturnHeaders accKey reqAccessKey

    AMAccessKeyAndHook accKey hook ->
      maybe (fromWebHook hook rawHeaders)
            (validateKeyAndReturnHeaders accKey)
            mReqAccessKey
  where
    rawHeaders = WI.requestHeaders req
    headers = headersTxt rawHeaders

    validateKeyAndReturnHeaders key reqKey = do
      when (reqKey /= key) accessKeyAuthErr
      return headers

    accessKeyAuthErr = throw400 AccessDenied $
          "access keys don't match or not found"

    headersTxt hdrsRaw =
      flip map hdrsRaw $ \(hdrName, hdrVal) ->
      (CS.cs $ original hdrName, CS.cs hdrVal)

mkSpockAction
  :: (MonadIO m)
  => (T.Text -> QErr -> Value)
  -> ServerCtx
  -> Handler BL.ByteString
  -> ActionT m ()
mkSpockAction qErrEncoder serverCtx handler = do
  req <- request
  reqBody <- liftIO $ strictRequestBody req
  role <- fromMaybe "admin" <$> header userRoleHeader

  accKeyHeader <- header accessKeyHeader
  headersRes <- runExceptT $
    fetchHeaders req accKeyHeader $ scServerMode serverCtx
  headers <- either (qErrToResp role) return headersRes

  let handlerState = HandlerCtx serverCtx reqBody headers

  t1 <- liftIO getCurrentTime -- for measuring response time purposes
  result <- liftIO $ runReaderT (runExceptT handler) handlerState
  t2 <- liftIO getCurrentTime -- for measuring response time purposes

  liftIO $ logger req (reqBody, result) $ Just (t1, t2)
  either (qErrToResp role) resToResp result
  where
    logger = scLogger serverCtx

    -- encode error response
    qErrToResp mRole qErr = do
      setStatus $ qeStatus qErr
      json $ qErrEncoder mRole qErr

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

v1ExplainHandler :: RQLExplain -> Handler BL.ByteString
v1ExplainHandler expQuery = dbAction
  where
    dbAction = do
      onlyAdmin
      scRef <- scCacheRef . hcServerCtx <$> ask
      schemaCache <- liftIO $ readIORef scRef
      pool <- scPGPool . hcServerCtx <$> ask
      isoL <- scIsolation . hcServerCtx <$> ask
      runExplainQuery pool isoL userInfo (fst schemaCache) selectQ

    selectQ = rqleQuery expQuery
    role = rqleRole expQuery
    headers = M.toList $ rqleHeaders expQuery
    userInfo = UserInfo role headers

v1QueryHandler :: RQLQuery -> Handler BL.ByteString
v1QueryHandler query = do
  lk <- scCacheLock . hcServerCtx <$> ask
  bool (fst <$> dbAction) (withLock lk dbActionReload) $
    queryNeedsReload query
  where
    -- Hit postgres
    dbAction = do
      userInfo <- parseUserInfo
      scRef <- scCacheRef . hcServerCtx <$> ask
      schemaCache <- liftIO $ readIORef scRef
      pool <- scPGPool . hcServerCtx <$> ask
      isoL <- scIsolation . hcServerCtx <$> ask
      runQuery pool isoL userInfo (fst schemaCache) query

    -- Also update the schema cache
    dbActionReload = do
      (resp, newSc) <- dbAction
      newGCtxMap <- GS.mkGCtxMap $ scTables newSc
      scRef <- scCacheRef . hcServerCtx <$> ask
      liftIO $ writeIORef scRef (newSc, newGCtxMap)
      return resp

v1Alpha1GQHandler :: GE.GraphQLRequest -> Handler BL.ByteString
v1Alpha1GQHandler query = do
  userInfo <- parseUserInfo
  scRef <- scCacheRef . hcServerCtx <$> ask
  cache <- liftIO $ readIORef scRef
  pool <- scPGPool . hcServerCtx <$> ask
  isoL <- scIsolation . hcServerCtx <$> ask
  GE.runGQ pool isoL userInfo (snd cache) query

-- v1Alpha1GQSchemaHandler :: Handler BL.ByteString
-- v1Alpha1GQSchemaHandler = do
--   scRef <- scCacheRef . hcServerCtx <$> ask
--   schemaCache <- liftIO $ readIORef scRef
--   onlyAdmin
--   GS.generateGSchemaH schemaCache

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
    Nothing          -> throw404 NotFound "No such resource exists"
  where
    qt = QualifiedTable publicSchema tn

app
  :: Q.TxIsolation
  -> Maybe String
  -> RavenLogger
  -> Q.PGPool
  -> AuthMode
  -> CorsConfig
  -> Bool
  -> SpockT IO ()
app isoLevel mRootDir logger pool mode corsCfg enableConsole = do
    cacheRef <- lift $ do
      pgResp <- liftIO $ runExceptT $ Q.runTx pool (Q.Serializable, Nothing) $ do
        Q.catchE defaultTxErrorHandler initStateTx
        sc <- buildSchemaCache
        (,) sc <$> GS.mkGCtxMap (scTables sc)
      either initErrExit return pgResp >>= newIORef

    cacheLock <- lift $ newMVar ()

    let serverCtx = ServerCtx isoLevel pool logger cacheRef cacheLock mode

    liftIO $ putStrLn "HasuraDB is now waiting for connections"

    -- cors middleware
    unless (ccDisabled corsCfg) $
      middleware $ corsMiddleware (mkDefaultCorsPolicy $ ccDomain corsCfg)

    -- API Console and Root Dir
    if enableConsole then do
      consoleHTML <- lift mkConsoleHTML
      serveApiConsole consoleHTML
    else maybe (return ()) (middleware . MS.staticPolicy . MS.addBase) mRootDir

    get "v1/version" getVersion

    get    ("v1/template" <//> var) $ tmpltGetOrDeleteH serverCtx
    post   ("v1/template" <//> var) $ tmpltPutOrPostH serverCtx
    put    ("v1/template" <//> var) $ tmpltPutOrPostH serverCtx
    delete ("v1/template" <//> var) $ tmpltGetOrDeleteH serverCtx

    post "v1/query" $ mkSpockAction encodeQErr serverCtx $ do
      query <- parseBody
      v1QueryHandler query

    post "v1/query/explain" $ mkSpockAction encodeQErr serverCtx $ do
      expQuery <- parseBody
      v1ExplainHandler expQuery

    post "v1alpha1/graphql" $ mkSpockAction GE.encodeGQErr serverCtx $ do
      query <- parseBody
      v1Alpha1GQHandler query

    -- get "v1alpha1/graphql/schema" $
    --   mkSpockAction encodeQErr serverCtx v1Alpha1GQSchemaHandler

    post ("api/1/table" <//> var <//> var) $ \tableName queryType ->
      mkSpockAction encodeQErr serverCtx $ legacyQueryHandler (TableName tableName) queryType

    hookAny GET $ \_ -> mkSpockAction encodeQErr serverCtx $
      throw404 NotFound "resource does not exist"

  where
    tmpltGetOrDeleteH serverCtx tmpltName = do
      tmpltArgs <- tmpltArgsFromQueryParams
      mkSpockAction encodeQErr serverCtx $ mkQTemplateAction tmpltName tmpltArgs

    tmpltPutOrPostH serverCtx tmpltName = do
      tmpltArgs <- tmpltArgsFromQueryParams
      mkSpockAction encodeQErr serverCtx $ do
        bodyTmpltArgs <- parseBody
        mkQTemplateAction tmpltName $ M.union bodyTmpltArgs tmpltArgs

    tmpltArgsFromQueryParams = do
      qparams <- params
      return $ M.fromList $ flip map qparams $
        \(a, b) -> (TemplateParam a, String b)

    mkQTemplateAction tmpltName tmpltArgs =
      v1QueryHandler $ RQExecuteQueryTemplate $
      ExecQueryTemplate (TQueryName tmpltName) tmpltArgs

    serveApiConsole htmlFile = do
      get root $ redirect "/console"
      get ("console" <//> wildcard) $ const $ html htmlFile
