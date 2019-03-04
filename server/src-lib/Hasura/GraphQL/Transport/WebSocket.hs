{-# LANGUAGE RankNTypes #-}

module Hasura.GraphQL.Transport.WebSocket
  ( createWSServerApp
  , createWSServerEnv
  ) where

import qualified Control.Concurrent.Async                    as A
import qualified Control.Concurrent.STM                      as STM
import qualified Data.Aeson                                  as J
import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.TH                               as J
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.CaseInsensitive                        as CI
import qualified Data.HashMap.Strict                         as Map
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as TE
import qualified Language.GraphQL.Draft.Syntax               as G
import qualified ListT
import qualified Network.HTTP.Client                         as H
import qualified Network.HTTP.Types                          as H
import qualified Network.WebSockets                          as WS
import qualified StmContainers.Map                           as STMMap

import           Control.Concurrent                          (threadDelay)
import           Data.ByteString                             (ByteString)
import qualified Data.IORef                                  as IORef

import           Hasura.EncJSON
import qualified Hasura.GraphQL.Execute                      as E
import           Hasura.GraphQL.Resolve.Context              (LazyRespTx)
import qualified Hasura.GraphQL.Resolve.LiveQuery            as LQ
import           Hasura.GraphQL.Schema                       (getGCtx)
import qualified Hasura.GraphQL.Transport.HTTP               as TH
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import qualified Hasura.GraphQL.Transport.WebSocket.Server   as WS
import qualified Hasura.Logging                              as L
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth                          (AuthMode,
                                                              getUserInfo)
import           Hasura.Server.Cors
import           Hasura.Server.Utils                         (bsToTxt)

-- uniquely identifies an operation
type GOperationId = (WS.WSId, OperationId)

type TxRunner = LazyRespTx -> IO (Either QErr EncJSON)

type OperationMap
  = STMMap.Map OperationId LQ.LiveQuery

newtype WsHeaders
  = WsHeaders { unWsHeaders :: [H.Header] }
  deriving (Show, Eq)

data WSConnState
  -- headers from the client for websockets
  = CSNotInitialised !WsHeaders
  | CSInitError Text
  -- headers from the client (in conn params) to forward to the remote schema
  | CSInitialised UserInfo [H.Header]

data WSConnData
  = WSConnData
  -- the role and headers are set only on connection_init message
  { _wscUser  :: !(IORef.IORef WSConnState)
  -- we only care about subscriptions,
  -- the other operations (query/mutations)
  -- are not tracked here
  , _wscOpMap :: !OperationMap
  }

type LiveQueryMap = LQ.LiveQueryMap GOperationId
type WSServer = WS.WSServer WSConnData

type WSConn = WS.WSConn WSConnData
sendMsg :: (MonadIO m) => WSConn -> ServerMsg -> m ()
sendMsg wsConn =
  liftIO . WS.sendMsg wsConn . encJToLBS . encodeServerMsg

data OpDetail
  = ODStarted
  | ODProtoErr !Text
  | ODQueryErr !QErr
  | ODCompleted
  | ODStopped
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 2
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''OpDetail)

data WSEvent
  = EAccepted
  | ERejected !QErr
  | EConnErr !ConnErrMsg
  | EOperation !OperationId !(Maybe OperationName) !OpDetail
  | EClosed
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 1
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''WSEvent)

data WSLog
  = WSLog
  { _wslWebsocketId :: !WS.WSId
  , _wslUser        :: !(Maybe UserVars)
  , _wslEvent       :: !WSEvent
  , _wslMsg         :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''WSLog)

instance L.ToEngineLog WSLog where
  toEngineLog wsLog =
    (L.LevelInfo, "ws-handler", J.toJSON wsLog)

data WSServerEnv
  = WSServerEnv
  { _wseLogger     :: !L.Logger
  , _wseServer     :: !WSServer
  , _wseRunTx      :: !TxRunner
  , _wseLiveQMap   :: !LiveQueryMap
  , _wseGCtxMap    :: !(IORef.IORef SchemaCache)
  , _wseHManager   :: !H.Manager
  , _wseQueryCache :: !E.QueryCache
  , _wseCorsPolicy :: !CorsPolicy
  , _wseSQLCtx     :: !SQLGenCtx
  }

onConn :: L.Logger -> CorsPolicy -> WS.OnConnH WSConnData
onConn (L.Logger logger) corsPolicy wsId requestHead = do
  res <- runExceptT $ do
    checkPath
    let reqHdrs = WS.requestHeaders requestHead
    headers <- maybe (return reqHdrs) (flip enforceCors reqHdrs . snd) getOrigin
    return $ WsHeaders $ filterWsHeaders headers
  either reject accept res

  where
    keepAliveAction wsConn = forever $ do
      sendMsg wsConn SMConnKeepAlive
      threadDelay $ 5 * 1000 * 1000

    accept hdrs = do
      logger $ WSLog wsId Nothing EAccepted Nothing
      connData <- WSConnData
                  <$> IORef.newIORef (CSNotInitialised hdrs)
                  <*> STMMap.newIO
      let acceptRequest = WS.defaultAcceptRequest
                          { WS.acceptSubprotocol = Just "graphql-ws"}
      return $ Right (connData, acceptRequest, Just keepAliveAction)

    reject qErr = do
      logger $ WSLog wsId Nothing (ERejected qErr) Nothing
      return $ Left $ WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (BL.toStrict $ J.encode $ encodeGQLErr False qErr)

    checkPath =
      when (WS.requestPath requestHead /= "/v1alpha1/graphql") $
      throw404 "only /v1alpha1/graphql is supported on websockets"

    getOrigin =
      find ((==) "Origin" . fst) (WS.requestHeaders requestHead)

    enforceCors :: ByteString -> [H.Header] -> ExceptT QErr IO [H.Header]
    enforceCors origin reqHdrs = case cpConfig corsPolicy of
      CCAllowAll -> return reqHdrs
      CCDisabled readCookie ->
        if readCookie
        then return reqHdrs
        else do
          liftIO $ logger $ WSLog wsId Nothing EAccepted (Just corsNote)
          return $ filter (\h -> fst h /= "Cookie") reqHdrs
      CCAllowedOrigins ds
        -- if the origin is in our cors domains, no error
        | bsToTxt origin `elem` dmFqdns ds   -> return reqHdrs
        -- if current origin is part of wildcard domain list, no error
        | inWildcardList ds (bsToTxt origin) -> return reqHdrs
        -- otherwise error
        | otherwise                          -> corsErr

    filterWsHeaders hdrs = flip filter hdrs $ \(n, _) ->
      n `notElem` [ "sec-websocket-key"
                  , "sec-websocket-version"
                  , "upgrade"
                  , "connection"
                  ]

    corsErr = throw400 AccessDenied
              "received origin header does not match configured CORS domains"

    corsNote = "Cookie is not read when CORS is disabled, because it is a potential "
            <> "security issue. If you're already handling CORS before Hasura and enforcing "
            <> "CORS on websocket connections, then you can use the flag --ws-read-cookie or "
            <> "HASURA_GRAPHQL_WS_READ_COOKIE to force read cookie when CORS is disabled."


onStart :: WSServerEnv -> WSConn -> StartMsg -> BL.ByteString -> IO ()
onStart serverEnv wsConn (StartMsg opId q) msgRaw = catchAndIgnore $ do

  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  when (isJust opM) $ withComplete $ sendConnErr $
    "an operation already exists with this id: " <> unOperationId opId

  userInfoM <- liftIO $ IORef.readIORef userInfoR
  (userInfo, reqHdrs) <- case userInfoM of
    CSInitialised userInfo reqHdrs -> return (userInfo, reqHdrs)
    CSInitError initErr -> do
      let connErr = "cannot start as connection_init failed with : " <> initErr
      withComplete $ sendConnErr connErr
    CSNotInitialised _ -> do
      let connErr = "start received before the connection is initialised"
      withComplete $ sendConnErr connErr

  -- validate and build tx
  sc <- liftIO $ IORef.readIORef gCtxMapRef

  gCtx <- fmap fst $ flip runStateT sc $
          getGCtx (userRole userInfo) (scGCtxMap sc)

  let schemaVer = scVersion sc

  execPlanE <- runExceptT $ E.getGQExecPlan queryCache
               userInfo schemaVer gCtx sqlGenCtx q
  (q', execPlan)  <- either (withComplete . preExecErr) return execPlanE

  case execPlan of
    E.GExPHasura opTy tx   -> runHasuraQ q' userInfo opTy tx
    E.GExPRemote rsi opDef -> do
      when (G._todType opDef == G.OperationTypeSubscription) $ withComplete $
        sendConnErr "subscription to remote server is not supported"
      resp <- runExceptT $ TH.runRemoteGQ httpMgr userInfo reqHdrs
              msgRaw rsi opDef
      either postExecErr sendSuccResp resp
      sendCompleted

  where
    runHasuraQ q' userInfo opTy tx = do
      let qTx = withUserInfo userInfo $ liftTx tx
      case opTy of
        G.OperationTypeSubscription -> do
          let lq = LQ.LiveQuery userInfo q'
          liftIO $ STM.atomically $ STMMap.insert lq opId opMap
          liftIO $ LQ.addLiveQuery runTx lqMap lq
            qTx (wsId, opId) liveQOnChange
          logOpEv ODStarted
        _ ->  do
          logOpEv ODStarted
          resp <- liftIO $ runTx qTx
          either postExecErr sendSuccResp resp
          sendCompleted

    WSServerEnv logger _ runTx lqMap gCtxMapRef
      httpMgr queryCache _ sqlGenCtx = serverEnv

    wsId = WS.getWSId wsConn
    WSConnData userInfoR opMap = WS.getData wsConn

    logOpEv opDet =
      logWSEvent logger wsConn $ EOperation opId (_grOperationName q) opDet

    sendConnErr connErr = do
      sendMsg wsConn $ SMErr $ ErrorMsg opId $ J.toJSON connErr
      logOpEv $ ODProtoErr connErr

    sendCompleted = do
      sendMsg wsConn $ SMComplete $ CompletionMsg opId
      logOpEv ODCompleted

    postExecErr qErr = do
      logOpEv $ ODQueryErr qErr
      sendMsg wsConn $ SMData $ DataMsg opId $ encJToLBS $
        encodeGQResp $ GQExecError $ pure $ encodeQErr False qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr qErr = do
      logOpEv $ ODQueryErr qErr
      sendMsg wsConn $ SMErr $ ErrorMsg opId $ encodeQErr False qErr

    sendSuccResp bs =
      sendMsg wsConn $ SMData $ DataMsg opId $ encJToLBS $
        encodeGQResp $ GQSuccess bs

    withComplete :: ExceptT () IO () -> ExceptT () IO a
    withComplete action = do
      action
      sendCompleted
      throwError ()

    -- on change, send message on the websocket
    liveQOnChange resp =
      WS.sendMsg wsConn $ encJToLBS $ encodeServerMsg $
      SMData $ DataMsg opId resp

    catchAndIgnore :: ExceptT () IO () -> IO ()
    catchAndIgnore m = void $ runExceptT m

onMessage
  :: AuthMode
  -> WSServerEnv
  -> WSConn -> BL.ByteString -> IO ()
onMessage authMode serverEnv wsConn msgRaw =
  case J.eitherDecode msgRaw of
    Left e    -> do
      let err = ConnErrMsg $ "parsing ClientMessage failed: " <> T.pack e
      logWSEvent logger wsConn $ EConnErr err
      sendMsg wsConn $ SMConnErr err

    Right msg -> case msg of
      CMConnInit params -> onConnInit (_wseLogger serverEnv)
                           (_wseHManager serverEnv)
                           wsConn authMode params
      CMStart startMsg  -> onStart serverEnv wsConn startMsg msgRaw
      CMStop stopMsg    -> onStop serverEnv wsConn stopMsg
      CMConnTerm        -> WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = _wseLogger serverEnv

onStop :: WSServerEnv -> WSConn -> StopMsg -> IO ()
onStop serverEnv wsConn (StopMsg opId) = do
  -- probably wrap the whole thing in a single tx?
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  case opM of
    Just liveQ -> do
      let opNameM = _grOperationName $ LQ._lqRequest liveQ
      logWSEvent logger wsConn $ EOperation opId opNameM ODStopped
      LQ.removeLiveQuery lqMap liveQ (wsId, opId)
    Nothing    -> return ()
  STM.atomically $ STMMap.delete opId opMap
  where
    logger = _wseLogger serverEnv
    lqMap  = _wseLiveQMap serverEnv
    wsId   = WS.getWSId wsConn
    opMap  = _wscOpMap $ WS.getData wsConn

logWSEvent
  :: (MonadIO m)
  => L.Logger -> WSConn -> WSEvent -> m ()
logWSEvent (L.Logger logger) wsConn wsEv = do
  userInfoME <- liftIO $ IORef.readIORef userInfoR
  let userInfoM = case userInfoME of
        CSInitialised userInfo _ -> return $ userVars userInfo
        _                        -> Nothing
  liftIO $ logger $ WSLog wsId userInfoM wsEv Nothing
  where
    WSConnData userInfoR _ = WS.getData wsConn
    wsId = WS.getWSId wsConn

onConnInit
  :: (MonadIO m)
  => L.Logger -> H.Manager -> WSConn -> AuthMode -> Maybe ConnParams -> m ()
onConnInit logger manager wsConn authMode connParamsM = do
  headers <- mkHeaders <$> liftIO (IORef.readIORef (_wscUser $ WS.getData wsConn))
  res <- runExceptT $ getUserInfo logger manager headers authMode
  case res of
    Left e  -> do
      liftIO $ IORef.writeIORef (_wscUser $ WS.getData wsConn) $
        CSInitError $ qeError e
      let connErr = ConnErrMsg $ qeError e
      logWSEvent logger wsConn $ EConnErr connErr
      sendMsg wsConn $ SMConnErr connErr
    Right userInfo -> do
      liftIO $ IORef.writeIORef (_wscUser $ WS.getData wsConn) $
        CSInitialised userInfo paramHeaders
      sendMsg wsConn SMConnAck
      -- TODO: send it periodically? Why doesn't apollo's protocol use
      -- ping/pong frames of websocket spec?
      sendMsg wsConn SMConnKeepAlive
  where
    mkHeaders st =
      paramHeaders ++ getClientHdrs st

    paramHeaders =
      [ (CI.mk $ TE.encodeUtf8 h, TE.encodeUtf8 v)
      | (h, v) <- maybe [] Map.toList $ connParamsM >>= _cpHeaders
      ]

    getClientHdrs st = case st of
      CSNotInitialised h -> unWsHeaders h
      _                  -> []

onClose
  :: L.Logger
  -> LiveQueryMap
  -> WS.ConnectionException
  -> WSConn
  -> IO ()
onClose logger lqMap _ wsConn = do
  logWSEvent logger wsConn EClosed
  operations <- STM.atomically $ ListT.toList $ STMMap.listT opMap
  void $ A.forConcurrently operations $ \(opId, liveQ) ->
    LQ.removeLiveQuery lqMap liveQ (wsId, opId)
  where
    wsId  = WS.getWSId wsConn
    opMap = _wscOpMap $ WS.getData wsConn

createWSServerEnv
  :: L.Logger
  -> H.Manager
  -> E.QueryCache
  -> SQLGenCtx
  -> IORef.IORef SchemaCache
  -> TxRunner -> CorsPolicy -> IO WSServerEnv
createWSServerEnv logger httpManager queryCache sqlGenCtx
  cacheRef runTx corsPolicy = do
  (wsServer, lqMap) <-
    STM.atomically $ (,) <$> WS.createWSServer logger <*> LQ.newLiveQueryMap
  return $ WSServerEnv logger wsServer runTx lqMap
    cacheRef httpManager queryCache corsPolicy sqlGenCtx

createWSServerApp :: AuthMode -> WSServerEnv -> WS.ServerApp
createWSServerApp authMode serverEnv =
  WS.createServerApp (_wseServer serverEnv) handlers
  where
    handlers =
      WS.WSHandlers
      (onConn (_wseLogger serverEnv) (_wseCorsPolicy serverEnv))
      (onMessage authMode serverEnv)
      (onClose (_wseLogger serverEnv) $ _wseLiveQMap serverEnv)
