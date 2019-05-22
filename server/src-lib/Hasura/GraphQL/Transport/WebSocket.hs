{-# LANGUAGE RankNTypes #-}

module Hasura.GraphQL.Transport.WebSocket
  ( createWSServerApp
  , createWSServerEnv
  , WSServerEnv
  ) where

import           Control.Concurrent                            (threadDelay)
import           Data.ByteString                               (ByteString)

import qualified Control.Concurrent.Async                      as A
import qualified Control.Concurrent.STM                        as STM
import qualified Data.Aeson                                    as J
import qualified Data.ByteString.Lazy                          as BL
import qualified Data.CaseInsensitive                          as CI
import qualified Data.HashMap.Strict                           as Map
import qualified Data.IORef                                    as IORef
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as TE
import qualified Data.Time.Clock                               as TC
import qualified Language.GraphQL.Draft.Syntax                 as G
import qualified ListT
import qualified Network.HTTP.Client                           as H
import qualified Network.HTTP.Types                            as H
import qualified Network.WebSockets                            as WS
import qualified StmContainers.Map                             as STMMap

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Connection
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Error                        (Code (StartFailed))
import           Hasura.Server.Auth                            (AuthMode, getUserInfoWithExpTime)
import           Hasura.Server.Cors
import           Hasura.Server.Utils                           (bsToTxt,
                                                                diffTimeToMicro)

import qualified Hasura.GraphQL.Execute                        as E
import qualified Hasura.GraphQL.Execute.LiveQuery              as LQ
import qualified Hasura.GraphQL.Transport.WebSocket.Client     as WS
import qualified Hasura.GraphQL.Transport.WebSocket.Server     as WS
import qualified Hasura.Logging                                as L

type OperationMap
  = STMMap.Map OperationId SubscriptionOperation --(LQ.LiveQueryId, Maybe OperationName)

data SubscriptionOperation
  = SOHasura !(LQ.LiveQueryId, Maybe OperationName)
  | SORemote !RemoteOperation
  --deriving (Show, Eq)

data WSConnData
  = WSConnData
  -- the role and headers are set only on connection_init message
  { _wscState     :: !(STM.TVar WSConnState)
  -- we only care about subscriptions,
  -- the other operations (query/mutations)
  -- are not tracked here
  , _wscOpMap     :: !OperationMap
  , _wscErrRespTy :: !ErrRespType
  }

data ErrRespType
  = ERTLegacy
  | ERTGraphqlCompliant
  deriving (Show)

type WSServer = WS.WSServer WSConnData

type WSConn = WS.WSConn WSConnData

sendMsg :: (MonadIO m) => WSConn -> ServerMsg -> m ()
sendMsg wsConn =
  liftIO . WS.sendMsg wsConn . encodeServerMsg

data WSServerEnv
  = WSServerEnv
  { _wseLogger          :: !L.Logger
  , _wseRunTx           :: !PGExecCtx
  , _wseLiveQMap        :: !LQ.LiveQueriesState
  , _wseGCtxMap         :: !(IORef.IORef (SchemaCache, SchemaCacheVer))
  , _wseHManager        :: !H.Manager
  , _wseCorsPolicy      :: !CorsPolicy
  , _wseSQLCtx          :: !SQLGenCtx
  , _wseQueryCache      :: !E.PlanCache
  , _wseServer          :: !WSServer
  , _wseEnableAllowlist :: !Bool
  }

onConn :: L.Logger -> CorsPolicy -> WS.OnConnH WSConnData
onConn (L.Logger logger) corsPolicy wsId requestHead = do
  res <- runExceptT $ do
    errType <- checkPath
    let reqHdrs = WS.requestHeaders requestHead
    headers <- maybe (return reqHdrs) (flip enforceCors reqHdrs . snd) getOrigin
    return (WsHeaders $ filterWsHeaders headers, errType)
  either reject (uncurry accept) res

  where
    keepAliveAction wsConn = forever $ do
      sendMsg wsConn SMConnKeepAlive
      threadDelay $ 5 * 1000 * 1000

    jwtExpiryHandler wsConn = do
      expTime <- STM.atomically $ do
        connState <- STM.readTVar $ (_wscState . WS.getData) wsConn
        case connState of
          CSNotInitialised _         -> STM.retry
          CSInitError _              -> STM.retry
          CSInitialised (ConnInitState _ _ _ expTimeM) ->
            maybe STM.retry return expTimeM
      currTime <- TC.getCurrentTime
      threadDelay $ diffTimeToMicro $ TC.diffUTCTime expTime currTime

    accept hdrs errType = do
      logger $ WSLog wsId Nothing EAccepted Nothing Nothing
      connData <- WSConnData
                  <$> STM.newTVarIO (CSNotInitialised hdrs)
                  <*> STMMap.newIO
                  <*> pure errType
      let acceptRequest = WS.defaultAcceptRequest
                          { WS.acceptSubprotocol = Just "graphql-ws"}
      return $ Right $ WS.AcceptWith connData acceptRequest
                       (Just keepAliveAction) (Just jwtExpiryHandler)

    reject qErr = do
      logger $ WSLog wsId Nothing (ERejected qErr) Nothing Nothing
      return $ Left $ WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (BL.toStrict $ J.encode $ encodeGQLErr False qErr)

    checkPath = case WS.requestPath requestHead of
      "/v1alpha1/graphql" -> return ERTLegacy
      "/v1/graphql"       -> return ERTGraphqlCompliant
      _                   ->
        throw404 "only '/v1/graphql', '/v1alpha1/graphql' are supported on websockets"

    getOrigin =
      find ((==) "Origin" . fst) (WS.requestHeaders requestHead)

    enforceCors :: ByteString -> [H.Header] -> ExceptT QErr IO [H.Header]
    enforceCors origin reqHdrs = case cpConfig corsPolicy of
      CCAllowAll -> return reqHdrs
      CCDisabled readCookie ->
        if readCookie
        then return reqHdrs
        else do
          liftIO $ logger $ WSLog wsId Nothing EAccepted (Just corsNote) Nothing
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



onStart :: WSServerEnv -> WSConn -> StartMsg -> IO ()
onStart serverEnv wsConn msg@(StartMsg opId q) = catchAndIgnore $ do

  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  when (isJust opM) $ withComplete $ sendStartErr $
    "an operation already exists with this id: " <> unOperationId opId

  userInfoM <- liftIO $ STM.readTVarIO userInfoR
  (userInfo, reqHdrs, _) <- case userInfoM of
    CSInitialised (ConnInitState userInfo reqHdrs st _) ->
      return (userInfo, reqHdrs, st)
    CSInitError initErr -> do
      let e = "cannot start as connection_init failed with : " <> initErr
      withComplete $ sendStartErr e
    CSNotInitialised _ -> do
      let e = "start received before the connection is initialised"
      withComplete $ sendStartErr e

  (sc, scVer) <- liftIO $ IORef.readIORef gCtxMapRef
  execPlanE <- runExceptT $ E.getResolvedExecPlan pgExecCtx
               planCache userInfo sqlGenCtx enableAL sc scVer q
  execPlan <- either (withComplete . preExecErr) return execPlanE
  case execPlan of
    E.GExPHasura resolvedOp ->
      runHasuraGQ userInfo resolvedOp
    E.GExPRemote rn rsi opDef  ->
      runRemoteGQ userInfo reqHdrs opDef (rn, rsi)
  where
    runHasuraGQ :: UserInfo -> E.ExecOp -> ExceptT () IO ()
    runHasuraGQ userInfo = \case
      E.ExOpQuery opTx ->
        execQueryOrMut $ runLazyTx' pgExecCtx opTx
      E.ExOpMutation opTx ->
        execQueryOrMut $ runLazyTx pgExecCtx $
        withUserInfo userInfo opTx
      E.ExOpSubs lqOp -> do
        lqId <- liftIO $ LQ.addLiveQuery lqMap lqOp liveQOnChange
        liftIO $ STM.atomically $
          STMMap.insert (SOHasura (lqId, _grOperationName q)) opId opMap
        logOpEv ODStarted

    execQueryOrMut action = do
      logOpEv ODStarted
      resp <- liftIO $ runExceptT action
      either postExecErr sendSuccResp resp
      sendCompleted

    hdrsToConnParams = ConnParams . Just . Map.fromList . hdrsToText

    runRemoteGQ
      :: UserInfo -> [H.Header]
      -> G.TypedOperationDefinition
      -> (RemoteSchemaName, RemoteSchemaInfo)
      -> ExceptT () IO ()
    runRemoteGQ userInfo reqHdrs opDef (rn, rsi) =
      case G._todType opDef of
        -- TODO: this should go into execRemoteGQ?
        G.OperationTypeSubscription -> do
          let connParams =
                bool (Just $ hdrsToConnParams reqHdrs) Nothing $ null reqHdrs
          res <- liftIO $ runExceptT $ WS.runGqlClient logger (rsUrl rsi)
                 wsConn userInfoR rn opId connParams msg
          onLeft res $ \e -> do
            logOpEv $ ODQueryErr $ err500 Unexpected $ T.pack $ show e
            withComplete $ preExecErr e
          logOpEv ODStarted

        _ -> do
          -- if it's not a subscription, use HTTP to execute the query on the
          -- remote server
          let payload = J.encode q
          resp <- runExceptT $ E.execRemoteGQ httpMgr userInfo reqHdrs payload
                  rsi opDef
          either postExecErr sendSuccResp resp
          sendCompleted

    WSServerEnv logger pgExecCtx lqMap gCtxMapRef httpMgr  _
      sqlGenCtx planCache _ enableAL = serverEnv

    WSConnData userInfoR opMap errRespTy = WS.getData wsConn

    logOpEv opDet =
      logWSEvent logger wsConn $ EOperation opId (_grOperationName q) opDet

    getErrFn errTy =
      case errTy of
        ERTLegacy           -> encodeQErr
        ERTGraphqlCompliant -> encodeGQLErr

    sendStartErr e = do
      let errFn = getErrFn errRespTy
      sendMsg wsConn $ SMErr $ ErrorMsg opId $ errFn False $
        err400 StartFailed e
      logOpEv $ ODProtoErr e

    sendCompleted = do
      sendMsg wsConn $ SMComplete $ CompletionMsg opId
      logOpEv ODCompleted

    postExecErr qErr = do
      let errFn = getErrFn errRespTy
      logOpEv $ ODQueryErr qErr
      sendMsg wsConn $ SMData $ DataMsg opId $
        GQExecError $ pure $ errFn False qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr qErr = do
      let errFn = getErrFn errRespTy
      logOpEv $ ODQueryErr qErr
      let err = case errRespTy of
            ERTLegacy           -> errFn False qErr
            ERTGraphqlCompliant -> J.object ["errors" J..= [errFn False qErr]]
      sendMsg wsConn $ SMErr $ ErrorMsg opId err

    sendSuccResp encJson =
      sendMsg wsConn $ SMData $ DataMsg opId $ GQSuccess $ encJToLBS encJson

    withComplete :: ExceptT () IO () -> ExceptT () IO a
    withComplete action = do
      action
      sendCompleted
      throwError ()

    -- on change, send message on the websocket
    liveQOnChange resp =
      WS.sendMsg wsConn $ encodeServerMsg $ SMData $ DataMsg opId resp

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
      CMStart startMsg  -> onStart serverEnv wsConn startMsg
      CMStop stopMsg    -> onStop serverEnv wsConn stopMsg
      CMConnTerm        -> WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = _wseLogger serverEnv

onStop :: WSServerEnv -> WSConn -> StopMsg -> IO ()
onStop serverEnv wsConn (StopMsg opId) = do
  -- probably wrap the whole thing in a single tx?
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  mapM_ stopOperation opM
  STM.atomically $ STMMap.delete opId opMap
  where
    logger = _wseLogger serverEnv
    lqMap  = _wseLiveQMap serverEnv
    opMap  = _wscOpMap $ WS.getData wsConn
    wsId   = WS.getWSId wsConn
    stateR = _wscState $ WS.getData wsConn

    stopOperation op = case op of
      SOHasura (lqId, opNameM) -> do
        logWSEvent logger wsConn $ EOperation opId opNameM ODStopped
        LQ.removeLiveQuery lqMap lqId
      SORemote remOp -> do
        connState <- liftIO $ STM.readTVarIO stateR
        let stData = getStateData connState
        onJust stData $ \(ConnInitState _ _ connMap _) ->
          onJust (findOperationId connMap (wsId, opId)) $ \(rn, _) ->
            WS.stopRemote logger stateR remOp rn opId

logWSEvent
  :: (MonadIO m)
  => L.Logger -> WSConn -> WSEvent -> m ()
logWSEvent (L.Logger logger) wsConn wsEv = do
  userInfoME <- liftIO $ STM.readTVarIO userInfoR
  let (userVarsM, jwtExpM) = case userInfoME of
        CSInitialised (ConnInitState userInfo _ _ jwtM) ->
          ( Just $ userVars userInfo
          , jwtM
          )
        _                             -> (Nothing, Nothing)
  liftIO $ logger $ WSLog wsId userVarsM wsEv Nothing jwtExpM
  where
    WSConnData userInfoR _ _ = WS.getData wsConn
    wsId = WS.getWSId wsConn

onConnInit
  :: (MonadIO m)
  => L.Logger -> H.Manager -> WSConn -> AuthMode -> Maybe ConnParams -> m ()
onConnInit logger manager wsConn authMode connParamsM = do
  headers <- mkHeaders <$> liftIO (STM.readTVarIO (_wscState $ WS.getData wsConn))
  res <- runExceptT $ getUserInfoWithExpTime logger manager headers authMode
  case res of
    Left e  -> do
      liftIO $ STM.atomically $ STM.writeTVar (_wscState $ WS.getData wsConn) $
        CSInitError $ qeError e
      let connErr = ConnErrMsg $ qeError e
      logWSEvent logger wsConn $ EConnErr connErr
      sendMsg wsConn $ SMConnErr connErr
    Right (userInfo, expTimeM) -> do
      liftIO $ STM.atomically $ STM.writeTVar (_wscState $ WS.getData wsConn) $
        --CSInitialised userInfo expTimeM paramHeaders
        CSInitialised (ConnInitState userInfo headers Map.empty expTimeM)
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
  -> LQ.LiveQueriesState
  -> WSConn
  -> IO ()
onClose logger lqMap wsConn = do
  logWSEvent logger wsConn EClosed
  operations <- STM.atomically $ ListT.toList $ STMMap.listT opMap
  void $ A.forConcurrently operations $ \(_, op) ->
    case op of
      SOHasura (lqId, _) -> LQ.removeLiveQuery lqMap lqId
      SORemote _ ->
        WS.closeRemote logger (_wscState $ WS._wcExtraData wsConn) wsId
  where
    opMap = _wscOpMap $ WS.getData wsConn
    wsId  = WS.getWSId wsConn

createWSServerEnv
  :: L.Logger
  -> PGExecCtx
  -> LQ.LiveQueriesState
  -> IORef.IORef (SchemaCache, SchemaCacheVer)
  -> H.Manager
  -> CorsPolicy
  -> SQLGenCtx
  -> Bool
  -> E.PlanCache
  -> IO WSServerEnv
createWSServerEnv logger pgExecCtx lqState cacheRef httpManager
  corsPolicy sqlGenCtx enableAL planCache = do
  wsServer <- STM.atomically $ WS.createWSServer logger
  return $ WSServerEnv logger
    pgExecCtx lqState cacheRef
    httpManager corsPolicy sqlGenCtx planCache wsServer enableAL

createWSServerApp :: AuthMode -> WSServerEnv -> WS.ServerApp
createWSServerApp authMode serverEnv =
  WS.createServerApp (_wseServer serverEnv) handlers
  where
    handlers =
      WS.WSHandlers
      (onConn (_wseLogger serverEnv) (_wseCorsPolicy serverEnv))
      (onMessage authMode serverEnv)
      (onClose (_wseLogger serverEnv) $ _wseLiveQMap serverEnv)
