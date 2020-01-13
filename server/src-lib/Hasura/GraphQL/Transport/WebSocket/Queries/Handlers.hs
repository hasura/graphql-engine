module Hasura.GraphQL.Transport.WebSocket.Queries.Handlers where

import qualified Control.Concurrent.Async                            as A
import qualified Control.Concurrent.STM                              as STM
import qualified Data.Aeson                                          as J
import qualified Data.ByteString.Lazy                                as BL
import qualified Data.CaseInsensitive                                as CI
import qualified Data.HashMap.Strict                                 as Map
import qualified Data.Text                                           as T
import qualified Data.Text.Encoding                                  as TE
import qualified Data.Time.Clock                                     as TC
import qualified Database.PG.Query                                   as Q
import qualified Language.GraphQL.Draft.Syntax                       as G
import qualified Network.HTTP.Client                                 as H
import qualified Network.HTTP.Types                                  as H
import qualified Network.WebSockets                                  as WS
import qualified StmContainers.Map                                   as STMMap

import           Control.Concurrent                                  (threadDelay)
import qualified ListT

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Common
import           Hasura.GraphQL.Transport.WebSocket.Queries.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Queries.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Error                              (Code (StartFailed))
import           Hasura.Server.Auth                                  (AuthMode, UserAuthentication,
                                                                      resolveUserInfo)
import           Hasura.Server.Context
import           Hasura.Server.Cors
import           Hasura.Server.Utils                                 (RequestId, diffTimeToMicro,
                                                                      getRequestId)

import qualified Hasura.GraphQL.Execute                              as E
import qualified Hasura.GraphQL.Execute.LiveQuery                    as LQ
import qualified Hasura.GraphQL.Execute.LiveQuery.Poll               as LQ
import qualified Hasura.GraphQL.Transport.WebSocket.Server           as WS
import qualified Hasura.Logging                                      as L

onConnHandler :: (MonadIO m)
              => L.Logger L.Hasura -> CorsPolicy -> WS.OnConnH m ConnState
onConnHandler (L.Logger logger) corsPolicy wsId requestHead = do
  res <- runExceptT $ do
    errType <- WS.checkPath requestHead
    let reqHdrs = WS.requestHeaders requestHead
    headers <- maybe (return reqHdrs) (flip enforceCors reqHdrs . snd) getOrigin
    return (WsHeaders $ filterWsHeaders headers, errType)
  either reject (uncurry accept) res

  where
    keepAliveAction wsConn = liftIO $ forever $ do
      sendMsg wsConn SMConnKeepAlive
      threadDelay $ 5 * 1000 * 1000

    jwtExpiryHandler connData _ = do
      expTime <- liftIO $ STM.atomically $ do
        connState <- STM.readTVar $ _wscUser connData
        case connState of
          CSNotInitialised _         -> STM.retry
          CSInitError _              -> STM.retry
          CSInitialised _ expTimeM _ ->
            maybe STM.retry return expTimeM
      currTime <- TC.getCurrentTime
      threadDelay $ diffTimeToMicro $ TC.diffUTCTime expTime currTime

    accept hdrs errType = do
      logger $ mkWsInfoLog Nothing (WsConnInfo wsId Nothing Nothing) EAccepted
      connData <- liftIO $ WSConnData
                  <$> STM.newTVarIO (CSNotInitialised hdrs)
                  <*> STMMap.newIO
                  <*> pure errType
      let acceptRequest = WS.defaultAcceptRequest
                          { WS.acceptSubprotocol = Just "graphql-ws"}
      return $ Right $ WS.AcceptWith (CSQueries connData) acceptRequest
                       (Just keepAliveAction) (Just (jwtExpiryHandler connData))

    reject qErr = do
      logger $ mkWsErrorLog Nothing (WsConnInfo wsId Nothing Nothing) (ERejected qErr)
      return $ Left $ WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (BL.toStrict $ J.encode $ encodeGQLErr False qErr)

    getOrigin =
      find ((==) "Origin" . fst) (WS.requestHeaders requestHead)

    enforceCors origin reqHdrs = case cpConfig corsPolicy of
      CCAllowAll -> return reqHdrs
      CCDisabled readCookie ->
        if readCookie
        then return reqHdrs
        else do
          lift $ logger $ mkWsInfoLog Nothing (WsConnInfo wsId Nothing (Just corsNote)) EAccepted
          return $ filter (\h -> fst h /= "Cookie") reqHdrs
      CCAllowedOrigins ds
        -- if the origin is in our cors domains, no error
        | bsToTxt origin `elem` dmFqdns ds   -> return reqHdrs
        -- if current origin is part of wildcard domain list, no error
        | inWildcardList ds (bsToTxt origin) -> return reqHdrs
        -- otherwise error
        | otherwise                          -> corsErr

    corsErr = throw400 AccessDenied
              "received origin header does not match configured CORS domains"

    corsNote = "Cookie is not read when CORS is disabled, because it is a potential "
            <> "security issue. If you're already handling CORS before Hasura and enforcing "
            <> "CORS on websocket connections, then you can use the flag --ws-read-cookie or "
            <> "HASURA_GRAPHQL_WS_READ_COOKIE to force read cookie when CORS is disabled."

onMessageHandler
  :: (MonadIO m, UserAuthentication m)
  => AuthMode
  -> WSServerEnv
  -> WSConnData
  -> WSConn
  -> BL.ByteString -> m ()
onMessageHandler authMode serverEnv connData wsConn msgRaw =
  case J.eitherDecode msgRaw of
    Left e    -> do
      let err = ConnErrMsg $ "parsing ClientMessage failed: " <> T.pack e
      logWSEvent logger wsConn connData $ EConnErr err
      sendMsg wsConn $ SMConnErr err

    Right msg -> case msg of
      CMConnInit params -> onConnInit logger (_wseHManager serverEnv)
                           wsConn connData authMode params
      CMStart startMsg  -> liftIO $ onStart serverEnv wsConn connData startMsg
      CMStop stopMsg    -> liftIO $ onStop logger (_wseLiveQMap serverEnv)
                                    wsConn connData stopMsg
      CMConnTerm        -> liftIO $ WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = _wseLogger serverEnv

onConnInit
  :: (MonadIO m, UserAuthentication m)
  => L.Logger L.Hasura -> H.Manager -> WSConn -> WSConnData -> AuthMode -> Maybe ConnParams -> m ()
onConnInit logger manager wsConn connData authMode connParamsM = do
  headers <- mkHeaders <$> liftIO (STM.readTVarIO (_wscUser connData))
  res <- resolveUserInfo logger manager headers authMode
  case res of
    Left e  -> do
      liftIO $ STM.atomically $ STM.writeTVar (_wscUser connData) $
        CSInitError $ qeError e
      let connErr = ConnErrMsg $ qeError e
      logWSEvent logger wsConn connData $ EConnErr connErr
      sendMsg wsConn $ SMConnErr connErr
    Right (userInfo, expTimeM) -> do
      liftIO $ STM.atomically $ STM.writeTVar (_wscUser connData) $
        CSInitialised userInfo expTimeM paramHeaders
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

onStart :: WSServerEnv -> WSConn -> WSConnData -> StartMsg -> IO ()
onStart serverEnv wsConn connData (StartMsg opId q) = catchAndIgnore $ do

  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  when (isJust opM) $ withComplete $ sendStartErr $
    "an operation already exists with this id: " <> unOperationId opId

  userInfoM <- liftIO $ STM.readTVarIO userInfoR
  (userInfo, reqHdrs) <- case userInfoM of
    CSInitialised userInfo _ reqHdrs -> return (userInfo, reqHdrs)
    CSInitError initErr -> do
      let e = "cannot start as connection_init failed with : " <> initErr
      withComplete $ sendStartErr e
    CSNotInitialised _ -> do
      let e = "start received before the connection is initialised"
      withComplete $ sendStartErr e

  requestId <- getRequestId reqHdrs
  (sc, scVer) <- liftIO getSchemaCache
  execPlanE <- runExceptT $ E.getResolvedExecPlan pgExecCtx
               planCache userInfo sqlGenCtx enableAL sc scVer q
  execPlan <- either (withComplete . preExecErr requestId) return execPlanE
  let execCtx = E.ExecutionCtx logger sqlGenCtx pgExecCtx
                planCache sc scVer httpMgr enableAL

  case execPlan of
    E.GExPHasura resolvedOp ->
      runHasuraGQ requestId q userInfo resolvedOp
    E.GExPRemote rsi opDef  ->
      runRemoteGQ execCtx requestId userInfo reqHdrs opDef rsi
  where
    runHasuraGQ :: RequestId -> GQLReqUnparsed -> UserInfo -> E.ExecOp
                -> ExceptT () IO ()
    runHasuraGQ reqId query userInfo = \case
      E.ExOpQuery opTx genSql ->
        execQueryOrMut reqId query genSql $ runLazyTx' pgExecCtx opTx
      E.ExOpMutation opTx ->
        execQueryOrMut reqId query Nothing $
          runLazyTx pgExecCtx Q.ReadWrite $ withUserInfo userInfo opTx
      E.ExOpSubs lqOp -> do
         -- log the graphql query
         L.unLogger logger $ QueryLog query Nothing reqId
         lqId <- liftIO $ LQ.addLiveQuery lqMap lqOp liveQOnChange
         liftIO $ STM.atomically $
           STMMap.insert (lqId, _grOperationName q) opId opMap
         logOpEv ODStarted (Just reqId)

    execQueryOrMut reqId query genSql action = do
      logOpEv ODStarted (Just reqId)
      -- log the generated SQL and the graphql query
      L.unLogger logger $ QueryLog query genSql reqId
      resp <- liftIO $ runExceptT action
      either (postExecErr reqId) sendSuccResp resp
      sendCompleted (Just reqId)

    runRemoteGQ :: E.ExecutionCtx -> RequestId -> UserInfo -> [H.Header]
                -> G.TypedOperationDefinition -> RemoteSchemaInfo
                -> ExceptT () IO ()
    runRemoteGQ execCtx reqId userInfo reqHdrs opDef rsi = do
      when (G._todType opDef == G.OperationTypeSubscription) $
        withComplete $ preExecErr reqId $
        err400 NotSupported "subscription to remote server is not supported"

      -- if it's not a subscription, use HTTP to execute the query on the remote
      resp <- runExceptT $ flip runReaderT execCtx $
              E.execRemoteGQ reqId userInfo reqHdrs q rsi opDef
      either (postExecErr reqId) (sendRemoteResp reqId . _hrBody) resp
      sendCompleted (Just reqId)

    sendRemoteResp reqId resp =
      case J.eitherDecodeStrict (encJToBS resp) of
        Left e    -> postExecErr reqId $ invalidGqlErr $ T.pack e
        Right res -> sendMsg wsConn $ SMData $ DataMsg opId (GRRemote res)

    invalidGqlErr err = err500 Unexpected $
      "Failed parsing GraphQL response from remote: " <> err

    WSServerEnv logger pgExecCtx lqMap getSchemaCache httpMgr _ sqlGenCtx planCache
      _ enableAL = serverEnv

    WSConnData userInfoR opMap errRespTy = connData

    logOpEv opTy reqId =
      logWSEvent logger wsConn connData $ EOperation opDet
      where
        opDet = OperationDetails opId reqId (_grOperationName q) opTy query
        -- log the query only in errors
        query = case opTy of
          ODQueryErr _ -> Just q
          _            -> Nothing

    getErrFn errTy =
      case errTy of
        WS.ERTLegacy           -> encodeQErr
        WS.ERTGraphqlCompliant -> encodeGQLErr

    sendStartErr e = do
      let errFn = getErrFn errRespTy
      sendMsg wsConn $ SMErr $ ErrorMsg opId $ errFn False $
        err400 StartFailed e
      logOpEv (ODProtoErr e) Nothing

    sendCompleted reqId = do
      sendMsg wsConn $ SMComplete $ CompletionMsg opId
      logOpEv ODCompleted reqId

    postExecErr reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId)
      sendMsg wsConn $ SMData $ DataMsg opId $
        GRHasura $ GQExecError $ pure $ errFn False qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId)
      let err = case errRespTy of
            WS.ERTLegacy           -> errFn False qErr
            WS.ERTGraphqlCompliant -> J.object ["errors" J..= [errFn False qErr]]
      sendMsg wsConn $ SMErr $ ErrorMsg opId err

    sendSuccResp encJson =
      sendMsg wsConn $ SMData $ DataMsg opId $
        GRHasura $ GQSuccess $ encJToLBS encJson

    withComplete :: ExceptT () IO () -> ExceptT () IO a
    withComplete action = do
      action
      sendCompleted Nothing
      throwError ()

    -- on change, send message on the websocket
    liveQOnChange :: LQ.OnChange
    liveQOnChange (GQSuccess (LQ.LiveQueryResponse bs dTime)) =
      sendMsgWithMetadata wsConn (SMData $ DataMsg opId $ GRHasura $ GQSuccess bs) $
        LQ.LiveQueryMetadata dTime
    liveQOnChange resp = sendMsg wsConn $ SMData $ DataMsg opId $ GRHasura $
      LQ._lqrPayload <$> resp

    catchAndIgnore :: ExceptT () IO () -> IO ()
    catchAndIgnore m = void $ runExceptT m

onStop
  :: L.Logger L.Hasura
  -> LQ.LiveQueriesState
  -> WSConn
  -> WSConnData
  -> StopMsg
  -> IO ()
onStop logger lqMap wsConn connData (StopMsg opId) = do
  -- probably wrap the whole thing in a single tx?
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  case opM of
    Just (lqId, opNameM) -> do
      logWSEvent logger wsConn connData $ EOperation $ opDet opNameM
      LQ.removeLiveQuery lqMap lqId
    Nothing    -> return ()
  STM.atomically $ STMMap.delete opId opMap
  where
    -- logger = _wseLogger serverEnv
    -- lqMap  = _wseLiveQMap serverEnv
    opMap  = _wscOpMap connData
    opDet n = OperationDetails opId Nothing n ODStopped Nothing

onCloseHandler
  :: MonadIO m
  => L.Logger L.Hasura
  -> LQ.LiveQueriesState
  -> WSConnData
  -> WSConn
  -> m ()
onCloseHandler logger lqMap connData wsConn = do
  logWSEvent logger wsConn connData EClosed
  operations <- liftIO $ STM.atomically $ ListT.toList $ STMMap.listT opMap
  void $ liftIO $ A.forConcurrently operations $ \(_, (lqId, _)) ->
    LQ.removeLiveQuery lqMap lqId
  where
    opMap = _wscOpMap connData

logWSEvent
  :: (MonadIO m)
  => L.Logger L.Hasura -> WSConn -> WSConnData -> WSEvent -> m ()
logWSEvent (L.Logger logger) wsConn connData wsEv = do
  userInfoME <- liftIO $ STM.readTVarIO userInfoR
  let (userVarsM, jwtExpM) = case userInfoME of
        CSInitialised userInfo jwtM _ -> ( Just $ userVars userInfo
                                         , jwtM
                                         )
        _                             -> (Nothing, Nothing)
  liftIO $ logger $ WSLog logLevel $ WSLogInfo userVarsM (WsConnInfo wsId jwtExpM Nothing) wsEv
  where
    WSConnData userInfoR _ _ = connData
    wsId = WS.getWSId wsConn
    logLevel = bool L.LevelInfo L.LevelError isError
    isError = case wsEv of
      EAccepted   -> False
      ERejected _ -> True
      EConnErr _  -> True
      EClosed     -> False
      EOperation op -> case _odOperationType op of
        ODStarted    -> False
        ODProtoErr _ -> True
        ODQueryErr _ -> True
        ODCompleted  -> False
        ODStopped    -> False

sendMsg :: (MonadIO m) => WSConn -> ServerMsg -> m ()
sendMsg wsConn msg =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse (encodeServerMsg msg) Nothing

sendMsgWithMetadata :: (MonadIO m) => WSConn -> ServerMsg -> LQ.LiveQueryMetadata -> m ()
sendMsgWithMetadata wsConn msg (LQ.LiveQueryMetadata execTime) =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse bs wsInfo
  where
    bs = encodeServerMsg msg
    wsInfo = Just $ WS.WSEventInfo
      { WS._wseiQueryExecutionTime = Just $ realToFrac execTime
      , WS._wseiResponseSize = Just $ BL.length bs
      }
