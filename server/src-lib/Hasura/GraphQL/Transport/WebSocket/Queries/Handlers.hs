{-# LANGUAGE RecordWildCards #-}
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

import           Control.Concurrent.Extended                         (sleep)
import           Data.String                                         (fromString)
import           GHC.AssertNF
import qualified ListT

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Common
import           Hasura.GraphQL.Transport.WebSocket.Queries.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Queries.Types
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth                                  (AuthMode, UserAuthentication,
                                                                      resolveUserInfo)
import           Hasura.Server.Cors
import           Hasura.Server.Utils                                 (RequestId, getRequestId)
import           Hasura.Server.Version
import           Hasura.Session

import qualified Hasura.GraphQL.Execute                              as E
import qualified Hasura.GraphQL.Execute.LiveQuery                    as LQ
import qualified Hasura.GraphQL.Execute.LiveQuery.Poll               as LQ
import qualified Hasura.GraphQL.Resolve.Action                       as RA
import qualified Hasura.GraphQL.Transport.WebSocket.Server           as WS
import qualified Hasura.Logging                                      as L
import qualified Hasura.Server.Telemetry.Counters                    as Telem

onConnHandler :: (MonadIO m)
              => L.Logger L.Hasura -> CorsPolicy -> WS.OnConnH m ConnState
onConnHandler (L.Logger logger) corsPolicy wsId requestHead = do
  res <- runExceptT $ do
    errType <- WS.checkPath requestHead
    let logCorsNote corsNote = lift $ logger $
          mkWsInfoLog Nothing (WsConnInfo wsId Nothing (Just corsNote)) EAccepted
    headers <- WS.getHeadersWithEnforceCors logCorsNote requestHead corsPolicy
    return (WsHeaders $ filterWsHeaders headers, errType)
  either reject (uncurry accept) res

  where
    keepAliveAction wsConn = liftIO $ forever $ do
      sendMsg wsConn SMConnKeepAlive
      sleep $ seconds 5

    accept hdrs errType = do
      logger $ mkWsInfoLog Nothing (WsConnInfo wsId Nothing Nothing) EAccepted
      connData <- liftIO $ WSConnData
                  <$> STM.newTVarIO (CSNotInitialised hdrs)
                  <*> STMMap.newIO
                  <*> pure errType
      let acceptRequest = WS.defaultAcceptRequest
                          { WS.acceptSubprotocol = Just "graphql-ws"}
      return $ Right $ WS.AcceptWith (CSQueries connData) acceptRequest
                       (Just keepAliveAction) tokenExpiryHandler

    reject qErr = do
      logger $ mkWsErrorLog Nothing (WsConnInfo wsId Nothing Nothing) (ERejected qErr)
      return $ Left $ WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (BL.toStrict $ J.encode $ encodeGQLErr False qErr)

onMessageHandler
  :: (HasVersion, MonadIO m, UserAuthentication m)
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
      -- The idea is cleanup will be handled by 'onClose', but...
      -- NOTE: we need to close the websocket connection when we receive the
      -- CMConnTerm message and calling WS.closeConn will definitely throw an
      -- exception, but I'm not sure if 'closeConn' is the correct thing here....
      CMConnTerm        -> liftIO $ WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = _wseLogger serverEnv

onConnInit
  :: (HasVersion, MonadIO m, UserAuthentication m)
  => L.Logger L.Hasura -> H.Manager -> WSConn -> WSConnData -> AuthMode -> Maybe ConnParams -> m ()
onConnInit logger manager wsConn connData authMode connParamsM = do
  headers <- mkHeaders <$> liftIO (STM.readTVarIO (_wscUser connData))
  res <- resolveUserInfo logger manager headers authMode
  case res of
    Left e  -> do
      let !initErr = CSInitError $ qeError e
      liftIO $ do
        $assertNFHere initErr  -- so we don't write thunks to mutable vars
        STM.atomically $ STM.writeTVar (_wscUser connData) initErr

      let connErr = ConnErrMsg $ qeError e
      logWSEvent logger wsConn connData $ EConnErr connErr
      sendMsg wsConn $ SMConnErr connErr
    Right (userInfo, expTimeM) -> do
      let !csInit = CSInitialised userInfo expTimeM paramHeaders
      liftIO $ do
        $assertNFHere csInit  -- so we don't write thunks to mutable vars
        STM.atomically $ STM.writeTVar (_wscUser connData) csInit

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

onStart :: HasVersion => WSServerEnv -> WSConn -> WSConnData -> StartMsg -> IO ()
onStart serverEnv wsConn connData (StartMsg opId q) = catchAndIgnore $ do
  timerTot <- startTimer
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  -- NOTE: it should be safe to rely on this check later on in this function, since we expect that
  -- we process all operations on a websocket connection serially:
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
  execPlanE <- runExceptT $ E.getResolvedExecPlan (RA.allowActions httpMgr reqHdrs)
               pgExecCtx planCache userInfo sqlGenCtx enableAL sc scVer httpMgr reqHdrs q
  (telemCacheHit, execPlan) <- either (withComplete . preExecErr requestId) return execPlanE
  let execCtx = E.ExecutionCtx logger sqlGenCtx pgExecCtx
                planCache sc scVer httpMgr enableAL

  case execPlan of
    E.GExPHasura resolvedOp ->
      runHasuraGQ timerTot telemCacheHit requestId q userInfo resolvedOp
    E.GExPRemote rsi opDef  ->
      runRemoteGQ timerTot telemCacheHit execCtx requestId userInfo reqHdrs opDef rsi
  where
    telemTransport = Telem.WebSocket
    runHasuraGQ :: ExceptT () IO DiffTime
                -> Telem.CacheHit -> RequestId -> GQLReqUnparsed -> UserInfo -> E.ExecOp
                -> ExceptT () IO ()
    runHasuraGQ timerTot telemCacheHit reqId query userInfo = \case
      E.ExOpQuery opTx genSql ->
        execQueryOrMut Telem.Query genSql $ runLazyTx' pgExecCtx opTx
      -- Response headers discarded over websockets
      E.ExOpMutation _ opTx ->
        execQueryOrMut Telem.Mutation Nothing $
          runLazyTx pgExecCtx Q.ReadWrite $ withUserInfo userInfo opTx
      E.ExOpSubs lqOp -> do
        -- log the graphql query
        L.unLogger logger $ QueryLog query Nothing reqId
        -- NOTE!: we mask async exceptions higher in the call stack, but it's
        -- crucial we don't lose lqId after addLiveQuery returns successfully.
        !lqId <- liftIO $ LQ.addLiveQuery logger lqMap lqOp liveQOnChange
        let !opName = _grOperationName q
        liftIO $ $assertNFHere $! (lqId, opName)  -- so we don't write thunks to mutable vars

        liftIO $ STM.atomically $
          -- NOTE: see crucial `lookup` check above, ensuring this doesn't clobber:
          STMMap.insert (lqId, opName) opId opMap
        logOpEv ODStarted (Just reqId)

      where
        telemLocality = Telem.Local
        execQueryOrMut telemQueryType genSql action = do
          logOpEv ODStarted (Just reqId)
          -- log the generated SQL and the graphql query
          L.unLogger logger $ QueryLog query genSql reqId
          withElapsedTime (liftIO $ runExceptT action) >>= \case
            (_,      Left err) -> postExecErr reqId err
            (telemTimeIO_DT, Right encJson) -> do
              -- Telemetry. NOTE: don't time network IO:
              telemTimeTot <- Seconds <$> timerTot
              sendSuccResp encJson $ LQ.LiveQueryMetadata telemTimeIO_DT
              let telemTimeIO = convertDuration telemTimeIO_DT
              Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}

          sendCompleted (Just reqId)

    runRemoteGQ :: ExceptT () IO DiffTime
                -> Telem.CacheHit -> E.ExecutionCtx -> RequestId -> UserInfo -> [H.Header]
                -> G.TypedOperationDefinition -> RemoteSchemaInfo
                -> ExceptT () IO ()
    runRemoteGQ timerTot telemCacheHit execCtx reqId userInfo reqHdrs opDef rsi = do
      let telemLocality = Telem.Remote
      telemQueryType <- case G._todType opDef of
        G.OperationTypeSubscription ->
          withComplete $ preExecErr reqId $
          err400 NotSupported "subscription to remote server is not supported"
        G.OperationTypeMutation -> return Telem.Mutation
        G.OperationTypeQuery    -> return Telem.Query

      -- if it's not a subscription, use HTTP to execute the query on the remote
      runExceptT (flip runReaderT execCtx $
        E.execRemoteGQ reqId userInfo reqHdrs q rsi (G._todType opDef)) >>= \case
          Left  err           -> postExecErr reqId err
          Right (telemTimeIO_DT, !val) -> do
            -- Telemetry. NOTE: don't time network IO:
            telemTimeTot <- Seconds <$> timerTot
            sendRemoteResp reqId (_hrBody val) $ LQ.LiveQueryMetadata telemTimeIO_DT
            let telemTimeIO = convertDuration telemTimeIO_DT
            Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}

      sendCompleted (Just reqId)

    sendRemoteResp reqId resp meta =
      case J.eitherDecodeStrict (encJToBS resp) of
        Left e    -> postExecErr reqId $ invalidGqlErr $ T.pack e
        Right res -> sendMsgWithMetadata wsConn (SMData $ DataMsg opId $ GRRemote res) meta

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
      sendMsg wsConn $
        SMErr $ ErrorMsg opId $ errFn False $ err400 StartFailed e
      logOpEv (ODProtoErr e) Nothing

    sendCompleted reqId = do
      sendMsg wsConn (SMComplete $ CompletionMsg opId)
      logOpEv ODCompleted reqId

    postExecErr reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId)
      sendMsg wsConn $ SMData $
        DataMsg opId $ GRHasura $ GQExecError $ pure $ errFn False qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId)
      let err = case errRespTy of
            WS.ERTLegacy           -> errFn False qErr
            WS.ERTGraphqlCompliant -> J.object ["errors" J..= [errFn False qErr]]
      sendMsg wsConn (SMErr $ ErrorMsg opId err)

    sendSuccResp encJson =
      sendMsgWithMetadata wsConn
        (SMData $ DataMsg opId $ GRHasura $ GQSuccess $ encJToLBS encJson)

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
  -- When a stop message is received for an operation, it may not be present in OpMap
  -- in these cases:
  -- 1. If the operation is a query/mutation - as we remove the operation from the
  -- OpMap as soon as it is executed
  -- 2. A misbehaving client
  -- 3. A bug on our end
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  case opM of
    Just (lqId, opNameM) -> do
      logWSEvent logger wsConn connData $ EOperation $ opDet opNameM
      LQ.removeLiveQuery logger lqMap lqId
    Nothing    ->
      L.unLogger logger $ L.UnstructuredLog L.LevelDebug $ fromString $
        "Received STOP for an operation that we have no record for: "
        <> show (unOperationId opId)
        <> " (could be a query/mutation operation or a misbehaving client or a bug)"
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
    LQ.removeLiveQuery logger lqMap lqId
  where
    opMap = _wscOpMap connData

logWSEvent
  :: (MonadIO m)
  => L.Logger L.Hasura -> WSConn -> WSConnData -> WSEvent -> m ()
logWSEvent (L.Logger logger) wsConn connData wsEv = do
  userInfoME <- liftIO $ STM.readTVarIO userInfoR
  let (userVarsM, jwtExpM) = case userInfoME of
        CSInitialised userInfo tokenM _ -> ( Just $ _uiSession userInfo
                                         , tokenM
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
