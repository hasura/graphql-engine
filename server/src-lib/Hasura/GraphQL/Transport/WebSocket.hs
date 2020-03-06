{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Hasura.GraphQL.Transport.WebSocket
  ( createWSServerApp
  , createWSServerEnv
  , stopWSServerApp
  , WSServerEnv
  ) where

-- NOTE!:
--   The handler functions 'onClose', 'onMessage', etc. depend for correctness on two properties:
--     - they run with async exceptions masked
--     - they do not race on the same connection

import qualified Control.Concurrent.Async.Lifted.Safe        as LA
import qualified Control.Concurrent.STM                      as STM
import qualified Control.Monad.Trans.Control                 as MC
import qualified Data.Aeson                                  as J
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.CaseInsensitive                        as CI
import qualified Data.HashMap.Strict                         as Map
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as TE
import qualified Data.Time.Clock                             as TC
import qualified Database.PG.Query                           as Q
import qualified Language.GraphQL.Draft.Syntax               as G
import qualified Network.HTTP.Client                         as H
import qualified Network.HTTP.Types                          as H
import qualified Network.WebSockets                          as WS
import qualified StmContainers.Map                           as STMMap

import           Control.Concurrent.Extended                 (sleep)
import           Control.Exception.Lifted
import           Data.String
import qualified ListT

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Error                      (Code (StartFailed))
import           Hasura.Server.Auth                          (AuthMode, UserAuthentication,
                                                              resolveUserInfo)
import           Hasura.Server.Context
import           Hasura.Server.Cors
import           Hasura.Server.Utils                         (RequestId, getRequestId)
import           Hasura.Server.Version                       (HasVersion)

import qualified Hasura.GraphQL.Execute                      as E
import qualified Hasura.GraphQL.Execute.LiveQuery            as LQ
import qualified Hasura.GraphQL.Execute.LiveQuery.Poll       as LQ
import qualified Hasura.GraphQL.Transport.WebSocket.Server   as WS
import qualified Hasura.Logging                              as L
import qualified Hasura.Server.Telemetry.Counters            as Telem

-- | 'LQ.LiveQueryId' comes from 'Hasura.GraphQL.Execute.LiveQuery.State.addLiveQuery'. We use
-- this to track a connection's operations so we can remove them from 'LiveQueryState', and
-- log.
--
-- NOTE!: This must be kept consistent with the global 'LiveQueryState', in 'onClose'
-- and 'onStart'.

sendMsg :: (MonadIO m) => WSConn -> ServerMsg -> m ()
sendMsg wsConn msg =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse (encodeServerMsg msg) Nothing

sendMsgWithMetadata :: (MonadIO m) => WSConn -> ServerMsg -> LQ.LiveQueryMetadata -> m ()
sendMsgWithMetadata wsConn msg (LQ.LiveQueryMetadata execTime) =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse bs wsInfo
  where
    bs = encodeServerMsg msg
    wsInfo = Just $ WS.WSServerEventInfo
      { WS._wseiQueryExecutionTime = Just $ realToFrac execTime
      , WS._wseiResponseSize = Just $ BL.length bs
      }

data WSServerEnv
  = WSServerEnv
  { _wseLogger          :: !(L.Logger L.Hasura)
  , _wseRunTx           :: !PGExecCtx
  , _wseLiveQMap        :: !LQ.LiveQueriesState
  , _wseGCtxMap         :: !(IO (SchemaCache, SchemaCacheVer))
  -- ^ an action that always returns the latest version of the schema cache. See 'SchemaCacheRef'.
  , _wseHManager        :: !H.Manager
  , _wseCorsPolicy      :: !CorsPolicy
  , _wseSQLCtx          :: !SQLGenCtx
  , _wseQueryCache      :: !E.PlanCache
  , _wseServer          :: !WSServer
  , _wseEnableAllowlist :: !Bool
  }

onConn :: (MonadIO m, WebSocketLog m)
       => L.Logger L.Hasura -> CorsPolicy -> WS.OnConnH m WSConnData
onConn hLogger corsPolicy wsId requestHead = do
  res <- runExceptT $ do
    errType <- checkPath
    let reqHdrs = WS.requestHeaders requestHead
    headers <- maybe (return reqHdrs) (flip enforceCors reqHdrs . snd) getOrigin
    return (WSHeaders $ filterWsHeaders headers, errType)
  either reject (uncurry accept) res

  where
    keepAliveAction wsConn = liftIO $ forever $ do
      sendMsg wsConn SMConnKeepAlive
      sleep $ seconds 5

    jwtExpiryHandler wsConn = do
      expTime <- liftIO $ STM.atomically $ do
        connState <- STM.readTVar $ (_wscUser . WS.getData) wsConn
        case connState of
          CSNotInitialised _        -> STM.retry
          CSInitError _              -> STM.retry
          CSInitialised _ expTimeM _ ->
            maybe STM.retry return expTimeM
      currTime <- TC.getCurrentTime
      sleep $ fromUnits $ TC.diffUTCTime expTime currTime

    accept hdrs errType = do
      logWebSocketSuccess hLogger Nothing (WSConnInfo wsId Nothing Nothing) EAccepted []
      connData <- liftIO $ WSConnData
                  <$> STM.newTVarIO (CSNotInitialised hdrs)
                  <*> STMMap.newIO
                  <*> pure errType
      let acceptRequest = WS.defaultAcceptRequest
                          { WS.acceptSubprotocol = Just "graphql-ws"}
      return $ Right $ WS.AcceptWith connData acceptRequest
                       (Just keepAliveAction) (Just jwtExpiryHandler)

    reject qErr = do
      logWebSocketError hLogger Nothing (WSConnInfo wsId Nothing Nothing) (ERejected qErr) []
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

    enforceCors origin reqHdrs = case cpConfig corsPolicy of
      CCAllowAll -> return reqHdrs
      CCDisabled readCookie ->
        if readCookie
        then return reqHdrs
        else do
          lift $ logWebSocketSuccess hLogger Nothing (WSConnInfo wsId Nothing (Just corsNote)) EAccepted []
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


onStart :: (MonadIO m, WebSocketLog m, HasVersion) => WSServerEnv -> WSConn -> StartMsg -> m ()
onStart serverEnv wsConn (StartMsg opId q) = catchAndIgnore $ do
  timerTot <- startTimer
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  when (isJust opM) $ withComplete $ lift $ sendStartErr $
    "an operation already exists with this id: " <> unOperationId opId

  userInfoM <- liftIO $ STM.readTVarIO userInfoR
  (userInfo, reqHdrs) <- case userInfoM of
    CSInitialised userInfo _ reqHdrs -> return (userInfo, reqHdrs)
    CSInitError initErr -> do
      let e = "cannot start as connection_init failed with : " <> initErr
      withComplete $ lift $ sendStartErr e
    CSNotInitialised _ -> do
      let e = "start received before the connection is initialised"
      withComplete $ lift $ sendStartErr e

  requestId <- getRequestId reqHdrs
  (sc, scVer) <- liftIO getSchemaCache
  execPlanE <- runExceptT $ E.getResolvedExecPlan pgExecCtx
               planCache userInfo sqlGenCtx enableAL sc scVer httpMgr reqHdrs q

  (telemCacheHit, execPlan) <- either (withComplete . lift . preExecErr requestId) return execPlanE
  let execCtx = E.ExecutionCtx logger sqlGenCtx pgExecCtx
                planCache sc scVer httpMgr enableAL

  case execPlan of
    E.GExPHasura resolvedOp ->
      runHasuraGQ timerTot telemCacheHit requestId q userInfo resolvedOp reqHdrs
    E.GExPRemote rsi opDef  ->
      runRemoteGQ timerTot telemCacheHit execCtx requestId userInfo reqHdrs opDef rsi
  where
    telemTransport = Telem.HTTP
    runHasuraGQ :: (MonadIO m, WebSocketLog m) => ExceptT () m DiffTime
                -> Telem.CacheHit -> RequestId -> GQLReqUnparsed
                -> UserInfo -> E.ExecOp -> [H.Header]
                -> ExceptT () m ()
    runHasuraGQ timerTot telemCacheHit reqId query userInfo execOp reqHdrs = case execOp of
      E.ExOpQuery opTx genSql ->
        execQueryOrMut Telem.Query genSql $ runLazyTx' pgExecCtx opTx
      E.ExOpMutation opTx ->
        execQueryOrMut Telem.Mutation Nothing $
          runLazyTx pgExecCtx Q.ReadWrite $ withUserInfo userInfo opTx
      E.ExOpSubs lqOp -> do
        -- log the graphql query
        L.unLogger logger $ QueryLog query Nothing reqId
        -- NOTE!: we mask async exceptions higher in the call stack, but it's
        -- crucial we don't lose lqId after addLiveQuery returns successfully.
        lqId <- liftIO $ LQ.addLiveQuery logger lqMap lqOp liveQOnChange
        liftIO $ STM.atomically $
          -- NOTE: see crucial `lookup` check above, ensuring this doesn't clobber:
          STMMap.insert (lqId, _grOperationName q) opId opMap
        lift $ logOpEv ODStarted (Just reqId) reqHdrs

      where
        telemLocality = Telem.Local
        execQueryOrMut telemQueryType genSql action = do
          lift $ logOpEv ODStarted (Just reqId) reqHdrs
          -- log the generated SQL and the graphql query
          L.unLogger logger $ QueryLog query genSql reqId
          withElapsedTime (liftIO $ runExceptT action) >>= \case
            (_,      Left err) -> lift $ postExecErr reqId err
            (telemTimeIO_DT, Right encJson) -> do
              -- Telemetry. NOTE: don't time network IO:
              telemTimeTot <- Seconds <$> timerTot
              lift $ sendSuccResp encJson $ LQ.LiveQueryMetadata telemTimeIO_DT
              let telemTimeIO = fromUnits telemTimeIO_DT
              Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}

          lift $ sendCompleted (Just reqId)

    runRemoteGQ :: (MonadIO m, WebSocketLog m) => ExceptT () m DiffTime
                -> Telem.CacheHit -> E.ExecutionCtx -> RequestId -> UserInfo -> [H.Header]
                -> G.TypedOperationDefinition -> RemoteSchemaInfo
                -> ExceptT () m ()
    runRemoteGQ timerTot telemCacheHit execCtx reqId userInfo reqHdrs opDef rsi = do
      let telemLocality = Telem.Remote
      telemQueryType <- case G._todType opDef of
        G.OperationTypeSubscription ->
          withComplete $ lift $ preExecErr reqId $
          err400 NotSupported "subscription to remote server is not supported"
        G.OperationTypeMutation -> return Telem.Mutation
        G.OperationTypeQuery    -> return Telem.Query

      -- if it's not a subscription, use HTTP to execute the query on the remote
      runExceptT (flip runReaderT execCtx $
        E.execRemoteGQ reqId userInfo reqHdrs q rsi opDef) >>= \case
          Left  err           -> lift $ postExecErr reqId err
          Right (telemTimeIO_DT, !val) -> do
            -- Telemetry. NOTE: don't time network IO:
            telemTimeTot <- Seconds <$> timerTot
            lift $ sendRemoteResp reqId (_hrBody val) $ LQ.LiveQueryMetadata telemTimeIO_DT
            let telemTimeIO = fromUnits telemTimeIO_DT
            Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}

      lift $ sendCompleted (Just reqId)

    sendRemoteResp :: (MonadIO m, WebSocketLog m) => RequestId -> EncJSON -> LQ.LiveQueryMetadata -> m ()
    sendRemoteResp reqId resp meta =
      case J.eitherDecodeStrict (encJToBS resp) of
        Left e    -> postExecErr reqId $ invalidGqlErr $ T.pack e
        Right res -> sendMsgWithMetadata wsConn (SMData $ DataMsg opId $ GRRemote res) meta

    invalidGqlErr err = err500 Unexpected $
      "Failed parsing GraphQL response from remote: " <> err

    WSServerEnv logger pgExecCtx lqMap getSchemaCache httpMgr _ sqlGenCtx planCache
      _ enableAL = serverEnv

    WSConnData userInfoR opMap errRespTy = WS.getData wsConn

    logOpEv :: (MonadIO m, WebSocketLog m) => OpDetail -> Maybe RequestId -> [H.Header] -> m ()
    logOpEv opTy reqId reqHdrs =
      logWSEvent logger wsConn (EOperation opDet) reqHdrs
      where
        opDet = OperationDetails opId reqId (_grOperationName q) opTy query
        -- log the query only in errors
        query = case opTy of
          ODQueryErr _ -> Just q
          _            -> Nothing

    getErrFn errTy =
      case errTy of
        ERTLegacy           -> encodeQErr
        ERTGraphqlCompliant -> encodeGQLErr

    sendStartErr :: (MonadIO m, WebSocketLog m) => Text -> m ()
    sendStartErr e = do
      let errFn = getErrFn errRespTy
      sendMsg wsConn $
        SMErr $ ErrorMsg opId $ errFn False $ err400 StartFailed e
      logOpEv (ODProtoErr e) Nothing []

    sendCompleted :: (MonadIO m, WebSocketLog m) => Maybe RequestId -> m ()
    sendCompleted reqId = do
      sendMsg wsConn (SMComplete $ CompletionMsg opId)
      logOpEv ODCompleted reqId []

    postExecErr :: (MonadIO m, WebSocketLog m) => RequestId -> QErr -> m ()
    postExecErr reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId) []
      sendMsg wsConn $ SMData $
        DataMsg opId $ GRHasura $ GQExecError $ pure $ errFn False qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr :: (MonadIO m, WebSocketLog m) => RequestId -> QErr -> m ()
    preExecErr reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId) []
      let err = case errRespTy of
            ERTLegacy           -> errFn False qErr
            ERTGraphqlCompliant -> J.object ["errors" J..= [errFn False qErr]]
      sendMsg wsConn (SMErr $ ErrorMsg opId err)

    sendSuccResp :: MonadIO m => EncJSON -> LQ.LiveQueryMetadata -> m ()
    sendSuccResp encJson =
      sendMsgWithMetadata wsConn
        (SMData $ DataMsg opId $ GRHasura $ GQSuccess $ encJToLBS encJson)

    withComplete :: (MonadIO m, WebSocketLog m) => ExceptT () m () -> ExceptT () m a
    withComplete action = do
      action
      lift $ sendCompleted Nothing
      throwError ()

    -- on change, send message on the websocket
    liveQOnChange :: LQ.OnChange
    liveQOnChange (GQSuccess (LQ.LiveQueryResponse bs dTime)) =
      sendMsgWithMetadata wsConn (SMData $ DataMsg opId $ GRHasura $ GQSuccess bs) $
        LQ.LiveQueryMetadata dTime
    liveQOnChange resp = sendMsg wsConn $ SMData $ DataMsg opId $ GRHasura $
      LQ._lqrPayload <$> resp

    catchAndIgnore :: MonadIO m => ExceptT () m () -> m ()
    catchAndIgnore m = void $ runExceptT m

onMessage
  :: (HasVersion, MonadIO m, UserAuthentication m, WebSocketLog m)
  => AuthMode
  -> WSServerEnv
  -> WSConn -> BL.ByteString -> m ()
onMessage authMode serverEnv wsConn msgRaw =
  case J.eitherDecode msgRaw of
    Left e    -> do
      let err = ConnErrMsg $ "parsing ClientMessage failed: " <> T.pack e
      logWSEvent logger wsConn (EConnErr err) []
      sendMsg wsConn $ SMConnErr err

    Right msg -> case msg of
      CMConnInit params -> onConnInit (_wseLogger serverEnv)
                           (_wseHManager serverEnv)
                           wsConn authMode params
      CMStart startMsg  -> onStart serverEnv wsConn startMsg
      CMStop stopMsg    -> onStop serverEnv wsConn stopMsg
      -- The idea is cleanup will be handled by 'onClose', but...
      -- NOTE: we need to close the websocket connection when we receive the
      -- CMConnTerm message and calling WS.closeConn will definitely throw an
      -- exception, but I'm not sure if 'closeConn' is the correct thing here....
      CMConnTerm        -> liftIO $ WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = _wseLogger serverEnv

onStop :: (MonadIO m, WebSocketLog m) => WSServerEnv -> WSConn -> StopMsg -> m ()
onStop serverEnv wsConn (StopMsg opId) = do
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  case opM of
    Just (lqId, opNameM) -> do
      logWSEvent logger wsConn (EOperation $ opDet opNameM) []
      liftIO $ LQ.removeLiveQuery logger lqMap lqId
    Nothing    ->
      L.unLogger logger $ L.UnstructuredLog L.LevelError $ fromString $
        "Received STOP for an operation "<>(show opId)<>" we have no record for. "<>
        "this could be a misbehaving client or a bug"
  liftIO $ STM.atomically $ STMMap.delete opId opMap
  where
    logger = _wseLogger serverEnv
    lqMap  = _wseLiveQMap serverEnv
    opMap  = _wscOpMap $ WS.getData wsConn
    opDet n = OperationDetails opId Nothing n ODStopped Nothing

logWSEvent
  :: (MonadIO m, WebSocketLog m)
  => L.Logger L.Hasura -> WSConn -> WSEvent -> [H.Header] -> m ()
logWSEvent hLogger wsConn wsEv reqHdrs = do
  userInfoME <- liftIO $ STM.readTVarIO userInfoR
  let (userVarsM, jwtExpM) = case userInfoME of
        CSInitialised userInfo jwtM _ -> ( Just $ userVars userInfo
                                         , jwtM
                                         )
        _                             -> (Nothing, Nothing)
  logWebSocket hLogger logLevel userVarsM (WSConnInfo wsId jwtExpM Nothing) wsEv reqHdrs
  where
    WSConnData userInfoR _ _ = WS.getData wsConn
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

onConnInit
  :: (HasVersion, MonadIO m, UserAuthentication m, WebSocketLog m)
  => L.Logger L.Hasura -> H.Manager -> WSConn -> AuthMode -> Maybe ConnParams -> m ()
onConnInit logger manager wsConn authMode connParamsM = do
  headers <- mkHeaders <$> liftIO (STM.readTVarIO (_wscUser $ WS.getData wsConn))
  res <- resolveUserInfo logger manager headers authMode
  case res of
    Left e  -> do
      liftIO $ STM.atomically $ STM.writeTVar (_wscUser $ WS.getData wsConn) $
        CSInitError $ qeError e
      let connErr = ConnErrMsg $ qeError e
      logWSEvent logger wsConn (EConnErr connErr) []
      sendMsg wsConn $ SMConnErr connErr
    Right (userInfo, expTimeM) -> do
      liftIO $ STM.atomically $ STM.writeTVar (_wscUser $ WS.getData wsConn) $
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
      CSNotInitialised h -> unWSHeaders h
      _                  -> []

onClose
  :: (MonadIO m, WebSocketLog m)
  => L.Logger L.Hasura
  -> LQ.LiveQueriesState
  -> WSConn
  -> m ()
onClose logger lqMap wsConn = do
  logWSEvent logger wsConn EClosed []
  operations <- liftIO $ STM.atomically $ ListT.toList $ STMMap.listT opMap
  liftIO $ for_ operations $ \(_, (lqId, _)) ->
    LQ.removeLiveQuery logger lqMap lqId
  where
    opMap = _wscOpMap $ WS.getData wsConn

createWSServerEnv
  :: (MonadIO m)
  => L.Logger L.Hasura
  -> PGExecCtx
  -> LQ.LiveQueriesState
  -> IO (SchemaCache, SchemaCacheVer)
  -> H.Manager
  -> CorsPolicy
  -> SQLGenCtx
  -> Bool
  -> E.PlanCache
  -> m WSServerEnv
createWSServerEnv logger pgExecCtx lqState getSchemaCache httpManager
  corsPolicy sqlGenCtx enableAL planCache = do
  wsServer <- liftIO $ STM.atomically $ WS.createWSServer logger
  return $
    WSServerEnv logger pgExecCtx lqState getSchemaCache httpManager corsPolicy
    sqlGenCtx planCache wsServer enableAL

createWSServerApp
  :: ( HasVersion
     , MonadIO m
     , MC.MonadBaseControl IO m
     , LA.Forall (LA.Pure m)
     , UserAuthentication m
     , WebSocketLog m
     )
  => AuthMode
  -> WSServerEnv
  -> WS.PendingConnection -> m ()
  -- ^ aka generalized 'WS.ServerApp'
createWSServerApp authMode serverEnv =
  WS.createServerApp (_wseServer serverEnv) handlers
  where
    handlers =
      WS.WSHandlers
      -- Mask async exceptions during event processing to help maintain integrity of mutable vars:
      (\rid rh ->  mask_ $ onConn (_wseLogger serverEnv) (_wseCorsPolicy serverEnv) rid rh)
      (\conn bs -> mask_ $ onMessage authMode serverEnv conn bs)
      (\conn ->    mask_ $ onClose (_wseLogger serverEnv) (_wseLiveQMap serverEnv) conn)

stopWSServerApp :: WSServerEnv -> IO ()
stopWSServerApp wsEnv = WS.shutdown (_wseServer wsEnv)
