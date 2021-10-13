{-# LANGUAGE CPP #-}

-- | This file contains the handlers that are used within websocket server.
--
-- This module export three main handlers for the websocket server ('onConn',
-- 'onMessage', 'onClose'), and two helpers for sending messages to the client
-- ('sendMsg', 'sendCloseWithMsg').
--
-- NOTE!
--  The handler functions 'onClose', 'onMessage', etc. depend for correctness on two properties:
--    - they run with async exceptions masked
--    - they do not race on the same connection
module Hasura.GraphQL.Transport.WebSocket
  ( onConn,
    onMessage,
    onClose,
    sendMsg,
    sendCloseWithMsg,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Control qualified as MC
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.TH qualified as J
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Dependent.Map qualified as DM
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock qualified as TC
import GHC.AssertNF.CPP
import Hasura.Backends.Postgres.Instances.Transport (runPGMutationTransaction)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Action qualified as EA
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.LiveQuery.Poll qualified as LQ
import Hasura.GraphQL.Execute.LiveQuery.State qualified as LQ
import Hasura.GraphQL.Execute.RemoteJoin qualified as RJ
import Hasura.GraphQL.Logging
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Parser.Directives (cached)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP
  ( MonadExecuteQuery (..),
    QueryCacheKey (..),
    ResultsFragment (..),
    buildRaw,
    coalescePostgresMutations,
    extractFieldFromResponse,
    filterVariablesFromQuery,
    runSessVarPred,
  )
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.GraphQL.Transport.Instances ()
import Hasura.GraphQL.Transport.WebSocket.Protocol
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Types
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SchemaCache (scApiLimits)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Auth
  ( AuthMode,
    UserAuthentication,
    resolveUserInfo,
  )
import Hasura.Server.Cors
import Hasura.Server.Init.Config (KeepAliveDelay (..))
import Hasura.Server.Limits
  ( HasResourceLimits (..),
    ResourceLimits (..),
  )
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Telemetry.Counters qualified as Telem
import Hasura.Server.Types (RequestId, getRequestId)
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import ListT qualified
import Network.HTTP.Client qualified as H
import Network.HTTP.Types qualified as H
import Network.WebSockets qualified as WS
import StmContainers.Map qualified as STMMap

-- | 'LQ.LiveQueryId' comes from 'Hasura.GraphQL.Execute.LiveQuery.State.addLiveQuery'. We use
-- this to track a connection's operations so we can remove them from 'LiveQueryState', and
-- log.
--
-- NOTE!: This must be kept consistent with the global 'LiveQueryState', in 'onClose'
-- and 'onStart'.
data OpDetail
  = ODStarted
  | ODProtoErr !Text
  | ODQueryErr !QErr
  | ODCompleted
  | ODStopped
  deriving (Show, Eq)

$( J.deriveToJSON
     J.defaultOptions
       { J.constructorTagModifier = J.snakeCase . drop 2,
         J.sumEncoding = J.TaggedObject "type" "detail"
       }
     ''OpDetail
 )

data OperationDetails = OperationDetails
  { _odOperationId :: !OperationId,
    _odRequestId :: !(Maybe RequestId),
    _odOperationName :: !(Maybe OperationName),
    _odOperationType :: !OpDetail,
    _odQuery :: !(Maybe GQLReqUnparsed),
    _odParameterizedQueryHash :: !(Maybe ParameterizedQueryHash)
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''OperationDetails)

data WSEvent
  = EAccepted
  | ERejected !QErr
  | EConnErr !ConnErrMsg
  | EOperation !OperationDetails
  | EClosed
  deriving (Show, Eq)

$( J.deriveToJSON
     J.defaultOptions
       { J.constructorTagModifier = J.snakeCase . drop 1,
         J.sumEncoding = J.TaggedObject "type" "detail"
       }
     ''WSEvent
 )

data WsConnInfo = WsConnInfo
  { _wsciWebsocketId :: !WS.WSId,
    _wsciTokenExpiry :: !(Maybe TC.UTCTime),
    _wsciMsg :: !(Maybe Text)
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''WsConnInfo)

data WSLogInfo = WSLogInfo
  { _wsliUserVars :: !(Maybe SessionVariables),
    _wsliConnectionInfo :: !WsConnInfo,
    _wsliEvent :: !WSEvent
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''WSLogInfo)

data WSLog = WSLog
  { _wslLogLevel :: !L.LogLevel,
    _wslInfo :: !WSLogInfo
  }

instance L.ToEngineLog WSLog L.Hasura where
  toEngineLog (WSLog logLevel wsLog) =
    (logLevel, L.ELTWebsocketLog, J.toJSON wsLog)

mkWsInfoLog :: Maybe SessionVariables -> WsConnInfo -> WSEvent -> WSLog
mkWsInfoLog uv ci ev =
  WSLog L.LevelInfo $ WSLogInfo uv ci ev

mkWsErrorLog :: Maybe SessionVariables -> WsConnInfo -> WSEvent -> WSLog
mkWsErrorLog uv ci ev =
  WSLog L.LevelError $ WSLogInfo uv ci ev

logWSEvent ::
  (MonadIO m) =>
  L.Logger L.Hasura ->
  WSConn ->
  WSEvent ->
  m ()
logWSEvent (L.Logger logger) wsConn wsEv = do
  userInfoME <- liftIO $ STM.readTVarIO userInfoR
  let (userVarsM, tokenExpM) = case userInfoME of
        CSInitialised WsClientState {..} ->
          ( Just $ _uiSession wscsUserInfo,
            wscsTokenExpTime
          )
        _ -> (Nothing, Nothing)
  liftIO $ logger $ WSLog logLevel $ WSLogInfo userVarsM (WsConnInfo wsId tokenExpM Nothing) wsEv
  where
    WSConnData userInfoR _ _ _ = WS.getData wsConn
    wsId = WS.getWSId wsConn
    logLevel = bool L.LevelInfo L.LevelError isError
    isError = case wsEv of
      EAccepted -> False
      ERejected _ -> True
      EConnErr _ -> True
      EClosed -> False
      EOperation operation -> case _odOperationType operation of
        ODStarted -> False
        ODProtoErr _ -> True
        ODQueryErr _ -> True
        ODCompleted -> False
        ODStopped -> False

sendMsg :: (MonadIO m) => WSConn -> ServerMsg -> m ()
sendMsg wsConn msg =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse (encodeServerMsg msg) Nothing

sendCloseWithMsg ::
  (MonadIO m) =>
  L.Logger L.Hasura ->
  WSConn ->
  ServerErrorCode ->
  Maybe ServerMsg ->
  m ()
sendCloseWithMsg logger wsConn errCode mErrServerMsg = do
  case mErrServerMsg of
    Just errServerMsg -> do
      sendMsg wsConn errServerMsg
    Nothing -> pure ()
  logWSEvent logger wsConn EClosed
  liftIO $ WS.sendClose wsc errMsg
  where
    wsc = WS.getRawWebSocketConnection wsConn
    errMsg = encodeServerErrorMsg errCode

sendMsgWithMetadata ::
  (MonadIO m) =>
  WSConn ->
  ServerMsg ->
  Maybe OperationName ->
  Maybe ParameterizedQueryHash ->
  LQ.LiveQueryMetadata ->
  m ()
sendMsgWithMetadata wsConn msg opName paramQueryHash (LQ.LiveQueryMetadata execTime) =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse bs wsInfo
  where
    bs = encodeServerMsg msg
    (msgType, operationId) = case msg of
      (SMNext (DataMsg opId _)) -> (Just SMT_GQL_NEXT, Just opId)
      (SMData (DataMsg opId _)) -> (Just SMT_GQL_DATA, Just opId)
      _ -> (Nothing, Nothing)
    wsInfo =
      Just
        $! WS.WSEventInfo
          { WS._wseiEventType = msgType,
            WS._wseiOperationId = operationId,
            WS._wseiOperationName = opName,
            WS._wseiQueryExecutionTime = Just $! realToFrac execTime,
            WS._wseiResponseSize = Just $! LBS.length bs,
            WS._wseiParameterizedQueryHash = paramQueryHash
          }

onConn ::
  (MonadIO m, MonadReader WSServerEnv m) =>
  WS.OnConnH m WSConnData
onConn wsId requestHead ipAddress onConnHActions = do
  res <- runExceptT $ do
    (errType, queryType) <- checkPath
    let reqHdrs = WS.requestHeaders requestHead
    headers <- maybe (return reqHdrs) (flip enforceCors reqHdrs . snd) getOrigin
    return (WsHeaders $ filterWsHeaders headers, errType, queryType)
  either reject accept res
  where
    kaAction = WS._wsaKeepAliveAction onConnHActions
    acceptRequest = WS._wsaAcceptRequest onConnHActions

    -- NOTE: the "Keep-Alive" delay is something that's mentioned
    -- in the Apollo spec. For 'graphql-ws', we're using the Ping
    -- messages that are part of the spec.
    keepAliveAction keepAliveDelay wsConn = do
      liftIO $
        forever $ do
          kaAction wsConn
          sleep $ seconds (unKeepAliveDelay keepAliveDelay)

    tokenExpiryHandler wsConn = do
      expTime <- liftIO $
        STM.atomically $ do
          connState <- STM.readTVar $ (_wscUser . WS.getData) wsConn
          case connState of
            CSNotInitialised _ _ -> STM.retry
            CSInitError _ -> STM.retry
            CSInitialised clientState -> onNothing (wscsTokenExpTime clientState) STM.retry
      currTime <- TC.getCurrentTime
      sleep $ convertDuration $ TC.diffUTCTime expTime currTime

    accept (hdrs, errType, queryType) = do
      (L.Logger logger) <- asks _wseLogger
      keepAliveDelay <- asks _wseKeepAliveDelay
      logger $ mkWsInfoLog Nothing (WsConnInfo wsId Nothing Nothing) EAccepted
      connData <-
        liftIO $
          WSConnData
            <$> STM.newTVarIO (CSNotInitialised hdrs ipAddress)
            <*> STMMap.newIO
            <*> pure errType
            <*> pure queryType

      pure $
        Right $
          WS.AcceptWith
            connData
            acceptRequest
            (keepAliveAction keepAliveDelay)
            tokenExpiryHandler

    reject qErr = do
      (L.Logger logger) <- asks _wseLogger
      logger $ mkWsErrorLog Nothing (WsConnInfo wsId Nothing Nothing) (ERejected qErr)
      return $
        Left $
          WS.RejectRequest
            (H.statusCode $ qeStatus qErr)
            (H.statusMessage $ qeStatus qErr)
            []
            (LBS.toStrict $ J.encode $ encodeGQLErr False qErr)

    checkPath = case WS.requestPath requestHead of
      "/v1alpha1/graphql" -> return (ERTLegacy, E.QueryHasura)
      "/v1/graphql" -> return (ERTGraphqlCompliant, E.QueryHasura)
      "/v1beta1/relay" -> return (ERTGraphqlCompliant, E.QueryRelay)
      _ ->
        throw404 "only '/v1/graphql', '/v1alpha1/graphql' and '/v1beta1/relay' are supported on websockets"

    getOrigin =
      find ((==) "Origin" . fst) (WS.requestHeaders requestHead)

    enforceCors origin reqHdrs = do
      (L.Logger logger) <- asks _wseLogger
      corsPolicy <- asks _wseCorsPolicy
      case cpConfig corsPolicy of
        CCAllowAll -> return reqHdrs
        CCDisabled readCookie ->
          if readCookie
            then return reqHdrs
            else do
              lift $ logger $ mkWsInfoLog Nothing (WsConnInfo wsId Nothing (Just corsNote)) EAccepted
              return $ filter (\h -> fst h /= "Cookie") reqHdrs
        CCAllowedOrigins ds
          -- if the origin is in our cors domains, no error
          | bsToTxt origin `elem` dmFqdns ds -> return reqHdrs
          -- if current origin is part of wildcard domain list, no error
          | inWildcardList ds (bsToTxt origin) -> return reqHdrs
          -- otherwise error
          | otherwise -> corsErr

    filterWsHeaders hdrs = flip filter hdrs $ \(n, _) ->
      n
        `notElem` [ "sec-websocket-key",
                    "sec-websocket-version",
                    "upgrade",
                    "connection"
                  ]

    corsErr =
      throw400
        AccessDenied
        "received origin header does not match configured CORS domains"

    corsNote =
      "Cookie is not read when CORS is disabled, because it is a potential "
        <> "security issue. If you're already handling CORS before Hasura and enforcing "
        <> "CORS on websocket connections, then you can use the flag --ws-read-cookie or "
        <> "HASURA_GRAPHQL_WS_READ_COOKIE to force read cookie when CORS is disabled."

onStart ::
  forall m.
  ( MonadIO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    Tracing.MonadTrace m,
    MonadExecuteQuery m,
    MC.MonadBaseControl IO m,
    MonadMetadataStorage (MetadataStorageT m),
    EB.MonadQueryTags m,
    HasResourceLimits m
  ) =>
  Env.Environment ->
  HashSet (L.EngineLogType L.Hasura) ->
  WSServerEnv ->
  WSConn ->
  StartMsg ->
  WS.WSActions WSConnData ->
  m ()
onStart env enabledLogTypes serverEnv wsConn (StartMsg opId q) onMessageActions = catchAndIgnore $ do
  timerTot <- startTimer
  op <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  let opName = _grOperationName q

  -- NOTE: it should be safe to rely on this check later on in this function, since we expect that
  -- we process all operations on a websocket connection serially:
  when (isJust op) $
    withComplete $
      sendStartErr $
        "an operation already exists with this id: " <> unOperationId opId

  userInfoM <- liftIO $ STM.readTVarIO userInfoR
  (userInfo, origReqHdrs, ipAddress) <- case userInfoM of
    CSInitialised WsClientState {..} -> return (wscsUserInfo, wscsReqHeaders, wscsIpAddress)
    CSInitError initErr -> do
      let e = "cannot start as connection_init failed with : " <> initErr
      withComplete $ sendStartErr e
    CSNotInitialised _ _ -> do
      let e = "start received before the connection is initialised"
      withComplete $ sendStartErr e

  (requestId, reqHdrs) <- getRequestId origReqHdrs
  (sc, scVer) <- liftIO getSchemaCache

  operationLimit <- askGraphqlOperationLimit
  let runLimits ::
        ExceptT (Either GQExecError QErr) (ExceptT () m) a ->
        ExceptT (Either GQExecError QErr) (ExceptT () m) a
      runLimits = withErr Right $ runResourceLimits $ operationLimit userInfo (scApiLimits sc)

  reqParsedE <- lift $ E.checkGQLExecution userInfo (reqHdrs, ipAddress) enableAL sc q
  reqParsed <- onLeft reqParsedE (withComplete . preExecErr requestId)
  execPlanE <-
    runExceptT $
      E.getResolvedExecPlan
        env
        logger
        userInfo
        sqlGenCtx
        sc
        scVer
        queryType
        httpMgr
        reqHdrs
        (q, reqParsed)
        requestId

  (parameterizedQueryHash, execPlan) <- onLeft execPlanE (withComplete . preExecErr requestId)

  case execPlan of
    E.QueryExecutionPlan queryPlan asts dirMap -> Tracing.trace "Query" $ do
      let filteredSessionVars = runSessVarPred (filterVariablesFromQuery asts) (_uiSession userInfo)
          cacheKey = QueryCacheKey reqParsed (_uiRole userInfo) filteredSessionVars
          remoteSchemas =
            OMap.elems queryPlan >>= \case
              E.ExecStepDB _remoteHeaders _ remoteJoins ->
                maybe [] (map RJ._rsjRemoteSchema . RJ.getRemoteSchemaJoins) remoteJoins
              _ -> []
          actionsInfo =
            foldl getExecStepActionWithActionInfo [] $
              OMap.elems $
                OMap.filter
                  ( \case
                      E.ExecStepAction _ _ _remoteJoins -> True
                      _ -> False
                  )
                  queryPlan
          cachedDirective = runIdentity <$> DM.lookup cached dirMap

      -- We ignore the response headers (containing TTL information) because
      -- WebSockets don't support them.
      (_responseHeaders, cachedValue) <- Tracing.interpTraceT (withExceptT mempty) $ cacheLookup remoteSchemas actionsInfo cacheKey cachedDirective
      case cachedValue of
        Just cachedResponseData -> do
          logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindCached
          sendSuccResp cachedResponseData opName parameterizedQueryHash $ LQ.LiveQueryMetadata 0
        Nothing -> do
          conclusion <- runExceptT $
            runLimits $
              forWithKey queryPlan $ \fieldName -> \case
                E.ExecStepDB _headers exists remoteJoins -> doQErr $ do
                  (telemTimeIO_DT, resp) <-
                    AB.dispatchAnyBackend @BackendTransport
                      exists
                      \(EB.DBStepInfo _ sourceConfig genSql tx :: EB.DBStepInfo b) ->
                        runDBQuery @b
                          requestId
                          q
                          fieldName
                          userInfo
                          logger
                          sourceConfig
                          tx
                          genSql
                  finalResponse <-
                    RJ.processRemoteJoins requestId logger env httpMgr reqHdrs userInfo resp remoteJoins q
                  pure $ ResultsFragment telemTimeIO_DT Telem.Local finalResponse []
                E.ExecStepRemote rsi resultCustomizer gqlReq -> do
                  logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindRemoteSchema
                  runRemoteGQ fieldName userInfo reqHdrs rsi resultCustomizer gqlReq
                E.ExecStepAction actionExecPlan _ remoteJoins -> do
                  logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindAction
                  (time, (resp, _)) <- doQErr $ do
                    (time, (resp, hdrs)) <- EA.runActionExecution userInfo actionExecPlan
                    finalResponse <-
                      RJ.processRemoteJoins requestId logger env httpMgr reqHdrs userInfo resp remoteJoins q
                    pure (time, (finalResponse, hdrs))
                  pure $ ResultsFragment time Telem.Empty resp []
                E.ExecStepRaw json -> do
                  logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindIntrospection
                  buildRaw json
          sendResultFromFragments Telem.Query timerTot requestId conclusion opName parameterizedQueryHash
          case conclusion of
            Left _ -> pure ()
            Right results -> do
              -- Note: The result of cacheStore is ignored here since we can't ensure that
              --       the WS client will respond correctly to multiple messages.
              void $
                Tracing.interpTraceT (withExceptT mempty) $
                  cacheStore cacheKey cachedDirective $
                    encJFromInsOrdHashMap $
                      rfResponse <$> OMap.mapKeys G.unName results

      liftIO $ sendCompleted (Just requestId) (Just parameterizedQueryHash)
    E.MutationExecutionPlan mutationPlan -> do
      -- See Note [Backwards-compatible transaction optimisation]
      case coalescePostgresMutations mutationPlan of
        -- we are in the aforementioned case; we circumvent the normal process
        Just (sourceConfig, pgMutations) -> do
          resp <-
            runExceptT $
              runLimits $
                doQErr $
                  runPGMutationTransaction requestId q userInfo logger sourceConfig pgMutations
          -- we do not construct result fragments since we have only one result
          handleResult requestId resp \(telemTimeIO_DT, results) -> do
            let telemQueryType = Telem.Query
                telemLocality = Telem.Local
                telemTimeIO = convertDuration telemTimeIO_DT
            telemTimeTot <- Seconds <$> timerTot
            sendSuccResp (encJFromInsOrdHashMap $ OMap.mapKeys G.unName results) opName parameterizedQueryHash $
              LQ.LiveQueryMetadata telemTimeIO_DT
            -- Telemetry. NOTE: don't time network IO:
            Telem.recordTimingMetric Telem.RequestDimensions {..} Telem.RequestTimings {..}

        -- we are not in the transaction case; proceeding normally
        Nothing -> do
          conclusion <- runExceptT $
            runLimits $
              forWithKey mutationPlan $ \fieldName -> \case
                -- Ignoring response headers since we can't send them over WebSocket
                E.ExecStepDB _responseHeaders exists remoteJoins -> doQErr $ do
                  (telemTimeIO_DT, resp) <-
                    AB.dispatchAnyBackend @BackendTransport
                      exists
                      \(EB.DBStepInfo _ sourceConfig genSql tx :: EB.DBStepInfo b) ->
                        runDBMutation @b
                          requestId
                          q
                          fieldName
                          userInfo
                          logger
                          sourceConfig
                          tx
                          genSql
                  finalResponse <-
                    RJ.processRemoteJoins requestId logger env httpMgr reqHdrs userInfo resp remoteJoins q
                  pure $ ResultsFragment telemTimeIO_DT Telem.Local finalResponse []
                E.ExecStepAction actionExecPlan _ remoteJoins -> do
                  logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindAction
                  (time, (resp, hdrs)) <- doQErr $ do
                    (time, (resp, hdrs)) <- EA.runActionExecution userInfo actionExecPlan
                    finalResponse <-
                      RJ.processRemoteJoins requestId logger env httpMgr reqHdrs userInfo resp remoteJoins q
                    pure (time, (finalResponse, hdrs))
                  pure $ ResultsFragment time Telem.Empty resp $ fromMaybe [] hdrs
                E.ExecStepRemote rsi resultCustomizer gqlReq -> do
                  logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindRemoteSchema
                  runRemoteGQ fieldName userInfo reqHdrs rsi resultCustomizer gqlReq
                E.ExecStepRaw json -> do
                  logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindIntrospection
                  buildRaw json
          sendResultFromFragments Telem.Query timerTot requestId conclusion opName parameterizedQueryHash
      liftIO $ sendCompleted (Just requestId) (Just parameterizedQueryHash)
    E.SubscriptionExecutionPlan subExec -> do
      case subExec of
        E.SEAsyncActionsWithNoRelationships actions -> do
          logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindAction
          liftIO do
            let allActionIds = map fst $ OMap.elems actions
            case NE.nonEmpty allActionIds of
              Nothing -> sendCompleted (Just requestId) (Just parameterizedQueryHash)
              Just actionIds -> do
                let sendResponseIO actionLogMap = do
                      (dTime, resultsE) <- withElapsedTime $
                        runExceptT $
                          for actions $ \(actionId, resultBuilder) -> do
                            actionLogResponse <-
                              Map.lookup actionId actionLogMap
                                `onNothing` throw500 "unexpected: cannot lookup action_id in response map"
                            liftEither $ resultBuilder actionLogResponse
                      case resultsE of
                        Left err -> sendError requestId err
                        Right results -> do
                          let dataMsg =
                                sendDataMsg $
                                  DataMsg opId $
                                    pure $
                                      encJToLBS $
                                        encJFromInsOrdHashMap $ OMap.mapKeys G.unName results
                          sendMsgWithMetadata wsConn dataMsg opName (Just parameterizedQueryHash) $ LQ.LiveQueryMetadata dTime

                    asyncActionQueryLive =
                      LQ.LAAQNoRelationships $
                        LQ.LiveAsyncActionQueryWithNoRelationships sendResponseIO (sendCompleted (Just requestId) (Just parameterizedQueryHash))

                LQ.addAsyncActionLiveQuery
                  (LQ._lqsAsyncActions lqMap)
                  opId
                  actionIds
                  (sendError requestId)
                  asyncActionQueryLive
        E.SEOnSourceDB actionIds liveQueryBuilder -> do
          actionLogMapE <- fmap fst <$> runExceptT (EA.fetchActionLogResponses actionIds)
          actionLogMap <- onLeft actionLogMapE (withComplete . preExecErr requestId)
          lqIdE <- liftIO $ startLiveQuery liveQueryBuilder parameterizedQueryHash requestId actionLogMap
          lqId <- onLeft lqIdE (withComplete . preExecErr requestId)

          -- Update async action query subscription state
          case NE.nonEmpty (toList actionIds) of
            Nothing -> do
              logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindDatabase
              -- No async action query fields present, do nothing.
              pure ()
            Just nonEmptyActionIds -> do
              logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindAction
              liftIO $ do
                let asyncActionQueryLive =
                      LQ.LAAQOnSourceDB $
                        LQ.LiveAsyncActionQueryOnSource lqId actionLogMap $
                          restartLiveQuery parameterizedQueryHash requestId liveQueryBuilder

                    onUnexpectedException err = do
                      sendError requestId err
                      stopOperation serverEnv wsConn opId (pure ()) -- Don't log in case opId don't exist
                LQ.addAsyncActionLiveQuery
                  (LQ._lqsAsyncActions lqMap)
                  opId
                  nonEmptyActionIds
                  onUnexpectedException
                  asyncActionQueryLive

      liftIO $ logOpEv ODStarted (Just requestId) (Just parameterizedQueryHash)
  where
    sendDataMsg = WS._wsaGetDataMessageType onMessageActions
    closeConnAction = WS._wsaConnectionCloseAction onMessageActions
    postExecErrAction = WS._wsaPostExecErrMessageAction onMessageActions

    getExecStepActionWithActionInfo acc execStep = case execStep of
      E.ExecStepAction _ actionInfo _remoteJoins -> (actionInfo : acc)
      _ -> acc

    doQErr ::
      Monad n =>
      ExceptT QErr n a ->
      ExceptT (Either GQExecError QErr) n a
    doQErr = withExceptT Right

    withErr ::
      forall e f n a.
      Monad n =>
      (e -> f) ->
      (ExceptT e (ExceptT f n) a -> ExceptT e (ExceptT f n) a) ->
      ExceptT f n a ->
      ExceptT f n a
    withErr embed f action = do
      res <- runExceptT $ f $ lift action
      onLeft res (\e -> throwError $ embed e)

    forWithKey = flip OMap.traverseWithKey

    telemTransport = Telem.WebSocket

    handleResult ::
      forall a.
      RequestId ->
      Either (Either GQExecError QErr) a ->
      (a -> ExceptT () m ()) ->
      ExceptT () m ()
    handleResult requestId r f = case r of
      Left (Left err) -> postExecErr' err
      Left (Right err) -> postExecErr requestId err
      Right results -> f results

    sendResultFromFragments telemQueryType timerTot requestId r opName pqh =
      handleResult requestId r \results -> do
        let telemLocality = foldMap rfLocality results
            telemTimeIO = convertDuration $ sum $ fmap rfTimeIO results
        telemTimeTot <- Seconds <$> timerTot
        sendSuccResp (encJFromInsOrdHashMap (fmap rfResponse (OMap.mapKeys G.unName results))) opName pqh $
          LQ.LiveQueryMetadata $ sum $ fmap rfTimeIO results
        -- Telemetry. NOTE: don't time network IO:
        Telem.recordTimingMetric Telem.RequestDimensions {..} Telem.RequestTimings {..}

    runRemoteGQ ::
      G.Name ->
      UserInfo ->
      [H.Header] ->
      RemoteSchemaInfo ->
      RemoteResultCustomizer ->
      GQLReqOutgoing ->
      ExceptT (Either GQExecError QErr) (ExceptT () m) ResultsFragment
    runRemoteGQ fieldName userInfo reqHdrs rsi resultCustomizer gqlReq = do
      (telemTimeIO_DT, _respHdrs, resp) <-
        doQErr $
          E.execRemoteGQ env httpMgr userInfo reqHdrs (rsDef rsi) gqlReq
      value <- mapExceptT lift $ extractFieldFromResponse fieldName rsi resultCustomizer resp
      return $ ResultsFragment telemTimeIO_DT Telem.Remote (encJFromOrderedValue value) []

    WSServerEnv
      logger
      lqMap
      getSchemaCache
      httpMgr
      _
      sqlGenCtx
      _
      enableAL
      _keepAliveDelay
      _connInitTime = serverEnv

    WSConnData userInfoR opMap errRespTy queryType = WS.getData wsConn

    logOpEv opTy reqId parameterizedQueryHash =
      -- See Note [Disable query printing when query-log is disabled]
      let queryToLog = bool Nothing (Just q) (Set.member L.ELTQueryLog enabledLogTypes)
       in logWSEvent logger wsConn $
            EOperation $
              OperationDetails opId reqId (_grOperationName q) opTy queryToLog parameterizedQueryHash

    getErrFn ERTLegacy = encodeQErr
    getErrFn ERTGraphqlCompliant = encodeGQLErr

    sendStartErr e = do
      let errFn = getErrFn errRespTy
      sendMsg wsConn $
        SMErr $ ErrorMsg opId $ errFn False $ err400 StartFailed e
      liftIO $ logOpEv (ODProtoErr e) Nothing Nothing
      liftIO $ closeConnAction wsConn opId (T.unpack e)

    sendCompleted reqId paramQueryHash = do
      sendMsg wsConn (SMComplete . CompletionMsg $ opId)
      logOpEv ODCompleted reqId paramQueryHash

    postExecErr :: RequestId -> QErr -> ExceptT () m ()
    postExecErr reqId qErr = do
      let errFn = getErrFn errRespTy False
      liftIO $ logOpEv (ODQueryErr qErr) (Just reqId) Nothing
      postExecErr' $ GQExecError $ pure $ errFn qErr

    postExecErr' :: GQExecError -> ExceptT () m ()
    postExecErr' qErr = liftIO $ postExecErrAction wsConn opId qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr reqId qErr = liftIO $ sendError reqId qErr

    sendError reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId) Nothing
      let err = case errRespTy of
            ERTLegacy -> errFn False qErr
            ERTGraphqlCompliant -> J.object ["errors" J..= [errFn False qErr]]
      sendMsg wsConn (SMErr $ ErrorMsg opId err)

    sendSuccResp ::
      EncJSON ->
      Maybe OperationName ->
      ParameterizedQueryHash ->
      LQ.LiveQueryMetadata ->
      ExceptT () m ()
    sendSuccResp encJson opName queryHash =
      sendMsgWithMetadata
        wsConn
        (sendDataMsg $ DataMsg opId $ pure $ encJToLBS encJson)
        opName
        (Just queryHash)

    withComplete :: ExceptT () m () -> ExceptT () m a
    withComplete action = do
      action
      liftIO $ sendCompleted Nothing Nothing
      throwError ()

    restartLiveQuery parameterizedQueryHash requestId liveQueryBuilder lqId actionLogMap = do
      LQ.removeLiveQuery logger (_wseServerMetrics serverEnv) lqMap lqId
      either (const Nothing) Just <$> startLiveQuery liveQueryBuilder parameterizedQueryHash requestId actionLogMap

    startLiveQuery liveQueryBuilder parameterizedQueryHash requestId actionLogMap = do
      liveQueryE <- runExceptT $ liveQueryBuilder actionLogMap
      for liveQueryE $ \(sourceName, E.LQP exists) -> do
        let !opName = _grOperationName q
            subscriberMetadata = LQ.mkSubscriberMetadata (WS.getWSId wsConn) opId opName requestId
        -- NOTE!: we mask async exceptions higher in the call stack, but it's
        -- crucial we don't lose lqId after addLiveQuery returns successfully.
        !lqId <- liftIO $ AB.dispatchAnyBackend @BackendTransport
          exists
          \(E.MultiplexedLiveQueryPlan liveQueryPlan) ->
            LQ.addLiveQuery
              logger
              (_wseServerMetrics serverEnv)
              subscriberMetadata
              lqMap
              sourceName
              parameterizedQueryHash
              opName
              requestId
              liveQueryPlan
              (liveQOnChange opName parameterizedQueryHash)
        liftIO $ $assertNFHere (lqId, opName) -- so we don't write thunks to mutable vars
        STM.atomically $
          -- NOTE: see crucial `lookup` check above, ensuring this doesn't clobber:
          STMMap.insert (lqId, opName) opId opMap
        pure lqId

    -- on change, send message on the websocket
    liveQOnChange :: Maybe OperationName -> ParameterizedQueryHash -> LQ.OnChange
    liveQOnChange opName queryHash = \case
      Right (LQ.LiveQueryResponse bs dTime) ->
        sendMsgWithMetadata
          wsConn
          (sendDataMsg $ DataMsg opId $ pure $ LBS.fromStrict bs)
          opName
          (Just queryHash)
          (LQ.LiveQueryMetadata dTime)
      resp ->
        sendMsg wsConn $
          sendDataMsg $ DataMsg opId $ LBS.fromStrict . LQ._lqrPayload <$> resp

    catchAndIgnore :: ExceptT () m () -> m ()
    catchAndIgnore m = void $ runExceptT m

onMessage ::
  ( MonadIO m,
    UserAuthentication (Tracing.TraceT m),
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    Tracing.HasReporter m,
    MonadExecuteQuery m,
    MC.MonadBaseControl IO m,
    MonadMetadataStorage (MetadataStorageT m),
    EB.MonadQueryTags m,
    HasResourceLimits m
  ) =>
  Env.Environment ->
  HashSet (L.EngineLogType L.Hasura) ->
  AuthMode ->
  WSServerEnv ->
  WSConn ->
  LBS.ByteString ->
  WS.WSActions WSConnData ->
  m ()
onMessage env enabledLogTypes authMode serverEnv wsConn msgRaw onMessageActions = Tracing.runTraceT "websocket" do
  case J.eitherDecode msgRaw of
    Left e -> do
      let err = ConnErrMsg $ "parsing ClientMessage failed: " <> T.pack e
      logWSEvent logger wsConn $ EConnErr err
      liftIO $ onErrAction wsConn err WS.onClientMessageParseErrorText
    Right msg -> case msg of
      -- common to both protocols
      CMConnInit params ->
        onConnInit
          logger
          (_wseHManager serverEnv)
          wsConn
          authMode
          params
          onErrAction
          keepAliveMessageAction
      CMStart startMsg -> onStart env enabledLogTypes serverEnv wsConn startMsg onMessageActions
      CMStop stopMsg -> onStop serverEnv wsConn stopMsg
      -- specfic to graphql-ws
      CMPing mPayload -> onPing wsConn mPayload
      CMPong mPayload -> onPong wsConn mPayload
      -- specific to apollo clients
      CMConnTerm -> liftIO $ WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = _wseLogger serverEnv
    onErrAction = WS._wsaOnErrorMessageAction onMessageActions
    keepAliveMessageAction = WS._wsaKeepAliveAction onMessageActions

onPing :: (MonadIO m) => WSConn -> Maybe PingPongPayload -> m ()
onPing wsConn mPayload =
  liftIO $ sendMsg wsConn (SMPong mPayload)

onPong :: (MonadIO m) => WSConn -> Maybe PingPongPayload -> m ()
onPong wsConn mPayload = liftIO $ case mPayload of
  Just message -> do
    when (message /= keepAliveMessage) $
      sendMsg wsConn (SMPing mPayload)
  -- NOTE: this is done to avoid sending Ping for every "keepalive" that the server sends
  Nothing -> sendMsg wsConn $ SMPing Nothing

onStop :: (MonadIO m) => WSServerEnv -> WSConn -> StopMsg -> m ()
onStop serverEnv wsConn (StopMsg opId) = liftIO $ do
  -- When a stop message is received for an operation, it may not be present in OpMap
  -- in these cases:
  -- 1. If the operation is a query/mutation - as we remove the operation from the
  -- OpMap as soon as it is executed
  -- 2. A misbehaving client
  -- 3. A bug on our end
  stopOperation serverEnv wsConn opId $
    L.unLogger logger $
      L.UnstructuredLog L.LevelDebug $
        fromString $
          "Received STOP for an operation that we have no record for: "
            <> show (unOperationId opId)
            <> " (could be a query/mutation operation or a misbehaving client or a bug)"
  where
    logger = _wseLogger serverEnv

stopOperation :: WSServerEnv -> WSConn -> OperationId -> IO () -> IO ()
stopOperation serverEnv wsConn opId logWhenOpNotExist = do
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  case opM of
    Just (lqId, opNameM) -> do
      logWSEvent logger wsConn $ EOperation $ opDet opNameM
      LQ.removeLiveQuery logger (_wseServerMetrics serverEnv) lqMap lqId
    Nothing -> logWhenOpNotExist
  STM.atomically $ STMMap.delete opId opMap
  where
    logger = _wseLogger serverEnv
    lqMap = _wseLiveQMap serverEnv
    opMap = _wscOpMap $ WS.getData wsConn
    opDet n = OperationDetails opId Nothing n ODStopped Nothing Nothing

onConnInit ::
  (MonadIO m, UserAuthentication (Tracing.TraceT m)) =>
  L.Logger L.Hasura ->
  H.Manager ->
  WSConn ->
  AuthMode ->
  Maybe ConnParams ->
  -- | this is the message handler for handling errors on initializing a from the client connection
  WS.WSOnErrorMessageAction WSConnData ->
  -- | this is the message handler for handling "keep-alive" messages to the client
  WS.WSKeepAliveMessageAction WSConnData ->
  Tracing.TraceT m ()
onConnInit logger manager wsConn authMode connParamsM onConnInitErrAction keepAliveMessageAction = do
  -- TODO(from master): what should be the behaviour of connection_init message when a
  -- connection is already iniatilized? Currently, we seem to be doing
  -- something arbitrary which isn't correct. Ideally, we should stick to
  -- this:
  --
  -- > Allow connection_init message only when the connection state is
  -- 'not initialised'. This means that there is no reason for the
  -- connection to be in `CSInitError` state.
  connState <- liftIO (STM.readTVarIO (_wscUser $ WS.getData wsConn))
  case getIpAddress connState of
    Left err -> unexpectedInitError err
    Right ipAddress -> do
      let headers = mkHeaders connState
      res <- resolveUserInfo logger manager headers authMode Nothing
      case res of
        Left e -> do
          let !initErr = CSInitError $ qeError e
          liftIO $ do
            $assertNFHere initErr -- so we don't write thunks to mutable vars
            STM.atomically $ STM.writeTVar (_wscUser $ WS.getData wsConn) initErr

          let connErr = ConnErrMsg $ qeError e
          logWSEvent logger wsConn $ EConnErr connErr
          liftIO $ onConnInitErrAction wsConn connErr WS.onConnInitErrorText
        Right (userInfo, expTimeM) -> do
          let !csInit = CSInitialised $ WsClientState userInfo expTimeM paramHeaders ipAddress
          liftIO $ do
            $assertNFHere csInit -- so we don't write thunks to mutable vars
            STM.atomically $ STM.writeTVar (_wscUser $ WS.getData wsConn) csInit

          sendMsg wsConn SMConnAck
          liftIO $ keepAliveMessageAction wsConn
  where
    unexpectedInitError e = do
      let connErr = ConnErrMsg e
      logWSEvent logger wsConn $ EConnErr connErr
      liftIO $ onConnInitErrAction wsConn connErr WS.onConnInitErrorText

    getIpAddress = \case
      CSNotInitialised _ ip -> return ip
      CSInitialised WsClientState {..} -> return wscsIpAddress
      CSInitError e -> Left e

    mkHeaders st =
      paramHeaders ++ getClientHdrs st

    paramHeaders =
      [ (CI.mk $ TE.encodeUtf8 h, TE.encodeUtf8 v)
        | (h, v) <- maybe [] Map.toList $ connParamsM >>= _cpHeaders
      ]

    getClientHdrs st = case st of
      CSNotInitialised h _ -> unWsHeaders h
      _ -> []

onClose ::
  MonadIO m =>
  L.Logger L.Hasura ->
  ServerMetrics ->
  LQ.LiveQueriesState ->
  WSConn ->
  m ()
onClose logger serverMetrics lqMap wsConn = do
  logWSEvent logger wsConn EClosed
  operations <- liftIO $ STM.atomically $ ListT.toList $ STMMap.listT opMap
  liftIO $
    for_ operations $ \(_, (lqId, _)) ->
      LQ.removeLiveQuery logger serverMetrics lqMap lqId
  where
    opMap = _wscOpMap $ WS.getData wsConn
