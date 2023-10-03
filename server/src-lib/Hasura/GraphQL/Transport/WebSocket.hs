{-# LANGUAGE TemplateHaskell #-}

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
    mkCloseWebsocketsOnMetadataChangeAction,
    runWebsocketCloseOnMetadataChangeAction,
    WebsocketCloseOnMetadataChangeAction,
  )
where

import Control.Concurrent.Extended (sleep)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Morph (hoist)
import Control.Monad.Trans.Control qualified as MC
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.Encoding qualified as J
import Data.Aeson.Ordered qualified as JO
import Data.Aeson.TH qualified as J
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Dependent.Map qualified as DM
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.Monoid (Endo (..))
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Extended (toTxt, (<>>))
import Data.Time.Clock
import Data.Time.Clock qualified as TC
import Data.Word (Word16)
import GHC.AssertNF.CPP
import Hasura.App.State
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Backends.Postgres.Instances.Transport (runPGMutationTransaction)
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Action qualified as EA
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.RemoteJoin qualified as RJ
import Hasura.GraphQL.Execute.Subscription.Plan qualified as ES
import Hasura.GraphQL.Execute.Subscription.Poll qualified as ES
import Hasura.GraphQL.Execute.Subscription.State qualified as ES
import Hasura.GraphQL.Logging
import Hasura.GraphQL.Namespace (RootFieldAlias (..))
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Parser.Directives (cached)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.GraphQL.Transport.Instances ()
import Hasura.GraphQL.Transport.WebSocket.Protocol
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Types
import Hasura.GraphQL.Transport.WebSocket.Types qualified as WS
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.Types.Common (MetricsConfig (_mcAnalyzeQueryVariables))
import Hasura.RQL.Types.OpenTelemetry (getOtelTracesPropagator)
import Hasura.RQL.Types.ResultCustomization
import Hasura.RQL.Types.SchemaCache (SchemaCache (scOpenTelemetryConfig), scApiLimits, scMetricsConfig)
import Hasura.RemoteSchema.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.AppStateRef
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
import Hasura.Server.Logging (ModelInfo (..), ModelInfoLog (..))
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Prometheus
  ( GraphQLRequestMetrics (..),
    PrometheusMetrics (..),
  )
import Hasura.Server.Telemetry.Counters qualified as Telem
import Hasura.Server.Types (GranularPrometheusMetricsState (..), ModelInfoLogState (..), MonadGetPolicies (..), RequestId, getRequestId)
import Hasura.Services.Network
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax (Name (..))
import Language.GraphQL.Draft.Syntax qualified as G
import ListT qualified
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.WebSockets qualified as WS
import Refined (unrefine)
import StmContainers.Map qualified as STMMap
import System.Metrics.Prometheus.Counter qualified as Prometheus.Counter
import System.Metrics.Prometheus.Histogram qualified as Prometheus.Histogram

-- | 'ES.SubscriberDetails' comes from 'Hasura.GraphQL.Execute.LiveQuery.State.addLiveQuery'. We use
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
  deriving (Eq)

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
  deriving (Eq)

$(J.deriveToJSON hasuraJSON ''OperationDetails)

data WSEvent
  = EAccepted
  | ERejected !QErr
  | EConnErr !ConnErrMsg
  | EOperation !OperationDetails
  | EClosed
  deriving (Eq)

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
  deriving (Eq)

$(J.deriveToJSON hasuraJSON ''WsConnInfo)

data WSLogInfo = WSLogInfo
  { _wsliUserVars :: !(Maybe SessionVariables),
    _wsliConnectionInfo :: !WsConnInfo,
    _wsliEvent :: !WSEvent
  }
  deriving (Eq)

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
sendMsg wsConn msg = liftIO do
  timer <- startTimer
  WS.sendMsg wsConn $ WS.WSQueueResponse (encodeServerMsg msg) Nothing timer

-- sendCloseWithMsg closes the websocket server with an error code that can be supplied as (Maybe Word16),
-- if there is `Nothing`, the server will be closed with an error code derived from ServerErrorCode
sendCloseWithMsg ::
  (MonadIO m) =>
  L.Logger L.Hasura ->
  WSConn ->
  ServerErrorCode ->
  Maybe ServerMsg ->
  Maybe Word16 ->
  m ()
sendCloseWithMsg logger wsConn errCode mErrServerMsg mCode = do
  case mErrServerMsg of
    Just errServerMsg -> do
      logWSEvent logger wsConn EClosed
      liftIO $ WS.sendMsgAndCloseConn wsConn errCloseCode errMsg errServerMsg
    Nothing -> do
      logWSEvent logger wsConn EClosed
      liftIO $ WS.sendCloseCode wsc errCloseCode errMsg
  where
    wsc = WS.getRawWebSocketConnection wsConn
    errMsg = encodeServerErrorMsg errCode
    errCloseCode = fromMaybe (getErrCode errCode) mCode
    getErrCode :: ServerErrorCode -> Word16
    getErrCode err = case err of
      ProtocolError1002 -> 1002
      GenericError4400 _ -> 4400
      Unauthorized4401 -> 4401
      Forbidden4403 -> 4403
      ConnectionInitTimeout4408 -> 4408
      NonUniqueSubscription4409 _ -> 4409
      TooManyRequests4429 -> 4429

sendMsgWithMetadata ::
  (MonadIO m) =>
  WSConn ->
  ServerMsg ->
  Maybe OperationName ->
  Maybe ParameterizedQueryHash ->
  ES.SubscriptionMetadata ->
  m ()
sendMsgWithMetadata wsConn msg opName paramQueryHash (ES.SubscriptionMetadata execTime) =
  liftIO do
    timer <- startTimer
    WS.sendMsg wsConn $ WS.WSQueueResponse bs wsInfo timer
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
  ( MonadFail m {- only due to https://gitlab.haskell.org/ghc/ghc/-/issues/15681 -},
    MonadIO m,
    MonadReader (WSServerEnv impl) m
  ) =>
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
    keepAliveAction keepAliveDelay wsConn =
      liftIO
        $ forever
        $ do
          kaAction wsConn
          sleep $ seconds (unrefine $ unKeepAliveDelay keepAliveDelay)

    tokenExpiryHandler wsConn = do
      expTime <- liftIO
        $ STM.atomically
        $ do
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
        liftIO
          $ WSConnData
          <$> STM.newTVarIO (CSNotInitialised hdrs ipAddress)
          <*> STMMap.newIO
          <*> pure errType
          <*> pure queryType

      pure
        $ Right
        $ WS.AcceptWith
          connData
          acceptRequest
          (keepAliveAction keepAliveDelay)
          tokenExpiryHandler

    reject qErr = do
      (L.Logger logger) <- asks _wseLogger
      logger $ mkWsErrorLog Nothing (WsConnInfo wsId Nothing Nothing) (ERejected qErr)
      return
        $ Left
        $ WS.RejectRequest
          (HTTP.statusCode $ qeStatus qErr)
          (HTTP.statusMessage $ qeStatus qErr)
          []
          (LBS.toStrict $ J.encodingToLazyByteString $ encodeGQLErr False qErr)

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
      corsPolicy <- liftIO =<< asks _wseCorsPolicy
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

-- Helper for avoiding boolean blindness
data ShouldCaptureQueryVariables
  = CaptureQueryVariables
  | DoNotCaptureQueryVariables

onStart ::
  forall m impl.
  ( MonadIO m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadExecutionLog m,
    Tracing.MonadTrace m,
    MonadExecuteQuery m,
    MC.MonadBaseControl IO m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  HashSet (L.EngineLogType L.Hasura) ->
  Maybe (CredentialCache AgentLicenseKey) ->
  WSServerEnv impl ->
  WSConn ->
  ShouldCaptureQueryVariables ->
  StartMsg ->
  WS.WSActions WSConnData ->
  m ()
onStart enabledLogTypes agentLicenseKey serverEnv wsConn shouldCaptureVariables (StartMsg opId q) onMessageActions = catchAndIgnore $ do
  modelInfoLogStatus' <- runGetModelInfoLogStatus
  modelInfoLogStatus <- liftIO modelInfoLogStatus'
  timerTot <- startTimer
  op <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  -- NOTE: it should be safe to rely on this check later on in this function, since we expect that
  -- we process all operations on a websocket connection serially:
  when (isJust op)
    $ withComplete
    $ sendStartErr
    $ "an operation already exists with this id: "
    <> unOperationId opId

  userInfoM <- liftIO $ STM.readTVarIO userInfoR
  (userInfo, origReqHdrs, ipAddress) <- case userInfoM of
    CSInitialised WsClientState {..} -> return (wscsUserInfo, wscsReqHeaders, wscsIpAddress)
    CSInitError initErr -> do
      let e = "cannot start as connection_init failed with: " <> initErr
      withComplete $ sendStartErr e
    CSNotInitialised _ _ -> do
      let e = "start received before the connection is initialised"
      withComplete $ sendStartErr e

  (requestId, reqHdrs) <- liftIO $ getRequestId origReqHdrs
  sc <- liftIO $ getSchemaCacheWithVersion appStateRef

  operationLimit <- askGraphqlOperationLimit requestId userInfo (scApiLimits sc)
  let runLimits ::
        ExceptT (Either GQExecError QErr) (ExceptT () m) a ->
        ExceptT (Either GQExecError QErr) (ExceptT () m) a
      runLimits = withErr Right $ runResourceLimits operationLimit

  env <- liftIO $ acEnvironment <$> getAppContext appStateRef
  sqlGenCtx <- liftIO $ acSQLGenCtx <$> getAppContext appStateRef
  enableAL <- liftIO $ acEnableAllowlist <$> getAppContext appStateRef

  (reqParsed, queryParts) <- Tracing.newSpan "Parse GraphQL" $ do
    reqParsedE <- lift $ E.checkGQLExecution userInfo (reqHdrs, ipAddress) enableAL sc q requestId
    reqParsed <- onLeft reqParsedE (withComplete . preExecErr requestId Nothing)
    queryPartsE <- runExceptT $ getSingleOperation reqParsed
    queryParts <- onLeft queryPartsE (withComplete . preExecErr requestId Nothing)
    pure (reqParsed, queryParts)

  let gqlOpType = G._todType queryParts
      opName = getOpNameFromParsedReq reqParsed
      maybeOperationName = _unOperationName <$> opName
      tracesPropagator = getOtelTracesPropagator $ scOpenTelemetryConfig sc
  for_ maybeOperationName $ \nm ->
    -- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/instrumentation/graphql/
    Tracing.attachMetadata [("graphql.operation.name", unName nm)]
  execPlanE <-
    runExceptT
      $ E.getResolvedExecPlan
        env
        logger
        prometheusMetrics
        userInfo
        sqlGenCtx
        readOnlyMode
        sc
        queryType
        reqHdrs
        q
        queryParts
        maybeOperationName
        requestId

  (parameterizedQueryHash, execPlan, modelInfoList) <- onLeft execPlanE (withComplete . preExecErr requestId (Just gqlOpType))

  case execPlan of
    E.QueryExecutionPlan queryPlan asts dirMap -> do
      let cachedDirective = runIdentity <$> DM.lookup cached dirMap

      -- We ignore the response headers (containing TTL information) because
      -- WebSockets don't support them.
      cachedValue <-
        cacheLookup queryPlan asts cachedDirective reqParsed userInfo reqHdrs >>= \case
          Right (_responseHeaders, cachedValue) -> pure cachedValue
          Left _err -> throwError ()
      case cachedValue of
        ResponseCached cachedResponseData -> do
          logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindCached
          let reportedExecutionTime = 0
          liftIO $ recordGQLQuerySuccess reportedExecutionTime gqlOpType
          modelInfoLogging modelInfoList True modelInfoLogStatus
          sendSuccResp cachedResponseData opName parameterizedQueryHash $ ES.SubscriptionMetadata reportedExecutionTime
        ResponseUncached storeResponseM -> do
          conclusion <- runExceptT
            $ runLimits
            $ forWithKey queryPlan
            $ \fieldName ->
              let getResponse = \case
                    E.ExecStepDB _headers exists remoteJoins -> doQErr $ do
                      (telemTimeIO_DT, resp) <-
                        AB.dispatchAnyBackend @BackendTransport
                          exists
                          \(EB.DBStepInfo _ sourceConfig genSql tx resolvedConnectionTemplate :: EB.DBStepInfo b) ->
                            runDBQuery @b
                              requestId
                              q
                              fieldName
                              userInfo
                              logger
                              agentLicenseKey
                              sourceConfig
                              (fmap (statsToAnyBackend @b) tx)
                              genSql
                              resolvedConnectionTemplate
                      (finalResponse, modelInfo) <-
                        RJ.processRemoteJoins requestId logger agentLicenseKey env reqHdrs userInfo resp remoteJoins q tracesPropagator
                      pure $ (AnnotatedResponsePart telemTimeIO_DT Telem.Local finalResponse [], modelInfo)
                    E.ExecStepRemote rsi resultCustomizer gqlReq remoteJoins -> do
                      logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindRemoteSchema
                      runRemoteGQ requestId q fieldName userInfo reqHdrs rsi resultCustomizer gqlReq remoteJoins tracesPropagator
                    E.ExecStepAction actionExecPlan _ remoteJoins -> do
                      logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindAction
                      (time, (resp, _), modelInfo) <- doQErr $ do
                        (time, (resp, hdrs)) <- EA.runActionExecution userInfo actionExecPlan
                        (finalResponse, modelInfo) <-
                          RJ.processRemoteJoins requestId logger agentLicenseKey env reqHdrs userInfo resp remoteJoins q tracesPropagator
                        pure (time, (finalResponse, hdrs), modelInfo)
                      pure $ (AnnotatedResponsePart time Telem.Empty resp [], modelInfo)
                    E.ExecStepRaw json -> do
                      logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindIntrospection
                      (,[]) <$> buildRaw json
                    E.ExecStepMulti lst -> do
                      allResponses <- traverse getResponse lst
                      let (allResponses', allModelInfo) = unzip allResponses
                      pure $ (AnnotatedResponsePart 0 Telem.Local (encJFromList (map arpResponse allResponses')) [], concat allModelInfo)
               in getResponse
          sendResultFromFragments Telem.Query timerTot requestId conclusion opName parameterizedQueryHash gqlOpType modelInfoList modelInfoLogStatus
          case (storeResponseM, conclusion) of
            (Just ResponseCacher {..}, Right results) -> do
              let (key, (compositeValue')) = unzip $ InsOrdHashMap.toList results
                  -- ignoring models from results here as we have already logged it in `sendResultFromFragments`
                  (annotatedResp, _model) = unzip compositeValue'
                  results' = InsOrdHashMap.fromList $ zip key annotatedResp
              -- Note: The result of `runStoreResponse` is ignored here since we can't ensure that
              --       the WS client will respond correctly to multiple messages.
              void
                $ runStoreResponse
                $ encodeAnnotatedResponseParts results'
            _ -> modelInfoLogging modelInfoList False modelInfoLogStatus

      liftIO $ sendCompleted (Just requestId) (Just parameterizedQueryHash)
    E.MutationExecutionPlan mutationPlan -> do
      -- See Note [Backwards-compatible transaction optimisation]
      case coalescePostgresMutations mutationPlan of
        -- we are in the aforementioned case; we circumvent the normal process
        Just (sourceConfig, resolvedConnectionTemplate, pgMutations) -> do
          resp <-
            runExceptT
              $ runLimits
              $ doQErr
              $ runPGMutationTransaction requestId q userInfo logger sourceConfig resolvedConnectionTemplate pgMutations
          -- we do not construct result fragments since we have only one result
          handleResult requestId gqlOpType resp \(telemTimeIO_DT, results) -> do
            let telemQueryType = Telem.Query
                telemLocality = Telem.Local
                telemTimeIO = convertDuration telemTimeIO_DT
            totalTime <- timerTot
            let telemTimeTot = Seconds totalTime
            sendSuccResp (encodeEncJSONResults results) opName parameterizedQueryHash
              $ ES.SubscriptionMetadata telemTimeIO_DT
            -- Telemetry. NOTE: don't time network IO:
            Telem.recordTimingMetric Telem.RequestDimensions {..} Telem.RequestTimings {..}
            liftIO $ recordGQLQuerySuccess totalTime gqlOpType

        -- we are not in the transaction case; proceeding normally
        Nothing -> do
          conclusion <- runExceptT
            $ runLimits
            $ forWithKey mutationPlan
            $ \fieldName ->
              let getResponse = \case
                    -- Ignoring response headers since we can't send them over WebSocket
                    E.ExecStepDB _responseHeaders exists remoteJoins -> doQErr $ do
                      (telemTimeIO_DT, resp) <-
                        AB.dispatchAnyBackend @BackendTransport
                          exists
                          \(EB.DBStepInfo _ sourceConfig genSql tx resolvedConnectionTemplate :: EB.DBStepInfo b) ->
                            runDBMutation @b
                              requestId
                              q
                              fieldName
                              userInfo
                              logger
                              agentLicenseKey
                              sourceConfig
                              (fmap EB.arResult tx)
                              genSql
                              resolvedConnectionTemplate
                      (finalResponse, modelInfo) <-
                        RJ.processRemoteJoins requestId logger agentLicenseKey env reqHdrs userInfo resp remoteJoins q tracesPropagator
                      pure $ (AnnotatedResponsePart telemTimeIO_DT Telem.Local finalResponse [], modelInfo)
                    E.ExecStepAction actionExecPlan _ remoteJoins -> do
                      logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindAction
                      (time, (resp, hdrs), modelInfo) <- doQErr $ do
                        (time, (resp, hdrs)) <- EA.runActionExecution userInfo actionExecPlan
                        (finalResponse, modelInfo) <-
                          RJ.processRemoteJoins requestId logger agentLicenseKey env reqHdrs userInfo resp remoteJoins q tracesPropagator
                        pure (time, (finalResponse, hdrs), modelInfo)
                      pure $ (AnnotatedResponsePart time Telem.Empty resp $ fromMaybe [] hdrs, modelInfo)
                    E.ExecStepRemote rsi resultCustomizer gqlReq remoteJoins -> do
                      logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindRemoteSchema
                      runRemoteGQ requestId q fieldName userInfo reqHdrs rsi resultCustomizer gqlReq remoteJoins tracesPropagator
                    E.ExecStepRaw json -> do
                      logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindIntrospection
                      (,[]) <$> buildRaw json
                    E.ExecStepMulti lst -> do
                      allResponses <- traverse getResponse lst
                      let (allResponses', allModelInfo) = unzip allResponses
                      pure $ (AnnotatedResponsePart 0 Telem.Local (encJFromList (map arpResponse allResponses')) [], concat allModelInfo)
               in getResponse
          sendResultFromFragments Telem.Query timerTot requestId conclusion opName parameterizedQueryHash gqlOpType modelInfoList modelInfoLogStatus
      liftIO $ sendCompleted (Just requestId) (Just parameterizedQueryHash)
    E.SubscriptionExecutionPlan (subExec, modifier) -> do
      case subExec of
        E.SEAsyncActionsWithNoRelationships actions -> do
          logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindAction
          liftIO do
            let allActionIds = map fst $ toList actions
            case NE.nonEmpty allActionIds of
              Nothing -> do
                -- This means there is no async action query field present and there is no live-query or streaming
                -- subscription present. Now, we need to check if the modifier is present or not. If it is present,
                -- then we need to send the modified empty object. If it is not present, then we need to send
                -- the completed message.
                case modifier of
                  Nothing -> sendCompleted (Just requestId) (Just parameterizedQueryHash)
                  Just modifier' -> do
                    let serverMsg = sendDataMsg $ DataMsg opId $ Right . encJToLBS . encJFromOrderedValue $ appEndo modifier' $ JO.Object $ JO.empty
                    sendMsg wsConn serverMsg
              Just actionIds -> do
                let sendResponseIO actionLogMap = do
                      (dTime, resultsE) <- withElapsedTime
                        $ runExceptT
                        $ for actions
                        $ \(actionId, resultBuilder) -> do
                          actionLogResponse <-
                            HashMap.lookup actionId actionLogMap
                              `onNothing` throw500 "unexpected: cannot lookup action_id in response map"
                          liftEither $ resultBuilder actionLogResponse
                      case resultsE of
                        Left err -> sendError requestId err
                        Right results -> do
                          let dataMsg =
                                sendDataMsg
                                  $ DataMsg opId
                                  $ pure
                                  $ encJToLBS
                                  $ encodeEncJSONResults results
                          sendMsgWithMetadata wsConn dataMsg opName (Just parameterizedQueryHash) $ ES.SubscriptionMetadata dTime

                    asyncActionQueryLive =
                      ES.LAAQNoRelationships
                        $ ES.LiveAsyncActionQueryWithNoRelationships sendResponseIO (sendCompleted (Just requestId) (Just parameterizedQueryHash))

                ES.addAsyncActionLiveQuery
                  (ES._ssAsyncActions subscriptionsState)
                  opId
                  actionIds
                  (sendError requestId)
                  asyncActionQueryLive
        E.SEOnSourceDB (E.SSLivequery actionIds liveQueryBuilder) -> do
          actionLogMapE <- fmap fst <$> runExceptT (EA.fetchActionLogResponses actionIds)
          actionLogMap <- onLeft actionLogMapE (withComplete . preExecErr requestId (Just gqlOpType))
          granularPrometheusMetricsState <- runGetPrometheusMetricsGranularity
          modelInfoLogStatus'' <- runGetModelInfoLogStatus
          opMetadataE <- liftIO $ startLiveQuery opName liveQueryBuilder parameterizedQueryHash requestId actionLogMap granularPrometheusMetricsState modifier modelInfoLogStatus''
          lqId <- onLeft opMetadataE (withComplete . preExecErr requestId (Just gqlOpType))
          -- Update async action query subscription state
          case NE.nonEmpty (toList actionIds) of
            Nothing -> do
              logQueryLog logger $ QueryLog q Nothing requestId (QueryLogKindDatabase Nothing)
              -- No async action query fields present, do nothing.
              pure ()
            Just nonEmptyActionIds -> do
              logQueryLog logger $ QueryLog q Nothing requestId QueryLogKindAction
              liftIO $ do
                let asyncActionQueryLive =
                      ES.LAAQOnSourceDB
                        $ ES.LiveAsyncActionQueryOnSource lqId actionLogMap
                        $ restartLiveQuery opName parameterizedQueryHash requestId liveQueryBuilder granularPrometheusMetricsState (_grOperationName reqParsed) modifier modelInfoLogStatus''

                    onUnexpectedException err = do
                      sendError requestId err
                      stopOperation serverEnv wsConn opId granularPrometheusMetricsState (pure ()) -- Don't log in case opId don't exist
                ES.addAsyncActionLiveQuery
                  (ES._ssAsyncActions subscriptionsState)
                  opId
                  nonEmptyActionIds
                  onUnexpectedException
                  asyncActionQueryLive
        E.SEOnSourceDB (E.SSStreaming rootFieldName streamQueryBuilder) -> do
          granularPrometheusMetricsState <- runGetPrometheusMetricsGranularity
          modelInfoLogStatus'' <- runGetModelInfoLogStatus
          liftIO $ startStreamingQuery rootFieldName streamQueryBuilder parameterizedQueryHash requestId granularPrometheusMetricsState modifier modelInfoLogStatus''

      liftIO $ Prometheus.Counter.inc (gqlRequestsSubscriptionSuccess gqlMetrics)
      liftIO $ logOpEv ODStarted (Just requestId) (Just parameterizedQueryHash)
  where
    sendDataMsg = WS._wsaGetDataMessageType onMessageActions
    closeConnAction = WS._wsaConnectionCloseAction onMessageActions
    postExecErrAction = WS._wsaPostExecErrMessageAction onMessageActions
    fmtErrorMessage = WS._wsaErrorMsgFormat onMessageActions

    modelInfoLogging modelInfoListLog cacheStatus getModelInfoLogStatus =
      when (getModelInfoLogStatus == ModelInfoLogOn) $ do
        for_ modelInfoListLog $ \(ModelInfoPart modelName modelType modelSourceName modelSourceType modelQueryType) -> do
          L.unLogger logger $ ModelInfoLog L.LevelInfo $ ModelInfo modelName (toTxt modelType) modelSourceName (toTxt <$> modelSourceType) (toTxt modelQueryType) cacheStatus

    doQErr ::
      (Monad n) =>
      ExceptT QErr n a ->
      ExceptT (Either GQExecError QErr) n a
    doQErr = withExceptT Right

    withErr ::
      forall e f n a.
      (Monad n) =>
      (e -> f) ->
      (ExceptT e (ExceptT f n) a -> ExceptT e (ExceptT f n) a) ->
      ExceptT f n a ->
      ExceptT f n a
    withErr embed f action = do
      res <- runExceptT $ f $ lift action
      onLeft res (throwError . embed)

    forWithKey = flip InsOrdHashMap.traverseWithKey

    telemTransport = Telem.WebSocket

    handleResult ::
      forall a.
      RequestId ->
      G.OperationType ->
      Either (Either GQExecError QErr) a ->
      (a -> ExceptT () m ()) ->
      ExceptT () m ()
    handleResult requestId gqlOpType r f = case r of
      Left (Left err) -> postExecErr' gqlOpType err
      Left (Right err) -> postExecErr requestId gqlOpType err
      Right results -> f results

    sendResultFromFragments telemQueryType timerTot requestId r opName pqh gqlOpType modelInfoList getModelInfoLogStatus =
      handleResult requestId gqlOpType r \results -> do
        let (key, (compositeValue')) = unzip $ InsOrdHashMap.toList results
            (annotatedResp, model) = unzip compositeValue'
            results' = InsOrdHashMap.fromList $ zip key annotatedResp
            modelInfoList' = concat model
        let telemLocality = foldMap arpLocality results'
            telemTimeIO = convertDuration $ sum $ fmap arpTimeIO results'
        totalTime <- timerTot
        let telemTimeTot = Seconds totalTime
        sendSuccResp (encodeAnnotatedResponseParts results') opName pqh
          $ ES.SubscriptionMetadata
          $ sum
          $ fmap arpTimeIO results'
        -- Telemetry. NOTE: don't time network IO:
        Telem.recordTimingMetric Telem.RequestDimensions {..} Telem.RequestTimings {..}
        modelInfoLogging (modelInfoList <> modelInfoList') False getModelInfoLogStatus
        liftIO $ (recordGQLQuerySuccess totalTime gqlOpType)

    runRemoteGQ ::
      RequestId ->
      GQLReqUnparsed ->
      RootFieldAlias ->
      UserInfo ->
      [HTTP.Header] ->
      RemoteSchemaInfo ->
      ResultCustomizer ->
      GQLReqOutgoing ->
      Maybe RJ.RemoteJoins ->
      Tracing.HttpPropagator ->
      ExceptT (Either GQExecError QErr) (ExceptT () m) (AnnotatedResponsePart, [ModelInfoPart])
    runRemoteGQ requestId reqUnparsed fieldName userInfo reqHdrs rsi resultCustomizer gqlReq remoteJoins tracesPropagator = Tracing.newSpan ("Remote schema query for root field " <>> fieldName) $ do
      env <- liftIO $ acEnvironment <$> getAppContext appStateRef
      (telemTimeIO_DT, _respHdrs, resp) <-
        doQErr
          $ E.execRemoteGQ env tracesPropagator userInfo reqHdrs (rsDef rsi) gqlReq
      value <- hoist lift $ extractFieldFromResponse fieldName resultCustomizer resp
      (finalResponse, modelInfo) <-
        doQErr
          $ RJ.processRemoteJoins
            requestId
            logger
            agentLicenseKey
            env
            reqHdrs
            userInfo
            -- TODO: avoid encode and decode here
            (encJFromOrderedValue value)
            remoteJoins
            reqUnparsed
            tracesPropagator
      return $ (AnnotatedResponsePart telemTimeIO_DT Telem.Remote finalResponse [], modelInfo)

    WSServerEnv
      logger
      subscriptionsState
      appStateRef
      _
      _
      readOnlyMode
      _
      _keepAliveDelay
      _serverMetrics
      prometheusMetrics
      _ = serverEnv

    -- Hook to retrieve the latest subscription options(live query + stream query options) from the `appStateRef`
    getSubscriptionOptions = fmap (\appCtx -> (acLiveQueryOptions appCtx, acStreamQueryOptions appCtx)) (getAppContext appStateRef)
    gqlMetrics = pmGraphQLRequestMetrics prometheusMetrics

    WSConnData userInfoR opMap errRespTy queryType = WS.getData wsConn

    logOpEv :: (MonadIO n) => OpDetail -> Maybe RequestId -> Maybe ParameterizedQueryHash -> n ()
    logOpEv opTy reqId parameterizedQueryHash =
      -- See Note [Disable query printing when query-log is disabled]
      let censoredReq =
            case shouldCaptureVariables of
              CaptureQueryVariables -> q
              DoNotCaptureQueryVariables -> q {_grVariables = Nothing}
          queryToLog = censoredReq <$ guard (Set.member L.ELTQueryLog enabledLogTypes)
       in logWSEvent logger wsConn
            $ EOperation
            $ OperationDetails opId reqId (_grOperationName q) opTy queryToLog parameterizedQueryHash

    getErrFn ERTLegacy = encodeQErr
    getErrFn ERTGraphqlCompliant = encodeGQLErr

    sendStartErr e = do
      let errFn = getErrFn errRespTy
      sendMsg wsConn
        $ SMErr
        $ ErrorMsg opId
        $ errFn False
        $ err400 StartFailed e
      liftIO $ logOpEv (ODProtoErr e) Nothing Nothing
      liftIO $ reportGQLQueryError Nothing
      liftIO $ closeConnAction wsConn opId (T.unpack e)

    sendCompleted reqId paramQueryHash = do
      sendMsg wsConn (SMComplete . CompletionMsg $ opId)
      logOpEv ODCompleted reqId paramQueryHash

    postExecErr ::
      RequestId ->
      G.OperationType ->
      QErr ->
      ExceptT () m ()
    postExecErr reqId gqlOpType qErr = do
      let errFn = getErrFn errRespTy False
      liftIO $ logOpEv (ODQueryErr qErr) (Just reqId) Nothing
      postExecErr' gqlOpType $ GQExecError $ pure $ errFn qErr

    postExecErr' :: G.OperationType -> GQExecError -> ExceptT () m ()
    postExecErr' gqlOpType qErr =
      liftIO $ do
        reportGQLQueryError (Just gqlOpType)
        postExecErrAction wsConn opId qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr reqId mGqlOpType qErr = do
      liftIO $ reportGQLQueryError mGqlOpType
      liftIO $ sendError reqId qErr

    sendError reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId) Nothing
      let err = case errRespTy of
            ERTLegacy -> errFn False qErr
            ERTGraphqlCompliant -> fmtErrorMessage [errFn False qErr]
      sendMsg wsConn (SMErr $ ErrorMsg opId err)

    sendSuccResp ::
      EncJSON ->
      Maybe OperationName ->
      ParameterizedQueryHash ->
      ES.SubscriptionMetadata ->
      ExceptT () m ()
    sendSuccResp encJson opName queryHash =
      sendMsgWithMetadata
        wsConn
        (sendDataMsg $ DataMsg opId $ pure $ encJToLBS encJson)
        opName
        (Just queryHash)

    withComplete ::
      ExceptT () m () ->
      ExceptT () m a
    withComplete action = do
      action
      liftIO $ sendCompleted Nothing Nothing
      throwError ()

    restartLiveQuery opName parameterizedQueryHash requestId liveQueryBuilder granularPrometheusMetricsState maybeOperationName modifier getModelInfoLogStatus lqId actionLogMap = do
      ES.removeLiveQuery logger (_wseServerMetrics serverEnv) (_wsePrometheusMetrics serverEnv) subscriptionsState lqId granularPrometheusMetricsState maybeOperationName
      either (const Nothing) Just <$> startLiveQuery opName liveQueryBuilder parameterizedQueryHash requestId actionLogMap granularPrometheusMetricsState modifier getModelInfoLogStatus

    startLiveQuery opName liveQueryBuilder parameterizedQueryHash requestId actionLogMap granularPrometheusMetricsState modifier modelInfoLogStatus = do
      liveQueryE <- runExceptT $ liveQueryBuilder actionLogMap

      for liveQueryE $ \((sourceName, E.SubscriptionQueryPlan exists), modelInfo) -> do
        let subscriberMetadata = ES.mkSubscriberMetadata (WS.getWSId wsConn) opId opName requestId
        -- NOTE!: we mask async exceptions higher in the call stack, but it's
        -- crucial we don't lose lqId after addLiveQuery returns successfully.
        !lqId <- liftIO $ AB.dispatchAnyBackend @BackendTransport
          exists
          \(E.MultiplexedSubscriptionQueryPlan liveQueryPlan) ->
            ES.addLiveQuery
              logger
              (_wseServerMetrics serverEnv)
              (_wsePrometheusMetrics serverEnv)
              subscriberMetadata
              subscriptionsState
              getSubscriptionOptions
              sourceName
              parameterizedQueryHash
              opName
              requestId
              liveQueryPlan
              granularPrometheusMetricsState
              (onChange opName parameterizedQueryHash $ ES._sqpNamespace liveQueryPlan)
              modifier
              modelInfo
              modelInfoLogStatus

        liftIO $ $assertNFHere (lqId, opName) -- so we don't write thunks to mutable vars
        STM.atomically
          $
          -- NOTE: see crucial `lookup` check above, ensuring this doesn't clobber:
          STMMap.insert (LiveQuerySubscriber lqId, opName) opId opMap
        pure lqId

    startStreamingQuery rootFieldName ((sourceName, E.SubscriptionQueryPlan exists), modelInfo) parameterizedQueryHash requestId granularPrometheusMetricsState modifier modelInfoLogStatus = do
      let !opName = _grOperationName q
          subscriberMetadata = ES.mkSubscriberMetadata (WS.getWSId wsConn) opId opName requestId
      -- NOTE!: we mask async exceptions higher in the call stack, but it's
      -- crucial we don't lose lqId after addLiveQuery returns successfully.
      streamSubscriberId <- liftIO $ AB.dispatchAnyBackend @BackendTransport
        exists
        \(E.MultiplexedSubscriptionQueryPlan streamQueryPlan) ->
          ES.addStreamSubscriptionQuery
            logger
            (_wseServerMetrics serverEnv)
            (_wsePrometheusMetrics serverEnv)
            subscriberMetadata
            subscriptionsState
            getSubscriptionOptions
            sourceName
            parameterizedQueryHash
            opName
            requestId
            (_rfaAlias rootFieldName)
            streamQueryPlan
            granularPrometheusMetricsState
            (onChange opName parameterizedQueryHash $ ES._sqpNamespace streamQueryPlan)
            modifier
            modelInfo
            modelInfoLogStatus
      liftIO $ $assertNFHere (streamSubscriberId, opName) -- so we don't write thunks to mutable vars
      STM.atomically
        $
        -- NOTE: see crucial `lookup` check above, ensuring this doesn't clobber:
        STMMap.insert (StreamingQuerySubscriber streamSubscriberId, opName) opId opMap
      pure ()

    -- on change, send message on the websocket
    onChange :: Maybe OperationName -> ParameterizedQueryHash -> Maybe Name -> ES.OnChange
    onChange opName queryHash namespace = \case
      Right (ES.SubscriptionResponse bs dTime) ->
        sendMsgWithMetadata
          wsConn
          (sendDataMsg $ DataMsg opId $ pure $ maybe LBS.fromStrict wrapNamespace namespace bs)
          opName
          (Just queryHash)
          (ES.SubscriptionMetadata dTime)
      resp ->
        sendMsg wsConn
          $ sendDataMsg
          $ DataMsg opId
          $ LBS.fromStrict
          . ES._lqrPayload
          <$> resp

    -- If the source has a namespace then we need to wrap the response
    -- from the DB in that namespace.
    wrapNamespace :: Name -> ByteString -> LBS.ByteString
    wrapNamespace namespace bs =
      encJToLBS $ encJFromAssocList [(unName namespace, encJFromBS bs)]

    catchAndIgnore :: ExceptT () m () -> m ()
    catchAndIgnore m = void $ runExceptT m

    reportGQLQueryError :: Maybe G.OperationType -> IO ()
    reportGQLQueryError = \case
      Nothing ->
        liftIO $ Prometheus.Counter.inc (gqlRequestsUnknownFailure gqlMetrics)
      Just opType -> case opType of
        G.OperationTypeQuery ->
          liftIO $ Prometheus.Counter.inc (gqlRequestsQueryFailure gqlMetrics)
        G.OperationTypeMutation ->
          liftIO $ Prometheus.Counter.inc (gqlRequestsMutationFailure gqlMetrics)
        G.OperationTypeSubscription ->
          liftIO $ Prometheus.Counter.inc (gqlRequestsSubscriptionFailure gqlMetrics)

    -- Tally and record execution times for successful GraphQL requests.
    recordGQLQuerySuccess :: DiffTime -> G.OperationType -> IO ()
    recordGQLQuerySuccess totalTime = \case
      G.OperationTypeQuery -> liftIO $ do
        Prometheus.Counter.inc (gqlRequestsQuerySuccess gqlMetrics)
        Prometheus.Histogram.observe (gqlExecutionTimeSecondsQuery gqlMetrics) (realToFrac totalTime)
      G.OperationTypeMutation -> liftIO $ do
        Prometheus.Counter.inc (gqlRequestsMutationSuccess gqlMetrics)
        Prometheus.Histogram.observe (gqlExecutionTimeSecondsMutation gqlMetrics) (realToFrac totalTime)
      G.OperationTypeSubscription ->
        -- We do not collect metrics for subscriptions at the request level.
        pure ()

onMessage ::
  ( MonadIO m,
    UserAuthentication m,
    E.MonadGQLExecutionCheck m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadExecuteQuery m,
    MC.MonadBaseControl IO m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m,
    Tracing.MonadTrace m,
    MonadGetPolicies m
  ) =>
  HashSet (L.EngineLogType L.Hasura) ->
  IO AuthMode ->
  WSServerEnv impl ->
  WSConn ->
  LBS.ByteString ->
  WS.WSActions WSConnData ->
  Maybe (CredentialCache AgentLicenseKey) ->
  m ()
onMessage enabledLogTypes authMode serverEnv wsConn msgRaw onMessageActions agentLicenseKey =
  Tracing.newTrace (_wseTraceSamplingPolicy serverEnv) "websocket" do
    case J.eitherDecode msgRaw of
      Left e -> do
        let err = ConnErrMsg $ "parsing ClientMessage failed: " <> T.pack e
        logWSEvent logger wsConn $ EConnErr err
        liftIO $ onErrAction wsConn err WS.ClientMessageParseFailed
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
        CMStart startMsg -> do
          schemaCache <- liftIO $ getSchemaCache $ _wseAppStateRef serverEnv
          let shouldCaptureVariables =
                if _mcAnalyzeQueryVariables (scMetricsConfig schemaCache)
                  then CaptureQueryVariables
                  else DoNotCaptureQueryVariables
          onStart enabledLogTypes agentLicenseKey serverEnv wsConn shouldCaptureVariables startMsg onMessageActions
        CMStop stopMsg -> do
          granularPrometheusMetricsState <- runGetPrometheusMetricsGranularity
          onStop serverEnv wsConn stopMsg granularPrometheusMetricsState
        -- specfic to graphql-ws
        CMPing mPayload -> onPing wsConn mPayload
        CMPong _mPayload -> pure ()
        -- specific to apollo clients
        CMConnTerm -> liftIO $ WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = _wseLogger serverEnv
    onErrAction = WS._wsaOnErrorMessageAction onMessageActions
    keepAliveMessageAction = WS._wsaKeepAliveAction onMessageActions

onPing :: (MonadIO m) => WSConn -> Maybe PingPongPayload -> m ()
onPing wsConn mPayload =
  liftIO $ sendMsg wsConn (SMPong mPayload)

onStop :: (Tracing.MonadTraceContext m, MonadIO m) => WSServerEnv impl -> WSConn -> StopMsg -> IO GranularPrometheusMetricsState -> m ()
onStop serverEnv wsConn (StopMsg opId) granularPrometheusMetricsState = do
  -- When a stop message is received for an operation, it may not be present in OpMap
  -- in these cases:
  -- 1. If the operation is a query/mutation - as we remove the operation from the
  -- OpMap as soon as it is executed
  -- 2. A misbehaving client
  -- 3. A bug on our end
  stopOperation serverEnv wsConn opId granularPrometheusMetricsState
    $ L.unLoggerTracing logger
    $ L.UnstructuredLog L.LevelDebug
    $ fromString
    $ "Received STOP for an operation that we have no record for: "
    <> show (unOperationId opId)
    <> " (could be a query/mutation operation or a misbehaving client or a bug)"
  where
    logger = _wseLogger serverEnv

stopOperation :: (MonadIO m) => WSServerEnv impl -> WSConn -> OperationId -> IO GranularPrometheusMetricsState -> m () -> m ()
stopOperation serverEnv wsConn opId granularPrometheusMetricsState logWhenOpNotExist = do
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  case opM of
    Just (subscriberDetails, operationName) -> do
      liftIO $ logWSEvent logger wsConn $ EOperation $ opDet operationName
      case subscriberDetails of
        LiveQuerySubscriber lqId ->
          liftIO $ ES.removeLiveQuery logger (_wseServerMetrics serverEnv) (_wsePrometheusMetrics serverEnv) subscriptionState lqId granularPrometheusMetricsState operationName
        StreamingQuerySubscriber streamSubscriberId ->
          liftIO $ ES.removeStreamingQuery logger (_wseServerMetrics serverEnv) (_wsePrometheusMetrics serverEnv) subscriptionState streamSubscriberId granularPrometheusMetricsState operationName
    Nothing -> logWhenOpNotExist
  liftIO $ STM.atomically $ STMMap.delete opId opMap
  where
    logger = _wseLogger serverEnv
    subscriptionState = _wseSubscriptionState serverEnv
    opMap = _wscOpMap $ WS.getData wsConn
    opDet n = OperationDetails opId Nothing n ODStopped Nothing Nothing

onConnInit ::
  (MonadIO m, UserAuthentication m) =>
  L.Logger L.Hasura ->
  HTTP.Manager ->
  WSConn ->
  IO AuthMode ->
  Maybe ConnParams ->
  -- | this is the message handler for handling errors on initializing a from the client connection
  WS.WSOnErrorMessageAction WSConnData ->
  -- | this is the message handler for handling "keep-alive" messages to the client
  WS.WSKeepAliveMessageAction WSConnData ->
  m ()
onConnInit logger manager wsConn getAuthMode connParamsM onConnInitErrAction keepAliveMessageAction = do
  -- TODO(from master): what should be the behaviour of connection_init message when a
  -- connection is already iniatilized? Currently, we seem to be doing
  -- something arbitrary which isn't correct. Ideally, we should stick to
  -- this:
  --
  -- > Allow connection_init message only when the connection state is
  -- 'not initialised'. This means that there is no reason for the
  -- connection to be in `CSInitError` state.
  connState <- liftIO (STM.readTVarIO (_wscUser $ WS.getData wsConn))
  authMode <- liftIO $ getAuthMode
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
          liftIO $ onConnInitErrAction wsConn connErr WS.ConnInitFailed
        -- we're ignoring the auth headers as headers are irrelevant in websockets
        Right (userInfo, expTimeM, _authHeaders, _) -> do
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
      liftIO $ onConnInitErrAction wsConn connErr WS.ConnInitFailed

    getIpAddress = \case
      CSNotInitialised _ ip -> return ip
      CSInitialised WsClientState {..} -> return wscsIpAddress
      CSInitError e -> Left e

    mkHeaders st =
      paramHeaders ++ getClientHdrs st

    paramHeaders =
      [ (CI.mk $ TE.encodeUtf8 h, TE.encodeUtf8 v)
        | (h, v) <- maybe [] HashMap.toList $ connParamsM >>= _cpHeaders
      ]

    getClientHdrs st = case st of
      CSNotInitialised h _ -> unWsHeaders h
      _ -> []

onClose ::
  (MonadIO m) =>
  L.Logger L.Hasura ->
  ServerMetrics ->
  PrometheusMetrics ->
  ES.SubscriptionsState ->
  WSConn ->
  IO GranularPrometheusMetricsState ->
  m ()
onClose logger serverMetrics prometheusMetrics subscriptionsState wsConn granularPrometheusMetricsState = do
  logWSEvent logger wsConn EClosed
  operations <- liftIO $ STM.atomically $ ListT.toList $ STMMap.listT opMap
  liftIO
    $ for_ operations
    $ \(_, (subscriber, operationName)) ->
      case subscriber of
        LiveQuerySubscriber lqId -> ES.removeLiveQuery logger serverMetrics prometheusMetrics subscriptionsState lqId granularPrometheusMetricsState operationName
        StreamingQuerySubscriber streamSubscriberId -> ES.removeStreamingQuery logger serverMetrics prometheusMetrics subscriptionsState streamSubscriberId granularPrometheusMetricsState operationName
  where
    opMap = _wscOpMap $ WS.getData wsConn

newtype WebsocketCloseOnMetadataChangeAction = WebsocketCloseOnMetadataChangeAction
  { runWebsocketCloseOnMetadataChangeAction :: IO ()
  }

-- | By default, we close all the websocket connections when the metadata changes. This function is used to create the
-- action that will be run when the metadata changes.
mkCloseWebsocketsOnMetadataChangeAction :: WS.WSServer WS.WSConnData -> WebsocketCloseOnMetadataChangeAction
mkCloseWebsocketsOnMetadataChangeAction wsServer =
  WebsocketCloseOnMetadataChangeAction
    $ WS.closeAllConnectionsWithReason
      wsServer
      "Closing all websocket connections as the metadata has changed"
      "Server state changed, restarting the server"
      id
