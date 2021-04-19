{-# LANGUAGE CPP #-}

module Hasura.GraphQL.Transport.WebSocket
  ( createWSServerApp
  , createWSServerEnv
  , stopWSServerApp
  , WSServerEnv
  , WSLog(..)
  ) where

-- NOTE!:
--   The handler functions 'onClose', 'onMessage', etc. depend for correctness on two properties:
--     - they run with async exceptions masked
--     - they do not race on the same connection

import           Hasura.Prelude

import qualified Control.Concurrent.Async.Lifted.Safe         as LA
import qualified Control.Concurrent.STM                       as STM
import qualified Control.Monad.Trans.Control                  as MC
import qualified Data.Aeson                                   as J
import qualified Data.Aeson.Casing                            as J
import qualified Data.Aeson.Ordered                           as JO
import qualified Data.Aeson.TH                                as J
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.CaseInsensitive                         as CI
import qualified Data.Environment                             as Env
import qualified Data.HashMap.Strict                          as Map
import qualified Data.HashMap.Strict.InsOrd                   as OMap
import qualified Data.List.NonEmpty                           as NE
import qualified Data.Text                                    as T
import qualified Data.Text.Encoding                           as TE
import qualified Data.Time.Clock                              as TC
import qualified Language.GraphQL.Draft.Syntax                as G
import qualified ListT
import qualified Network.HTTP.Client                          as H
import qualified Network.HTTP.Types                           as H
import qualified Network.Wai.Extended                         as Wai
import qualified Network.WebSockets                           as WS
import qualified StmContainers.Map                            as STMMap

import           Control.Concurrent.Extended                  (sleep)
import           Control.Exception.Lifted
import           Data.String
#ifndef PROFILING
import           GHC.AssertNF
#endif

import qualified Hasura.GraphQL.Execute                       as E
import qualified Hasura.GraphQL.Execute.Action                as EA
import qualified Hasura.GraphQL.Execute.Backend               as EB
import qualified Hasura.GraphQL.Execute.LiveQuery.Poll        as LQ
import qualified Hasura.GraphQL.Execute.LiveQuery.State       as LQ
import qualified Hasura.GraphQL.Transport.WebSocket.Server    as WS
import qualified Hasura.Logging                               as L
import qualified Hasura.SQL.AnyBackend                        as AB
import qualified Hasura.Server.Telemetry.Counters             as Telem
import qualified Hasura.Tracing                               as Tracing

import           Hasura.Backends.Postgres.Instances.Transport (runPGMutationTransaction)
import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.Backend
import           Hasura.GraphQL.Transport.HTTP                (MonadExecuteQuery (..),
                                                               QueryCacheKey (..),
                                                               ResultsFragment (..), buildRaw,
                                                               coalescePostgresMutations,
                                                               extractFieldFromResponse,
                                                               filterVariablesFromQuery,
                                                               runSessVarPred)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.Instances           ()
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Auth                           (AuthMode, UserAuthentication,
                                                               resolveUserInfo)
import           Hasura.Server.Cors
import           Hasura.Server.Init.Config                    (KeepAliveDelay (..))
import           Hasura.Server.Types                          (RequestId, getRequestId)
import           Hasura.Server.Version                        (HasVersion)
import           Hasura.Session


-- | 'LQ.LiveQueryId' comes from 'Hasura.GraphQL.Execute.LiveQuery.State.addLiveQuery'. We use
-- this to track a connection's operations so we can remove them from 'LiveQueryState', and
-- log.
--
-- NOTE!: This must be kept consistent with the global 'LiveQueryState', in 'onClose'
-- and 'onStart'.
type OperationMap
  = STMMap.Map OperationId (LQ.LiveQueryId, Maybe OperationName)

newtype WsHeaders
  = WsHeaders { unWsHeaders :: [H.Header] }
  deriving (Show, Eq)

data ErrRespType
  = ERTLegacy
  | ERTGraphqlCompliant
  deriving (Show)

data WSConnState
  = CSNotInitialised !WsHeaders !Wai.IpAddress
  -- ^ headers and IP address from the client for websockets
  | CSInitError !Text
  | CSInitialised !WsClientState

data WsClientState
  = WsClientState
  { wscsUserInfo     :: !UserInfo
  -- ^ the 'UserInfo' required to execute the GraphQL query
  , wscsTokenExpTime :: !(Maybe TC.UTCTime)
  -- ^ the JWT/token expiry time, if any
  , wscsReqHeaders   :: ![H.Header]
  -- ^ headers from the client (in conn params) to forward to the remote schema
  , wscsIpAddress    :: !Wai.IpAddress
  -- ^ IP address required for 'MonadGQLAuthorization'
  }

data WSConnData
  = WSConnData
  -- the role and headers are set only on connection_init message
  { _wscUser      :: !(STM.TVar WSConnState)
  -- we only care about subscriptions,
  -- the other operations (query/mutations)
  -- are not tracked here
  , _wscOpMap     :: !OperationMap
  , _wscErrRespTy :: !ErrRespType
  , _wscAPIType   :: !E.GraphQLQueryType
  }

type WSServer = WS.WSServer WSConnData

type WSConn = WS.WSConn WSConnData

sendMsg :: (MonadIO m) => WSConn -> ServerMsg -> m ()
sendMsg wsConn msg =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse (encodeServerMsg msg) Nothing

sendMsgWithMetadata :: (MonadIO m) => WSConn -> ServerMsg -> LQ.LiveQueryMetadata -> m ()
sendMsgWithMetadata wsConn msg (LQ.LiveQueryMetadata execTime) =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse bs wsInfo
  where
    bs = encodeServerMsg msg
    (msgType, operationId) = case msg of
      (SMData (DataMsg opId _)) -> (Just SMT_GQL_DATA, Just opId)
      _                         -> (Nothing, Nothing)
    wsInfo = Just $! WS.WSEventInfo
      { WS._wseiEventType = msgType
      , WS._wseiOperationId = operationId
      , WS._wseiQueryExecutionTime = Just $! realToFrac execTime
      , WS._wseiResponseSize = Just $! LBS.length bs
      }

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

data OperationDetails
  = OperationDetails
  { _odOperationId   :: !OperationId
  , _odRequestId     :: !(Maybe RequestId)
  , _odOperationName :: !(Maybe OperationName)
  , _odOperationType :: !OpDetail
  , _odQuery         :: !(Maybe GQLReqUnparsed)
  } deriving (Show, Eq)
$(J.deriveToJSON hasuraJSON ''OperationDetails)

data WSEvent
  = EAccepted
  | ERejected !QErr
  | EConnErr !ConnErrMsg
  | EOperation !OperationDetails
  | EClosed
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 1
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''WSEvent)

data WsConnInfo
  = WsConnInfo
  { _wsciWebsocketId :: !WS.WSId
  , _wsciTokenExpiry :: !(Maybe TC.UTCTime)
  , _wsciMsg         :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveToJSON hasuraJSON ''WsConnInfo)

data WSLogInfo
  = WSLogInfo
  { _wsliUserVars       :: !(Maybe SessionVariables)
  , _wsliConnectionInfo :: !WsConnInfo
  , _wsliEvent          :: !WSEvent
  } deriving (Show, Eq)
$(J.deriveToJSON hasuraJSON ''WSLogInfo)

data WSLog
  = WSLog
  { _wslLogLevel :: !L.LogLevel
  , _wslInfo     :: !WSLogInfo
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

data WSServerEnv
  = WSServerEnv
  { _wseLogger          :: !(L.Logger L.Hasura)
  , _wseLiveQMap        :: !LQ.LiveQueriesState
  , _wseGCtxMap         :: !(IO (SchemaCache, SchemaCacheVer))
  -- ^ an action that always returns the latest version of the schema cache. See 'SchemaCacheRef'.
  , _wseHManager        :: !H.Manager
  , _wseCorsPolicy      :: !CorsPolicy
  , _wseSQLCtx          :: !SQLGenCtx
  -- , _wseQueryCache      :: !E.PlanCache -- See Note [Temporarily disabling query plan caching]
  , _wseServer          :: !WSServer
  , _wseEnableAllowlist :: !Bool
  , _wseKeepAliveDelay  :: !KeepAliveDelay
  }

onConn :: (MonadIO m, MonadReader WSServerEnv m)
       => WS.OnConnH m WSConnData
onConn wsId requestHead ipAddress = do
  res <- runExceptT $ do
    (errType, queryType) <- checkPath
    let reqHdrs = WS.requestHeaders requestHead
    headers <- maybe (return reqHdrs) (flip enforceCors reqHdrs . snd) getOrigin
    return (WsHeaders $ filterWsHeaders headers, errType, queryType)
  either reject accept res

  where
    keepAliveAction keepAliveDelay wsConn = do
      liftIO $ forever $ do
        sendMsg wsConn SMConnKeepAlive
        sleep $ seconds (unKeepAliveDelay keepAliveDelay)

    tokenExpiryHandler wsConn = do
      expTime <- liftIO $ STM.atomically $ do
        connState <- STM.readTVar $ (_wscUser . WS.getData) wsConn
        case connState of
          CSNotInitialised _ _      -> STM.retry
          CSInitError _             -> STM.retry
          CSInitialised clientState -> onNothing (wscsTokenExpTime clientState) STM.retry
      currTime <- TC.getCurrentTime
      sleep $ convertDuration $ TC.diffUTCTime expTime currTime

    accept (hdrs, errType, queryType) = do
      (L.Logger logger) <- asks _wseLogger
      keepAliveDelay <- asks _wseKeepAliveDelay
      logger $ mkWsInfoLog Nothing (WsConnInfo wsId Nothing Nothing) EAccepted
      connData <- liftIO $ WSConnData
                  <$> STM.newTVarIO (CSNotInitialised hdrs ipAddress)
                  <*> STMMap.newIO
                  <*> pure errType
                  <*> pure queryType
      let acceptRequest = WS.defaultAcceptRequest
                          { WS.acceptSubprotocol = Just "graphql-ws"}
      return $ Right $ WS.AcceptWith connData acceptRequest (keepAliveAction keepAliveDelay) tokenExpiryHandler
    reject qErr = do
      (L.Logger logger) <- asks _wseLogger
      logger $ mkWsErrorLog Nothing (WsConnInfo wsId Nothing Nothing) (ERejected qErr)
      return $ Left $ WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (LBS.toStrict $ J.encode $ encodeGQLErr False qErr)

    checkPath = case WS.requestPath requestHead of
      "/v1alpha1/graphql" -> return (ERTLegacy, E.QueryHasura)
      "/v1/graphql"       -> return (ERTGraphqlCompliant, E.QueryHasura)
      "/v1beta1/relay"    -> return (ERTGraphqlCompliant, E.QueryRelay)
      _                   ->
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

onStart
  :: forall m
   . ( HasVersion
     , MonadIO m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , Tracing.MonadTrace m
     , MonadExecuteQuery m
     , MC.MonadBaseControl IO m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => Env.Environment
  -> WSServerEnv
  -> WSConn
  -> StartMsg
  -> m ()
onStart env serverEnv wsConn (StartMsg opId q) = catchAndIgnore $ do
  timerTot <- startTimer
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  -- NOTE: it should be safe to rely on this check later on in this function, since we expect that
  -- we process all operations on a websocket connection serially:
  when (isJust opM) $ withComplete $ sendStartErr $
    "an operation already exists with this id: " <> unOperationId opId

  userInfoM <- liftIO $ STM.readTVarIO userInfoR
  (userInfo, reqHdrs, ipAddress) <- case userInfoM of
    CSInitialised WsClientState{..} -> return (wscsUserInfo, wscsReqHeaders, wscsIpAddress)
    CSInitError initErr -> do
      let e = "cannot start as connection_init failed with : " <> initErr
      withComplete $ sendStartErr e
    CSNotInitialised _ _ -> do
      let e = "start received before the connection is initialised"
      withComplete $ sendStartErr e

  requestId <- getRequestId reqHdrs
  (sc, scVer) <- liftIO getSchemaCache

  reqParsedE <- lift $ E.checkGQLExecution userInfo (reqHdrs, ipAddress) enableAL sc q
  reqParsed <- onLeft reqParsedE (withComplete . preExecErr requestId)
  execPlanE <- runExceptT $ E.getResolvedExecPlan
    env logger {- planCache -}
    userInfo sqlGenCtx sc scVer queryType
    httpMgr reqHdrs (q, reqParsed)

  (telemCacheHit, (_normalizeSelSet, execPlan)) <- onLeft execPlanE (withComplete . preExecErr requestId)

  case execPlan of
    E.QueryExecutionPlan queryPlan asts -> Tracing.trace "Query" $ do
      let filteredSessionVars = runSessVarPred (filterVariablesFromQuery asts) (_uiSession userInfo)
          cacheKey = QueryCacheKey reqParsed (_uiRole userInfo) filteredSessionVars
          remoteJoins = OMap.elems queryPlan >>= \case
            E.ExecStepDB _remoteHeaders exists ->
              AB.dispatchAnyBackend @BackendTransport exists EB.getRemoteSchemaInfo
            _ -> []
          actionsInfo = foldl getExecStepActionWithActionInfo [] $ OMap.elems $ OMap.filter (\x -> case x of
              E.ExecStepAction (_, _) -> True
              _                       -> False
              ) queryPlan

      -- We ignore the response headers (containing TTL information) because
      -- WebSockets don't support them.
      (_responseHeaders, cachedValue) <- Tracing.interpTraceT (withExceptT mempty) $ cacheLookup remoteJoins actionsInfo cacheKey
      case cachedValue of
        Just cachedResponseData -> do
          sendSuccResp cachedResponseData $ LQ.LiveQueryMetadata 0
        Nothing -> do
          conclusion <- runExceptT $ forWithKey queryPlan $ \fieldName -> \case
            E.ExecStepDB _headers exists -> doQErr $ do
              (telemTimeIO_DT, resp) <-
                AB.dispatchAnyBackend @BackendTransport exists
                  \(EB.DBStepInfo _ sourceConfig genSql tx) ->
                     runDBQuery
                       requestId
                       q
                       fieldName
                       userInfo
                       logger
                       sourceConfig
                       tx
                       genSql
              return $ ResultsFragment telemTimeIO_DT Telem.Local resp []
            E.ExecStepRemote rsi gqlReq -> do
              runRemoteGQ fieldName userInfo reqHdrs rsi gqlReq
            E.ExecStepAction (actionExecPlan, _) -> do
              (time, (r, _)) <- doQErr $ EA.runActionExecution actionExecPlan
              pure $ ResultsFragment time Telem.Empty r []
            E.ExecStepRaw json ->
              buildRaw json
          buildResultFromFragments Telem.Query telemCacheHit timerTot requestId conclusion
          case conclusion of
            Left _        -> pure ()
            Right results -> Tracing.interpTraceT (withExceptT mempty) $
                             cacheStore cacheKey $ encJFromInsOrdHashMap $
                             rfResponse <$> OMap.mapKeys G.unName results
      liftIO $ sendCompleted (Just requestId)

    E.MutationExecutionPlan mutationPlan -> do
      -- See Note [Backwards-compatible transaction optimisation]
      case coalescePostgresMutations mutationPlan of
        -- we are in the aforementioned case; we circumvent the normal process
        Just (sourceConfig, pgMutations) -> do
          resp <- runExceptT $ doQErr $
            runPGMutationTransaction requestId q userInfo logger sourceConfig pgMutations
          -- we do not construct result fragments since we have only one result
          buildResult requestId resp \(telemTimeIO_DT, results) -> do
            let telemQueryType = Telem.Query
                telemLocality  = Telem.Local
                telemTimeIO    = convertDuration telemTimeIO_DT
            telemTimeTot <- Seconds <$> timerTot
            sendSuccResp (encJFromInsOrdHashMap $ OMap.mapKeys G.unName results) $
              LQ.LiveQueryMetadata telemTimeIO_DT
            -- Telemetry. NOTE: don't time network IO:
            Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}

        -- we are not in the transaction case; proceeding normally
        Nothing -> do
          conclusion <- runExceptT $ forWithKey mutationPlan $ \fieldName -> \case
            -- Ignoring response headers since we can't send them over WebSocket
            E.ExecStepDB _responseHeaders exists -> doQErr $ do
              (telemTimeIO_DT, resp) <-
                AB.dispatchAnyBackend @BackendTransport exists
                  \(EB.DBStepInfo _ sourceConfig genSql tx) ->
                       runDBMutation
                         requestId
                         q
                         fieldName
                         userInfo
                         logger
                         sourceConfig
                         tx
                         genSql
              return $ ResultsFragment telemTimeIO_DT Telem.Local resp []
            E.ExecStepAction (actionExecPlan, _) -> do
              (time, (r, hdrs)) <- doQErr $ EA.runActionExecution actionExecPlan
              pure $ ResultsFragment time Telem.Empty r $ fromMaybe [] hdrs
            E.ExecStepRemote rsi gqlReq -> do
              runRemoteGQ fieldName userInfo reqHdrs rsi gqlReq
            E.ExecStepRaw json ->
              buildRaw json
          buildResultFromFragments Telem.Query telemCacheHit timerTot requestId conclusion
      liftIO $ sendCompleted (Just requestId)

    E.SubscriptionExecutionPlan subExec -> do
      -- log the graphql query
      logQueryLog logger $ QueryLog q Nothing requestId Subscription

      case subExec of
        E.SEAsyncActionsWithNoRelationships actions -> liftIO do
          let allActionIds = map fst $ OMap.elems actions
          case NE.nonEmpty allActionIds of
            Nothing -> sendCompleted $ Just requestId
            Just actionIds -> do
              let sendResponseIO actionLogMap = do
                   (dTime, resultsE) <- withElapsedTime $ runExceptT $
                     for actions $ \(actionId, resultBuilder) -> do
                       actionLogResponse <- Map.lookup actionId actionLogMap
                         `onNothing` throw500 "unexpected: cannot lookup action_id in response map"
                       liftEither $ resultBuilder actionLogResponse
                   case resultsE of
                     Left err -> sendError requestId err
                     Right results -> do
                       let dataMsg = SMData $ DataMsg opId $ pure $ encJToLBS $
                                     encJFromInsOrdHashMap $ OMap.mapKeys G.unName results
                       sendMsgWithMetadata wsConn dataMsg $ LQ.LiveQueryMetadata dTime

                  asyncActionQueryLive = LQ.LAAQNoRelationships $
                    LQ.LiveAsyncActionQueryWithNoRelationships sendResponseIO (sendCompleted (Just requestId))

              LQ.addAsyncActionLiveQuery (LQ._lqsAsyncActions lqMap) opId actionIds
                                         (sendError requestId) asyncActionQueryLive

        E.SEOnSourceDB actionIds liveQueryBuilder -> do
          actionLogMapE <- fmap fst <$> runExceptT (EA.fetchActionLogResponses actionIds)
          actionLogMap <- onLeft actionLogMapE (withComplete . preExecErr requestId)
          lqIdE <- liftIO $ startLiveQuery liveQueryBuilder actionLogMap
          lqId <- onLeft lqIdE (withComplete . preExecErr requestId)

          -- Update async action query subscription state
          case NE.nonEmpty (toList actionIds) of
            Nothing                ->
              -- No async action query fields present, do nothing.
              pure ()
            Just nonEmptyActionIds -> liftIO $ do
              let asyncActionQueryLive = LQ.LAAQOnSourceDB $
                    LQ.LiveAsyncActionQueryOnSource lqId actionLogMap $ restartLiveQuery liveQueryBuilder

                  onUnexpectedException err = do
                    sendError requestId err
                    stopOperation serverEnv wsConn opId (pure ()) -- Don't log in case opId don't exist

              LQ.addAsyncActionLiveQuery (LQ._lqsAsyncActions lqMap) opId
                                          nonEmptyActionIds onUnexpectedException
                                          asyncActionQueryLive

      liftIO $ logOpEv ODStarted (Just requestId)
  where
    getExecStepActionWithActionInfo acc execStep = case execStep of
       E.ExecStepAction (_, actionInfo) -> (actionInfo:acc)
       _                                -> acc

    doQErr = withExceptT Right

    forWithKey = flip OMap.traverseWithKey

    telemTransport = Telem.WebSocket

    buildResult
      :: forall a
       . RequestId
      -> Either (Either GQExecError QErr) a
      -> (a -> ExceptT () m ())
      -> ExceptT () m ()
    buildResult requestId r f = case r of
      Left (Left  err) -> postExecErr' err
      Left (Right err) -> postExecErr requestId err
      Right results    -> f results

    buildResultFromFragments telemQueryType telemCacheHit timerTot requestId r =
      buildResult requestId r \results -> do
        let telemLocality = foldMap rfLocality results
            telemTimeIO   = convertDuration $ sum $ fmap rfTimeIO results
        telemTimeTot <- Seconds <$> timerTot
        sendSuccResp (encJFromInsOrdHashMap (fmap rfResponse (OMap.mapKeys G.unName results))) $
          LQ.LiveQueryMetadata $ sum $ fmap rfTimeIO results
        -- Telemetry. NOTE: don't time network IO:
        Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}

    runRemoteGQ fieldName userInfo reqHdrs rsi gqlReq = do
      (telemTimeIO_DT, _respHdrs, resp) <-
        doQErr $ E.execRemoteGQ env httpMgr userInfo reqHdrs rsi gqlReq
      value <- mapExceptT lift $ extractFieldFromResponse (G.unName fieldName) resp
      return $ ResultsFragment telemTimeIO_DT Telem.Remote (JO.toEncJSON value) []

    WSServerEnv logger lqMap getSchemaCache httpMgr _ sqlGenCtx {- planCache -}
      _ enableAL _keepAliveDelay = serverEnv

    WSConnData userInfoR opMap errRespTy queryType = WS.getData wsConn

    logOpEv opTy reqId = logWSEvent logger wsConn $ EOperation $
      OperationDetails opId reqId (_grOperationName q) opTy $
      -- log the query only in errors
      case opTy of
        ODQueryErr _ -> Just q
        _            -> Nothing

    getErrFn ERTLegacy           = encodeQErr
    getErrFn ERTGraphqlCompliant = encodeGQLErr

    sendStartErr e = do
      let errFn = getErrFn errRespTy
      sendMsg wsConn $
        SMErr $ ErrorMsg opId $ errFn False $ err400 StartFailed e
      liftIO $ logOpEv (ODProtoErr e) Nothing

    sendCompleted reqId = do
      sendMsg wsConn (SMComplete $ CompletionMsg opId)
      logOpEv ODCompleted reqId

    postExecErr :: RequestId -> QErr -> ExceptT () m ()
    postExecErr reqId qErr = do
      let errFn = getErrFn errRespTy False
      liftIO $ logOpEv (ODQueryErr qErr) (Just reqId)
      postExecErr' $ GQExecError $ pure $ errFn qErr

    postExecErr' :: GQExecError -> ExceptT () m ()
    postExecErr' qErr = do
      sendMsg wsConn $ SMData $
        DataMsg opId $ throwError qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr reqId qErr = liftIO $ sendError reqId qErr

    sendError reqId qErr = do
      let errFn = getErrFn errRespTy
      logOpEv (ODQueryErr qErr) (Just reqId)
      let err = case errRespTy of
            ERTLegacy           -> errFn False qErr
            ERTGraphqlCompliant -> J.object ["errors" J..= [errFn False qErr]]
      sendMsg wsConn (SMErr $ ErrorMsg opId err)

    sendSuccResp :: EncJSON -> LQ.LiveQueryMetadata -> ExceptT () m ()
    sendSuccResp encJson =
      sendMsgWithMetadata wsConn
        (SMData $ DataMsg opId $ pure $ encJToLBS encJson)

    withComplete :: ExceptT () m () -> ExceptT () m a
    withComplete action = do
      action
      liftIO $ sendCompleted Nothing
      throwError ()

    restartLiveQuery liveQueryBuilder lqId actionLogMap = do
      LQ.removeLiveQuery logger lqMap lqId
      either (const Nothing) Just <$> startLiveQuery liveQueryBuilder actionLogMap

    startLiveQuery liveQueryBuilder actionLogMap = do
      liveQueryE <- runExceptT $ liveQueryBuilder actionLogMap
      for liveQueryE $ \(sourceName, E.LQP exists) -> do
        let subscriberMetadata = LQ.mkSubscriberMetadata $ J.object
                                 [ "websocket_id" J..= WS.getWSId wsConn
                                 , "operation_id" J..= opId
                                 ]

        -- NOTE!: we mask async exceptions higher in the call stack, but it's
        -- crucial we don't lose lqId after addLiveQuery returns successfully.
        !lqId <- liftIO $ AB.dispatchAnyBackend @BackendTransport exists
          \(E.MultiplexedLiveQueryPlan liveQueryPlan) ->
            LQ.addLiveQuery logger subscriberMetadata lqMap sourceName liveQueryPlan liveQOnChange
        let !opName = _grOperationName q
#ifndef PROFILING
        liftIO $ $assertNFHere (lqId, opName)  -- so we don't write thunks to mutable vars
#endif

        STM.atomically $
          -- NOTE: see crucial `lookup` check above, ensuring this doesn't clobber:
          STMMap.insert (lqId, opName) opId opMap
        pure lqId

    -- on change, send message on the websocket
    liveQOnChange :: LQ.OnChange
    liveQOnChange = \case
      Right (LQ.LiveQueryResponse bs dTime) ->
        sendMsgWithMetadata wsConn
        (SMData $ DataMsg opId $ pure $ LBS.fromStrict bs)
        (LQ.LiveQueryMetadata dTime)
      resp -> sendMsg wsConn $ SMData $ DataMsg opId $ LBS.fromStrict . LQ._lqrPayload <$> resp

    catchAndIgnore :: ExceptT () m () -> m ()
    catchAndIgnore m = void $ runExceptT m

onMessage
  :: ( HasVersion
     , MonadIO m
     , UserAuthentication (Tracing.TraceT m)
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , Tracing.HasReporter m
     , MonadExecuteQuery m
     , MC.MonadBaseControl IO m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => Env.Environment
  -> AuthMode
  -> WSServerEnv
  -> WSConn -> LBS.ByteString -> m ()
onMessage env authMode serverEnv wsConn msgRaw = Tracing.runTraceT "websocket" do
  case J.eitherDecode msgRaw of
    Left e    -> do
      let err = ConnErrMsg $ "parsing ClientMessage failed: " <> T.pack e
      logWSEvent logger wsConn $ EConnErr err
      sendMsg wsConn $ SMConnErr err

    Right msg -> case msg of
      CMConnInit params -> onConnInit (_wseLogger serverEnv)
                           (_wseHManager serverEnv)
                           wsConn authMode params
      CMStart startMsg  -> onStart env serverEnv wsConn startMsg
      CMStop stopMsg    -> liftIO $ onStop serverEnv wsConn stopMsg
      -- The idea is cleanup will be handled by 'onClose', but...
      -- NOTE: we need to close the websocket connection when we receive the
      -- CMConnTerm message and calling WS.closeConn will definitely throw an
      -- exception, but I'm not sure if 'closeConn' is the correct thing here....
      CMConnTerm        -> liftIO $ WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = _wseLogger serverEnv


onStop :: WSServerEnv -> WSConn -> StopMsg -> IO ()
onStop serverEnv wsConn (StopMsg opId) = do
  -- When a stop message is received for an operation, it may not be present in OpMap
  -- in these cases:
  -- 1. If the operation is a query/mutation - as we remove the operation from the
  -- OpMap as soon as it is executed
  -- 2. A misbehaving client
  -- 3. A bug on our end
  stopOperation serverEnv wsConn opId $
    L.unLogger logger $ L.UnstructuredLog L.LevelDebug $ fromString $
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
      LQ.removeLiveQuery logger lqMap lqId
    Nothing    -> logWhenOpNotExist
  STM.atomically $ STMMap.delete opId opMap
  where
    logger = _wseLogger serverEnv
    lqMap  = _wseLiveQMap serverEnv
    opMap  = _wscOpMap $ WS.getData wsConn
    opDet n = OperationDetails opId Nothing n ODStopped Nothing

logWSEvent
  :: (MonadIO m)
  => L.Logger L.Hasura -> WSConn -> WSEvent -> m ()
logWSEvent (L.Logger logger) wsConn wsEv = do
  userInfoME <- liftIO $ STM.readTVarIO userInfoR
  let (userVarsM, tokenExpM) = case userInfoME of
        CSInitialised WsClientState{..} -> ( Just $ _uiSession wscsUserInfo
                                           , wscsTokenExpTime
                                           )
        _                               -> (Nothing, Nothing)
  liftIO $ logger $ WSLog logLevel $ WSLogInfo userVarsM (WsConnInfo wsId tokenExpM Nothing) wsEv
  where
    WSConnData userInfoR _ _ _ = WS.getData wsConn
    wsId = WS.getWSId wsConn
    logLevel = bool L.LevelInfo L.LevelError isError
    isError = case wsEv of
      EAccepted   -> False
      ERejected _ -> True
      EConnErr _  -> True
      EClosed     -> False
      EOperation operation -> case _odOperationType operation of
        ODStarted    -> False
        ODProtoErr _ -> True
        ODQueryErr _ -> True
        ODCompleted  -> False
        ODStopped    -> False

onConnInit
  :: (HasVersion, MonadIO m, UserAuthentication (Tracing.TraceT m))
  => L.Logger L.Hasura -> H.Manager -> WSConn -> AuthMode
  -> Maybe ConnParams -> Tracing.TraceT m ()
onConnInit logger manager wsConn authMode connParamsM = do
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
#ifndef PROFILING
            $assertNFHere initErr  -- so we don't write thunks to mutable vars
#endif
            STM.atomically $ STM.writeTVar (_wscUser $ WS.getData wsConn) initErr

          let connErr = ConnErrMsg $ qeError e
          logWSEvent logger wsConn $ EConnErr connErr
          sendMsg wsConn $ SMConnErr connErr

        Right (userInfo, expTimeM) -> do
          let !csInit = CSInitialised $ WsClientState userInfo expTimeM paramHeaders ipAddress
          liftIO $ do
#ifndef PROFILING
            $assertNFHere csInit  -- so we don't write thunks to mutable vars
#endif
            STM.atomically $ STM.writeTVar (_wscUser $ WS.getData wsConn) csInit

          sendMsg wsConn SMConnAck
          -- TODO(from master): send it periodically? Why doesn't apollo's protocol use
          -- ping/pong frames of websocket spec?
          sendMsg wsConn SMConnKeepAlive
  where
    unexpectedInitError e = do
      let connErr = ConnErrMsg e
      logWSEvent logger wsConn $ EConnErr connErr
      sendMsg wsConn $ SMConnErr connErr

    getIpAddress = \case
      CSNotInitialised _ ip           -> return ip
      CSInitialised WsClientState{..} -> return wscsIpAddress
      CSInitError e                   -> Left e

    mkHeaders st =
      paramHeaders ++ getClientHdrs st

    paramHeaders =
      [ (CI.mk $ TE.encodeUtf8 h, TE.encodeUtf8 v)
      | (h, v) <- maybe [] Map.toList $ connParamsM >>= _cpHeaders
      ]

    getClientHdrs st = case st of
      CSNotInitialised h _ -> unWsHeaders h
      _                    -> []

onClose
  :: MonadIO m
  => L.Logger L.Hasura
  -> LQ.LiveQueriesState
  -> WSConn
  -> m ()
onClose logger lqMap wsConn = do
  logWSEvent logger wsConn EClosed
  operations <- liftIO $ STM.atomically $ ListT.toList $ STMMap.listT opMap
  liftIO $ for_ operations $ \(_, (lqId, _)) ->
    LQ.removeLiveQuery logger lqMap lqId
  where
    opMap = _wscOpMap $ WS.getData wsConn

createWSServerEnv
  :: (MonadIO m)
  => L.Logger L.Hasura
  -> LQ.LiveQueriesState
  -> IO (SchemaCache, SchemaCacheVer)
  -> H.Manager
  -> CorsPolicy
  -> SQLGenCtx
  -> Bool
  -> KeepAliveDelay
  -- -> E.PlanCache
  -> m WSServerEnv
createWSServerEnv logger lqState getSchemaCache httpManager
  corsPolicy sqlGenCtx enableAL keepAliveDelay {- planCache -} = do
  wsServer <- liftIO $ STM.atomically $ WS.createWSServer logger
  return $
    WSServerEnv logger lqState getSchemaCache httpManager corsPolicy
    sqlGenCtx {- planCache -} wsServer enableAL keepAliveDelay

createWSServerApp
  :: ( HasVersion
     , MonadIO m
     , MC.MonadBaseControl IO m
     , LA.Forall (LA.Pure m)
     , UserAuthentication (Tracing.TraceT m)
     , E.MonadGQLExecutionCheck m
     , WS.MonadWSLog m
     , MonadQueryLog m
     , Tracing.HasReporter m
     , MonadExecuteQuery m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => Env.Environment
  -> AuthMode
  -> WSServerEnv
  -> WS.HasuraServerApp m
--   -- ^ aka generalized 'WS.ServerApp'
createWSServerApp env authMode serverEnv = \ !ipAddress !pendingConn ->
  WS.createServerApp (_wseServer serverEnv) handlers ipAddress pendingConn
  where
    handlers =
      WS.WSHandlers
      -- Mask async exceptions during event processing to help maintain integrity of mutable vars:
      (\rid rh ip -> mask_ $ flip runReaderT serverEnv $ onConn rid rh ip)
      (\conn bs -> mask_ $ onMessage env authMode serverEnv conn bs)
      (mask_ . onClose (_wseLogger serverEnv) (_wseLiveQMap serverEnv))

stopWSServerApp :: WSServerEnv -> IO ()
stopWSServerApp wsEnv = WS.shutdown (_wseServer wsEnv)
