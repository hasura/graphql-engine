{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.GraphQL.Transport.WebSocket.Server
  ( AcceptWith (AcceptWith),
    HasuraServerApp,
    MessageDetails (MessageDetails),
    MonadWSLog (..),
    OnConnH,
    WSActions (..),
    WSConn,
    WSErrorMessage (..),
    WSEvent (EMessageSent),
    WSEventInfo (WSEventInfo, _wseiEventType, _wseiOperationId, _wseiOperationName, _wseiParameterizedQueryHash, _wseiQueryExecutionTime, _wseiResponseSize),
    WSHandlers (WSHandlers),
    WSId,
    WSKeepAliveMessageAction,
    WSLog (WSLog),
    WSOnErrorMessageAction,
    WSQueueResponse (WSQueueResponse),
    WSServer (..),
    websocketConnectionReaper,
    closeConn,
    sendMsgAndCloseConn,
    createServerApp,
    createWSServer,
    closeAllConnectionsWithReason,
    getData,
    getRawWebSocketConnection,
    getWSId,
    mkWSServerErrorCode,
    sendMsg,
    shutdown,

    -- * exported for testing
    mkUnsafeWSId,
  )
where

import Control.Concurrent.Async qualified as A
import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.Extended (sleep)
import Control.Concurrent.STM (readTVarIO)
import Control.Concurrent.STM qualified as STM
import Control.Exception.Lifted
import Control.Monad.Trans.Control qualified as MC
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.TH qualified as J
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.HashSet qualified as Set
import Data.SerializableBlob qualified as SB
import Data.String
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Word (Word16)
import GHC.AssertNF.CPP
import GHC.Int (Int64)
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Schema.NamingCase (hasNamingConventionChanged)
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.GraphQL.Transport.WebSocket.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Common (MetricsConfig (..), SQLGenCtx (..))
import Hasura.RQL.Types.NamingCase (NamingCase (..))
import Hasura.RQL.Types.SchemaCache
import Hasura.Server.Auth (AuthMode, compareAuthMode)
import Hasura.Server.Cors (CorsPolicy)
import Hasura.Server.Init.Config (AllowListStatus (..), WSConnectionInitTimeout (..))
import Hasura.Server.Prometheus
  ( DynamicSubscriptionLabel (..),
    PrometheusMetrics (..),
    recordMetricWithLabel,
  )
import Hasura.Server.Types (ExperimentalFeature (..), MonadGetPolicies (runGetPrometheusMetricsGranularity))
import ListT qualified
import Network.Wai.Extended (IpAddress)
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Refined (unrefine)
import StmContainers.Map qualified as STMMap
import System.IO.Error qualified as E
import System.Metrics.Prometheus.Counter qualified as Prometheus.Counter
import System.Metrics.Prometheus.CounterVector qualified as CounterVector
import System.Metrics.Prometheus.Histogram qualified as Prometheus.Histogram
import System.TimeManager qualified as TM

newtype WSId = WSId {unWSId :: UUID.UUID}
  deriving (Show, Eq, Hashable)

mkUnsafeWSId :: UUID.UUID -> WSId
mkUnsafeWSId = WSId

instance J.ToJSON WSId where
  toJSON (WSId uuid) =
    J.toJSON $ UUID.toText uuid

-- | Websocket message and other details
data MessageDetails = MessageDetails
  { _mdMessage :: !SB.SerializableBlob,
    _mdMessageSize :: !Int64
  }
  deriving (Show)

$(J.deriveToJSON hasuraJSON ''MessageDetails)

data WSEvent
  = EConnectionRequest
  | EAccepted
  | ERejected
  | EMessageReceived !MessageDetails
  | EMessageSent !MessageDetails
  | EJwtExpired
  | ECloseReceived
  | ECloseSent !SB.SerializableBlob
  | EClosed
  deriving (Show)

$( J.deriveToJSON
     J.defaultOptions
       { J.constructorTagModifier = J.snakeCase . drop 1,
         J.sumEncoding = J.TaggedObject "type" "detail"
       }
     ''WSEvent
 )

-- extra websocket event info
data WSEventInfo = WSEventInfo
  { _wseiEventType :: !(Maybe ServerMsgType),
    _wseiOperationId :: !(Maybe OperationId),
    _wseiOperationName :: !(Maybe OperationName),
    _wseiQueryExecutionTime :: !(Maybe Double),
    _wseiResponseSize :: !(Maybe Int64),
    _wseiParameterizedQueryHash :: !(Maybe ParameterizedQueryHash)
  }
  deriving (Show, Eq)

$( J.deriveToJSON
     J.defaultOptions
       { J.fieldLabelModifier = J.snakeCase . drop 5,
         J.omitNothingFields = True
       }
     ''WSEventInfo
 )

data WSLog = WSLog
  { _wslWebsocketId :: !WSId,
    _wslEvent :: !WSEvent,
    _wslMetadata :: !(Maybe WSEventInfo)
  }
  deriving (Show)

$( J.deriveToJSON
     J.defaultOptions
       { J.fieldLabelModifier = J.snakeCase . drop 4,
         J.omitNothingFields = True
       }
     ''WSLog
 )

class (Monad m) => MonadWSLog m where
  -- | Takes WS server log data and logs it
  -- logWSServer
  logWSLog :: L.Logger L.Hasura -> WSLog -> m ()

instance (MonadWSLog m) => MonadWSLog (ExceptT e m) where
  logWSLog l ws = lift $ logWSLog l ws

instance (MonadWSLog m) => MonadWSLog (ReaderT r m) where
  logWSLog l ws = lift $ logWSLog l ws

instance L.ToEngineLog WSLog L.Hasura where
  toEngineLog wsLog =
    (L.LevelDebug, L.ELTInternal L.ILTWsServer, J.toJSON wsLog)

data WSReaperThreadLog = WSReaperThreadLog
  { _wrtlMessage :: Text
  }
  deriving (Show)

instance L.ToEngineLog WSReaperThreadLog L.Hasura where
  toEngineLog (WSReaperThreadLog message) =
    (L.LevelInfo, L.ELTInternal L.ILTWsServer, J.toJSON message)

data WSQueueResponse = WSQueueResponse
  { _wsqrMessage :: BL.ByteString,
    -- | extra metadata that we use for other actions, such as print log
    -- we don't want to inlcude them into websocket message payload
    _wsqrEventInfo :: Maybe WSEventInfo,
    -- | Timer to compute the time for which the websocket message
    --   remains queued.
    _wsqrTimer :: IO DiffTime
  }

data WSConn a = WSConn
  { _wcConnId :: !WSId,
    _wcLogger :: !(L.Logger L.Hasura),
    _wcConnRaw :: !WS.Connection,
    _wcSendQ :: !(STM.TQueue WSQueueResponse),
    _wcExtraData :: !a
  }

getRawWebSocketConnection :: WSConn a -> WS.Connection
getRawWebSocketConnection = _wcConnRaw

getData :: WSConn a -> a
getData = _wcExtraData

getWSId :: WSConn a -> WSId
getWSId = _wcConnId

closeConn :: WSConn a -> BL.ByteString -> IO ()
closeConn wsConn = closeConnWithCode wsConn 1000 -- 1000 is "normal close"

-- | Closes a connection with code 1012, which means "Server is restarting"
-- good clients will implement a retry logic with a backoff of a few seconds
forceConnReconnect :: (MonadIO m) => WSConn a -> BL.ByteString -> m ()
forceConnReconnect wsConn bs = liftIO $ closeConnWithCode wsConn 1012 bs

closeConnWithCode :: WSConn a -> Word16 -> BL.ByteString -> IO ()
closeConnWithCode wsConn code bs = do
  ((\x -> L.unLogger x) . _wcLogger) wsConn
    $ WSLog (_wcConnId wsConn) (ECloseSent $ SB.fromLBS bs) Nothing
  WS.sendCloseCode (_wcConnRaw wsConn) code bs

sendMsgAndCloseConn :: WSConn a -> Word16 -> BL.ByteString -> ServerMsg -> IO ()
sendMsgAndCloseConn wsConn errCode bs serverErr = do
  WS.sendTextData (_wcConnRaw wsConn) (encodeServerMsg serverErr)
  WS.sendCloseCode (_wcConnRaw wsConn) errCode bs

-- writes to a queue instead of the raw connection
-- so that sendMsg doesn't block
sendMsg :: WSConn a -> WSQueueResponse -> IO ()
sendMsg wsConn !resp = do
  $assertNFHere resp -- so we don't write thunks to mutable vars
  STM.atomically $ STM.writeTQueue (_wcSendQ wsConn) resp

type ConnMap a = STMMap.Map WSId (WSConn a)

data ServerStatus a
  = AcceptingConns !(ConnMap a)
  | ShuttingDown

data WSServer a = WSServer
  { _wssLogger :: L.Logger L.Hasura,
    -- | Keep track of the security sensitive user configuration to perform
    -- maintenance actions
    _wssSecuritySensitiveUserConfig :: STM.TVar SecuritySensitiveUserConfig,
    -- | See e.g. createServerApp.onAccept for how we use STM to preserve consistency
    _wssStatus :: STM.TVar (ServerStatus a)
  }

-- These are security sensitive user configuration. That is, if any of the
-- following config changes, we need to perform maintenance actions like closing
-- all websocket connections
data SecuritySensitiveUserConfig = SecuritySensitiveUserConfig
  { ssucAuthMode :: AuthMode,
    ssucEnableAllowlist :: AllowListStatus,
    ssucAllowlist :: InlinedAllowlist,
    ssucCorsPolicy :: CorsPolicy,
    ssucSQLGenCtx :: SQLGenCtx,
    ssucExperimentalFeatures :: Set.HashSet ExperimentalFeature,
    ssucDefaultNamingCase :: NamingCase
  }

createWSServer :: AuthMode -> AllowListStatus -> InlinedAllowlist -> CorsPolicy -> SQLGenCtx -> Set.HashSet ExperimentalFeature -> NamingCase -> L.Logger L.Hasura -> STM.STM (WSServer a)
createWSServer authMode enableAllowlist allowlist corsPolicy sqlGenCtx experimentalFeatured defaultNamingCase logger = do
  connMap <- STMMap.new
  userConfRef <- STM.newTVar $ SecuritySensitiveUserConfig authMode enableAllowlist allowlist corsPolicy sqlGenCtx experimentalFeatured defaultNamingCase
  serverStatus <- STM.newTVar (AcceptingConns connMap)
  return $ WSServer logger userConfRef serverStatus

closeAllWith ::
  (BL.ByteString -> WSConn a -> IO ()) ->
  BL.ByteString ->
  [(WSId, WSConn a)] ->
  IO ()
closeAllWith closer msg conns =
  void $ A.mapConcurrently (closer msg . snd) conns

closeAllConnectionsWithReason ::
  WSServer a ->
  String ->
  BL.ByteString ->
  (SecuritySensitiveUserConfig -> SecuritySensitiveUserConfig) ->
  IO ()
closeAllConnectionsWithReason (WSServer (L.Logger writeLog) userConfRef serverStatus) logMsg reason updateConf = do
  writeLog
    $ WSReaperThreadLog
    $ fromString
    $ logMsg
  conns <- STM.atomically $ do
    STM.modifyTVar' userConfRef updateConf
    flushConnMap serverStatus
  closeAllWith (flip forceConnReconnect) reason conns

-- | Resets the current connections map to an empty one if the server is
-- running and returns the list of connections that were in the map
-- before flushing it.
flushConnMap :: STM.TVar (ServerStatus a) -> STM.STM [(WSId, WSConn a)]
flushConnMap serverStatus = do
  status <- STM.readTVar serverStatus
  case status of
    AcceptingConns connMap -> do
      conns <- ListT.toList $ STMMap.listT connMap
      STMMap.reset connMap
      return conns
    ShuttingDown -> return []

data AcceptWith a = AcceptWith
  { _awData :: !a,
    _awReq :: !WS.AcceptRequest,
    _awKeepAlive :: !(WSConn a -> IO ()),
    _awOnJwtExpiry :: !(WSConn a -> IO ())
  }

-- | These set of functions or message handlers is used by the
--   server while communicating with the client. They are particularly
--   useful for the case when the messages being sent to the client
--   are different for each of the sub-protocol(s) supported by the server.
type WSKeepAliveMessageAction a = WSConn a -> IO ()

type WSPostExecErrMessageAction a = WSConn a -> OperationId -> GQExecError -> IO ()

type WSOnErrorMessageAction a = WSConn a -> ConnErrMsg -> WSErrorMessage -> IO ()

type WSCloseConnAction a = WSConn a -> OperationId -> String -> IO ()

-- | Used for specific actions within the `onConn` and `onMessage` handlers
data WSActions a = WSActions
  { _wsaPostExecErrMessageAction :: !(WSPostExecErrMessageAction a),
    _wsaOnErrorMessageAction :: !(WSOnErrorMessageAction a),
    _wsaConnectionCloseAction :: !(WSCloseConnAction a),
    -- | NOTE: keep alive action was made redundant because we need to send this message
    -- after the connection has been successfully established after `connection_init`
    _wsaKeepAliveAction :: !(WSKeepAliveMessageAction a),
    _wsaGetDataMessageType :: !(DataMsg -> ServerMsg),
    _wsaAcceptRequest :: !WS.AcceptRequest,
    _wsaErrorMsgFormat :: !([J.Encoding] -> J.Encoding)
  }

data WSErrorMessage = ClientMessageParseFailed | ConnInitFailed

mkWSServerErrorCode :: WSSubProtocol -> WSErrorMessage -> ConnErrMsg -> ServerErrorCode
mkWSServerErrorCode subProtocol errorMessage connErrMsg = case errorMessage of
  ClientMessageParseFailed -> (GenericError4400 $ ("Parsing client message failed: ") <> (T.unpack . unConnErrMsg $ connErrMsg))
  ConnInitFailed -> case subProtocol of
    Apollo -> (GenericError4400 $ ("Connection initialization failed: ") <> (T.unpack . unConnErrMsg $ connErrMsg))
    GraphQLWS -> Forbidden4403

type OnConnH m a = WSId -> WS.RequestHead -> IpAddress -> WSActions a -> m (Either WS.RejectRequest (AcceptWith a))

-- type OnMessageH m a = WSConn a -> BL.ByteString -> WSActions a -> m ()

type OnCloseH m a = WSConn a -> m ()

-- | aka generalized 'WS.ServerApp' over @m@, which takes an IPAddress
type HasuraServerApp m = IpAddress -> WS.PendingConnection -> m ()

-- | NOTE: The types of `_hOnConn` and `_hOnMessage` were updated from `OnConnH` and `OnMessageH`
-- because we needed to pass the subprotcol here to these methods to eventually get to `OnConnH` and `OnMessageH`.
-- Please see `createServerApp` to get a better understanding of how these handlers are used.
data WSHandlers m a = WSHandlers
  { _hOnConn :: (WSId -> WS.RequestHead -> IpAddress -> WSSubProtocol -> m (Either WS.RejectRequest (AcceptWith a))),
    _hOnMessage :: (WSConn a -> BL.ByteString -> WSSubProtocol -> m ()),
    _hOnClose :: OnCloseH m a
  }

-- | The background thread responsible for closing all websocket connections
-- when security sensitive user configuration changes. It checks for changes in
-- the auth mode, allowlist, cors config, stringify num, dangerous boolean collapse,
-- stringify big query numeric, experimental features and invalidates/closes all
-- connections if there are any changes.
websocketConnectionReaper :: IO (AuthMode, AllowListStatus, CorsPolicy, SQLGenCtx, Set.HashSet ExperimentalFeature, NamingCase) -> IO SchemaCache -> WSServer a -> IO Void
websocketConnectionReaper getLatestConfig getSchemaCache ws@(WSServer _ userConfRef _) =
  forever $ do
    (currAuthMode, currEnableAllowlist, currCorsPolicy, currSqlGenCtx, currExperimentalFeatures, currDefaultNamingCase) <- getLatestConfig
    currAllowlist <- scAllowlist <$> getSchemaCache
    SecuritySensitiveUserConfig prevAuthMode prevEnableAllowlist prevAllowlist prevCorsPolicy prevSqlGenCtx prevExperimentalFeatures prevDefaultNamingCase <- readTVarIO userConfRef
    -- check and close all connections if required
    checkAndReapConnections
      (currAuthMode, prevAuthMode)
      (currCorsPolicy, prevCorsPolicy)
      (currEnableAllowlist, prevEnableAllowlist)
      (currAllowlist, prevAllowlist)
      (currSqlGenCtx, prevSqlGenCtx)
      (currExperimentalFeatures, prevExperimentalFeatures)
      (currDefaultNamingCase, prevDefaultNamingCase)
    sleep $ seconds 1
  where
    -- Close all connections based on -
    -- if CorsPolicy changed -> close
    -- if AuthMode changed -> close
    -- if AllowlistEnabled -> enabled from disabled -> close
    -- if AllowlistEnabled -> allowlist collection changed -> close
    -- if HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES  changed -> close
    -- if HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE changed -> close
    -- if 'bigquery_string_numeric_input', 'hide_aggregation_predicates', 'hide_stream_fields' values added/remove from experimental features -> close
    -- if naming convention changes -> close
    checkAndReapConnections
      (currAuthMode, prevAuthMode)
      (currCorsPolicy, prevCorsPolicy)
      (currEnableAllowlist, prevEnableAllowlist)
      (currAllowlist, prevAllowlist)
      (currSqlGenCtx, prevSqlGenCtx)
      (currExperimentalFeatures, prevExperimentalFeatures)
      (currDefaultNamingCase, prevDefaultNamingCase) = do
        hasAuthModeChanged <- not <$> compareAuthMode currAuthMode prevAuthMode
        let hasCorsPolicyChanged = currCorsPolicy /= prevCorsPolicy
            hasAllowlistEnabled = prevEnableAllowlist == AllowListDisabled && currEnableAllowlist == AllowListEnabled
            hasAllowlistUpdated =
              (prevEnableAllowlist == AllowListEnabled && currEnableAllowlist == AllowListEnabled) && (currAllowlist /= prevAllowlist)
            hasStringifyNumChanged = stringifyNum currSqlGenCtx /= stringifyNum prevSqlGenCtx
            hasDangerousBooleanCollapseChanged = dangerousBooleanCollapse currSqlGenCtx /= dangerousBooleanCollapse prevSqlGenCtx
            -- The bigqueryStringNumericInput of SQLGenCtx is built from the experimentalFeature, hence no need to check for this field
            -- in experimentalFeatures again.
            hasBigqueryStringNumericInputChanged = bigqueryStringNumericInput currSqlGenCtx /= bigqueryStringNumericInput prevSqlGenCtx
            hasHideAggregationPredicatesChanged = (EFHideAggregationPredicates `elem` currExperimentalFeatures) && (EFHideAggregationPredicates `elem` prevExperimentalFeatures)
            hasHideStreamFieldsChanged = (EFHideStreamFields `elem` currExperimentalFeatures) && (EFHideStreamFields `elem` prevExperimentalFeatures)
            hasDefaultNamingCaseChanged = hasNamingConventionChanged (prevExperimentalFeatures, prevDefaultNamingCase) (currExperimentalFeatures, currDefaultNamingCase)
        if
          -- if CORS policy has changed, close all connections
          | hasCorsPolicyChanged ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as the cors policy changed"
                "cors policy changed"
                (\conf -> conf {ssucCorsPolicy = currCorsPolicy})
          -- if any auth config has changed, close all connections
          | hasAuthModeChanged ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as the auth mode changed"
                "auth mode changed"
                (\conf -> conf {ssucAuthMode = currAuthMode})
          -- In case of allowlist, we need to check if the allowlist has changed.
          -- If the allowlist is disabled, we keep all the connections as is.
          -- If the allowlist is enabled from a disabled state, we need to close all the
          -- connections.
          | hasAllowlistEnabled ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as allow list is enabled"
                "allow list enabled"
                (\conf -> conf {ssucEnableAllowlist = currEnableAllowlist})
          -- If the allowlist is already enabled and there are any changes made to the
          -- allowlist, we need to close all the connections.
          | hasAllowlistUpdated ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as the allow list has been updated"
                "allow list updated"
                (\conf -> conf {ssucAllowlist = currAllowlist})
          -- if HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES has changed, close all connections
          | hasStringifyNumChanged ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as the HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES setting changed"
                "HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES env var changed"
                (\conf -> conf {ssucSQLGenCtx = currSqlGenCtx})
          -- if HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE has changed, close all connections
          | hasDangerousBooleanCollapseChanged ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as the HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE setting changed"
                "HASURA_GRAPHQL_V1_BOOLEAN_NULL_COLLAPSE env var changed"
                (\conf -> conf {ssucSQLGenCtx = currSqlGenCtx})
          -- if 'bigquery_string_numeric_input' option added/removed from experimental features, close all connections
          | hasBigqueryStringNumericInputChanged ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as the 'bigquery_string_numeric_input' option has been added/removed from HASURA_GRAPHQL_EXPERIMENTAL_FEATURES"
                "'bigquery_string_numeric_input' removed/added in HASURA_GRAPHQL_EXPERIMENTAL_FEATURES env var"
                (\conf -> conf {ssucSQLGenCtx = currSqlGenCtx})
          -- if 'hide_aggregation_predicates' option added/removed from experimental features, close all connections
          | hasHideAggregationPredicatesChanged ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as the 'hide-aggregation-predicates' option has been added/removed from HASURA_GRAPHQL_EXPERIMENTAL_FEATURES"
                "'hide-aggregation-predicates' removed/added in HASURA_GRAPHQL_EXPERIMENTAL_FEATURES env var"
                (\conf -> conf {ssucExperimentalFeatures = currExperimentalFeatures})
          -- if 'hide_stream_fields' option added/removed from experimental features, close all connections
          | hasHideStreamFieldsChanged ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as the 'hide-stream-fields' option has been added/removed from HASURA_GRAPHQL_EXPERIMENTAL_FEATURES"
                "'hide-stream-fields' removed/added in HASURA_GRAPHQL_EXPERIMENTAL_FEATURES env var"
                (\conf -> conf {ssucExperimentalFeatures = currExperimentalFeatures})
          -- if naming convention has been changed, close all connections
          | hasDefaultNamingCaseChanged ->
              closeAllConnectionsWithReason
                ws
                "closing all websocket connections as the 'naming_convention' option has been added/removed from HASURA_GRAPHQL_EXPERIMENTAL_FEATURES and the HASURA_GRAPHQL_DEFAULT_NAMING_CONVENTION has changed"
                "naming convention has been changed"
                (\conf -> conf {ssucExperimentalFeatures = currExperimentalFeatures, ssucDefaultNamingCase = currDefaultNamingCase})
          | otherwise -> pure ()

createServerApp ::
  (MonadIO m, MC.MonadBaseControl IO m, LA.Forall (LA.Pure m), MonadWSLog m, MonadGetPolicies m) =>
  IO MetricsConfig ->
  WSConnectionInitTimeout ->
  WSServer a ->
  PrometheusMetrics ->
  -- | user provided handlers
  WSHandlers m a ->
  -- | aka WS.ServerApp
  HasuraServerApp m
{-# INLINE createServerApp #-}
createServerApp getMetricsConfig wsConnInitTimeout (WSServer logger@(L.Logger writeLog) _ serverStatus) prometheusMetrics wsHandlers !ipAddress !pendingConn = do
  wsId <- WSId <$> liftIO UUID.nextRandom
  logWSLog logger $ WSLog wsId EConnectionRequest Nothing
  -- NOTE: this timer is specific to `graphql-ws`. the server has to close the connection
  -- if the client doesn't send a `connection_init` message within the timeout period
  wsConnInitTimer <- liftIO $ getNewWSTimer (unrefine $ unWSConnectionInitTimeout wsConnInitTimeout)
  status <- liftIO $ STM.readTVarIO serverStatus
  case status of
    AcceptingConns _ -> logUnexpectedExceptions $ do
      onConnRes <- connHandler wsId reqHead ipAddress subProtocol
      either (onReject wsId) (onAccept wsConnInitTimer wsId) onConnRes
    ShuttingDown ->
      onReject wsId shuttingDownReject
  where
    reqHead = WS.pendingRequest pendingConn

    getSubProtocolHeader rhdrs =
      filter (\(x, _) -> x == (CI.mk . B.pack $ "Sec-WebSocket-Protocol")) $ WS.requestHeaders rhdrs

    subProtocol = case getSubProtocolHeader reqHead of
      [sph] -> toWSSubProtocol . B.unpack . snd $ sph
      _ -> Apollo -- NOTE: we default to the apollo implemenation
    connHandler = _hOnConn wsHandlers
    messageHandler = _hOnMessage wsHandlers
    closeHandler = _hOnClose wsHandlers

    logUnexpectedExceptions = flip catches handlers
      where
        handlers =
          [ -- this exception occurs under the normal course of the web server running. Also fairly common during shutdowns.
            -- Common suggestion is to gobble it.
            -- Refer: https://hackage.haskell.org/package/warp-3.3.24/docs/src/Network.Wai.Handler.Warp.Settings.html#defaultShouldDisplayException
            Handler $ \(_ :: TM.TimeoutThread) -> pure (),
            Handler $ \(e :: Warp.InvalidRequest) -> do
              writeLog
                $ L.UnstructuredLog L.LevelError
                $ fromString
                $ "Client exception: "
                <> show e
              throwIO e,
            Handler $ \(e :: SomeException) -> do
              writeLog
                $ L.UnstructuredLog L.LevelError
                $ fromString
                $ "Unexpected exception raised in websocket. Please report this as a bug: "
                <> show e
              throwIO e
          ]

    shuttingDownReject =
      WS.RejectRequest
        503
        "Service Unavailable"
        [("Retry-After", "0")]
        "Server is shutting down"

    onReject wsId rejectRequest = do
      liftIO $ WS.rejectRequestWith pendingConn rejectRequest
      logWSLog logger $ WSLog wsId ERejected Nothing

    onAccept wsConnInitTimer wsId (AcceptWith a acceptWithParams keepAlive onJwtExpiry) = do
      conn <- liftIO $ WS.acceptRequestWith pendingConn acceptWithParams
      logWSLog logger $ WSLog wsId EAccepted Nothing
      sendQ <- liftIO STM.newTQueueIO
      let !wsConn = WSConn wsId logger conn sendQ a
      -- TODO there are many thunks here. Difficult to trace how much is retained, and
      --      how much of that would be shared anyway.
      --      Requires a fork of 'wai-websockets' and 'websockets', it looks like.
      --      Adding `package` stanzas with -Xstrict -XStrictData for those two packages
      --      helped, cutting the number of thunks approximately in half.
      liftIO $ $assertNFHere wsConn -- so we don't write thunks to mutable vars
      let whenAcceptingInsertConn = liftIO
            $ STM.atomically
            $ do
              status <- STM.readTVar serverStatus
              case status of
                ShuttingDown -> pure ()
                AcceptingConns connMap -> STMMap.insert wsConn wsId connMap
              return status

      -- ensure we clean up connMap even if an unexpected exception is raised from our worker
      -- threads, or an async exception is raised somewhere in the body here:
      bracket
        whenAcceptingInsertConn
        (onConnClose wsConn)
        $ \case
          ShuttingDown -> do
            -- Bad luck, we were in the process of shutting the server down but a new
            -- connection was accepted. Let's just close it politely
            forceConnReconnect wsConn "shutting server down"
            closeHandler wsConn
          AcceptingConns _ -> do
            let rcv = forever $ do
                  shouldCaptureVariables <- liftIO $ _mcAnalyzeQueryVariables <$> getMetricsConfig
                  -- Process all messages serially (important!), in a separate thread:
                  msg <-
                    liftIO
                      $
                      -- Re-throw "receiveloop: resource vanished (Connection reset by peer)" :
                      --   https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Recv.hs#L112
                      -- as WS exception signaling cleanup below. It's not clear why exactly this gets
                      -- raised occasionally; I suspect an equivalent handler is missing from WS itself.
                      -- Regardless this should be safe:
                      handleJust (guard . E.isResourceVanishedError) (\() -> throw WS.ConnectionClosed)
                      $ WS.receiveData conn
                  let messageLength = BL.length msg
                      censoredMessage =
                        MessageDetails
                          (SB.fromLBS (if shouldCaptureVariables then msg else "<censored>"))
                          messageLength
                  liftIO
                    $ Prometheus.Counter.add
                      (pmWebSocketBytesReceived prometheusMetrics)
                      messageLength
                  logWSLog logger $ WSLog wsId (EMessageReceived censoredMessage) Nothing
                  messageHandler wsConn msg subProtocol

            let send = forever $ do
                  WSQueueResponse msg wsInfo wsTimer <- liftIO $ STM.atomically $ STM.readTQueue sendQ
                  messageQueueTime <- liftIO $ realToFrac <$> wsTimer
                  (messageWriteTime, _) <- liftIO $ withElapsedTime $ WS.sendTextData conn msg
                  let messageLength = BL.length msg
                      messageDetails = MessageDetails (SB.fromLBS msg) messageLength
                      parameterizedQueryHash = wsInfo >>= _wseiParameterizedQueryHash
                      operationName = wsInfo >>= _wseiOperationName
                      promMetricGranularLabel = DynamicSubscriptionLabel parameterizedQueryHash operationName
                      promMetricLabel = DynamicSubscriptionLabel Nothing Nothing
                      websocketBytesSentMetric = pmWebSocketBytesSent prometheusMetrics
                  granularPrometheusMetricsState <- runGetPrometheusMetricsGranularity
                  liftIO $ do
                    recordMetricWithLabel
                      granularPrometheusMetricsState
                      True
                      (CounterVector.add websocketBytesSentMetric promMetricGranularLabel messageLength)
                      (CounterVector.add websocketBytesSentMetric promMetricLabel messageLength)
                    Prometheus.Histogram.observe
                      (pmWebsocketMsgQueueTimeSeconds prometheusMetrics)
                      messageQueueTime
                    Prometheus.Histogram.observe
                      (pmWebsocketMsgWriteTimeSeconds prometheusMetrics)
                      (realToFrac messageWriteTime)
                  logWSLog logger $ WSLog wsId (EMessageSent messageDetails) wsInfo

            -- withAsync lets us be very sure that if e.g. an async exception is raised while we're
            -- forking that the threads we launched will be cleaned up. See also below.
            LA.withAsync rcv $ \rcvRef -> do
              LA.withAsync send $ \sendRef -> do
                LA.withAsync (liftIO $ keepAlive wsConn) $ \keepAliveRef -> do
                  LA.withAsync (liftIO $ onJwtExpiry wsConn) $ \onJwtExpiryRef -> do
                    -- once connection is accepted, check the status of the timer, and if it's expired, close the connection for `graphql-ws`
                    timeoutStatus <- liftIO $ getWSTimerState wsConnInitTimer
                    when (timeoutStatus == Done && subProtocol == GraphQLWS)
                      $ liftIO
                      $ closeConnWithCode wsConn 4408 "Connection initialisation timed out"

                    -- terminates on WS.ConnectionException and JWT expiry
                    let waitOnRefs = [keepAliveRef, onJwtExpiryRef, rcvRef, sendRef]
                    -- withAnyCancel re-raises exceptions from forkedThreads, and is guarenteed to cancel in
                    -- case of async exceptions raised while blocking here:
                    try (LA.waitAnyCancel waitOnRefs) >>= \case
                      -- NOTE: 'websockets' is a bit of a rat's nest at the moment wrt
                      -- exceptions; for now handle all ConnectionException by closing
                      -- and cleaning up, see: https://github.com/jaspervdj/websockets/issues/48
                      Left (_ :: WS.ConnectionException) -> do
                        logWSLog logger $ WSLog (_wcConnId wsConn) ECloseReceived Nothing
                      -- this will happen when jwt is expired
                      Right _ -> do
                        logWSLog logger $ WSLog (_wcConnId wsConn) EJwtExpired Nothing

    onConnClose wsConn = \case
      ShuttingDown -> pure ()
      AcceptingConns connMap -> do
        liftIO $ STM.atomically $ STMMap.delete (_wcConnId wsConn) connMap
        closeHandler wsConn
        logWSLog logger $ WSLog (_wcConnId wsConn) EClosed Nothing

shutdown :: WSServer a -> IO ()
shutdown (WSServer (L.Logger writeLog) _ serverStatus) = do
  writeLog $ L.debugT "Shutting websockets server down"
  conns <- STM.atomically $ do
    conns <- flushConnMap serverStatus
    STM.writeTVar serverStatus ShuttingDown
    pure conns

  closeAllWith (flip forceConnReconnect) "shutting server down" conns
