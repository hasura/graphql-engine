module Hasura.GraphQL.Transport.WSServerApp
  ( createWSServerApp,
    stopWSServerApp,
    createWSServerEnv,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.STM qualified as STM
import Control.Exception.Lifted
import Control.Monad.Trans.Control qualified as MC
import Data.Aeson (object, toJSON, (.=))
import Data.ByteString.Char8 qualified as B (pack)
import Data.Environment qualified as Env
import Data.Text (pack, unpack)
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.Subscription.State qualified as ES
import Hasura.GraphQL.Logging
import Hasura.GraphQL.Transport.HTTP (MonadExecuteQuery)
import Hasura.GraphQL.Transport.Instances ()
import Hasura.GraphQL.Transport.WebSocket
import Hasura.GraphQL.Transport.WebSocket.Protocol
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Types
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.Types
import Hasura.Server.Auth (AuthMode, UserAuthentication)
import Hasura.Server.Cors
import Hasura.Server.Init.Config
  ( KeepAliveDelay,
    WSConnectionInitTimeout,
  )
import Hasura.Server.Limits
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Types (ReadOnlyMode)
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client qualified as HTTP
import Network.WebSockets qualified as WS
import System.Metrics.Gauge qualified as EKG.Gauge

createWSServerApp ::
  ( MonadIO m,
    MC.MonadBaseControl IO m,
    LA.Forall (LA.Pure m),
    UserAuthentication (Tracing.TraceT m),
    E.MonadGQLExecutionCheck m,
    WS.MonadWSLog m,
    MonadQueryLog m,
    Tracing.HasReporter m,
    MonadExecuteQuery m,
    MonadMetadataStorage (MetadataStorageT m),
    EB.MonadQueryTags m,
    HasResourceLimits m
  ) =>
  Env.Environment ->
  HashSet (L.EngineLogType L.Hasura) ->
  AuthMode ->
  WSServerEnv ->
  WSConnectionInitTimeout ->
  WS.HasuraServerApp m
--   -- ^ aka generalized 'WS.ServerApp'
createWSServerApp env enabledLogTypes authMode serverEnv connInitTimeout = \ !ipAddress !pendingConn ->
  WS.createServerApp connInitTimeout (_wseServer serverEnv) handlers ipAddress pendingConn
  where
    handlers =
      WS.WSHandlers
        onConnHandler
        onMessageHandler
        onCloseHandler

    logger = _wseLogger serverEnv
    serverMetrics = _wseServerMetrics serverEnv

    wsActions = mkWSActions logger

    -- Mask async exceptions during event processing to help maintain integrity of mutable vars:
    -- here `sp` stands for sub-protocol
    onConnHandler rid rh ip sp = mask_ do
      liftIO $ EKG.Gauge.inc $ smWebsocketConnections serverMetrics
      flip runReaderT serverEnv $ onConn rid rh ip (wsActions sp)

    onMessageHandler conn bs sp =
      mask_ $
        onMessage env enabledLogTypes authMode serverEnv conn bs (wsActions sp)

    onCloseHandler conn = mask_ do
      liftIO $ EKG.Gauge.dec $ smWebsocketConnections serverMetrics
      onClose logger serverMetrics (_wseLiveQMap serverEnv) conn

stopWSServerApp :: WSServerEnv -> IO ()
stopWSServerApp wsEnv = WS.shutdown (_wseServer wsEnv)

createWSServerEnv ::
  (MonadIO m) =>
  L.Logger L.Hasura ->
  ES.SubscriptionsState ->
  IO (SchemaCache, SchemaCacheVer) ->
  HTTP.Manager ->
  CorsPolicy ->
  SQLGenCtx ->
  ReadOnlyMode ->
  Bool ->
  KeepAliveDelay ->
  ServerMetrics ->
  m WSServerEnv
createWSServerEnv
  logger
  lqState
  getSchemaCache
  httpManager
  corsPolicy
  sqlGenCtx
  readOnlyMode
  enableAL
  keepAliveDelay
  serverMetrics = do
    wsServer <- liftIO $ STM.atomically $ WS.createWSServer logger
    pure $
      WSServerEnv
        logger
        lqState
        getSchemaCache
        httpManager
        corsPolicy
        sqlGenCtx
        readOnlyMode
        wsServer
        enableAL
        keepAliveDelay
        serverMetrics

mkWSActions :: L.Logger L.Hasura -> WSSubProtocol -> WS.WSActions WSConnData
mkWSActions logger subProtocol =
  WS.WSActions
    mkPostExecErrMessageAction
    mkOnErrorMessageAction
    mkConnectionCloseAction
    keepAliveAction
    getServerMsgType
    mkAcceptRequest
    fmtErrorMessage
  where
    mkPostExecErrMessageAction wsConn opId execErr =
      sendMsg wsConn $ case subProtocol of
        Apollo -> SMData $ DataMsg opId $ throwError execErr
        GraphQLWS -> SMErr $ ErrorMsg opId $ toJSON execErr

    mkOnErrorMessageAction wsConn err mErrMsg = case subProtocol of
      Apollo -> sendMsg wsConn $ SMConnErr err
      GraphQLWS -> sendCloseWithMsg logger wsConn (GenericError4400 $ (fromMaybe "" mErrMsg) <> (unpack . unConnErrMsg $ err)) Nothing Nothing

    mkConnectionCloseAction wsConn opId errMsg =
      when (subProtocol == GraphQLWS) $
        sendCloseWithMsg logger wsConn (GenericError4400 errMsg) (Just . SMErr $ ErrorMsg opId $ toJSON (pack errMsg)) (Just 1000)

    getServerMsgType = case subProtocol of
      Apollo -> SMData
      GraphQLWS -> SMNext

    keepAliveAction wsConn = sendMsg wsConn $
      case subProtocol of
        Apollo -> SMConnKeepAlive
        GraphQLWS -> SMPing . Just $ keepAliveMessage

    mkAcceptRequest =
      WS.defaultAcceptRequest
        { WS.acceptSubprotocol = Just . B.pack . showSubProtocol $ subProtocol
        }

    fmtErrorMessage errMsgs = case subProtocol of
      Apollo -> object ["errors" .= errMsgs]
      GraphQLWS -> toJSON errMsgs
