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
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as J
import Data.ByteString.Char8 qualified as B (pack)
import Data.Text (pack)
import Hasura.App.State
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.CredentialCache
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Logging
import Hasura.GraphQL.Transport.HTTP (MonadExecuteQuery)
import Hasura.GraphQL.Transport.HTTP.Protocol (encodeGQExecError)
import Hasura.GraphQL.Transport.Instances ()
import Hasura.GraphQL.Transport.WebSocket
import Hasura.GraphQL.Transport.WebSocket.Protocol
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.GraphQL.Transport.WebSocket.Types
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.Types.SchemaCache
import Hasura.Server.AppStateRef
import Hasura.Server.Auth (UserAuthentication)
import Hasura.Server.Init.Config
  ( WSConnectionInitTimeout,
  )
import Hasura.Server.Limits
import Hasura.Server.Metrics (ServerMetrics (..))
import Hasura.Server.Prometheus
  ( PrometheusMetrics (..),
    decWebsocketConnections,
    incWebsocketConnections,
  )
import Hasura.Server.Types (MonadGetPolicies (..))
import Hasura.Services.Network
import Hasura.Tracing qualified as Tracing
import Network.WebSockets qualified as WS
import System.Metrics.Gauge qualified as EKG.Gauge

createWSServerApp ::
  ( MonadIO m,
    MonadFail m, -- only due to https://gitlab.haskell.org/ghc/ghc/-/issues/15681
    MC.MonadBaseControl IO m,
    LA.Forall (LA.Pure m),
    UserAuthentication m,
    E.MonadGQLExecutionCheck m,
    WS.MonadWSLog m,
    MonadQueryLog m,
    MonadExecutionLog m,
    MonadExecuteQuery m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    HasResourceLimits m,
    ProvidesNetwork m,
    Tracing.MonadTrace m,
    MonadGetPolicies m
  ) =>
  HashSet (L.EngineLogType L.Hasura) ->
  WSServerEnv impl ->
  WSConnectionInitTimeout ->
  Maybe (CredentialCache AgentLicenseKey) ->
  -- | aka generalized 'WS.ServerApp'
  WS.HasuraServerApp m
createWSServerApp enabledLogTypes serverEnv connInitTimeout licenseKeyCache = \ !ipAddress !pendingConn -> do
  let getMetricsConfig = scMetricsConfig <$> getSchemaCache (_wseAppStateRef serverEnv)
  WS.createServerApp getMetricsConfig connInitTimeout (_wseServer serverEnv) prometheusMetrics handlers ipAddress pendingConn
  where
    handlers =
      WS.WSHandlers
        onConnHandler
        onMessageHandler
        onCloseHandler

    logger = _wseLogger serverEnv
    serverMetrics = _wseServerMetrics serverEnv
    prometheusMetrics = _wsePrometheusMetrics serverEnv

    getAuthMode = acAuthMode <$> getAppContext (_wseAppStateRef serverEnv)
    wsActions = mkWSActions logger

    -- Mask async exceptions during event processing to help maintain integrity of mutable vars:
    -- here `sp` stands for sub-protocol
    onConnHandler rid rh ip sp = mask_ do
      liftIO $ EKG.Gauge.inc $ smWebsocketConnections serverMetrics
      liftIO $ incWebsocketConnections $ pmConnections prometheusMetrics
      flip runReaderT serverEnv $ onConn rid rh ip (wsActions sp)

    onMessageHandler conn bs sp =
      mask_
        $ onMessage enabledLogTypes getAuthMode serverEnv conn bs (wsActions sp) licenseKeyCache

    onCloseHandler conn = mask_ do
      granularPrometheusMetricsState <- runGetPrometheusMetricsGranularity
      liftIO $ EKG.Gauge.dec $ smWebsocketConnections serverMetrics
      liftIO $ decWebsocketConnections $ pmConnections prometheusMetrics
      onClose logger serverMetrics prometheusMetrics (_wseSubscriptionState serverEnv) conn granularPrometheusMetricsState

stopWSServerApp :: WSServerEnv impl -> IO ()
stopWSServerApp wsEnv = WS.shutdown (_wseServer wsEnv)

createWSServerEnv ::
  ( HasAppEnv m,
    MonadIO m
  ) =>
  AppStateRef impl ->
  m (WSServerEnv impl)
createWSServerEnv appStateRef = do
  AppEnv {..} <- askAppEnv
  let getCorsPolicy = acCorsPolicy <$> getAppContext appStateRef
      logger = _lsLogger appEnvLoggers

  AppContext {acEnableAllowlist, acAuthMode, acSQLGenCtx, acExperimentalFeatures, acDefaultNamingConvention} <- liftIO $ getAppContext appStateRef
  allowlist <- liftIO $ scAllowlist <$> getSchemaCache appStateRef
  corsPolicy <- liftIO getCorsPolicy

  wsServer <- liftIO $ STM.atomically $ WS.createWSServer acAuthMode acEnableAllowlist allowlist corsPolicy acSQLGenCtx acExperimentalFeatures acDefaultNamingConvention logger

  pure
    $ WSServerEnv
      (_lsLogger appEnvLoggers)
      appEnvSubscriptionState
      appStateRef
      appEnvManager
      getCorsPolicy
      appEnvEnableReadOnlyMode
      wsServer
      appEnvWebSocketKeepAlive
      appEnvServerMetrics
      appEnvPrometheusMetrics
      appEnvTraceSamplingPolicy

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
        GraphQLWS -> SMErr $ ErrorMsg opId $ encodeGQExecError execErr

    mkOnErrorMessageAction wsConn err mErrMsg =
      case subProtocol of
        Apollo ->
          case mErrMsg of
            WS.ConnInitFailed -> sendCloseWithMsg logger wsConn (WS.mkWSServerErrorCode subProtocol mErrMsg err) (Just $ SMConnErr err) Nothing
            WS.ClientMessageParseFailed -> sendMsg wsConn $ SMConnErr err
        GraphQLWS -> sendCloseWithMsg logger wsConn (WS.mkWSServerErrorCode subProtocol mErrMsg err) Nothing Nothing

    mkConnectionCloseAction wsConn opId errMsg =
      when (subProtocol == GraphQLWS)
        $ sendCloseWithMsg logger wsConn (GenericError4400 errMsg) (Just . SMErr $ ErrorMsg opId $ J.toEncoding (pack errMsg)) (Just 1000)

    getServerMsgType = case subProtocol of
      Apollo -> SMData
      GraphQLWS -> SMNext

    keepAliveAction wsConn = sendMsg wsConn
      $ case subProtocol of
        Apollo -> SMConnKeepAlive
        GraphQLWS -> SMPing . Just $ keepAliveMessage

    mkAcceptRequest =
      WS.defaultAcceptRequest
        { WS.acceptSubprotocol = Just . B.pack . showSubProtocol $ subProtocol
        }

    fmtErrorMessage errMsgs = case subProtocol of
      Apollo -> J.pairs (J.pair "errors" $ J.list id errMsgs)
      GraphQLWS -> J.list id errMsgs
