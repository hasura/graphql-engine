module Hasura.GraphQL.Transport.WSServerApp
  ( createWSServerApp
  , stopWSServerApp
  , createWSServerEnv
  ) where

import           Hasura.Prelude

import qualified Control.Concurrent.Async.Lifted.Safe        as LA
import qualified Control.Concurrent.STM                      as STM
import qualified Control.Monad.Trans.Control                 as MC
import qualified Data.ByteString.Char8                       as B (pack)
import qualified Data.Environment                            as Env
import qualified Network.HTTP.Client                         as H
import qualified Network.WebSockets                          as WS
import qualified System.Metrics.Gauge                        as EKG.Gauge

import           Control.Exception.Lifted
import           Data.Aeson                                  (toJSON)
import           Data.Text                                   (pack, unpack)

import qualified Hasura.GraphQL.Execute                      as E
import qualified Hasura.GraphQL.Execute.Backend              as EB
import qualified Hasura.GraphQL.Execute.LiveQuery.State      as LQ
import qualified Hasura.GraphQL.Transport.WebSocket.Server   as WS
import qualified Hasura.Logging                              as L
import qualified Hasura.Tracing                              as Tracing

import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP               (MonadExecuteQuery)
import           Hasura.GraphQL.Transport.Instances          ()
import           Hasura.GraphQL.Transport.WebSocket
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Types
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Auth                          (AuthMode, UserAuthentication)
import           Hasura.Server.Cors
import           Hasura.Server.Init.Config                   (KeepAliveDelay,
                                                              WSConnectionInitTimeout)
import           Hasura.Server.Metrics                       (ServerMetrics (..))
import           Hasura.Server.Version                       (HasVersion)

createWSServerApp ::
  ( HasVersion
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
  , EB.MonadQueryTags m
  )
  => Env.Environment
  -> HashSet (L.EngineLogType L.Hasura)
  -> AuthMode
  -> WSServerEnv
  -> WSConnectionInitTimeout
  -> WS.HasuraServerApp m
--   -- ^ aka generalized 'WS.ServerApp'
createWSServerApp env enabledLogTypes authMode serverEnv connInitTimeout = \ !ipAddress !pendingConn ->
  WS.createServerApp connInitTimeout (_wseServer serverEnv) handlers ipAddress pendingConn
  where
    handlers = WS.WSHandlers
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

    onMessageHandler conn bs sp = mask_ $
      onMessage env enabledLogTypes authMode serverEnv conn bs (wsActions sp)

    onCloseHandler conn = mask_ do
      liftIO $ EKG.Gauge.dec $ smWebsocketConnections serverMetrics
      onClose logger serverMetrics (_wseLiveQMap serverEnv) conn

stopWSServerApp :: WSServerEnv -> IO ()
stopWSServerApp wsEnv = WS.shutdown (_wseServer wsEnv)

createWSServerEnv :: (MonadIO m)
  => L.Logger L.Hasura
  -> LQ.LiveQueriesState
  -> IO (SchemaCache, SchemaCacheVer)
  -> H.Manager
  -> CorsPolicy
  -> SQLGenCtx
  -> Bool
  -> KeepAliveDelay
  -> ServerMetrics
  -> m WSServerEnv
createWSServerEnv logger lqState getSchemaCache httpManager
  corsPolicy sqlGenCtx enableAL keepAliveDelay serverMetrics = do
  wsServer <- liftIO $ STM.atomically $ WS.createWSServer logger
  pure $ WSServerEnv logger lqState getSchemaCache httpManager
    corsPolicy sqlGenCtx wsServer enableAL keepAliveDelay serverMetrics

mkWSActions :: L.Logger L.Hasura -> WSSubProtocol -> WS.WSActions WSConnData
mkWSActions logger subProtocol =
  WS.WSActions
    mkPostExecErrMessageAction
    mkOnErrorMessageAction
    mkConnectionCloseAction
    keepAliveAction
    getServerMsgType
    mkAcceptRequest
  where
    mkPostExecErrMessageAction wsConn opId execErr =
      sendMsg wsConn $ case subProtocol of
        Apollo    -> SMData $ DataMsg  opId $ throwError execErr
        GraphQLWS -> SMErr  $ ErrorMsg opId $ toJSON execErr

    mkOnErrorMessageAction wsConn err mErrMsg = case subProtocol of
      Apollo    -> sendMsg wsConn $ SMConnErr err
      GraphQLWS -> sendCloseWithMsg logger wsConn (GenericError4400 $ (fromMaybe "" mErrMsg) <> (unpack . unConnErrMsg $ err)) Nothing

    mkConnectionCloseAction wsConn opId errMsg =
      when (subProtocol == GraphQLWS) $
        sendCloseWithMsg logger wsConn (GenericError4400 errMsg) (Just . SMErr $ ErrorMsg opId $ toJSON (pack errMsg))

    getServerMsgType = case subProtocol of
      Apollo    -> SMData
      GraphQLWS -> SMNext

    keepAliveAction wsConn = sendMsg wsConn $
      case subProtocol of
        Apollo    -> SMConnKeepAlive
        GraphQLWS -> SMPing . Just $ keepAliveMessage

    mkAcceptRequest = WS.defaultAcceptRequest {
      WS.acceptSubprotocol = Just . B.pack . showSubProtocol $ subProtocol
    }
