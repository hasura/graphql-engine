module Hasura.GraphQL.Transport.WebSocket.Client
  ( runGqlClient
  , sendStopMsg
  , updateState
  , stopRemote
  , closeRemote
  ) where

import           Control.Concurrent                            (ThreadId,
                                                                forkIO,
                                                                killThread,
                                                                myThreadId)
import           Control.Exception                             (SomeException,
                                                                try)

import           Hasura.GraphQL.Transport.WebSocket.Connection
import           Hasura.GraphQL.Transport.WebSocket.Protocol   (OperationId (..))
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Control.Concurrent.STM                        as STM
import qualified Data.Aeson                                    as J
import qualified Data.ByteString.Lazy                          as BL
import qualified Data.HashMap.Strict                           as Map
import qualified Data.Text                                     as T
import qualified Network.URI                                   as URI
import qualified Network.WebSockets                            as WS

import qualified Hasura.GraphQL.Transport.WebSocket.Protocol   as WS
import qualified Hasura.GraphQL.Transport.WebSocket.Server     as WS
import qualified Hasura.Logging                                as L


sendInit :: WS.Connection -> Maybe WS.ConnParams -> IO ()
sendInit conn reqHdrs =
  WS.sendTextData conn $ J.encode (WS.CMConnInit reqHdrs)

sendStopMsg :: WS.Connection -> WS.StopMsg -> IO ()
sendStopMsg conn msg =
  WS.sendTextData conn $ J.encode $ WS.CMStop msg

mkGraphqlProxy
  :: WS.WSConn a
  -> STM.TVar WSConnState
  -> RemoteSchemaName
  -> Maybe WS.ConnParams -- connection params in conn_init from the client
  -> OperationId
  -> WS.StartMsg -- the start msg
  -> ThreadId    -- The receive client threadId
  -> WS.ClientApp ()
mkGraphqlProxy wsConn stRef rn hdrs opId payload threadId destConn = do
  -- setup initial connection protocol
  setupInitialGraphqlProto destConn
  let newState = WebsocketProxyState threadId destConn [(wsId, opId)]
  updateState stRef rn newState

  -- setting up the proxy from remote to hasura
  proxy destConn srcConn
  where
    wsId    = WS._wcConnId wsConn
    srcConn = WS._wcConnRaw wsConn

    proxy recvConn sendConn =
      --TODO: protocol level proxying
      forever $ do
        msg <- WS.receiveData recvConn
        sendMsg sendConn msg

    sendMsg :: WS.Connection -> BL.ByteString -> IO ()
    sendMsg = WS.sendTextData

    -- send an init message with the same payload recieved, and then send the
    -- payload as it is (assuming this is the start msg)
    setupInitialGraphqlProto conn = do
      sendInit conn hdrs
      WS.sendTextData conn $ J.encode $ WS.CMStart payload


runGqlClient'
  :: L.Logger
  -> URI.URI
  -> WS.WSConn a
  -> STM.TVar WSConnState
  -> RemoteSchemaName
  -> OperationId
  -> Maybe WS.ConnParams
  -> WS.StartMsg
  -> ExceptT QErr IO ()
runGqlClient' (L.Logger logger) url wsConn stRef rn opId hdrs payload = do
  host <- maybe (throw500 "empty hostname for websocket conn") return mHost
  void $ liftIO $ forkIO $ do
    tid <- myThreadId
    let gqClient = mkGraphqlProxy wsConn stRef rn hdrs opId payload tid
    res <- try $ WS.runClient host port path gqClient
    onLeft res $ \e -> do
      let err = T.pack $ show (e :: SomeException)
          opDet = ODQueryErr $ err500 Unexpected err
      logger $ WSLog wsId Nothing (EOperation opId Nothing opDet)
        (Just "exception from runClient thread") Nothing
  where
    uriAuth = URI.uriAuthority url
    path = URI.uriPath url
    mHost = (URI.uriUserInfo <$> uriAuth) <> (URI.uriRegName <$> uriAuth)
    port = let val = maybe "80" (drop 1 . URI.uriPort) uriAuth
           in read $ bool val "80" (null val)
    wsId = WS._wcConnId wsConn


runGqlClient
  :: L.Logger
  -> URI.URI
  -> WS.WSConn a
  -> STM.TVar WSConnState
  -> RemoteSchemaName
  -> OperationId
  -> Maybe WS.ConnParams
  -> WS.StartMsg
  -> ExceptT QErr IO ()
runGqlClient logger url wsConn stRef rn opId hdrs startMsg = do
  mState <- getWsProxyState stRef rn
  case mState of
    Nothing ->
      runGqlClient' logger url wsConn stRef rn opId hdrs startMsg
    Just st -> do
      -- send init message and the raw message on the existing conn
      let wsconn   = _wpsRemoteConn st
          opids    = _wpsOperations st
          wsId     = WS._wcConnId wsConn
          newState = st { _wpsOperations = opids ++ [(wsId, opId)] }
      --conn <- maybe (throwError wsConnErr) return wsconn
      liftIO $ do
        updateState stRef rn newState
        -- sendInit conn hdrs
        -- send only start message on existing connection
        WS.sendTextData wsconn $ J.encode startMsg

updateState
  :: STM.TVar WSConnState
  -> RemoteSchemaName
  -> WebsocketProxyState
  -> IO ()
updateState stRef rn wsProxyState = do
  -- this updates the IORef only on start msg, should we be doing this with a
  -- lock?
  st <- STM.readTVarIO stRef
  let rmConnState = getStateData st
  onJust rmConnState $ \connState -> do
    let rmConnState' = Map.insert rn wsProxyState $ _cisRemoteConn connState
    let newSt = connState {_cisRemoteConn = rmConnState'}
    STM.atomically $ STM.writeTVar stRef (CSInitialised newSt)

getWsProxyState
  :: (MonadIO m)
  => STM.TVar WSConnState
  -> RemoteSchemaName
  -> m (Maybe WebsocketProxyState)
getWsProxyState ref rn = do
  st <- liftIO $ STM.readTVarIO ref
  return $ getStateData st >>= (Map.lookup rn . _cisRemoteConn)

-- given a websocket id (WSId), close connections to that remote from
-- WSConnState
-- and updates state :: modifies IORef
closeRemote :: L.Logger -> STM.TVar WSConnState -> WS.WSId -> IO ()
closeRemote (L.Logger logger) stRef wsId = do
  logger $ L.debugT $ "closing connections belonging to: " <>
    T.pack (show wsId)
  connState <- liftIO $ STM.readTVarIO stRef
  let stData = getStateData connState
  onJust stData $ \ciSt@(ConnInitState _ _ connMap _) ->
    onJust (findWebsocketId connMap wsId) $ \(rn, wst) -> do
      let (WebsocketProxyState thrId _ _) = wst
      --A.cancel rmOp
      liftIO $ killThread thrId
      let newConnMap = Map.delete rn connMap
          newState = CSInitialised $ ciSt {_cisRemoteConn = newConnMap }
      STM.atomically $ STM.writeTVar stRef newState


stopRemote
  :: L.Logger -> WebsocketProxyState -> OperationId
  -> ExceptT QErr IO WebsocketProxyState
stopRemote (L.Logger logger) wsState opId = do
  liftIO $ logger $ L.debugT "stopping the remote.."
  let (WebsocketProxyState thrId wsConn ops) = wsState
  --remoteConn <- maybe (throwError wsConnErr) return wsConn
  liftIO $ sendStopMsg wsConn $ WS.StopMsg opId
  -- remaing operations
  let remOps = filter (\(_, oId) -> oId /= opId) ops
  when (null remOps) $ do
    -- close the client connection to remote
    liftIO $ logger $ L.debugT "no remaining ops; closing remote"
    liftIO $ killThread thrId
  return $ wsState { _wpsOperations = remOps }
