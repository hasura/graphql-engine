module Hasura.GraphQL.Transport.WebSocket.Client
  ( runGqlClient
  , WebsocketPayload (..)
  , sendStopMsg
  , updateState
  , stopRemote
  , closeRemote
  )
  where

import           Control.Concurrent                            (forkIO,
                                                                killThread)
import           Control.Exception                             (SomeException,
                                                                try)

import           Hasura.GraphQL.Transport.WebSocket.Connection
import           Hasura.GraphQL.Transport.WebSocket.Protocol   (OperationId (..))
import           Hasura.Prelude
import           Hasura.RQL.Types

--import qualified Control.Concurrent.Async                      as A
import qualified Data.Aeson                                    as J
import qualified Data.Aeson.Casing                             as J
import qualified Data.Aeson.TH                                 as J
import qualified Data.ByteString.Lazy                          as BL
import qualified Data.HashMap.Strict                           as Map
import qualified Data.IORef                                    as IORef
import qualified Data.Text                                     as T
import qualified Network.URI                                   as URI
import qualified Network.WebSockets                            as WS

import qualified Hasura.GraphQL.Transport.WebSocket.Server     as WS
import qualified Hasura.Logging                                as L


-- | TODO:
-- | The following ADT is required so that we can parse the incoming websocket
-- | frame, and only pick the payload, for remote schema queries.
-- | Ideally we should use `tartMsg` from Websocket.Protocol, but as
-- | `GraphQLRequest` doesn't have a ToJSON instance we are using our own type to
-- | get only the payload
data WebsocketPayload
  = WebsocketPayload
  { _wpId      :: !Text
  , _wpType    :: !Text
  , _wpPayload :: !J.Value
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''WebsocketPayload)


sendInit :: WS.Connection -> WebsocketPayload -> IO ()
sendInit conn payload =
  WS.sendTextData conn $ J.encode $
    J.object [ "type" J..= ("connection_init" :: Text)
             , "payload" J..= _wpPayload payload
             ]

sendStopMsg :: WS.Connection -> OperationId -> IO ()
sendStopMsg conn opId =
  WS.sendTextData conn $ J.encode $
    J.object [ "type" J..= ("stop" :: Text)
             , "id" J..= unOperationId opId
             ]


mkGraphqlProxy
  :: WS.WSConn a
  -> IORef.IORef WSConnState
  -> RemoteSchemaName
  -> WebsocketPayload -- the start msg
  -> WS.ClientApp ()
mkGraphqlProxy wsconn stRef rn payload destConn = do
  -- setup initial connection protocol
  setupInitialGraphqlProto destConn
  res <- getWsProxyState stRef rn
  -- if corresponding state is not found, are we silently ignoring it?
  -- the state won't have the remote's websocket conn. which is actually problematic
  onJust res $ \curState -> do
    let newState = curState{ _wpsRemoteConn = Just destConn }
    updateState stRef rn newState

  -- setting up the proxy from remote to hasura
  proxy destConn srcConn
  where
    srcConn = WS._wcConnRaw wsconn
    proxy recvConn sendConn =
      forever $ do
        msg <- WS.receiveData recvConn
        sendMsg sendConn msg

    sendMsg :: WS.Connection -> BL.ByteString -> IO ()
    sendMsg = WS.sendTextData

    -- send an init message with the same payload recieved, and then send the
    -- payload as it is (assuming this is the start msg)
    setupInitialGraphqlProto conn = do
      sendInit conn payload
      WS.sendTextData conn $ J.encode payload


runGqlClient'
  :: L.Logger
  -> URI.URI
  -> WS.WSConn a
  -> IORef.IORef WSConnState
  -> RemoteSchemaName
  -> OperationId
  -> WebsocketPayload
  -> ExceptT QErr IO ()
runGqlClient' (L.Logger logger) url wsConn stRef rn opId payload = do
  host <- maybe (throw500 "empty hostname for websocket conn") return mHost
  let gqClient = mkGraphqlProxy wsConn stRef rn payload

  thrId <- liftIO $ forkIO $ do
    res <- try $ WS.runClient host port path gqClient
    onLeft res $ \e -> do
      let err = T.pack $ show (e :: SomeException)
          opDet = ODQueryErr $ err500 Unexpected err
      logger $ WSLog wsId Nothing (EOperation opId Nothing opDet)
        (Just "exception from runClient thread")

  let newState = WebsocketProxyState thrId Nothing [(wsId, opId)]
  liftIO $ updateState stRef rn newState

  where
    mHost = (URI.uriUserInfo <$> uriAuth) <> (URI.uriRegName <$> uriAuth)
    port = read $ maybe "80" (drop 1 . URI.uriPort) uriAuth
    path = URI.uriPath url
    uriAuth = URI.uriAuthority url
    wsId = WS._wcConnId wsConn


runGqlClient
  :: L.Logger
  -> URI.URI
  -> WS.WSConn a
  -> IORef.IORef WSConnState
  -> RemoteSchemaName
  -> OperationId
  -> WebsocketPayload
  -> ExceptT QErr IO ()
runGqlClient logger url wsConn stRef rn opId payload = do
  mState <- getWsProxyState stRef rn
  case mState of
    Nothing ->
      runGqlClient' logger url wsConn stRef rn opId payload
    Just st -> do
      -- send init message and the raw message on the existing conn
      let wsconn   = _wpsRemoteConn st
          opids    = _wpsOperations st
          wsId     = WS._wcConnId wsConn
          newState = st { _wpsOperations = opids ++ [(wsId, opId)] }
      conn <- maybe (throwError wsConnErr) return wsconn
      liftIO $ do
        updateState stRef rn newState
        sendInit conn payload
        WS.sendTextData conn $ J.encode payload

updateState
  :: IORef.IORef WSConnState
  -> RemoteSchemaName
  -> WebsocketProxyState
  -> IO ()
updateState stRef rn wsProxyState = do
  -- this updates the IORef only on start msg, should we be doing this with a
  -- lock?
  st <- IORef.readIORef stRef
  let rmConnState = getStateData st
  onJust rmConnState $ \connState -> do
    let rmConnState' = Map.insert rn wsProxyState $ _cisRemoteConn connState
    let newSt = connState {_cisRemoteConn = rmConnState'}
    IORef.writeIORef stRef (CSInitialised newSt)

getWsProxyState
  :: (MonadIO m)
  => IORef.IORef WSConnState
  -> RemoteSchemaName
  -> m (Maybe WebsocketProxyState)
getWsProxyState ref rn = do
  st <- liftIO $ IORef.readIORef ref
  let mCurState = getStateData st
  case mCurState of
    Nothing                            -> return Nothing
    Just (ConnInitState _ _ rmConnMap) -> return $ Map.lookup rn rmConnMap


-- given a websocket id (WSId), close connections to that remote from
-- WSConnState
-- and updates state :: modifies IORef
closeRemote :: L.Logger -> IORef.IORef WSConnState -> WS.WSId -> IO ()
closeRemote (L.Logger logger) stRef wsId = do
  logger $ L.debugT $ "closing connections belonging to: " <>
    T.pack (show wsId)
  connState <- liftIO $ IORef.readIORef stRef
  let stData = getStateData connState
  onJust stData $ \ciSt@(ConnInitState _ _ connMap) ->
    onJust (findWebsocketId connMap wsId) $ \(rn, wst) -> do
      let (WebsocketProxyState thrId _ _) = wst
      --A.cancel rmOp
      liftIO $ killThread thrId
      let newConnMap = Map.delete rn connMap
          newState = CSInitialised $ ciSt {_cisRemoteConn = newConnMap }
      IORef.writeIORef stRef newState


stopRemote
  :: L.Logger -> WebsocketProxyState -> OperationId
  -> ExceptT QErr IO WebsocketProxyState
stopRemote (L.Logger logger) wsState aOpId = do
  liftIO $ logger $ L.debugT "stopping the remote.."
  let (WebsocketProxyState thrId wsConn ops) = wsState
  remoteConn <- maybe (throwError wsConnErr) return wsConn
  liftIO $ sendStopMsg remoteConn aOpId
  -- remaing operations
  let remOps = filter (\(_, opId) -> opId /= aOpId) ops
  when (null remOps) $ do
    -- close the client connection to remote
    liftIO $ logger $ L.debugT "no remaining ops; closing remote"
    liftIO $ killThread thrId
  return $ wsState { _wpsOperations = remOps }

wsConnErr :: QErr
wsConnErr = err500 Unexpected "remote websocket conn not found in state"
