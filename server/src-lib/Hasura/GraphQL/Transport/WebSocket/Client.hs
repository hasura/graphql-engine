module Hasura.GraphQL.Transport.WebSocket.Client
  ( runGqlClient
  , clearState
  , WebsocketPayload (..)
  , sendStopMsg
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

import qualified Control.Concurrent.Async                      as A
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

data WebSocketClientErr
  = WSCEEmptyHostErr
  | WSCEConnErr Text
  | WSCEInternal Text
  deriving (Eq)

instance Show WebSocketClientErr where
  show = \case
    WSCEEmptyHostErr -> "empty hostname"
    WSCEConnErr e    -> T.unpack $ "connection error: " <> e
    WSCEInternal e   -> T.unpack $ "internal error: " <> e

-- this function recieves the start msg (WebsocketPayload) when a subscription
-- query is sent
mkGraphqlProxy
  :: WS.WSConn a
  -> IORef.IORef WSConnState
  -> RemoteSchemaName
  -> OperationId
  -> WebsocketPayload
  -> WS.ClientApp ()
mkGraphqlProxy wsconn stRef rn opId payload destConn = do
  -- setup initial connection protocol
  setupInitialGraphqlProto destConn
  -- setting up the proxy from remote to hasura
  op <- A.async $ proxy destConn srcConn
  let wsId = WS._wcConnId wsconn
  let newState = WebsocketProxyState op Nothing destConn [(wsId, opId)]
  updateState stRef rn newState
  A.wait op
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


runGqlClient'
  :: L.Logger
  -> URI.URI
  -> WS.WSConn a
  -> IORef.IORef WSConnState
  -> RemoteSchemaName
  -> OperationId
  -> WebsocketPayload
  -> ExceptT WebSocketClientErr IO ()
runGqlClient' (L.Logger logger) url wsConn stRef rn opId payload =
  case host of
    Nothing -> throwError WSCEEmptyHostErr
    Just h  -> do
      let gqClient = mkGraphqlProxy wsConn stRef rn opId payload

      thrId <- liftIO $ forkIO $ do
        res <- try $ WS.runClient h port path gqClient
        onLeft res $ \e -> do
          let err = T.pack $ show (e :: SomeException)
              opDet = ODQueryErr $ err500 Unexpected err
          logger $ WSLog wsId Nothing (EOperation opId Nothing opDet) Nothing

      res <- getWsProxyState stRef rn
      onJust res $ \curState -> do
        let newState = curState{_wpsRunClientThread = Just thrId}
        liftIO $ updateState stRef rn newState
  where
    host = (URI.uriUserInfo <$> uriAuth) <> (URI.uriRegName <$> uriAuth)
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
  -> ExceptT WebSocketClientErr IO ()
runGqlClient logger url wsConn stRef rn opId payload = do
  mState <- getWsProxyState stRef rn
  case mState of
    Nothing ->
      runGqlClient' logger url wsConn stRef rn opId payload
    Just st -> do
      -- send init message and the raw message on the existing conn
      let conn     = _wpsRemoteConn st
          opids    = _wpsOperations st
          wsId     = WS._wcConnId wsConn
          newState = st { _wpsOperations = opids ++ [(wsId, opId)] }
      liftIO $ updateState stRef rn newState
      liftIO $ sendInit conn payload
      liftIO $ WS.sendTextData conn $ J.encode payload

updateState
  :: IORef.IORef WSConnState
  -> RemoteSchemaName
  -> WebsocketProxyState
  -> IO ()
updateState stRef rn wsProxyState = do
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

--stopOperation :: OperationId -> RemoteConnState
clearState :: L.Logger -> WebsocketProxyState -> IO ()
clearState (L.Logger logger) (WebsocketProxyState rmOp thrId _ _) = do
  logger $ L.debugT "cancelling async operations.."
  A.cancel rmOp
  onJust thrId killThread
