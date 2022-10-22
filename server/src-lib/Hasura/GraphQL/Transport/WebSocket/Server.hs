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
    WSEvent (EMessageSent),
    WSEventInfo (WSEventInfo, _wseiEventType, _wseiOperationId, _wseiOperationName, _wseiParameterizedQueryHash, _wseiQueryExecutionTime, _wseiResponseSize),
    WSHandlers (WSHandlers),
    WSId,
    WSKeepAliveMessageAction,
    WSLog (WSLog),
    WSOnErrorMessageAction,
    WSQueueResponse (WSQueueResponse),
    WSServer,
    closeConn,
    createServerApp,
    createWSServer,
    getData,
    getRawWebSocketConnection,
    getWSId,
    onClientMessageParseErrorText,
    onConnInitErrorText,
    sendMsg,
    shutdown,

    -- * exported for testing
    mkUnsafeWSId,
  )
where

import Control.Concurrent.Async qualified as A
import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.STM qualified as STM
import Control.Exception.Lifted
import Control.Monad.Trans.Control qualified as MC
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Aeson.TH qualified as J
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.String
import Data.TByteString qualified as TBS
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Word (Word16)
import GHC.AssertNF.CPP
import GHC.Int (Int64)
import Hasura.GraphQL.ParameterizedQueryHash (ParameterizedQueryHash)
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.GraphQL.Transport.WebSocket.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.Server.Init.Config (WSConnectionInitTimeout (..))
import ListT qualified
import Network.Wai.Extended (IpAddress)
import Network.WebSockets qualified as WS
import StmContainers.Map qualified as STMMap
import System.IO.Error qualified as E

newtype WSId = WSId {unWSId :: UUID.UUID}
  deriving (Show, Eq, Hashable)

mkUnsafeWSId :: UUID.UUID -> WSId
mkUnsafeWSId = WSId

instance J.ToJSON WSId where
  toJSON (WSId uuid) =
    J.toJSON $ UUID.toText uuid

-- | Websocket message and other details
data MessageDetails = MessageDetails
  { _mdMessage :: !TBS.TByteString,
    _mdMessageSize :: !Int64
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON ''MessageDetails)

data WSEvent
  = EConnectionRequest
  | EAccepted
  | ERejected
  | EMessageReceived !MessageDetails
  | EMessageSent !MessageDetails
  | EJwtExpired
  | ECloseReceived
  | ECloseSent !TBS.TByteString
  | EClosed
  deriving (Show, Eq)

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
  deriving (Show, Eq)

$( J.deriveToJSON
     J.defaultOptions
       { J.fieldLabelModifier = J.snakeCase . drop 4,
         J.omitNothingFields = True
       }
     ''WSLog
 )

class Monad m => MonadWSLog m where
  -- | Takes WS server log data and logs it
  -- logWSServer
  logWSLog :: L.Logger L.Hasura -> WSLog -> m ()

instance MonadWSLog m => MonadWSLog (ExceptT e m) where
  logWSLog l ws = lift $ logWSLog l ws

instance MonadWSLog m => MonadWSLog (ReaderT r m) where
  logWSLog l ws = lift $ logWSLog l ws

instance L.ToEngineLog WSLog L.Hasura where
  toEngineLog wsLog =
    (L.LevelDebug, L.ELTInternal L.ILTWsServer, J.toJSON wsLog)

data WSQueueResponse = WSQueueResponse
  { _wsqrMessage :: !BL.ByteString,
    -- | extra metadata that we use for other actions, such as print log
    -- we don't want to inlcude them into websocket message payload
    _wsqrEventInfo :: !(Maybe WSEventInfo)
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
forceConnReconnect :: MonadIO m => WSConn a -> BL.ByteString -> m ()
forceConnReconnect wsConn bs = liftIO $ closeConnWithCode wsConn 1012 bs

closeConnWithCode :: WSConn a -> Word16 -> BL.ByteString -> IO ()
closeConnWithCode wsConn code bs = do
  (L.unLogger . _wcLogger) wsConn $
    WSLog (_wcConnId wsConn) (ECloseSent $ TBS.fromLBS bs) Nothing
  WS.sendCloseCode (_wcConnRaw wsConn) code bs

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
  { _wssLogger :: !(L.Logger L.Hasura),
    -- | See e.g. createServerApp.onAccept for how we use STM to preserve consistency
    _wssStatus :: !(STM.TVar (ServerStatus a))
  }

createWSServer :: L.Logger L.Hasura -> STM.STM (WSServer a)
createWSServer logger = do
  connMap <- STMMap.new
  serverStatus <- STM.newTVar (AcceptingConns connMap)
  return $ WSServer logger serverStatus

closeAllWith ::
  (BL.ByteString -> WSConn a -> IO ()) ->
  BL.ByteString ->
  [(WSId, WSConn a)] ->
  IO ()
closeAllWith closer msg conns =
  void $ A.mapConcurrently (closer msg . snd) conns

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

type WSOnErrorMessageAction a = WSConn a -> ConnErrMsg -> Maybe String -> IO ()

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
    _wsaErrorMsgFormat :: !([J.Value] -> J.Value)
  }

-- | to be used with `WSOnErrorMessageAction`
onClientMessageParseErrorText :: Maybe String
onClientMessageParseErrorText = Just "Parsing client message failed: "

-- | to be used with `WSOnErrorMessageAction`
onConnInitErrorText :: Maybe String
onConnInitErrorText = Just "Connection initialization failed: "

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

createServerApp ::
  (MonadIO m, MC.MonadBaseControl IO m, LA.Forall (LA.Pure m), MonadWSLog m) =>
  WSConnectionInitTimeout ->
  WSServer a ->
  -- | user provided handlers
  WSHandlers m a ->
  -- | aka WS.ServerApp
  HasuraServerApp m
{-# INLINE createServerApp #-}
createServerApp wsConnInitTimeout (WSServer logger@(L.Logger writeLog) serverStatus) wsHandlers !ipAddress !pendingConn = do
  wsId <- WSId <$> liftIO UUID.nextRandom
  logWSLog logger $ WSLog wsId EConnectionRequest Nothing
  -- NOTE: this timer is specific to `graphql-ws`. the server has to close the connection
  -- if the client doesn't send a `connection_init` message within the timeout period
  wsConnInitTimer <- liftIO $ getNewWSTimer (unWSConnectionInitTimeout wsConnInitTimeout)
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

    -- It's not clear what the unexpected exception handling story here should be. So at
    -- least log properly and re-raise:
    logUnexpectedExceptions = handle $ \(e :: SomeException) -> do
      writeLog $
        L.UnstructuredLog L.LevelError $
          fromString $
            "Unexpected exception raised in websocket. Please report this as a bug: " <> show e
      throwIO e

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
      let whenAcceptingInsertConn = liftIO $
            STM.atomically $ do
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
                  -- Process all messages serially (important!), in a separate thread:
                  msg <-
                    liftIO $
                      -- Re-throw "receiveloop: resource vanished (Connection reset by peer)" :
                      --   https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Recv.hs#L112
                      -- as WS exception signaling cleanup below. It's not clear why exactly this gets
                      -- raised occasionally; I suspect an equivalent handler is missing from WS itself.
                      -- Regardless this should be safe:
                      handleJust (guard . E.isResourceVanishedError) (\() -> throw WS.ConnectionClosed) $
                        WS.receiveData conn
                  let message = MessageDetails (TBS.fromLBS msg) (BL.length msg)
                  logWSLog logger $ WSLog wsId (EMessageReceived message) Nothing
                  messageHandler wsConn msg subProtocol

            let send = forever $ do
                  WSQueueResponse msg wsInfo <- liftIO $ STM.atomically $ STM.readTQueue sendQ
                  let message = MessageDetails (TBS.fromLBS msg) (BL.length msg)
                  liftIO $ WS.sendTextData conn msg
                  logWSLog logger $ WSLog wsId (EMessageSent message) wsInfo

            -- withAsync lets us be very sure that if e.g. an async exception is raised while we're
            -- forking that the threads we launched will be cleaned up. See also below.
            LA.withAsync rcv $ \rcvRef -> do
              LA.withAsync send $ \sendRef -> do
                LA.withAsync (liftIO $ keepAlive wsConn) $ \keepAliveRef -> do
                  LA.withAsync (liftIO $ onJwtExpiry wsConn) $ \onJwtExpiryRef -> do
                    -- once connection is accepted, check the status of the timer, and if it's expired, close the connection for `graphql-ws`
                    timeoutStatus <- liftIO $ getWSTimerState wsConnInitTimer
                    when (timeoutStatus == Done && subProtocol == GraphQLWS) $
                      liftIO $ closeConnWithCode wsConn 4408 "Connection initialisation timed out"

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
shutdown (WSServer (L.Logger writeLog) serverStatus) = do
  writeLog $ L.debugT "Shutting websockets server down"
  conns <- STM.atomically $ do
    conns <- flushConnMap serverStatus
    STM.writeTVar serverStatus ShuttingDown
    return conns
  closeAllWith (flip forceConnReconnect) "shutting server down" conns
