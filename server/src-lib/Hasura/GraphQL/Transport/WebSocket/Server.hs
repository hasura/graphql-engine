{-# LANGUAGE RankNTypes #-}

module Hasura.GraphQL.Transport.WebSocket.Server
  ( WSId(..)

  , WSConn
  , getData
  , getWSId
  , closeConn
  , sendMsg

  , AcceptWith(..)
  , OnConnH
  , OnCloseH
  , OnMessageH
  , WSHandlers(..)

  , WSServer
  , createWSServer
  , closeAll
  , createServerApp
  , shutdown
  ) where

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM   as STM
import qualified Data.Aeson               as J
import qualified Data.Aeson.Casing        as J
import qualified Data.Aeson.TH            as J
import qualified Data.ByteString.Lazy     as BL
import qualified Data.TByteString         as TBS
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID
import qualified ListT
import qualified Network.WebSockets       as WS
import qualified StmContainers.Map        as STMMap
import           Data.Word                (Word16)

import           Control.Exception        (try)
import qualified Hasura.Logging           as L
import           Hasura.Prelude

newtype WSId
  = WSId { unWSId :: UUID.UUID }
  deriving (Show, Eq, Hashable)

instance J.ToJSON WSId where
  toJSON (WSId uuid) =
    J.toJSON $ UUID.toText uuid

data WSEvent
  = EConnectionRequest
  | EAccepted
  | ERejected
  | EMessageReceived !TBS.TByteString
  | EMessageSent !TBS.TByteString
  | EJwtExpired
  | ECloseReceived
  | ECloseSent !TBS.TByteString
  | EClosed
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 1
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''WSEvent)

data WSLog
  = WSLog
  { _wslWebsocketId :: !WSId
  , _wslEvent       :: !WSEvent
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''WSLog)

instance L.ToEngineLog WSLog where
  toEngineLog wsLog =
    (L.LevelDebug, L.ELTWsServer, J.toJSON wsLog)

data WSConn a
  = WSConn
  { _wcConnId    :: !WSId
  , _wcLogger    :: !L.Logger
  , _wcConnRaw   :: !WS.Connection
  , _wcSendQ     :: !(STM.TQueue BL.ByteString)
  , _wcExtraData :: !a
  }

getData :: WSConn a -> a
getData = _wcExtraData

getWSId :: WSConn a -> WSId
getWSId = _wcConnId

closeConn :: WSConn a -> BL.ByteString -> IO ()
closeConn wsConn bs = closeConnWithCode wsConn 1000 bs -- 1000 is "normal close"

-- | Closes a connection with code 1012, which means "Server is restarting"
-- good clients will implement a retry logic with a backoff of a few seconds
forceConnReconnect :: WSConn a -> BL.ByteString -> IO ()
forceConnReconnect wsConn bs = closeConnWithCode wsConn 1012 bs

closeConnWithCode :: WSConn a -> Word16 -> BL.ByteString -> IO ()
closeConnWithCode wsConn code bs = do
  (L.unLogger . _wcLogger) wsConn $ WSLog (_wcConnId wsConn) $ ECloseSent $ TBS.fromLBS bs
  WS.sendCloseCode (_wcConnRaw wsConn) code bs

-- writes to a queue instead of the raw connection
-- so that sendMsg doesn't block
sendMsg :: WSConn a -> BL.ByteString -> IO ()
sendMsg wsConn msg =
  STM.atomically $ STM.writeTQueue (_wcSendQ wsConn) msg

type ConnMap a = STMMap.Map WSId (WSConn a)

data ServerStatus a
  = AcceptingConns !(ConnMap a)
  | ShuttingDown

data WSServer a
  = WSServer
  { _wssLogger   :: L.Logger
  , _wssStatus :: !(STM.TVar (ServerStatus a))
  }

createWSServer :: L.Logger -> STM.STM (WSServer a)
createWSServer logger = do
  connMap <- STMMap.new
  serverStatus <- STM.newTVar (AcceptingConns connMap)
  return $ WSServer logger serverStatus

closeAll :: WSServer a -> BL.ByteString -> IO ()
closeAll (WSServer (L.Logger writeLog) serverStatus) msg = do
  writeLog $ L.debugT "closing all connections"
  conns <- STM.atomically $ flushConnMap serverStatus
  closeAllWith (flip closeConn) msg conns

closeAllWith
  :: (BL.ByteString -> WSConn a -> IO ())
  -> BL.ByteString
  -> [(WSId, WSConn a)]
  -> IO ()
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

data AcceptWith a
  = AcceptWith
  { _awData        :: !a
  , _awReq         :: !WS.AcceptRequest
  , _awKeepAlive   :: !(Maybe (WSConn a -> IO ()))
  , _awOnJwtExpiry :: !(Maybe (WSConn a -> IO ()))
  }

type OnConnH a    = WSId -> WS.RequestHead ->
                    IO (Either WS.RejectRequest (AcceptWith a))
type OnCloseH a   = WSConn a -> IO ()
type OnMessageH a = WSConn a -> BL.ByteString -> IO ()

data WSHandlers a
  = WSHandlers
  { _hOnConn    :: OnConnH a
  , _hOnMessage :: OnMessageH a
  , _hOnClose   :: OnCloseH a
  }

createServerApp
  :: WSServer a
  -- user provided handlers
  -> WSHandlers a
  -- aka WS.ServerApp
  -> WS.PendingConnection
  -> IO ()
createServerApp (WSServer logger@(L.Logger writeLog) serverStatus) wsHandlers pendingConn = do
  wsId <- WSId <$> UUID.nextRandom
  writeLog $ WSLog wsId EConnectionRequest
  status <- STM.readTVarIO serverStatus
  case status of
    AcceptingConns _ -> do
      let reqHead = WS.pendingRequest pendingConn
      onConnRes <- _hOnConn wsHandlers wsId reqHead
      either (onReject wsId) (onAccept wsId) onConnRes

    ShuttingDown ->
      onReject wsId shuttingDownReject

  where
    shuttingDownReject =
      WS.RejectRequest 503
                        "Service Unavailable"
                        [("Retry-After", "0")]
                        "Server is shutting down"

    onReject wsId rejectRequest = do
      WS.rejectRequestWith pendingConn rejectRequest
      writeLog $ WSLog wsId ERejected

    onAccept wsId (AcceptWith a acceptWithParams keepAliveM onJwtExpiryM) = do
      conn  <- WS.acceptRequestWith pendingConn acceptWithParams
      writeLog $ WSLog wsId EAccepted
      sendQ <- STM.newTQueueIO
      let wsConn = WSConn wsId logger conn sendQ a

      status <- STM.atomically $ do
        status <- STM.readTVar serverStatus
        case status of
          ShuttingDown -> pure ()
          AcceptingConns connMap -> STMMap.insert wsConn wsId connMap
        return status

      case status of
        ShuttingDown -> do
          -- Bad luck, we were in the process of shutting the server down but a new
          -- connection was accepted. Let's just close it politely
          forceConnReconnect wsConn "shutting server down"
          _hOnClose wsHandlers wsConn

        AcceptingConns connMap -> do
          rcvRef  <- A.async $ forever $ do
            msg <- WS.receiveData conn
            writeLog $ WSLog wsId $ EMessageReceived $ TBS.fromLBS msg
            _hOnMessage wsHandlers wsConn msg

          sendRef <- A.async $ forever $ do
            msg <- STM.atomically $ STM.readTQueue sendQ
            WS.sendTextData conn msg
            writeLog $ WSLog wsId $ EMessageSent $ TBS.fromLBS msg

          keepAliveRefM <- forM keepAliveM $ \action -> A.async $ action wsConn
          onJwtExpiryRefM <- forM onJwtExpiryM $ \action -> A.async $ action wsConn

          -- terminates on WS.ConnectionException and JWT expiry
          let waitOnRefs = catMaybes [keepAliveRefM, onJwtExpiryRefM]
                           <> [rcvRef, sendRef]
          res <- try $ A.waitAnyCancel waitOnRefs

          case res of
            Left ( _ :: WS.ConnectionException) -> do
              writeLog $ WSLog (_wcConnId wsConn) ECloseReceived
              onConnClose connMap wsConn
            -- this will happen when jwt is expired
            Right _ -> do
              writeLog $ WSLog (_wcConnId wsConn) EJwtExpired
              onConnClose connMap wsConn

    onConnClose connMap wsConn = do
      STM.atomically $ STMMap.delete (_wcConnId wsConn) connMap
      _hOnClose wsHandlers wsConn
      writeLog $ WSLog (_wcConnId wsConn) EClosed


shutdown :: WSServer a -> IO ()
shutdown (WSServer (L.Logger writeLog) serverStatus) = do
  writeLog $ L.debugT "Shutting websockets server down"
  conns <- STM.atomically $ do
    conns <- flushConnMap serverStatus
    STM.writeTVar serverStatus ShuttingDown
    return conns
  closeAllWith (flip forceConnReconnect) "shutting server down" conns
