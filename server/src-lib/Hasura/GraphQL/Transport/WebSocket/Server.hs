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
    (L.LevelDebug, "ws-server", J.toJSON wsLog)

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
closeConn wsConn bs = do
  (L.unLogger . _wcLogger) wsConn $ WSLog (_wcConnId wsConn) $ ECloseSent $ TBS.fromLBS bs
  WS.sendClose (_wcConnRaw wsConn) bs

-- writes to a queue instead of the raw connection
-- so that sendMsg doesn't block
sendMsg :: WSConn a -> BL.ByteString -> IO ()
sendMsg wsConn msg =
  STM.atomically $ STM.writeTQueue (_wcSendQ wsConn) msg

type ConnMap a = STMMap.Map WSId (WSConn a)

data WSServer a
  = WSServer
  { _wssLogger  :: L.Logger
  , _wssConnMap :: ConnMap a
  }

createWSServer :: L.Logger -> STM.STM (WSServer a)
createWSServer logger = WSServer logger <$> STMMap.new

closeAll :: WSServer a -> BL.ByteString -> IO ()
closeAll (WSServer (L.Logger writeLog) connMap) msg = do
  writeLog $ L.debugT "closing all connections"
  conns <- STM.atomically $ do
    conns <- ListT.toList $ STMMap.listT connMap
    STMMap.reset connMap
    return conns
  void $ A.mapConcurrently (flip closeConn msg . snd) conns

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
  -> WS.PendingConnection -> IO ()
createServerApp (WSServer logger@(L.Logger writeLog) connMap) wsHandlers pendingConn = do
  wsId <- WSId <$> UUID.nextRandom
  writeLog $ WSLog wsId EConnectionRequest
  let reqHead = WS.pendingRequest pendingConn
  onConnRes <- _hOnConn wsHandlers wsId reqHead
  either (onReject wsId) (onAccept wsId) onConnRes

  where
    onReject wsId rejectRequest = do
      WS.rejectRequestWith pendingConn rejectRequest
      writeLog $ WSLog wsId ERejected

    onAccept wsId (AcceptWith a acceptWithParams keepAliveM onJwtExpiryM) = do
      conn  <- WS.acceptRequestWith pendingConn acceptWithParams
      writeLog $ WSLog wsId EAccepted

      sendQ <- STM.newTQueueIO
      let wsConn = WSConn wsId logger conn sendQ a
      STM.atomically $ STMMap.insert wsConn wsId connMap

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
          onConnClose wsConn
        -- this will happen when jwt is expired
        Right _ -> do
          writeLog $ WSLog (_wcConnId wsConn) EJwtExpired
          onConnClose wsConn

    onConnClose wsConn = do
      STM.atomically $ STMMap.delete (_wcConnId wsConn) connMap
      _hOnClose wsHandlers wsConn
      writeLog $ WSLog (_wcConnId wsConn) EClosed
