{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RankNTypes               #-}

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
  , WSEventInfo(..)
  , WSQueueResponse(..)
  , ServerMsgType(..)
  , createWSServer
  , closeAll
  , createServerApp
  , shutdown

  , MonadWSLog (..)
  , HasuraServerApp
  , WSEvent(..)
  , WSLog(..)
  ) where

import qualified Control.Concurrent.Async                    as A
import qualified Control.Concurrent.Async.Lifted.Safe        as LA
import qualified Control.Concurrent.STM                      as STM
import           Control.Exception.Lifted
import qualified Control.Monad.Trans.Control                 as MC
import qualified Data.Aeson                                  as J
import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.TH                               as J
import qualified Data.ByteString.Lazy                        as BL
import           Data.String
import qualified Data.TByteString                            as TBS
import qualified Data.UUID                                   as UUID
import qualified Data.UUID.V4                                as UUID
import           Data.Word                                   (Word16)
import           GHC.AssertNF
import           GHC.Int                                     (Int64)
import           Hasura.Prelude
import qualified ListT
import           Network.Wai.Extended                        (IpAddress)
import qualified Network.WebSockets                          as WS
import qualified StmContainers.Map                           as STMMap
import qualified System.IO.Error                             as E

import           Hasura.GraphQL.Transport.WebSocket.Protocol (OperationId, ServerMsgType (..))
import qualified Hasura.Logging                              as L

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

-- extra websocket event info
data WSEventInfo
  = WSEventInfo
  { _wseiEventType          :: !(Maybe ServerMsgType)
  , _wseiOperationId        :: !(Maybe OperationId)
  , _wseiQueryExecutionTime :: !(Maybe Double)
  , _wseiResponseSize       :: !(Maybe Int64)
  } deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.fieldLabelModifier = J.snakeCase . drop 5
                   , J.omitNothingFields = True
                   }
  ''WSEventInfo)

data WSLog
  = WSLog
  { _wslWebsocketId :: !WSId
  , _wslEvent       :: !WSEvent
  , _wslMetadata    :: !(Maybe WSEventInfo)
  } deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.fieldLabelModifier = J.snakeCase . drop 4
                   , J.omitNothingFields = True
                   }
  ''WSLog)

instance L.ToEngineLog WSLog L.Hasura where
  toEngineLog wsLog =
    (L.LevelDebug, L.ELTInternal L.ILTWsServer, J.toJSON wsLog)

class Monad m => MonadWSLog m where
  -- | Takes WS server log data and logs it
  -- logWSServer
  logWSLog :: L.Logger L.Hasura -> WSLog -> m ()

instance MonadWSLog m => MonadWSLog (ExceptT e m) where
  logWSLog l ws = lift $ logWSLog l ws

instance MonadWSLog m => MonadWSLog (ReaderT r m) where
  logWSLog l ws = lift $ logWSLog l ws

data WSQueueResponse
  = WSQueueResponse
  { _wsqrMessage   :: !BL.ByteString
  , _wsqrEventInfo :: !(Maybe WSEventInfo)
  -- ^ extra metadata that we use for other actions, such as print log
  -- we don't want to inlcude them into websocket message payload
  }

data WSConn a
  = WSConn
  { _wcConnId    :: !WSId
  , _wcLogger    :: !(L.Logger L.Hasura)
  , _wcConnRaw   :: !WS.Connection
  , _wcExtraData :: !a
  }

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

sendMsg :: L.Logger L.Hasura -> WSConn a -> WSQueueResponse -> IO ()
sendMsg (L.Logger writeLog) WSConn{..} = \WSQueueResponse{..} -> do
  liftIO $ WS.sendTextData _wcConnRaw _wsqrMessage
  writeLog $ WSLog _wcConnId (EMessageSent $ TBS.fromLBS _wsqrMessage) _wsqrEventInfo


type ConnMap a = STMMap.Map WSId (WSConn a)

data ServerStatus a
  = AcceptingConns !(ConnMap a)
  | ShuttingDown

data WSServer a
  = WSServer
  { _wssLogger :: !(L.Logger L.Hasura)
  , _wssStatus :: !(STM.TVar (ServerStatus a))
  -- ^ See e.g. createServerApp.onAccept for how we use STM to preserve consistency
  }

createWSServer :: L.Logger L.Hasura -> STM.STM (WSServer a)
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
  , _awKeepAlive   :: !(WSConn a -> IO ())
  , _awOnJwtExpiry :: !(WSConn a -> IO ())
  }

type OnConnH m a    = WSId -> WS.RequestHead -> IpAddress -> m (Either WS.RejectRequest (AcceptWith a))
type OnCloseH m a   = WSConn a -> m ()
type OnMessageH m a = WSConn a -> BL.ByteString -> m ()

data WSHandlers m a
  = WSHandlers
  { _hOnConn    :: OnConnH m a
  , _hOnMessage :: OnMessageH m a
  , _hOnClose   :: OnCloseH m a
  }

-- | aka generalized 'WS.ServerApp' over @m@, which takes an IPAddress
type HasuraServerApp m = IpAddress -> WS.PendingConnection -> m ()

createServerApp
  :: (MonadIO m, MC.MonadBaseControl IO m, LA.Forall (LA.Pure m), MonadWSLog m)
  => WSServer a
  -- user provided handlers
  -> WSHandlers m a
  -- aka WS.ServerApp
  -> HasuraServerApp m
{-# INLINE createServerApp #-}
createServerApp (WSServer logger@(L.Logger writeLog) serverStatus) wsHandlers !ipAddress !pendingConn = do
  wsId <- WSId <$> liftIO UUID.nextRandom
  logWSLog logger $ WSLog wsId EConnectionRequest Nothing
  status <- liftIO $ STM.readTVarIO serverStatus
  case status of
    AcceptingConns _ -> logUnexpectedExceptions $ do
      let reqHead = WS.pendingRequest pendingConn
      onConnRes <- _hOnConn wsHandlers wsId reqHead ipAddress
      either (onReject wsId) (onAccept wsId) onConnRes

    ShuttingDown ->
      onReject wsId shuttingDownReject

  where
    -- It's not clear what the unexpected exception handling story here should be. So at
    -- least log properly and re-raise:
    logUnexpectedExceptions = handle $ \(e :: SomeException) -> do
      writeLog $ L.UnstructuredLog L.LevelError $ fromString $
        "Unexpected exception raised in websocket. Please report this as a bug: "<>show e
      throwIO e

    shuttingDownReject =
      WS.RejectRequest 503
                        "Service Unavailable"
                        [("Retry-After", "0")]
                        "Server is shutting down"

    onReject wsId rejectRequest = do
      liftIO $ WS.rejectRequestWith pendingConn rejectRequest
      logWSLog logger $ WSLog wsId ERejected Nothing

    onAccept wsId (AcceptWith a acceptWithParams keepAlive onJwtExpiry) = do
      conn  <- liftIO $ WS.acceptRequestWith pendingConn acceptWithParams
      logWSLog logger $ WSLog wsId EAccepted Nothing
      let !wsConn = WSConn wsId logger conn a
      -- TODO there are many thunks here. Difficult to trace how much is retained, and
      --      how much of that would be shared anyway.
      --      Requires a fork of 'wai-websockets' and 'websockets', it looks like.
      --      Adding `package` stanzas with -Xstrict -XStrictData for those two packages
      --      helped, cutting the number of thunks approximately in half.
      liftIO $ $assertNFHere wsConn  -- so we don't write thunks to mutable vars

      let whenAcceptingInsertConn = liftIO $ STM.atomically $ do
            status <- STM.readTVar serverStatus
            case status of
              ShuttingDown           -> pure ()
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
          _hOnClose wsHandlers wsConn

        AcceptingConns _ -> do
          let rcv = forever $ do
                -- Process all messages serially (important!), in a separate thread:
                msg <- liftIO $
                  -- Re-throw "receiveloop: resource vanished (Connection reset by peer)" :
                  --   https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Recv.hs#L112
                  -- as WS exception signaling cleanup below. It's not clear why exactly this gets
                  -- raised occasionally; I suspect an equivalent handler is missing from WS itself.
                  -- Regardless this should be safe:
                  handleJust (guard . E.isResourceVanishedError) (\()-> throw WS.ConnectionClosed) $
                    WS.receiveData conn
                logWSLog logger $ WSLog wsId (EMessageReceived $ TBS.fromLBS msg) Nothing
                _hOnMessage wsHandlers wsConn msg

          -- withAsync lets us be very sure that if e.g. an async exception is raised while we're
          -- forking that the threads we launched will be cleaned up. See also below.
          LA.withAsync rcv $ \rcvRef -> do
          -- TODO consider a more efficient scheme, 
          --      e.g. https://github.com/hasura/graphql-engine-internal/issues/462
          LA.withAsync (liftIO $ keepAlive wsConn) $ \keepAliveRef -> do
          -- TODO also try to replace this with 'registerTimeout' or something like above:
          LA.withAsync (liftIO $ onJwtExpiry wsConn) $ \onJwtExpiryRef -> do

          -- terminates on WS.ConnectionException and JWT expiry
          let waitOnRefs = [keepAliveRef, onJwtExpiryRef, rcvRef]
          -- withAnyCancel re-raises exceptions from forkedThreads, and is guarenteed to cancel in
          -- case of async exceptions raised while blocking here:
          try (LA.waitAnyCancel waitOnRefs) >>= \case
            -- NOTE: 'websockets' is a bit of a rat's nest at the moment wrt
            -- exceptions; for now handle all ConnectionException by closing
            -- and cleaning up, see: https://github.com/jaspervdj/websockets/issues/48
            Left ( _ :: WS.ConnectionException) -> do
              logWSLog logger $ WSLog (_wcConnId wsConn) ECloseReceived Nothing
            -- this will happen when jwt is expired
            Right _ -> do
              logWSLog logger $ WSLog (_wcConnId wsConn) EJwtExpired Nothing

    onConnClose wsConn = \case
      ShuttingDown -> pure ()
      AcceptingConns connMap -> do
        liftIO $ STM.atomically $ STMMap.delete (_wcConnId wsConn) connMap
        _hOnClose wsHandlers wsConn
        logWSLog logger $ WSLog (_wcConnId wsConn) EClosed Nothing


shutdown :: WSServer a -> IO ()
shutdown (WSServer (L.Logger writeLog) serverStatus) = do
  writeLog $ L.debugT "Shutting websockets server down"
  conns <- STM.atomically $ do
    conns <- flushConnMap serverStatus
    STM.writeTVar serverStatus ShuttingDown
    return conns
  closeAllWith (flip forceConnReconnect) "shutting server down" conns
