{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hasura.GraphQL.Transport.WebSocket.Server
  ( WSId(..)

  , WSConn
  , getData
  , getWSId
  , closeConn
  , sendMsg

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
import qualified Data.ByteString.Lazy     as BL
import qualified Data.UUID                as UUID
import qualified Data.UUID.V4             as UUID
import qualified ListT
import qualified Network.WebSockets       as WS
import qualified STMContainers.Map        as STMMap

import           Control.Exception        (try)
import           Hasura.Prelude

newtype WSId
  = WSId { unWSId :: UUID.UUID }
  deriving (Show, Eq, Hashable)

type AcceptWith a = (a, WS.AcceptRequest, Maybe (WSConn a -> IO ()))
type OnConnH a    = WSId -> WS.RequestHead ->
                    IO (Either WS.RejectRequest (AcceptWith a))
type OnCloseH a   = WS.ConnectionException -> WSConn a -> IO ()
type OnMessageH a = WSConn a -> BL.ByteString -> IO ()

data WSHandlers a
  = WSHandlers
  { _hOnConn    :: OnConnH a
  , _hOnMessage :: OnMessageH a
  , _hOnClose   :: OnCloseH a
  }

data WSConn a
  = WSConn
  { _wcConnId    :: !WSId
  , _wcConnRaw   :: !WS.Connection
  , _wcSendQ     :: !(STM.TQueue BL.ByteString)
  , _wcExtraData :: !a
  }

getData :: WSConn a -> a
getData = _wcExtraData

getWSId :: WSConn a -> WSId
getWSId = _wcConnId

closeConn :: WSConn a -> BL.ByteString -> IO ()
closeConn wsConn =
  WS.sendClose (_wcConnRaw wsConn)

-- writes to a queue instead of the raw connection
-- so that sendMsg doesn't block
sendMsg :: WSConn a -> BL.ByteString -> IO ()
sendMsg wsConn msg =
  STM.atomically $ STM.writeTQueue (_wcSendQ wsConn) msg

type ConnMap a = STMMap.Map WSId (WSConn a)

newtype WSServer a =
  WSServer { _unServerState :: ConnMap a}

createWSServer :: STM.STM (WSServer a)
createWSServer = WSServer <$> STMMap.new

closeAll :: WSServer a -> BL.ByteString -> IO ()
closeAll (WSServer connMap) msg = do
  conns <- STM.atomically $ do
    conns <- ListT.toList $ STMMap.stream connMap
    STMMap.deleteAll connMap
    return conns
  void $ A.mapConcurrently (flip closeConn msg . snd) conns

createServerApp
  :: WSServer a
  -- user provided handlers
  -> WSHandlers a
  -- aka WS.ServerApp
  -> WS.PendingConnection -> IO ()
createServerApp (WSServer connMap) wsHandlers pendingConn = do
  wsId <- WSId <$> UUID.nextRandom
  let reqHead = WS.pendingRequest pendingConn
  onConnRes <- _hOnConn wsHandlers wsId reqHead
  either (WS.rejectRequestWith pendingConn) (onAccept wsId) onConnRes

  where

    onAccept wsId (a, acceptWithParams, keepAliveM) = do
      conn  <- WS.acceptRequestWith pendingConn acceptWithParams
      sendQ <- STM.newTQueueIO

      let wsConn = WSConn wsId conn sendQ a

      STM.atomically $ STMMap.insert wsConn wsId connMap

      putStrLn $ "connected: " <> show wsId

      rcvRef  <- A.async $ forever $ do
        msg <- WS.receiveData conn
        putStrLn $ "recv: " <> show wsId
        print msg
        _hOnMessage wsHandlers wsConn msg

      sendRef <- A.async $ forever $ do
        msg <- STM.atomically $ STM.readTQueue sendQ
        putStrLn $ "send: " <> show wsId
        print msg
        WS.sendTextData conn msg

      keepAliveRefM <- forM keepAliveM $ \action -> A.async $ action wsConn

      -- terminates on WS.ConnectionException
      let waitOnRefs = maybeToList keepAliveRefM <> [rcvRef, sendRef]
      res <- try $ A.waitAnyCancel waitOnRefs

      case res of
        Left e  -> onConnClose e wsConn
        -- this will never happen as both the threads never finish
        Right _ -> return ()

    onConnClose e wsConn = do
      putStrLn $ "close: " <> show (_wcConnId wsConn) <> ": " <> show e
      STM.atomically $ STMMap.delete (_wcConnId wsConn) connMap
      _hOnClose wsHandlers e wsConn
