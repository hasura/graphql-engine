{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Redis.ConnectionContext (
    ConnectionContext(..)
  , ConnectTimeout(..)
  , ConnectionLostException(..)
  , PortID(..)
  , connect
  , disconnect
  , send
  , recv
  , errConnClosed
  , enableTLS
  , flush
  , ioErrorToConnLost
) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import Control.Monad(when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.IORef as IOR
import qualified Data.Time as Time
import Data.Maybe (fromMaybe)
import Control.Concurrent.MVar(newMVar, readMVar, swapMVar)
import Control.Exception(bracketOnError, Exception, throwIO, try)
import           Data.Typeable
import Data.Functor(void)
import qualified Network.Socket as NS
import qualified Network.TLS as TLS
import System.Environment (lookupEnv)
import System.IO(Handle, hSetBinaryMode, hClose, IOMode(..), hFlush, hIsOpen)
import System.IO.Error(catchIOError)
import Text.Read (readMaybe)

data ConnectionContext = NormalHandle Handle | TLSContext TLS.Context

instance Show ConnectionContext where
    show (NormalHandle _) = "NormalHandle"
    show (TLSContext _) = "TLSContext"

data Connection = Connection
    { ctx :: ConnectionContext
    , lastRecvRef :: IOR.IORef (Maybe B.ByteString) }

instance Show Connection where
    show Connection{..} = "Connection{ ctx = " ++ show ctx ++ ", lastRecvRef = IORef}"

data ConnectPhase
  = PhaseUnknown
  | PhaseResolve
  | PhaseOpenSocket
  deriving (Show)

newtype ConnectTimeout = ConnectTimeout ConnectPhase
  deriving (Show, Typeable)

instance Exception ConnectTimeout

data ConnectionLostException = ConnectionLost deriving Show
instance Exception ConnectionLostException

data PortID = PortNumber NS.PortNumber
            | UnixSocket String
            deriving (Eq, Show)

connect :: NS.HostName -> PortID -> Maybe Int -> IO ConnectionContext
connect hostName portId timeoutOpt =
  bracketOnError hConnect hClose $ \h -> do
    hSetBinaryMode h True
    return $ NormalHandle h
  where
        hConnect = do
          phaseMVar <- newMVar PhaseUnknown
          let doConnect = hConnect' phaseMVar
          envTimeout <- round . (\x -> (x :: Time.NominalDiffTime) * 1000000) . realToFrac . fromMaybe (0.5 :: Double) . (>>= readMaybe) <$> lookupEnv "REDIS_CONNECT_TIMEOUT"
          result <- race doConnect (threadDelay $ fromMaybe envTimeout timeoutOpt)
          case result of
            Left h -> return h
            Right () -> do
              phase <- readMVar phaseMVar
              errConnectTimeout phase
        hConnect' mvar = bracketOnError createSock NS.close $ \sock -> do
          NS.setSocketOption sock NS.KeepAlive 1
          void $ swapMVar mvar PhaseResolve
          void $ swapMVar mvar PhaseOpenSocket
          NS.socketToHandle sock ReadWriteMode
          where
            createSock = case portId of
              PortNumber portNumber -> do
                addrInfo <- getHostAddrInfo hostName portNumber
                connectSocket addrInfo
              UnixSocket addr -> bracketOnError
                (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol)
                NS.close
                (\sock -> NS.connect sock (NS.SockAddrUnix addr) >> return sock)

getHostAddrInfo :: NS.HostName -> NS.PortNumber -> IO [NS.AddrInfo]
getHostAddrInfo hostname port =
  NS.getAddrInfo (Just hints) (Just hostname) (Just $ show port)
  where
    hints = NS.defaultHints
      { NS.addrSocketType = NS.Stream }

errConnectTimeout :: ConnectPhase -> IO a
errConnectTimeout phase = throwIO $ ConnectTimeout phase

connectSocket :: [NS.AddrInfo] -> IO NS.Socket
connectSocket [] = error "connectSocket: unexpected empty list"
connectSocket (addr:rest) = tryConnect >>= \case
  Right sock -> return sock
  Left err   -> if null rest
                then throwIO err
                else connectSocket rest
  where
    tryConnect :: IO (Either IOError NS.Socket)
    tryConnect = bracketOnError createSock NS.close $ \sock ->
      try (NS.connect sock $ NS.addrAddress addr) >>= \case
      Right () -> return (Right sock)
      Left err -> NS.close sock >> return (Left err)
      where
        createSock = NS.socket (NS.addrFamily addr)
                               (NS.addrSocketType addr)
                               (NS.addrProtocol addr)

send :: ConnectionContext -> B.ByteString -> IO ()
send (NormalHandle h) requestData =
      ioErrorToConnLost (B.hPut h requestData)
send (TLSContext ctx) requestData =
        ioErrorToConnLost (TLS.sendData ctx (LB.fromStrict requestData))

recv :: ConnectionContext -> IO B.ByteString
recv (NormalHandle h) = ioErrorToConnLost $ B.hGetSome h 4096
recv (TLSContext ctx) = TLS.recvData ctx


ioErrorToConnLost :: IO a -> IO a
ioErrorToConnLost a = a `catchIOError` (\x -> putStrLn ("exception while running redis query: " <> show x) *> errConnClosed)

errConnClosed :: IO a
errConnClosed = throwIO ConnectionLost


enableTLS :: TLS.ClientParams -> ConnectionContext -> IO ConnectionContext
enableTLS tlsParams (NormalHandle h) = do
  ctx <- TLS.contextNew h tlsParams
  TLS.handshake ctx
  return $ TLSContext ctx
enableTLS _ c@(TLSContext _) = return c

disconnect :: ConnectionContext -> IO ()
disconnect (NormalHandle h) = do
  open <- hIsOpen h
  when open $ hClose h
disconnect (TLSContext ctx) = do
  TLS.bye ctx
  TLS.contextClose ctx

flush :: ConnectionContext -> IO ()
flush (NormalHandle h) = hFlush h
flush (TLSContext c) = TLS.contextFlush c
