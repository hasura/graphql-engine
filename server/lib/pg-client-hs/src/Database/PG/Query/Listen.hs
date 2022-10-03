{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# HLINT ignore "Use onLeft" #-}
{-# HLINT ignore "Use onNothing" #-}

-- Reference:- https://github.com/hasura/skor/blob/master/src/skor.c

module Database.PG.Query.Listen
  ( PGChannel (..),
    NotifyHandler,
    PGNotifyEvent (..),
    listen,
  )
where

-------------------------------------------------------------------------------

import Control.Concurrent (threadWaitRead)
import Control.Exception.Safe (displayException, try)
import Control.Monad (forever, unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Foldable
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text qualified as T
import Database.PG.Query.Connection
import Database.PG.Query.Pool
import Database.PG.Query.Transaction
import Database.PostgreSQL.LibPQ qualified as PQ
import Prelude

-------------------------------------------------------------------------------

type PGChannel :: Type
newtype PGChannel = PGChannel {getChannelTxt :: T.Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString)

type PGNotifyEvent :: Type
data PGNotifyEvent
  = PNEOnStart
  | PNEPQNotify !PQ.Notify
  deriving stock (Show)

type NotifyHandler :: Type
type NotifyHandler = PGNotifyEvent -> IO ()

-- | listen on given channel
listen ::
  ( FromPGConnErr e,
    FromPGTxErr e,
    MonadError e m,
    MonadIO m,
    MonadBaseControl IO m
  ) =>
  PGPool ->
  PGChannel ->
  NotifyHandler ->
  m ()
listen pool channel handler = catchConnErr $
  withExpiringPGconn pool $ \pgConn -> do
    let conn = pgPQConn pgConn

    -- Issue listen command
    eRes <-
      liftIO $
        runExceptT $
          execMulti pgConn (mkTemplate listenCmd) $ const $ return ()
    either throwTxErr return eRes
    -- Emit onStart event
    liftIO $ handler PNEOnStart
    forever $ do
      -- Make postgres connection ready for reading
      r <- liftIO $ runExceptT $ waitForReadReadiness conn
      either (throwError . fromPGConnErr) return r
      -- Check for input
      success <- liftIO $ PQ.consumeInput conn
      unless success $ throwConsumeFailed conn
      liftIO $ processNotifs conn
  where
    listenCmd = "LISTEN  " <> getChannelTxt channel <> ";"
    throwTxErr =
      throwError . fromPGTxErr . PGTxErr listenCmd [] False
    throwConsumeFailed conn = do
      msg <- liftIO $ readConnErr conn
      throwError $ fromPGConnErr $ PGConnErr msg

    processNotifs conn = do
      -- Collect notification
      mNotify <- PQ.notifies conn
      for_ mNotify $ \n -> do
        -- Apply notify handler on arrived notification
        handler $ PNEPQNotify n
        -- Process remaining notifications if any
        processNotifs conn

waitForReadReadiness :: PQ.Connection -> ExceptT PGConnErr IO ()
waitForReadReadiness conn = do
  -- Get file descriptor of underlying socket of a connection
  mFd <- lift $ PQ.socket conn
  fd <- maybe (throwError $ PGConnErr "connection is not currently open") pure mFd
  -- Wait for the socket to be ready for reading
  waitResult <- lift . try $ threadWaitRead fd
  either (throwError . ioErrorToPGConnErr) return waitResult
  where
    ioErrorToPGConnErr :: IOError -> PGConnErr
    ioErrorToPGConnErr = PGConnErr . T.pack . displayException
