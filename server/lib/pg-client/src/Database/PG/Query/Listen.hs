{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import Control.Monad.Except
import Data.Foldable
import Data.String (IsString)
import Data.Text qualified as T
import Database.PG.Query.Connection
import Database.PG.Query.Pool
import Database.PG.Query.Transaction
import Database.PostgreSQL.LibPQ qualified as PQ
import Prelude

-------------------------------------------------------------------------------

newtype PGChannel = PGChannel {getChannelTxt :: T.Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString)

data PGNotifyEvent
  = PNEOnStart
  | PNEPQNotify !PQ.Notify
  deriving stock (Show)

type NotifyHandler = PGNotifyEvent -> IO ()

-- | listen on given channel
listen ::
  ( FromPGConnErr e,
    FromPGTxErr e,
    MonadError e m,
    MonadIO m
  ) =>
  PGPool ->
  PGChannel ->
  NotifyHandler ->
  m ()
listen pool channel handler = (>>= liftEither) $ liftIO $ runExceptT $ withConn pool $ \pgConn -> do
  let conn = pgPQConn pgConn

  -- Issue listen command
  withExceptT (fromPGTxErr . handleTxErr) $
    execMulti pgConn (mkTemplate listenCmd) $
      const $
        return ()
  -- Emit onStart event
  lift $ handler PNEOnStart
  forever $ withExceptT fromPGConnErr $ do
    -- Make postgres connection ready for reading
    waitForReadReadiness conn
    -- Check for input
    success <- lift $ PQ.consumeInput conn
    unless success $ throwConsumeFailed conn
    lift $ processNotifs conn
  where
    listenCmd = "LISTEN  " <> getChannelTxt channel <> ";"
    handleTxErr =
      PGTxErr listenCmd [] False
    throwConsumeFailed conn = do
      msg <- lift $ readConnErr conn
      throwError $ PGConnErr msg

    processNotifs conn = do
      -- Collect a notification
      mNotify <- PQ.notifies conn
      for_ @Maybe mNotify $ \n -> do
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
  withExceptT ioErrorToPGConnErr $ ExceptT $ try $ threadWaitRead fd
  where
    ioErrorToPGConnErr :: IOError -> PGConnErr
    ioErrorToPGConnErr = PGConnErr . T.pack . displayException
