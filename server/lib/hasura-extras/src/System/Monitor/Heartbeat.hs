{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "avoid Control.Concurrent.threadDelay" #-}
{-# HLINT ignore "avoid Control.Concurrent.forkIO" #-}

-- | This module implements a simple heartbeat monitor that is to be run in 'main'
-- before argument parsing.
--
-- This is a relatively simple way to implement robust distributed process management.
--
-- A heartbeat is simply the string "HB" sent over some channel at a certain interval,
-- indicating that the process should continue executing. When the heartbeats stop,
-- that's a signal the process should terminate. This way, no processes are left
-- running if their spawning/managing process crashes.
module System.Monitor.Heartbeat
  ( -- * Listening for heartbeats
    HeartbeatOptions (..),
    HeartbeatSource (..),
    monitorHeartbeat,
    monitorHeartbeatMain,

    -- * Emitting heartbeats
    emitHeartbeatHandle,
    heartbeatThread,
  )
where

import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Monad
import Data.ByteString qualified as BS
import Data.Char
import Data.IORef
import Data.Time
import Data.Typeable
import GHC.Generics
import Options.Generic
import System.Environment
import System.Exit
import System.IO
import Prelude

-- | Start monitoring for heartbeats, configured by arguments given to the executable.
--
-- The format of arguments are:
--
-- +Heartbeat
--   --enable-monitoring <Bool, default False>
--   --frequency-seconds <Int, default 10>
--   --source <HeartbeatSource, default StdInSource>
-- -Heartbeat
--
-- Running this action before doing application argument parsing gives a similar
-- behavior to how the GHC runtime is configured by '+RTS ... -RTS' arguments
-- that are censored for the application.
monitorHeartbeatMain :: IO () -> IO ()
monitorHeartbeatMain main = do
  (options, otherArgs) <- getHeartbeatOptions
  when (hoEnableMonitoring options) (monitorHeartbeat options)
  withArgs otherArgs main

monitorHeartbeat :: HeartbeatOptions Unwrapped -> IO ()
monitorHeartbeat HeartbeatOptions {..} = do
  mainThread <- myThreadId
  check <- heartbeatChecker hoSource
  void $ forkIO $ while $ do
    threadDelay (hoFrequencySeconds * 10 ^ (6 :: Int))

    latestBeat <- check
    now <- getCurrentTime

    let missedBeats =
          (now `diffUTCTime` latestBeat)
            / secondsToNominalDiffTime (fromIntegral hoFrequencySeconds)

    if (missedBeats > 2)
      then do
        putStrLn "Heartbeats have stopped - Exiting"
        throwTo mainThread ExitSuccess
        return False
      else return True
  where
    while body = do
      cond <- body
      when cond (while body)

heartbeatChecker :: HeartbeatSource -> IO (IO UTCTime)
heartbeatChecker StdInSource = do
  start <- getCurrentTime
  lastHeartbeat <- newIORef start
  void $ forkIO $ forever $ do
    hb <- getLine
    case hb of
      "HB" -> do
        now <- getCurrentTime
        writeIORef lastHeartbeat now
      _ -> return ()

  return $ readIORef lastHeartbeat

getHeartbeatOptions :: IO (HeartbeatOptions Unwrapped, [String])
getHeartbeatOptions = do
  args <- getArgs
  let (heartbeatArgs, otherArgs) = filterHeartbeatArgs args
  options <- parseHeartbeatArgs heartbeatArgs
  return (options, otherArgs)
  where
    filterHeartbeatArgs :: [String] -> ([String], [String])
    filterHeartbeatArgs args = case span (/= "+Heartbeat") args of
      (prefix, []) -> ([], prefix)
      (prefix, _ : heartbeatsStart) -> case span (/= "-Heartbeat") heartbeatsStart of
        (heartbeats, []) -> (heartbeats, prefix)
        (heartbeats, _ : rest) ->
          let (heartbeats', rest') = filterHeartbeatArgs rest
           in (heartbeats ++ heartbeats', prefix ++ rest')

    parseHeartbeatArgs :: [String] -> IO (HeartbeatOptions Unwrapped)
    parseHeartbeatArgs args = do
      withArgs args $ unwrapRecord ("NOTE: The following options have to go withing +Heartbeat ... -Heartbeat brackets!")

data HeartbeatOptions w = HeartbeatOptions
  { hoEnableMonitoring :: w ::: Bool <!> "False" <?> "Whether to enable heartbeat monitoring",
    hoFrequencySeconds :: w ::: Int <!> "10" <?> "Number of seconds between each expected heartbeat",
    hoSource :: w ::: HeartbeatSource <!> "StdInSource" <?> "The source to listen to heartbeats from"
  }
  deriving (Generic)

instance ParseRecord (HeartbeatOptions Wrapped) where
  parseRecord =
    parseRecordWithModifiers
      lispCaseModifiers
        { fieldNameModifier =
            fieldNameModifier
              lispCaseModifiers
              . dropWhile isLower
        }

data HeartbeatSource = StdInSource
  deriving (Show, Read, Eq, Typeable, Generic)

instance ParseField HeartbeatSource

instance ParseFields HeartbeatSource

instance ParseRecord HeartbeatSource

-- * Functions for sending heartbeats

-- | Emit a heartbeat message on a handle.
emitHeartbeatHandle :: Handle -> IO ()
emitHeartbeatHandle h = do
  BS.hPutStr h "HB\n"
  hFlush h

-- | Spawn a thread emitting heartbeats, returning an action that cancels the
-- thread.
heartbeatThread :: IO () -> Int -> IO (IO ())
heartbeatThread emitHeartbeat frequencySeconds = do
  threadHandle <- Async.async $ forever $ do
    emitHeartbeat
    threadDelay (frequencySeconds * 10 ^ (6 :: Int))
  return (Async.cancel threadHandle)
