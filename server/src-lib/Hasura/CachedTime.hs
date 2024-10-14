-- safety for unsafePerformIO below
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Hasura.CachedTime (cachedRecentFormattedTimeAndZone) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (uninterruptibleMask_)
import Data.ByteString.Char8 qualified as B8
import Data.IORef
import Data.Time.Clock qualified as Time
import Data.Time.Format
import Data.Time.LocalTime qualified as Time
import Hasura.Prelude
import System.IO.Unsafe

-- | A fast timestamp source, updated every 1sec, at the whims of the RTS, calling
-- 'Time.getCurrentTimeZone' and 'Time.getCurrentTime'
--
-- We also store an equivalent RFC7231 timestamp for use in the @Date@ HTTP
-- header, avoiding 6% latency regression from computing it every time.
-- We use this at call sites to try to avoid warp's code path that uses the
-- auto-update library to do this same thing.
--
-- Formerly we used the auto-update library but observed bugs. See
-- "Hasura.Logging" and #10662
--
-- NOTE: if we wanted to make this more resilient to this thread being
-- descheduled for long periods, we could store monotonic timestamp here (fast)
-- then logging threads could do the same and determine if the time is stale. I
-- considered doing the same to also get more granular timestamps but it seems
-- the addUTCTime makes this just as slow as getCurrentTime
cachedRecentFormattedTimeAndZone :: IORef (Time.UTCTime, Time.TimeZone, B8.ByteString)
{-# NOINLINE cachedRecentFormattedTimeAndZone #-}
cachedRecentFormattedTimeAndZone = unsafePerformIO do
  tRef <- getTimeAndZone >>= newIORef
  void $ forkIO $ uninterruptibleMask_ $ forever do
    threadDelay $ 1000 * 1000
    getTimeAndZone >>= writeIORef tRef
  pure tRef
  where
    getTimeAndZone = do
      !tz <- Time.getCurrentTimeZone
      !t <- Time.getCurrentTime
      let !tRFC7231 = B8.pack $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" t
      pure (t, tz, tRFC7231)
