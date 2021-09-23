module Hasura.GC where
  
import           Hasura.Prelude
  
import           GHC.Stats
import           Hasura.Logging
import           System.Mem                  (performMajorGC)

import qualified Control.Concurrent.Extended as C

-- | The RTS's idle GC doesn't work for us:
--
--    - when `-I` is too low it may fire continuously causing scary high CPU
--      when idle among other issues (see #2565)
--    - when we set it higher it won't run at all leading to memory being
--      retained when idle (especially noticeable when users are benchmarking and
--      see memory stay high after finishing). In the theoretical worst case
--      there is such low haskell heap pressure that we never run finalizers to
--      free the foreign data from e.g. libpq.
--    - as of GHC 8.10.2 we have access to `-Iw`, but those two knobs still
--      don’t give us a guarantee that a major GC will always run at some
--      minumum frequency (e.g. for finalizers)
--
-- ...so we hack together our own using GHC.Stats, which should have
-- insignificant runtime overhead.
ourIdleGC
  :: Logger Hasura
  -> DiffTime -- ^ Run a major GC when we've been "idle" for idleInterval
  -> DiffTime -- ^ ...as long as it has been > minGCInterval time since the last major GC
  -> DiffTime -- ^ Additionally, if it has been > maxNoGCInterval time, force a GC regardless.
  -> IO void
ourIdleGC (Logger logger) idleInterval minGCInterval maxNoGCInterval =
  startTimer >>= go 0 0
  where
    go gcs_prev major_gcs_prev timerSinceLastMajorGC = do
      timeSinceLastGC <- timerSinceLastMajorGC
      when (timeSinceLastGC < minGCInterval) $ do
        -- no need to check idle until we've passed the minGCInterval:
        C.sleep (minGCInterval - timeSinceLastGC)

      RTSStats{gcs, major_gcs} <- getRTSStats
      -- We use minor GCs as a proxy for "activity", which seems to work
      -- well-enough (in tests it stays stable for a few seconds when we're
      -- logically "idle" and otherwise increments quickly)
      let areIdle = gcs == gcs_prev
          areOverdue = timeSinceLastGC > maxNoGCInterval

         -- a major GC was run since last iteration (cool!), reset timer:
      if | major_gcs > major_gcs_prev -> do
             startTimer >>= go gcs major_gcs

         -- we are idle and its a good time to do a GC, or we're overdue and must run a GC:
         | areIdle || areOverdue -> do
             when (areOverdue && not areIdle) $
               logger $ UnstructuredLog LevelWarn $
                 "Overdue for a major GC: forcing one even though we don't appear to be idle"
             performMajorGC
             startTimer >>= go (gcs+1) (major_gcs+1)

         -- else keep the timer running, waiting for us to go idle:
         | otherwise -> do
             C.sleep idleInterval
             go gcs major_gcs timerSinceLastMajorGC