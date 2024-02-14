{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "avoid Control.Concurrent.forkIO" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Tasks
  ( incrementCounterWithMultipleWriters,
  )
where

import Control.Concurrent
import Control.Monad
import Data.Int (Int64)
import qualified System.Metrics.Prometheus.Counter as C

-- | Perform 100,000 atomic increments using 100 concurrent writers, and
-- check the final count.
incrementCounterWithMultipleWriters :: IO Int64
incrementCounterWithMultipleWriters = do
  counter <- C.new
  locks <- replicateM n newEmptyMVar
  mapM_ (forkIO . work counter iters) locks
  mapM_ takeMVar locks
  total <- C.readInt counter
  unless (fromIntegral total == n * iters) $
    error "Incorrect count!"
  pure total
  where
    n = 100
    iters = 100000

    work :: C.Counter -> Int -> MVar () -> IO ()
    work !_ 0 !lock = putMVar lock ()
    work counter i lock = C.inc counter >> work counter (i - 1) lock
