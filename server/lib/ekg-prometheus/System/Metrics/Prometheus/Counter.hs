-- | This module defines a type for mutable, integer-valued counters.
-- Counters are non-negative, monotonically increasing values and can
-- be used to track e.g. the number of requests served since program
-- start.  All operations on counters are thread-safe.
module System.Metrics.Prometheus.Counter
  ( Counter,
    new,
    read,
    readInt,
    inc,
    add,
  )
where

import qualified Data.Atomic as Atomic
import Data.Int (Int64)
import Prelude hiding (read)

-- | A mutable, integer-valued counter.
newtype Counter = C {unC :: Atomic.Atomic}

-- | Create a new, zero initialized, counter.
new :: IO Counter
new = C `fmap` Atomic.new 0

-- | Get the current value of the counter as a 'Double'.
--
-- > read = fmap fromIntegral . readInt
read :: Counter -> IO Double
read = fmap fromIntegral . readInt

-- | Get the current value of the counter.
readInt :: Counter -> IO Int64
readInt = Atomic.read . unC

-- | Increase the counter by one.
inc :: Counter -> IO ()
inc counter = add counter 1

-- | Add the argument to the counter.
add :: Counter -> Int64 -> IO ()
add counter = Atomic.add (unC counter)
