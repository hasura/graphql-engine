-- | This module defines a type for mutable, integer-valued gauges.
-- Gauges are variable values and can be used to track e.g. the
-- current number of concurrent connections. All operations on gauges
-- are thread-safe.
module System.Metrics.Prometheus.Gauge
  ( Gauge,
    new,
    read,
    readInt,
    inc,
    dec,
    add,
    subtract,
    set,
  )
where

import qualified Data.Atomic as Atomic
import Data.Int (Int64)
import Prelude hiding (read, subtract)

-- | A mutable, integer-valued gauge.
newtype Gauge = C {unC :: Atomic.Atomic}

-- | Create a new, zero initialized, gauge.
new :: IO Gauge
new = C `fmap` Atomic.new 0

-- | Get the current value of the gauge as a 'Double'.
--
-- > read = fmap fromIntegral . readInt
read :: Gauge -> IO Double
read = fmap fromIntegral . readInt

-- | Get the current value of the gauge.
readInt :: Gauge -> IO Int64
readInt = Atomic.read . unC

-- | Increase the gauge by one.
inc :: Gauge -> IO ()
inc gauge = add gauge 1

-- | Decrease the gauge by one.
dec :: Gauge -> IO ()
dec gauge = subtract gauge 1

-- | Increase the gauge by the given amount.
add :: Gauge -> Int64 -> IO ()
add gauge = Atomic.add (unC gauge)

-- | Decrease the gauge by the given amount.
subtract :: Gauge -> Int64 -> IO ()
subtract gauge = Atomic.subtract (unC gauge)

-- | Set the gauge to the given value.
set :: Gauge -> Int64 -> IO ()
set gauge = Atomic.write (unC gauge)
