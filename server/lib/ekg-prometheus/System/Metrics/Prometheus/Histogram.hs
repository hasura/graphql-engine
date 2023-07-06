{-
The module is based on the System.Metrics.Prometheus.Prometheus.Metric.Histogram
module of the prometheus-2.2.3 package:

https://hackage.haskell.org/package/prometheus-2.2.3

The license for prometheus-2.2.3 follows:

BSD 3-Clause License

Copyright (c) 2016-present, Bitnomial, Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

\* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

\* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

\* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# LANGUAGE TupleSections #-}

-- | This module defines a type for mutable histograms with fixed, user-defined
-- buckets. All operations on histograms are thread-safe.
module System.Metrics.Prometheus.Histogram
  ( Histogram,
    HistogramSample (..),
    UpperBound,
    new,
    read,
    observe,
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (read)

newtype Histogram = Histogram {unHistogram :: IORef HistogramSample}

type UpperBound = Double -- Inclusive upper bounds

data HistogramSample = HistogramSample
  { histBuckets :: !(Map UpperBound Int64), -- Non-cumulative counts
    histSum :: !Double,
    histCount :: !Int64
  }
  deriving (Eq, Show)

-- | Create a new, empty histogram. The buckets of the histogram are fixed and
-- defined by the given upper bounds.
new :: [UpperBound] -> IO Histogram
new buckets = Histogram <$> newIORef emptyData
  where
    emptyData =
      HistogramSample
        { histBuckets = Map.fromList $ map (,0) buckets,
          histSum = 0.0,
          histCount = 0
        }

-- | Get the current counts and sum of the observations of the histogram.
read :: Histogram -> IO HistogramSample
read = readIORef . unHistogram

-- | Add an observation to the histogram.
observe :: Histogram -> Double -> IO ()
observe (Histogram histRef) x =
  atomicModifyIORef' histRef $ \histData ->
    let newHistData =
          HistogramSample
            { histBuckets = updateBuckets x (histBuckets histData),
              histSum = histSum histData + x,
              histCount = histCount histData + 1
            }
     in (newHistData, ())

updateBuckets :: Double -> Map UpperBound Int64 -> Map UpperBound Int64
updateBuckets x buckets =
  case Map.lookupGE x buckets of
    Nothing -> buckets
    Just (bucket, count) -> Map.insert bucket (count + 1) buckets
