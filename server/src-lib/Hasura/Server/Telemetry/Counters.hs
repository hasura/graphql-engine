{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
--  Counters used in telemetry collection. Additional counters can be added here.and
--  serviced in "Hasura.Server.Telemetry".
module Hasura.Server.Telemetry.Counters
  ( -- * Service timing and counts, by various dimensions

    -- ** Local metric recording
    recordTimingMetric,
    RequestDimensions (..),
    RequestTimings (..),

    -- *** Dimensions
    QueryType (..),
    Locality (..),
    Transport (..),

    -- ** Metric upload
    dumpServiceTimingMetrics,
    ServiceTimingMetrics (..),
    ServiceTimingMetric (..),
    RunningTimeBucket (..),
    RequestTimingsCount (..),
  )
where

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import GHC.IO.Unsafe (unsafePerformIO)
import Hasura.Prelude

-- | The properties that characterize this request. The dimensions over which
-- we collect metrics for each serviced request.
data RequestDimensions = RequestDimensions
  { telemQueryType :: !QueryType,
    telemLocality :: !Locality,
    telemTransport :: !Transport
  }
  deriving (Show, Generic, Eq, Ord)

instance Hashable RequestDimensions

-- | Accumulated time metrics.
data RequestTimings = RequestTimings
  { -- | Time spent waiting on PG/remote http calls
    telemTimeIO :: !Seconds,
    -- | Total service time for request (including 'telemTimeIO')
    telemTimeTot :: !Seconds
  }

-- | Sum
instance Semigroup RequestTimings where
  RequestTimings a b <> RequestTimings x y = RequestTimings (a + x) (b + y)

-- | 'RequestTimings' along with the count
data RequestTimingsCount = RequestTimingsCount
  { telemTimeIO :: !Seconds,
    telemTimeTot :: !Seconds,
    -- | The number of requests that have contributed to the accumulated timings above.
    -- So e.g. @telemTimeTot / count@ would give the mean service time.
    telemCount :: !Word
  }
  deriving (Show, Generic, Eq, Ord)

-- | Sum
instance Semigroup RequestTimingsCount where
  RequestTimingsCount a b c <> RequestTimingsCount x y z =
    RequestTimingsCount (a + x) (b + y) (c + z)

-- | Internal. Counts and durations across many 'RequestDimensions'.
--
-- NOTE: We use the global mutable variable pattern for metric collection
-- counters for convenience at collection site (don't wear hairshirts that
-- discourage useful reporting).
requestCounters :: IORef (HashMap.HashMap (RequestDimensions, RunningTimeBucket) RequestTimingsCount)
{-# NOINLINE requestCounters #-}
requestCounters = unsafePerformIO $ newIORef HashMap.empty

-- | Internal. Since these metrics are accumulated while graphql-engine is
-- running and sent periodically, we need to include a tag that is unique for
-- each start of hge. This lets us e.g. query for just the latest uploaded
-- sample for each start of hge.
--
-- We use time rather than a UUID since having this be monotonic increasing is
-- convenient.
approxStartTime :: POSIXTime
{-# NOINLINE approxStartTime #-}
approxStartTime = unsafePerformIO getPOSIXTime

-- | Was this request a mutation (involved DB writes)?
data QueryType = Mutation | Query
  deriving (Enum, Show, Eq, Ord, Generic)

instance Hashable QueryType

instance J.ToJSON QueryType

instance J.FromJSON QueryType

-- | Was this a PG local query, or did it involve remote execution?
data Locality
  = -- | No data was fetched
    Empty
  | -- | local DB data
    Local
  | -- | remote schema
    Remote
  | -- | mixed
    Heterogeneous
  deriving (Enum, Show, Eq, Ord, Generic)

instance Hashable Locality

instance J.ToJSON Locality

instance J.FromJSON Locality

instance Semigroup Locality where
  Empty <> x = x
  x <> Empty = x
  x <> y | x == y = x
  _ <> _ = Heterogeneous

instance Monoid Locality where
  mempty = Empty

-- | Was this a query over http or websockets?
data Transport = HTTP | WebSocket
  deriving (Enum, Show, Eq, Ord, Generic)

instance Hashable Transport

instance J.ToJSON Transport

instance J.FromJSON Transport

-- | The timings and counts here were from requests with total time longer than
-- 'bucketGreaterThan' (but less than any larger bucket cutoff times).
newtype RunningTimeBucket = RunningTimeBucket {bucketGreaterThan :: Seconds}
  deriving (Ord, Eq, Show, Generic, J.ToJSON, J.FromJSON, Hashable)

-- NOTE: an HDR histogram is a nice way to collect metrics when you don't know
-- a priori what the most useful binning is. It's not clear how we'd make use
-- of that here though. So these buckets are arbitrary, and can be adjusted as
-- needed, but we shouldn't have more than a handful to keep payload size down.
totalTimeBuckets :: [RunningTimeBucket]
totalTimeBuckets = coerce [0.000, 0.001, 0.050, 1.000, 3600.000 :: Seconds]

-- | Save a timing metric sample in our in-memory store. These will be
-- accumulated and uploaded periodically in "Hasura.Server.Telemetry".
recordTimingMetric :: (MonadIO m) => RequestDimensions -> RequestTimings -> m ()
recordTimingMetric reqDimensions RequestTimings {..} = liftIO $ do
  let ourBucket =
        fromMaybe (RunningTimeBucket 0)
          $ listToMaybe -- although we expect 'head' would be safe here
          $ dropWhile (> coerce telemTimeTot)
          $ reverse
          $ sort totalTimeBuckets
  atomicModifyIORef' requestCounters
    $ (,())
    . HashMap.insertWith (<>) (reqDimensions, ourBucket) RequestTimingsCount {telemCount = 1, ..}

-- | The final shape of this part of our metrics data JSON. This should allow
-- reasonably efficient querying using GIN indexes and JSONB containment
-- operations (which treat arrays as sets).
data ServiceTimingMetrics = ServiceTimingMetrics
  { -- | This is set to a new unique value when the counters reset (e.g. because of a restart)
    collectionTag :: Int,
    serviceTimingMetrics :: [ServiceTimingMetric]
  }
  deriving (Show, Generic, Eq, Ord)

data ServiceTimingMetric = ServiceTimingMetric
  { dimensions :: RequestDimensions,
    bucket :: RunningTimeBucket,
    metrics :: RequestTimingsCount
  }
  deriving (Show, Generic, Eq, Ord)

instance J.FromJSON RequestTimingsCount where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON RequestTimingsCount where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

instance J.FromJSON RequestDimensions where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON RequestDimensions where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

instance J.ToJSON ServiceTimingMetric

instance J.FromJSON ServiceTimingMetric

instance J.ToJSON ServiceTimingMetrics

instance J.FromJSON ServiceTimingMetrics

dumpServiceTimingMetrics :: (MonadIO m) => m ServiceTimingMetrics
dumpServiceTimingMetrics = liftIO $ do
  cs <- readIORef requestCounters
  let serviceTimingMetrics = flip map (HashMap.toList cs)
        $ \((dimensions, bucket), metrics) -> ServiceTimingMetric {..}
      collectionTag = round approxStartTime
  return ServiceTimingMetrics {..}
