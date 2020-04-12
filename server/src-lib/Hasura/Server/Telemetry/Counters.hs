{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-|
  Counters used in telemetry collection. Additional counters can be added here.and
  serviced in "Hasura.Server.Telemetry".
-}

module Hasura.Server.Telemetry.Counters
  (
  -- * Service timing and counts, by various dimensions
  -- ** Local metric recording
    recordTimingMetric
  , RequestDimensions(..), RequestTimings(..)
  -- *** Dimensions
  , CacheHit(..), QueryType(..), Locality(..), Transport(..)
  -- ** Metric upload
  , dumpServiceTimingMetrics
  , ServiceTimingMetrics(..)
  , ServiceTimingMetric(..)
  , RunningTimeBucket(..)
  , RequestTimingsCount(..)
  )
  where

import           Hasura.Prelude

import qualified Data.Aeson            as A
import qualified Data.Aeson.Casing     as A
import qualified Data.Aeson.TH         as A
import qualified Data.HashMap.Strict   as HM

import           Data.IORef
import           Data.Ord              (Down (..))
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           GHC.IO.Unsafe         (unsafePerformIO)


-- | The properties that characterize this request. The dimensions over which
-- we collect metrics for each serviced request.
data RequestDimensions =
  RequestDimensions {
        telemCacheHit  :: !CacheHit
      , telemQueryType :: !QueryType
      , telemLocality  :: !Locality
      , telemTransport :: !Transport
      }
      deriving (Show, Generic, Eq)

instance Hashable RequestDimensions


-- | Accumulated time metrics.
data RequestTimings  =
  RequestTimings {
      telemTimeIO  :: !Seconds
    -- ^ Time spent waiting on PG/remote http calls
    , telemTimeTot :: !Seconds
    -- ^ Total service time for request (including 'telemTimeIO')
    }

-- | Sum
instance Semigroup RequestTimings where
  RequestTimings a b <> RequestTimings x y = RequestTimings (a+x) (b+y)

-- | 'RequestTimings' along with the count
data RequestTimingsCount  =
  RequestTimingsCount {
      telemTimeIO  :: !Seconds
    , telemTimeTot :: !Seconds
    , telemCount   :: !Word
    -- ^ The number of requests that have contributed to the accumulated timings above.
    -- So e.g. @telemTimeTot / count@ would give the mean service time.
    }
      deriving (Show, Generic, Eq)

-- | Sum
instance Semigroup RequestTimingsCount where
  RequestTimingsCount a b c <> RequestTimingsCount x y z =
    RequestTimingsCount (a+x) (b+y) (c+z)

-- | Internal. Counts and durations across many 'RequestDimensions'.
--
-- NOTE: We use the global mutable variable pattern for metric collection
-- counters for convenience at collection site (don't wear hairshirts that
-- discourage useful reporting).
requestCounters :: IORef (HM.HashMap (RequestDimensions, RunningTimeBucket) RequestTimingsCount)
{-# NOINLINE requestCounters #-}
requestCounters = unsafePerformIO $ newIORef HM.empty

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

-- | Did this request hit the plan cache?
data CacheHit = Hit | Miss
  deriving (Enum, Show, Eq, Generic)
instance Hashable CacheHit
instance A.ToJSON CacheHit
instance A.FromJSON CacheHit

-- | Was this request a mutation (involved DB writes)?
data QueryType = Mutation | Query
  deriving (Enum, Show, Eq, Generic)
instance Hashable QueryType
instance A.ToJSON QueryType
instance A.FromJSON QueryType

-- | Was this a PG local query, or did it involve remote execution?
data Locality = Local | Remote
  deriving (Enum, Show, Eq, Generic)
instance Hashable Locality
instance A.ToJSON Locality
instance A.FromJSON Locality

-- | Was this a query over http or websockets?
data Transport = HTTP | WebSocket
  deriving (Enum, Show, Eq, Generic)
instance Hashable Transport
instance A.ToJSON Transport
instance A.FromJSON Transport

-- | The timings and counts here were from requests with total time longer than
-- 'bucketGreaterThan' (but less than any larger bucket cutoff times).
newtype RunningTimeBucket = RunningTimeBucket { bucketGreaterThan :: Seconds }
  deriving (Fractional, Num, Ord, Eq, Show, Generic, A.ToJSON, A.FromJSON, Hashable)


-- NOTE: an HDR histogram is a nice way to collect metrics when you don't know
-- a priori what the most useful binning is. It's not clear how we'd make use
-- of that here though.
totalTimeBuckets :: [RunningTimeBucket]
totalTimeBuckets = [0, 1000, 10*1000, 100*1000]

-- | Save a timing metric sample in our in-memory store. These will be
-- accumulated and uploaded periodically in "Hasura.Server.Telemetry".
recordTimingMetric :: MonadIO m=> RequestDimensions -> RequestTimings -> m ()
recordTimingMetric reqDimensions RequestTimings{..} = liftIO $ do
  let ourBucket = fromMaybe 0 $ -- although we expect 'head' would be safe here
        listToMaybe $ dropWhile (> realToFrac telemTimeTot) $
        sortOn Down totalTimeBuckets
  atomicModifyIORef' requestCounters $ (,()) .
    HM.insertWith (<>) (reqDimensions, ourBucket) RequestTimingsCount{telemCount = 1, ..}

-- | The final shape of this part of our metrics data JSON. This should allow
-- reasonably efficient querying using GIN indexes and JSONB containment
-- operations (which treat arrays as sets).
data ServiceTimingMetrics
  = ServiceTimingMetrics
  { collectionTag        :: Int
    -- ^ This is set to a new unique value when the counters reset (e.g. because of a restart)
  , serviceTimingMetrics :: [ServiceTimingMetric]
  }
  deriving (Show, Generic, Eq)
data ServiceTimingMetric
  = ServiceTimingMetric
  { dimensions :: RequestDimensions
  , bucket     :: RunningTimeBucket
  , metrics    :: RequestTimingsCount
  }
  deriving (Show, Generic, Eq)


$(A.deriveJSON (A.aesonDrop 5 A.snakeCase) ''RequestTimingsCount)
$(A.deriveJSON (A.aesonDrop 5 A.snakeCase) ''RequestDimensions)

instance A.ToJSON ServiceTimingMetric
instance A.FromJSON ServiceTimingMetric
instance A.ToJSON ServiceTimingMetrics
instance A.FromJSON ServiceTimingMetrics

dumpServiceTimingMetrics :: MonadIO m=> m ServiceTimingMetrics
dumpServiceTimingMetrics = liftIO $ do
  cs <- readIORef requestCounters
  let serviceTimingMetrics = flip map (HM.toList cs) $
        \((dimensions, bucket), metrics)-> ServiceTimingMetric{..}
      collectionTag = round approxStartTime
  return ServiceTimingMetrics{..}


