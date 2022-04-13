module Hasura.GraphQL.Execute.Subscription.Options
  ( SubscriptionsOptions (..),
    LiveQueriesOptions,
    StreamQueriesOptions,
    BatchSize,
    unBatchSize,
    RefetchInterval,
    unRefetchInterval,
    mkSubscriptionsOptions,
    mkBatchSize,
    mkRefetchInterval,
  )
where

import Data.Aeson qualified as J
import Hasura.Prelude
import Hasura.RQL.Types.Common

data SubscriptionsOptions = SubscriptionsOptions
  { _lqoBatchSize :: !BatchSize,
    _lqoRefetchInterval :: !RefetchInterval
  }
  deriving (Show, Eq)

type LiveQueriesOptions = SubscriptionsOptions

type StreamQueriesOptions = SubscriptionsOptions

mkSubscriptionsOptions :: Maybe BatchSize -> Maybe RefetchInterval -> SubscriptionsOptions
mkSubscriptionsOptions batchSize refetchInterval =
  SubscriptionsOptions
    { _lqoBatchSize = fromMaybe (BatchSize 100) batchSize,
      _lqoRefetchInterval = fromMaybe (RefetchInterval 1) refetchInterval
    }

instance J.ToJSON SubscriptionsOptions where
  toJSON (SubscriptionsOptions batchSize refetchInterval) =
    J.object
      [ "batch_size" J..= batchSize,
        "refetch_delay" J..= refetchInterval
      ]

instance J.FromJSON SubscriptionsOptions where
  parseJSON = J.withObject "live query options" \o ->
    SubscriptionsOptions <$> o J..: "batch_size"
      <*> o J..: "refetch_delay"

newtype BatchSize = BatchSize {unBatchSize :: NonNegativeInt}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

mkBatchSize :: Int -> Maybe BatchSize
mkBatchSize x = BatchSize <$> mkNonNegativeInt x

-- TODO this is treated as milliseconds in fromEnv and as seconds in ToJSON.
--      ideally this would have e.g. ... unRefetchInterval :: Milliseconds
newtype RefetchInterval = RefetchInterval {unRefetchInterval :: NonNegativeDiffTime}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

mkRefetchInterval :: DiffTime -> Maybe RefetchInterval
mkRefetchInterval x = RefetchInterval <$> mkNonNegativeDiffTime x
