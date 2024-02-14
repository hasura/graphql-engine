{-# LANGUAGE TemplateHaskell #-}

module Hasura.GraphQL.Execute.Subscription.Options
  ( SubscriptionsOptions (..),
    LiveQueriesOptions,
    StreamQueriesOptions,
    BatchSize (..),
    RefetchInterval (..),
    mkSubscriptionsOptions,
    mkBatchSize,
    mkRefetchInterval,
  )
where

import Data.Aeson qualified as J
import Hasura.Base.Instances ()
import Hasura.Prelude
import Refined (NonNegative, Refined, refineFail, refineTH)

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
    { _lqoBatchSize = fromMaybe (BatchSize $$(refineTH 100)) batchSize,
      _lqoRefetchInterval = fromMaybe (RefetchInterval $$(refineTH 1)) refetchInterval
    }

instance J.ToJSON SubscriptionsOptions where
  toJSON (SubscriptionsOptions batchSize refetchInterval) =
    J.object
      [ "batch_size" J..= batchSize,
        "refetch_delay" J..= refetchInterval
      ]

instance J.FromJSON SubscriptionsOptions where
  parseJSON = J.withObject "live query options" \o ->
    SubscriptionsOptions
      <$> o
      J..: "batch_size"
      <*> o
      J..: "refetch_delay"

newtype BatchSize = BatchSize {unBatchSize :: Refined NonNegative Int}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

mkBatchSize :: Int -> Maybe BatchSize
mkBatchSize x = BatchSize <$> refineFail x

-- TODO this is treated as milliseconds in fromEnv and as seconds in ToJSON.
--      ideally this would have e.g. ... unRefetchInterval :: Milliseconds
newtype RefetchInterval = RefetchInterval {unRefetchInterval :: Refined NonNegative DiffTime}
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

mkRefetchInterval :: DiffTime -> Maybe RefetchInterval
mkRefetchInterval x = RefetchInterval <$> refineFail x
