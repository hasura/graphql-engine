module Hasura.GraphQL.Execute.LiveQuery.Options
  ( LiveQueriesOptions(..)
  , BatchSize
  , unBatchSize
  , RefetchInterval
  , unRefetchInterval
  , mkLiveQueriesOptions
  , mkBatchSize
  , mkRefetchInterval
  ) where

import           Hasura.Prelude

import qualified Data.Aeson              as J

import           Hasura.RQL.Types.Common

data LiveQueriesOptions
  = LiveQueriesOptions
  { _lqoBatchSize       :: !BatchSize
  , _lqoRefetchInterval :: !RefetchInterval
  } deriving (Show, Eq)

mkLiveQueriesOptions :: Maybe BatchSize -> Maybe RefetchInterval -> LiveQueriesOptions
mkLiveQueriesOptions batchSize refetchInterval = LiveQueriesOptions
  { _lqoBatchSize = fromMaybe (BatchSize 100) batchSize
  , _lqoRefetchInterval = fromMaybe (RefetchInterval 1) refetchInterval
  }

instance J.ToJSON LiveQueriesOptions where
  toJSON (LiveQueriesOptions batchSize refetchInterval) =
    J.object [ "batch_size" J..= batchSize
             , "refetch_delay" J..= refetchInterval
             ]

instance J.FromJSON LiveQueriesOptions where
  parseJSON = J.withObject "live query options" \o ->
    LiveQueriesOptions <$> o J..: "batch_size"
                       <*> o J..: "refetch_delay"

newtype BatchSize = BatchSize { unBatchSize :: NonNegativeInt }
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

mkBatchSize :: Int -> Maybe BatchSize
mkBatchSize x = BatchSize <$> mkNonNegativeInt x

-- TODO this is treated as milliseconds in fromEnv and as seconds in ToJSON.
--      ideally this would have e.g. ... unRefetchInterval :: Milliseconds
newtype RefetchInterval = RefetchInterval { unRefetchInterval :: NonNegativeDiffTime }
  deriving (Show, Eq, J.ToJSON, J.FromJSON)

mkRefetchInterval :: DiffTime -> Maybe RefetchInterval
mkRefetchInterval x = RefetchInterval <$> mkNonNegativeDiffTime x
