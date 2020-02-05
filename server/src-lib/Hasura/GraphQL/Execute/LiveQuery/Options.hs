module Hasura.GraphQL.Execute.LiveQuery.Options
  ( LiveQueriesOptions(..)
  , BatchSize(..)
  , RefetchInterval(..)
  , mkLiveQueriesOptions
  ) where

import           Hasura.Prelude

import qualified Data.Aeson            as J

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

newtype BatchSize = BatchSize { unBatchSize :: Int }
  deriving (Show, Eq, J.ToJSON)

-- TODO this is treated as milliseconds in fromEnv and as seconds in ToJSON. 
--      ideally this would have e.g. ... unRefetchInterval :: Milliseconds
newtype RefetchInterval = RefetchInterval { unRefetchInterval :: DiffTime }
  deriving (Show, Eq, J.ToJSON)
