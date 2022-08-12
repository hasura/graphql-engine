module Hasura.RQL.Types.Subscription
  ( CursorOrdering (..),
    SubscriptionType (..),
  )
where

import Hasura.Prelude

-- | CursorOrdering is used in the streaming subscriptions to specify how to order the cursor.
data CursorOrdering = COAscending | CODescending deriving (Show, Eq, Generic)

instance Hashable CursorOrdering

data SubscriptionType = Streaming | LiveQuery deriving (Show, Eq, Generic)
