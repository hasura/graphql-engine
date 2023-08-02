-- | Multiplexed subscription poller threads; see "Hasura.GraphQL.Execute.Subscription" for details.
module Hasura.GraphQL.Execute.Subscription.Poll
  ( -- * Pollers
    Poller (..),
    PollerId (..),
    PollerIOState (..),
    pollLiveQuery,
    pollStreamingQuery,
    PollerKey (..),
    BackendPollerKey (..),
    PollerMap,
    dumpPollerMap,
    PollDetails (..),
    BatchExecutionDetails (..),
    CohortExecutionDetails (..),
    SubscriptionPostPollHook,
    defaultSubscriptionPostPollHook,

    -- * Cohorts
    Cohort (..),
    CohortId,
    newCohortId,
    CohortVariables,
    CohortKey,
    CohortMap,

    -- * Subscribers
    Subscriber (..),
    SubscriberId,
    newSubscriberId,
    SubscriberMetadata,
    mkSubscriberMetadata,
    unSubscriberMetadata,
    SubscriberMap,
    OnChange,
    SubscriptionGQResponse,
    SubscriptionResponse (..),
    SubscriptionMetadata (..),
    SubscriberExecutionDetails (..),

    -- * Batch
    BatchId (..),
  )
where

import Hasura.GraphQL.Execute.Subscription.Poll.Common
import Hasura.GraphQL.Execute.Subscription.Poll.LiveQuery
import Hasura.GraphQL.Execute.Subscription.Poll.StreamingQuery
