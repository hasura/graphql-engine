module Hasura.GraphQL.Execute.Subscription.Types
  ( SubscriptionTypes (..),
  )
where

import Control.Concurrent.STM qualified as STM
import Data.Kind (Type)
import Hasura.GraphQL.Execute.Subscription.Plan (CursorVariableValues)
import Hasura.GraphQL.Execute.Subscription.Poll.Common qualified as C
import Hasura.RQL.Types.Subscription (SubscriptionType (..))

class SubscriptionTypes (s :: SubscriptionType) where
  type Cohort s :: Type
  type CohortSnapshot s :: Type
  type CohortMap s :: Type
  type Poller s :: Type
  type PollerMap s :: Type

instance SubscriptionTypes 'LiveQuery where
  type Cohort 'LiveQuery = C.Cohort ()
  type CohortSnapshot 'LiveQuery = C.CohortSnapshot
  type CohortMap 'LiveQuery = C.CohortMap ()
  type Poller 'LiveQuery = C.Poller ()
  type PollerMap 'LiveQuery = C.PollerMap ()

instance SubscriptionTypes 'Streaming where
  type Cohort 'Streaming = C.Cohort (STM.TVar CursorVariableValues)
  type CohortSnapshot 'Streaming = C.CohortSnapshot
  type CohortMap 'Streaming = C.CohortMap (STM.TVar CursorVariableValues)
  type Poller 'Streaming = C.Poller (STM.TVar CursorVariableValues)
  type PollerMap 'Streaming = C.PollerMap (STM.TVar CursorVariableValues)
