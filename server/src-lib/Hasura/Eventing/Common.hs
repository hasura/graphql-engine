module Hasura.Eventing.Common
  ( LockedEventsCtx (..),
    saveLockedEvents,
    removeEventFromLockedEvents,
    generateScheduleTimes,
    cleanupSchedulesToBeGenerated,
  )
where

import Control.Arrow.Extended
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.List (unfoldr)
import Data.Set qualified as Set
import Data.Time
import Hasura.Prelude
import Hasura.RQL.Types.Action (LockedActionEventId)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Eventing (EventId)
import Hasura.RQL.Types.ScheduledTrigger (CronEventId, OneOffScheduledEventId)
import System.Cron

data LockedEventsCtx = LockedEventsCtx
  { leCronEvents :: TVar (Set.Set CronEventId),
    leOneOffEvents :: TVar (Set.Set OneOffScheduledEventId),
    leEvents :: TVar (HashMap SourceName (Set.Set EventId)),
    leActionEvents :: TVar (Set.Set LockedActionEventId)
  }

-- | After the events are fetched from the DB, we store the locked events
--   in a hash set(order doesn't matter and look ups are faster) in the
--   event engine context
saveLockedEvents :: (MonadIO m) => [EventId] -> TVar (Set.Set EventId) -> m ()
saveLockedEvents eventIds lockedEvents =
  liftIO $
    atomically $ do
      lockedEventsVals <- readTVar lockedEvents
      writeTVar lockedEvents
        $! Set.union lockedEventsVals
        $ Set.fromList eventIds

-- | Remove an event from the 'LockedEventsCtx' after it has been processed
removeEventFromLockedEvents ::
  MonadIO m => EventId -> TVar (Set.Set EventId) -> m ()
removeEventFromLockedEvents eventId lockedEvents =
  liftIO $
    atomically $ do
      lockedEventsVals <- readTVar lockedEvents
      writeTVar lockedEvents $! Set.delete eventId lockedEventsVals

-- | Generates next @n events starting @from according to 'CronSchedule'
generateScheduleTimes :: UTCTime -> Int -> CronSchedule -> [UTCTime]
generateScheduleTimes from n cron = take n $ go from
  where
    go = unfoldr (fmap dup . nextMatch cron)

-- | number of cleanup schedules to be generated in one iteration
cleanupSchedulesToBeGenerated :: Int
cleanupSchedulesToBeGenerated = 50
