{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Eventing.Common
  ( LockedEventsCtx (..),
    saveLockedEvents,
    removeEventFromLockedEvents,
    generateScheduleTimes,
    cleanupSchedulesToBeGenerated,
    deleteEventTriggerLogsInBatchesWith,
  )
where

import Control.Arrow.Extended
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.List (unfoldr)
import Data.Set qualified as Set
import Data.Time
import Hasura.Base.Error (QErr)
import Hasura.Prelude
import Hasura.RQL.Types.Action (LockedActionEventId)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
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
  liftIO
    $ atomically
    $ do
      lockedEventsVals <- readTVar lockedEvents
      writeTVar lockedEvents
        $! Set.union lockedEventsVals
        $ Set.fromList eventIds

-- | Remove an event from the 'LockedEventsCtx' after it has been processed
removeEventFromLockedEvents ::
  (MonadIO m) => EventId -> TVar (Set.Set EventId) -> m ()
removeEventFromLockedEvents eventId lockedEvents =
  liftIO
    $ atomically
    $ do
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

deleteEventTriggerLogsInBatchesWith ::
  (MonadIO m, MonadError QErr m) =>
  IO (Maybe (TriggerLogCleanupConfig, EventTriggerCleanupStatus)) ->
  TriggerLogCleanupConfig ->
  (TriggerLogCleanupConfig -> IO (Either QErr DeletedEventLogStats)) ->
  m DeletedEventLogStats
deleteEventTriggerLogsInBatchesWith getLatestCleanupConfig oldCleanupConfig dbLogDeleteAction = do
  -- fetch the latest cleanup config from the schema cache
  latestCleanupConfig <- liftIO getLatestCleanupConfig
  case latestCleanupConfig of
    -- if the cleanup has been paused, then don't delete anything
    Just (_, ETCSPaused) -> pure (DeletedEventLogStats 0 0)
    nonPausedNewConfig -> do
      -- get latest cleanup config if available, else use the older one
      let cleanupConfig = maybe oldCleanupConfig fst nonPausedNewConfig
      -- delete one batch of the logs
      deletedStatsForCurrentBatch@(DeletedEventLogStats delEventLogsInBatch delInvocationLogsInBatch) <-
        liftEitherM $ liftIO $ dbLogDeleteAction cleanupConfig
      -- If no logs has been deleted, then end the recursion
      if deletedStatsForCurrentBatch == (DeletedEventLogStats 0 0)
        then pure deletedStatsForCurrentBatch
        else do
          -- if non zero logs has been deleted then do a recursion
          (DeletedEventLogStats deletedRemainingEventLogs deletedRemainingInvocationLogs) <-
            deleteEventTriggerLogsInBatchesWith getLatestCleanupConfig cleanupConfig dbLogDeleteAction
          -- Finally collect all the statistics
          pure (DeletedEventLogStats (delEventLogsInBatch + deletedRemainingEventLogs) (delInvocationLogsInBatch + deletedRemainingInvocationLogs))
