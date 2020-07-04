module Hasura.Eventing.Common where

import           Hasura.Prelude
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Hasura.RQL.Types.EventTrigger     (EventId)
import           Hasura.RQL.Types.ScheduledTrigger (CronEventId,StandAloneScheduledEventId)

import qualified Data.Set                       as Set

data LockedEventsCtx
  = LockedEventsCtx
  { leCronEvents :: TVar (Set.Set CronEventId)
  , leStandAloneEvents :: TVar (Set.Set StandAloneScheduledEventId)
  , leEvents :: TVar (Set.Set EventId)
  }

initLockedEventsCtx :: STM LockedEventsCtx
initLockedEventsCtx = do
  leCronEvents <- newTVar Set.empty
  leStandAloneEvents <- newTVar Set.empty
  leEvents <- newTVar Set.empty
  return $ LockedEventsCtx{..}

-- | After the events are fetched from the DB, we store the locked events
--   in a hash set(order doesn't matter and look ups are faster) in the
--   event engine context
saveLockedEvents :: [Text] -> TVar (Set.Set Text)  -> IO ()
saveLockedEvents eventIds lockedEvents =
  atomically $ do
    lockedEventsVals <- readTVar lockedEvents
    writeTVar lockedEvents $!
      Set.union lockedEventsVals $ Set.fromList eventIds

-- | Remove an event from the 'LockedEventsCtx' after it has been processed
removeEventFromLockedEvents :: Text -> TVar (Set.Set Text) -> IO ()
removeEventFromLockedEvents eventId lockedEvents =
  atomically $ do
  lockedEventsVals <- readTVar lockedEvents
  writeTVar lockedEvents $! Set.delete eventId lockedEventsVals
