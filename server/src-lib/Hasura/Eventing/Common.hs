module Hasura.Eventing.Common
    ( PGTextArray(..)
    , LockedEventsCtx(..)
    , saveLockedEvents
    , removeEventFromLockedEvents
    ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Hasura.Prelude
import           Hasura.RQL.Types.Action           (LockedActionEventId)
import           Hasura.RQL.Types.EventTrigger     (EventId)
import           Hasura.RQL.Types.ScheduledTrigger (CronEventId, OneOffScheduledEventId)

import qualified Data.Set                          as Set
import qualified Database.PG.Query                 as Q
import qualified Database.PG.Query.PTI             as PTI
import qualified PostgreSQL.Binary.Encoding        as PE

-- | PGTextArray is only used for PG array encoding
newtype PGTextArray =
  PGTextArray { unPGTextArray :: [Text]}
  deriving (Show, Eq)

instance Q.ToPrepArg PGTextArray where
  toPrepVal (PGTextArray l) =
    Q.toPrepValHelper PTI.unknown encoder l
    where
      -- 25 is the OID value of TEXT, https://jdbc.postgresql.org/development/privateapi/constant-values.html
      encoder = PE.array 25 . PE.dimensionArray foldl' (PE.encodingArray . PE.text_strict)

data LockedEventsCtx
  = LockedEventsCtx
  { leCronEvents   :: TVar (Set.Set CronEventId)
  , leOneOffEvents :: TVar (Set.Set OneOffScheduledEventId)
  , leEvents       :: TVar (Set.Set EventId)
  , leActionEvents :: TVar (Set.Set LockedActionEventId)
  }

-- | After the events are fetched from the DB, we store the locked events
--   in a hash set(order doesn't matter and look ups are faster) in the
--   event engine context
saveLockedEvents :: (MonadIO m) => [EventId] -> TVar (Set.Set EventId) -> m ()
saveLockedEvents eventIds lockedEvents =
  liftIO $ atomically $ do
    lockedEventsVals <- readTVar lockedEvents
    writeTVar lockedEvents $!
      Set.union lockedEventsVals $ Set.fromList eventIds

-- | Remove an event from the 'LockedEventsCtx' after it has been processed
removeEventFromLockedEvents
  :: MonadIO m => EventId -> TVar (Set.Set EventId) -> m ()
removeEventFromLockedEvents eventId lockedEvents =
  liftIO $ atomically $ do
  lockedEventsVals <- readTVar lockedEvents
  writeTVar lockedEvents $! Set.delete eventId lockedEventsVals
