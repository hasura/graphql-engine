-- | This module has type class and types which implements the Metadata Storage Abstraction
module Hasura.Metadata.Class
  ( MetadataStorageT(..)
  , runMetadataStorageT
  , MonadMetadataStorage(..)
  )
where

import           Hasura.Eventing.HTTP
import           Hasura.Eventing.ScheduledTrigger.Types
import           Hasura.Prelude
import           Hasura.RQL.Types

import           Control.Monad.Morph                    (MFunctor, hoist)

import qualified Hasura.Tracing                         as Tracing

newtype MetadataStorageT m a
  = MetadataStorageT {unMetadataStorageT :: ExceptT QErr m a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MFunctor
           , MonadTrans
           , MonadIO
           )

runMetadataStorageT
  :: MetadataStorageT m a -> m (Either QErr a)
runMetadataStorageT =
  runExceptT . unMetadataStorageT

{- Note [Todo: Common interface for eventing sub-system]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Scheduled and Table event triggers have their own implementation
and tables in Postgres. The core logic for both is same. So it
is necessary to have a unified interface (maybe via a Postgres extension)
where all event triggers implementation can extend from. By this we can
fairly reduce the Postgres schema foot print and interactions to the database.

TODO: Reference to open issue or rfc?
-}

-- | Metadata storage abstraction via a type class.
--
-- This class has functions broadly related to:
--
-- 1. Metadata Management (TODO)
-- ---------------------
-- Basic metadata management functions such as retrieving metadata from storage
-- database and replacing the given metadata.
--
-- 2. Scheduled Triggers
-- ---------------------
-- Eventing sub-system for scheduled triggers is implemented via metadata storage.
-- All necessary functions are included in the type class. (TODO) The console has
-- This also includes functions to fetch events and their invocations so that the
-- console can show them in the UI.
-- For more details, refer description in 'Hasura.Eventing.ScheduledTrigger' module.
--
-- 3. Async Actions (TODO)
-- ----------------
-- Operations to implement async actions sub-system. This includes recording an
-- async action event and retreiving the details of action delivery to the webhook.
-- For more details see Note [Async action architecture] in 'Hasura.GraphQL.Execute.Action' module.

class (Monad m) => MonadMetadataStorage m where

  -- Scheduled triggers
  -- By design, scheduled trigger eventing has many database interactions.
  -- Hence we have many functions. We can reduce the functions by having
  -- a common interface for eventing. See Note [Todo: Common interface for eventing sub-system]
  getDeprivedCronTriggerStats :: MetadataStorageT m [CronTriggerStats]
  getScheduledEventsForDelivery :: MetadataStorageT m ([CronEvent], [OneOffScheduledEvent])
  insertScheduledEvent :: ScheduledEventSeed -> MetadataStorageT m ()
  insertScheduledEventInvocation
    :: Invocation 'ScheduledType -> ScheduledEventType -> MetadataStorageT m ()
  setScheduledEventOp
    :: ScheduledEventId -> ScheduledEventOp -> ScheduledEventType -> MetadataStorageT m ()
  unlockScheduledEvents
    :: ScheduledEventType -> [ScheduledEventId] -> MetadataStorageT m Int
  unlockAllLockedScheduledEvents :: MetadataStorageT m ()

instance (MonadMetadataStorage m) => MonadMetadataStorage (ReaderT r m) where

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = (hoist lift) getScheduledEventsForDelivery
  insertScheduledEvent               = (hoist lift) . insertScheduledEvent
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = (hoist lift) $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents

instance (MonadMetadataStorage m) => MonadMetadataStorage (ExceptT e m) where

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = (hoist lift) getScheduledEventsForDelivery
  insertScheduledEvent               = (hoist lift) . insertScheduledEvent
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = (hoist lift) $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents

instance (MonadMetadataStorage m) => MonadMetadataStorage (Tracing.TraceT m) where

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = (hoist lift) getScheduledEventsForDelivery
  insertScheduledEvent               = (hoist lift) . insertScheduledEvent
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = (hoist lift) $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents

instance (MonadMetadataStorage m) => MonadMetadataStorage (LazyTxT e m) where

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = (hoist lift) getScheduledEventsForDelivery
  insertScheduledEvent               = (hoist lift) . insertScheduledEvent
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = (hoist lift) $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
