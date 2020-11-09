-- | This module has type class and types which implements the Metadata Storage Abstraction
{-# LANGUAGE UndecidableInstances #-}
module Hasura.Metadata.Class
  ( MetadataStorageT(..)
  , runMetadataStorageT
  , MonadMetadataStorage(..)
  )
where

import           Control.Monad.Morph                    (MFunctor)

import           Hasura.Eventing.HTTP
import           Hasura.Eventing.ScheduledTrigger.Types
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Hasura.Tracing                         as Tracing

-- | Metadata storage transformer that enables raising @'QErr' exceptions.
newtype MetadataStorageT m a
  = MetadataStorageT {unMetadataStorageT :: ExceptT QErr m a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadTrans
           , MonadIO
           , MFunctor
           , Tracing.HasReporter
           )

runMetadataStorageT
  :: MetadataStorageT m a -> m (Either QErr a)
runMetadataStorageT =
  runExceptT . unMetadataStorageT

{- Note [Todo: Common interface for eventing sub-system]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Postgres tables' event triggers and scheduled event triggers are similar in the
core logic. But currently, their implementation is completely isolated and do not share
a common schema in Postgres. We're having a plan to simplify them via a common interface
(maybe via a Postgres extension). This will potentially reduce number of interactions made
to database and schema foot print.

TODO: Reference to open issue or rfc?
-}

-- | Metadata storage abstraction via a type class.
--
-- This type class enables storing and managing Hasura metadata in an isolated
-- database which will not interfere with user's database where tables/functions
-- are defined. Hence, it'll enable support for databases of multiple backends
-- like MySQL, MSSQL etc.
--
-- This class has functions broadly related to:
--
-- 1. Metadata Management
-- ----------------------
-- TODO
-- Basic metadata management functions such as retrieving metadata from storage
-- database and replacing the given metadata.
--
-- 2. Scheduled Triggers
-- ---------------------
-- Eventing sub-system for scheduled triggers is implemented via metadata storage.
-- All necessary functions are included in the type class.
-- TODO
-- This also includes functions to fetch events and their invocations so that the
-- console can show them in the UI.
-- For more details, refer description in 'Hasura.Eventing.ScheduledTrigger' module.
--
-- 3. Async Actions
-- ----------------
-- TODO
-- Operations to implement async actions sub-system. This includes recording an
-- async action event and retreiving the details of action delivery to the webhook.
-- For more details see Note [Async action architecture] in 'Hasura.GraphQL.Execute.Action' module.
--
-- It is believed that all the above three are implemented in a single storage
-- system (ex: a Postgres database). We can split the functions into appropriate and
-- specific type classes in future iterations if required.

class (MonadError QErr m) => MonadMetadataStorage m where

  -- Scheduled triggers
  -- By design, scheduled trigger eventing has many database interactions.
  -- Hence we have many functions. We can reduce the functions by having
  -- a common interface for eventing. See Note [Todo: Common interface for eventing sub-system]
  getDeprivedCronTriggerStats :: m [CronTriggerStats]
  getScheduledEventsForDelivery :: m ([CronEvent], [OneOffScheduledEvent])
  insertScheduledEvent :: ScheduledEventSeed -> m ()
  insertScheduledEventInvocation :: Invocation 'ScheduledType -> ScheduledEventType -> m ()
  setScheduledEventOp :: ScheduledEventId -> ScheduledEventOp -> ScheduledEventType -> m ()
  unlockScheduledEvents :: ScheduledEventType -> [ScheduledEventId] -> m Int
  unlockAllLockedScheduledEvents :: m ()

instance (MonadMetadataStorage m) => MonadMetadataStorage (ReaderT r m) where
  getDeprivedCronTriggerStats        = lift getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = lift getScheduledEventsForDelivery
  insertScheduledEvent               = lift . insertScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = lift unlockAllLockedScheduledEvents

instance (MonadMetadataStorage m) => MonadMetadataStorage (Tracing.TraceT m) where
  getDeprivedCronTriggerStats        = lift getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = lift getScheduledEventsForDelivery
  insertScheduledEvent               = lift . insertScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = lift unlockAllLockedScheduledEvents
