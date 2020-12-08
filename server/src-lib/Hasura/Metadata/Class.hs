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

{- Note [Todo: Common interface for eventing sub-system]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Postgres tables' event triggers and scheduled event triggers are similar in the
core logic. But currently, their implementation is completely isolated and do not
share a common schema in Postgres. We're having a plan to simplify them via a
common 'event storage and retrieval' interface (maybe via a Postgres extension?).
This will potentially reduce number of interactions made to database and schema foot print.

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
-- 1. Metadata Management (TODO: Need to be added to the type class)
-- ----------------------
-- Basic metadata management functions such as retrieving metadata from storage
-- database and replacing the given metadata.
--
-- 2. Scheduled Triggers
-- ---------------------
-- Eventing sub-system for scheduled triggers is implemented via metadata storage.
-- For more details, refer description in 'Hasura.Eventing.ScheduledTrigger' module.
--
-- TODO: Functions need to be added to the type class
-- - Retrieving invocation logs from storage (console requirement)
-- - Deleting an scheduled event
-- - Creating an one-off scheduled event
--
-- 3. Async Actions (TODO: Need to be added to the type class)
-- ----------------
-- Operations to implement async actions sub-system. This includes recording an
-- async action event and retreiving the details of action delivery to the webhook.
-- For more details see Note [Async action architecture] in 'Hasura.GraphQL.Execute.Action' module.
--
-- It is believed that all the above three are implemented in a single storage
-- system (ex: a Postgres database). We can split the functions into appropriate and
-- specific type classes in future iterations if required.

class (MonadError QErr m) => MonadMetadataStorage m where

  -- Scheduled triggers
  -- TODO:-
  -- Ideally we would've liked to avoid having functions that are specific to
  -- scheduled/cron triggers and instead have functions that provide a generic
  -- 'event storage and retrieval' interface but we'll have to change a lot of
  -- existing code for scheduled and cron triggers. We can get to this after the
  -- multi-source work is done. See Note [Todo: Common interface for eventing sub-system]
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

{- Note [Generic MetadataStorageT transformer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All methods of the MonadMetadataStorage class may fail, which we represent in
the usual way using a MonadError superclass:

    class MonadError QErr m => MonadMetadataStorage m

However, unusually, the location where we pick a concrete MonadMetadataStorage
instance is not a context where we can handle errors, and as such the monad at
that point has no MonadError instance! Instead, clients of MonadMetadataStorage
are expected to handle errors /locally/, even though the code is parameterized
over an arbitrary metadata storage mechanism.

To encode this, we take a slightly unorthodox approach involving the auxiliary
MetadataStorageT transformer, which is really just a wrapper around ExceptT:

    newtype MetadataStorageT m a
      = MetadataStorageT { unMetadataStorageT :: ExceptT QErr m a }

We then define MonadMetadataStorage instances on a transformer stack comprising
both MetadataStorageT and a concrete base monad:

    instance MonadMetadataStorage (MetadataStorageT PGMetadataStorageApp)

This looks unconventional, but it allows polymorphic code to be parameterized
over the metadata storage implementation while still handling errors locally.
Such functions include a constraint of the form

    MonadMetadataStorage (MetadataStorageT m) => ...

and use runMetadataStorageT at the location where errors should be handled, e.g.:

    result <- runMetadataStorageT do
      {- ... some metadata operations ... -}
    case result of
      Left err -> ...
      Right value -> ...

In other words, runMetadataStorageT serves as a marker that says “I’m going to
handle exceptions raised by metadata operations right here,” which allows them
to be handled more locally than the point at which the concrete
MonadMetadataStorage instance (and thus the particular metadata storage
implementation) is actually chosen. -}

-- | The 'MetadataStorageT' transformer adds ability to throw exceptions
-- for monads deriving @'MonadMetadataStorage' instance.
-- For more details see Note [Generic MetadataStorageT transformer]
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
