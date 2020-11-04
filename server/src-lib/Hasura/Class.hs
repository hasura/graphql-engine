module Hasura.Class
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


class (Monad m) => MonadMetadataStorage m where

  -- Scheduled triggers
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
