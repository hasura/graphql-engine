module Hasura.Class
  ( SchemaSyncEventProcessResult(..)
  , SchemaSyncEventPayload(..)
  , MetadataStorageT(..)
  , runMetadataStorageT
  , MonadMetadataStorage(..)
  )
where

import           Hasura.Eventing.HTTP
import           Hasura.Eventing.ScheduledTrigger.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Types
import           Hasura.Session

import           Control.Monad.Morph                    (MFunctor, hoist)

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.Time                              as UTC
import qualified Hasura.Tracing                         as Tracing
import qualified Network.HTTP.Types                     as HTTP

data SchemaSyncEventProcessResult
  = SchemaSyncEventProcessResult
  { _sseprShouldReload       :: !Bool
  , _sseprCacheInvalidations :: !CacheInvalidations
  }

data SchemaSyncEventPayload
  = SchemaSyncEventPayload
  { _ssepInstanceId    :: !InstanceId
  , _ssepOccurredAt    :: !UTC.UTCTime
  , _ssepInvalidations :: !CacheInvalidations
  }
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''SchemaSyncEventPayload)

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

  -- Metadata
  getMetadata :: MetadataStorageT m Metadata

  setMetadata :: Metadata -> MetadataStorageT m ()
  notifySchemaCacheSync :: InstanceId -> CacheInvalidations -> MetadataStorageT m ()
  processSchemaSyncEventPayload
    :: InstanceId -> J.Value -> MetadataStorageT m SchemaSyncEventProcessResult
  getCatalogState :: MetadataStorageT m CatalogState
  setCatalogState :: CatalogStateType -> J.Value -> MetadataStorageT m ()

  -- Async actions
  insertAction
    :: ActionName -> SessionVariables -> [HTTP.Header] -> J.Value
    -> MetadataStorageT m ActionId
  fetchUndeliveredActionEvents :: MetadataStorageT m [ActionLogItem]
  setActionStatus :: ActionId -> AsyncActionStatus -> MetadataStorageT m ()
  fetchActionResponse :: ActionId -> MetadataStorageT m ActionLogResponse

  -- Scheduled triggers
  getDeprivedCronTriggerStats :: MetadataStorageT m [CronTriggerStats]
  getScheduledEventsForDelivery :: MetadataStorageT m ([CronEvent], [OneOffScheduledEvent])
  getOneOffScheduledEvents :: MetadataStorageT m [OneOffScheduledEvent]
  getCronEvents :: TriggerName -> MetadataStorageT m [CronEvent]
  getInvocations :: ScheduledEventId -> ScheduledEventType -> MetadataStorageT m [ScheduledEventInvocation]
  insertScheduledEvent :: ScheduledEventSeed -> MetadataStorageT m ()
  deleteScheduledEvent :: ScheduledEventId -> ScheduledEventType -> MetadataStorageT m ()
  insertScheduledEventInvocation
    :: Invocation 'ScheduledType -> ScheduledEventType -> MetadataStorageT m ()
  setScheduledEventOp
    :: ScheduledEventId -> ScheduledEventOp -> ScheduledEventType -> MetadataStorageT m ()
  unlockScheduledEvents
    :: ScheduledEventType -> [ScheduledEventId] -> MetadataStorageT m Int
  unlockAllLockedScheduledEvents :: MetadataStorageT m ()
  clearFutureCronEvents :: TriggerName -> MetadataStorageT m ()

instance (MonadMetadataStorage m) => MonadMetadataStorage (ReaderT r m) where

  getMetadata                       = (hoist lift) getMetadata
  setMetadata                       = (hoist lift) . setMetadata
  notifySchemaCacheSync a b         = (hoist lift) $ notifySchemaCacheSync a b
  processSchemaSyncEventPayload a b = (hoist lift) $ processSchemaSyncEventPayload a b
  getCatalogState                   = (hoist lift) getCatalogState
  setCatalogState a b               = (hoist lift) $ setCatalogState a b

  insertAction a b c d = (hoist lift) $ insertAction a b c d
  fetchUndeliveredActionEvents = (hoist lift) fetchUndeliveredActionEvents
  setActionStatus a b = (hoist lift) $ setActionStatus a b
  fetchActionResponse = (hoist lift) . fetchActionResponse

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = (hoist lift) getScheduledEventsForDelivery
  getOneOffScheduledEvents           = (hoist lift) getOneOffScheduledEvents
  getCronEvents                      = (hoist lift) . getCronEvents
  getInvocations a b                 = (hoist lift) $ getInvocations a b
  insertScheduledEvent               = (hoist lift) . insertScheduledEvent
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = (hoist lift) $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
  clearFutureCronEvents              = (hoist lift) . clearFutureCronEvents
  deleteScheduledEvent a b           = (hoist lift) $ deleteScheduledEvent a b

instance (MonadMetadataStorage m) => MonadMetadataStorage (ExceptT e m) where

  getMetadata                       = (hoist lift) getMetadata
  setMetadata                       = (hoist lift) . setMetadata
  notifySchemaCacheSync a b         = (hoist lift) $ notifySchemaCacheSync a b
  processSchemaSyncEventPayload a b = (hoist lift) $ processSchemaSyncEventPayload a b
  getCatalogState                   = (hoist lift) getCatalogState
  setCatalogState a b               = (hoist lift) $ setCatalogState a b

  insertAction a b c d = (hoist lift) $ insertAction a b c d
  fetchUndeliveredActionEvents = (hoist lift) fetchUndeliveredActionEvents
  setActionStatus a b = (hoist lift) $ setActionStatus a b
  fetchActionResponse = (hoist lift) . fetchActionResponse

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = (hoist lift) getScheduledEventsForDelivery
  getOneOffScheduledEvents           = (hoist lift) getOneOffScheduledEvents
  getCronEvents                      = (hoist lift) . getCronEvents
  getInvocations a b                 = (hoist lift) $ getInvocations a b
  insertScheduledEvent               = (hoist lift) . insertScheduledEvent
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = (hoist lift) $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
  clearFutureCronEvents              = (hoist lift) . clearFutureCronEvents
  deleteScheduledEvent a b           = (hoist lift) $ deleteScheduledEvent a b

instance (MonadMetadataStorage m) => MonadMetadataStorage (Tracing.TraceT m) where

  getMetadata                       = (hoist lift) getMetadata
  setMetadata                       = (hoist lift) . setMetadata
  notifySchemaCacheSync a b         = (hoist lift) $ notifySchemaCacheSync a b
  processSchemaSyncEventPayload a b = (hoist lift) $ processSchemaSyncEventPayload a b
  getCatalogState                   = (hoist lift) getCatalogState
  setCatalogState a b               = (hoist lift) $ setCatalogState a b

  insertAction a b c d = (hoist lift) $ insertAction a b c d
  fetchUndeliveredActionEvents = (hoist lift) fetchUndeliveredActionEvents
  setActionStatus a b = (hoist lift) $ setActionStatus a b
  fetchActionResponse = (hoist lift) . fetchActionResponse

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = (hoist lift) getScheduledEventsForDelivery
  getOneOffScheduledEvents           = (hoist lift) getOneOffScheduledEvents
  getCronEvents                      = (hoist lift) . getCronEvents
  getInvocations a b                 = (hoist lift) $ getInvocations a b
  insertScheduledEvent               = (hoist lift) . insertScheduledEvent
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = (hoist lift) $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
  clearFutureCronEvents              = (hoist lift) . clearFutureCronEvents
  deleteScheduledEvent a b           = (hoist lift) $ deleteScheduledEvent a b
