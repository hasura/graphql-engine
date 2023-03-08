-- | This module has type class and types which implements the Metadata Storage Abstraction
module Hasura.Metadata.Class
  ( SchemaSyncEventProcessResult (..),
    MonadMetadataStorage (..),
    MonadMetadataStorageQueryAPI (..),
  )
where

import Control.Monad.Trans.Managed
import Data.Aeson
import Database.PG.Query qualified as PG
import Hasura.Base.Error
import Hasura.Eventing.ScheduledTrigger.Types
import Hasura.Prelude
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Types qualified as HTTP

data SchemaSyncEventProcessResult = SchemaSyncEventProcessResult
  { _sseprShouldReload :: !Bool,
    _sseprCacheInvalidations :: !CacheInvalidations
  }

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
-- Error-handling is handled explicitly, with every function returning an
-- `Either QErr`. This is inelegant, but is required: we want the caller to
-- explictly deal with errors, rather than letting them surface to a "lower"
-- monad, because we want to implement this as a "Service", on the base monad,
-- so that different implementations of the engine can choose how to implement
-- it; and those base monads do not include error handling, all error handling
-- must be done at the endpoint level. As a result, we choose to make the errors
-- explicit in the return type rather than making assumptions about the stack.
--
-- This class has functions broadly related to:
--
-- 1. Metadata Management
-- ----------------------
-- Basic metadata management functions such as retrieving metadata from storage
-- database and replacing the given metadata.
-- TODO: Console specific operations
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
-- 3. Async Actions
-- ----------------
-- Operations to implement async actions sub-system. This includes recording an
-- async action event and retreiving the details of action delivery to the webhook.
-- For more details see Note [Async action architecture] in 'Hasura.GraphQL.Execute.Action' module.
--
-- It is believed that all the above three are implemented in a single storage
-- system (ex: a Postgres database). We can split the functions into appropriate and
-- specific type classes in future iterations if required.
class Monad m => MonadMetadataStorage m where
  -- Metadata
  fetchMetadataResourceVersion :: m (Either QErr MetadataResourceVersion)
  fetchMetadata :: m (Either QErr (Metadata, MetadataResourceVersion))
  fetchMetadataNotifications :: MetadataResourceVersion -> InstanceId -> m (Either QErr [(MetadataResourceVersion, CacheInvalidations)])
  setMetadata :: MetadataResourceVersion -> Metadata -> m (Either QErr MetadataResourceVersion)
  notifySchemaCacheSync :: MetadataResourceVersion -> InstanceId -> CacheInvalidations -> m (Either QErr ())
  getCatalogState :: m (Either QErr CatalogState)

  -- the `setCatalogState` function is used by the console and CLI to store its state
  -- it is disabled when maintenance mode is on
  setCatalogState :: CatalogStateType -> Value -> m (Either QErr ())

  -- get the @db_uuid@ that we store in the database.
  getMetadataDbUid :: m (Either QErr MetadataDbId)
  checkMetadataStorageHealth :: m (Either QErr ())

  -- Scheduled triggers
  -- TODO:-
  -- Ideally we would've liked to avoid having functions that are specific to
  -- scheduled/cron triggers and instead have functions that provide a generic
  -- 'event storage and retrieval' interface but we'll have to change a lot of
  -- existing code for scheduled and cron triggers. We can get to this after the
  -- multi-source work is done. See Note [Todo: Common interface for eventing sub-system]
  getDeprivedCronTriggerStats :: [TriggerName] -> m (Either QErr [CronTriggerStats])
  getScheduledEventsForDelivery :: m (Either QErr ([CronEvent], [OneOffScheduledEvent]))
  insertCronEvents :: [CronEventSeed] -> m (Either QErr ())
  insertOneOffScheduledEvent :: OneOffEvent -> m (Either QErr EventId)
  insertScheduledEventInvocation :: Invocation 'ScheduledType -> ScheduledEventType -> m (Either QErr ())
  setScheduledEventOp :: ScheduledEventId -> ScheduledEventOp -> ScheduledEventType -> m (Either QErr ())
  unlockScheduledEvents :: ScheduledEventType -> [ScheduledEventId] -> m (Either QErr Int)
  unlockAllLockedScheduledEvents :: m (Either QErr ())
  clearFutureCronEvents :: ClearCronEvents -> m (Either QErr ())

  -- Console API requirements
  getOneOffScheduledEvents :: ScheduledEventPagination -> [ScheduledEventStatus] -> RowsCountOption -> m (Either QErr (WithOptionalTotalCount [OneOffScheduledEvent]))
  getCronEvents :: TriggerName -> ScheduledEventPagination -> [ScheduledEventStatus] -> RowsCountOption -> m (Either QErr (WithOptionalTotalCount [CronEvent]))
  getScheduledEventInvocations :: GetScheduledEventInvocations -> m (Either QErr (WithOptionalTotalCount [ScheduledEventInvocation]))
  deleteScheduledEvent :: ScheduledEventId -> ScheduledEventType -> m (Either QErr ())

  -- Async actions
  insertAction ::
    ActionName ->
    SessionVariables ->
    [HTTP.Header] ->
    Value ->
    m (Either QErr ActionId)
  fetchUndeliveredActionEvents :: m (Either QErr [ActionLogItem])
  setActionStatus :: ActionId -> AsyncActionStatus -> m (Either QErr ())
  fetchActionResponse :: ActionId -> m (Either QErr ActionLogResponse)
  clearActionData :: ActionName -> m (Either QErr ())
  setProcessingActionLogsToPending :: LockedActionIdArray -> m (Either QErr ())

instance (MonadMetadataStorage m) => MonadMetadataStorage (ReaderT r m) where
  fetchMetadataResourceVersion = lift fetchMetadataResourceVersion
  fetchMetadata = lift fetchMetadata
  fetchMetadataNotifications a b = lift $ fetchMetadataNotifications a b
  setMetadata r = lift . setMetadata r
  notifySchemaCacheSync a b c = lift $ notifySchemaCacheSync a b c
  getCatalogState = lift getCatalogState
  setCatalogState a b = lift $ setCatalogState a b

  getMetadataDbUid = lift getMetadataDbUid
  checkMetadataStorageHealth = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery = lift getScheduledEventsForDelivery
  insertCronEvents = lift . insertCronEvents
  insertOneOffScheduledEvent = lift . insertOneOffScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b c = lift $ getOneOffScheduledEvents a b c
  getCronEvents a b c d = lift $ getCronEvents a b c d
  getScheduledEventInvocations a = lift $ getScheduledEventInvocations a
  deleteScheduledEvent a b = lift $ deleteScheduledEvent a b

  insertAction a b c d = lift $ insertAction a b c d
  fetchUndeliveredActionEvents = lift fetchUndeliveredActionEvents
  setActionStatus a b = lift $ setActionStatus a b
  fetchActionResponse = lift . fetchActionResponse
  clearActionData = lift . clearActionData
  setProcessingActionLogsToPending = lift . setProcessingActionLogsToPending

instance (MonadMetadataStorage m) => MonadMetadataStorage (StateT s m) where
  fetchMetadataResourceVersion = lift fetchMetadataResourceVersion
  fetchMetadata = lift fetchMetadata
  fetchMetadataNotifications a b = lift $ fetchMetadataNotifications a b
  setMetadata r = lift . setMetadata r
  notifySchemaCacheSync a b c = lift $ notifySchemaCacheSync a b c
  getCatalogState = lift getCatalogState
  setCatalogState a b = lift $ setCatalogState a b

  getMetadataDbUid = lift getMetadataDbUid
  checkMetadataStorageHealth = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery = lift getScheduledEventsForDelivery
  insertCronEvents = lift . insertCronEvents
  insertOneOffScheduledEvent = lift . insertOneOffScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b c = lift $ getOneOffScheduledEvents a b c
  getCronEvents a b c d = lift $ getCronEvents a b c d
  getScheduledEventInvocations a = lift $ getScheduledEventInvocations a
  deleteScheduledEvent a b = lift $ deleteScheduledEvent a b

  insertAction a b c d = lift $ insertAction a b c d
  fetchUndeliveredActionEvents = lift fetchUndeliveredActionEvents
  setActionStatus a b = lift $ setActionStatus a b
  fetchActionResponse = lift . fetchActionResponse
  clearActionData = lift . clearActionData
  setProcessingActionLogsToPending = lift . setProcessingActionLogsToPending

instance (MonadMetadataStorage m) => MonadMetadataStorage (ExceptT e m) where
  fetchMetadataResourceVersion = lift fetchMetadataResourceVersion
  fetchMetadata = lift fetchMetadata
  fetchMetadataNotifications a b = lift $ fetchMetadataNotifications a b
  setMetadata r = lift . setMetadata r
  notifySchemaCacheSync a b c = lift $ notifySchemaCacheSync a b c
  getCatalogState = lift getCatalogState
  setCatalogState a b = lift $ setCatalogState a b

  getMetadataDbUid = lift getMetadataDbUid
  checkMetadataStorageHealth = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery = lift getScheduledEventsForDelivery
  insertCronEvents = lift . insertCronEvents
  insertOneOffScheduledEvent = lift . insertOneOffScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b c = lift $ getOneOffScheduledEvents a b c
  getCronEvents a b c d = lift $ getCronEvents a b c d
  getScheduledEventInvocations a = lift $ getScheduledEventInvocations a
  deleteScheduledEvent a b = lift $ deleteScheduledEvent a b

  insertAction a b c d = lift $ insertAction a b c d
  fetchUndeliveredActionEvents = lift fetchUndeliveredActionEvents
  setActionStatus a b = lift $ setActionStatus a b
  fetchActionResponse = lift . fetchActionResponse
  clearActionData = lift . clearActionData
  setProcessingActionLogsToPending = lift . setProcessingActionLogsToPending

instance (MonadMetadataStorage m) => MonadMetadataStorage (Tracing.TraceT m) where
  fetchMetadataResourceVersion = lift fetchMetadataResourceVersion
  fetchMetadata = lift fetchMetadata
  fetchMetadataNotifications a b = lift $ fetchMetadataNotifications a b
  setMetadata r = lift . setMetadata r
  notifySchemaCacheSync a b c = lift $ notifySchemaCacheSync a b c
  getCatalogState = lift getCatalogState
  setCatalogState a b = lift $ setCatalogState a b

  getMetadataDbUid = lift getMetadataDbUid
  checkMetadataStorageHealth = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery = lift getScheduledEventsForDelivery
  insertCronEvents = lift . insertCronEvents
  insertOneOffScheduledEvent = lift . insertOneOffScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b c = lift $ getOneOffScheduledEvents a b c
  getCronEvents a b c d = lift $ getCronEvents a b c d
  getScheduledEventInvocations a = lift $ getScheduledEventInvocations a
  deleteScheduledEvent a b = lift $ deleteScheduledEvent a b

  insertAction a b c d = lift $ insertAction a b c d
  fetchUndeliveredActionEvents = lift fetchUndeliveredActionEvents
  setActionStatus a b = lift $ setActionStatus a b
  fetchActionResponse = lift . fetchActionResponse
  clearActionData = lift . clearActionData
  setProcessingActionLogsToPending = lift . setProcessingActionLogsToPending

instance (MonadMetadataStorage m) => MonadMetadataStorage (MetadataT m) where
  fetchMetadataResourceVersion = lift fetchMetadataResourceVersion
  fetchMetadata = lift fetchMetadata
  fetchMetadataNotifications a b = lift $ fetchMetadataNotifications a b
  setMetadata r = lift . setMetadata r
  notifySchemaCacheSync a b c = lift $ notifySchemaCacheSync a b c
  getCatalogState = lift getCatalogState
  setCatalogState a b = lift $ setCatalogState a b

  getMetadataDbUid = lift getMetadataDbUid
  checkMetadataStorageHealth = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery = lift getScheduledEventsForDelivery
  insertCronEvents = lift . insertCronEvents
  insertOneOffScheduledEvent = lift . insertOneOffScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b c = lift $ getOneOffScheduledEvents a b c
  getCronEvents a b c d = lift $ getCronEvents a b c d
  getScheduledEventInvocations a = lift $ getScheduledEventInvocations a
  deleteScheduledEvent a b = lift $ deleteScheduledEvent a b

  insertAction a b c d = lift $ insertAction a b c d
  fetchUndeliveredActionEvents = lift fetchUndeliveredActionEvents
  setActionStatus a b = lift $ setActionStatus a b
  fetchActionResponse = lift . fetchActionResponse
  clearActionData = lift . clearActionData
  setProcessingActionLogsToPending = lift . setProcessingActionLogsToPending

instance (MonadMetadataStorage m) => MonadMetadataStorage (PG.TxET e m) where
  fetchMetadataResourceVersion = lift fetchMetadataResourceVersion
  fetchMetadata = lift fetchMetadata
  fetchMetadataNotifications a b = lift $ fetchMetadataNotifications a b
  setMetadata r = lift . setMetadata r
  notifySchemaCacheSync a b c = lift $ notifySchemaCacheSync a b c
  getCatalogState = lift getCatalogState
  setCatalogState a b = lift $ setCatalogState a b

  getMetadataDbUid = lift getMetadataDbUid
  checkMetadataStorageHealth = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery = lift getScheduledEventsForDelivery
  insertCronEvents = lift . insertCronEvents
  insertOneOffScheduledEvent = lift . insertOneOffScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b c = lift $ getOneOffScheduledEvents a b c
  getCronEvents a b c d = lift $ getCronEvents a b c d
  getScheduledEventInvocations a = lift $ getScheduledEventInvocations a
  deleteScheduledEvent a b = lift $ deleteScheduledEvent a b

  insertAction a b c d = lift $ insertAction a b c d
  fetchUndeliveredActionEvents = lift fetchUndeliveredActionEvents
  setActionStatus a b = lift $ setActionStatus a b
  fetchActionResponse = lift . fetchActionResponse
  clearActionData = lift . clearActionData
  setProcessingActionLogsToPending = lift . setProcessingActionLogsToPending

instance (MonadMetadataStorage m) => MonadMetadataStorage (ManagedT m) where
  fetchMetadataResourceVersion = lift fetchMetadataResourceVersion
  fetchMetadata = lift fetchMetadata
  fetchMetadataNotifications a b = lift $ fetchMetadataNotifications a b
  setMetadata r = lift . setMetadata r
  notifySchemaCacheSync a b c = lift $ notifySchemaCacheSync a b c
  getCatalogState = lift getCatalogState
  setCatalogState a b = lift $ setCatalogState a b

  getMetadataDbUid = lift getMetadataDbUid
  checkMetadataStorageHealth = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery = lift getScheduledEventsForDelivery
  insertCronEvents = lift . insertCronEvents
  insertOneOffScheduledEvent = lift . insertOneOffScheduledEvent
  insertScheduledEventInvocation a b = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b c = lift $ getOneOffScheduledEvents a b c
  getCronEvents a b c d = lift $ getCronEvents a b c d
  getScheduledEventInvocations a = lift $ getScheduledEventInvocations a
  deleteScheduledEvent a b = lift $ deleteScheduledEvent a b

  insertAction a b c d = lift $ insertAction a b c d
  fetchUndeliveredActionEvents = lift fetchUndeliveredActionEvents
  setActionStatus a b = lift $ setActionStatus a b
  fetchActionResponse = lift . fetchActionResponse
  clearActionData = lift . clearActionData
  setProcessingActionLogsToPending = lift . setProcessingActionLogsToPending

-- | Operations from @'MonadMetadataStorage' used in '/v1/query' and '/v1/metadata' APIs
class (MonadMetadataStorage m) => MonadMetadataStorageQueryAPI m where
  -- | Record a one-off event
  createOneOffScheduledEvent :: OneOffEvent -> m (Either QErr EventId)
  createOneOffScheduledEvent = insertOneOffScheduledEvent

  -- | Record a cron event
  createCronEvents :: [CronEventSeed] -> m (Either QErr ())
  createCronEvents = insertCronEvents

  -- | Clear cron events
  dropFutureCronEvents :: ClearCronEvents -> m (Either QErr ())
  dropFutureCronEvents = clearFutureCronEvents

  -- | Delete async action logs
  deleteActionData :: ActionName -> m (Either QErr ())
  deleteActionData = clearActionData

  -- | Fetch cron/oneoff scheduled event invocations
  fetchScheduledEventInvocations ::
    GetScheduledEventInvocations ->
    m (Either QErr (WithOptionalTotalCount [ScheduledEventInvocation]))
  fetchScheduledEventInvocations = getScheduledEventInvocations

  -- | Fetch cron/oneoff scheduled events
  fetchScheduledEvents :: GetScheduledEvents -> m (Either QErr Value)
  fetchScheduledEvents GetScheduledEvents {..} = do
    let totalCountToJSON WithOptionalTotalCount {..} =
          object $
            ("events" .= _wtcData) : (maybe mempty (\count -> ["count" .= count]) _wtcCount)
    case _gseScheduledEvent of
      SEOneOff -> (fmap . fmap) totalCountToJSON $ getOneOffScheduledEvents _gsePagination _gseStatus _gseGetRowsCount
      SECron name -> (fmap . fmap) totalCountToJSON $ getCronEvents name _gsePagination _gseStatus _gseGetRowsCount

  -- | Drop a cron/oneoff scheduled event
  dropEvent :: ScheduledEventId -> ScheduledEventType -> m (Either QErr ())
  dropEvent = deleteScheduledEvent

  -- | Retrieve the state from metadata storage catalog
  fetchCatalogState :: m (Either QErr CatalogState)
  fetchCatalogState = getCatalogState

  -- | Update the state from metadata storage catalog
  updateCatalogState :: CatalogStateType -> Value -> m (Either QErr ())
  updateCatalogState = setCatalogState

instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (ReaderT r m)

instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (StateT s m)

instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (ExceptT s m)

instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (Tracing.TraceT m)

instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (MetadataT m)

instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (PG.TxET QErr m)
