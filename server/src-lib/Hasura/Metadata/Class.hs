-- | This module has type class and types which implements the Metadata Storage Abstraction
module Hasura.Metadata.Class
  ( SchemaSyncEventProcessResult (..),
    MonadMetadataStorage (..),
    MonadEECredentialsStorage (..),
    createOneOffScheduledEvent,
    createCronEvents,
    dropFutureCronEvents,
    deleteActionData,
    fetchScheduledEventInvocations,
    fetchScheduledEvents,
    dropEvent,
    fetchCatalogState,
    updateCatalogState,
  )
where

import Control.Monad.Trans.Extended
import Control.Monad.Trans.Managed
import Data.Aeson
import Hasura.Base.Error
import Hasura.Eventing.ScheduledTrigger.Types
import Hasura.Prelude
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.EECredentials
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing.Monad (TraceT)
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
-- like MSSQL etc.
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
class (Monad m) => MonadMetadataStorage m where
  -- Metadata
  fetchMetadataResourceVersion :: m (Either QErr MetadataResourceVersion)
  fetchMetadata :: m (Either QErr MetadataWithResourceVersion)
  fetchMetadataNotifications :: MetadataResourceVersion -> InstanceId -> m (Either QErr [(MetadataResourceVersion, CacheInvalidations)])
  setMetadata :: MetadataResourceVersion -> Metadata -> m (Either QErr MetadataResourceVersion)
  notifySchemaCacheSync :: MetadataResourceVersion -> InstanceId -> CacheInvalidations -> m (Either QErr ())
  getCatalogState :: m (Either QErr CatalogState)

  -- the `setCatalogState` function is used by the console and CLI to store its state
  -- it is disabled when maintenance mode is on
  setCatalogState :: CatalogStateType -> Value -> m (Either QErr ())

  -- methods for storing and retrieving source introspection which is stored in
  -- "stored introspection" db. See 'StoredIntrospection'.
  -- This returns a @Maybe StoredIntrospection@ as in some distributions
  -- fetching of source introspection may not be available
  fetchSourceIntrospection :: MetadataResourceVersion -> m (Either QErr (Maybe StoredIntrospection))
  storeSourceIntrospection :: StoredIntrospection -> MetadataResourceVersion -> m (Either QErr ())

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
  getScheduledEventsForDelivery :: [TriggerName] -> m (Either QErr ([CronEvent], [OneOffScheduledEvent]))
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
  fetchUndeliveredActionEvents :: Int -> m (Either QErr [ActionLogItem])
  setActionStatus :: ActionId -> AsyncActionStatus -> m (Either QErr ())
  fetchActionResponse :: ActionId -> m (Either QErr ActionLogResponse)
  clearActionData :: ActionName -> m (Either QErr ())
  setProcessingActionLogsToPending :: LockedActionIdArray -> m (Either QErr ())

instance (MonadMetadataStorage m, MonadTrans t, Monad (t m)) => MonadMetadataStorage (TransT t m) where
  fetchMetadataResourceVersion = lift fetchMetadataResourceVersion
  fetchMetadata = lift fetchMetadata
  fetchMetadataNotifications a b = lift $ fetchMetadataNotifications a b
  setMetadata r = lift . setMetadata r
  notifySchemaCacheSync a b c = lift $ notifySchemaCacheSync a b c
  getCatalogState = lift getCatalogState
  setCatalogState a b = lift $ setCatalogState a b

  fetchSourceIntrospection = lift . fetchSourceIntrospection
  storeSourceIntrospection a b = lift $ storeSourceIntrospection a b

  getMetadataDbUid = lift getMetadataDbUid
  checkMetadataStorageHealth = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery = lift . getScheduledEventsForDelivery
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
  fetchUndeliveredActionEvents a = lift $ fetchUndeliveredActionEvents a
  setActionStatus a b = lift $ setActionStatus a b
  fetchActionResponse = lift . fetchActionResponse
  clearActionData = lift . clearActionData
  setProcessingActionLogsToPending = lift . setProcessingActionLogsToPending

deriving via (TransT (ReaderT r) m) instance (MonadMetadataStorage m) => MonadMetadataStorage (ReaderT r m)

deriving via (TransT (StateT s) m) instance (MonadMetadataStorage m) => MonadMetadataStorage (StateT s m)

deriving via (TransT (ExceptT e) m) instance (MonadMetadataStorage m) => MonadMetadataStorage (ExceptT e m)

deriving via (TransT MetadataT m) instance (MonadMetadataStorage m) => MonadMetadataStorage (MetadataT m)

deriving via (TransT ManagedT m) instance (MonadMetadataStorage m) => MonadMetadataStorage (ManagedT m)

deriving via (TransT TraceT m) instance (MonadMetadataStorage m) => MonadMetadataStorage (TraceT m)

-- | Record a one-off event
createOneOffScheduledEvent :: (MonadMetadataStorage m) => OneOffEvent -> m (Either QErr EventId)
createOneOffScheduledEvent = insertOneOffScheduledEvent

-- | Record a cron event
createCronEvents :: (MonadMetadataStorage m) => [CronEventSeed] -> m (Either QErr ())
createCronEvents = insertCronEvents

-- | Clear cron events
dropFutureCronEvents :: (MonadMetadataStorage m) => ClearCronEvents -> m (Either QErr ())
dropFutureCronEvents = clearFutureCronEvents

-- | Delete async action logs
deleteActionData :: (MonadMetadataStorage m) => ActionName -> m (Either QErr ())
deleteActionData = clearActionData

-- | Fetch cron/oneoff scheduled event invocations
fetchScheduledEventInvocations ::
  (MonadMetadataStorage m) =>
  GetScheduledEventInvocations ->
  m (Either QErr (WithOptionalTotalCount [ScheduledEventInvocation]))
fetchScheduledEventInvocations = getScheduledEventInvocations

-- | Fetch cron/oneoff scheduled events
fetchScheduledEvents :: (MonadMetadataStorage m) => GetScheduledEvents -> m (Either QErr Value)
fetchScheduledEvents GetScheduledEvents {..} = do
  let totalCountToJSON WithOptionalTotalCount {..} =
        object
          $ ("events" .= _wtcData)
          : (maybe mempty (\count -> ["count" .= count]) _wtcCount)
  case _gseScheduledEvent of
    SEOneOff -> (fmap . fmap) totalCountToJSON $ getOneOffScheduledEvents _gsePagination _gseStatus _gseGetRowsCount
    SECron name -> (fmap . fmap) totalCountToJSON $ getCronEvents name _gsePagination _gseStatus _gseGetRowsCount

-- | Drop a cron/oneoff scheduled event
dropEvent :: (MonadMetadataStorage m) => ScheduledEventId -> ScheduledEventType -> m (Either QErr ())
dropEvent = deleteScheduledEvent

-- | Retrieve the state from metadata storage catalog
fetchCatalogState :: (MonadMetadataStorage m) => m (Either QErr CatalogState)
fetchCatalogState = getCatalogState

-- | Update the state from metadata storage catalog
updateCatalogState :: (MonadMetadataStorage m) => CatalogStateType -> Value -> m (Either QErr ())
updateCatalogState = setCatalogState

-- | Metadata database operations for EE credentials storage.
--
-- This class is only necessary because we haven't written an implementation
-- for storing EE credentials in Cloud.
class (Monad m) => MonadEECredentialsStorage m where
  getEEClientCredentials :: m (Either QErr (Maybe EEClientCredentials))
  setEEClientCredentials :: EEClientCredentials -> m (Either QErr ())

instance (MonadEECredentialsStorage m, MonadTrans t, Monad (t m)) => MonadEECredentialsStorage (TransT t m) where
  getEEClientCredentials = lift getEEClientCredentials
  setEEClientCredentials a = lift $ setEEClientCredentials a

deriving via (TransT (ReaderT r) m) instance (MonadEECredentialsStorage m) => MonadEECredentialsStorage (ReaderT r m)

deriving via (TransT (StateT s) m) instance (MonadEECredentialsStorage m) => MonadEECredentialsStorage (StateT s m)

deriving via (TransT (ExceptT e) m) instance (MonadEECredentialsStorage m) => MonadEECredentialsStorage (ExceptT e m)

deriving via (TransT MetadataT m) instance (MonadEECredentialsStorage m) => MonadEECredentialsStorage (MetadataT m)

deriving via (TransT ManagedT m) instance (MonadEECredentialsStorage m) => MonadEECredentialsStorage (ManagedT m)
