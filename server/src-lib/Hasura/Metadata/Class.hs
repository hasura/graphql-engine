-- | This module has type class and types which implements the Metadata Storage Abstraction
{-# LANGUAGE UndecidableInstances #-}
module Hasura.Metadata.Class
  ( SchemaSyncEventProcessResult(..)
  , MetadataStorageT(..)
  , runMetadataStorageT
  , MonadMetadataStorage(..)
  , MonadMetadataStorageQueryAPI(..)
  )
where

import           Hasura.Prelude

import qualified Network.HTTP.Types                     as HTTP

import           Control.Monad.Morph                    (MFunctor, hoist)
import           Control.Monad.Trans.Control            (MonadBaseControl)
import           Data.Aeson
import           Network.HTTP.Client.Extended

import qualified Hasura.Tracing                         as Tracing

import           Hasura.Base.Error
import           Hasura.Eventing.HTTP
import           Hasura.Eventing.ScheduledTrigger.Types
import           Hasura.RQL.Types
import           Hasura.Server.Types
import           Hasura.Session


data SchemaSyncEventProcessResult
  = SchemaSyncEventProcessResult
  { _sseprShouldReload       :: !Bool
  , _sseprCacheInvalidations :: !CacheInvalidations
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

class (MonadError QErr m) => MonadMetadataStorage m where

  -- Metadata
  fetchMetadataResourceVersion :: m MetadataResourceVersion
  fetchMetadata :: m (Metadata, MetadataResourceVersion)
  fetchMetadataNotifications ::  MetadataResourceVersion -> InstanceId -> m [(MetadataResourceVersion, CacheInvalidations)]
  setMetadata :: MetadataResourceVersion -> Metadata -> m MetadataResourceVersion
  notifySchemaCacheSync :: MetadataResourceVersion -> InstanceId -> CacheInvalidations -> m ()
  getCatalogState :: m CatalogState
  -- the `setCatalogState` function is used by the console and CLI to store its state
  -- it is disabled when maintenance mode is on
  setCatalogState :: CatalogStateType -> Value -> m ()

  -- get the @db_uuid@ that we store in the database.
  getDatabaseUid :: m Text
  checkMetadataStorageHealth  :: m Bool

  -- Scheduled triggers
  -- TODO:-
  -- Ideally we would've liked to avoid having functions that are specific to
  -- scheduled/cron triggers and instead have functions that provide a generic
  -- 'event storage and retrieval' interface but we'll have to change a lot of
  -- existing code for scheduled and cron triggers. We can get to this after the
  -- multi-source work is done. See Note [Todo: Common interface for eventing sub-system]
  getDeprivedCronTriggerStats :: [TriggerName] -> m [CronTriggerStats]
  getScheduledEventsForDelivery :: m ([CronEvent], [OneOffScheduledEvent])
  insertScheduledEvent :: ScheduledEventSeed -> m ()
  insertScheduledEventInvocation :: Invocation 'ScheduledType -> ScheduledEventType -> m ()
  setScheduledEventOp :: ScheduledEventId -> ScheduledEventOp -> ScheduledEventType -> m ()
  unlockScheduledEvents :: ScheduledEventType -> [ScheduledEventId] -> m Int
  unlockAllLockedScheduledEvents :: m ()
  clearFutureCronEvents :: ClearCronEvents -> m ()
  -- Console API requirements
  getOneOffScheduledEvents :: ScheduledEventPagination -> [ScheduledEventStatus] -> m (WithTotalCount [OneOffScheduledEvent])
  getCronEvents :: TriggerName -> ScheduledEventPagination -> [ScheduledEventStatus] -> m (WithTotalCount [CronEvent])
  getInvocations :: GetInvocationsBy -> ScheduledEventPagination -> m (WithTotalCount [ScheduledEventInvocation])
  deleteScheduledEvent :: ScheduledEventId -> ScheduledEventType -> m ()

  -- Async actions
  insertAction
    :: ActionName -> SessionVariables -> [HTTP.Header] -> Value
    -> m ActionId
  fetchUndeliveredActionEvents :: m [ActionLogItem]
  setActionStatus :: ActionId -> AsyncActionStatus -> m ()
  fetchActionResponse :: ActionId -> m ActionLogResponse
  clearActionData :: ActionName -> m ()
  setProcessingActionLogsToPending :: LockedActionIdArray -> m ()

instance (MonadMetadataStorage m) => MonadMetadataStorage (ReaderT r m) where
  fetchMetadataResourceVersion         = lift fetchMetadataResourceVersion
  fetchMetadata                        = lift fetchMetadata
  fetchMetadataNotifications a b       = lift $ fetchMetadataNotifications a b
  setMetadata r                        = lift . setMetadata r
  notifySchemaCacheSync a b c          = lift $ notifySchemaCacheSync a b c
  getCatalogState                      = lift getCatalogState
  setCatalogState a b                  = lift $ setCatalogState a b

  getDatabaseUid                       = lift getDatabaseUid
  checkMetadataStorageHealth           = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats          = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery        = lift getScheduledEventsForDelivery
  insertScheduledEvent                 = lift . insertScheduledEvent
  insertScheduledEventInvocation a b   = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c            = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b            = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents       = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents                = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b         = lift $ getOneOffScheduledEvents a b
  getCronEvents a b c                  = lift $ getCronEvents a b c
  getInvocations a b                   = lift $ getInvocations a b
  deleteScheduledEvent a b             = lift $ deleteScheduledEvent a b


  insertAction a b c d                 = lift $ insertAction a b c d
  fetchUndeliveredActionEvents         = lift fetchUndeliveredActionEvents
  setActionStatus a b                  = lift $ setActionStatus a b
  fetchActionResponse                  = lift . fetchActionResponse
  clearActionData                      = lift . clearActionData
  setProcessingActionLogsToPending     = lift . setProcessingActionLogsToPending


instance (MonadMetadataStorage m) => MonadMetadataStorage (StateT s m) where
  fetchMetadataResourceVersion         = lift fetchMetadataResourceVersion
  fetchMetadata                        = lift fetchMetadata
  fetchMetadataNotifications a b       = lift $ fetchMetadataNotifications a b
  setMetadata r                        = lift . setMetadata r
  notifySchemaCacheSync a b c          = lift $ notifySchemaCacheSync a b c
  getCatalogState                      = lift getCatalogState
  setCatalogState a b                  = lift $ setCatalogState a b

  getDatabaseUid                       = lift getDatabaseUid
  checkMetadataStorageHealth           = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats          = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery        = lift getScheduledEventsForDelivery
  insertScheduledEvent                 = lift . insertScheduledEvent
  insertScheduledEventInvocation a b   = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c            = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b            = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents       = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents                = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b         = lift $ getOneOffScheduledEvents a b
  getCronEvents a b c                  = lift $ getCronEvents a b c
  getInvocations a b                   = lift $ getInvocations a b
  deleteScheduledEvent a b             = lift $ deleteScheduledEvent a b


  insertAction a b c d                 = lift $ insertAction a b c d
  fetchUndeliveredActionEvents         = lift fetchUndeliveredActionEvents
  setActionStatus a b                  = lift $ setActionStatus a b
  fetchActionResponse                  = lift . fetchActionResponse
  clearActionData                      = lift . clearActionData
  setProcessingActionLogsToPending     = lift . setProcessingActionLogsToPending


instance (MonadMetadataStorage m) => MonadMetadataStorage (Tracing.TraceT m) where
  fetchMetadataResourceVersion         = lift fetchMetadataResourceVersion
  fetchMetadata                        = lift fetchMetadata
  fetchMetadataNotifications a b       = lift $ fetchMetadataNotifications a b
  setMetadata r                        = lift . setMetadata r
  notifySchemaCacheSync a b c          = lift $ notifySchemaCacheSync a b c
  getCatalogState                      = lift getCatalogState
  setCatalogState a b                  = lift $ setCatalogState a b

  getDatabaseUid                       = lift getDatabaseUid
  checkMetadataStorageHealth           = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats          = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery        = lift getScheduledEventsForDelivery
  insertScheduledEvent                 = lift . insertScheduledEvent
  insertScheduledEventInvocation a b   = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c            = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b            = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents       = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents                = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b         = lift $ getOneOffScheduledEvents a b
  getCronEvents a b c                  = lift $ getCronEvents a b c
  getInvocations a b                   = lift $ getInvocations a b
  deleteScheduledEvent a b             = lift $ deleteScheduledEvent a b


  insertAction a b c d                 = lift $ insertAction a b c d
  fetchUndeliveredActionEvents         = lift fetchUndeliveredActionEvents
  setActionStatus a b                  = lift $ setActionStatus a b
  fetchActionResponse                  = lift . fetchActionResponse
  clearActionData                      = lift . clearActionData
  setProcessingActionLogsToPending     = lift . setProcessingActionLogsToPending

instance (MonadMetadataStorage m) => MonadMetadataStorage (ExceptT QErr m) where
  fetchMetadataResourceVersion         = lift fetchMetadataResourceVersion
  fetchMetadata                        = lift fetchMetadata
  fetchMetadataNotifications a b       = lift $ fetchMetadataNotifications a b
  setMetadata r                        = lift . setMetadata r
  notifySchemaCacheSync a b c          = lift $ notifySchemaCacheSync a b c
  getCatalogState                      = lift getCatalogState
  setCatalogState a b                  = lift $ setCatalogState a b

  getDatabaseUid                       = lift getDatabaseUid
  checkMetadataStorageHealth           = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats          = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery        = lift getScheduledEventsForDelivery
  insertScheduledEvent                 = lift . insertScheduledEvent
  insertScheduledEventInvocation a b   = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c            = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b            = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents       = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents                = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b         = lift $ getOneOffScheduledEvents a b
  getCronEvents a b c                  = lift $ getCronEvents a b c
  getInvocations a b                   = lift $ getInvocations a b
  deleteScheduledEvent a b             = lift $ deleteScheduledEvent a b


  insertAction a b c d                 = lift $ insertAction a b c d
  fetchUndeliveredActionEvents         = lift fetchUndeliveredActionEvents
  setActionStatus a b                  = lift $ setActionStatus a b
  fetchActionResponse                  = lift . fetchActionResponse
  clearActionData                      = lift . clearActionData
  setProcessingActionLogsToPending     = lift . setProcessingActionLogsToPending

instance (MonadMetadataStorage m) => MonadMetadataStorage (MetadataT m) where
  fetchMetadataResourceVersion         = lift fetchMetadataResourceVersion
  fetchMetadata                        = lift fetchMetadata
  fetchMetadataNotifications a b       = lift $ fetchMetadataNotifications a b
  setMetadata r                        = lift . setMetadata r
  notifySchemaCacheSync a b c          = lift $ notifySchemaCacheSync a b c
  getCatalogState                      = lift getCatalogState
  setCatalogState a b                  = lift $ setCatalogState a b

  getDatabaseUid                       = lift getDatabaseUid
  checkMetadataStorageHealth           = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats          = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery        = lift getScheduledEventsForDelivery
  insertScheduledEvent                 = lift . insertScheduledEvent
  insertScheduledEventInvocation a b   = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c            = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b            = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents       = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents                = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b         = lift $ getOneOffScheduledEvents a b
  getCronEvents a b c                  = lift $ getCronEvents a b c
  getInvocations a b                   = lift $ getInvocations a b
  deleteScheduledEvent a b             = lift $ deleteScheduledEvent a b


  insertAction a b c d                 = lift $ insertAction a b c d
  fetchUndeliveredActionEvents         = lift fetchUndeliveredActionEvents
  setActionStatus a b                  = lift $ setActionStatus a b
  fetchActionResponse                  = lift . fetchActionResponse
  clearActionData                      = lift . clearActionData
  setProcessingActionLogsToPending     = lift . setProcessingActionLogsToPending

instance (MonadMetadataStorage m) => MonadMetadataStorage (LazyTxT QErr m) where
  fetchMetadataResourceVersion         = lift fetchMetadataResourceVersion
  fetchMetadata                        = lift fetchMetadata
  fetchMetadataNotifications a b       = lift $ fetchMetadataNotifications a b
  setMetadata r                        = lift . setMetadata r
  notifySchemaCacheSync a b c          = lift $ notifySchemaCacheSync a b c
  getCatalogState                      = lift getCatalogState
  setCatalogState a b                  = lift $ setCatalogState a b

  getDatabaseUid                       = lift getDatabaseUid
  checkMetadataStorageHealth           = lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats          = lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery        = lift getScheduledEventsForDelivery
  insertScheduledEvent                 = lift . insertScheduledEvent
  insertScheduledEventInvocation a b   = lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c            = lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b            = lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents       = lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents                = lift . clearFutureCronEvents
  getOneOffScheduledEvents a b         = lift $ getOneOffScheduledEvents a b
  getCronEvents a b c                  = lift $ getCronEvents a b c
  getInvocations a b                   = lift $ getInvocations a b
  deleteScheduledEvent a b             = lift $ deleteScheduledEvent a b


  insertAction a b c d                 = lift $ insertAction a b c d
  fetchUndeliveredActionEvents         = lift fetchUndeliveredActionEvents
  setActionStatus a b                  = lift $ setActionStatus a b
  fetchActionResponse                  = lift . fetchActionResponse
  clearActionData                      = lift . clearActionData
  setProcessingActionLogsToPending     = lift . setProcessingActionLogsToPending

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
           , MonadReader r
           , MonadState s
           , MonadTrans
           , MonadIO
           , MFunctor
           , Tracing.HasReporter
           , Tracing.MonadTrace
           , MonadResolveSource
           , HasHttpManagerM
           , HasServerConfigCtx
           )

deriving instance (MonadBase IO m) => MonadBase IO (MetadataStorageT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (MetadataStorageT m)

runMetadataStorageT
  :: MetadataStorageT m a -> m (Either QErr a)
runMetadataStorageT =
  runExceptT . unMetadataStorageT

instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, MonadMetadataStorage (MetadataStorageT m))
  => MonadMetadataStorage (MetadataStorageT (t m)) where

  fetchMetadataResourceVersion      = hoist lift fetchMetadataResourceVersion
  fetchMetadata                     = hoist lift fetchMetadata
  fetchMetadataNotifications a b    = hoist lift $ fetchMetadataNotifications a b
  setMetadata r                     = hoist lift . setMetadata r
  notifySchemaCacheSync a b c       = hoist lift $ notifySchemaCacheSync a b c
  getCatalogState                   = hoist lift getCatalogState
  setCatalogState a b               = hoist lift $ setCatalogState a b

  getDatabaseUid = hoist lift getDatabaseUid
  checkMetadataStorageHealth = hoist lift checkMetadataStorageHealth

  getDeprivedCronTriggerStats        = hoist lift . getDeprivedCronTriggerStats
  getScheduledEventsForDelivery      = hoist lift getScheduledEventsForDelivery
  insertScheduledEvent               = hoist lift . insertScheduledEvent
  insertScheduledEventInvocation a b = hoist lift $ insertScheduledEventInvocation a b
  setScheduledEventOp a b c          = hoist lift $ setScheduledEventOp a b c
  unlockScheduledEvents a b          = hoist lift $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = hoist lift $ unlockAllLockedScheduledEvents
  clearFutureCronEvents              = hoist lift . clearFutureCronEvents
  getOneOffScheduledEvents a b       = hoist lift $ getOneOffScheduledEvents a b
  getCronEvents a b c                = hoist lift $ getCronEvents a b c
  getInvocations a b                 = hoist lift $ getInvocations a b
  deleteScheduledEvent a b           = hoist lift $ deleteScheduledEvent a b

  insertAction a b c d               = hoist lift $ insertAction a b c d
  fetchUndeliveredActionEvents       = hoist lift fetchUndeliveredActionEvents
  setActionStatus a b                = hoist lift $ setActionStatus a b
  fetchActionResponse                = hoist lift . fetchActionResponse
  clearActionData                    = hoist lift . clearActionData
  setProcessingActionLogsToPending     = hoist lift . setProcessingActionLogsToPending

-- | Operations from @'MonadMetadataStorage' used in '/v1/query' and '/v1/metadata' APIs
class (MonadMetadataStorage m) => MonadMetadataStorageQueryAPI m where
  -- | Record a cron/one-off event
  createScheduledEvent :: ScheduledEventSeed -> m ()
  createScheduledEvent = insertScheduledEvent

  -- | Clear cron events
  dropFutureCronEvents :: ClearCronEvents -> m ()
  dropFutureCronEvents = clearFutureCronEvents

  -- | Delete async action logs
  deleteActionData :: ActionName -> m ()
  deleteActionData = clearActionData

  -- | Fetch cron/oneoff scheduled event invocations
  fetchInvocations
    :: GetInvocationsBy
    -> ScheduledEventPagination
    -> m (WithTotalCount [ScheduledEventInvocation])
  fetchInvocations = getInvocations

  -- | Fetch cron/oneoff scheduled events
  fetchScheduledEvents :: GetScheduledEvents -> m Value
  fetchScheduledEvents GetScheduledEvents{..} = do
    let totalCountToJSON WithTotalCount{..} =
          object ["count" .= _wtcCount, "events" .= _wtcData]
    case _gseScheduledEvent of
      SEOneOff    -> totalCountToJSON <$> getOneOffScheduledEvents _gsePagination _gseStatus
      SECron name -> totalCountToJSON <$> getCronEvents name _gsePagination _gseStatus

  -- | Drop a cron/oneoff scheduled event
  dropEvent :: ScheduledEventId -> ScheduledEventType -> m ()
  dropEvent = deleteScheduledEvent

  -- | Retrieve the state from metadata storage catalog
  fetchCatalogState :: m CatalogState
  fetchCatalogState = getCatalogState

  -- | Update the state from metadata storage catalog
  updateCatalogState :: CatalogStateType -> Value -> m ()
  updateCatalogState = setCatalogState

instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (ReaderT r m)
instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (StateT s m)
instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (Tracing.TraceT m)
instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (MetadataT m)
instance (MonadMetadataStorageQueryAPI m) => MonadMetadataStorageQueryAPI (LazyTxT QErr m)
