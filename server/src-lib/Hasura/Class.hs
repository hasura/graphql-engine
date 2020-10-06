module Hasura.Class
  ( SchemaSyncEventProcessResult(..)
  , SchemaSyncEventPayload(..)
  , MetadataStorageT(..)
  , runMetadataStorageT
  , MonadMetadataStorage(..)
  )
where

import           Hasura.Db
import           Hasura.Eventing.HTTP
import           Hasura.Eventing.ScheduledTrigger.Types
import           Hasura.GraphQL.Parser                  hiding (column)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Types
import           Hasura.Server.Utils
import           Hasura.Session
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.RQL.DML.Select.Internal         as RS

import           Control.Monad.Morph                    (MFunctor, hoist)
import           Data.Int                               (Int64)

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Time                              as UTC
import qualified Database.PG.Query                      as Q
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP
import qualified Text.Builder                           as TB (run)

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

  -- Async actions
  insertAction
    :: ActionName -> SessionVariables -> [HTTP.Header] -> J.Value
    -> MetadataStorageT m ActionId
  fetchUndeliveredActionEvents :: MetadataStorageT m [ActionLogItem]
  setActionStatus :: ActionId -> AsyncActionStatus -> MetadataStorageT m ()
  fetchActionResponse :: ActionId -> MetadataStorageT m ActionLogResponse

  -- Scheduled triggers
  getDeprivedCronTriggerStats :: MetadataStorageT m [CronTriggerStats]
  getPartialCronEvents :: MetadataStorageT m [CronEventPartial]
  getOneOffScheduledEvents :: MetadataStorageT m [OneOffScheduledEvent]
  insertCronEvents :: [CronEventSeed] -> MetadataStorageT m ()
  insertScheduledEventInvocation
    :: Invocation 'ScheduledType -> ScheduledEventType -> MetadataStorageT m ()
  setScheduledEventRetry
    :: ScheduledEventFull -> UTC.UTCTime -> ScheduledEventType -> MetadataStorageT m ()
  setScheduledEventStatus
    :: ScheduledEventId -> ScheduledEventStatus -> ScheduledEventType -> MetadataStorageT m ()
  unlockScheduledEvents
    :: ScheduledEventType -> [ScheduledEventId] -> MetadataStorageT m Int
  unlockAllLockedScheduledEvents :: MetadataStorageT m ()
  clearFutureCronEvents :: TriggerName -> MetadataStorageT m ()

instance (MonadMetadataStorage m) => MonadMetadataStorage (ReaderT r m) where

  getMetadata                       = (hoist lift) getMetadata
  setMetadata                       = (hoist lift) . setMetadata
  notifySchemaCacheSync a b         = (hoist lift) $ notifySchemaCacheSync a b
  processSchemaSyncEventPayload a b = (hoist lift) $ processSchemaSyncEventPayload a b

  insertAction a b c d = (hoist lift) $ insertAction a b c d
  fetchUndeliveredActionEvents = (hoist lift) fetchUndeliveredActionEvents
  setActionStatus a b = (hoist lift) $ setActionStatus a b
  fetchActionResponse = (hoist lift) . fetchActionResponse

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getPartialCronEvents               = (hoist lift) getPartialCronEvents
  getOneOffScheduledEvents           = (hoist lift) getOneOffScheduledEvents
  insertCronEvents                   = (hoist lift) . insertCronEvents
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventRetry a b c       = (hoist lift) $ setScheduledEventRetry a b c
  setScheduledEventStatus a b c      = (hoist lift) $ setScheduledEventStatus a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
  clearFutureCronEvents              = (hoist lift) . clearFutureCronEvents

instance (MonadMetadataStorage m) => MonadMetadataStorage (ExceptT e m) where

  getMetadata                       = (hoist lift) getMetadata
  setMetadata                       = (hoist lift) . setMetadata
  notifySchemaCacheSync a b         = (hoist lift) $ notifySchemaCacheSync a b
  processSchemaSyncEventPayload a b = (hoist lift) $ processSchemaSyncEventPayload a b

  insertAction a b c d = (hoist lift) $ insertAction a b c d
  fetchUndeliveredActionEvents = (hoist lift) fetchUndeliveredActionEvents
  setActionStatus a b = (hoist lift) $ setActionStatus a b
  fetchActionResponse = (hoist lift) . fetchActionResponse

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getPartialCronEvents               = (hoist lift) getPartialCronEvents
  getOneOffScheduledEvents           = (hoist lift) getOneOffScheduledEvents
  insertCronEvents                   = (hoist lift) . insertCronEvents
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventRetry a b c       = (hoist lift) $ setScheduledEventRetry a b c
  setScheduledEventStatus a b c      = (hoist lift) $ setScheduledEventStatus a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
  clearFutureCronEvents              = (hoist lift) . clearFutureCronEvents

instance (MonadMetadataStorage m) => MonadMetadataStorage (Tracing.TraceT m) where

  getMetadata                       = (hoist lift) getMetadata
  setMetadata                       = (hoist lift) . setMetadata
  notifySchemaCacheSync a b         = (hoist lift) $ notifySchemaCacheSync a b
  processSchemaSyncEventPayload a b = (hoist lift) $ processSchemaSyncEventPayload a b

  insertAction a b c d = (hoist lift) $ insertAction a b c d
  fetchUndeliveredActionEvents = (hoist lift) fetchUndeliveredActionEvents
  setActionStatus a b = (hoist lift) $ setActionStatus a b
  fetchActionResponse = (hoist lift) . fetchActionResponse

  getDeprivedCronTriggerStats        = (hoist lift) getDeprivedCronTriggerStats
  getPartialCronEvents               = (hoist lift) getPartialCronEvents
  getOneOffScheduledEvents           = (hoist lift) getOneOffScheduledEvents
  insertCronEvents                   = (hoist lift) . insertCronEvents
  insertScheduledEventInvocation a b = (hoist lift) $ insertScheduledEventInvocation a b
  setScheduledEventRetry a b c       = (hoist lift) $ setScheduledEventRetry a b c
  setScheduledEventStatus a b c      = (hoist lift) $ setScheduledEventStatus a b c
  unlockScheduledEvents a b          = (hoist lift) $ unlockScheduledEvents a b
  unlockAllLockedScheduledEvents     = (hoist lift) unlockAllLockedScheduledEvents
  clearFutureCronEvents              = (hoist lift) . clearFutureCronEvents
