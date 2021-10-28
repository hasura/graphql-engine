module Hasura.RQL.Types.Eventing.Backend where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Set qualified as Set
import Data.Time.Clock qualified as Time
import Hasura.Backends.Postgres.DDL.EventTrigger qualified as PG
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column (ColumnInfo)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Source
import Hasura.SQL.Backend
import Hasura.Server.Types
import Hasura.Session (UserInfo)
import Hasura.Tracing qualified as Tracing

-- | The @BackendEventTrigger@ type class contains functions which
--   interact with the source database to perform event trigger related
--   operations like fetching pending events from the database or
--   inserting a new invocation log after processing an event.
class Backend b => BackendEventTrigger (b :: BackendType) where
  -- | insertManualEvent inserts the specified event
  --   in the event log table, note that this method should
  --   also set the trace context and session variables in the
  --   source database context (if available)
  insertManualEvent ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    TableName b ->
    TriggerName ->
    Value ->
    UserInfo ->
    Tracing.TraceContext ->
    m EventId

  -- | @fetchUndeliveredEvents@ fetches the undelivered events
  --   from the source and locks the events which are fetched for
  --   processing. The locking is done so that when there are multiple
  --   instances of graphql-engine connected to the same source they don't
  --   end up processing the same events concurrently. Also, it's crucial
  --   that the SQL query used to fetch events in this function uses something like Postgres's
  --   `FOR UPDATE SKIP LOCKED` mechanism so that it skips past the events which are locked by the database and pick
  --   newer undelivered events to achieve maximum throughput.
  --
  --   The locking mechanism for event triggers is timestamp based i.e. when an event
  --   is fetched from the database, the `locked` column will contain the timestamp of when
  --   it was fetched from the database. Undelivered events will have `NULL` value as their `locked` column value.
  --   The idea behind having a timestamp based locking mechanism is that if the graphql-engine is shutdown
  --   abruptly with events being fetched by the events processor, it will be locked and after the shutdown it will
  --   remain locked. Now with a timestamp based lock, when the graphql-engine is started again it will also fetch
  --   events which have a `locked` value of older than 30 mins along with the undelivered events. So, this way
  --   no events remain in a `locked` state.
  fetchUndeliveredEvents ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    SourceName ->
    MaintenanceMode ->
    FetchBatchSize ->
    m [Event b]

  -- | Ad-hoc function to set a retry for an undelivered event
  setRetry ::
    ( MonadIO m,
      MonadError QErr m
    ) =>
    SourceConfig b ->
    Event b ->
    Time.UTCTime ->
    Maybe MaintenanceModeVersion ->
    m ()

  -- | @getMaintenanceModeVersion@ gets the source catalog version from the
  --   source
  getMaintenanceModeVersion ::
    ( MonadIO m,
      MonadError QErr m
    ) =>
    SourceConfig b ->
    m MaintenanceModeVersion

  -- | @recordSuccess@ records a successful event invocation, it does a couple
  --   of things,
  --
  --   1. Insert the invocation in the invocation logs table
  --   2. Mark the event as 'delivered' in the event_log table
  recordSuccess ::
    MonadIO m =>
    SourceConfig b ->
    Event b ->
    Invocation 'EventType ->
    Maybe MaintenanceModeVersion ->
    m (Either QErr ())

  -- | @recordError@ records an erronous event invocation, it does a couple
  --   of things,
  --
  --   1. Insert the invocation in the invocation logs table
  --   2. Depending on the value of `ProcessEventError`, it will either,
  --        - Set a retry for the given event
  --        - Mark the event as 'error'
  recordError ::
    MonadIO m =>
    SourceConfig b ->
    Event b ->
    Invocation 'EventType ->
    ProcessEventError ->
    Maybe MaintenanceModeVersion ->
    m (Either QErr ())

  -- | @recordError'@ records an erronous event invocation, it does a couple
  --   of things,
  --
  --   1. If present, insert the invocation in the invocation logs table
  --   2. Depending on the value of `ProcessEventError`, it will either,
  --        - Set a retry for the given event
  --        - Mark the event as 'error'
  recordError' ::
    MonadIO m =>
    SourceConfig b ->
    Event b ->
    Maybe (Invocation 'EventType) ->
    ProcessEventError ->
    Maybe MaintenanceModeVersion ->
    m (Either QErr ())

  -- | @dropTriggerAndArchiveEvents@ drops the database trigger and
  --   marks all the events related to the event trigger as archived.
  --   See Note [Cleanup for dropped triggers]
  dropTriggerAndArchiveEvents ::
    ( MonadIO m,
      MonadError QErr m
    ) =>
    SourceConfig b ->
    TriggerName ->
    m ()

  redeliverEvent ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    EventId ->
    m ()

  -- | @unlockEventsInSource@ unlocks the cached locked events captured
  --   by the events when a graceful shutdown is initiated, so that when
  --   the graphql-engine is started up again these events can be fetched
  --   to process them immediately.
  unlockEventsInSource ::
    MonadIO m =>
    SourceConfig b ->
    Set.Set EventId ->
    m (Either QErr Int)

  createTableEventTrigger ::
    (MonadBaseControl IO m, MonadIO m) =>
    ServerConfigCtx ->
    SourceConfig b ->
    TableName b ->
    [ColumnInfo b] ->
    TriggerName ->
    TriggerOpsDef b ->
    m (Either QErr ())

instance BackendEventTrigger ('Postgres 'Vanilla) where
  insertManualEvent = PG.insertManualEvent
  fetchUndeliveredEvents = PG.fetchUndeliveredEvents
  setRetry = PG.setRetry
  getMaintenanceModeVersion = PG.getMaintenanceModeVersion
  recordSuccess = PG.recordSuccess
  recordError = PG.recordError
  recordError' = PG.recordError'
  dropTriggerAndArchiveEvents = PG.dropTriggerAndArchiveEvents
  redeliverEvent = PG.redeliverEvent
  unlockEventsInSource = PG.unlockEventsInSource
  createTableEventTrigger = PG.createTableEventTrigger

instance BackendEventTrigger ('Postgres 'Citus) where
  insertManualEvent _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  fetchUndeliveredEvents _ _ _ _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  setRetry _ _ _ _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  recordSuccess _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"
  getMaintenanceModeVersion _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  recordError _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"
  recordError' _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"
  dropTriggerAndArchiveEvents _ _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  unlockEventsInSource _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"
  createTableEventTrigger _ _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"

instance BackendEventTrigger 'MSSQL where
  insertManualEvent _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for MS-SQL sources"
  fetchUndeliveredEvents _ _ _ _ = throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  setRetry _ _ _ _ = throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  recordSuccess _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  getMaintenanceModeVersion _ = throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  recordError _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  recordError' _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  dropTriggerAndArchiveEvents _ _ = throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  unlockEventsInSource _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  createTableEventTrigger _ _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MS-SQL sources"

instance BackendEventTrigger 'BigQuery where
  insertManualEvent _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  fetchUndeliveredEvents _ _ _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  setRetry _ _ _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  recordSuccess _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  getMaintenanceModeVersion _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  recordError _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  recordError' _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  dropTriggerAndArchiveEvents _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  unlockEventsInSource _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  createTableEventTrigger _ _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"

instance BackendEventTrigger 'MySQL where
  insertManualEvent _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  fetchUndeliveredEvents _ _ _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  setRetry _ _ _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  recordSuccess _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
  getMaintenanceModeVersion _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  recordError _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
  recordError' _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
  dropTriggerAndArchiveEvents _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  unlockEventsInSource _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
  createTableEventTrigger _ _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
