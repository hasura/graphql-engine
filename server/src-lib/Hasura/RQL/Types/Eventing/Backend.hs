module Hasura.RQL.Types.Eventing.Backend
  ( BackendEventTrigger (..),
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Set.NonEmpty qualified as NE
import Data.Time.Clock qualified as Time
import Hasura.Backends.MSSQL.DDL.EventTrigger qualified as MSSQL
import Hasura.Backends.Postgres.DDL.EventTrigger qualified as Postgres
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column (ColumnInfo)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table (PrimaryKey)
import Hasura.SQL.Backend
import Hasura.Server.Types (HasServerConfigCtx, MaintenanceMode, ServerConfigCtx)
import Hasura.Session (UserInfo)
import Hasura.Tracing qualified as Tracing

-- | The @BackendEventTrigger@ type class contains functions which interacts
--   with the source database to perform event trigger related operations like
--   fetching pending events from the database or inserting a new invocation log
--   after processing an event.
class Backend b => BackendEventTrigger (b :: BackendType) where
  -- | insertManualEvent inserts the specified event in the event log table,
  --   note that this method should also set the trace context and session
  --   variables in the source database context (if available).
  insertManualEvent ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    TableName b ->
    TriggerName ->
    Value ->
    UserInfo ->
    Tracing.TraceContext ->
    m EventId

  -- | @fetchUndeliveredEvents@ fetches the undelivered events from the source
  --   and locks those events for processing. The locking is done so that when
  --   there are multiple instances of graphql-engine connected to the same
  --   source they don't end up processing the same events concurrently.
  --
  --   Also, it's crucial that the SQL query used to fetch events in this
  --   function uses something like Postgres's `FOR UPDATE SKIP LOCKED`
  --   mechanism so that it skips past the events which are locked by the
  --   database and pick newer undelivered events to achieve maximum throughput.
  --
  --   The locking mechanism for event triggers is timestamp based i.e. when an
  --   event is fetched from the database, the `locked` column will contain the
  --   timestamp of when it was fetched from the database. Undelivered events
  --   will have `NULL` value as their `locked` column value.
  --
  --   The idea behind having a timestamp based locking mechanism is that if the
  --   graphql-engine is shutdown abruptly with events being fetched by the
  --   events processor, it will be locked and after the shutdown it will remain
  --   locked. Now with a timestamp based lock, when the graphql-engine is
  --   started again it will also fetch events which have a `locked` value of
  --   older than 30 mins along with the undelivered events. So, this way no
  --   events remain in a `locked` state.
  --
  --   When fetching the events from the event_log table we also include the
  --   list of the triggers that exist in the metadata at that point of time,
  --   because we have seen in some cases there are events that do not belong to
  --   any of the event triggers present in the metadata and those are fetched
  --   only to be failed saying the said event trigger doesn't exist. So, to
  --   avoid this (atleast, as much as possible) we get only the events of the
  --   event triggers we have in the metadata.
  fetchUndeliveredEvents ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    SourceName ->
    -- | List of trigger names which exist in the metadata
    [TriggerName] ->
    MaintenanceMode () ->
    FetchBatchSize ->
    m [Event b]

  -- | Ad-hoc function to set a retry for an undelivered event
  setRetry ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    Event b ->
    Time.UTCTime ->
    MaintenanceMode MaintenanceModeVersion ->
    m ()

  -- | @getMaintenanceModeVersion@ gets the source catalog version from the
  --   source
  getMaintenanceModeVersion ::
    (MonadIO m, MonadError QErr m) =>
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
    MaintenanceMode MaintenanceModeVersion ->
    m (Either QErr ())

  -- | @recordError@ records an erronous event invocation, it does a couple of
  --   things,
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
    MaintenanceMode MaintenanceModeVersion ->
    m (Either QErr ())

  -- | @recordError'@ records an erronous event invocation, it does a couple of
  --   things,
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
    MaintenanceMode MaintenanceModeVersion ->
    m (Either QErr ())

  -- | @dropTriggerAndArchiveEvents@ drops the database trigger and
  --   marks all the events related to the event trigger as archived.
  --   See Note [Cleanup for dropped triggers]
  dropTriggerAndArchiveEvents ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    TriggerName ->
    TableName b ->
    m ()

  -- | @dropDanglingSQLTriggger@ is used to delete the extraneous SQL triggers
  --   created by an event trigger. The extraneous SQL triggers can be created
  --   when an event trigger's definition is replaced to a new definition. For
  --   example, an event trigger `authors_all` had an INSERT and UPDATE trigger
  --   defined earlier and after it has UPDATE and DELETE triggers. So, in this
  --   case, we need to drop the trigger created by us earlier for the INSERT
  --   trigger.
  dropDanglingSQLTrigger ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    TriggerName ->
    TableName b ->
    HashSet Ops ->
    m ()

  redeliverEvent ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    EventId ->
    m ()

  -- | @unlockEventsInSource@ unlocks the cached locked events which were
  --   captured when a graceful shutdown is initiated, so that when the
  --   graphql-engine restarts these events can be fetched to process them
  --   immediately.
  unlockEventsInSource ::
    MonadIO m =>
    SourceConfig b ->
    NE.NESet EventId ->
    m (Either QErr Int)

  -- | @createMissingSQLTriggers@ checks in the source whether all the triggers
  --   exist according to the event trigger's specification. If any SQL trigger doesn't
  --   exist then it will create it.
  createMissingSQLTriggers ::
    (MonadIO m, MonadError QErr m, MonadBaseControl IO m, Backend b, HasServerConfigCtx m) =>
    SourceConfig b ->
    TableName b ->
    ([ColumnInfo b], Maybe (PrimaryKey b (ColumnInfo b))) ->
    TriggerName ->
    TriggerOnReplication ->
    TriggerOpsDef b ->
    m ()

  createTableEventTrigger ::
    (MonadBaseControl IO m, MonadIO m, MonadError QErr m) =>
    ServerConfigCtx ->
    SourceConfig b ->
    TableName b ->
    [ColumnInfo b] ->
    TriggerName ->
    TriggerOnReplication ->
    TriggerOpsDef b ->
    -- TODO: Naveen: Find a better way to pass these extra, backend specific
    -- parameters instead of adding a bunch of Maybes to the type class
    -- functions.
    --
    -- Update event trigger on MS-SQL are only supported on tables with primary
    -- keys. Hence the PrimaryKey argument below.
    Maybe (PrimaryKey b (ColumnInfo b)) ->
    m (Either QErr ())

  checkIfTriggerExists ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    TriggerName ->
    HashSet Ops ->
    m Bool

  -- | @addCleanupSchedules@ adds cleanup logs for given trigger names and cleanup configs.
  --   This will perform the following steps:
  --
  --   1. Get last scheduled cleanup event and count.
  --   2. If count is less than 5, then add add more cleanup logs, else do nothing
  addCleanupSchedules ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    [(TriggerName, AutoTriggerLogCleanupConfig)] ->
    m ()

  -- | @deleteAllScheduledCleanups@ deletes all scheduled cleanup logs for a given event trigger
  deleteAllScheduledCleanups ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    TriggerName ->
    m ()

  -- | @getCleanupEventsForDeletion@ returns the cleanup logs that are to be deleted.
  -- This will perform the following steps:
  --
  -- 1. Get the scheduled cleanup events that were scheduled before current time.
  -- 2. If there are multiple entries for the same trigger name with different scheduled time,
  --    then fetch the latest entry and mark others as dead.
  getCleanupEventsForDeletion ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    m [(Text, TriggerName)]

  -- | @updateCleanupEventStatusToDead@ updates the event trigger cleanup logs as dead
  updateCleanupEventStatusToDead ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    [Text] ->
    m ()

  -- | @updateCleanupEventStatusToPaused@ updates the cleanup log status to `paused` if the event trigger configuration is paused.
  updateCleanupEventStatusToPaused ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    Text ->
    m ()

  -- | @updateCleanupEventStatusToCompleted@ updates the cleanup log status after the event logs are deleted.
  -- This will perform the following steps:
  --
  -- 1. Updates the cleanup config status to `completed`.
  -- 2. Updates the number of event logs and event invocation logs that were deleted for a trigger name
  updateCleanupEventStatusToCompleted ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    Text ->
    DeletedEventLogStats ->
    m ()

  -- | @deleteEventTriggerLogs@ deletes the event logs (and event invocation logs) based on the cleanup configuration given
  -- This will perform the following steps:
  --
  -- 1. Select all the dead events based on criteria set in the cleanup config.
  -- 2. Lock the events in the database so that other HGE instances don't pick them up for deletion.
  -- 3. Based on the config, perform the delete action.
  deleteEventTriggerLogs ::
    (MonadIO m, MonadError QErr m) =>
    SourceConfig b ->
    TriggerLogCleanupConfig ->
    IO (Maybe (TriggerLogCleanupConfig, EventTriggerCleanupStatus)) ->
    m DeletedEventLogStats

--------------------------------------------------------------------------------
-- TODO: move those instances to 'Backend/*/Instances/Eventing' and create a
-- corresponding 'Instances.hs' file in this directory to import them, similarly
-- to how we import instances for other backend classes. This would
-- significantly reduce the number of files in the core engine that end up
-- depending / importing backend-specific files.

instance BackendEventTrigger ('Postgres 'Vanilla) where
  insertManualEvent = Postgres.insertManualEvent
  fetchUndeliveredEvents = Postgres.fetchUndeliveredEvents
  setRetry = Postgres.setRetry
  getMaintenanceModeVersion = Postgres.getMaintenanceModeVersion
  recordSuccess = Postgres.recordSuccess
  recordError = Postgres.recordError
  recordError' = Postgres.recordError'
  dropTriggerAndArchiveEvents = Postgres.dropTriggerAndArchiveEvents
  dropDanglingSQLTrigger = Postgres.dropDanglingSQLTrigger
  redeliverEvent = Postgres.redeliverEvent
  unlockEventsInSource = Postgres.unlockEventsInSource
  createTableEventTrigger = Postgres.createTableEventTrigger
  createMissingSQLTriggers = Postgres.createMissingSQLTriggers
  checkIfTriggerExists = Postgres.checkIfTriggerExists
  addCleanupSchedules = Postgres.addCleanupSchedules
  deleteAllScheduledCleanups = Postgres.deleteAllScheduledCleanups
  getCleanupEventsForDeletion = Postgres.getCleanupEventsForDeletion
  updateCleanupEventStatusToDead = Postgres.updateCleanupEventStatusToDead
  updateCleanupEventStatusToPaused = Postgres.updateCleanupEventStatusToPaused
  updateCleanupEventStatusToCompleted = Postgres.updateCleanupEventStatusToCompleted
  deleteEventTriggerLogs = Postgres.deleteEventTriggerLogs

instance BackendEventTrigger ('Postgres 'Citus) where
  insertManualEvent _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  fetchUndeliveredEvents _ _ _ _ _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  setRetry _ _ _ _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  recordSuccess _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"
  getMaintenanceModeVersion _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  recordError _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"
  recordError' _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"
  dropTriggerAndArchiveEvents _ _ _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  dropDanglingSQLTrigger _ _ _ _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for Citus sources"
  unlockEventsInSource _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"
  createTableEventTrigger _ _ _ _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for Citus sources"
  createMissingSQLTriggers _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  checkIfTriggerExists _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  addCleanupSchedules _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  deleteAllScheduledCleanups _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  getCleanupEventsForDeletion _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  updateCleanupEventStatusToDead _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  updateCleanupEventStatusToPaused _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  updateCleanupEventStatusToCompleted _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  deleteEventTriggerLogs _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"

instance BackendEventTrigger ('Postgres 'Cockroach) where
  insertManualEvent = Postgres.insertManualEvent
  fetchUndeliveredEvents = Postgres.fetchUndeliveredEvents
  setRetry = Postgres.setRetry
  getMaintenanceModeVersion = Postgres.getMaintenanceModeVersion
  recordSuccess = Postgres.recordSuccess
  recordError = Postgres.recordError
  recordError' = Postgres.recordError'
  dropTriggerAndArchiveEvents = Postgres.dropTriggerAndArchiveEvents
  dropDanglingSQLTrigger = Postgres.dropDanglingSQLTrigger
  redeliverEvent = Postgres.redeliverEvent
  unlockEventsInSource = Postgres.unlockEventsInSource
  createTableEventTrigger = Postgres.createTableEventTrigger
  createMissingSQLTriggers = Postgres.createMissingSQLTriggers
  checkIfTriggerExists = Postgres.checkIfTriggerExists
  addCleanupSchedules = Postgres.addCleanupSchedules
  deleteAllScheduledCleanups = Postgres.deleteAllScheduledCleanups
  getCleanupEventsForDeletion = Postgres.getCleanupEventsForDeletion
  updateCleanupEventStatusToDead = Postgres.updateCleanupEventStatusToDead
  updateCleanupEventStatusToPaused = Postgres.updateCleanupEventStatusToPaused
  updateCleanupEventStatusToCompleted = Postgres.updateCleanupEventStatusToCompleted
  deleteEventTriggerLogs = Postgres.deleteEventTriggerLogs

instance BackendEventTrigger 'MSSQL where
  insertManualEvent = MSSQL.insertManualEvent
  fetchUndeliveredEvents = MSSQL.fetchUndeliveredEvents
  setRetry = MSSQL.setRetry
  recordSuccess = MSSQL.recordSuccess
  getMaintenanceModeVersion = MSSQL.getMaintenanceModeVersion
  recordError = MSSQL.recordError
  recordError' = MSSQL.recordError'
  dropTriggerAndArchiveEvents = MSSQL.dropTriggerAndArchiveEvents
  redeliverEvent = MSSQL.redeliverEvent
  unlockEventsInSource = MSSQL.unlockEventsInSource
  dropDanglingSQLTrigger = MSSQL.dropDanglingSQLTrigger
  createTableEventTrigger = MSSQL.createTableEventTrigger
  createMissingSQLTriggers = MSSQL.createMissingSQLTriggers
  checkIfTriggerExists = MSSQL.checkIfTriggerExists
  addCleanupSchedules = MSSQL.addCleanupSchedules
  deleteAllScheduledCleanups = MSSQL.deleteAllScheduledCleanups
  getCleanupEventsForDeletion = MSSQL.getCleanupEventsForDeletion
  updateCleanupEventStatusToDead = MSSQL.updateCleanupEventStatusToDead
  updateCleanupEventStatusToPaused = MSSQL.updateCleanupEventStatusToPaused
  updateCleanupEventStatusToCompleted = MSSQL.updateCleanupEventStatusToCompleted
  deleteEventTriggerLogs = MSSQL.deleteEventTriggerLogs

instance BackendEventTrigger 'BigQuery where
  insertManualEvent _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  fetchUndeliveredEvents _ _ _ _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  setRetry _ _ _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  recordSuccess _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  getMaintenanceModeVersion _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  recordError _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  recordError' _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  dropTriggerAndArchiveEvents _ _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  dropDanglingSQLTrigger _ _ _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  unlockEventsInSource _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  createTableEventTrigger _ _ _ _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  createMissingSQLTriggers _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  checkIfTriggerExists _ _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  addCleanupSchedules _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  deleteAllScheduledCleanups _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  getCleanupEventsForDeletion _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  updateCleanupEventStatusToDead _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  updateCleanupEventStatusToPaused _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  updateCleanupEventStatusToCompleted _ _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"
  deleteEventTriggerLogs _ _ _ = throw400 NotSupported $ "Event triggers are not supported for BigQuery sources"

instance BackendEventTrigger 'MySQL where
  insertManualEvent _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  fetchUndeliveredEvents _ _ _ _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  setRetry _ _ _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  recordSuccess _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
  getMaintenanceModeVersion _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  recordError _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
  recordError' _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
  dropTriggerAndArchiveEvents _ _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  dropDanglingSQLTrigger _ _ _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  unlockEventsInSource _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
  createTableEventTrigger _ _ _ _ _ _ _ _ = runExceptT $ throw400 NotSupported "Event triggers are not supported for MySQL sources"
  createMissingSQLTriggers _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  checkIfTriggerExists _ _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  addCleanupSchedules _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  deleteAllScheduledCleanups _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  getCleanupEventsForDeletion _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  updateCleanupEventStatusToDead _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  updateCleanupEventStatusToPaused _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  updateCleanupEventStatusToCompleted _ _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"
  deleteEventTriggerLogs _ _ _ = throw400 NotSupported $ "Event triggers are not supported for MySQL sources"

--------------------------------------------------------------------------------

-- TODO(jkachmar): See if there isn't a way to define the function that
-- implement these methods in the 'Hasura.Experimental.Adapters' module
-- hierarchy just to keep everything as tidy as possible for that section of
-- code.
instance BackendEventTrigger 'DataConnector where
  insertManualEvent _ _ _ _ _ _ =
    throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  fetchUndeliveredEvents _ _ _ _ _ =
    throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  setRetry _ _ _ _ =
    throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  recordSuccess _ _ _ _ =
    runExceptT $ throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  getMaintenanceModeVersion _ =
    throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  recordError _ _ _ _ _ =
    runExceptT $ throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  recordError' _ _ _ _ _ =
    runExceptT $ throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  dropTriggerAndArchiveEvents _ _ _ =
    throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  dropDanglingSQLTrigger _ _ _ _ =
    throw400 NotSupported "Event triggers are not supported for the Data Connector backend"
  redeliverEvent _ _ =
    throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  unlockEventsInSource _ _ =
    runExceptT $ throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  createTableEventTrigger _ _ _ _ _ _ _ _ =
    runExceptT $ throw400 NotSupported "Event triggers are not supported for the Data Connector backend."
  createMissingSQLTriggers _ _ _ _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Data Connector backend."
  checkIfTriggerExists _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Data Connector backend."
  addCleanupSchedules _ _ = throw400 NotSupported $ "Event triggers are not supported for Data Connector backend."
  deleteAllScheduledCleanups _ _ = throw400 NotSupported $ "Event triggers are not supported for Data Connector backend."
  getCleanupEventsForDeletion _ = throw400 NotSupported $ "Event triggers are not supported for Data Connector sources"
  updateCleanupEventStatusToDead _ _ = throw400 NotSupported $ "Event triggers are not supported for Data Connector sources"
  updateCleanupEventStatusToPaused _ _ = throw400 NotSupported $ "Event triggers are not supported for Data Connector sources"
  updateCleanupEventStatusToCompleted _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Data Connector sources"
  deleteEventTriggerLogs _ _ _ = throw400 NotSupported $ "Event triggers are not supported for Data Connector sources"
