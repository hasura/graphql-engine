{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Postgres DDL EventTrigger
--
-- Used for creating event triggers for metadata changes.
--
-- See 'Hasura.Eventing.Backend'.
module Hasura.Backends.Postgres.DDL.EventTrigger
  ( insertManualEvent,
    redeliverEvent,
    dropTriggerAndArchiveEvents,
    createTableEventTrigger,
    createMissingSQLTriggers,
    dropTriggerQ,
    dropDanglingSQLTrigger,
    mkAllTriggersQ,
    getMaintenanceModeVersion,
    fetchUndeliveredEvents,
    setRetry,
    recordSuccess,
    recordError,
    recordError',
    unlockEventsInSource,
    updateColumnInEventTrigger,
    checkIfTriggerExists,
    addCleanupSchedules,
    deleteAllScheduledCleanups,
    getCleanupEventsForDeletion,
    updateCleanupEventStatusToDead,
    updateCleanupEventStatusToPaused,
    updateCleanupEventStatusToCompleted,
    deleteEventTriggerLogs,
    fetchEventLogs,
    fetchEventInvocationLogs,
    fetchEventById,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.FileEmbed (makeRelativeToProject)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Int (Int64)
import Data.Set.NonEmpty qualified as NE
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime)
import Data.Time.Clock qualified as Time
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.SQL.DML
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Backends.Postgres.Translate.Column
import Hasura.Base.Error
import Hasura.Eventing.Common
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend, SourceConfig, TableName)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.ScheduledTrigger (formatTime')
import Hasura.RQL.Types.Source
import Hasura.SQL.Types
import Hasura.Server.Migrate.Internal
import Hasura.Server.Migrate.LatestVersion
import Hasura.Server.Migrate.Version
import Hasura.Server.Types
import Hasura.Session
import Hasura.Table.Cache (PrimaryKey)
import Hasura.Tracing qualified as Tracing
import Text.Builder qualified as TB
import Text.Shakespeare.Text qualified as ST

fetchUndeliveredEvents ::
  (MonadIO m, MonadError QErr m) =>
  SourceConfig ('Postgres pgKind) ->
  SourceName ->
  [TriggerName] ->
  MaintenanceMode () ->
  FetchBatchSize ->
  m [Event ('Postgres pgKind)]
fetchUndeliveredEvents sourceConfig sourceName triggerNames maintenanceMode fetchBatchSize = do
  fetchEventsTxE <-
    case maintenanceMode of
      MaintenanceModeEnabled () -> do
        maintenanceModeVersion <- liftIO $ runPgSourceReadTx sourceConfig getMaintenanceModeVersionTx
        pure $ fmap (fetchEventsMaintenanceMode sourceName triggerNames fetchBatchSize) maintenanceModeVersion
      MaintenanceModeDisabled -> pure $ Right $ fetchEvents sourceName triggerNames fetchBatchSize
  case fetchEventsTxE of
    Left err -> throwError $ prefixQErr "something went wrong while fetching events: " err
    Right fetchEventsTx ->
      liftEitherM
        $ liftIO
        $ runPgSourceWriteTx sourceConfig InternalRawQuery fetchEventsTx

setRetry ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  SourceConfig ('Postgres pgKind) ->
  Event ('Postgres pgKind) ->
  Time.UTCTime ->
  MaintenanceMode MaintenanceModeVersion ->
  m ()
setRetry sourceConfig event retryTime maintenanceModeVersion =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig InternalRawQuery (setRetryTx event retryTime maintenanceModeVersion)

insertManualEvent ::
  (MonadIO m, MonadError QErr m) =>
  SourceConfig ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  TriggerName ->
  Value ->
  UserInfo ->
  Maybe Tracing.TraceContext ->
  m EventId
insertManualEvent sourceConfig tableName triggerName payload userInfo traceCtx =
  -- NOTE: The methods `setTraceContextInTx` and `setHeadersTx` are being used
  -- to ensure that the trace context and user info are set with valid values
  -- while being used in the PG function `insert_event_log`.
  -- See Issue(#7087) for more details on a bug that was being caused
  -- in the absence of these methods.
  liftEitherM
    $ liftIO
    $ runPgSourceWriteTx sourceConfig InternalRawQuery
    $ setHeadersTx (_uiSession userInfo)
    >> setTraceContextInTx traceCtx
    >> insertPGManualEvent tableName triggerName payload

getMaintenanceModeVersion ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  SourceConfig ('Postgres pgKind) ->
  m MaintenanceModeVersion
getMaintenanceModeVersion sourceConfig =
  liftEitherM $ liftIO $ runPgSourceReadTx sourceConfig getMaintenanceModeVersionTx

recordSuccess ::
  (MonadIO m) =>
  SourceConfig ('Postgres pgKind) ->
  Event ('Postgres pgKind) ->
  Invocation 'EventType ->
  MaintenanceMode MaintenanceModeVersion ->
  m (Either QErr ())
recordSuccess sourceConfig event invocation maintenanceModeVersion =
  liftIO
    $ runPgSourceWriteTx sourceConfig InternalRawQuery
    $ do
      insertInvocation (tmName (eTrigger event)) invocation
      setSuccessTx event maintenanceModeVersion

recordError ::
  (MonadIO m) =>
  SourceConfig ('Postgres pgKind) ->
  Event ('Postgres pgKind) ->
  Invocation 'EventType ->
  ProcessEventError ->
  MaintenanceMode MaintenanceModeVersion ->
  m (Either QErr ())
recordError sourceConfig event invocation processEventError maintenanceModeVersion =
  recordError' sourceConfig event (Just invocation) processEventError maintenanceModeVersion

recordError' ::
  (MonadIO m) =>
  SourceConfig ('Postgres pgKind) ->
  Event ('Postgres pgKind) ->
  Maybe (Invocation 'EventType) ->
  ProcessEventError ->
  MaintenanceMode MaintenanceModeVersion ->
  m (Either QErr ())
recordError' sourceConfig event invocation processEventError maintenanceModeVersion =
  liftIO
    $ runPgSourceWriteTx sourceConfig InternalRawQuery
    $ do
      for_ invocation $ insertInvocation (tmName (eTrigger event))
      case processEventError of
        PESetRetry retryTime -> setRetryTx event retryTime maintenanceModeVersion
        PESetError -> setErrorTx event maintenanceModeVersion

redeliverEvent ::
  (MonadIO m, MonadError QErr m) =>
  SourceConfig ('Postgres pgKind) ->
  EventId ->
  m ()
redeliverEvent sourceConfig eventId =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig InternalRawQuery (redeliverEventTx eventId)

dropTriggerAndArchiveEvents ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  SourceConfig ('Postgres pgKind) ->
  TriggerName ->
  QualifiedTable ->
  m ()
dropTriggerAndArchiveEvents sourceConfig triggerName _table =
  liftEitherM
    $ liftIO
    $ runPgSourceWriteTx sourceConfig InternalRawQuery
    $ do
      dropTriggerQ triggerName
      archiveEvents triggerName

createMissingSQLTriggers ::
  ( MonadIO m,
    MonadError QErr m,
    MonadBaseControl IO m,
    Backend ('Postgres pgKind)
  ) =>
  SQLGenCtx ->
  PGSourceConfig ->
  TableName ('Postgres pgKind) ->
  ([(ColumnInfo ('Postgres pgKind))], Maybe (PrimaryKey ('Postgres pgKind) (ColumnInfo ('Postgres pgKind)))) ->
  TriggerName ->
  TriggerOnReplication ->
  TriggerOpsDef ('Postgres pgKind) ->
  m ()
createMissingSQLTriggers serverConfigCtx sourceConfig table (allCols, _) triggerName triggerOnReplication opsDefinition = do
  liftEitherM
    $ runPgSourceWriteTx sourceConfig InternalRawQuery
    $ do
      for_ (tdInsert opsDefinition) (doesSQLTriggerExist INSERT)
      for_ (tdUpdate opsDefinition) (doesSQLTriggerExist UPDATE)
      for_ (tdDelete opsDefinition) (doesSQLTriggerExist DELETE)
  where
    doesSQLTriggerExist op opSpec = do
      let opTriggerName = pgTriggerName op triggerName
      doesOpTriggerFunctionExist <-
        runIdentity
          . PG.getRow
          <$> PG.withQE
            defaultTxErrorHandler
            [PG.sql|
                 SELECT EXISTS
                   ( SELECT 1
                     FROM pg_proc
                     WHERE proname = $1
                   )
              |]
            (Identity opTriggerName)
            True
      unless doesOpTriggerFunctionExist
        $ flip runReaderT serverConfigCtx
        $ mkTrigger triggerName table triggerOnReplication allCols op opSpec

createTableEventTrigger ::
  (Backend ('Postgres pgKind), MonadIO m, MonadBaseControl IO m) =>
  SQLGenCtx ->
  PGSourceConfig ->
  QualifiedTable ->
  [ColumnInfo ('Postgres pgKind)] ->
  TriggerName ->
  TriggerOnReplication ->
  TriggerOpsDef ('Postgres pgKind) ->
  Maybe (PrimaryKey ('Postgres pgKind) (ColumnInfo ('Postgres pgKind))) ->
  m (Either QErr ())
createTableEventTrigger serverConfigCtx sourceConfig table columns triggerName triggerOnReplication opsDefinition _ = runPgSourceWriteTx sourceConfig InternalRawQuery $ do
  -- Create the given triggers
  flip runReaderT serverConfigCtx
    $ mkAllTriggersQ triggerName table triggerOnReplication columns opsDefinition

dropDanglingSQLTrigger ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  SourceConfig ('Postgres pgKind) ->
  TriggerName ->
  QualifiedTable ->
  HashSet Ops ->
  m ()
dropDanglingSQLTrigger sourceConfig triggerName _ ops =
  liftEitherM
    $ liftIO
    $ runPgSourceWriteTx sourceConfig InternalRawQuery
    $ traverse_ (dropTriggerOp triggerName) ops

updateColumnInEventTrigger ::
  QualifiedTable ->
  PGCol ->
  PGCol ->
  QualifiedTable ->
  EventTriggerConf ('Postgres pgKind) ->
  EventTriggerConf ('Postgres pgKind)
updateColumnInEventTrigger table oCol nCol refTable = rewriteEventTriggerConf
  where
    rewriteSubsCols = \case
      SubCStar -> SubCStar
      SubCArray cols -> SubCArray $ map getNewCol cols
    rewriteOpSpec (SubscribeOpSpec listenColumns deliveryColumns) =
      SubscribeOpSpec
        (rewriteSubsCols listenColumns)
        (rewriteSubsCols <$> deliveryColumns)
    rewriteTrigOpsDef (TriggerOpsDef ins upd del man) =
      TriggerOpsDef
        (rewriteOpSpec <$> ins)
        (rewriteOpSpec <$> upd)
        (rewriteOpSpec <$> del)
        man
    rewriteEventTriggerConf etc =
      etc
        { etcDefinition =
            rewriteTrigOpsDef $ etcDefinition etc
        }
    getNewCol col =
      if table == refTable && oCol == col then nCol else col

unlockEventsInSource ::
  (MonadIO m) =>
  SourceConfig ('Postgres pgKind) ->
  NE.NESet EventId ->
  m (Either QErr Int)
unlockEventsInSource sourceConfig eventIds =
  liftIO $ runPgSourceWriteTx sourceConfig InternalRawQuery (unlockEventsTx $ toList eventIds)

-- Check if any trigger function for any of the operation exists with the 'triggerName'
checkIfTriggerExists ::
  (MonadIO m, MonadError QErr m) =>
  PGSourceConfig ->
  TriggerName ->
  HashSet Ops ->
  m Bool
checkIfTriggerExists sourceConfig triggerName ops = do
  liftEitherM
    $ liftIO
    $ runPgSourceWriteTx sourceConfig InternalRawQuery
    $
    -- We want to avoid creating event triggers with same name since this will
    -- cause undesired behaviour. Note that only SQL functions associated with
    -- SQL triggers are dropped when "replace = true" is set in the event trigger
    -- configuration. Hence, the best way to check if we should allow the
    -- creation of a trigger with the name 'triggerName' is to check if any
    -- function with such a name exists in the the hdb_catalog.
    --
    -- For eg: If a create_event_trigger request comes with trigger name as
    -- "triggerName" and there is already a trigger with "triggerName" in the
    -- metadata, then
    --    1. When "replace = false", the function with name 'triggerName' exists
    --       so the creation is not allowed
    --    2. When "replace = true", the function with name 'triggerName' is first
    --       dropped, hence we are allowed to create the trigger with name
    --       'triggerName'
    fmap or (traverse (checkIfFunctionExistsQ triggerName) (HashSet.toList ops))

---- DATABASE QUERIES ---------------------
--
--   The API for our in-database work queue:
-------------------------------------------

insertInvocation :: TriggerName -> Invocation 'EventType -> PG.TxE QErr ()
insertInvocation tName invo = do
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
          INSERT INTO hdb_catalog.event_invocation_logs (event_id, trigger_name, status, request, response)
          VALUES ($1, $2, $3, $4, $5)
          |]
    ( iEventId invo,
      (triggerNameToTxt tName),
      fromIntegral <$> iStatus invo :: Maybe Int64,
      PG.ViaJSON $ toJSON $ iRequest invo,
      PG.ViaJSON $ toJSON $ iResponse invo
    )
    True
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
          UPDATE hdb_catalog.event_log

          SET tries = tries + 1
          WHERE id = $1
          |]
    (Identity $ iEventId invo)
    True

insertPGManualEvent ::
  QualifiedTable ->
  TriggerName ->
  Value ->
  PG.TxE QErr EventId
insertPGManualEvent (QualifiedObject schemaName tableName) triggerName rowData = do
  runIdentity
    . PG.getRow
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
    SELECT hdb_catalog.insert_event_log($1, $2, $3, $4, $5)
  |]
      (schemaName, tableName, triggerName, (tshow MANUAL), PG.ViaJSON rowData)
      False

archiveEvents :: TriggerName -> PG.TxE QErr ()
archiveEvents trn =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
           UPDATE hdb_catalog.event_log
           SET archived = 't'
           WHERE trigger_name = $1
                |]
    (Identity trn)
    False

getMaintenanceModeVersionTx :: PG.TxE QErr MaintenanceModeVersion
getMaintenanceModeVersionTx = liftTx $ do
  catalogVersion <- getCatalogVersion -- From the user's DB
  -- the previous version and the current version will change depending
  -- upon between which versions we need to support maintenance mode
  if
    | catalogVersion == MetadataCatalogVersion 40 -> pure PreviousMMVersion
    -- The catalog is migrated to the 43rd version for a source
    -- which was initialised by a v1 graphql-engine instance (See @initSource@).
    | catalogVersion == MetadataCatalogVersion 43 -> pure CurrentMMVersion
    | catalogVersion == latestCatalogVersion -> pure CurrentMMVersion
    | otherwise ->
        throw500
          $ "Maintenance mode is only supported with catalog versions: 40, 43 and "
          <> tshow latestCatalogVersionString

-- | Lock and return events not yet being processed or completed, up to some
-- limit. Process events approximately in created_at order, but we make no
-- ordering guarentees; events can and will race. Nevertheless we want to
-- ensure newer change events don't starve older ones.
fetchEvents :: SourceName -> [TriggerName] -> FetchBatchSize -> PG.TxE QErr [Event ('Postgres pgKind)]
fetchEvents source triggerNames (FetchBatchSize fetchBatchSize) =
  map uncurryEvent
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
      UPDATE hdb_catalog.event_log
      SET locked = NOW()
      WHERE id IN ( SELECT l.id
                    FROM hdb_catalog.event_log l
                    WHERE l.delivered = 'f' and l.error = 'f'
                          and (l.locked IS NULL or l.locked < (NOW() - interval '30 minute'))
                          and (l.next_retry_at is NULL or l.next_retry_at <= now())
                          and l.archived = 'f'
                          and l.trigger_name = ANY($2)
                    /* NB: this ordering is important for our index `event_log_fetch_events` */
                    /* (see `init_pg_source.sql`) */
                    ORDER BY locked NULLS FIRST, next_retry_at NULLS FIRST, created_at
                    LIMIT $1
                    FOR UPDATE SKIP LOCKED )
      RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at, next_retry_at, 
      -- We need the UTC values of `created_at` and `next_retry_at` for metrics
      -- calculation.
      --
      -- Only `TIMESTAMPZ` (time with timezone offset) values can be used for proper  
      -- conversions between timezones.
      --
      -- Since `created_at` and `next_retry_at` are `TIMESTAMP` values (time without
      -- timezone offset), we first convert them to TIMESTAMPZ` values by appending
      -- the timezone offset of the database. And then convert the `TIMESTAMPZ`
      -- values to UTC.
      (select (select created_at at time zone (select current_setting ('TIMEZONE'))) at time zone 'UTC'), 
      (select (select next_retry_at at time zone (select current_setting ('TIMEZONE'))) at time zone 'UTC');
      |]
      (limit, triggerNamesTxt)
      True
  where
    uncurryEvent (id', sourceName, tableName, triggerName, PG.ViaJSON payload, tries, created, retryAt, createdAtUTC, retryAtUTC :: Maybe UTCTime) =
      Event
        { eId = id',
          eSource = source,
          eTable = QualifiedObject sourceName tableName,
          eTrigger = TriggerMetadata triggerName,
          eEvent = payload,
          eTries = tries,
          eCreatedAt = created,
          eRetryAt = retryAt,
          -- eCreatedAtUTC and eRetryAtUTC are used for calculating metrics only
          eCreatedAtUTC = createdAtUTC,
          eRetryAtUTC = retryAtUTC
        }
    limit = fromIntegral fetchBatchSize :: Word64

    triggerNamesTxt = PGTextArray $ triggerNameToTxt <$> triggerNames

fetchEventsMaintenanceMode :: SourceName -> [TriggerName] -> FetchBatchSize -> MaintenanceModeVersion -> PG.TxE QErr [Event ('Postgres pgKind)]
fetchEventsMaintenanceMode sourceName triggerNames fetchBatchSize = \case
  PreviousMMVersion ->
    map uncurryEvent
      <$> PG.withQE
        defaultTxErrorHandler
        [PG.sql|
        UPDATE hdb_catalog.event_log
        SET locked = 't'
        WHERE id IN ( SELECT l.id
                      FROM hdb_catalog.event_log l
                      WHERE l.delivered = 'f' and l.error = 'f' and l.locked = 'f'
                            and (l.next_retry_at is NULL or l.next_retry_at <= now())
                            and l.archived = 'f'
                      ORDER BY created_at
                      LIMIT $1
                      FOR UPDATE SKIP LOCKED )
        RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at, next_retry_at,
        (select (select created_at at time zone (select current_setting ('TIMEZONE'))) at time zone 'UTC'), 
        (select (select next_retry_at at time zone (select current_setting ('TIMEZONE'))) at time zone 'UTC');
        |]
        (Identity limit)
        True
    where
      uncurryEvent (id', sn, tn, trn, PG.ViaJSON payload, tries, created, retryAt, createdAtUTC, retryAtUTC) =
        Event
          { eId = id',
            eSource = SNDefault, -- in v1, there'll only be the default source
            eTable = QualifiedObject sn tn,
            eTrigger = TriggerMetadata trn,
            eEvent = payload,
            eTries = tries,
            eCreatedAt = created,
            eRetryAt = retryAt,
            -- eCreatedAtUTC and eRetryAtUTC are used for calculating metrics only
            eCreatedAtUTC = createdAtUTC,
            eRetryAtUTC = retryAtUTC
          }
      limit = fromIntegral (_unFetchBatchSize fetchBatchSize) :: Word64
  CurrentMMVersion -> fetchEvents sourceName triggerNames fetchBatchSize

setSuccessTx :: Event ('Postgres pgKind) -> MaintenanceMode MaintenanceModeVersion -> PG.TxE QErr ()
setSuccessTx e = \case
  (MaintenanceModeEnabled PreviousMMVersion) ->
    PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
    UPDATE hdb_catalog.event_log
    SET delivered = 't', next_retry_at = NULL, locked = 'f'
    WHERE id = $1
    |]
      (Identity $ eId e)
      True
  (MaintenanceModeEnabled CurrentMMVersion) -> latestVersionSetSuccess
  MaintenanceModeDisabled -> latestVersionSetSuccess
  where
    latestVersionSetSuccess =
      PG.unitQE
        defaultTxErrorHandler
        [PG.sql|
      UPDATE hdb_catalog.event_log
      SET delivered = 't', next_retry_at = NULL, locked = NULL
      WHERE id = $1
      |]
        (Identity $ eId e)
        True

setErrorTx :: Event ('Postgres pgKind) -> MaintenanceMode MaintenanceModeVersion -> PG.TxE QErr ()
setErrorTx e = \case
  (MaintenanceModeEnabled PreviousMMVersion) ->
    PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
    UPDATE hdb_catalog.event_log
    SET error = 't', next_retry_at = NULL, locked = 'f'
    WHERE id = $1
    |]
      (Identity $ eId e)
      True
  (MaintenanceModeEnabled CurrentMMVersion) -> latestVersionSetError
  MaintenanceModeDisabled -> latestVersionSetError
  where
    latestVersionSetError =
      PG.unitQE
        defaultTxErrorHandler
        [PG.sql|
      UPDATE hdb_catalog.event_log
      SET error = 't', next_retry_at = NULL, locked = NULL
      WHERE id = $1
      |]
        (Identity $ eId e)
        True

setRetryTx :: Event ('Postgres pgKind) -> Time.UTCTime -> MaintenanceMode MaintenanceModeVersion -> PG.TxE QErr ()
setRetryTx e time = \case
  (MaintenanceModeEnabled PreviousMMVersion) ->
    PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
    UPDATE hdb_catalog.event_log
    SET next_retry_at = $1, locked = 'f'
    WHERE id = $2
    |]
      (time, eId e)
      True
  (MaintenanceModeEnabled CurrentMMVersion) -> latestVersionSetRetry
  MaintenanceModeDisabled -> latestVersionSetRetry
  where
    latestVersionSetRetry =
      PG.unitQE
        defaultTxErrorHandler
        [PG.sql|
              UPDATE hdb_catalog.event_log
              SET next_retry_at = $1, locked = NULL
              WHERE id = $2
              |]
        (time, eId e)
        True

dropTriggerQ :: TriggerName -> PG.TxE QErr ()
dropTriggerQ trn =
  mapM_ (dropTriggerOp trn) [INSERT, UPDATE, DELETE]

dropTriggerOp :: TriggerName -> Ops -> PG.TxE QErr ()
dropTriggerOp triggerName triggerOp =
  PG.unitQE
    defaultTxErrorHandler
    (PG.fromText $ getDropFuncSql triggerOp)
    ()
    False
  where
    getDropFuncSql :: Ops -> Text
    getDropFuncSql op =
      "DROP FUNCTION IF EXISTS"
        <> " hdb_catalog."
        <> unQualifiedTriggerName (pgIdenTrigger op triggerName)
        <> "()"
        <> " CASCADE"

checkEvent :: EventId -> PG.TxE QErr ()
checkEvent eid = do
  events <-
    PG.withQE
      defaultTxErrorHandler
      [PG.sql|
              SELECT l.locked IS NOT NULL AND l.locked >= (NOW() - interval '30 minute')
              FROM hdb_catalog.event_log l
              WHERE l.id = $1
              |]
      (Identity eid)
      True
  event <- getEvent events
  assertEventUnlocked event
  where
    getEvent [] = throw400 NotExists "event not found"
    getEvent (x : _) = return x

    assertEventUnlocked (Identity locked) =
      when locked
        $ throw400 Busy "event is already being processed"

markForDelivery :: EventId -> PG.TxE QErr ()
markForDelivery eid =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
          UPDATE hdb_catalog.event_log
          SET
          delivered = 'f',
          error = 'f',
          tries = 0
          WHERE id = $1
          |]
    (Identity eid)
    True

redeliverEventTx :: EventId -> PG.TxE QErr ()
redeliverEventTx eventId = do
  checkEvent eventId
  markForDelivery eventId

-- | unlockEvents takes an array of 'EventId' and unlocks them. This function is called
--   when a graceful shutdown is initiated.
unlockEventsTx :: [EventId] -> PG.TxE QErr Int
unlockEventsTx eventIds =
  runIdentity
    . PG.getRow
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
     WITH "cte" AS
     (UPDATE hdb_catalog.event_log
     SET locked = NULL
     WHERE id = ANY($1::text[])
     -- only unlock those events that have been locked, it's possible
     -- that an event has been processed but not yet been removed from
     -- the saved locked events, which will lead to a double send
     AND locked IS NOT NULL
     RETURNING *)
     SELECT count(*) FROM "cte"
   |]
      (Identity $ PGTextArray $ map unEventId eventIds)
      True

---- Postgres event trigger utility functions ---------------------

-- | QualifiedTriggerName is a type to store the name of the SQL trigger.
--   An example of it is `"notify_hasura_users_all_INSERT"` where `users_all`
--   is the name of the event trigger.
newtype QualifiedTriggerName = QualifiedTriggerName {unQualifiedTriggerName :: Text}
  deriving (Show, Eq, PG.ToPrepArg)

pgTriggerName :: Ops -> TriggerName -> QualifiedTriggerName
pgTriggerName op trn = qualifyTriggerName op $ triggerNameToTxt trn
  where
    qualifyTriggerName op' trn' =
      QualifiedTriggerName $ "notify_hasura_" <> trn' <> "_" <> tshow op'

-- | pgIdenTrigger is a method used to construct the name of the pg function
-- used for event triggers which are present in the hdb_catalog schema.
pgIdenTrigger :: Ops -> TriggerName -> QualifiedTriggerName
pgIdenTrigger op = QualifiedTriggerName . pgFmtIdentifier . unQualifiedTriggerName . pgTriggerName op

-- | Define the pgSQL trigger functions on database events.
mkTriggerFunctionQ ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadTx m, MonadReader SQLGenCtx m) =>
  TriggerName ->
  QualifiedTable ->
  [ColumnInfo ('Postgres pgKind)] ->
  Ops ->
  SubscribeOpSpec ('Postgres pgKind) ->
  m QualifiedTriggerName
mkTriggerFunctionQ triggerName (QualifiedObject schema table) allCols op (SubscribeOpSpec listenColumns deliveryColumns') = do
  strfyNum <- asks stringifyNum
  let dbQualifiedTriggerName = pgIdenTrigger op triggerName
  () <-
    liftTx
      $ PG.multiQE defaultTxErrorHandler
      $ PG.fromText
      . TL.toStrict
      $ let
            -- If there are no specific delivery columns selected by user then all the columns will be delivered
            -- in payload hence 'SubCStar'.
            deliveryColumns = fromMaybe SubCStar deliveryColumns'
            getApplicableColumns = \case
              SubCStar -> allCols
              SubCArray cols -> getColInfos cols allCols

            -- Columns that should be present in the payload. By default, all columns are present.
            applicableDeliveryCols = getApplicableColumns deliveryColumns
            getRowExpression opVar = applyRowToJson' $ mkRowExpression opVar strfyNum applicableDeliveryCols

            -- Columns that user subscribed to listen for changes. By default, we listen on all columns.
            applicableListenCols = getApplicableColumns listenColumns
            renderRow opVar = applyRow $ mkRowExpression opVar strfyNum applicableListenCols

            oldDataExp = case op of
              INSERT -> SENull
              UPDATE -> getRowExpression OLD
              DELETE -> getRowExpression OLD
              MANUAL -> SENull
            newDataExp = case op of
              INSERT -> getRowExpression NEW
              UPDATE -> getRowExpression NEW
              DELETE -> SENull
              MANUAL -> SENull

            name = triggerNameToTxt triggerName
            qualifiedTriggerName = unQualifiedTriggerName dbQualifiedTriggerName
            schemaName = pgFmtLit $ getSchemaTxt schema
            tableName = pgFmtLit $ getTableTxt table

            oldRow = toSQLTxt $ renderRow OLD
            newRow = toSQLTxt $ renderRow NEW
            oldPayloadExpression = toSQLTxt oldDataExp
            newPayloadExpression = toSQLTxt newDataExp
         in $(makeRelativeToProject "src-rsr/trigger.sql.shakespeare" >>= ST.stextFile)
  pure dbQualifiedTriggerName
  where
    applyRowToJson' e = SEFnApp "row_to_json" [e] Nothing
    applyRow e = SEFnApp "row" [e] Nothing
    opToQual = QualVar . tshow

    mkRowExpression opVar strfyNum columns =
      mkRowExp $ map (\col -> toExtractor (mkQId opVar strfyNum col) col) columns

    mkQId opVar strfyNum colInfo =
      toJSONableExp strfyNum (ciType colInfo) False Nothing
        $ SEQIdentifier
        $ QIdentifier (opToQual opVar)
        $ toIdentifier
        $ ciColumn colInfo

    -- Generate the SQL expression
    toExtractor sqlExp column
      -- If the column type is either 'Geography' or 'Geometry', then after applying the 'ST_AsGeoJSON' function
      -- to the column, alias the value of the expression with the column name else it uses `st_asgeojson` as
      -- the column name.
      | isScalarColumnWhere isGeoType (ciType column) = Extractor sqlExp (Just $ getAlias column)
      | otherwise = Extractor sqlExp Nothing
    getAlias col = toColumnAlias $ Identifier $ getPGColTxt (ciColumn col)

checkIfTriggerExistsForTableQ ::
  QualifiedTriggerName ->
  QualifiedTable ->
  PG.TxE QErr Bool
checkIfTriggerExistsForTableQ (QualifiedTriggerName triggerName) (QualifiedObject schemaName tableName) =
  fmap (runIdentity . PG.getRow)
    $ PG.withQE
      defaultTxErrorHandler
      -- 'regclass' converts non-quoted strings to lowercase but since identifiers
      -- such as table name needs are case-sensitive, we add quotes to table name
      -- using 'quote_ident'.
      -- Ref: https://www.postgresql.org/message-id/3896142.1620136761%40sss.pgh.pa.us
      [PG.sql|
      SELECT EXISTS (
        SELECT 1
        FROM pg_trigger
        WHERE NOT tgisinternal
        AND tgname = $1 AND tgrelid = (quote_ident($2) || '.' || quote_ident($3))::regclass
        )
     |]
      (triggerName, schemaName, tableName)
      True

checkIfFunctionExistsQ ::
  TriggerName ->
  Ops ->
  PG.TxE QErr Bool
checkIfFunctionExistsQ triggerName op = do
  let qualifiedTriggerName = pgTriggerName op triggerName
  fmap (runIdentity . PG.getRow)
    $ PG.withQE
      defaultTxErrorHandler
      [PG.sql|
      SELECT EXISTS (
        SELECT 1
        FROM pg_catalog.pg_proc
        JOIN pg_namespace ON pg_catalog.pg_proc.pronamespace = pg_namespace.oid
        WHERE proname = $1
        AND pg_namespace.nspname = 'hdb_catalog'
        )
     |]
      (Identity qualifiedTriggerName)
      True

mkTrigger ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadTx m, MonadReader SQLGenCtx m) =>
  TriggerName ->
  QualifiedTable ->
  TriggerOnReplication ->
  [ColumnInfo ('Postgres pgKind)] ->
  Ops ->
  SubscribeOpSpec ('Postgres pgKind) ->
  m ()
mkTrigger triggerName table triggerOnReplication allCols op subOpSpec = do
  -- create/replace the trigger function
  QualifiedTriggerName dbTriggerNameTxt <- mkTriggerFunctionQ triggerName table allCols op subOpSpec
  -- check if the SQL trigger exists and only if the SQL trigger doesn't exist
  -- we create the SQL trigger.
  doesTriggerExist <- liftTx $ checkIfTriggerExistsForTableQ (pgTriggerName op triggerName) table
  unless doesTriggerExist
    $ let createTriggerSqlQuery =
            PG.fromText $ createTriggerSQL dbTriggerNameTxt (toSQLTxt table) (tshow op)
       in liftTx $ do
            PG.unitQE defaultTxErrorHandler createTriggerSqlQuery () False
            when (triggerOnReplication == TOREnableTrigger)
              $ PG.unitQE defaultTxErrorHandler (alwaysEnableTriggerQuery dbTriggerNameTxt (toSQLTxt table)) () False
  where
    createTriggerSQL triggerNameTxt tableName opText =
      [ST.st|
         CREATE TRIGGER #{triggerNameTxt} AFTER #{opText} ON #{tableName} FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.#{triggerNameTxt}()
      |]

    alwaysEnableTriggerQuery triggerNameTxt tableTxt =
      PG.fromText
        $ [ST.st|
        ALTER TABLE #{tableTxt} ENABLE ALWAYS TRIGGER #{triggerNameTxt};
      |]

mkAllTriggersQ ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadTx m, MonadReader SQLGenCtx m) =>
  TriggerName ->
  QualifiedTable ->
  TriggerOnReplication ->
  [ColumnInfo ('Postgres pgKind)] ->
  TriggerOpsDef ('Postgres pgKind) ->
  m ()
mkAllTriggersQ triggerName table triggerOnReplication allCols fullspec = do
  for_ (tdInsert fullspec) (mkTrigger triggerName table triggerOnReplication allCols INSERT)
  for_ (tdUpdate fullspec) (mkTrigger triggerName table triggerOnReplication allCols UPDATE)
  for_ (tdDelete fullspec) (mkTrigger triggerName table triggerOnReplication allCols DELETE)

-- | Add cleanup logs for given trigger names and cleanup configs. This will perform the following steps:
--
--   1. Get last scheduled cleanup event and count.
--   2. If count is less than 5, then add add more cleanup logs, else do nothing
addCleanupSchedules ::
  (MonadIO m, MonadError QErr m) =>
  PGSourceConfig ->
  [(TriggerName, AutoTriggerLogCleanupConfig)] ->
  m ()
addCleanupSchedules sourceConfig triggersWithcleanupConfig =
  unless (null triggersWithcleanupConfig) $ do
    let triggerNames = map fst triggersWithcleanupConfig
    countAndLastSchedules <- liftEitherM $ liftIO $ runPgSourceReadTx sourceConfig $ selectLastCleanupScheduledTimestamp triggerNames
    currTime <- liftIO $ Time.getCurrentTime
    let triggerMap = HashMap.fromList $ map (\(triggerName, count, lastTime) -> (triggerName, (count, lastTime))) countAndLastSchedules
        scheduledTriggersAndTimestamps =
          mapMaybe
            ( \(triggerName, cleanupConfig) ->
                let lastScheduledTime = case HashMap.lookup triggerName triggerMap of
                      Nothing -> Just currTime
                      Just (count, lastTime) -> if count < 5 then (Just lastTime) else Nothing
                 in fmap
                      ( \lastScheduledTimestamp ->
                          (triggerName, generateScheduleTimes lastScheduledTimestamp cleanupSchedulesToBeGenerated (_atlccSchedule cleanupConfig))
                      )
                      lastScheduledTime
            )
            triggersWithcleanupConfig
    unless (null scheduledTriggersAndTimestamps)
      $ liftEitherM
      $ liftIO
      $ runPgSourceWriteTx sourceConfig InternalRawQuery
      $ insertEventTriggerCleanupLogsTx scheduledTriggersAndTimestamps

-- | Insert the cleanup logs for the fiven trigger name and schedules
insertEventTriggerCleanupLogsTx :: [(TriggerName, [Time.UTCTime])] -> PG.TxET QErr IO ()
insertEventTriggerCleanupLogsTx triggersWithschedules = do
  let insertCleanupEventsSql =
        TB.run
          $ toSQL
            S.SQLInsert
              { siTable = cleanupLogTable,
                siCols = map unsafePGCol ["trigger_name", "scheduled_at", "status"],
                siValues = S.ValuesExp $ concatMap genArr triggersWithschedules,
                siConflict = Just $ S.DoNothing Nothing,
                siRet = Nothing
              }
  PG.unitQE defaultTxErrorHandler (PG.fromText insertCleanupEventsSql) () False
  where
    cleanupLogTable = QualifiedObject "hdb_catalog" "hdb_event_log_cleanups"
    genArr (t, schedules) = map (toTupleExp . (\s -> [(triggerNameToTxt t), (formatTime' s), "scheduled"])) schedules
    toTupleExp = S.TupleExp . map S.SELit

-- | Get the last scheduled timestamp for a given event trigger name
selectLastCleanupScheduledTimestamp :: [TriggerName] -> PG.TxET QErr IO [(TriggerName, Int, Time.UTCTime)]
selectLastCleanupScheduledTimestamp triggerNames =
  PG.withQE
    defaultTxErrorHandler
    [PG.sql|
      SELECT trigger_name, count(1), max(scheduled_at)
      FROM hdb_catalog.hdb_event_log_cleanups
      WHERE status='scheduled' AND trigger_name = ANY($1::text[])
      GROUP BY trigger_name
    |]
    (Identity $ PGTextArray $ map triggerNameToTxt triggerNames)
    True

deleteAllScheduledCleanupsTx :: TriggerName -> PG.TxE QErr ()
deleteAllScheduledCleanupsTx triggerName = do
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
      DELETE from hdb_catalog.hdb_event_log_cleanups
      WHERE (status = 'scheduled') AND (trigger_name = $1)
    |]
    (Identity (triggerNameToTxt triggerName))
    True

-- | @deleteAllScheduledCleanups@ deletes all scheduled cleanup logs for a given event trigger
deleteAllScheduledCleanups ::
  (MonadIO m, MonadError QErr m) =>
  PGSourceConfig ->
  TriggerName ->
  m ()
deleteAllScheduledCleanups sourceConfig triggerName =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig InternalRawQuery $ deleteAllScheduledCleanupsTx triggerName

getCleanupEventsForDeletionTx :: PG.TxE QErr ([(Text, TriggerName)])
getCleanupEventsForDeletionTx =
  PG.withQE
    defaultTxErrorHandler
    [PG.sql|
          WITH latest_events as (
            SELECT * from hdb_catalog.hdb_event_log_cleanups WHERE status = 'scheduled' AND scheduled_at < (now() at time zone 'utc')
          ),
            grouped_events as (
              SELECT trigger_name, max(scheduled_at) as scheduled_at
                from latest_events
              group by trigger_name
            ),
            mark_events_as_dead as (
              UPDATE hdb_catalog.hdb_event_log_cleanups l
              SET status = 'dead'
              FROM grouped_events AS g
              WHERE l.trigger_name = g.trigger_name AND l.scheduled_at < g.scheduled_at AND l.status = 'scheduled'
            )
          SELECT l.id, l.trigger_name
            FROM latest_events l
                JOIN grouped_events g ON l.trigger_name = g.trigger_name
                WHERE l.scheduled_at = g.scheduled_at;
      |]
    ()
    False

-- | @getCleanupEventsForDeletion@ returns the cleanup logs that are to be deleted.
-- This will perform the following steps:
--
-- 1. Get the scheduled cleanup events that were scheduled before current time.
-- 2. If there are multiple entries for the same trigger name with different scheduled time,
--    then fetch the latest entry and mark others as dead.
getCleanupEventsForDeletion ::
  (MonadIO m, MonadError QErr m) =>
  PGSourceConfig ->
  m [(Text, TriggerName)]
getCleanupEventsForDeletion sourceConfig =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig InternalRawQuery $ getCleanupEventsForDeletionTx

markCleanupEventsAsDeadTx :: [Text] -> PG.TxE QErr ()
markCleanupEventsAsDeadTx toDeadEvents = do
  unless (null toDeadEvents)
    $ PG.unitQE
      defaultTxErrorHandler
      [PG.sql|
        UPDATE hdb_catalog.hdb_event_log_cleanups l
        SET status = 'dead'
        WHERE id = ANY($1::text[])
      |]
      (Identity $ PGTextArray toDeadEvents)
      True

-- unitQueryE HGE.defaultMSSQLTxErrorHandler $
--   rawUnescapedText . LT.toStrict $
--     $(makeRelativeToProject "src-rsr/mssql/event_logs_cleanup_sqls/mssql_update_events_to_dead.sql.shakespeare" >>= ST.stextFile)

updateCleanupEventStatusToDead ::
  (MonadIO m, MonadError QErr m) =>
  PGSourceConfig ->
  [Text] ->
  m ()
updateCleanupEventStatusToDead sourceConfig toDeadEvents =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig InternalRawQuery $ markCleanupEventsAsDeadTx toDeadEvents

updateCleanupEventStatusToPausedTx :: Text -> PG.TxE QErr ()
updateCleanupEventStatusToPausedTx cleanupLogId =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
          UPDATE hdb_catalog.hdb_event_log_cleanups
          SET status = 'paused'
          WHERE id = $1
          |]
    (Identity cleanupLogId)
    True

-- | @updateCleanupEventStatusToPaused@ updates the cleanup log status to `paused` if the event trigger configuration is paused.
updateCleanupEventStatusToPaused ::
  (MonadIO m, MonadError QErr m) =>
  PGSourceConfig ->
  Text ->
  m ()
updateCleanupEventStatusToPaused sourceConfig cleanupLogId =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig InternalRawQuery $ updateCleanupEventStatusToPausedTx cleanupLogId

updateCleanupEventStatusToCompletedTx :: Text -> DeletedEventLogStats -> PG.TxE QErr ()
updateCleanupEventStatusToCompletedTx cleanupLogId (DeletedEventLogStats numEventLogs numInvocationLogs) =
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
          UPDATE hdb_catalog.hdb_event_log_cleanups
          SET status = 'completed', deleted_event_logs = $2 , deleted_event_invocation_logs = $3
          WHERE id = $1
          |]
    (cleanupLogId, delLogs, delInvLogs)
    True
  where
    delLogs = (fromIntegral $ numEventLogs) :: Int64
    delInvLogs = (fromIntegral $ numInvocationLogs) :: Int64

-- | @updateCleanupEventStatusToCompleted@ updates the cleanup log status after the event logs are deleted.
-- This will perform the following steps:
--
-- 1. Updates the cleanup config status to `completed`.
-- 2. Updates the number of event logs and event invocation logs that were deleted for a trigger name
updateCleanupEventStatusToCompleted ::
  (MonadIO m, MonadError QErr m) =>
  PGSourceConfig ->
  Text ->
  DeletedEventLogStats ->
  m ()
updateCleanupEventStatusToCompleted sourceConfig cleanupLogId delStats =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig InternalRawQuery $ updateCleanupEventStatusToCompletedTx cleanupLogId delStats

deleteEventTriggerLogsTx :: TriggerLogCleanupConfig -> PG.TxE QErr DeletedEventLogStats
deleteEventTriggerLogsTx TriggerLogCleanupConfig {..} = do
  -- Setting the timeout
  PG.unitQE defaultTxErrorHandler (PG.fromText $ "SET statement_timeout = " <> (tshow qTimeout)) () True
  -- Select all the dead events based on criteria set in the cleanup config.
  deadEventIDs <-
    map runIdentity
      <$> PG.withQE
        defaultTxErrorHandler
        ( PG.fromText
            [ST.st|
          SELECT id FROM hdb_catalog.event_log
          WHERE ((delivered = true OR error = true) AND trigger_name = $1)
          AND created_at < now() - interval '#{qRetentionPeriod}'
          AND locked IS NULL
          LIMIT $2
        |]
        )
        (qTriggerName, qBatchSize)
        True
  --  Lock the events in the database so that other HGE instances don't pick them up for deletion.
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
      UPDATE hdb_catalog.event_log
      SET locked = now()
      WHERE id = ANY($1::text[]);
    |]
    (Identity $ PGTextArray $ map unEventId deadEventIDs)
    True
  --  Based on the config either delete the corresponding invocation logs or set trigger_name
  --  to appropriate value. Please note that the event_id won't exist anymore in the event_log
  --  table, but we are still retaining it for debugging purpose.
  deletedInvocationLogs <-
    if tlccCleanInvocationLogs
      then
        runIdentity
          . PG.getRow
          <$> PG.withQE
            defaultTxErrorHandler
            [PG.sql|
              WITH deletedInvocations AS (
                DELETE FROM hdb_catalog.event_invocation_logs
                WHERE event_id = ANY($1::text[])
                RETURNING 1
              )
              SELECT count(*) FROM deletedInvocations;
            |]
            (Identity $ PGTextArray $ map unEventId deadEventIDs)
            True
      else do
        PG.unitQE
          defaultTxErrorHandler
          [PG.sql|
            UPDATE hdb_catalog.event_invocation_logs
            SET trigger_name = $2
            WHERE event_id = ANY($1::text[])
          |]
          (PGTextArray $ map unEventId deadEventIDs, qTriggerName)
          True
        pure 0
  --  Finally delete the event logs.
  deletedEventLogs <-
    runIdentity
      . PG.getRow
      <$> PG.withQE
        defaultTxErrorHandler
        [PG.sql|
          WITH deletedEvents AS (
            DELETE FROM hdb_catalog.event_log
            WHERE id = ANY($1::text[])
            RETURNING 1
          )
          SELECT count(*) FROM deletedEvents;
        |]
        (Identity $ PGTextArray $ map unEventId deadEventIDs)
        True
  -- Resetting the timeout to default value (0)
  PG.unitQE
    defaultTxErrorHandler
    [PG.sql|
      SET statement_timeout = 0;
    |]
    ()
    False
  pure DeletedEventLogStats {..}
  where
    qTimeout = (fromIntegral $ tlccTimeout * 1000) :: Int64
    qTriggerName = triggerNameToTxt tlccEventTriggerName
    qRetentionPeriod = tshow tlccClearOlderThan <> " hours"
    qBatchSize = (fromIntegral tlccBatchSize) :: Int64

-- | @deleteEventTriggerLogs@ deletes the event logs (and event invocation logs) based on the cleanup configuration given
-- This will perform the following steps:
--
-- 1. Select all the dead events based on criteria set in the cleanup config.
-- 2. Lock the events in the database so that other HGE instances don't pick them up for deletion.
-- 3. Based on the config, perform the delete action.
deleteEventTriggerLogs ::
  (MonadIO m, MonadError QErr m) =>
  PGSourceConfig ->
  TriggerLogCleanupConfig ->
  IO (Maybe (TriggerLogCleanupConfig, EventTriggerCleanupStatus)) ->
  m DeletedEventLogStats
deleteEventTriggerLogs sourceConfig oldCleanupConfig getLatestCleanupConfig = do
  deleteEventTriggerLogsInBatchesWith getLatestCleanupConfig oldCleanupConfig $ \cleanupConfig -> do
    runPgSourceWriteTx sourceConfig InternalRawQuery $ deleteEventTriggerLogsTx cleanupConfig

fetchEventLogs ::
  (MonadError QErr m, MonadIO m) =>
  PGSourceConfig ->
  GetEventLogs b ->
  m [EventLog]
fetchEventLogs sourceConfig getEventLogs = do
  liftIO (runPgSourceReadTx sourceConfig $ fetchEventLogsTxE getEventLogs)
    `onLeftM` (throwError . prefixQErr "unexpected error while fetching event logs: ")

fetchEventLogsTxE :: GetEventLogs b -> PG.TxE QErr [EventLog]
fetchEventLogsTxE GetEventLogs {..} = do
  case status of
    Pending -> do
      map uncurryEventLog
        <$> PG.withQE
          defaultTxErrorHandler
          [PG.sql|
            SELECT id, schema_name, table_name, trigger_name, payload, delivered, error, tries, created_at, locked, next_retry_at, archived
              FROM hdb_catalog.event_log
              WHERE trigger_name = $1 
              AND delivered=false AND error=false AND archived=false ORDER BY created_at DESC LIMIT $2 OFFSET $3;
            |]
          (triggerName, limit, offset)
          True
    Processed -> do
      map uncurryEventLog
        <$> PG.withQE
          defaultTxErrorHandler
          [PG.sql|
            SELECT id, schema_name, table_name, trigger_name, payload, delivered, error, tries, created_at, locked, next_retry_at, archived
              FROM hdb_catalog.event_log
              WHERE trigger_name = $1 
              AND (delivered=true OR error=true) AND archived=false ORDER BY created_at DESC LIMIT $2 OFFSET $3;
            |]
          (triggerName, limit, offset)
          True
    All -> do
      map uncurryEventLog
        <$> PG.withQE
          defaultTxErrorHandler
          [PG.sql|
            SELECT id, schema_name, table_name, trigger_name, payload, delivered, error, tries, created_at, locked, next_retry_at, archived
              FROM hdb_catalog.event_log
              WHERE trigger_name = $1 
              ORDER BY created_at DESC LIMIT $2 OFFSET $3;
            |]
          (triggerName, limit, offset)
          True
  where
    triggerName = triggerNameToTxt _gelName
    status = _gelStatus
    limit :: Int64 = fromIntegral $ _gelLimit
    offset :: Int64 = fromIntegral $ _gelOffset

fetchEventInvocationLogs ::
  (MonadError QErr m, MonadIO m) =>
  PGSourceConfig ->
  GetEventInvocations b ->
  m [EventInvocationLog]
fetchEventInvocationLogs sourceConfig getEventInvocationLogs = do
  liftIO (runPgSourceReadTx sourceConfig $ fetchEventInvocationLogsTxE getEventInvocationLogs)
    `onLeftM` (throwError . prefixQErr "unexpected error while fetching invocation logs: ")

fetchEventInvocationLogsTxE :: GetEventInvocations b -> PG.TxE QErr [EventInvocationLog]
fetchEventInvocationLogsTxE GetEventInvocations {..} = do
  map uncurryEventInvocationLog
    <$> PG.withQE
      defaultTxErrorHandler
      [PG.sql|
        SELECT id, trigger_name, event_id, status, request, response, created_at
          FROM hdb_catalog.event_invocation_logs
          WHERE trigger_name = $1 
          ORDER BY created_at DESC LIMIT $2 OFFSET $3;
        |]
      (triggerName, limit, offset)
      True
  where
    triggerName = triggerNameToTxt _geiName
    limit :: Int64 = fromIntegral $ _geiLimit
    offset :: Int64 = fromIntegral $ _geiOffset

fetchEventById ::
  (MonadError QErr m, MonadIO m) =>
  PGSourceConfig ->
  GetEventById b ->
  m (EventLogWithInvocations)
fetchEventById sourceConfig getEventById = do
  fetchEventByIdTxE' <- liftIO $ runPgSourceReadTx sourceConfig $ fetchEventByIdTxE getEventById
  case fetchEventByIdTxE' of
    Left err ->
      throwError
        $ prefixQErr ("unexpected error while fetching event with id " <> eventId <> ": ") err
    Right eventLogWithInvocations -> do
      if isNothing (elwiEvent eventLogWithInvocations)
        then throw400 NotExists errMsg
        else return eventLogWithInvocations
  where
    eventId = unEventId $ _gebiEventId getEventById
    errMsg = "event id " <> eventId <> " does not exist"

fetchEventByIdTxE :: GetEventById b -> PG.TxE QErr (EventLogWithInvocations)
fetchEventByIdTxE GetEventById {..} = do
  events <-
    map uncurryEventLog
      <$> PG.withQE
        defaultTxErrorHandler
        [PG.sql|
          SELECT id, schema_name, table_name, trigger_name, payload, delivered, error, tries, created_at, locked, next_retry_at, archived
            FROM hdb_catalog.event_log
            WHERE id = $1;
          |]
        (Identity eventId)
        True
  case events of
    [] -> return $ EventLogWithInvocations Nothing []
    [event] -> do
      invocations <-
        map uncurryEventInvocationLog
          <$> PG.withQE
            defaultTxErrorHandler
            [PG.sql|
              SELECT id, trigger_name, event_id, status, request, response, created_at
                FROM hdb_catalog.event_invocation_logs
                WHERE event_id = $1
                ORDER BY created_at DESC LIMIT $2 OFFSET $3;
              |]
            (eventId, limit, offset)
            True
      pure $ EventLogWithInvocations (Just event) invocations
    _ -> throw500 $ "Unexpected error: Multiple events present with event id " <> eventId
  where
    eventId = unEventId _gebiEventId
    limit :: Int64 = fromIntegral $ _gebiInvocationLogLimit
    offset :: Int64 = fromIntegral $ _gebiInvocationLogOffset

uncurryEventLog ::
  (EventId, Text, Text, TriggerName, PG.ViaJSON Value, Bool, Bool, Int, Time.UTCTime, Maybe Time.UTCTime, Maybe Time.UTCTime, Bool) ->
  EventLog
uncurryEventLog (eventId, schemaName, tableName, triggerName, PG.ViaJSON payload, delivered, isError, tries, createdAt, locked, nextRetryAt, archived) =
  EventLog
    { elId = eventId,
      elSchemaName = schemaName,
      elTableName = tableName,
      elTriggerName = triggerName,
      elPayload = payload,
      elDelivered = delivered,
      elError = isError,
      elTries = tries,
      elCreatedAt = createdAt,
      elLocked = locked,
      elNextRetryAt = nextRetryAt,
      elArchived = archived
    }

uncurryEventInvocationLog ::
  (Text, TriggerName, EventId, Maybe Int, PG.ViaJSON Value, PG.ViaJSON Value, Time.UTCTime) ->
  EventInvocationLog
uncurryEventInvocationLog (invocationId, triggerName, eventId, status, PG.ViaJSON request, PG.ViaJSON response, createdAt) =
  EventInvocationLog
    { eilId = invocationId,
      eilTriggerName = triggerName,
      eilEventId = eventId,
      eilHttpStatus = status,
      eilRequest = request,
      eilResponse = response,
      eilCreatedAt = createdAt
    }
