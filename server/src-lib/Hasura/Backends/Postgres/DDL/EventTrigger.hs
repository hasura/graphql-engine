-- | Postgres DDL EventTrigger
--
-- Used for creating event triggers for metadata changes.
--
-- See 'Hasura.RQL.DDL.Schema.Cache' and 'Hasura.RQL.Types.Eventing.Backend'.
module Hasura.Backends.Postgres.DDL.EventTrigger
  ( insertManualEvent,
    redeliverEvent,
    dropTriggerAndArchiveEvents,
    createTableEventTrigger,
    dropTriggerQ,
    mkAllTriggersQ,
    getMaintenanceModeVersion,
    fetchUndeliveredEvents,
    setRetry,
    recordSuccess,
    recordError,
    recordError',
    unlockEventsInSource,
    updateColumnInEventTrigger,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.FileEmbed (makeRelativeToProject)
import Data.Int (Int64)
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Data.Time.Clock qualified as Time
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.SQL.DML
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Backends.Postgres.Translate.Column
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend (Backend, SourceConfig, TableName)
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table ()
import Hasura.SQL.Backend
import Hasura.SQL.Types
import Hasura.Server.Migrate.Internal
import Hasura.Server.Migrate.Version
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Text.Shakespeare.Text qualified as ST

-- Corresponds to the 'OLD' and 'NEW' Postgres records; see
-- https://www.postgresql.org/docs/current/plpgsql-trigger.html
data OpVar = OLD | NEW deriving (Show)

fetchUndeliveredEvents ::
  (MonadIO m, MonadError QErr m) =>
  SourceConfig ('Postgres pgKind) ->
  SourceName ->
  MaintenanceMode ->
  FetchBatchSize ->
  m [Event ('Postgres pgKind)]
fetchUndeliveredEvents sourceConfig sourceName maintenanceMode fetchBatchSize = do
  fetchEventsTxE <-
    case maintenanceMode of
      MaintenanceModeEnabled -> do
        maintenanceModeVersion <- liftIO $ runPgSourceReadTx sourceConfig getMaintenanceModeVersionTx
        pure $ fmap (fetchEventsMaintenanceMode sourceName fetchBatchSize) maintenanceModeVersion
      MaintenanceModeDisabled -> pure $ Right $ fetchEvents sourceName fetchBatchSize
  case fetchEventsTxE of
    Left err -> throw500 $ "something went wrong while fetching events: " <> tshow err
    Right fetchEventsTx ->
      liftEitherM $
        liftIO $
          runPgSourceWriteTx sourceConfig fetchEventsTx

setRetry ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  SourceConfig ('Postgres pgKind) ->
  Event ('Postgres pgKind) ->
  Time.UTCTime ->
  Maybe MaintenanceModeVersion ->
  m ()
setRetry sourceConfig event retryTime maintenanceModeVersion =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig (setRetryTx event retryTime maintenanceModeVersion)

insertManualEvent ::
  (MonadIO m, MonadError QErr m) =>
  SourceConfig ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  TriggerName ->
  Value ->
  UserInfo ->
  Tracing.TraceContext ->
  m EventId
insertManualEvent sourceConfig tableName triggerName payload userInfo traceCtx =
  -- NOTE: The methods `setTraceContextInTx` and `setHeadersTx` are being used
  -- to ensure that the trace context and user info are set with valid values
  -- while being used in the PG function `insert_event_log`.
  -- See Issue(#7087) for more details on a bug that was being caused
  -- in the absence of these methods.
  liftEitherM $
    liftIO $
      runPgSourceWriteTx sourceConfig $
        setHeadersTx (_uiSession userInfo)
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
  Maybe MaintenanceModeVersion ->
  m (Either QErr ())
recordSuccess sourceConfig event invocation maintenanceModeVersion =
  liftIO $
    runPgSourceWriteTx sourceConfig $ do
      insertInvocation invocation
      setSuccessTx event maintenanceModeVersion

recordError ::
  (MonadIO m) =>
  SourceConfig ('Postgres pgKind) ->
  Event ('Postgres pgKind) ->
  Invocation 'EventType ->
  ProcessEventError ->
  Maybe MaintenanceModeVersion ->
  m (Either QErr ())
recordError sourceConfig event invocation processEventError maintenanceModeVersion =
  recordError' sourceConfig event (Just invocation) processEventError maintenanceModeVersion

recordError' ::
  (MonadIO m) =>
  SourceConfig ('Postgres pgKind) ->
  Event ('Postgres pgKind) ->
  Maybe (Invocation 'EventType) ->
  ProcessEventError ->
  Maybe MaintenanceModeVersion ->
  m (Either QErr ())
recordError' sourceConfig event invocation processEventError maintenanceModeVersion =
  liftIO $
    runPgSourceWriteTx sourceConfig $ do
      onJust invocation insertInvocation
      case processEventError of
        PESetRetry retryTime -> setRetryTx event retryTime maintenanceModeVersion
        PESetError -> setErrorTx event maintenanceModeVersion

redeliverEvent ::
  (MonadIO m, MonadError QErr m) =>
  SourceConfig ('Postgres pgKind) ->
  EventId ->
  m ()
redeliverEvent sourceConfig eventId =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig (redeliverEventTx eventId)

dropTriggerAndArchiveEvents ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  SourceConfig ('Postgres pgKind) ->
  TriggerName ->
  m ()
dropTriggerAndArchiveEvents sourceConfig triggerName =
  liftEitherM $
    liftIO $
      runPgSourceWriteTx sourceConfig $ do
        dropTriggerQ triggerName
        archiveEvents triggerName

createTableEventTrigger ::
  (Backend ('Postgres pgKind), MonadIO m, MonadBaseControl IO m) =>
  ServerConfigCtx ->
  PGSourceConfig ->
  QualifiedTable ->
  [ColumnInfo ('Postgres pgKind)] ->
  TriggerName ->
  TriggerOpsDef ('Postgres pgKind) ->
  m (Either QErr ())
createTableEventTrigger serverConfigCtx sourceConfig table columns triggerName opsDefinition = runPgSourceWriteTx sourceConfig $ do
  -- Clean all existing triggers
  liftTx $ dropTriggerQ triggerName -- executes DROP IF EXISTS.. sql
  -- Create the given triggers
  flip runReaderT serverConfigCtx $
    mkAllTriggersQ triggerName table columns opsDefinition

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
  MonadIO m =>
  SourceConfig ('Postgres pgKind) ->
  Set.Set EventId ->
  m (Either QErr Int)
unlockEventsInSource sourceConfig eventIds =
  liftIO $ runPgSourceWriteTx sourceConfig (unlockEventsTx $ toList eventIds)

---- DATABASE QUERIES ---------------------
--
--   The API for our in-database work queue:
-------------------------------------------

insertInvocation :: Invocation 'EventType -> Q.TxE QErr ()
insertInvocation invo = do
  Q.unitQE
    defaultTxErrorHandler
    [Q.sql|
          INSERT INTO hdb_catalog.event_invocation_logs (event_id, status, request, response)
          VALUES ($1, $2, $3, $4)
          |]
    ( iEventId invo,
      fromIntegral <$> iStatus invo :: Maybe Int64,
      Q.AltJ $ toJSON $ iRequest invo,
      Q.AltJ $ toJSON $ iResponse invo
    )
    True
  Q.unitQE
    defaultTxErrorHandler
    [Q.sql|
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
  Q.TxE QErr EventId
insertPGManualEvent (QualifiedObject schemaName tableName) triggerName rowData = do
  runIdentity . Q.getRow
    <$> Q.withQE
      defaultTxErrorHandler
      [Q.sql|
    SELECT hdb_catalog.insert_event_log($1, $2, $3, $4, $5)
  |]
      (schemaName, tableName, triggerName, (tshow MANUAL), Q.AltJ rowData)
      False

archiveEvents :: TriggerName -> Q.TxE QErr ()
archiveEvents trn =
  Q.unitQE
    defaultTxErrorHandler
    [Q.sql|
           UPDATE hdb_catalog.event_log
           SET archived = 't'
           WHERE trigger_name = $1
                |]
    (Identity trn)
    False

getMaintenanceModeVersionTx :: Q.TxE QErr MaintenanceModeVersion
getMaintenanceModeVersionTx = liftTx $ do
  catalogVersion <- getCatalogVersion -- From the user's DB
  -- the previous version and the current version will change depending
  -- upon between which versions we need to support maintenance mode
  if
      | catalogVersion == 40 -> pure PreviousMMVersion
      -- The catalog is migrated to the 43rd version for a source
      -- which was initialised by a v1 graphql-engine instance (See @initSource@).
      | catalogVersion == 43 -> pure CurrentMMVersion
      | catalogVersion == fromInteger latestCatalogVersion -> pure CurrentMMVersion
      | otherwise ->
        throw500 $
          "Maintenance mode is only supported with catalog versions: 40, 43 and "
            <> tshow latestCatalogVersionString

-- | Lock and return events not yet being processed or completed, up to some
-- limit. Process events approximately in created_at order, but we make no
-- ordering guarentees; events can and will race. Nevertheless we want to
-- ensure newer change events don't starve older ones.
fetchEvents :: SourceName -> FetchBatchSize -> Q.TxE QErr [Event ('Postgres pgKind)]
fetchEvents source (FetchBatchSize fetchBatchSize) =
  map uncurryEvent
    <$> Q.listQE
      defaultTxErrorHandler
      [Q.sql|
      UPDATE hdb_catalog.event_log
      SET locked = NOW()
      WHERE id IN ( SELECT l.id
                    FROM hdb_catalog.event_log l
                    WHERE l.delivered = 'f' and l.error = 'f'
                          and (l.locked IS NULL or l.locked < (NOW() - interval '30 minute'))
                          and (l.next_retry_at is NULL or l.next_retry_at <= now())
                          and l.archived = 'f'
                    /* NB: this ordering is important for our index `event_log_fetch_events` */
                    /* (see `init_pg_source.sql`) */
                    ORDER BY locked NULLS FIRST, next_retry_at NULLS FIRST, created_at
                    LIMIT $1
                    FOR UPDATE SKIP LOCKED )
      RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at
      |]
      (Identity limit)
      True
  where
    uncurryEvent (id', sn, tn, trn, Q.AltJ payload, tries, created) =
      Event
        { eId = id',
          eSource = source,
          eTable = QualifiedObject sn tn,
          eTrigger = TriggerMetadata trn,
          eEvent = payload,
          eTries = tries,
          eCreatedAt = created
        }
    limit = fromIntegral fetchBatchSize :: Word64

fetchEventsMaintenanceMode :: SourceName -> FetchBatchSize -> MaintenanceModeVersion -> Q.TxE QErr [Event ('Postgres pgKind)]
fetchEventsMaintenanceMode sourceName fetchBatchSize = \case
  PreviousMMVersion ->
    map uncurryEvent
      <$> Q.listQE
        defaultTxErrorHandler
        [Q.sql|
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
        RETURNING id, schema_name, table_name, trigger_name, payload::json, tries, created_at
        |]
        (Identity limit)
        True
    where
      uncurryEvent (id', sn, tn, trn, Q.AltJ payload, tries, created) =
        Event
          { eId = id',
            eSource = SNDefault, -- in v1, there'll only be the default source
            eTable = QualifiedObject sn tn,
            eTrigger = TriggerMetadata trn,
            eEvent = payload,
            eTries = tries,
            eCreatedAt = created
          }
      limit = fromIntegral (_unFetchBatchSize fetchBatchSize) :: Word64
  CurrentMMVersion -> fetchEvents sourceName fetchBatchSize

setSuccessTx :: Event ('Postgres pgKind) -> Maybe MaintenanceModeVersion -> Q.TxE QErr ()
setSuccessTx e = \case
  Just PreviousMMVersion ->
    Q.unitQE
      defaultTxErrorHandler
      [Q.sql|
    UPDATE hdb_catalog.event_log
    SET delivered = 't', next_retry_at = NULL, locked = 'f'
    WHERE id = $1
    |]
      (Identity $ eId e)
      True
  Just CurrentMMVersion -> latestVersionSetSuccess
  Nothing -> latestVersionSetSuccess
  where
    latestVersionSetSuccess =
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
      UPDATE hdb_catalog.event_log
      SET delivered = 't', next_retry_at = NULL, locked = NULL
      WHERE id = $1
      |]
        (Identity $ eId e)
        True

setErrorTx :: Event ('Postgres pgKind) -> Maybe MaintenanceModeVersion -> Q.TxE QErr ()
setErrorTx e = \case
  Just PreviousMMVersion ->
    Q.unitQE
      defaultTxErrorHandler
      [Q.sql|
    UPDATE hdb_catalog.event_log
    SET error = 't', next_retry_at = NULL, locked = 'f'
    WHERE id = $1
    |]
      (Identity $ eId e)
      True
  Just CurrentMMVersion -> latestVersionSetError
  Nothing -> latestVersionSetError
  where
    latestVersionSetError =
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
      UPDATE hdb_catalog.event_log
      SET error = 't', next_retry_at = NULL, locked = NULL
      WHERE id = $1
      |]
        (Identity $ eId e)
        True

setRetryTx :: Event ('Postgres pgKind) -> Time.UTCTime -> Maybe MaintenanceModeVersion -> Q.TxE QErr ()
setRetryTx e time = \case
  Just PreviousMMVersion ->
    Q.unitQE
      defaultTxErrorHandler
      [Q.sql|
    UPDATE hdb_catalog.event_log
    SET next_retry_at = $1, locked = 'f'
    WHERE id = $2
    |]
      (time, eId e)
      True
  Just CurrentMMVersion -> latestVersionSetRetry
  Nothing -> latestVersionSetRetry
  where
    latestVersionSetRetry =
      Q.unitQE
        defaultTxErrorHandler
        [Q.sql|
              UPDATE hdb_catalog.event_log
              SET next_retry_at = $1, locked = NULL
              WHERE id = $2
              |]
        (time, eId e)
        True

dropTriggerQ :: TriggerName -> Q.TxE QErr ()
dropTriggerQ trn =
  mapM_
    ( \op ->
        Q.unitQE
          defaultTxErrorHandler
          (Q.fromText $ getDropFuncSql op)
          ()
          False
    )
    [INSERT, UPDATE, DELETE]
  where
    getDropFuncSql :: Ops -> Text
    getDropFuncSql op =
      "DROP FUNCTION IF EXISTS"
        <> " hdb_catalog."
        <> pgIdenTrigger op trn
        <> "()"
        <> " CASCADE"

checkEvent :: EventId -> Q.TxE QErr ()
checkEvent eid = do
  events <-
    Q.listQE
      defaultTxErrorHandler
      [Q.sql|
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
      when locked $
        throw400 Busy "event is already being processed"

markForDelivery :: EventId -> Q.TxE QErr ()
markForDelivery eid =
  Q.unitQE
    defaultTxErrorHandler
    [Q.sql|
          UPDATE hdb_catalog.event_log
          SET
          delivered = 'f',
          error = 'f',
          tries = 0
          WHERE id = $1
          |]
    (Identity eid)
    True

redeliverEventTx :: EventId -> Q.TxE QErr ()
redeliverEventTx eventId = do
  checkEvent eventId
  markForDelivery eventId

-- | unlockEvents takes an array of 'EventId' and unlocks them. This function is called
--   when a graceful shutdown is initiated.
unlockEventsTx :: [EventId] -> Q.TxE QErr Int
unlockEventsTx eventIds =
  runIdentity . Q.getRow
    <$> Q.withQE
      defaultTxErrorHandler
      [Q.sql|
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

-- | pgIdenTrigger is a method used to construct the name of the pg function
-- used for event triggers which are present in the hdb_catalog schema.
pgIdenTrigger :: Ops -> TriggerName -> Text
pgIdenTrigger op trn = pgFmtIdentifier . qualifyTriggerName op $ triggerNameToTxt trn
  where
    qualifyTriggerName op' trn' = "notify_hasura_" <> trn' <> "_" <> tshow op'

-- | Define the pgSQL trigger functions on database events.
mkTriggerQ ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadTx m, MonadReader ServerConfigCtx m) =>
  TriggerName ->
  QualifiedTable ->
  [ColumnInfo ('Postgres pgKind)] ->
  Ops ->
  SubscribeOpSpec ('Postgres pgKind) ->
  m ()
mkTriggerQ trn qt@(QualifiedObject schema table) allCols op (SubscribeOpSpec listenColumns deliveryColumns') = do
  strfyNum <- stringifyNum . _sccSQLGenCtx <$> ask
  liftTx $
    Q.multiQE defaultTxErrorHandler $
      Q.fromText . TL.toStrict $
        let -- If there are no specific delivery columns selected by user then all the columns will be delivered
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

            name = triggerNameToTxt trn
            qualifiedTriggerName = pgIdenTrigger op trn
            qualifiedTable = toSQLTxt qt
            schemaName = pgFmtLit $ getSchemaTxt schema
            tableName = pgFmtLit $ getTableTxt table

            operation = tshow op
            oldRow = toSQLTxt $ renderRow OLD
            newRow = toSQLTxt $ renderRow NEW
            oldPayloadExpression = toSQLTxt oldDataExp
            newPayloadExpression = toSQLTxt newDataExp
         in $(makeRelativeToProject "src-rsr/trigger.sql.shakespeare" >>= ST.stextFile)
  where
    applyRowToJson' e = SEFnApp "row_to_json" [e] Nothing
    applyRow e = SEFnApp "row" [e] Nothing
    opToQual = QualVar . tshow

    mkRowExpression opVar strfyNum columns =
      mkRowExp $ map (\col -> toExtractor (mkQId opVar strfyNum col) col) columns

    mkQId opVar strfyNum colInfo =
      toJSONableExp strfyNum (ciType colInfo) False $
        SEQIdentifier $ QIdentifier (opToQual opVar) $ toIdentifier $ ciColumn colInfo

    -- Generate the SQL expression
    toExtractor sqlExp column
      -- If the column type is either 'Geography' or 'Geometry', then after applying the 'ST_AsGeoJSON' function
      -- to the column, alias the value of the expression with the column name else it uses `st_asgeojson` as
      -- the column name.
      | isScalarColumnWhere isGeoType (ciType column) = Extractor sqlExp (Just $ getAlias column)
      | otherwise = Extractor sqlExp Nothing
    getAlias col = toAlias $ Identifier $ getPGColTxt (ciColumn col)

mkAllTriggersQ ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadTx m, MonadReader ServerConfigCtx m) =>
  TriggerName ->
  QualifiedTable ->
  [ColumnInfo ('Postgres pgKind)] ->
  TriggerOpsDef ('Postgres pgKind) ->
  m ()
mkAllTriggersQ trn qt allCols fullspec = do
  onJust (tdInsert fullspec) (mkTriggerQ trn qt allCols INSERT)
  onJust (tdUpdate fullspec) (mkTriggerQ trn qt allCols UPDATE)
  onJust (tdDelete fullspec) (mkTriggerQ trn qt allCols DELETE)
