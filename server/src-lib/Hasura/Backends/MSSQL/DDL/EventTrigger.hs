{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.MSSQL.DDL.EventTrigger
  ( createTableEventTrigger,
    fetchUndeliveredEvents,
    setRetry,
    recordSuccess,
    recordError,
    recordError',
    dropTriggerAndArchiveEvents,
    dropDanglingSQLTrigger,
    redeliverEvent,
    insertManualEvent,
    unlockEventsInSource,
    getMaintenanceModeVersion,
    qualifyTableName,
  )
where

import Data.Aeson qualified as J
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (makeRelativeToProject)
import Data.Set.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated, toTxt)
import Data.Text.Lazy qualified as LT
import Data.Text.NonEmpty (mkNonEmptyTextUnsafe)
import Data.Time
import Database.MSSQL.Transaction (TxE, multiRowQueryE, singleRowQueryE, unitQueryE)
import Database.ODBC.SQLServer (Datetime2 (..), rawUnescapedText, toSql)
import Database.ODBC.TH qualified as ODBC
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.DDL.Source.Version
import Hasura.Backends.MSSQL.SQL.Error qualified as HGE
import Hasura.Backends.MSSQL.ToQuery (fromTableName, toQueryFlat)
import Hasura.Backends.MSSQL.Types (SchemaName (..), TableName (..))
import Hasura.Backends.MSSQL.Types.Internal (columnNameText, geoTypes)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing (EventId (..), OpVar (..))
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table (PrimaryKey (..))
import Hasura.SQL.Backend
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Text.Shakespeare.Text qualified as ST

fetchUndeliveredEvents ::
  (MonadIO m, MonadError QErr m) =>
  MSSQLSourceConfig ->
  SourceName ->
  [TriggerName] ->
  MaintenanceMode () ->
  FetchBatchSize ->
  m [Event 'MSSQL]
fetchUndeliveredEvents sourceConfig sourceName triggerNames _ fetchBatchSize = do
  liftEitherM $
    liftIO $
      runMSSQLSourceWriteTx sourceConfig $
        fetchEvents sourceName triggerNames fetchBatchSize

setRetry ::
  (MonadIO m, MonadError QErr m) =>
  MSSQLSourceConfig ->
  Event 'MSSQL ->
  UTCTime ->
  MaintenanceMode MaintenanceModeVersion ->
  m ()
setRetry sourceConfig event retryTime maintenanceModeVersion = do
  liftEitherM $
    liftIO $
      runMSSQLSourceWriteTx sourceConfig $
        setRetryTx event retryTime maintenanceModeVersion

insertManualEvent ::
  (MonadIO m, MonadError QErr m) =>
  MSSQLSourceConfig ->
  TableName ->
  TriggerName ->
  J.Value ->
  UserInfo ->
  Tracing.TraceContext ->
  m EventId
insertManualEvent sourceConfig tableName triggerName payload _userInfo _traceCtx =
  liftEitherM $
    liftIO $
      runMSSQLSourceWriteTx sourceConfig $
        -- TODO: Include TraceContext in payload
        insertMSSQLManualEventTx tableName triggerName payload

getMaintenanceModeVersion ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  MSSQLSourceConfig ->
  m MaintenanceModeVersion
getMaintenanceModeVersion sourceConfig =
  liftEitherM $
    liftIO $
      runMSSQLSourceReadTx sourceConfig $ getMaintenanceModeVersionTx

recordSuccess ::
  (MonadIO m) =>
  MSSQLSourceConfig ->
  Event 'MSSQL ->
  Invocation 'EventType ->
  MaintenanceMode MaintenanceModeVersion ->
  m (Either QErr ())
recordSuccess sourceConfig event invocation maintenanceModeVersion =
  liftIO $
    runMSSQLSourceWriteTx sourceConfig $ do
      insertInvocation invocation
      setSuccessTx event maintenanceModeVersion

recordError ::
  (MonadIO m) =>
  MSSQLSourceConfig ->
  Event 'MSSQL ->
  Invocation 'EventType ->
  ProcessEventError ->
  MaintenanceMode MaintenanceModeVersion ->
  m (Either QErr ())
recordError sourceConfig event invocation processEventError maintenanceModeVersion =
  recordError' sourceConfig event (Just invocation) processEventError maintenanceModeVersion

recordError' ::
  (MonadIO m) =>
  MSSQLSourceConfig ->
  Event 'MSSQL ->
  Maybe (Invocation 'EventType) ->
  ProcessEventError ->
  MaintenanceMode MaintenanceModeVersion ->
  m (Either QErr ())
recordError' sourceConfig event invocation processEventError maintenanceModeVersion =
  liftIO $
    runMSSQLSourceWriteTx sourceConfig $ do
      onJust invocation insertInvocation
      case processEventError of
        PESetRetry retryTime -> do
          setRetryTx event retryTime maintenanceModeVersion
        PESetError -> setErrorTx event maintenanceModeVersion

redeliverEvent ::
  (MonadIO m, MonadError QErr m) =>
  MSSQLSourceConfig ->
  EventId ->
  m ()
redeliverEvent sourceConfig eventId =
  liftEitherM $
    liftIO $
      runMSSQLSourceWriteTx sourceConfig $ do
        checkEventTx eventId
        markForDeliveryTx eventId

dropTriggerAndArchiveEvents ::
  (MonadIO m, MonadError QErr m) =>
  MSSQLSourceConfig ->
  TriggerName ->
  TableName ->
  m ()
dropTriggerAndArchiveEvents sourceConfig triggerName table =
  liftEitherM $
    liftIO $
      runMSSQLSourceWriteTx sourceConfig $ do
        dropTriggerQ triggerName (tableSchema table)
        archiveEvents triggerName

dropDanglingSQLTrigger ::
  (MonadIO m, MonadError QErr m) =>
  MSSQLSourceConfig ->
  TriggerName ->
  TableName ->
  HashSet Ops ->
  m ()
dropDanglingSQLTrigger sourceConfig triggerName table ops =
  liftEitherM $
    liftIO $
      runMSSQLSourceWriteTx sourceConfig $ do
        traverse_ (dropTriggerOp triggerName (tableSchema table)) ops

createTableEventTrigger ::
  (MonadIO m) =>
  ServerConfigCtx ->
  MSSQLSourceConfig ->
  TableName ->
  [ColumnInfo 'MSSQL] ->
  TriggerName ->
  TriggerOpsDef 'MSSQL ->
  Maybe (PrimaryKey 'MSSQL (ColumnInfo 'MSSQL)) ->
  m (Either QErr ())
createTableEventTrigger _serverConfigCtx sourceConfig table columns triggerName opsDefinition primaryKeyMaybe = do
  liftIO $
    runMSSQLSourceWriteTx sourceConfig $ do
      dropTriggerQ triggerName (tableSchema table)
      mkAllTriggersQ triggerName table columns opsDefinition primaryKeyMaybe

unlockEventsInSource ::
  MonadIO m =>
  MSSQLSourceConfig ->
  NE.NESet EventId ->
  m (Either QErr Int)
unlockEventsInSource sourceConfig eventIds =
  liftIO $
    runMSSQLSourceWriteTx sourceConfig $ do
      unlockEventsTx $ toList eventIds

---- DATABASE QUERIES ---------------------
--
--   The API for our in-database work queue:
-------------------------------------------

insertInvocation :: Invocation 'EventType -> TxE QErr ()
insertInvocation invo = do
  unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      INSERT INTO hdb_catalog.event_invocation_logs (event_id, status, request, response)
          VALUES ($invoEventId, $invoStatus, $invoRequest, $invoResponse)
    |]

  unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      UPDATE hdb_catalog.event_log

      SET tries = tries + 1
      WHERE id = $invoEventId
    |]
  where
    invoEventId = unEventId $ iEventId invo
    invoStatus = fromIntegral <$> iStatus invo :: Maybe Int
    invoRequest = J.encode $ J.toJSON $ iRequest invo
    invoResponse = J.encode $ J.toJSON $ iResponse invo

insertMSSQLManualEventTx ::
  TableName ->
  TriggerName ->
  J.Value ->
  TxE QErr EventId
insertMSSQLManualEventTx (TableName tableName (SchemaName schemaName)) triggerName rowData = do
  eventId <-
    singleRowQueryE
      HGE.defaultMSSQLTxErrorHandler
      [ODBC.sql|
          INSERT INTO hdb_catalog.event_log (schema_name, table_name, trigger_name, payload)
          OUTPUT CONVERT(varchar(MAX), inserted.id)
          VALUES
          ($schemaName, $tableName, $triggerNameTxt, $payload)
        |]
  return (EventId (bsToTxt eventId))
  where
    triggerNameTxt = triggerNameToTxt triggerName
    payload = J.encode rowData

setSuccessTx :: Event 'MSSQL -> MaintenanceMode MaintenanceModeVersion -> TxE QErr ()
setSuccessTx event = \case
  (MaintenanceModeEnabled PreviousMMVersion) -> throw500 "unexpected: no previous maintenance mode version found for MSSQL source"
  (MaintenanceModeEnabled CurrentMMVersion) -> latestVersionSetSuccess
  MaintenanceModeDisabled -> latestVersionSetSuccess
  where
    eventId = unEventId $ eId event

    latestVersionSetSuccess =
      unitQueryE
        HGE.defaultMSSQLTxErrorHandler
        [ODBC.sql|
          UPDATE hdb_catalog.event_log
          SET delivered = 1 , next_retry_at = NULL, locked = NULL
          WHERE id = $eventId
        |]

setErrorTx :: Event 'MSSQL -> MaintenanceMode MaintenanceModeVersion -> TxE QErr ()
setErrorTx event = \case
  (MaintenanceModeEnabled PreviousMMVersion) -> throw500 "unexpected: there is no previous maintenance mode version supported for MSSQL event triggers"
  (MaintenanceModeEnabled CurrentMMVersion) -> latestVersionSetSuccess
  MaintenanceModeDisabled -> latestVersionSetSuccess
  where
    eventId = unEventId $ eId event

    latestVersionSetSuccess =
      unitQueryE
        HGE.defaultMSSQLTxErrorHandler
        [ODBC.sql|
          UPDATE hdb_catalog.event_log
          SET error = 1 , next_retry_at = NULL, locked = NULL
          WHERE id = $eventId
        |]

-- See Note [UTCTIME not supported in SQL Server]
setRetryTx :: Event 'MSSQL -> UTCTime -> MaintenanceMode MaintenanceModeVersion -> TxE QErr ()
setRetryTx event utcTime maintenanceMode = do
  time <- convertUTCToDatetime2 utcTime
  case maintenanceMode of
    (MaintenanceModeEnabled PreviousMMVersion) -> throw500 "unexpected: there is no previous maintenance mode version supported for MSSQL event triggers"
    (MaintenanceModeEnabled CurrentMMVersion) -> latestVersionSetRetry time
    MaintenanceModeDisabled -> latestVersionSetRetry time
  where
    eventId = unEventId $ eId event
    -- NOTE: Naveen: The following method to convert from Datetime to Datetimeoffset  was
    -- taken from https://stackoverflow.com/questions/17866311/how-to-cast-datetime-to-datetimeoffset
    latestVersionSetRetry time =
      unitQueryE
        HGE.defaultMSSQLTxErrorHandler
        [ODBC.sql|
          UPDATE hdb_catalog.event_log
          SET next_retry_at = TODATETIMEOFFSET ($time, DATEPART(TZOFFSET, SYSDATETIMEOFFSET())), locked = NULL
          WHERE id = $eventId
        |]

-- | Lock and return events not yet being processed or completed, up to some
-- limit. Process events approximately in created_at order, but we make no
-- ordering guarentees; events can and will race. Nevertheless we want to
-- ensure newer change events don't starve older ones.
fetchEvents :: SourceName -> [TriggerName] -> FetchBatchSize -> TxE QErr [Event 'MSSQL]
fetchEvents source triggerNames (FetchBatchSize fetchBatchSize) = do
  -- The reason we do not inline the SQL but rather  create a template string is due
  -- to the problem with `ODBC.sql` variable substitution. When you use a variable
  -- whose value is a text and it contains a single quote `'`, the `ODBC.sql`
  -- function escapes that single quote. i.e it converts `'` to `''`
  --
  -- Note: A single quote in MSSQL is escaped by doubling it up (`''`)
  --
  -- This is problematic, since we use a list of list of trigger names to fetch and
  -- lock the events. A list of trigger names in MSSQL looks like Eg:
  -- ('insert_test_books', 'et_test_bigint')
  --
  -- We use this list of trigger names in the `IN` operator to fetch only those
  -- events.
  --
  -- If we were to use the `ODBC.sql` function it would convert the list into
  -- something which is not a valid list.
  -- Eg: ('insert_test_books', 'et_test_bigint') -> (''insert_test_books'', ''et_test_bigint'')
  --
  -- Due to the problematic variable substitution of `ODBC.sql` it is imperative that
  -- we resort to template strings, since that does not do any changes to the string.
  events <-
    multiRowQueryE HGE.defaultMSSQLTxErrorHandler $
      rawUnescapedText . LT.toStrict $
        $(makeRelativeToProject "src-rsr/mssql/mssql_fetch_events.sql.shakespeare" >>= ST.stextFile)
  mapM uncurryEvent events
  where
    -- Creates a list of trigger names to be used for 'IN' operator
    -- Eg: ('insert_test_books', 'et_test_bigint')
    --
    -- We cannot use 'commaseperated()' because it creates the Text as
    -- 'insert_test_books, et_test_bigint' which is not useful to compare values in
    -- 'IN' MSSQL operator.
    triggerNamesTxt = "(" <> commaSeparated (map (\t -> "'" <> toTxt t <> "'") triggerNames) <> ")"

    uncurryEvent (id', sn, tn, trn, payload' :: BL.ByteString, tries, created_at :: B.ByteString) = do
      payload <- encodePayload payload'
      createdAt <- convertTime created_at

      pure $
        Event
          { eId = EventId (bsToTxt id'),
            eSource = source,
            eTable = (TableName tn (SchemaName sn)),
            eTrigger = TriggerMetadata (TriggerName $ mkNonEmptyTextUnsafe trn),
            eEvent = payload,
            eTries = tries,
            eCreatedAt = createdAt
          }

    -- Note: We do not have JSON datatype in SQL Server. But since in
    -- 'mkAllTriggersQ' we ensure that all the values in the payload column of
    -- hdb_catalog.event_log is always a JSON. We can directly decode the payload
    -- value and not worry that the decoding will fail.
    --
    -- We ensure that the values in 'hd_catalog.event_log' is always a JSON is by
    -- using the 'FOR JSON PATH' MSSQL operand when inserting value into the
    -- 'hdb_catalog.event_log' table.
    encodePayload :: (J.FromJSON a, QErrM m) => BL.ByteString -> m a
    encodePayload payload =
      onLeft
        (J.eitherDecode payload)
        (\_ -> throw500 $ T.pack "payload decode failed while fetching MSSQL events")

    -- Note: The ODBC server does not have a FromJSON instance of UTCTime and only
    -- supports DateTime2  and SmallDateTime. But the above two data types do not
    -- have time offsets and 'Event' stores the time as UTCTime. But during
    -- 'mkAllTriggersQ' we do save them as UTC Time format. So we can directly decode
    -- the time we get from the DB as UTCTime and not worry about exception being
    -- thrown during decoding.
    --
    -- We ensure that the time stored in 'create_at' column is a UTCTime, by
    -- defaulting the 'created_at' column to use 'SYSDATETIMEOFFSET()' MSSQL function
    -- in 'init_mssql_source.sql' file. The 'SYSDATETIMEOFFSET()' function returns
    -- value that contains the date and time of the computer on which the instance of
    -- SQL Server is running. The time zone offset is included.
    convertTime :: (QErrM m) => B.ByteString -> m UTCTime
    convertTime createdAt =
      onLeft
        (readEither (T.unpack $ bsToTxt createdAt) :: Either String UTCTime)
        (\_ -> throw500 $ T.pack "conversion to UTCTime failed while fetching MSSQL events")

dropTriggerQ :: TriggerName -> SchemaName -> TxE QErr ()
dropTriggerQ triggerName schemaName =
  mapM_ (dropTriggerOp triggerName schemaName) [INSERT, UPDATE, DELETE]

dropTriggerOp :: TriggerName -> SchemaName -> Ops -> TxE QErr ()
dropTriggerOp triggerName schemaName triggerOp =
  unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    (rawUnescapedText $ getDropTriggerSQL triggerOp)
  where
    getDropTriggerSQL :: Ops -> Text
    getDropTriggerSQL op =
      "DROP TRIGGER IF EXISTS " <> unQualifiedTriggerName (msssqlIdenTrigger op schemaName triggerName)

archiveEvents :: TriggerName -> TxE QErr ()
archiveEvents triggerName =
  unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      UPDATE hdb_catalog.event_log
      SET archived = 1
      WHERE trigger_name = $triggerNameTxt
    |]
  where
    triggerNameTxt = triggerNameToTxt triggerName

checkEventTx :: EventId -> TxE QErr ()
checkEventTx eventId = do
  -- If an event got locked within the last 30 minutes then it means that the event
  -- got picked up by during the last fetch-event poll and is being processed. Hence
  -- we do not allow the redelivery of such an event.
  (events :: [Bool]) <-
    multiRowQueryE
      HGE.defaultMSSQLTxErrorHandler
      [ODBC.sql|
        SELECT
          CAST(CASE 
                  WHEN (l.locked IS NOT NULL AND l.locked >= DATEADD(MINUTE, -30, SYSDATETIMEOFFSET())) THEN 1 ELSE 0
              END 
          AS bit)
        FROM hdb_catalog.event_log l
        WHERE l.id = $eId
      |]

  event <- getEvent events
  assertEventUnlocked event
  where
    eId = unEventId $ eventId

    getEvent [] = throw400 NotExists "event not found"
    getEvent (x : _) = return x

    assertEventUnlocked locked =
      when locked $
        throw400 Busy "event is already being processed"

markForDeliveryTx :: EventId -> TxE QErr ()
markForDeliveryTx eventId = do
  unitQueryE
    HGE.defaultMSSQLTxErrorHandler
    [ODBC.sql|
      UPDATE hdb_catalog.event_log
      SET delivered = 0, error = 0, tries = 0
      WHERE id = $eId
    |]
  where
    eId = unEventId $ eventId

unlockEventsTx :: [EventId] -> TxE QErr Int
unlockEventsTx eventIds = do
  numEvents <-
    singleRowQueryE HGE.defaultMSSQLTxErrorHandler $
      rawUnescapedText . LT.toStrict $
        -- EventIds as list of VALUES (Eg: ('123-abc'), ('456-vgh'), ('234-asd'))
        let eventIdsValues = generateValuesFromEvents eventIds
         in $(makeRelativeToProject "src-rsr/mssql/mssql_unlock_events.sql.shakespeare" >>= ST.stextFile)
  return numEvents
  where
    generateValuesFromEvents :: [EventId] -> Text
    -- creates a list of event id's  (('123-abc'), ('456-vgh'), ('234-asd'))
    generateValuesFromEvents events = commaSeparated values
      where
        values = map (\e -> "(" <> toTxt e <> ")") events

getMaintenanceModeVersionTx :: TxE QErr MaintenanceModeVersion
getMaintenanceModeVersionTx = do
  catalogVersion <- getSourceCatalogVersion
  if
      | catalogVersion == latestSourceCatalogVersion -> pure CurrentMMVersion
      | otherwise ->
        throw500 $
          "Maintenance mode is only supported with catalog versions: "
            <> tshow latestSourceCatalogVersion
            <> " but received "
            <> tshow catalogVersion

-- | Note: UTCTIME not supported in SQL Server
--
-- Refer 'ToSql UTCTIME' instance of odbc package:
-- https://github.com/fpco/odbc/blob/f4f04ea15d14e9a3ed455f7c728dc08734eef8ae/src/Database/ODBC/SQLServer.hs#L377
--
-- We use SYSDATETIMEOFFSET() to store time values along with it's time
-- zone offset in event_log table. Since ODBC server does not support time zones,
-- we use a workaround.
--
-- We wrap the time value in Datetime2, but before we insert it into the
-- event_log table we convert it into UTCTIME using the 'TODATETIMEOFFSET()'
-- sql function.
convertUTCToDatetime2 :: MonadIO m => UTCTime -> m Datetime2
convertUTCToDatetime2 utcTime = do
  timezone <- liftIO $ getTimeZone utcTime
  let localTime = utcToLocalTime timezone utcTime
  return $ Datetime2 localTime

---- MSSQL event trigger utility functions -----------------

newtype QualifiedTriggerName = QualifiedTriggerName {unQualifiedTriggerName :: Text}

-- | Store a fragment of SQL expression
newtype SQLFragment = SQLFragment {unSQLFragment :: Text}

msssqlIdenTrigger :: Ops -> SchemaName -> TriggerName -> QualifiedTriggerName
msssqlIdenTrigger op (SchemaName schemaName) triggerName =
  QualifiedTriggerName $ qualifyHasuraTriggerName op $ triggerNameToTxt triggerName
  where
    qualifyHasuraTriggerName op' triggerName' = schemaName <> "." <> "notify_hasura_" <> triggerName' <> "_" <> tshow op'

mkAllTriggersQ ::
  MonadMSSQLTx m =>
  TriggerName ->
  TableName ->
  [ColumnInfo 'MSSQL] ->
  TriggerOpsDef 'MSSQL ->
  Maybe (PrimaryKey 'MSSQL (ColumnInfo 'MSSQL)) ->
  m ()
mkAllTriggersQ triggerName tableName allCols fullSpec primaryKey = do
  onJust (tdInsert fullSpec) (mkInsertTriggerQ triggerName tableName allCols)
  onJust (tdDelete fullSpec) (mkDeleteTriggerQ triggerName tableName allCols)
  onJust (tdUpdate fullSpec) (mkUpdateTriggerQ triggerName tableName allCols primaryKey)

getApplicableColumns :: [ColumnInfo 'MSSQL] -> SubscribeColumns 'MSSQL -> [ColumnInfo 'MSSQL]
getApplicableColumns allColumnInfos = \case
  SubCStar -> allColumnInfos
  SubCArray cols -> getColInfos cols allColumnInfos

-- | Currently we do not support Event Triggers on columns of Spatial data types.
-- We do this because, currently the graphQL API for these types is broken
-- for MSSQL sources. Ref: https://github.com/hasura/graphql-engine-mono/issues/787
checkSpatialDataTypeColumns ::
  MonadMSSQLTx m =>
  [ColumnInfo 'MSSQL] ->
  SubscribeOpSpec 'MSSQL ->
  m ()
checkSpatialDataTypeColumns allCols (SubscribeOpSpec listenCols deliveryCols) = do
  let listenColumns = getApplicableColumns allCols listenCols
      deliveryColumns = getApplicableColumns allCols $ fromMaybe SubCStar deliveryCols
      isGeoTypesInListenCols = any (isScalarColumnWhere isGeoType . ciType) listenColumns
      isGeoTypesInDeliversCols = any (isScalarColumnWhere isGeoType . ciType) deliveryColumns
  when (isGeoTypesInListenCols || isGeoTypesInDeliversCols) $
    throw400 NotSupported "Event triggers for MS-SQL sources are not supported on tables having Geometry or Geography column types"
  where
    isGeoType = (`elem` geoTypes)

mkInsertTriggerQ ::
  MonadMSSQLTx m =>
  TriggerName ->
  TableName ->
  [ColumnInfo 'MSSQL] ->
  SubscribeOpSpec 'MSSQL ->
  m ()
mkInsertTriggerQ triggerName table allCols subOpSpec@(SubscribeOpSpec _listenCols deliveryCols) = do
  checkSpatialDataTypeColumns allCols subOpSpec
  liftMSSQLTx $ do
    unitQueryE HGE.defaultMSSQLTxErrorHandler $
      rawUnescapedText . LT.toStrict $ do
        let deliveryColumns = getApplicableColumns allCols $ fromMaybe SubCStar deliveryCols
        mkInsertTriggerQuery table triggerName deliveryColumns

mkDeleteTriggerQ ::
  MonadMSSQLTx m =>
  TriggerName ->
  TableName ->
  [ColumnInfo 'MSSQL] ->
  SubscribeOpSpec 'MSSQL ->
  m ()
mkDeleteTriggerQ triggerName table allCols subOpSpec@(SubscribeOpSpec _listenCols deliveryCols) = do
  checkSpatialDataTypeColumns allCols subOpSpec
  liftMSSQLTx $ do
    unitQueryE HGE.defaultMSSQLTxErrorHandler $
      rawUnescapedText . LT.toStrict $ do
        let deliveryColumns = getApplicableColumns allCols $ fromMaybe SubCStar deliveryCols
        mkDeleteTriggerQuery table triggerName deliveryColumns

mkUpdateTriggerQ ::
  MonadMSSQLTx m =>
  TriggerName ->
  TableName ->
  [ColumnInfo 'MSSQL] ->
  Maybe (PrimaryKey 'MSSQL (ColumnInfo 'MSSQL)) ->
  SubscribeOpSpec 'MSSQL ->
  m ()
mkUpdateTriggerQ triggerName table allCols primaryKeyMaybe subOpSpec@(SubscribeOpSpec listenCols deliveryCols) = do
  checkSpatialDataTypeColumns allCols subOpSpec
  liftMSSQLTx $ do
    primaryKey <- onNothing primaryKeyMaybe (throw400 NotSupported "Update event triggers for MS-SQL sources are only supported on tables with primary keys")
    let deliveryColumns = getApplicableColumns allCols $ fromMaybe SubCStar deliveryCols
        listenColumns = getApplicableColumns allCols listenCols
    unitQueryE HGE.defaultMSSQLTxErrorHandler $
      rawUnescapedText . LT.toStrict $
        mkUpdateTriggerQuery table triggerName listenColumns deliveryColumns primaryKey

-- Create alias for columns
-- eg: If colPrefixMaybe is defined then 'inserted.id as payload.data.old.id'
--     else 'id as payload.data.old.id'
-- We create such an alias for the columns of the table because it helps in
-- structuring the JSON payload.
generateColumnTriggerAlias :: OpVar -> Maybe Text -> ColumnInfo 'MSSQL -> SQLFragment
generateColumnTriggerAlias op colPrefixMaybe colInfo =
  let opText =
        case op of
          OLD -> "old"
          NEW -> "new"
      -- Let's say we have a column 'id', dbColNameText returns the column name as
      -- text. i.e 'id'
      dbColNameText = columnNameText $ ciColumn colInfo
      joinPrefixedDbColNameText =
        case colPrefixMaybe of
          -- prefix with the joining table's name
          -- `id` -> `inserted.id` (prefix = 'inserted')
          Just colPrefix -> colPrefix <> "." <> dbColNameText
          -- do not prefix anthing to the column name
          Nothing -> dbColNameText
      -- create the alias for the column
      -- `payload.data.old.id` (opText = old) (dbColNameText = id)
      dbColAlias = "payload.data" <> "." <> opText <> "." <> dbColNameText
   in -- create the SQL alias using the `as` keyword
      -- If colPrefixMaybe existed then alias will be `inserted.id as payload.data.old.id`
      -- If no colPrefixMaybe was Nothing then alias will be 'id as payload.data.old.id`
      SQLFragment $ LT.toStrict $ [ST.stext| #{joinPrefixedDbColNameText} as [#{dbColAlias}]|]

-- Converts tables name to the format [SCHEMA].[TABLENAME]
-- eg: [dbo].[author], [hge].[books]
qualifyTableName :: TableName -> Text
qualifyTableName = toTxt . toQueryFlat . fromTableName

mkInsertTriggerQuery :: TableName -> TriggerName -> [ColumnInfo 'MSSQL] -> LT.Text
mkInsertTriggerQuery table@(TableName tableName schema@(SchemaName schemaName)) triggerName columns =
  let QualifiedTriggerName qualifiedTriggerName = msssqlIdenTrigger INSERT schema triggerName
      triggerNameText = triggerNameToTxt triggerName
      qualifiedTableName = qualifyTableName table
      operation = tshow INSERT
      deliveryColsSQLExpression :: Text =
        commaSeparated $ map (unSQLFragment . generateColumnTriggerAlias NEW Nothing) columns
   in $(makeRelativeToProject "src-rsr/mssql/mssql_insert_trigger.sql.shakespeare" >>= ST.stextFile)

mkDeleteTriggerQuery :: TableName -> TriggerName -> [ColumnInfo 'MSSQL] -> LT.Text
mkDeleteTriggerQuery table@(TableName tableName schema@(SchemaName schemaName)) triggerName columns =
  let QualifiedTriggerName qualifiedTriggerName = msssqlIdenTrigger DELETE schema triggerName
      triggerNameText = triggerNameToTxt triggerName
      qualifiedTableName = qualifyTableName table
      operation = tshow DELETE
      deliveryColsSQLExpression :: Text = commaSeparated $ map (unSQLFragment . generateColumnTriggerAlias OLD Nothing) columns
   in $(makeRelativeToProject "src-rsr/mssql/mssql_delete_trigger.sql.shakespeare" >>= ST.stextFile)

-- Creates Primary Join key expression for UPDATE SQL Trigger
-- eg: 'INSERTED.id = DELETED.id AND INSERTED.emp_code = DELETED.emp_code'
mkPrimaryKeyJoinExp :: Text -> Text -> [ColumnInfo 'MSSQL] -> SQLFragment
mkPrimaryKeyJoinExp lhsPrefix rhsPrefix columns =
  SQLFragment $ T.intercalate " AND " $ singleColExp <$> columns
  where
    singleColExp colInfo =
      let dbColNameText = columnNameText $ ciColumn colInfo
       in LT.toStrict $ [ST.stext| #{lhsPrefix}.#{dbColNameText} = #{rhsPrefix}.#{dbColNameText} |]

-- Creates the WHERE clause for UPDATE SQL Trigger
-- eg: If no listenColumns are defined then the where clause is an empty text
--     else, 'WHERE INSERTED.id != DELETED.id OR INSERTED.name != DELTED.name'
mkListenColumnsExp :: Text -> Text -> [ColumnInfo 'MSSQL] -> SQLFragment
mkListenColumnsExp _ _ [] = SQLFragment ""
mkListenColumnsExp lhsPrefix rhsPrefix columns =
  SQLFragment $ "where " <> T.intercalate " OR " (singleColExp <$> columns)
  where
    singleColExp colInfo =
      let dbColNameText = columnNameText $ ciColumn colInfo
       in LT.toStrict $ [ST.stext| #{lhsPrefix}.#{dbColNameText} != #{rhsPrefix}.#{dbColNameText} |]

-- | Check if primary key is present in listen columns
-- We use this in update event trigger, to check if the primary key has been updated
-- and construct the payload accordingly.
isPrimaryKeyInListenColumns :: [ColumnInfo 'MSSQL] -> PrimaryKey 'MSSQL (ColumnInfo 'MSSQL) -> SQLFragment
isPrimaryKeyInListenColumns listenCols primaryKey =
  case (toList (_pkColumns primaryKey) `intersect` listenCols) of
    -- Do not fire Event Trigger if primary key not in listen column
    [] -> SQLFragment $ "1 != 1"
    -- Fire Event Trigger, since primary key is in listen column
    _ -> SQLFragment $ "1 = 1"

{- Note [Update Event Trigger MSSQL Spec]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An MS-SQL trigger is different from a postgres trigger in some ways
  * MS-SQL doesn't support triggers which trigger for each row, so in case of
    mutations which affect multiple rows, there'll only be a single trigger
    fired which will contain the data of all the rows that were affected.
  * MS-SQL maintains two logical tables, namely, [`inserted` and `deleted`](https://docs.microsoft.com/en-us/sql/relational-databases/triggers/use-the-inserted-and-deleted-tables?view=sql-server-ver15).
    The rows in the `inserted` table are copies of the new rows in the trigger
    table and similarly the `deleted` table contains the copies of the rows
    that were deleted from the trigger table.
  * When there's an update transaction, the old data (before the update) will
    be copied to the `deleted` table and the new data will be copied to the
    `inserted` table.

Since we deliver the 'old' and 'new' data in the event trigger payload, we would need
a way to correlate the values from `inserted` and `deleted` tables. And this is why,
It is mandatory for a MSSQL Update trigger table to have a primary key. We use this
primary key to correlate between `inserted` and `deleted`

MSSQL UPDATE trigger's join clause depends on the fact that the primary key is never
updated. But when the primary key is updated, you cannot join the 'INSERTED' and
the 'DELETED' tables. Hence for those cases, we consider the old payload as NULL and
the new payload will contain the updated row changes.

To figure out if a primary key has been updated, we do the following:
For each row present in the INSERTED table, we check if there are any rows in DELETED
tabled that has the same primary key as that of the row in INSERTED table. If such a
row does not exists, then it means that the primary key has been updated. The sample
SQL which does this looks like:
  SELECT * FROM INSERTED
  WHERE NOT EXISTS (SELECT * FROM DELETED WHERE  INSERTED.id = DELETED.id )

The spec for MSSQL UPDATE Event Trigger is as follows:
1. UPDATE Event Trigger can only be created on tables with a primary key.
2. When Primary Key is not updated during a UPDATE transaction then both 'data.new'
   and 'data.old' fields in payload will be constructed.
3. When Primary Key is updated during a UPDATE transaction then there are two cases:
    a. If the updated Primary key is equal to one of the already present primary key in
       the table then, we construct both the 'data.old' and 'data.new'
    b. If the updated primary key is not equal to any of the already present primary key
       in the table then, 'data.old' is NULL and only 'data.new' is constructed.
-}
mkUpdateTriggerQuery :: TableName -> TriggerName -> [ColumnInfo 'MSSQL] -> [ColumnInfo 'MSSQL] -> PrimaryKey 'MSSQL (ColumnInfo 'MSSQL) -> LT.Text
mkUpdateTriggerQuery
  table@(TableName tableName schema@(SchemaName schemaName))
  triggerName
  listenColumns
  deliveryColumns
  primaryKey =
    let QualifiedTriggerName qualifiedTriggerName = msssqlIdenTrigger UPDATE schema triggerName
        triggerNameText = triggerNameToTxt triggerName
        qualifiedTableName = qualifyTableName table
        operation = tshow UPDATE

        oldDeliveryColsSQLExp :: Text = commaSeparated $ map (unSQLFragment . generateColumnTriggerAlias OLD (Just "DELETED")) deliveryColumns
        newDeliveryColsSQLExp :: Text = commaSeparated $ map (unSQLFragment . generateColumnTriggerAlias NEW (Just "INSERTED")) deliveryColumns

        -- When Primary key is updated, then 'data.old' would be NULL
        -- See Note [Update Event Trigger MSSQL Spec]
        oldDeliveryColsSQLExpWhenPrimaryKeyUpdated :: Text =
          "NULL as [payload.data.old]"
        newDeliveryColsSQLExpWhenPrimaryKeyUpdated :: Text =
          commaSeparated $ map (unSQLFragment . generateColumnTriggerAlias NEW (Just "INSERTED")) deliveryColumns

        primaryKeyJoinExp = unSQLFragment $ mkPrimaryKeyJoinExp "INSERTED" "DELETED" (toList (_pkColumns primaryKey))
        listenColumnExp = unSQLFragment $ mkListenColumnsExp "INSERTED" "DELETED" listenColumns
        isPrimaryKeyInListenColumnsExp = unSQLFragment $ isPrimaryKeyInListenColumns listenColumns primaryKey
     in $(makeRelativeToProject "src-rsr/mssql/mssql_update_trigger.sql.shakespeare" >>= ST.stextFile)
