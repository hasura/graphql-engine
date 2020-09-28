module Hasura.Class
  ( SchemaSyncEventProcessResult(..)
  , SchemaSyncEventPayload(..)
  , InsertCronEventsTx
  , UnlockScheduledEventsTx
  , processOutputSelectionSet
  , mkJsonAggSelect
  , fetchMetadata
  , setMetadata
  , MonadMetadataStorageTx(..)
  )
where

import           Hasura.Db
import           Hasura.Eventing.HTTP
import           Hasura.Eventing.ScheduledTrigger.Types
import           Hasura.GraphQL.Parser                  hiding (column)
import           Hasura.Prelude
import           Hasura.RQL.Types                       hiding (fetchMetadata)
import           Hasura.Server.Types
import           Hasura.Server.Utils
import           Hasura.Session
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.RQL.DML.Select.Internal         as RS

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

-- Transactions related to metadata management
type FetchMetadataTx = Q.TxE QErr Metadata
type SetMetadataTx = Metadata -> Q.TxE QErr ()
type NotifySchemaCacheSyncTx = InstanceId -> CacheInvalidations -> Q.TxE QErr ()

fetchMetadata :: FetchMetadataTx
fetchMetadata = do
  rows <- Q.withQE defaultTxErrorHandler
    [Q.sql|
       SELECT metadata from hdb_catalog.hdb_metadata where id = 1
    |] () True
  case rows of
    []                             -> pure emptyMetadata
    [(Identity (Q.AltJ metadata))] -> pure metadata
    _                              -> throw500 "multiple rows in hdb_metadata table"

setMetadata :: SetMetadataTx
setMetadata metadata =
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
     INSERT INTO hdb_catalog.hdb_metadata
       (id, metadata) VALUES (1, $1::json)
     ON CONFLICT (id) DO UPDATE SET metadata = $1::json
    |] (Identity $ Q.AltJ metadata) True

notifySchemaCacheSync :: NotifySchemaCacheSyncTx
notifySchemaCacheSync instanceId invalidations = do
  Q.Discard () <- Q.withQE defaultTxErrorHandler [Q.sql|
      SELECT pg_notify('hasura_schema_update', json_build_object(
        'instance_id', $1,
        'occurred_at', NOW(),
        'invalidations', $2
        )::text
      )
    |] (instanceId, Q.AltJ invalidations) True
  pure ()

processSchemaSyncEventPayload'
  :: (Monad m)
  => InstanceId -> J.Value -> m (Either Text SchemaSyncEventProcessResult)
processSchemaSyncEventPayload' instanceId payloadValue = pure $ do
  eventPayload <- fmapL qeError $ runExcept $ decodeValue payloadValue
  let _sseprShouldReload = instanceId /= _ssepInstanceId eventPayload
      _sseprCacheInvalidations = _ssepInvalidations eventPayload
  pure SchemaSyncEventProcessResult{..}

-- Transactions related to Async actions
type InsertActionTx = ActionName -> SessionVariables -> [HTTP.Header] -> J.Value -> Q.TxE QErr ActionId
type UndeliveredActionsTx = Q.TxE QErr [ActionLogItem]
type SetActionStatusTx = ActionId -> AsyncActionStatus -> Q.TxE QErr ()
type AsyncActionQueryResolver = UserInfo -> AnnActionAsyncQuery UnpreparedValue -> RS.AnnSimpleSelG UnpreparedValue

insertActionTx :: InsertActionTx
insertActionTx actionName sessionVariables httpHeaders inputArgsPayload =
  runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
    INSERT INTO
        "hdb_catalog"."hdb_action_log"
        ("action_name", "session_variables", "request_headers", "input_payload", "status")
    VALUES
        ($1, $2, $3, $4, $5)
    RETURNING "id"
   |]
    ( actionName
    , Q.AltJ sessionVariables
    , Q.AltJ $ toHeadersMap httpHeaders
    , Q.AltJ inputArgsPayload
    , "created"::Text
    ) False
  where
    toHeadersMap = Map.fromList . map ((bsToTxt . CI.original) *** bsToTxt)

undeliveredEventsTx :: UndeliveredActionsTx
undeliveredEventsTx =
  map mapEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
    update hdb_catalog.hdb_action_log set status = 'processing'
    where
      id in (
        select id from hdb_catalog.hdb_action_log
        where status = 'created'
        for update skip locked limit 10
      )
    returning
      id, action_name, request_headers::json, session_variables::json, input_payload::json
  |] () False
 where
   mapEvent (actionId, actionName, Q.AltJ headersMap,
             Q.AltJ sessionVariables, Q.AltJ inputPayload) =
     ActionLogItem actionId actionName (fromHeadersMap headersMap) sessionVariables inputPayload

   fromHeadersMap = map ((CI.mk . txtToBs) *** txtToBs) . Map.toList

setActionStatusTx :: SetActionStatusTx
setActionStatusTx actionId = \case
  AASCompleted responsePayload ->
    Q.unitQE defaultTxErrorHandler [Q.sql|
      update hdb_catalog.hdb_action_log
      set response_payload = $1, status = 'completed'
      where id = $2
    |] (Q.AltJ responsePayload, actionId) False

  AASError qerr                ->
    Q.unitQE defaultTxErrorHandler [Q.sql|
      update hdb_catalog.hdb_action_log
      set errors = $1, status = 'error'
      where id = $2
    |] (Q.AltJ qerr, actionId) False

-- TODO: Add tracing here? Avoided now because currently the function is pure
resolveAsyncActionQuery :: AsyncActionQueryResolver
resolveAsyncActionQuery userInfo annAction =
  let annotatedFields = asyncFields <&> second \case
        AsyncTypename t -> RS.AFExpression t
        AsyncOutput annFields ->
          -- See Note [Resolving async action query/subscription]
          let inputTableArgument = RS.AETableRow $ Just $ Iden "response_payload"
              jsonAggSelect = mkJsonAggSelect outputType
          in RS.AFComputedField $ RS.CFSTable jsonAggSelect $
             processOutputSelectionSet inputTableArgument outputType
             definitionList annFields stringifyNumerics

        AsyncId        -> mkAnnFldFromPGCol "id" PGUUID
        AsyncCreatedAt -> mkAnnFldFromPGCol "created_at" PGTimeStampTZ
        AsyncErrors    -> mkAnnFldFromPGCol "errors" PGJSONB

      tableFromExp = RS.FromTable actionLogTable
      tableArguments = RS.noSelectArgs
                       { RS._saWhere = Just tableBoolExpression}
      tablePermissions = RS.TablePerm annBoolExpTrue Nothing

  in RS.AnnSelectG annotatedFields tableFromExp tablePermissions
     tableArguments stringifyNumerics
  where
    AnnActionAsyncQuery _ actionId outputType asyncFields definitionList stringifyNumerics = annAction
    actionLogTable = QualifiedObject (SchemaName "hdb_catalog") (TableName "hdb_action_log")

    -- TODO (from master):- Avoid using PGColumnInfo
    mkAnnFldFromPGCol column' columnType =
      flip RS.mkAnnColumnField Nothing $
      PGColumnInfo (unsafePGCol column') (G.unsafeMkName column') 0 (PGColumnScalar columnType) True Nothing

    tableBoolExpression =
      let actionIdColumnInfo = PGColumnInfo (unsafePGCol "id") $$(G.litName "id")
                               0 (PGColumnScalar PGUUID) False Nothing
          actionIdColumnEq = BoolFld $ AVCol actionIdColumnInfo [AEQ True actionId]
          sessionVarsColumnInfo = PGColumnInfo (unsafePGCol "session_variables") $$(G.litName "session_variables")
                                  0 (PGColumnScalar PGJSONB) False Nothing
          sessionVarValue = flip UVParameter Nothing $ PGColumnValue (PGColumnScalar PGJSONB) $
                            WithScalarType PGJSONB $ PGValJSONB $ Q.JSONB $ J.toJSON $ _uiSession userInfo
          sessionVarsColumnEq = BoolFld $ AVCol sessionVarsColumnInfo [AEQ True sessionVarValue]

      -- For non-admin roles, accessing an async action's response should be allowed only for the user
      -- who initiated the action through mutation. The action's response is accessible for a query/subscription
      -- only when it's session variables are equal to that of action's.
      in if isAdmin (_uiRole userInfo) then actionIdColumnEq
         else BoolAnd [actionIdColumnEq, sessionVarsColumnEq]

processOutputSelectionSet
  :: RS.ArgumentExp v
  -> GraphQLType
  -> [(PGCol, PGScalarType)]
  -> RS.AnnFieldsG v
  -> Bool
  -> RS.AnnSimpleSelG v
processOutputSelectionSet tableRowInput actionOutputType definitionList annotatedFields =
  RS.AnnSelectG annotatedFields selectFrom RS.noTablePermissions RS.noSelectArgs
  where
    jsonbToPostgresRecordFunction =
      QualifiedObject "pg_catalog" $ FunctionName $
      if isListType actionOutputType then
        "jsonb_to_recordset" -- Multirow array response
      else "jsonb_to_record" -- Single object response

    functionArgs = RS.FunctionArgsExp [tableRowInput] mempty
    selectFrom = RS.FromFunction jsonbToPostgresRecordFunction functionArgs $ Just definitionList

mkJsonAggSelect :: GraphQLType -> RS.JsonAggSelect
mkJsonAggSelect =
  bool RS.JASSingleObject RS.JASMultipleRows . isListType

-- Scheduled triggers related
type DeprivedCronTriggersStatsTx = Q.TxE QErr [CronTriggerStats]
type PartialCronEventsTx = Q.TxE QErr [CronEventPartial]
type OneOffScheduledEventsTx = Q.TxE QErr [OneOffScheduledEvent]
type InsertCronEventsTx = [CronEventSeed] -> Q.TxE QErr ()
type InsertInvocationTx = Invocation 'ScheduledType -> ScheduledEventType -> Q.TxE QErr ()
type SetRetryTx = ScheduledEventFull -> UTC.UTCTime -> ScheduledEventType -> Q.TxE QErr ()
type SetScheduledEventStatusTx = ScheduledEventId -> ScheduledEventStatus -> ScheduledEventType -> Q.TxE QErr ()
type UnlockScheduledEventsTx = ScheduledEventType -> [ScheduledEventId] -> Q.TxE QErr Int
type UnlockAllLockedScheduledEventsTx = Q.TxE QErr ()
type ClearFutureCronEventsTx = TriggerName -> Q.TxE QErr ()

getDeprivedCronTriggerStats :: DeprivedCronTriggersStatsTx
getDeprivedCronTriggerStats =
  map (\(n, count, maxTx) -> CronTriggerStats n count maxTx) <$>
    Q.listQE defaultTxErrorHandler
    [Q.sql|
     SELECT name, upcoming_events_count, max_scheduled_time
      FROM hdb_catalog.hdb_cron_events_stats
      WHERE upcoming_events_count < 100
     |] () True

getPartialCronEvents :: PartialCronEventsTx
getPartialCronEvents = do
  map uncurryEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.hdb_cron_events
      SET status = 'locked'
      WHERE id IN ( SELECT t.id
                    FROM hdb_catalog.hdb_cron_events t
                    WHERE ( t.status = 'scheduled'
                            and (
                             (t.next_retry_at is NULL and t.scheduled_time <= now()) or
                             (t.next_retry_at is not NULL and t.next_retry_at <= now())
                            )
                          )
                    FOR UPDATE SKIP LOCKED
                    )
      RETURNING id, trigger_name, scheduled_time, tries, created_at
      |] () True
  where uncurryEvent (i, n, st, tries, createdAt) = CronEventPartial i n st tries createdAt

getOneOffScheduledEvents :: OneOffScheduledEventsTx
getOneOffScheduledEvents = do
  map uncurryOneOffScheduledEvent <$> Q.listQE defaultTxErrorHandler [Q.sql|
      UPDATE hdb_catalog.hdb_scheduled_events
      SET status = 'locked'
      WHERE id IN ( SELECT t.id
                    FROM hdb_catalog.hdb_scheduled_events t
                    WHERE ( t.status = 'scheduled'
                            and (
                             (t.next_retry_at is NULL and t.scheduled_time <= now()) or
                             (t.next_retry_at is not NULL and t.next_retry_at <= now())
                            )
                          )
                    FOR UPDATE SKIP LOCKED
                    )
      RETURNING id, webhook_conf, scheduled_time, retry_conf, payload, header_conf, tries, comment, created_at
      |] () False
  where
    uncurryOneOffScheduledEvent ( eventId
                                , webhookConf
                                , scheduledTime
                                , retryConf
                                , payload
                                , headerConf
                                , tries
                                , comment
                                , createdAt) =
      OneOffScheduledEvent eventId
                           scheduledTime
                           tries
                           (Q.getAltJ webhookConf)
                           (Q.getAltJ payload)
                           (Q.getAltJ retryConf)
                           (Q.getAltJ headerConf)
                           comment
                           createdAt

insertCronEvents :: InsertCronEventsTx
insertCronEvents events = do
  let insertCronEventsSql = TB.run $ toSQL
        SQLInsert
          { siTable    = cronEventsTable
          , siCols     = map unsafePGCol ["trigger_name", "scheduled_time"]
          , siValues   = ValuesExp $ map (toTupleExp . toArr) events
          , siConflict = Just $ DoNothing Nothing
          , siRet      = Nothing
          }
  Q.unitQE defaultTxErrorHandler (Q.fromText insertCronEventsSql) () False
  where
    toArr (CronEventSeed n t) = [(triggerNameToTxt n), (formatTime' t)]
    toTupleExp = TupleExp . map SELit

insertInvocation :: InsertInvocationTx
insertInvocation invo type' = do
  case type' of
    Cron -> do
      Q.unitQE defaultTxErrorHandler
        [Q.sql|
         INSERT INTO hdb_catalog.hdb_cron_event_invocation_logs
         (event_id, status, request, response)
         VALUES ($1, $2, $3, $4)
        |] ( iEventId invo
             , fromIntegral $ iStatus invo :: Int64
             , Q.AltJ $ J.toJSON $ iRequest invo
             , Q.AltJ $ J.toJSON $ iResponse invo) True
      Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.hdb_cron_events
          SET tries = tries + 1
          WHERE id = $1
          |] (Identity $ iEventId invo) True
    OneOff -> do
      Q.unitQE defaultTxErrorHandler
        [Q.sql|
         INSERT INTO hdb_catalog.hdb_scheduled_event_invocation_logs
         (event_id, status, request, response)
         VALUES ($1, $2, $3, $4)
        |] ( iEventId invo
             , fromIntegral $ iStatus invo :: Int64
             , Q.AltJ $ J.toJSON $ iRequest invo
             , Q.AltJ $ J.toJSON $ iResponse invo) True
      Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.hdb_scheduled_events
          SET tries = tries + 1
          WHERE id = $1
          |] (Identity $ iEventId invo) True

setRetry :: SetRetryTx
setRetry se time type' =
  case type' of
    Cron ->
      Q.unitQE defaultTxErrorHandler [Q.sql|
        UPDATE hdb_catalog.hdb_cron_events
        SET next_retry_at = $1,
        STATUS = 'scheduled'
        WHERE id = $2
        |] (time, sefId se) True
    OneOff ->
      Q.unitQE defaultTxErrorHandler [Q.sql|
        UPDATE hdb_catalog.hdb_scheduled_events
        SET next_retry_at = $1,
        STATUS = 'scheduled'
        WHERE id = $2
        |] (time, sefId se) True

setScheduledEventStatus :: SetScheduledEventStatusTx
setScheduledEventStatus scheduledEventId status type' =
  case type' of
    Cron -> do
      Q.unitQE defaultTxErrorHandler
       [Q.sql|
        UPDATE hdb_catalog.hdb_cron_events
        SET status = $2
        WHERE id = $1
       |] (scheduledEventId, status) True
    OneOff -> do
      Q.unitQE defaultTxErrorHandler
       [Q.sql|
        UPDATE hdb_catalog.hdb_scheduled_events
        SET status = $2
        WHERE id = $1
       |] (scheduledEventId, status) True

unlockScheduledEvents :: UnlockScheduledEventsTx
unlockScheduledEvents type' eventIds =
  case type' of
    Cron ->
      (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
      [Q.sql|
        WITH "cte" AS
        (UPDATE hdb_catalog.hdb_cron_events
        SET status = 'scheduled'
        WHERE id = ANY($1::text[]) and status = 'locked'
        RETURNING *)
        SELECT count(*) FROM "cte"
      |] (Identity $ ScheduledEventIdArray eventIds) True

    OneOff ->
      (runIdentity . Q.getRow) <$> Q.withQE defaultTxErrorHandler
      [Q.sql|
        WITH "cte" AS
        (UPDATE hdb_catalog.hdb_scheduled_events
        SET status = 'scheduled'
        WHERE id = ANY($1::text[]) AND status = 'locked'
        RETURNING *)
        SELECT count(*) FROM "cte"
      |] (Identity $ ScheduledEventIdArray eventIds) True

unlockAllLockedScheduledEvents :: UnlockAllLockedScheduledEventsTx
unlockAllLockedScheduledEvents = do
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.hdb_cron_events
          SET status = 'scheduled'
          WHERE status = 'locked'
          |] () True
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.hdb_scheduled_events
          SET status = 'scheduled'
          WHERE status = 'locked'
          |] () True

clearFutureCronEvents :: ClearFutureCronEventsTx
clearFutureCronEvents triggerName =
  Q.unitQE defaultTxErrorHandler
  [Q.sql|
    DELETE FROM hdb_catalog.hdb_cron_events
    WHERE trigger_name = $1 AND scheduled_time > now() AND tries = 0
  |] (Identity triggerName) False


class (Monad m) => MonadMetadataStorageTx m where

  -- Metadata
  getFetchMetadataTx :: m FetchMetadataTx
  default getFetchMetadataTx :: m FetchMetadataTx
  getFetchMetadataTx = pure fetchMetadata

  getSetMetadataTx :: m SetMetadataTx
  default getSetMetadataTx :: m SetMetadataTx
  getSetMetadataTx = pure setMetadata

  getNotifySchemaCacheSyncTx :: m NotifySchemaCacheSyncTx
  default getNotifySchemaCacheSyncTx :: m NotifySchemaCacheSyncTx
  getNotifySchemaCacheSyncTx = pure notifySchemaCacheSync

  processSchemaSyncEventPayload :: InstanceId -> J.Value -> m (Either Text SchemaSyncEventProcessResult)
  default processSchemaSyncEventPayload :: InstanceId -> J.Value -> m (Either Text SchemaSyncEventProcessResult)
  processSchemaSyncEventPayload = processSchemaSyncEventPayload'

  -- Async actions
  getInsertActionTx :: m InsertActionTx
  default getInsertActionTx :: m InsertActionTx
  getInsertActionTx = pure insertActionTx

  getUndeliveredEventsTx :: m UndeliveredActionsTx
  default getUndeliveredEventsTx :: m UndeliveredActionsTx
  getUndeliveredEventsTx = pure undeliveredEventsTx

  getSetActionStatusTx :: m SetActionStatusTx
  default getSetActionStatusTx :: m SetActionStatusTx
  getSetActionStatusTx = pure setActionStatusTx

  getAsyncActionQueryResolver :: m AsyncActionQueryResolver
  default getAsyncActionQueryResolver :: m AsyncActionQueryResolver
  getAsyncActionQueryResolver = pure resolveAsyncActionQuery

  -- Scheduled triggers
  getDeprivedCronTriggerStatsTx :: m DeprivedCronTriggersStatsTx
  default getDeprivedCronTriggerStatsTx :: m DeprivedCronTriggersStatsTx
  getDeprivedCronTriggerStatsTx = pure getDeprivedCronTriggerStats

  getPartialCronEventsTx :: m PartialCronEventsTx
  default getPartialCronEventsTx :: m PartialCronEventsTx
  getPartialCronEventsTx = pure getPartialCronEvents

  getOneOffScheduledEventsTx :: m OneOffScheduledEventsTx
  default getOneOffScheduledEventsTx :: m OneOffScheduledEventsTx
  getOneOffScheduledEventsTx = pure getOneOffScheduledEvents

  getInsertCronEventsTx :: m InsertCronEventsTx
  default getInsertCronEventsTx :: m InsertCronEventsTx
  getInsertCronEventsTx = pure insertCronEvents

  getInsertInvocationTx :: m InsertInvocationTx
  default getInsertInvocationTx :: m InsertInvocationTx
  getInsertInvocationTx = pure insertInvocation

  getSetRetryTx :: m SetRetryTx
  default getSetRetryTx :: m SetRetryTx
  getSetRetryTx = pure setRetry

  getSetScheduledEventStatusTx :: m SetScheduledEventStatusTx
  default getSetScheduledEventStatusTx :: m SetScheduledEventStatusTx
  getSetScheduledEventStatusTx = pure setScheduledEventStatus

  getUnlockScheduledEventsTx :: m UnlockScheduledEventsTx
  default getUnlockScheduledEventsTx :: m UnlockScheduledEventsTx
  getUnlockScheduledEventsTx = pure unlockScheduledEvents

  getUnlockAllLockedScheduledEventsTx :: m UnlockAllLockedScheduledEventsTx
  default getUnlockAllLockedScheduledEventsTx :: m UnlockAllLockedScheduledEventsTx
  getUnlockAllLockedScheduledEventsTx = pure unlockAllLockedScheduledEvents

  getClearFutureCronEventsTx :: m ClearFutureCronEventsTx
  default getClearFutureCronEventsTx :: m ClearFutureCronEventsTx
  getClearFutureCronEventsTx = pure clearFutureCronEvents

instance (MonadMetadataStorageTx m) => MonadMetadataStorageTx (ReaderT r m) where

  getFetchMetadataTx         = lift getFetchMetadataTx
  getSetMetadataTx           = lift getSetMetadataTx
  getNotifySchemaCacheSyncTx = lift getNotifySchemaCacheSyncTx

  getInsertActionTx      = lift getInsertActionTx
  getUndeliveredEventsTx = lift getUndeliveredEventsTx
  getSetActionStatusTx   = lift getSetActionStatusTx

  getDeprivedCronTriggerStatsTx       = lift getDeprivedCronTriggerStatsTx
  getPartialCronEventsTx              = lift getPartialCronEventsTx
  getOneOffScheduledEventsTx          = lift getOneOffScheduledEventsTx
  getInsertCronEventsTx               = lift getInsertCronEventsTx
  getInsertInvocationTx               = lift getInsertInvocationTx
  getSetRetryTx                       = lift getSetRetryTx
  getSetScheduledEventStatusTx        = lift getSetScheduledEventStatusTx
  getUnlockAllLockedScheduledEventsTx = lift getUnlockAllLockedScheduledEventsTx
  getClearFutureCronEventsTx          = lift getClearFutureCronEventsTx

instance (MonadMetadataStorageTx m) => MonadMetadataStorageTx (ExceptT e m) where

  getFetchMetadataTx         = lift getFetchMetadataTx
  getSetMetadataTx           = lift getSetMetadataTx
  getNotifySchemaCacheSyncTx = lift getNotifySchemaCacheSyncTx

  getInsertActionTx      = lift getInsertActionTx
  getUndeliveredEventsTx = lift getUndeliveredEventsTx
  getSetActionStatusTx   = lift getSetActionStatusTx

  getDeprivedCronTriggerStatsTx       = lift getDeprivedCronTriggerStatsTx
  getPartialCronEventsTx              = lift getPartialCronEventsTx
  getOneOffScheduledEventsTx          = lift getOneOffScheduledEventsTx
  getInsertCronEventsTx               = lift getInsertCronEventsTx
  getInsertInvocationTx               = lift getInsertInvocationTx
  getSetRetryTx                       = lift getSetRetryTx
  getSetScheduledEventStatusTx        = lift getSetScheduledEventStatusTx
  getUnlockAllLockedScheduledEventsTx = lift getUnlockAllLockedScheduledEventsTx
  getClearFutureCronEventsTx          = lift getClearFutureCronEventsTx

instance (MonadMetadataStorageTx m) => MonadMetadataStorageTx (Tracing.TraceT m) where

  getFetchMetadataTx         = lift getFetchMetadataTx
  getSetMetadataTx           = lift getSetMetadataTx
  getNotifySchemaCacheSyncTx = lift getNotifySchemaCacheSyncTx

  getInsertActionTx      = lift getInsertActionTx
  getUndeliveredEventsTx = lift getUndeliveredEventsTx
  getSetActionStatusTx   = lift getSetActionStatusTx

  getDeprivedCronTriggerStatsTx       = lift getDeprivedCronTriggerStatsTx
  getPartialCronEventsTx              = lift getPartialCronEventsTx
  getOneOffScheduledEventsTx          = lift getOneOffScheduledEventsTx
  getInsertCronEventsTx               = lift getInsertCronEventsTx
  getInsertInvocationTx               = lift getInsertInvocationTx
  getSetRetryTx                       = lift getSetRetryTx
  getSetScheduledEventStatusTx        = lift getSetScheduledEventStatusTx
  getUnlockAllLockedScheduledEventsTx = lift getUnlockAllLockedScheduledEventsTx
  getClearFutureCronEventsTx          = lift getClearFutureCronEventsTx
