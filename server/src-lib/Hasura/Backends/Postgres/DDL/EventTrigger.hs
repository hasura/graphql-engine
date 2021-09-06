module Hasura.Backends.Postgres.DDL.EventTrigger
  ( insertManualEvent
  , redeliverEvent
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query                   as Q

import           Data.Aeson

import qualified Hasura.Tracing                      as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Types  hiding (TableName)
import           Hasura.Base.Error
import           Hasura.RQL.Types.Backend            (SourceConfig, TableName)
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Table              ()
import           Hasura.SQL.Backend
import           Hasura.Session

insertManualEvent
  :: (MonadIO m, MonadError QErr m)
  => SourceConfig ('Postgres pgKind)
  -> TableName ('Postgres pgKind)
  -> TriggerName
  -> Value
  -> UserInfo
  -> Tracing.TraceContext
  -> m EventId
insertManualEvent sourceConfig tableName triggerName payload userInfo traceCtx =
  -- NOTE: The methods `setTraceContextInTx` and `setHeadersTx` are being used
  -- to ensure that the trace context and user info are set with valid values
  -- while being used in the PG function `insert_event_log`.
  -- See Issue(#7087) for more details on a bug that was being caused
  -- in the absence of these methods.
  liftEitherM
  $ liftIO
  $ runPgSourceWriteTx sourceConfig
  $ setHeadersTx (_uiSession userInfo)
  >> setTraceContextInTx traceCtx
  >> insertPGManualEvent tableName triggerName payload

redeliverEvent
  :: (MonadIO m, MonadError QErr m)
  => SourceConfig ('Postgres pgKind)
  -> EventId
  -> m ()
redeliverEvent sourceConfig eventId =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig (redeliverEventTx eventId)

---- DATABASE QUERIES ---------------------
--
--   The API for our in-database work queue:
-------------------------------------------

insertPGManualEvent
  :: QualifiedTable
  -> TriggerName
  -> Value
  -> Q.TxE QErr EventId
insertPGManualEvent (QualifiedObject schemaName tableName) triggerName rowData = do
  runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler [Q.sql|
    SELECT hdb_catalog.insert_event_log($1, $2, $3, $4, $5)
  |] (schemaName, tableName, triggerName, (tshow MANUAL), Q.AltJ rowData) False


checkEvent :: EventId -> Q.TxE QErr ()
checkEvent eid = do
  events <- Q.listQE defaultTxErrorHandler
            [Q.sql|
              SELECT l.locked IS NOT NULL AND l.locked >= (NOW() - interval '30 minute')
              FROM hdb_catalog.event_log l
              WHERE l.id = $1
              |] (Identity eid) True
  event <- getEvent events
  assertEventUnlocked event
  where
    getEvent []    = throw400 NotExists "event not found"
    getEvent (x:_) = return x

    assertEventUnlocked (Identity locked) = when locked $
      throw400 Busy "event is already being processed"

markForDelivery :: EventId -> Q.TxE QErr ()
markForDelivery eid =
  Q.unitQE defaultTxErrorHandler [Q.sql|
          UPDATE hdb_catalog.event_log
          SET
          delivered = 'f',
          error = 'f',
          tries = 0
          WHERE id = $1
          |] (Identity eid) True

redeliverEventTx :: EventId -> Q.TxE QErr ()
redeliverEventTx eventId = do
  checkEvent eventId
  markForDelivery eventId
