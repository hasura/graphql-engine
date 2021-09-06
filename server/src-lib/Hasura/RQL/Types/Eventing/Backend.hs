module Hasura.RQL.Types.Eventing.Backend where

import           Hasura.Prelude

import           Data.Aeson

import qualified Hasura.Backends.Postgres.DDL.EventTrigger as PG
import qualified Hasura.Tracing                            as Tracing

import           Hasura.Base.Error
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Source                   ()
import           Hasura.SQL.Backend
import           Hasura.Session                            (UserInfo)

-- | The @BackendEventTrigger@ type class contains functions which
--   interact with the source database to perform event trigger related
--   operations like fetching pending events from the database or
--   inserting a new invocation log after processing an event.
class Backend b => BackendEventTrigger (b :: BackendType) where

  -- | insertManualEvent inserts the specified event
  --   in the event log table, note that this method should
  --   also set the trace context and session variables in the
  --   source database context (if available)
  insertManualEvent
    :: ( MonadIO m, MonadError QErr m)
    => SourceConfig b
    -> TableName b
    -> TriggerName
    -> Value
    -> UserInfo
    -> Tracing.TraceContext
    -> m EventId

  redeliverEvent
    :: (MonadIO m, MonadError QErr m)
    => SourceConfig b
    -> EventId
    -> m ()

instance BackendEventTrigger ('Postgres 'Vanilla) where
  insertManualEvent           = PG.insertManualEvent
  redeliverEvent              = PG.redeliverEvent

instance BackendEventTrigger ('Postgres 'Citus) where
  insertManualEvent _ _ _ _ _ _   = throw400 NotSupported $ "Event triggers are not supported for Citus sources"
  redeliverEvent _ _ = throw400 NotSupported $ "Event triggers are not supported for Citus sources"

instance BackendEventTrigger 'MSSQL where
  insertManualEvent _ _ _ _ _ _   = throw400 NotSupported "Event triggers are not supported for MS-SQL sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for MS-SQL sources"

instance BackendEventTrigger 'BigQuery where
  insertManualEvent _ _ _ _ _ _   = throw400 NotSupported "Event triggers are not supported for BigQuery sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for BigQuery sources"

instance BackendEventTrigger 'MySQL where
  insertManualEvent _ _ _ _ _ _  = throw400 NotSupported "Event triggers are not supported for MySQL sources"
  redeliverEvent _ _ = throw400 NotSupported "Event triggers are not supported for MySQL sources"
