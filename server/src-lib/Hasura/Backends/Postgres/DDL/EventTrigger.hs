module Hasura.Backends.Postgres.DDL.EventTrigger
  ( insertManualEvent
  , redeliverEvent
  , dropTriggerAndArchiveEvents
  , createTableEventTrigger
  , dropTriggerQ
  , mkAllTriggersQ
  ) where

import           Hasura.Prelude

import qualified Data.Text.Lazy                            as TL
import qualified Database.PG.Query                         as Q
import qualified Text.Shakespeare.Text                     as ST

import           Control.Monad.Trans.Control               (MonadBaseControl)
import           Data.Aeson
import           Data.FileEmbed                            (makeRelativeToProject)

import qualified Hasura.Tracing                            as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.DML
import           Hasura.Backends.Postgres.SQL.Types        hiding (TableName)
import           Hasura.Backends.Postgres.Translate.Column
import           Hasura.Base.Error
import           Hasura.RQL.Types.Backend                  (Backend, SourceConfig, TableName)
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Table                    ()
import           Hasura.SQL.Backend
import           Hasura.SQL.Types
import           Hasura.Server.Types
import           Hasura.Session

-- Corresponds to the 'OLD' and 'NEW' Postgres records; see
-- https://www.postgresql.org/docs/current/plpgsql-trigger.html
data OpVar = OLD | NEW deriving (Show)

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

dropTriggerAndArchiveEvents
  :: ( MonadIO m
     , MonadError QErr m
     )
  => SourceConfig ('Postgres pgKind)
  -> TriggerName
  -> m ()
dropTriggerAndArchiveEvents sourceConfig triggerName =
  liftEitherM $ liftIO $ runPgSourceWriteTx sourceConfig $ do
    dropTriggerQ triggerName
    archiveEvents triggerName

createTableEventTrigger
  :: (Backend ('Postgres pgKind), MonadIO m, MonadBaseControl IO m)
  => ServerConfigCtx
  -> PGSourceConfig
  -> QualifiedTable
  -> [ColumnInfo ('Postgres pgKind)]
  -> TriggerName
  -> TriggerOpsDef ('Postgres pgKind)
  -> m (Either QErr ())
createTableEventTrigger serverConfigCtx sourceConfig table columns triggerName opsDefinition = runPgSourceWriteTx sourceConfig $ do
  -- Clean all existing triggers
  liftTx $ dropTriggerQ triggerName -- executes DROP IF EXISTS.. sql
  -- Create the given triggers
  flip runReaderT serverConfigCtx $
    mkAllTriggersQ triggerName table columns opsDefinition

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

dropTriggerQ :: TriggerName -> Q.TxE QErr ()
dropTriggerQ trn =
  mapM_ (\op -> Q.unitQE
                defaultTxErrorHandler
          (Q.fromText $ getDropFuncSql op) () False) [INSERT, UPDATE, DELETE]
  where
    getDropFuncSql :: Ops -> Text
    getDropFuncSql op =
      "DROP FUNCTION IF EXISTS"
      <> " hdb_catalog." <> pgIdenTrigger op trn <> "()"
      <> " CASCADE"

archiveEvents :: TriggerName -> Q.TxE QErr ()
archiveEvents trn =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.event_log
           SET archived = 't'
           WHERE trigger_name = $1
                |] (Identity trn) False

---- Postgres event trigger utility functions ---------------------

-- | pgIdenTrigger is a method used to construct the name of the pg function
-- used for event triggers which are present in the hdb_catalog schema.
pgIdenTrigger:: Ops -> TriggerName -> Text
pgIdenTrigger op trn = pgFmtIdentifier . qualifyTriggerName op $ triggerNameToTxt trn
  where
    qualifyTriggerName op' trn' = "notify_hasura_" <> trn' <> "_" <> tshow op'

-- | Define the pgSQL trigger functions on database events.
mkTriggerQ
  :: forall pgKind m
   . (Backend ('Postgres pgKind), MonadTx m, MonadReader ServerConfigCtx m)
  => TriggerName
  -> QualifiedTable
  -> [ColumnInfo ('Postgres pgKind)]
  -> Ops
  -> SubscribeOpSpec ('Postgres pgKind)
  -> m ()
mkTriggerQ trn qt@(QualifiedObject schema table) allCols op (SubscribeOpSpec listenColumns deliveryColumns') = do
  strfyNum <- stringifyNum . _sccSQLGenCtx <$> ask
  liftTx $ Q.multiQE defaultTxErrorHandler $ Q.fromText . TL.toStrict $
    let
        -- If there are no specific delivery columns selected by user then all the columns will be delivered
        -- in payload hence 'SubCStar'.
        deliveryColumns = fromMaybe SubCStar deliveryColumns'
        getApplicableColumns = \case
          SubCStar       -> allCols
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
        tableName  = pgFmtLit $ getTableTxt table

        operation = tshow op
        oldRow = toSQLTxt $ renderRow OLD
        newRow = toSQLTxt $ renderRow NEW
        oldPayloadExpression = toSQLTxt oldDataExp
        newPayloadExpression = toSQLTxt newDataExp

    in $(makeRelativeToProject "src-rsr/trigger.sql.shakespeare" >>= ST.stextFile )
  where
    applyRowToJson' e = SEFnApp "row_to_json" [e] Nothing
    applyRow e = SEFnApp "row" [e] Nothing
    opToQual = QualVar . tshow

    mkRowExpression opVar strfyNum columns
      = mkRowExp $ map (\col -> toExtractor (mkQId opVar strfyNum col) col) columns

    mkQId opVar strfyNum colInfo  = toJSONableExp strfyNum (pgiType colInfo) False $
          SEQIdentifier $ QIdentifier (opToQual opVar) $ toIdentifier $ pgiColumn colInfo

    -- Generate the SQL expression
    toExtractor sqlExp column
      -- If the column type is either 'Geography' or 'Geometry', then after applying the 'ST_AsGeoJSON' function
      -- to the column, alias the value of the expression with the column name else it uses `st_asgeojson` as
      -- the column name.
      | isScalarColumnWhere isGeoType (pgiType column) = Extractor sqlExp (Just $ getAlias column)
      | otherwise  = Extractor sqlExp Nothing
    getAlias col = toAlias $ Identifier $ getPGColTxt (pgiColumn col)

mkAllTriggersQ
  :: forall pgKind m
   . (Backend ('Postgres pgKind), MonadTx m, MonadReader ServerConfigCtx m)
  => TriggerName
  -> QualifiedTable
  -> [ColumnInfo ('Postgres pgKind)]
  -> TriggerOpsDef ('Postgres pgKind)
  -> m ()
mkAllTriggersQ trn qt allCols fullspec = do
  onJust (tdInsert fullspec) (mkTriggerQ trn qt allCols INSERT)
  onJust (tdUpdate fullspec) (mkTriggerQ trn qt allCols UPDATE)
  onJust (tdDelete fullspec) (mkTriggerQ trn qt allCols DELETE)
