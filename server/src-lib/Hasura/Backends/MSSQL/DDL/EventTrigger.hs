module Hasura.Backends.MSSQL.DDL.EventTrigger (createTableEventTrigger) where

import Data.FileEmbed (makeRelativeToProject)
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Text.Lazy qualified as LT
import Database.MSSQL.Transaction (TxE, unitQueryE)
import Database.ODBC.SQLServer (rawUnescapedText)
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.SQL.Error qualified as HGE
import Hasura.Backends.MSSQL.Types (SchemaName (..), TableName (..))
import Hasura.Backends.MSSQL.Types.Internal (columnNameText, geoTypes)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Eventing (OpVar (..))
import Hasura.RQL.Types.Table (PrimaryKey (..))
import Hasura.SQL.Backend
import Hasura.Server.Types
import Text.Shakespeare.Text qualified as ST

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
createTableEventTrigger _serverConfigCtx (MSSQLSourceConfig _ mssqlExecCtx) table columns triggerName opsDefinition primaryKeyMaybe = do
  liftIO $
    runExceptT $ do
      mssqlRunReadWrite mssqlExecCtx $ do
        dropTriggerQ triggerName (tableSchema table)
        mkAllTriggersQ triggerName table columns opsDefinition primaryKeyMaybe

newtype QualifiedTriggerName = QualifiedTriggerName {unQualifiedTriggerName :: Text}

msssqlIdenTrigger :: Ops -> SchemaName -> TriggerName -> QualifiedTriggerName
msssqlIdenTrigger op (SchemaName schemaName) triggerName =
  QualifiedTriggerName $ qualifyHasuraTriggerName op $ triggerNameToTxt triggerName
  where
    qualifyHasuraTriggerName op' triggerName' = schemaName <> "." <> "notify_hasura_" <> triggerName' <> "_" <> tshow op'

dropTriggerQ :: TriggerName -> SchemaName -> TxE QErr ()
dropTriggerQ triggerName schemaName =
  mapM_
    ( \op ->
        unitQueryE
          HGE.defaultMSSQLTxErrorHandler
          (rawUnescapedText $ getDropTriggerSQL op)
    )
    [INSERT, UPDATE, DELETE]
  where
    getDropTriggerSQL :: Ops -> Text
    getDropTriggerSQL op =
      "DROP TRIGGER IF EXISTS " <> unQualifiedTriggerName (msssqlIdenTrigger op schemaName triggerName)

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
      isGeoTypesInListenCols = any (\c -> isScalarColumnWhere isGeoType (ciType c)) listenColumns
      isGeoTypesInDeliversCols = any (\c -> isScalarColumnWhere isGeoType (ciType c)) deliveryColumns
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

generateColumnTriggerAlias :: OpVar -> Maybe Text -> ColumnInfo 'MSSQL -> Text
generateColumnTriggerAlias op colPrefixMaybe colInfo =
  let opText =
        case op of
          OLD -> "old"
          NEW -> "new"
      dbColNameText = columnNameText $ ciColumn colInfo
      joinPrefixedDbColNameText =
        -- prefix with the joining table's name
        -- `id` -> `inserted.id` (prefix = 'inserted')
        case colPrefixMaybe of
          Just colPrefix -> colPrefix <> "." <> dbColNameText
          Nothing -> dbColNameText
      dbColAlias = "data" <> "." <> opText <> "." <> dbColNameText
   in LT.toStrict $ [ST.stext| #{joinPrefixedDbColNameText} as [#{dbColAlias}]|]

qualifyTableName :: TableName -> Text
qualifyTableName (TableName tableName (SchemaName schemaName)) =
  if schemaName == "dbo"
    then tableName
    else schemaName <> "." <> tableName

mkInsertTriggerQuery :: TableName -> TriggerName -> [ColumnInfo 'MSSQL] -> LT.Text
mkInsertTriggerQuery table@(TableName tableName schema@(SchemaName schemaName)) triggerName columns =
  let QualifiedTriggerName qualifiedTriggerName = msssqlIdenTrigger INSERT schema triggerName
      triggerNameText = triggerNameToTxt triggerName
      qualifiedTableName = qualifyTableName table
      deliveryColsSQLExpression :: Text =
        commaSeparated $ map (generateColumnTriggerAlias NEW Nothing) columns
   in $(makeRelativeToProject "src-rsr/mssql_insert_trigger.sql.shakespeare" >>= ST.stextFile)

mkDeleteTriggerQuery :: TableName -> TriggerName -> [ColumnInfo 'MSSQL] -> LT.Text
mkDeleteTriggerQuery table@(TableName tableName schema@(SchemaName schemaName)) triggerName columns =
  let QualifiedTriggerName qualifiedTriggerName = msssqlIdenTrigger DELETE schema triggerName
      triggerNameText = triggerNameToTxt triggerName
      qualifiedTableName = qualifyTableName table
      deliveryColsSQLExpression :: Text = commaSeparated $ map (generateColumnTriggerAlias OLD Nothing) columns
   in $(makeRelativeToProject "src-rsr/mssql_delete_trigger.sql.shakespeare" >>= ST.stextFile)

mkPrimaryKeyJoinExp :: Text -> Text -> [ColumnInfo 'MSSQL] -> Text
mkPrimaryKeyJoinExp lhsPrefix rhsPrefix columns =
  T.intercalate " AND " $ singleColExp <$> columns
  where
    singleColExp colInfo =
      let dbColNameText = columnNameText $ ciColumn colInfo
       in LT.toStrict $ [ST.stext| #{lhsPrefix}.#{dbColNameText} = #{rhsPrefix}.#{dbColNameText} |]

mkListenColumnsExp :: Text -> Text -> [ColumnInfo 'MSSQL] -> Text
mkListenColumnsExp _ _ [] = ""
mkListenColumnsExp lhsPrefix rhsPrefix columns =
  "where " <> (T.intercalate " OR " $ singleColExp <$> columns)
  where
    singleColExp colInfo =
      let dbColNameText = columnNameText $ ciColumn colInfo
       in LT.toStrict $ [ST.stext| #{lhsPrefix}.#{dbColNameText} != #{rhsPrefix}.#{dbColNameText} |]

--
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
        oldDeliveryColsSQLExp :: Text = commaSeparated $ map (generateColumnTriggerAlias NEW (Just "INSERTED")) deliveryColumns
        newDeliveryColsSQLExp :: Text = commaSeparated $ map (generateColumnTriggerAlias OLD (Just "DELETED")) deliveryColumns

        primaryKeyJoinExp = mkPrimaryKeyJoinExp "INSERTED" "DELETED" (toList (_pkColumns primaryKey))
        listenColumnExp = mkListenColumnsExp "INSERTED" "DELETED" listenColumns
     in $(makeRelativeToProject "src-rsr/mssql_update_trigger.sql.shakespeare" >>= ST.stextFile)
