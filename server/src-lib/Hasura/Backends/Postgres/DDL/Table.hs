module Hasura.Backends.Postgres.DDL.Table
  ( createTableEventTrigger
  , buildEventTriggerInfo
  , updateColumnInEventTrigger
  , fetchAndValidateEnumValues
  , delTriggerQ
  , mkAllTriggersQ
  , getHeaderInfosFromConf
  )
where

import           Hasura.Prelude

import qualified Data.Environment                    as Env
import qualified Data.HashMap.Strict                 as Map
import qualified Data.List.NonEmpty                  as NE
import qualified Data.Sequence                       as Seq
import qualified Data.Sequence.NonEmpty              as NESeq
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as TL
import qualified Database.PG.Query                   as Q
import qualified Language.GraphQL.Draft.Syntax       as G
import qualified Text.Shakespeare.Text               as ST

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Control.Monad.Validate
import           Data.FileEmbed                      (makeRelativeToProject)
import           Data.List                           (delete)
import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend               as AB

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.DML
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Base.Error
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.SchemaCache
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend
import           Hasura.SQL.Types
import           Hasura.Server.Types
import           Hasura.Server.Utils


-- | Create the table event trigger in the database in a @'/v1/query' API
-- transaction as soon as after @'runCreateEventTriggerQuery' is called and
-- in building schema cache.
createTableEventTrigger
  :: (Backend ('Postgres pgKind), MonadIO m, MonadBaseControl IO m)
  => ServerConfigCtx
  -> PGSourceConfig
  -> QualifiedTable
  -> [ColumnInfo ('Postgres pgKind)]
  -> TriggerName
  -> TriggerOpsDef
  -> m (Either QErr ())
createTableEventTrigger serverConfigCtx sourceConfig table columns triggerName opsDefinition = runPgSourceWriteTx sourceConfig $ do
  -- Clean all existing triggers
  liftTx $ delTriggerQ triggerName -- executes DROP IF EXISTS.. sql
  -- Create the given triggers
  flip runReaderT serverConfigCtx $
    mkAllTriggersQ triggerName table columns opsDefinition

delTriggerQ :: TriggerName -> Q.TxE QErr ()
delTriggerQ trn =
  mapM_ (\op -> Q.unitQE
                defaultTxErrorHandler
          (Q.fromText $ getDropFuncSql op) () False) [INSERT, UPDATE, DELETE]
  where
    getDropFuncSql :: Ops -> T.Text
    getDropFuncSql op =
      "DROP FUNCTION IF EXISTS"
      <> " hdb_catalog." <> pgIdenTrigger op trn <> "()"
      <> " CASCADE"

-- pgIdenTrigger is a method used to construct the name of the pg function
-- used for event triggers which are present in the hdb_catalog schema.
pgIdenTrigger:: Ops -> TriggerName -> Text
pgIdenTrigger op trn = pgFmtIdentifier . qualifyTriggerName op $ triggerNameToTxt trn
  where
    qualifyTriggerName op' trn' = "notify_hasura_" <> trn' <> "_" <> tshow op'

mkAllTriggersQ
  :: forall pgKind m
   . (Backend ('Postgres pgKind), MonadTx m, MonadReader ServerConfigCtx m)
  => TriggerName
  -> QualifiedTable
  -> [ColumnInfo ('Postgres pgKind)]
  -> TriggerOpsDef
  -> m ()
mkAllTriggersQ trn qt allCols fullspec = do
  onJust (tdInsert fullspec) (mkTriggerQ trn qt allCols INSERT)
  onJust (tdUpdate fullspec) (mkTriggerQ trn qt allCols UPDATE)
  onJust (tdDelete fullspec) (mkTriggerQ trn qt allCols DELETE)

data OpVar = OLD | NEW deriving (Show)

-- | Formats each columns to appropriate SQL expression
toJSONableExp :: Bool -> ColumnType ('Postgres pgKind) -> Bool -> SQLExp -> SQLExp
toJSONableExp strfyNum colTy  asText expn
  -- If its a numeric column greater than a 32-bit integer, we have to stringify it as JSON spec doesn't support >32-bit integers
  | asText || (isScalarColumnWhere isBigNum colTy && strfyNum) =
    expn `SETyAnn` textTypeAnn
  -- If the column is either a `Geometry` or `Geography` then apply the `ST_AsGeoJSON` function to convert it into GeoJSON format
  | isScalarColumnWhere isGeoType colTy =
      SEFnApp "ST_AsGeoJSON"
      [ expn
      , SEUnsafe "15" -- max decimal digits
      , SEUnsafe "4"  -- to print out crs
      ] Nothing `SETyAnn` jsonTypeAnn
  | otherwise = expn

-- | Define the pgSQL trigger functions on database events.
mkTriggerQ
  :: forall pgKind m
   . (Backend ('Postgres pgKind), MonadTx m, MonadReader ServerConfigCtx m)
  => TriggerName
  -> QualifiedTable
  -> [ColumnInfo ('Postgres pgKind)]
  -> Ops
  -> SubscribeOpSpec
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

buildEventTriggerInfo
  :: forall (pgKind :: PostgresKind) m
   . (Backend ('Postgres pgKind),  QErrM m)
  => Env.Environment
  -> SourceName
  -> QualifiedTable
  -> EventTriggerConf
  -> m (EventTriggerInfo, [SchemaDependency])
buildEventTriggerInfo env source qt (EventTriggerConf name def webhook webhookFromEnv rconf mheaders) = do
  webhookConf <- case (webhook, webhookFromEnv) of
    (Just w, Nothing)    -> return $ WCValue w
    (Nothing, Just wEnv) -> return $ WCEnv wEnv
    _                    -> throw500 "expected webhook or webhook_from_env"
  let headerConfs = fromMaybe [] mheaders
  webhookInfo <- getWebhookInfoFromConf env webhookConf
  headerInfos <- getHeaderInfosFromConf env headerConfs
  let eTrigInfo = EventTriggerInfo name def rconf webhookInfo headerInfos
      tabDep = SchemaDependency
                 (SOSourceObj source
                   $ AB.mkAnyBackend
                   $ SOITable @('Postgres pgKind) qt)
                 DRParent
  pure (eTrigInfo, tabDep:getTrigDefDeps @pgKind source qt def)

getTrigDefDeps
  :: forall (pgKind :: PostgresKind)
   . (Backend ('Postgres pgKind))
  => SourceName
  -> QualifiedTable
  -> TriggerOpsDef
  -> [SchemaDependency]
getTrigDefDeps source qt (TriggerOpsDef mIns mUpd mDel _) =
  mconcat $ catMaybes [ subsOpSpecDeps <$> mIns
                      , subsOpSpecDeps <$> mUpd
                      , subsOpSpecDeps <$> mDel
                      ]
  where
    subsOpSpecDeps :: SubscribeOpSpec -> [SchemaDependency]
    subsOpSpecDeps os =
      let cols = getColsFromSub $ sosColumns os
          colDeps = flip map cols $ \col ->
            SchemaDependency
              (SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITableObj @('Postgres pgKind) qt (TOCol @('Postgres pgKind) col))
              DRColumn
          payload = maybe [] getColsFromSub (sosPayload os)
          payloadDeps = flip map payload $ \col ->
            SchemaDependency
              (SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITableObj qt (TOCol @('Postgres pgKind) col))
              DRPayload
        in colDeps <> payloadDeps
    getColsFromSub sc = case sc of
      SubCStar         -> []
      SubCArray pgcols -> pgcols

getHeaderInfosFromConf
  :: QErrM m
  => Env.Environment
  -> [HeaderConf]
  -> m [EventHeaderInfo]
getHeaderInfosFromConf env = mapM getHeader
  where
    getHeader :: QErrM m => HeaderConf -> m EventHeaderInfo
    getHeader hconf = case hconf of
      (HeaderConf _ (HVValue val)) -> return $ EventHeaderInfo hconf val
      (HeaderConf _ (HVEnv val))   -> do
        envVal <- getEnv env val
        return $ EventHeaderInfo hconf envVal

getWebhookInfoFromConf
  :: QErrM m
  => Env.Environment
  -> WebhookConf
  -> m WebhookConfInfo
getWebhookInfoFromConf env wc = case wc of
  WCValue w -> do
    resolvedWebhook <- resolveWebhook env w
    return $ WebhookConfInfo wc $ unResolvedWebhook resolvedWebhook
  WCEnv we -> do
    envVal <- getEnv env we
    return $ WebhookConfInfo wc envVal

updateColumnInEventTrigger
  :: QualifiedTable
  -> PGCol
  -> PGCol
  -> QualifiedTable
  -> EventTriggerConf -> EventTriggerConf
updateColumnInEventTrigger table oCol nCol refTable = rewriteEventTriggerConf
  where
    rewriteSubsCols = \case
      SubCStar       -> SubCStar
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
      etc { etcDefinition =
            rewriteTrigOpsDef $ etcDefinition etc
          }
    getNewCol col =
      if table == refTable && oCol == col then nCol else col

data EnumTableIntegrityError (b :: BackendType)
  = EnumTablePostgresError !Text
  | EnumTableMissingPrimaryKey
  | EnumTableMultiColumnPrimaryKey ![PGCol]
  | EnumTableNonTextualPrimaryKey !(RawColumnInfo b)
  | EnumTableNoEnumValues
  | EnumTableInvalidEnumValueNames !(NE.NonEmpty Text)
  | EnumTableNonTextualCommentColumn !(RawColumnInfo b)
  | EnumTableTooManyColumns ![PGCol]

fetchAndValidateEnumValues
  :: forall pgKind m
   . (Backend ('Postgres pgKind), MonadIO m, MonadBaseControl IO m)
  => PGSourceConfig
  -> QualifiedTable
  -> Maybe (PrimaryKey ('Postgres pgKind) (RawColumnInfo ('Postgres pgKind)))
  -> [RawColumnInfo ('Postgres pgKind)]
  -> m (Either QErr EnumValues)
fetchAndValidateEnumValues pgSourceConfig tableName maybePrimaryKey columnInfos = runExceptT $
  either (throw400 ConstraintViolation . showErrors) pure =<< runValidateT fetchAndValidate
  where
    fetchAndValidate
      :: (MonadIO n, MonadBaseControl IO n, MonadValidate [EnumTableIntegrityError ('Postgres pgKind)] n)
      => n EnumValues
    fetchAndValidate = do
      maybePrimaryKeyColumn <- tolerate validatePrimaryKey
      maybeCommentColumn <- validateColumns maybePrimaryKeyColumn
      case maybePrimaryKeyColumn of
        Nothing               -> refute mempty
        Just primaryKeyColumn -> do
          result <- runPgSourceReadTx pgSourceConfig $ runValidateT $
                    fetchEnumValuesFromDb tableName primaryKeyColumn maybeCommentColumn
          case result of
            Left e             -> (refute . pure . EnumTablePostgresError . qeError) e
            Right (Left vErrs) -> refute vErrs
            Right (Right r)    -> pure r
      where
        validatePrimaryKey = case maybePrimaryKey of
          Nothing -> refute [EnumTableMissingPrimaryKey]
          Just primaryKey -> case _pkColumns primaryKey of
            column NESeq.:<|| Seq.Empty -> case prciType column of
              PGText -> pure column
              _      -> refute [EnumTableNonTextualPrimaryKey column]
            columns -> refute [EnumTableMultiColumnPrimaryKey $ map prciName (toList columns)]

        validateColumns primaryKeyColumn = do
          let nonPrimaryKeyColumns = maybe columnInfos (`delete` columnInfos) primaryKeyColumn
          case nonPrimaryKeyColumns of
            [] -> pure Nothing
            [column] -> case prciType column of
              PGText -> pure $ Just column
              _      -> dispute [EnumTableNonTextualCommentColumn column] $> Nothing
            columns -> dispute [EnumTableTooManyColumns $ map prciName columns] $> Nothing

    showErrors :: [EnumTableIntegrityError ('Postgres pgKind)] -> Text
    showErrors allErrors =
      "the table " <> tableName <<> " cannot be used as an enum " <> reasonsMessage
      where
        reasonsMessage = makeReasonMessage allErrors showOne

        showOne :: EnumTableIntegrityError ('Postgres pgKind) -> Text
        showOne = \case
          EnumTablePostgresError err -> "postgres error: " <> err
          EnumTableMissingPrimaryKey -> "the table must have a primary key"
          EnumTableMultiColumnPrimaryKey cols ->
            "the table’s primary key must not span multiple columns ("
              <> commaSeparated (sort cols) <> ")"
          EnumTableNonTextualPrimaryKey colInfo -> typeMismatch "primary key" colInfo PGText
          EnumTableNoEnumValues -> "the table must have at least one row"
          EnumTableInvalidEnumValueNames values ->
            let pluralString = " are not valid GraphQL enum value names"
                valuesString = case NE.reverse (NE.sort values) of
                  value NE.:| [] -> "value " <> value <<> " is not a valid GraphQL enum value name"
                  value2 NE.:| [value1] -> "values " <> value1 <<> " and " <> value2 <<> pluralString
                  lastValue NE.:| otherValues ->
                    "values " <> commaSeparated (reverse otherValues) <> ", and "
                      <> lastValue <<> pluralString
            in "the " <> valuesString
          EnumTableNonTextualCommentColumn colInfo -> typeMismatch "comment column" colInfo PGText
          EnumTableTooManyColumns cols ->
            "the table must have exactly one primary key and optionally one comment column, not "
              <> tshow (length cols) <> " columns ("
              <> commaSeparated (sort cols) <> ")"
          where
            typeMismatch description colInfo expected =
              "the table’s " <> description <> " (" <> prciName colInfo <<> ") must have type "
                <> expected <<> ", not type " <>> prciType colInfo

fetchEnumValuesFromDb
  :: forall pgKind m
   . (MonadTx m, MonadValidate [EnumTableIntegrityError ('Postgres pgKind)] m)
  => QualifiedTable
  -> RawColumnInfo ('Postgres pgKind)
  -> Maybe (RawColumnInfo ('Postgres pgKind))
  -> m EnumValues
fetchEnumValuesFromDb tableName primaryKeyColumn maybeCommentColumn = do
  let nullExtr = Extractor SENull Nothing
      commentExtr = maybe nullExtr (mkExtr . prciName) maybeCommentColumn
      query = Q.fromBuilder $ toSQL mkSelect
        { selFrom = Just $ mkSimpleFromExp tableName
        , selExtr = [mkExtr (prciName primaryKeyColumn), commentExtr] }
  rawEnumValues <- liftTx $ Q.withQE defaultTxErrorHandler query () True
  when (null rawEnumValues) $ dispute [EnumTableNoEnumValues]
  let enumValues = flip map rawEnumValues $
        \(enumValueText, comment) ->
          case mkValidEnumValueName enumValueText of
            Nothing        -> Left enumValueText
            Just enumValue -> Right (EnumValue enumValue, EnumValueInfo comment)
      badNames = lefts enumValues
      validEnums = rights enumValues
  case NE.nonEmpty badNames of
    Just someBadNames -> refute [EnumTableInvalidEnumValueNames someBadNames]
    Nothing           -> pure $ Map.fromList validEnums
  where
    -- https://graphql.github.io/graphql-spec/June2018/#EnumValue
    mkValidEnumValueName name =
      if name `elem` ["true", "false", "null"] then Nothing
      else G.mkName name
