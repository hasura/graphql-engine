{-# LANGUAGE Arrows #-}

-- | Description: Create/delete SQL tables to/from Hasura metadata.
module Hasura.RQL.DDL.Schema.Table
  ( TrackTable(..)
  , runTrackTableQ
  , trackExistingTableOrViewP2

  , TrackTableV2(..)
  , runTrackTableV2Q

  , UntrackTable(..)
  , runUntrackTableQ

  , SetTableIsEnum(..)
  , runSetExistingTableIsEnumQ

  , SetTableCustomFields(..)
  , runSetTableCustomFieldsQV2

  , buildTableCache
  , delTableAndDirectDeps
  , processTableChanges
  ) where

import           Hasura.EncJSON
import           Hasura.GraphQL.Schema.Common       (textToName)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Enum
import           Hasura.RQL.DDL.Schema.Rename
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.Server.Utils
import           Hasura.SQL.Types

import qualified Database.PG.Query                  as Q
import qualified Hasura.Incremental                 as Inc
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Arrow.Extended
import           Control.Lens.Extended              hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift                  ()
import           Language.Haskell.TH.Syntax         (Lift)
import           Network.URI.Extended               ()

import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.HashSet                       as S
import qualified Data.Text                          as T

data TrackTable
  = TrackTable
  { tName   :: !QualifiedTable
  , tIsEnum :: !Bool
  } deriving (Show, Eq, Lift)

instance FromJSON TrackTable where
  parseJSON v = withOptions <|> withoutOptions
    where
      withOptions = flip (withObject "TrackTable") v $ \o -> TrackTable
        <$> o .: "table"
        <*> o .:? "is_enum" .!= False
      withoutOptions = TrackTable <$> parseJSON v <*> pure False

instance ToJSON TrackTable where
  toJSON (TrackTable name isEnum)
    | isEnum = object [ "table" .= name, "is_enum" .= isEnum ]
    | otherwise = toJSON name

data SetTableIsEnum
  = SetTableIsEnum
  { stieTable  :: !QualifiedTable
  , stieIsEnum :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''SetTableIsEnum)

data UntrackTable =
  UntrackTable
  { utTable   :: !QualifiedTable
  , utCascade :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''UntrackTable)

-- | Track table/view, Phase 1:
-- Validate table tracking operation. Fails if table is already being tracked,
-- or if a function with the same name is being tracked.
trackExistingTableOrViewP1 :: (QErrM m, CacheRWM m) => QualifiedTable -> m ()
trackExistingTableOrViewP1 qt = do
  rawSchemaCache <- askSchemaCache
  when (M.member qt $ scTables rawSchemaCache) $
    throw400 AlreadyTracked $ "view/table already tracked : " <>> qt
  let qf = fmap (FunctionName . getTableTxt) qt
  when (M.member qf $ scFunctions rawSchemaCache) $
    throw400 NotSupported $ "function with name " <> qt <<> " already exists"

trackExistingTableOrViewP2
  :: (MonadTx m, CacheRWM m, HasSystemDefined m)
  => QualifiedTable -> Bool -> TableConfig -> m EncJSON
trackExistingTableOrViewP2 tableName isEnum config = do
  sc <- askSchemaCache
  -- FIXME
  -- let defGCtx = scDefaultRemoteGCtx sc
  -- GS.checkConflictingNode defGCtx $ GS.qualObjectToName tableName
  saveTableToCatalog tableName isEnum config
  buildSchemaCacheFor (MOTable tableName)
  return successMsg

runTrackTableQ
  :: (MonadTx m, CacheRWM m, HasSystemDefined m) => TrackTable -> m EncJSON
runTrackTableQ (TrackTable qt isEnum) = do
  trackExistingTableOrViewP1 qt
  trackExistingTableOrViewP2 qt isEnum emptyTableConfig

data TrackTableV2
  = TrackTableV2
  { ttv2Table         :: !TrackTable
  , ttv2Configuration :: !TableConfig
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''TrackTableV2)

runTrackTableV2Q
  :: (MonadTx m, CacheRWM m, HasSystemDefined m) => TrackTableV2 -> m EncJSON
runTrackTableV2Q (TrackTableV2 (TrackTable qt isEnum) config) = do
  trackExistingTableOrViewP1 qt
  trackExistingTableOrViewP2 qt isEnum config

runSetExistingTableIsEnumQ :: (MonadTx m, CacheRWM m) => SetTableIsEnum -> m EncJSON
runSetExistingTableIsEnumQ (SetTableIsEnum tableName isEnum) = do
  void $ askTabInfo tableName -- assert that table is tracked
  updateTableIsEnumInCatalog tableName isEnum
  buildSchemaCacheFor (MOTable tableName)
  return successMsg

data SetTableCustomFields
  = SetTableCustomFields
  { _stcfTable             :: !QualifiedTable
  , _stcfCustomRootFields  :: !TableCustomRootFields
  , _stcfCustomColumnNames :: !CustomColumnNames
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 5 snakeCase) ''SetTableCustomFields)

instance FromJSON SetTableCustomFields where
  parseJSON = withObject "SetTableCustomFields" $ \o ->
    SetTableCustomFields
    <$> o .: "table"
    <*> o .:? "custom_root_fields" .!= emptyCustomRootFields
    <*> o .:? "custom_column_names" .!= M.empty

runSetTableCustomFieldsQV2
  :: (MonadTx m, CacheRWM m) => SetTableCustomFields -> m EncJSON
runSetTableCustomFieldsQV2 (SetTableCustomFields tableName rootFields columnNames) = do
  void $ askTabInfo tableName -- assert that table is tracked
  updateTableConfig tableName (TableConfig rootFields columnNames)
  buildSchemaCacheFor (MOTable tableName)
  return successMsg

unTrackExistingTableOrViewP1
  :: (CacheRM m, QErrM m) => UntrackTable -> m ()
unTrackExistingTableOrViewP1 (UntrackTable vn _) = do
  rawSchemaCache <- askSchemaCache
  case M.lookup vn (scTables rawSchemaCache) of
    Just ti ->
      -- Check if table/view is system defined
      when (isSystemDefined $ _tciSystemDefined $ _tiCoreInfo ti) $ throw400 NotSupported $
        vn <<> " is system defined, cannot untrack"
    Nothing -> throw400 AlreadyUntracked $
      "view/table already untracked : " <>> vn

unTrackExistingTableOrViewP2
  :: (CacheRWM m, MonadTx m)
  => UntrackTable -> m EncJSON
unTrackExistingTableOrViewP2 (UntrackTable qtn cascade) = withNewInconsistentObjsCheck do
  sc <- askSchemaCache

  -- Get relational, query template and function dependants
  let allDeps = getDependentObjs sc (SOTable qtn)
      indirectDeps = filter (not . isDirectDep) allDeps
  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not (or cascade)) $ reportDepsExt indirectDeps []
  -- Purge all the dependents from state
  mapM_ purgeDependentObject indirectDeps
  -- delete the table and its direct dependencies
  delTableAndDirectDeps qtn
  buildSchemaCache

  pure successMsg
  where
    isDirectDep = \case
      (SOTableObj dtn _) -> qtn == dtn
      _                  -> False

runUntrackTableQ
  :: (CacheRWM m, MonadTx m)
  => UntrackTable -> m EncJSON
runUntrackTableQ q = do
  unTrackExistingTableOrViewP1 q
  unTrackExistingTableOrViewP2 q

processTableChanges :: (MonadTx m, CacheRM m) => TableCoreInfo -> TableDiff -> m ()
processTableChanges ti tableDiff = do
  -- If table rename occurs then don't replace constraints and
  -- process dropped/added columns, because schema reload happens eventually
  sc <- askSchemaCache
  let tn = _tciName ti
      withOldTabName = do
        procAlteredCols sc tn

      withNewTabName newTN = do
        let tnGQL = _qualObjectToName newTN
            defGCtx = _scDefaultRemoteGCtx sc
        -- check for GraphQL schema conflicts on new name
        -- FIXME
        -- _checkConflictingNode defGCtx tnGQL
        procAlteredCols sc tn
        -- update new table in catalog
        renameTableInCatalog newTN tn

  -- Process computed field diff
  processComputedFieldDiff tn
  -- Drop custom column names for dropped columns
  possiblyDropCustomColumnNames tn
  maybe withOldTabName withNewTabName mNewName
  where
    TableDiff mNewName droppedCols _ alteredCols _ computedFieldDiff _ _ = tableDiff

    possiblyDropCustomColumnNames tn = do
      let TableConfig customFields customColumnNames = _tciCustomConfig ti
          modifiedCustomColumnNames = foldl' (flip M.delete) customColumnNames droppedCols
      when (modifiedCustomColumnNames /= customColumnNames) $
        liftTx $ updateTableConfig tn $ TableConfig customFields modifiedCustomColumnNames

    procAlteredCols sc tn = for_ alteredCols $
      \( PGRawColumnInfo oldName _ oldType _ _
       , PGRawColumnInfo newName _ newType _ _ ) -> do
        if | oldName /= newName -> renameColInCatalog oldName newName tn (_tciFieldInfoMap ti)

           | oldType /= newType -> do
              let colId = SOTableObj tn $ TOCol oldName
                  typeDepObjs = getDependentObjsWith (== DROnType) sc colId

              unless (null typeDepObjs) $ throw400 DependencyError $
                "cannot change type of column " <> oldName <<> " in table "
                <> tn <<> " because of the following dependencies : " <>
                reportSchemaObjs typeDepObjs

           | otherwise -> pure ()

    processComputedFieldDiff table  = do
      let ComputedFieldDiff _ altered overloaded = computedFieldDiff
          getFunction = fmFunction . ccmFunctionMeta
      forM_ overloaded $ \(columnName, function) ->
        throw400 NotSupported $ "The function " <> function
        <<> " associated with computed field" <> columnName
        <<> " of table " <> table <<> " is being overloaded"
      forM_ altered $ \(old, new) ->
        if | (fmType . ccmFunctionMeta) new == FTVOLATILE ->
             throw400 NotSupported $ "The type of function " <> getFunction old
             <<> " associated with computed field " <> ccmName old
             <<> " of table " <> table <<> " is being altered to \"VOLATILE\""
           | otherwise -> pure ()

delTableAndDirectDeps :: (MonadTx m) => QualifiedTable -> m ()
delTableAndDirectDeps qtn@(QualifiedObject sn tn) = do
  liftTx $ Q.catchE defaultTxErrorHandler $ do
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_relationship"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_permission"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."event_triggers"
             WHERE schema_name = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_computed_field"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
    Q.unitQ [Q.sql|
             DELETE FROM "hdb_catalog"."hdb_remote_relationship"
             WHERE table_schema = $1 AND table_name = $2
              |] (sn, tn) False
  deleteTableFromCatalog qtn

-- | Builds an initial @'TableCache' 'PGColumnInfo'@ from catalog information. Does not fill in
-- '_tiRolePermInfoMap' or '_tiEventTriggerInfoMap' at all, and '_tiFieldInfoMap' only contains
-- columns, not relationships; those pieces of information are filled in by later stages.
buildTableCache
  :: forall arr m
   . ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , Inc.ArrowCache m arr, MonadTx m )
  => ( [CatalogTable]
     , Inc.Dependency Inc.InvalidationKey
     ) `arr` M.HashMap QualifiedTable TableRawInfo
buildTableCache = Inc.cache proc (catalogTables, reloadMetadataInvalidationKey) -> do
  rawTableInfos <-
    (| Inc.keyed (| withTable (\tables
         -> (tables, reloadMetadataInvalidationKey)
         >- first noDuplicateTables >>> buildRawTableInfo) |)
    |) (M.groupOnNE _ctName catalogTables)
  let rawTableCache = M.catMaybes rawTableInfos
      enumTables = flip M.mapMaybe rawTableCache \rawTableInfo ->
        (,) <$> _tciPrimaryKey rawTableInfo <*> _tciEnumValues rawTableInfo
  tableInfos <-
    (| Inc.keyed (| withTable (\table -> processTableInfo -< (enumTables, table)) |)
    |) rawTableCache
  returnA -< M.catMaybes tableInfos
  where
    withTable :: ErrorA QErr arr (e, s) a -> arr (e, (QualifiedTable, s)) (Maybe a)
    withTable f = withRecordInconsistency f <<<
      second (first $ arr \name -> MetadataObject (MOTable name) (toJSON name))

    noDuplicateTables = proc tables -> case tables of
      table :| [] -> returnA -< table
      _           -> throwA -< err400 AlreadyExists "duplication definition for table"

    -- Step 1: Build the raw table cache from metadata information.
    buildRawTableInfo
      :: ErrorA QErr arr
       ( CatalogTable
       , Inc.Dependency Inc.InvalidationKey
       ) (TableCoreInfoG PGRawColumnInfo PGCol)
    buildRawTableInfo = Inc.cache proc (catalogTable, reloadMetadataInvalidationKey) -> do
      let CatalogTable name systemDefined isEnum config maybeInfo = catalogTable
      catalogInfo <-
        (| onNothingA (throwA -<
             err400 NotExists $ "no such table/view exists in postgres: " <>> name)
        |) maybeInfo

      let columns = _ctiColumns catalogInfo
          columnMap = mapFromL (fromPGCol . prciName) columns
          primaryKey = _ctiPrimaryKey catalogInfo
      rawPrimaryKey <- liftEitherA -< traverse (resolvePrimaryKeyColumns columnMap) primaryKey
      enumValues <- if isEnum
        then do
          -- We want to make sure we reload enum values whenever someone explicitly calls
          -- `reload_metadata`.
          Inc.dependOn -< reloadMetadataInvalidationKey
          bindErrorA -< Just <$> fetchAndValidateEnumValues name rawPrimaryKey columns
        else returnA -< Nothing

      returnA -< TableCoreInfo
        { _tciName = name
        , _tciSystemDefined = systemDefined
        , _tciFieldInfoMap = columnMap
        , _tciPrimaryKey = primaryKey
        , _tciUniqueConstraints = _ctiUniqueConstraints catalogInfo
        , _tciForeignKeys = S.map unCatalogForeignKey $ _ctiForeignKeys catalogInfo
        , _tciViewInfo = _ctiViewInfo catalogInfo
        , _tciEnumValues = enumValues
        , _tciCustomConfig = config
        , _tciDescription = _ctiDescription catalogInfo
        }

    -- Step 2: Process the raw table cache to replace Postgres column types with logical column
    -- types.
    processTableInfo
      :: ErrorA QErr arr
       ( M.HashMap QualifiedTable (PrimaryKey PGCol, EnumValues)
       , TableCoreInfoG PGRawColumnInfo PGCol
       ) TableRawInfo
    processTableInfo = proc (enumTables, rawInfo) -> liftEitherA -< do
      let columns = _tciFieldInfoMap rawInfo
          enumReferences = resolveEnumReferences enumTables (_tciForeignKeys rawInfo)
      columnInfoMap <-
            alignCustomColumnNames columns (_tcCustomColumnNames $ _tciCustomConfig rawInfo)
        >>= traverse (processColumnInfo enumReferences (_tciName rawInfo))
      assertNoDuplicateFieldNames (M.elems columnInfoMap)

      primaryKey <- traverse (resolvePrimaryKeyColumns columnInfoMap) (_tciPrimaryKey rawInfo)
      pure rawInfo
        { _tciFieldInfoMap = columnInfoMap
        , _tciPrimaryKey = primaryKey
        }

    resolvePrimaryKeyColumns
      :: (QErrM n) => HashMap FieldName a -> PrimaryKey PGCol -> n (PrimaryKey a)
    resolvePrimaryKeyColumns columnMap = traverseOf (pkColumns.traverse) \columnName ->
      M.lookup (fromPGCol columnName) columnMap
        `onNothing` throw500 "column in primary key not in table!"

    alignCustomColumnNames
      :: (QErrM n)
      => FieldInfoMap PGRawColumnInfo
      -> CustomColumnNames
      -> n (FieldInfoMap (PGRawColumnInfo, G.Name))
    alignCustomColumnNames columns customNames = do
      let customNamesByFieldName = M.fromList $ map (first fromPGCol) $ M.toList customNames
      flip M.traverseWithKey (align columns customNamesByFieldName) \columnName -> \case
        This column -> (column,) <$> textToName (getFieldNameTxt columnName)
        These column customName -> pure (column, customName)
        That customName -> throw400 NotExists $ "the custom field name " <> customName
          <<> " was given for the column " <> columnName <<> ", but no such column exists"

    -- | “Processes” a 'PGRawColumnInfo' into a 'PGColumnInfo' by resolving its type using a map of
    -- known enum tables.
    processColumnInfo
      :: (QErrM n)
      => M.HashMap PGCol (NonEmpty EnumReference)
      -> QualifiedTable -- ^ the table this column belongs to
      -> (PGRawColumnInfo, G.Name)
      -> n PGColumnInfo
    processColumnInfo tableEnumReferences tableName (rawInfo, name) = do
      resolvedType <- resolveColumnType
      pure PGColumnInfo
        { pgiColumn = pgCol
        , pgiName = name
        , pgiPosition = prciPosition rawInfo
        , pgiType = resolvedType
        , pgiIsNullable = prciIsNullable rawInfo
        , pgiDescription = prciDescription rawInfo
        }
      where
        pgCol = prciName rawInfo
        resolveColumnType =
          case M.lookup pgCol tableEnumReferences of
            -- no references? not an enum
            Nothing -> pure $ PGColumnScalar (prciType rawInfo)
            -- one reference? is an enum
            Just (enumReference:|[]) -> pure $ PGColumnEnumReference enumReference
            -- multiple referenced enums? the schema is strange, so let’s reject it
            Just enumReferences -> throw400 ConstraintViolation
              $ "column " <> prciName rawInfo <<> " in table " <> tableName
              <<> " references multiple enum tables ("
              <> T.intercalate ", " (map (dquote . erTable) $ toList enumReferences) <> ")"

    assertNoDuplicateFieldNames columns =
      flip M.traverseWithKey (M.groupOn pgiName columns) \name columnsWithName ->
        case columnsWithName of
          one:two:more -> throw400 AlreadyExists $ "the definitions of columns "
            <> englishList "and" (dquoteTxt . pgiColumn <$> (one:|two:more))
            <> " are in conflict: they are mapped to the same field name, " <>> name
          _ -> pure ()
