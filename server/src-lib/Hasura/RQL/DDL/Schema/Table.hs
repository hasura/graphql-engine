{-# LANGUAGE Arrows #-}

-- | Description: Create/delete SQL tables to/from Hasura metadata.
module Hasura.RQL.DDL.Schema.Table
  ( TrackTable(..)
  , runTrackTableQ

  , TrackTableV2(..)
  , runTrackTableV2Q

  , UntrackTable(..)
  , runUntrackTableQ

  , SetTableIsEnum(..)
  , runSetExistingTableIsEnumQ

  , SetTableCustomFields(..)
  , runSetTableCustomFieldsQV2

  , buildTableCache
  , dropTableInMetadata
  -- , delTableAndDirectDeps
  , processTableChanges
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as Map
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.HashSet                       as S
import qualified Data.Text                          as T
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Arrow.Extended
import           Control.Lens.Extended              hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Instances.TH.Lift                  ()
import           Language.Haskell.TH.Syntax         (Lift)
import           Network.URI.Extended               ()

import qualified Hasura.Incremental                 as Inc

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Schema.Common       (textToName)
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Catalog
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Enum
import           Hasura.RQL.DDL.Schema.Rename
import           Hasura.RQL.Types                   hiding (fmFunction)
import           Hasura.Server.Utils
import           Hasura.SQL.Types


data TrackTable
  = TrackTable
  { tSource :: !SourceName
  , tName   :: !QualifiedTable
  , tIsEnum :: !Bool
  } deriving (Show, Eq, Lift)

instance FromJSON TrackTable where
  parseJSON v = withOptions <|> withoutOptions
    where
      withOptions = flip (withObject "TrackTable") v $ \o -> TrackTable
        <$> o .:? "source" .!= defaultSource
        <*> o .: "table"
        <*> o .:? "is_enum" .!= False
      withoutOptions = TrackTable <$> pure defaultSource <*> parseJSON v <*> pure False

instance ToJSON TrackTable where
  toJSON (TrackTable source name isEnum)
    = object [ "source" .= source, "table" .= name, "is_enum" .= isEnum ]

data SetTableIsEnum
  = SetTableIsEnum
  { stieSource :: !SourceName
  , stieTable  :: !QualifiedTable
  , stieIsEnum :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''SetTableIsEnum)

data UntrackTable =
  UntrackTable
  { utSource  :: !SourceName
  , utTable   :: !QualifiedTable
  , utCascade :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''UntrackTable)

isTableTracked :: SchemaCache -> SourceName -> QualifiedTable -> Bool
isTableTracked sc source tableName =
  isJust $ getPGTableInfo source tableName $ scPostgres sc

-- | Track table/view, Phase 1:
-- Validate table tracking operation. Fails if table is already being tracked,
-- or if a function with the same name is being tracked.
trackExistingTableOrViewP1 :: (QErrM m, CacheRWM m) => SourceName -> QualifiedTable -> m ()
trackExistingTableOrViewP1 source qt = do
  rawSchemaCache <- askSchemaCache
  when (isTableTracked rawSchemaCache source qt) $
    throw400 AlreadyTracked $ "view/table already tracked : " <>> qt
  let qf = fmap (FunctionName . getTableTxt) qt
  when (isJust $ getPGFunctionInfo source qf $ scPostgres rawSchemaCache) $
    throw400 NotSupported $ "function with name " <> qt <<> " already exists"

-- | Check whether a given name would conflict with the current schema by doing
-- an internal introspection
checkConflictingNode
  :: MonadError QErr m
  => SchemaCache
  -> T.Text
  -> m ()
checkConflictingNode sc tnGQL = do
  let queryParser = gqlQueryParser $ scUnauthenticatedGQLContext sc
      -- {
      --   __schema {
      --     queryType {
      --       fields {
      --         name
      --       }
      --     }
      --   }
      -- }
      introspectionQuery =
        [ G.SelectionField $ G.Field Nothing $$(G.litName "__schema") mempty []
          [ G.SelectionField $ G.Field Nothing $$(G.litName "queryType") mempty []
            [ G.SelectionField $ G.Field Nothing $$(G.litName "fields") mempty []
              [ G.SelectionField $ G.Field Nothing $$(G.litName "name") mempty []
                []
              ]
            ]
          ]
        ]
  case queryParser introspectionQuery of
    Left _ -> pure ()
    Right (results, _reusability) -> do
      case OMap.lookup $$(G.litName "__schema") results of
        Just (RFRaw (Object schema)) -> do
          let names = do
                Object queryType <- Map.lookup "queryType" schema
                Array fields <- Map.lookup "fields" queryType
                traverse (\case Object field -> do
                                  String name <- Map.lookup "name" field
                                  pure name
                                _ -> Nothing) fields
          case names of
            Nothing -> pure ()
            Just ns ->
              if tnGQL `elem` ns
              then throw400 RemoteSchemaConflicts $
                   "node " <> tnGQL <>
                   " already exists in current graphql schema"
              else pure ()
        _ -> pure ()

trackExistingTableOrViewP2
  :: (MonadError QErr m, CacheRWM m)
  => SourceName -> QualifiedTable -> Bool -> TableConfig -> m EncJSON
trackExistingTableOrViewP2 source tableName isEnum config = do
  sc <- askSchemaCache
  {-
  The next line does more than what it says on the tin.  Removing the following
  call to 'checkConflictingNode' causes memory usage to spike when newly
  tracking a large amount (~100) of tables.  The memory usage can be triggered
  by first creating a large amount of tables through SQL, without tracking the
  tables, and then clicking "track all" in the console.  Curiously, this high
  memory usage happens even when no substantial GraphQL schema is generated.
  -}
  checkConflictingNode sc $ snakeCaseQualObject tableName
  -- saveTableToCatalog tableName isEnum config
  let metadata = mkTableMeta tableName isEnum config
  buildSchemaCacheFor (MOSourceObjId source $ SMOTable tableName)
    $ MetadataModifier
    $ metaSources.ix source.smTables %~ Map.insert tableName metadata
  pure successMsg

runTrackTableQ
  :: (MonadError QErr m, CacheRWM m) => TrackTable -> m EncJSON
runTrackTableQ (TrackTable source qt isEnum) = do
  trackExistingTableOrViewP1 source qt
  trackExistingTableOrViewP2 source qt isEnum emptyTableConfig

data TrackTableV2
  = TrackTableV2
  { ttv2Table         :: !TrackTable
  , ttv2Configuration :: !TableConfig
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 4 snakeCase) ''TrackTableV2)

runTrackTableV2Q
  :: (MonadError QErr m, CacheRWM m) => TrackTableV2 -> m EncJSON
runTrackTableV2Q (TrackTableV2 (TrackTable source qt isEnum) config) = do
  trackExistingTableOrViewP1 source qt
  trackExistingTableOrViewP2 source qt isEnum config

runSetExistingTableIsEnumQ :: (MonadError QErr m, CacheRWM m) => SetTableIsEnum -> m EncJSON
runSetExistingTableIsEnumQ (SetTableIsEnum source tableName isEnum) = do
  void $ askTabInfo source tableName -- assert that table is tracked
  -- updateTableIsEnumInCatalog tableName isEnum
  buildSchemaCacheFor (MOSourceObjId source $ SMOTable tableName)
    $ MetadataModifier
    $ tableMetadataSetter source tableName.tmIsEnum .~ isEnum
  return successMsg

data SetTableCustomFields
  = SetTableCustomFields
  { _stcfSource            :: !SourceName
  , _stcfTable             :: !QualifiedTable
  , _stcfCustomRootFields  :: !TableCustomRootFields
  , _stcfCustomColumnNames :: !CustomColumnNames
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 5 snakeCase) ''SetTableCustomFields)

instance FromJSON SetTableCustomFields where
  parseJSON = withObject "SetTableCustomFields" $ \o ->
    SetTableCustomFields
    <$> o .:? "source" .!= defaultSource
    <*> o .: "table"
    <*> o .:? "custom_root_fields" .!= emptyCustomRootFields
    <*> o .:? "custom_column_names" .!= Map.empty

runSetTableCustomFieldsQV2
  :: (MonadError QErr m, CacheRWM m) => SetTableCustomFields -> m EncJSON
runSetTableCustomFieldsQV2 (SetTableCustomFields source tableName rootFields columnNames) = do
  void $ askTabInfo source tableName -- assert that table is tracked
  let tableConfig = TableConfig rootFields columnNames
  -- updateTableConfig tableName (TableConfig rootFields columnNames)
  buildSchemaCacheFor (MOSourceObjId source $ SMOTable tableName)
    $ MetadataModifier
    $ tableMetadataSetter source tableName.tmConfiguration .~ tableConfig
  return successMsg

unTrackExistingTableOrViewP1
  :: (CacheRM m, QErrM m) => UntrackTable -> m ()
unTrackExistingTableOrViewP1 (UntrackTable source vn _) = do
  rawSchemaCache <- askSchemaCache
  case getPGTableInfo source vn $ scPostgres rawSchemaCache of
    Just ti ->
      -- Check if table/view is system defined
      when (isSystemDefined $ _tciSystemDefined $ _tiCoreInfo ti) $ throw400 NotSupported $
        vn <<> " is system defined, cannot untrack"
    Nothing -> throw400 AlreadyUntracked $
      "view/table already untracked : " <>> vn

unTrackExistingTableOrViewP2
  :: (CacheRWM m, MonadError QErr m)
  => UntrackTable -> m EncJSON
unTrackExistingTableOrViewP2 (UntrackTable source qtn cascade) = withNewInconsistentObjsCheck do
  sc <- askSchemaCache

  -- Get relational, query template and function dependants
  let allDeps = getDependentObjs sc (SOSourceObj source $ SOITable qtn)
      indirectDeps = filter (not . isDirectDep) allDeps
  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not (or cascade)) $ reportDepsExt indirectDeps []
  -- Purge all the dependents from state
  metadataModifier <- execWriterT do
    mapM_ (purgeDependentObject >=> tell) indirectDeps
    tell $ dropTableInMetadata source qtn
  -- delete the table and its direct dependencies
  buildSchemaCache metadataModifier
  pure successMsg
  where
    isDirectDep = \case
      SOSourceObj s (SOITableObj dtn _) ->
        s == source && qtn == dtn
      _                  -> False

dropTableInMetadata :: SourceName -> QualifiedTable -> MetadataModifier
dropTableInMetadata source table =
  MetadataModifier $ metaSources.ix source.smTables %~ Map.delete table

runUntrackTableQ
  :: (CacheRWM m, MonadError QErr m)
  => UntrackTable -> m EncJSON
runUntrackTableQ q = do
  unTrackExistingTableOrViewP1 q
  unTrackExistingTableOrViewP2 q

processTableChanges
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => SourceName -> TableCoreInfo -> TableDiff -> m ()
processTableChanges source ti tableDiff = do
  -- If table rename occurs then don't replace constraints and
  -- process dropped/added columns, because schema reload happens eventually
  sc <- askSchemaCache
  let tn = _tciName ti
      withOldTabName = do
        procAlteredCols sc tn

      withNewTabName newTN = do
        let tnGQL = snakeCaseQualObject newTN
        -- check for GraphQL schema conflicts on new name
        checkConflictingNode sc tnGQL
        procAlteredCols sc tn
        -- update new table in metadata
        renameTableInMetadata source newTN tn

  -- Process computed field diff
  processComputedFieldDiff tn
  -- Drop custom column names for dropped columns
  possiblyDropCustomColumnNames tn
  maybe withOldTabName withNewTabName mNewName
  where
    TableDiff mNewName droppedCols _ alteredCols _ computedFieldDiff _ _ = tableDiff

    possiblyDropCustomColumnNames tn = do
      let TableConfig customFields customColumnNames = _tciCustomConfig ti
          modifiedCustomColumnNames = foldl' (flip Map.delete) customColumnNames droppedCols
      when (modifiedCustomColumnNames /= customColumnNames) $
        tell $ MetadataModifier $
          tableMetadataSetter source tn.tmConfiguration .~ (TableConfig customFields modifiedCustomColumnNames)

    procAlteredCols sc tn = for_ alteredCols $
      \( PGRawColumnInfo oldName _ oldType _ _
       , PGRawColumnInfo newName _ newType _ _ ) -> do
        if | oldName /= newName ->
             renameColumnInMetadata oldName newName source tn (_tciFieldInfoMap ti)

           | oldType /= newType -> do
              let colId = SOSourceObj source $ SOITableObj tn $ TOCol oldName
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

-- delTableAndDirectDeps :: (MonadError QErr m) => QualifiedTable -> m ()
-- delTableAndDirectDeps qtn@(QualifiedObject sn tn) = do
--   liftTx $ Q.catchE defaultTxErrorHandler $ do
--     Q.unitQ [Q.sql|
--              DELETE FROM "hdb_catalog"."hdb_relationship"
--              WHERE table_schema = $1 AND table_name = $2
--               |] (sn, tn) False
--     Q.unitQ [Q.sql|
--              DELETE FROM "hdb_catalog"."hdb_permission"
--              WHERE table_schema = $1 AND table_name = $2
--               |] (sn, tn) False
--     Q.unitQ [Q.sql|
--              DELETE FROM "hdb_catalog"."event_triggers"
--              WHERE schema_name = $1 AND table_name = $2
--               |] (sn, tn) False
--     Q.unitQ [Q.sql|
--              DELETE FROM "hdb_catalog"."hdb_computed_field"
--              WHERE table_schema = $1 AND table_name = $2
--               |] (sn, tn) False
--     Q.unitQ [Q.sql|
--              DELETE FROM "hdb_catalog"."hdb_remote_relationship"
--              WHERE table_schema = $1 AND table_name = $2
--               |] (sn, tn) False
--   deleteTableFromCatalog qtn

-- | Builds an initial @'TableCache' 'PGColumnInfo'@ from catalog information. Does not fill in
-- '_tiRolePermInfoMap' or '_tiEventTriggerInfoMap' at all, and '_tiFieldInfoMap' only contains
-- columns, not relationships; those pieces of information are filled in by later stages.
buildTableCache
  :: forall arr m
   . ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , Inc.ArrowCache m arr, MonadIO m
     )
  => ( SourceName
     , PGSourceConfig
     , PostgresTablesMetadata
     , [TableBuildInput]
     , Inc.Dependency Inc.InvalidationKey
     ) `arr` Map.HashMap QualifiedTable TableRawInfo
buildTableCache = Inc.cache proc (source, pgSourceConfig, pgTables, tableBuildInputs, reloadMetadataInvalidationKey) -> do
  rawTableInfos <-
    (| Inc.keyed (| withTable (\tables
         -> (tables, (pgSourceConfig, pgTables, reloadMetadataInvalidationKey))
         >- first noDuplicateTables >>> buildRawTableInfo) |)
    |) (withSourceInKey source $ Map.groupOnNE _tbiName tableBuildInputs)
  let rawTableCache = removeSourceInKey $ Map.catMaybes rawTableInfos
      enumTables = flip Map.mapMaybe rawTableCache \rawTableInfo ->
        (,) <$> _tciPrimaryKey rawTableInfo <*> _tciEnumValues rawTableInfo
  tableInfos <-
    (| Inc.keyed (| withTable (\table -> processTableInfo -< (enumTables, table)) |)
    |) (withSourceInKey source rawTableCache)
  returnA -< removeSourceInKey (Map.catMaybes tableInfos)
  where
    withSourceInKey :: (Eq k, Hashable k) => SourceName -> HashMap k v -> HashMap (SourceName, k) v
    withSourceInKey source = Map.fromList . map (first (source,)) . Map.toList

    removeSourceInKey :: (Eq k, Hashable k) => HashMap (SourceName, k) v -> HashMap k v
    removeSourceInKey = Map.fromList . map (first snd) . Map.toList

    withTable :: ErrorA QErr arr (e, s) a -> arr (e, ((SourceName, QualifiedTable), s)) (Maybe a)
    withTable f = withRecordInconsistency f <<<
      second (first $ arr \(source, name) -> MetadataObject (MOSourceObjId source $ SMOTable name) (toJSON name))

    noDuplicateTables = proc tables -> case tables of
      table :| [] -> returnA -< table
      _           -> throwA -< err400 AlreadyExists "duplication definition for table"

    -- Step 1: Build the raw table cache from metadata information.
    buildRawTableInfo
      :: ErrorA QErr arr
       ( TableBuildInput
       , (PGSourceConfig, PostgresTablesMetadata, Inc.Dependency Inc.InvalidationKey)
       ) (TableCoreInfoG PGRawColumnInfo PGCol)
    buildRawTableInfo = Inc.cache proc (tableBuildInput, (pgSourceConfig, pgTables, reloadMetadataInvalidationKey)) -> do
      let TableBuildInput name isEnum config = tableBuildInput
          maybeInfo = Map.lookup name pgTables
      metadataTable <-
        (| onNothingA (throwA -<
             err400 NotExists $ "no such table/view exists in postgres: " <>> name)
        |) maybeInfo

      let columns = _tmiColumns metadataTable
          columnMap = mapFromL (fromPGCol . prciName) columns
          primaryKey = _tmiPrimaryKey metadataTable
      rawPrimaryKey <- liftEitherA -< traverse (resolvePrimaryKeyColumns columnMap) primaryKey
      enumValues <- if isEnum
        then do
          -- We want to make sure we reload enum values whenever someone explicitly calls
          -- `reload_metadata`.
          Inc.dependOn -< reloadMetadataInvalidationKey
          eitherEnums <- bindA -< fetchAndValidateEnumValues pgSourceConfig name rawPrimaryKey columns
          liftEitherA -< Just <$> eitherEnums
        else returnA -< Nothing

      returnA -< TableCoreInfo
        { _tciName = name
        , _tciSystemDefined = SystemDefined False
        , _tciFieldInfoMap = columnMap
        , _tciPrimaryKey = primaryKey
        , _tciUniqueConstraints = _tmiUniqueConstraints metadataTable
        , _tciForeignKeys = S.map unForeignKeyMetadata $ _tmiForeignKeys metadataTable
        , _tciViewInfo = _tmiViewInfo metadataTable
        , _tciEnumValues = enumValues
        , _tciCustomConfig = config
        , _tciDescription = _tmiDescription metadataTable
        }

    -- Step 2: Process the raw table cache to replace Postgres column types with logical column
    -- types.
    processTableInfo
      :: ErrorA QErr arr
       ( Map.HashMap QualifiedTable (PrimaryKey PGCol, EnumValues)
       , TableCoreInfoG PGRawColumnInfo PGCol
       ) TableRawInfo
    processTableInfo = proc (enumTables, rawInfo) -> liftEitherA -< do
      let columns = _tciFieldInfoMap rawInfo
          enumReferences = resolveEnumReferences enumTables (_tciForeignKeys rawInfo)
      columnInfoMap <-
            alignCustomColumnNames columns (_tcCustomColumnNames $ _tciCustomConfig rawInfo)
        >>= traverse (processColumnInfo enumReferences (_tciName rawInfo))
      assertNoDuplicateFieldNames (Map.elems columnInfoMap)

      primaryKey <- traverse (resolvePrimaryKeyColumns columnInfoMap) (_tciPrimaryKey rawInfo)
      pure rawInfo
        { _tciFieldInfoMap = columnInfoMap
        , _tciPrimaryKey = primaryKey
        }

    resolvePrimaryKeyColumns
      :: (QErrM n) => HashMap FieldName a -> PrimaryKey PGCol -> n (PrimaryKey a)
    resolvePrimaryKeyColumns columnMap = traverseOf (pkColumns.traverse) \columnName ->
      Map.lookup (fromPGCol columnName) columnMap
        `onNothing` throw500 "column in primary key not in table!"

    alignCustomColumnNames
      :: (QErrM n)
      => FieldInfoMap PGRawColumnInfo
      -> CustomColumnNames
      -> n (FieldInfoMap (PGRawColumnInfo, G.Name))
    alignCustomColumnNames columns customNames = do
      let customNamesByFieldName = Map.fromList $ map (first fromPGCol) $ Map.toList customNames
      flip Map.traverseWithKey (align columns customNamesByFieldName) \columnName -> \case
        This column -> (column,) <$> textToName (getFieldNameTxt columnName)
        These column customName -> pure (column, customName)
        That customName -> throw400 NotExists $ "the custom field name " <> customName
          <<> " was given for the column " <> columnName <<> ", but no such column exists"

    -- | “Processes” a 'PGRawColumnInfo' into a 'PGColumnInfo' by resolving its type using a map of
    -- known enum tables.
    processColumnInfo
      :: (QErrM n)
      => Map.HashMap PGCol (NonEmpty EnumReference)
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
          case Map.lookup pgCol tableEnumReferences of
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
      flip Map.traverseWithKey (Map.groupOn pgiName columns) \name columnsWithName ->
        case columnsWithName of
          one:two:more -> throw400 AlreadyExists $ "the definitions of columns "
            <> englishList "and" (dquoteTxt . pgiColumn <$> (one:|two:more))
            <> " are in conflict: they are mapped to the same field name, " <>> name
          _ -> pure ()
