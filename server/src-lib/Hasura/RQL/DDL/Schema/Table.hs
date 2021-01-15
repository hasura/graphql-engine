{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows              #-}

-- | Description: Create/delete SQL tables to/from Hasura metadata.
module Hasura.RQL.DDL.Schema.Table
  ( TrackTable(..)
  , runTrackTableQ

  , TrackTableV2(..)
  , runTrackTableV2Q

  , UntrackTable(..)
  , runUntrackTableQ
  , dropTableInMetadata

  , SetTableIsEnum(..)
  , runSetExistingTableIsEnumQ

  , SetTableCustomFields(..)
  , runSetTableCustomFieldsQV2

  , SetTableCustomization(..)
  , runSetTableCustomization

  , buildTableCache
  , processTableChanges
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as Map
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.HashSet                       as S
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Arrow.Extended
import           Control.Lens.Extended              hiding ((.=))
import           Control.Monad.Trans.Control        (MonadBaseControl)
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended

import qualified Hasura.Incremental                 as Inc

import           Hasura.Backends.Postgres.SQL.Types (FunctionName (..), QualifiedTable,
                                                     snakeCaseQualifiedObject)
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Schema.Common       (textToName)
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Common
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.DDL.Schema.Enum
import           Hasura.RQL.DDL.Schema.Rename
import           Hasura.RQL.Types                   hiding (fmFunction)
import           Hasura.Server.Utils


data TrackTable
  = TrackTable
  { tSource :: !SourceName
  , tName   :: !QualifiedTable
  , tIsEnum :: !Bool
  } deriving (Show, Eq)

instance FromJSON TrackTable where
  parseJSON v = withOptions <|> withoutOptions
    where
      withOptions = flip (withObject "TrackTable") v $ \o -> TrackTable
        <$> o .:? "source" .!= defaultSource
        <*> o .: "table"
        <*> o .:? "is_enum" .!= False
      withoutOptions = TrackTable defaultSource <$> parseJSON v <*> pure False

instance ToJSON TrackTable where
  toJSON (TrackTable source name isEnum)
    | isEnum = object [ "source" .= source, "table" .= name, "is_enum" .= isEnum ]
    | otherwise = toJSON name

data SetTableIsEnum
  = SetTableIsEnum
  { stieSource :: !SourceName
  , stieTable  :: !QualifiedTable
  , stieIsEnum :: !Bool
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 4 snakeCase) ''SetTableIsEnum)

instance FromJSON SetTableIsEnum where
  parseJSON = withObject "Object" $ \o ->
    SetTableIsEnum
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "is_enum"

data UntrackTable =
  UntrackTable
  { utSource  :: !SourceName
  , utTable   :: !QualifiedTable
  , utCascade :: !Bool
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''UntrackTable)

instance FromJSON UntrackTable where
  parseJSON = withObject "Object" $ \o ->
    UntrackTable
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .:? "cascade" .!= False

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
  let qf = fmap (FunctionName . toTxt) qt
  when (isJust $ getPGFunctionInfo source qf $ scPostgres rawSchemaCache) $
    throw400 NotSupported $ "function with name " <> qt <<> " already exists"

-- | Check whether a given name would conflict with the current schema by doing
-- an internal introspection
checkConflictingNode
  :: MonadError QErr m
  => SchemaCache
  -> Text
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
  :: (MonadError QErr m, CacheRWM m, MetadataM m)
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
  checkConflictingNode sc $ snakeCaseQualifiedObject tableName
  let metadata = mkTableMeta tableName isEnum config
  buildSchemaCacheFor (MOSourceObjId source $ SMOTable tableName)
    $ MetadataModifier
    $ metaSources.ix source.smTables %~ OMap.insert tableName metadata
  pure successMsg

runTrackTableQ
  :: (MonadError QErr m, CacheRWM m, MetadataM m) => TrackTable -> m EncJSON
runTrackTableQ (TrackTable source qt isEnum) = do
  trackExistingTableOrViewP1 source qt
  trackExistingTableOrViewP2 source qt isEnum emptyTableConfig

data TrackTableV2
  = TrackTableV2
  { ttv2Table         :: !TrackTable
  , ttv2Configuration :: !TableConfig
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 4 snakeCase) ''TrackTableV2)

instance FromJSON TrackTableV2 where
  parseJSON = withObject "Object" $ \o -> do
    table <- parseJSON $ Object o
    configuration <- o .:? "configuration" .!= emptyTableConfig
    pure $ TrackTableV2 table configuration

runTrackTableV2Q
  :: (MonadError QErr m, CacheRWM m, MetadataM m) => TrackTableV2 -> m EncJSON
runTrackTableV2Q (TrackTableV2 (TrackTable source qt isEnum) config) = do
  trackExistingTableOrViewP1 source qt
  trackExistingTableOrViewP2 source qt isEnum config

runSetExistingTableIsEnumQ :: (MonadError QErr m, CacheRWM m, MetadataM m) => SetTableIsEnum -> m EncJSON
runSetExistingTableIsEnumQ (SetTableIsEnum source tableName isEnum) = do
  void $ askTabInfo source tableName -- assert that table is tracked
  buildSchemaCacheFor (MOSourceObjId source $ SMOTable tableName)
    $ MetadataModifier
    $ tableMetadataSetter source tableName.tmIsEnum .~ isEnum
  return successMsg

data SetTableCustomization
  = SetTableCustomization
  { _stcSource        :: !SourceName
  , _stcTable         :: !QualifiedTable
  , _stcConfiguration :: !TableConfig
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 4 snakeCase) ''SetTableCustomization)

instance FromJSON SetTableCustomization where
  parseJSON = withObject "Object" $ \o ->
    SetTableCustomization
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "configuration"

data SetTableCustomFields
  = SetTableCustomFields
  { _stcfSource            :: !SourceName
  , _stcfTable             :: !QualifiedTable
  , _stcfCustomRootFields  :: !TableCustomRootFields
  , _stcfCustomColumnNames :: !CustomColumnNames
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 5 snakeCase) ''SetTableCustomFields)

instance FromJSON SetTableCustomFields where
  parseJSON = withObject "SetTableCustomFields" $ \o ->
    SetTableCustomFields
    <$> o .:? "source" .!= defaultSource
    <*> o .: "table"
    <*> o .:? "custom_root_fields" .!= emptyCustomRootFields
    <*> o .:? "custom_column_names" .!= Map.empty

runSetTableCustomFieldsQV2
  :: (QErrM m, CacheRWM m, MetadataM m) => SetTableCustomFields -> m EncJSON
runSetTableCustomFieldsQV2 (SetTableCustomFields source tableName rootFields columnNames) = do
  void $ askTabInfo source tableName -- assert that table is tracked
  let tableConfig = TableConfig rootFields columnNames Nothing
  buildSchemaCacheFor (MOSourceObjId source $ SMOTable tableName)
    $ MetadataModifier
    $ tableMetadataSetter source tableName.tmConfiguration .~ tableConfig
  return successMsg

runSetTableCustomization
  :: (QErrM m, CacheRWM m, MetadataM m) => SetTableCustomization -> m EncJSON
runSetTableCustomization (SetTableCustomization source table config) = do
  void $ askTabInfo source table
  buildSchemaCacheFor (MOSourceObjId source $ SMOTable table)
    $ MetadataModifier
    $ tableMetadataSetter source table.tmConfiguration .~ config
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
  :: (CacheRWM m, QErrM m, MetadataM m)
  => UntrackTable -> m EncJSON
unTrackExistingTableOrViewP2 (UntrackTable source qtn cascade) = withNewInconsistentObjsCheck do
  sc <- askSchemaCache

  -- Get relational, query template and function dependants
  let allDeps = getDependentObjs sc (SOSourceObj source $ SOITable qtn)
      indirectDeps = filter (not . isDirectDep) allDeps
  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not cascade) $ reportDepsExt indirectDeps []
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
  MetadataModifier $ metaSources.ix source.smTables %~ OMap.delete table

runUntrackTableQ
  :: (CacheRWM m, QErrM m, MetadataM m)
  => UntrackTable -> m EncJSON
runUntrackTableQ q = do
  unTrackExistingTableOrViewP1 q
  unTrackExistingTableOrViewP2 q

processTableChanges
  :: ( MonadError QErr m
     , CacheRM m
     , MonadWriter MetadataModifier m
     )
  => SourceName -> TableCoreInfo 'Postgres -> TableDiff 'Postgres -> m ()
processTableChanges source ti tableDiff = do
  -- If table rename occurs then don't replace constraints and
  -- process dropped/added columns, because schema reload happens eventually
  sc <- askSchemaCache
  let tn = _tciName ti
      withOldTabName = do
        procAlteredCols sc tn

      withNewTabName newTN = do
        let tnGQL = snakeCaseQualifiedObject newTN
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
      let TableConfig customFields customColumnNames customName = _tciCustomConfig ti
          modifiedCustomColumnNames = foldl' (flip Map.delete) customColumnNames droppedCols
      when (modifiedCustomColumnNames /= customColumnNames) $
        tell $ MetadataModifier $
          tableMetadataSetter source tn.tmConfiguration .~ (TableConfig customFields modifiedCustomColumnNames customName)

    procAlteredCols sc tn = for_ alteredCols $
      \( RawColumnInfo oldName _ oldType _ _
       , RawColumnInfo newName _ newType _ _ ) -> do
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

-- | Builds an initial @'TableCache' 'ColumnInfo'@ from catalog information. Does not fill in
-- '_tiRolePermInfoMap' or '_tiEventTriggerInfoMap' at all, and '_tiFieldInfoMap' only contains
-- columns, not relationships; those pieces of information are filled in by later stages.
buildTableCache
  :: forall arr m
   . ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , Inc.ArrowCache m arr, MonadIO m, MonadBaseControl IO m
     )
  => ( SourceName
     , SourceConfig 'Postgres
     , DBTablesMetadata 'Postgres
     , [TableBuildInput]
     , Inc.Dependency Inc.InvalidationKey
     ) `arr` Map.HashMap (TableName 'Postgres) (TableCoreInfoG 'Postgres (ColumnInfo 'Postgres) (ColumnInfo 'Postgres))
buildTableCache = Inc.cache proc (source, pgSourceConfig, pgTables, tableBuildInputs, reloadMetadataInvalidationKey) -> do
  rawTableInfos <-
    (| Inc.keyed (| withTable (\tables -> do
         table <- noDuplicateTables -< tables
         let maybeInfo = Map.lookup (_tbiName table) pgTables
         buildRawTableInfo -< (table, maybeInfo, pgSourceConfig, reloadMetadataInvalidationKey)
         )
       |)
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
    withSourceInKey source = mapKeys (source,)

    removeSourceInKey :: (Eq k, Hashable k) => HashMap (SourceName, k) v -> HashMap k v
    removeSourceInKey = mapKeys snd

    withTable :: ErrorA QErr arr (e, s) a -> arr (e, ((SourceName, TableName 'Postgres), s)) (Maybe a)
    withTable f = withRecordInconsistency f <<<
      second (first $ arr \(source, name) -> MetadataObject (MOSourceObjId source $ SMOTable name) (toJSON name))

    noDuplicateTables = proc tables -> case tables of
      table :| [] -> returnA -< table
      _           -> throwA -< err400 AlreadyExists "duplication definition for table"

    -- Step 1: Build the raw table cache from metadata information.
    buildRawTableInfo
      :: ErrorA QErr arr
       ( TableBuildInput
       , Maybe (DBTableMetadata 'Postgres)
       , SourceConfig 'Postgres
       , Inc.Dependency Inc.InvalidationKey
       ) (TableCoreInfoG 'Postgres (RawColumnInfo 'Postgres) (Column 'Postgres))
    buildRawTableInfo = Inc.cache proc (tableBuildInput, maybeInfo, pgSourceConfig, reloadMetadataInvalidationKey) -> do
      let TableBuildInput name isEnum config = tableBuildInput
      metadataTable <-
        (| onNothingA (throwA -<
             err400 NotExists $ "no such table/view exists in postgres: " <>> name)
        |) maybeInfo

      let columns = _ptmiColumns metadataTable
          columnMap = mapFromL (FieldName . toTxt . prciName) columns
          primaryKey = _ptmiPrimaryKey metadataTable
      rawPrimaryKey <- liftEitherA -< traverse (resolvePrimaryKeyColumns @'Postgres columnMap) primaryKey
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
        , _tciUniqueConstraints = _ptmiUniqueConstraints metadataTable
        , _tciForeignKeys = S.map unForeignKeyMetadata $ _ptmiForeignKeys metadataTable
        , _tciViewInfo = _ptmiViewInfo metadataTable
        , _tciEnumValues = enumValues
        , _tciCustomConfig = config
        , _tciDescription = _ptmiDescription metadataTable
        }

    -- Step 2: Process the raw table cache to replace Postgres column types with logical column
    -- types.
    processTableInfo
      :: forall b
       . Backend b
      => ErrorA QErr arr
       ( Map.HashMap (TableName b) (PrimaryKey (Column b), EnumValues)
       , TableCoreInfoG b (RawColumnInfo b) (Column b)
       ) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b))
    processTableInfo = proc (enumTables, rawInfo) -> liftEitherA -< do
      let columns = _tciFieldInfoMap rawInfo
          enumReferences = resolveEnumReferences enumTables (_tciForeignKeys rawInfo)
      columnInfoMap <-
            alignCustomColumnNames columns (_tcCustomColumnNames $ _tciCustomConfig rawInfo)
        >>= traverse (processColumnInfo enumReferences (_tciName rawInfo))
      assertNoDuplicateFieldNames (Map.elems columnInfoMap)

      primaryKey <- traverse (resolvePrimaryKeyColumns @b columnInfoMap) (_tciPrimaryKey rawInfo)
      pure rawInfo
        { _tciFieldInfoMap = columnInfoMap
        , _tciPrimaryKey = primaryKey
        }

    resolvePrimaryKeyColumns
      :: forall b n a . (Backend b, QErrM n) => HashMap FieldName a -> PrimaryKey (Column b) -> n (PrimaryKey a)
    resolvePrimaryKeyColumns columnMap = traverseOf (pkColumns.traverse) \columnName ->
      Map.lookup (FieldName (toTxt columnName)) columnMap
        `onNothing` throw500 "column in primary key not in table!"

    alignCustomColumnNames
      :: (QErrM n)
      => FieldInfoMap (RawColumnInfo b)
      -> CustomColumnNames
      -> n (FieldInfoMap (RawColumnInfo b, G.Name))
    alignCustomColumnNames columns customNames = do
      let customNamesByFieldName = mapKeys (fromCol @'Postgres) customNames
      flip Map.traverseWithKey (align columns customNamesByFieldName) \columnName -> \case
        This column -> (column,) <$> textToName (getFieldNameTxt columnName)
        These column customName -> pure (column, customName)
        That customName -> throw400 NotExists $ "the custom field name " <> customName
          <<> " was given for the column " <> columnName <<> ", but no such column exists"

    -- | “Processes” a '(RawColumnInfo 'Postgres)' into a 'PGColumnInfo' by resolving its type using a map of
    -- known enum tables.
    processColumnInfo
      :: (Backend b, QErrM n)
      => Map.HashMap (Column b) (NonEmpty (EnumReference b))
      -> TableName b -- ^ the table this column belongs to
      -> (RawColumnInfo b, G.Name)
      -> n (ColumnInfo b)
    processColumnInfo tableEnumReferences tableName (rawInfo, name) = do
      resolvedType <- resolveColumnType
      pure ColumnInfo
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
            Nothing -> pure $ ColumnScalar (prciType rawInfo)
            -- one reference? is an enum
            Just (enumReference:|[]) -> pure $ ColumnEnumReference enumReference
            -- multiple referenced enums? the schema is strange, so let’s reject it
            Just enumReferences -> throw400 ConstraintViolation
              $ "column " <> prciName rawInfo <<> " in table " <> tableName
              <<> " references multiple enum tables ("
              <> commaSeparated (map (dquote . erTable) $ toList enumReferences) <> ")"

    assertNoDuplicateFieldNames columns =
      flip Map.traverseWithKey (Map.groupOn pgiName columns) \name columnsWithName ->
        case columnsWithName of
          one:two:more -> throw400 AlreadyExists $ "the definitions of columns "
            <> englishList "and" (dquote . pgiColumn <$> (one:|two:more))
            <> " are in conflict: they are mapped to the same field name, " <>> name
          _ -> pure ()
