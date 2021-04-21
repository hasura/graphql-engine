{-# LANGUAGE Arrows #-}

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
  , checkConflictingNode
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
import           Data.Aeson.TH
import           Data.Text.Extended

import qualified Hasura.Incremental                 as Inc
import qualified Hasura.SQL.AnyBackend              as AB

import           Hasura.Backends.Postgres.SQL.Types (QualifiedTable)
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Schema.Common       (textToName)
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Common
import           Hasura.RQL.DDL.Schema.Enum         (resolveEnumReferences)
import           Hasura.RQL.Types                   hiding (fmFunction)
import           Hasura.Server.Utils


data TrackTable b
  = TrackTable
  { tSource :: !SourceName
  , tName   :: !(TableName b)
  , tIsEnum :: !Bool
  }
deriving instance (Backend b) => Show (TrackTable b)
deriving instance (Backend b) => Eq (TrackTable b)

instance (Backend b) => FromJSON (TrackTable b) where
  parseJSON v = withOptions <|> withoutOptions
    where
      withOptions = flip (withObject "TrackTable") v $ \o -> TrackTable
        <$> o .:? "source" .!= defaultSource
        <*> o .: "table"
        <*> o .:? "is_enum" .!= False
      withoutOptions = TrackTable defaultSource <$> parseJSON v <*> pure False

instance (Backend b) => ToJSON (TrackTable b) where
  toJSON (TrackTable source name isEnum)
    | isEnum = object [ "source" .= source, "table" .= name, "is_enum" .= isEnum ]
    | otherwise = toJSON name

data SetTableIsEnum
  = SetTableIsEnum
  { stieSource :: !SourceName
  , stieTable  :: !QualifiedTable
  , stieIsEnum :: !Bool
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON ''SetTableIsEnum)

instance FromJSON SetTableIsEnum where
  parseJSON = withObject "Object" $ \o ->
    SetTableIsEnum
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "is_enum"

data UntrackTable b =
  UntrackTable
  { utSource  :: !SourceName
  , utTable   :: !(TableName b)
  , utCascade :: !Bool
  } deriving (Generic)
deriving instance (Backend b) => Show (UntrackTable b)
deriving instance (Backend b) => Eq (UntrackTable b)
instance (Backend b) => ToJSON (UntrackTable b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields=True}

instance (Backend b) => FromJSON (UntrackTable b) where
  parseJSON = withObject "Object" $ \o ->
    UntrackTable
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .:? "cascade" .!= False

isTableTracked :: forall b. (Backend b) => SourceInfo b -> TableName b -> Bool
isTableTracked sourceInfo tableName =
  isJust $ Map.lookup tableName $ _siTables sourceInfo

-- | Track table/view, Phase 1:
-- Validate table tracking operation. Fails if table is already being tracked,
-- or if a function with the same name is being tracked.
trackExistingTableOrViewP1 :: forall m b. (QErrM m, CacheRWM m, Backend b, MetadataM m) => SourceName -> TableName b -> m ()
trackExistingTableOrViewP1 source tableName = do
  sourceInfo <- askSourceInfo source
  when (isTableTracked sourceInfo tableName) $
    throw400 AlreadyTracked $ "view/table already tracked : " <>> tableName
  let functionName = tableToFunction tableName
  when (isJust $ Map.lookup functionName $ _siFunctions sourceInfo) $
    throw400 NotSupported $ "function with name " <> tableName <<> " already exists"

-- | Check whether a given name would conflict with the current schema by doing
-- an internal introspection
checkConflictingNode
  :: forall m. MonadError QErr m
  => SchemaCache
  -> Text
  -> m ()
checkConflictingNode sc tnGQL = do
  let GQLContext queryParser _ = scUnauthenticatedGQLContext sc
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
  :: forall m b
   . (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b)
  => SourceName -> TableName b -> Bool -> TableConfig b -> m EncJSON
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
  checkConflictingNode sc $ snakeCaseTableName tableName
  let metadata = mkTableMeta tableName isEnum config
  buildSchemaCacheFor
    (MOSourceObjId source
         $ AB.mkAnyBackend
         $ SMOTable tableName)
    $ MetadataModifier
    $ metaSources.ix source.toSourceMetadata.smTables %~ OMap.insert tableName metadata
  pure successMsg

runTrackTableQ
  :: (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) => TrackTable b -> m EncJSON
runTrackTableQ (TrackTable source qt isEnum) = do
  trackExistingTableOrViewP1 source qt
  trackExistingTableOrViewP2 source qt isEnum emptyTableConfig

data TrackTableV2 b
  = TrackTableV2
  { ttv2Table         :: !(TrackTable b)
  , ttv2Configuration :: !(TableConfig b)
  } deriving (Show, Eq, Generic)
instance (Backend b) => ToJSON (TrackTableV2 b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (TrackTableV2 b) where
  parseJSON = withObject "Object" $ \o -> do
    table <- parseJSON $ Object o
    configuration <- o .:? "configuration" .!= emptyTableConfig
    pure $ TrackTableV2 table configuration

runTrackTableV2Q
  :: (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) => TrackTableV2 b -> m EncJSON
runTrackTableV2Q (TrackTableV2 (TrackTable source qt isEnum) config) = do
  trackExistingTableOrViewP1 source qt
  trackExistingTableOrViewP2 source qt isEnum config

runSetExistingTableIsEnumQ :: (MonadError QErr m, CacheRWM m, MetadataM m) => SetTableIsEnum -> m EncJSON
runSetExistingTableIsEnumQ (SetTableIsEnum source tableName isEnum) = do
  void $ askTabInfo source tableName -- assert that table is tracked
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable tableName)
    $ MetadataModifier
    $ tableMetadataSetter source tableName.tmIsEnum .~ isEnum
  return successMsg

data SetTableCustomization
  = SetTableCustomization
  { _stcSource        :: !SourceName
  , _stcTable         :: !QualifiedTable
  , _stcConfiguration :: !(TableConfig 'Postgres)
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON ''SetTableCustomization)

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
  , _stcfCustomColumnNames :: !(CustomColumnNames 'Postgres)
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON ''SetTableCustomFields)

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
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable tableName)
    $ MetadataModifier
    $ tableMetadataSetter source tableName.tmConfiguration .~ tableConfig
  return successMsg

runSetTableCustomization
  :: (QErrM m, CacheRWM m, MetadataM m) => SetTableCustomization -> m EncJSON
runSetTableCustomization (SetTableCustomization source table config) = do
  void $ askTabInfo source table
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable table)
    $ MetadataModifier
    $ tableMetadataSetter source table.tmConfiguration .~ config
  return successMsg

unTrackExistingTableOrViewP1
  :: forall m b. (CacheRM m, QErrM m, Backend b) => UntrackTable b -> m ()
unTrackExistingTableOrViewP1 (UntrackTable source vn _) = do
  schemaCache <- askSchemaCache
  tableInfo   <- unsafeTableInfo @b source vn (scSources schemaCache)
    `onNothing` throw400 AlreadyUntracked ("view/table already untracked : " <>> vn)
  when (isSystemDefined $ _tciSystemDefined $ _tiCoreInfo tableInfo) $
    throw400 NotSupported $ vn <<> " is system defined, cannot untrack"

unTrackExistingTableOrViewP2
  :: forall m b
   . (CacheRWM m, QErrM m, MetadataM m, BackendMetadata b)
  => UntrackTable b -> m EncJSON
unTrackExistingTableOrViewP2 (UntrackTable source qtn cascade) = withNewInconsistentObjsCheck do
  sc <- askSchemaCache

  -- Get relational, query template and function dependants
  let allDeps =
        getDependentObjs
          sc
          (SOSourceObj source $ AB.mkAnyBackend $ SOITable qtn)
      indirectDeps = mapMaybe getIndirectDep allDeps
  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not cascade)
    $ reportDepsExt
        (map (SOSourceObj source . AB.mkAnyBackend) indirectDeps)
        []
  -- Purge all the dependents from state
  metadataModifier <- execWriterT do
    mapM_ (purgeDependentObject source >=> tell) indirectDeps
    tell $ dropTableInMetadata source qtn
  -- delete the table and its direct dependencies
  buildSchemaCache metadataModifier
  pure successMsg
  where
    getIndirectDep :: SchemaObjId -> Maybe (SourceObjId b)
    getIndirectDep = \case
      SOSourceObj s exists ->
        -- If the dependency is to any other source, it automatically is an
        -- indirect dependency, hence the cast is safe here. However, we don't
        -- have these cross source dependencies yet
        AB.unpackAnyBackend exists >>= \case
          v@(SOITableObj dtn _) ->
            if not (s == source && qtn == dtn) then Just v else Nothing
          v                 -> Just v
      _                     -> Nothing

dropTableInMetadata :: (BackendMetadata b) => SourceName -> TableName b -> MetadataModifier
dropTableInMetadata source table =
  MetadataModifier $ metaSources.ix source.toSourceMetadata.smTables %~ OMap.delete table

runUntrackTableQ
  :: (CacheRWM m, QErrM m, MetadataM m, BackendMetadata b)
  => UntrackTable b -> m EncJSON
runUntrackTableQ q = do
  unTrackExistingTableOrViewP1 q
  unTrackExistingTableOrViewP2 q

-- | Builds an initial @'TableCache' 'ColumnInfo'@ from catalog information. Does not fill in
-- '_tiRolePermInfoMap' or '_tiEventTriggerInfoMap' at all, and '_tiFieldInfoMap' only contains
-- columns, not relationships; those pieces of information are filled in by later stages.
buildTableCache
  :: forall arr m b
   . ( ArrowChoice arr, Inc.ArrowDistribute arr, ArrowWriter (Seq CollectedInfo) arr
     , Inc.ArrowCache m arr, MonadIO m, MonadBaseControl IO m, BackendMetadata b
     )
  => ( SourceName
     , SourceConfig b
     , DBTablesMetadata b
     , [TableBuildInput b]
     , Inc.Dependency Inc.InvalidationKey
     ) `arr` Map.HashMap (TableName b) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b))
buildTableCache = Inc.cache proc (source, sourceConfig, dbTablesMeta, tableBuildInputs, reloadMetadataInvalidationKey) -> do
  rawTableInfos <-
    (| Inc.keyed (| withTable (\tables -> do
         table <- noDuplicateTables -< tables
         let maybeInfo = Map.lookup (_tbiName table) dbTablesMeta
         buildRawTableInfo -< (table, maybeInfo, sourceConfig, reloadMetadataInvalidationKey)
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

    withTable :: ErrorA QErr arr (e, s) a -> arr (e, ((SourceName, TableName b), s)) (Maybe a)
    withTable f = withRecordInconsistency f <<<
      second (first $ arr \(source, name) ->
        MetadataObject
          (MOSourceObjId source
            $ AB.mkAnyBackend
            $ SMOTable name)
          (toJSON name))

    noDuplicateTables = proc tables -> case tables of
      table :| [] -> returnA -< table
      _           -> throwA -< err400 AlreadyExists "duplication definition for table"

    -- Step 1: Build the raw table cache from metadata information.
    buildRawTableInfo
      :: ErrorA QErr arr
       ( TableBuildInput b
       , Maybe (DBTableMetadata b)
       , SourceConfig b
       , Inc.Dependency Inc.InvalidationKey
       ) (TableCoreInfoG b (RawColumnInfo b) (Column b))
    buildRawTableInfo = Inc.cache proc (tableBuildInput, maybeInfo, sourceConfig, reloadMetadataInvalidationKey) -> do
      let TableBuildInput name isEnum config = tableBuildInput
      metadataTable <-
        (| onNothingA (throwA -<
             err400 NotExists $ "no such table/view exists in source: " <>> name)
        |) maybeInfo

      let columns :: [RawColumnInfo b] = _ptmiColumns metadataTable
          columnMap = mapFromL (FieldName . toTxt . prciName) columns
          primaryKey = _ptmiPrimaryKey metadataTable
      rawPrimaryKey <- liftEitherA -< traverse (resolvePrimaryKeyColumns columnMap) primaryKey
      enumValues <- if isEnum
        then do
          -- We want to make sure we reload enum values whenever someone explicitly calls
          -- `reload_metadata`.
          Inc.dependOn -< reloadMetadataInvalidationKey
          eitherEnums <- bindA -< fetchAndValidateEnumValues sourceConfig name rawPrimaryKey columns
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
      :: ErrorA QErr arr
       ( Map.HashMap (TableName b) (PrimaryKey b (Column b), EnumValues)
       , TableCoreInfoG b (RawColumnInfo b) (Column b)
       ) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b))
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
      :: forall n a . (QErrM n) => HashMap FieldName a -> PrimaryKey b (Column b) -> n (PrimaryKey b a)
    resolvePrimaryKeyColumns columnMap = traverseOf (pkColumns.traverse) \columnName ->
      Map.lookup (FieldName (toTxt columnName)) columnMap
        `onNothing` throw500 "column in primary key not in table!"

    alignCustomColumnNames
      :: (QErrM n)
      => FieldInfoMap (RawColumnInfo b)
      -> CustomColumnNames b
      -> n (FieldInfoMap (RawColumnInfo b, G.Name))
    alignCustomColumnNames columns customNames = do
      let customNamesByFieldName = mapKeys (fromCol @b) customNames
      flip Map.traverseWithKey (align columns customNamesByFieldName) \columnName -> \case
        This column -> (column,) <$> textToName (getFieldNameTxt columnName)
        These column customName -> pure (column, customName)
        That customName -> throw400 NotExists $ "the custom field name " <> customName
          <<> " was given for the column " <> columnName <<> ", but no such column exists"

    -- | “Processes” a '(RawColumnInfo 'Postgres)' into a 'PGColumnInfo' by resolving its type using a map of
    -- known enum tables.
    processColumnInfo
      :: (QErrM n)
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
