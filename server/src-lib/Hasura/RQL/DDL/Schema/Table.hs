{-# LANGUAGE Arrows #-}

-- | Description: Create/delete SQL tables to/from Hasura metadata.
module Hasura.RQL.DDL.Schema.Table
  ( TrackTable (..),
    runTrackTableQ,
    TrackTableV2 (..),
    runTrackTableV2Q,
    UntrackTable (..),
    runUntrackTableQ,
    dropTableInMetadata,
    SetTableIsEnum (..),
    runSetExistingTableIsEnumQ,
    SetTableCustomFields (..),
    runSetTableCustomFieldsQV2,
    SetTableCustomization (..),
    runSetTableCustomization,
    buildTableCache,
    checkConflictingNode,
  )
where

import Control.Arrow.Extended
import Control.Lens.Extended hiding ((.=))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson.Ordered qualified as JO
import Data.Align (align)
import Data.HashMap.Strict.Extended qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as S
import Data.Text.Extended
import Data.These (These (..))
import Hasura.Backends.Postgres.SQL.Types (PGDescription (..), QualifiedTable)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Context
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Schema.Common (purgeDependencies, textToName)
import Hasura.Incremental qualified as Inc
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Enum (resolveEnumReferences)
import Hasura.RQL.IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.Utils
import Language.GraphQL.Draft.Syntax qualified as G

data TrackTable b = TrackTable
  { tSource :: !SourceName,
    tName :: !(TableName b),
    tIsEnum :: !Bool
  }

deriving instance (Backend b) => Show (TrackTable b)

deriving instance (Backend b) => Eq (TrackTable b)

instance (Backend b) => FromJSON (TrackTable b) where
  parseJSON v = withOptions v <|> withoutOptions
    where
      withOptions = withObject "TrackTable" \o ->
        TrackTable
          <$> o .:? "source" .!= defaultSource
          <*> o .: "table"
          <*> o .:? "is_enum" .!= False
      withoutOptions = TrackTable defaultSource <$> parseJSON v <*> pure False

data SetTableIsEnum = SetTableIsEnum
  { stieSource :: !SourceName,
    stieTable :: !QualifiedTable,
    stieIsEnum :: !Bool
  }
  deriving (Show, Eq)

instance FromJSON SetTableIsEnum where
  parseJSON = withObject "SetTableIsEnum" $ \o ->
    SetTableIsEnum
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "is_enum"

data UntrackTable b = UntrackTable
  { utSource :: !SourceName,
    utTable :: !(TableName b),
    utCascade :: !Bool
  }

deriving instance (Backend b) => Show (UntrackTable b)

deriving instance (Backend b) => Eq (UntrackTable b)

instance (Backend b) => FromJSON (UntrackTable b) where
  parseJSON = withObject "UntrackTable" $ \o ->
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
trackExistingTableOrViewP1 ::
  forall b m.
  (QErrM m, CacheRWM m, Backend b, MetadataM m) =>
  SourceName ->
  TableName b ->
  m ()
trackExistingTableOrViewP1 source tableName = do
  sourceInfo <- askSourceInfo source
  when (isTableTracked @b sourceInfo tableName) $
    throw400 AlreadyTracked $ "view/table already tracked : " <>> tableName
  let functionName = tableToFunction @b tableName
  when (isJust $ Map.lookup functionName $ _siFunctions @b sourceInfo) $
    throw400 NotSupported $ "function with name " <> tableName <<> " already exists"

-- | Check whether a given name would conflict with the current schema by doing
-- an internal introspection
checkConflictingNode ::
  forall m.
  MonadError QErr m =>
  SchemaCache ->
  Text ->
  m ()
checkConflictingNode sc tnGQL = do
  let GQLContext queryParser _ _ = scUnauthenticatedGQLContext sc
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
        [ G.SelectionField $
            G.Field
              Nothing
              G.___schema
              mempty
              []
              [ G.SelectionField $
                  G.Field
                    Nothing
                    G._queryType
                    mempty
                    []
                    [ G.SelectionField $
                        G.Field
                          Nothing
                          G._fields
                          mempty
                          []
                          [ G.SelectionField $
                              G.Field
                                Nothing
                                G._name
                                mempty
                                []
                                []
                          ]
                    ]
              ]
        ]
  case queryParser introspectionQuery of
    Left _ -> pure ()
    Right results -> do
      case OMap.lookup (mkUnNamespacedRootFieldAlias G.___schema) results of
        Just (RFRaw (JO.Object schema)) -> do
          let names = do
                JO.Object queryType <- JO.lookup "queryType" schema
                JO.Array fields <- JO.lookup "fields" queryType
                for fields \case
                  JO.Object field -> do
                    JO.String name <- JO.lookup "name" field
                    pure name
                  _ -> Nothing
          case names of
            Nothing -> pure ()
            Just ns ->
              if tnGQL `elem` ns
                then
                  throw400 RemoteSchemaConflicts $
                    "node " <> tnGQL
                      <> " already exists in current graphql schema"
                else pure ()
        _ -> pure ()

trackExistingTableOrViewP2 ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  SourceName ->
  TableName b ->
  Bool ->
  TableConfig b ->
  m EncJSON
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
  checkConflictingNode sc $ snakeCaseTableName @b tableName
  let metadata = mkTableMeta tableName isEnum config
  buildSchemaCacheFor
    ( MOSourceObjId source $
        AB.mkAnyBackend $
          SMOTable @b tableName
    )
    $ MetadataModifier $
      metaSources . ix source . toSourceMetadata . smTables %~ OMap.insert tableName metadata
  pure successMsg

runTrackTableQ ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  TrackTable b ->
  m EncJSON
runTrackTableQ (TrackTable source qt isEnum) = do
  trackExistingTableOrViewP1 @b source qt
  trackExistingTableOrViewP2 @b source qt isEnum emptyTableConfig

data TrackTableV2 b = TrackTableV2
  { ttv2Table :: !(TrackTable b),
    ttv2Configuration :: !(TableConfig b)
  }
  deriving (Show, Eq)

instance (Backend b) => FromJSON (TrackTableV2 b) where
  parseJSON = withObject "TrackTableV2" $ \o -> do
    table <- parseJSON $ Object o
    configuration <- o .:? "configuration" .!= emptyTableConfig
    pure $ TrackTableV2 table configuration

runTrackTableV2Q ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  TrackTableV2 b ->
  m EncJSON
runTrackTableV2Q (TrackTableV2 (TrackTable source qt isEnum) config) = do
  trackExistingTableOrViewP1 @b source qt
  trackExistingTableOrViewP2 @b source qt isEnum config

runSetExistingTableIsEnumQ :: (MonadError QErr m, CacheRWM m, MetadataM m) => SetTableIsEnum -> m EncJSON
runSetExistingTableIsEnumQ (SetTableIsEnum source tableName isEnum) = do
  void $ askTableInfo @('Postgres 'Vanilla) source tableName -- assert that table is tracked
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable @('Postgres 'Vanilla) tableName)
    $ MetadataModifier $
      tableMetadataSetter @('Postgres 'Vanilla) source tableName . tmIsEnum .~ isEnum
  return successMsg

data SetTableCustomization b = SetTableCustomization
  { _stcSource :: !SourceName,
    _stcTable :: !(TableName b),
    _stcConfiguration :: !(TableConfig b)
  }
  deriving (Show, Eq)

instance (Backend b) => FromJSON (SetTableCustomization b) where
  parseJSON = withObject "SetTableCustomization" $ \o ->
    SetTableCustomization
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "configuration"

data SetTableCustomFields = SetTableCustomFields
  { _stcfSource :: !SourceName,
    _stcfTable :: !QualifiedTable,
    _stcfCustomRootFields :: !TableCustomRootFields,
    _stcfCustomColumnNames :: !(HashMap (Column ('Postgres 'Vanilla)) G.Name)
  }
  deriving (Show, Eq)

instance FromJSON SetTableCustomFields where
  parseJSON = withObject "SetTableCustomFields" $ \o ->
    SetTableCustomFields
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .:? "custom_root_fields" .!= emptyCustomRootFields
      <*> o .:? "custom_column_names" .!= Map.empty

runSetTableCustomFieldsQV2 ::
  (QErrM m, CacheRWM m, MetadataM m) => SetTableCustomFields -> m EncJSON
runSetTableCustomFieldsQV2 (SetTableCustomFields source tableName rootFields columnNames) = do
  void $ askTableInfo @('Postgres 'Vanilla) source tableName -- assert that table is tracked
  let columnConfig = (\name -> mempty {_ccfgCustomName = Just name}) <$> columnNames
  let tableConfig = TableConfig @('Postgres 'Vanilla) rootFields columnConfig Nothing Automatic
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable @('Postgres 'Vanilla) tableName)
    $ MetadataModifier $
      tableMetadataSetter source tableName . tmConfiguration .~ tableConfig
  return successMsg

runSetTableCustomization ::
  forall b m.
  (QErrM m, CacheRWM m, MetadataM m, Backend b, BackendMetadata b) =>
  SetTableCustomization b ->
  m EncJSON
runSetTableCustomization (SetTableCustomization source table config) = do
  void $ askTableInfo @b source table
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable @b table)
    $ MetadataModifier $
      tableMetadataSetter source table . tmConfiguration .~ config
  return successMsg

unTrackExistingTableOrViewP1 ::
  forall b m.
  (CacheRM m, QErrM m, Backend b) =>
  UntrackTable b ->
  m ()
unTrackExistingTableOrViewP1 (UntrackTable source vn _) = do
  schemaCache <- askSchemaCache
  tableInfo <-
    unsafeTableInfo @b source vn (scSources schemaCache)
      `onNothing` throw400 AlreadyUntracked ("view/table already untracked : " <>> vn)
  when (isSystemDefined $ _tciSystemDefined $ _tiCoreInfo tableInfo) $
    throw400 NotSupported $ vn <<> " is system defined, cannot untrack"

unTrackExistingTableOrViewP2 ::
  forall b m.
  (CacheRWM m, QErrM m, MetadataM m, BackendMetadata b) =>
  UntrackTable b ->
  m EncJSON
unTrackExistingTableOrViewP2 (UntrackTable source qtn cascade) = withNewInconsistentObjsCheck do
  sc <- askSchemaCache

  -- Get relational, query template and function dependants
  let allDeps =
        getDependentObjs
          sc
          (SOSourceObj source $ AB.mkAnyBackend $ SOITable @b qtn)
      indirectDeps = mapMaybe getIndirectDep allDeps

  -- Report bach with an error if cascade is not set
  when (indirectDeps /= [] && not cascade) $
    reportDependentObjectsExist indirectDeps
  -- Purge all the dependents from state
  metadataModifier <- execWriterT do
    purgeDependencies indirectDeps
    tell $ dropTableInMetadata @b source qtn
  -- delete the table and its direct dependencies
  withNewInconsistentObjsCheck $ buildSchemaCache metadataModifier
  pure successMsg
  where
    getIndirectDep :: SchemaObjId -> Maybe SchemaObjId
    getIndirectDep = \case
      sourceObj@(SOSourceObj s exists) ->
        -- If the dependency is to any other source, it automatically is an
        -- indirect dependency, hence the cast is safe here. However, we don't
        -- have these cross source dependencies yet
        AB.unpackAnyBackend @b exists >>= \case
          (SOITableObj dtn _) ->
            if not (s == source && qtn == dtn) then Just sourceObj else Nothing
          _ -> Just sourceObj
      -- A remote schema can have a remote relationship with a table. So when a
      -- table is dropped, the remote relationship in remote schema also needs to
      -- be removed.
      sourceObj@(SORemoteSchemaRemoteRelationship {}) -> Just sourceObj
      _ -> Nothing

runUntrackTableQ ::
  forall b m.
  (CacheRWM m, QErrM m, MetadataM m, BackendMetadata b) =>
  UntrackTable b ->
  m EncJSON
runUntrackTableQ q = do
  unTrackExistingTableOrViewP1 @b q
  unTrackExistingTableOrViewP2 @b q

-- | Builds an initial @'TableCache' 'ColumnInfo'@ from catalog information. Does not fill in
-- '_tiRolePermInfoMap' or '_tiEventTriggerInfoMap' at all, and '_tiFieldInfoMap' only contains
-- columns, not relationships; those pieces of information are filled in by later stages.
buildTableCache ::
  forall arr m b.
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectedInfo) arr,
    Inc.ArrowCache m arr,
    MonadIO m,
    MonadBaseControl IO m,
    BackendMetadata b
  ) =>
  ( SourceName,
    SourceConfig b,
    DBTablesMetadata b,
    [TableBuildInput b],
    Inc.Dependency Inc.InvalidationKey
  )
    `arr` Map.HashMap (TableName b) (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b))
buildTableCache = Inc.cache proc (source, sourceConfig, dbTablesMeta, tableBuildInputs, reloadMetadataInvalidationKey) -> do
  rawTableInfos <-
    (|
      Inc.keyed
        (|
          withTable
            ( \tables -> do
                table <- noDuplicateTables -< tables
                let maybeInfo = Map.lookup (_tbiName table) dbTablesMeta
                buildRawTableInfo -< (table, maybeInfo, sourceConfig, reloadMetadataInvalidationKey)
            )
        |)
      |) (withSourceInKey source $ Map.groupOnNE _tbiName tableBuildInputs)
  let rawTableCache = removeSourceInKey $ Map.catMaybes rawTableInfos
      enumTables = flip Map.mapMaybe rawTableCache \rawTableInfo ->
        (,,) <$> _tciPrimaryKey rawTableInfo <*> pure (_tciCustomConfig rawTableInfo) <*> _tciEnumValues rawTableInfo
  tableInfos <-
    (|
      Inc.keyed
        (| withTable (\table -> processTableInfo -< (enumTables, table)) |)
      |) (withSourceInKey source rawTableCache)
  returnA -< removeSourceInKey (Map.catMaybes tableInfos)
  where
    withSourceInKey :: (Eq k, Hashable k) => SourceName -> HashMap k v -> HashMap (SourceName, k) v
    withSourceInKey source = mapKeys (source,)

    removeSourceInKey :: (Eq k, Hashable k) => HashMap (SourceName, k) v -> HashMap k v
    removeSourceInKey = mapKeys snd

    withTable :: ErrorA QErr arr (e, s) a -> arr (e, ((SourceName, TableName b), s)) (Maybe a)
    withTable f =
      withRecordInconsistency f
        <<< second
          ( first $ arr \(source, name) ->
              MetadataObject
                ( MOSourceObjId source $
                    AB.mkAnyBackend $
                      SMOTable @b name
                )
                (toJSON name)
          )

    noDuplicateTables = proc tables -> case tables of
      table :| [] -> returnA -< table
      _ -> throwA -< err400 AlreadyExists "duplication definition for table"

    -- Step 1: Build the raw table cache from metadata information.
    buildRawTableInfo ::
      ErrorA
        QErr
        arr
        ( TableBuildInput b,
          Maybe (DBTableMetadata b),
          SourceConfig b,
          Inc.Dependency Inc.InvalidationKey
        )
        (TableCoreInfoG b (RawColumnInfo b) (Column b))
    buildRawTableInfo = Inc.cache proc (tableBuildInput, maybeInfo, sourceConfig, reloadMetadataInvalidationKey) -> do
      let TableBuildInput name isEnum config = tableBuildInput
      metadataTable <-
        (|
          onNothingA
            ( throwA
                -<
                  err400 NotExists $ "no such table/view exists in source: " <>> name
            )
          |) maybeInfo

      let columns :: [RawColumnInfo b] = _ptmiColumns metadataTable
          columnMap = mapFromL (FieldName . toTxt . rciName) columns
          primaryKey = _ptmiPrimaryKey metadataTable
          description = buildDescription name config metadataTable
      rawPrimaryKey <- liftEitherA -< traverse (resolvePrimaryKeyColumns columnMap) primaryKey
      enumValues <-
        if isEnum
          then do
            -- We want to make sure we reload enum values whenever someone explicitly calls
            -- `reload_metadata`.
            Inc.dependOn -< reloadMetadataInvalidationKey
            eitherEnums <- bindA -< fetchAndValidateEnumValues sourceConfig name rawPrimaryKey columns
            liftEitherA -< Just <$> eitherEnums
          else returnA -< Nothing

      returnA
        -<
          TableCoreInfo
            { _tciName = name,
              _tciSystemDefined = SystemDefined False,
              _tciFieldInfoMap = columnMap,
              _tciPrimaryKey = primaryKey,
              _tciUniqueConstraints = _ptmiUniqueConstraints metadataTable,
              _tciForeignKeys = S.map unForeignKeyMetadata $ _ptmiForeignKeys metadataTable,
              _tciViewInfo = _ptmiViewInfo metadataTable,
              _tciEnumValues = enumValues,
              _tciCustomConfig = config,
              _tciDescription = description,
              _tciExtraTableMetadata = _ptmiExtraTableMetadata metadataTable
            }

    -- Step 2: Process the raw table cache to replace Postgres column types with logical column
    -- types.
    processTableInfo ::
      ErrorA
        QErr
        arr
        ( Map.HashMap (TableName b) (PrimaryKey b (Column b), TableConfig b, EnumValues),
          TableCoreInfoG b (RawColumnInfo b) (Column b)
        )
        (TableCoreInfoG b (ColumnInfo b) (ColumnInfo b))
    processTableInfo = proc (enumTables, rawInfo) ->
      liftEitherA
        -< do
          let columns = _tciFieldInfoMap rawInfo
              enumReferences = resolveEnumReferences enumTables (_tciForeignKeys rawInfo)
          columnInfoMap <-
            collectColumnConfiguration columns (_tciCustomConfig rawInfo)
              >>= traverse (processColumnInfo enumReferences (_tciName rawInfo))
          assertNoDuplicateFieldNames (Map.elems columnInfoMap)

          primaryKey <- traverse (resolvePrimaryKeyColumns columnInfoMap) (_tciPrimaryKey rawInfo)
          pure
            rawInfo
              { _tciFieldInfoMap = columnInfoMap,
                _tciPrimaryKey = primaryKey
              }

    resolvePrimaryKeyColumns ::
      forall n a. (QErrM n) => HashMap FieldName a -> PrimaryKey b (Column b) -> n (PrimaryKey b a)
    resolvePrimaryKeyColumns columnMap = traverseOf (pkColumns . traverse) \columnName ->
      Map.lookup (FieldName (toTxt columnName)) columnMap
        `onNothing` throw500 "column in primary key not in table!"

    collectColumnConfiguration ::
      (QErrM n) =>
      FieldInfoMap (RawColumnInfo b) ->
      TableConfig b ->
      n (FieldInfoMap (RawColumnInfo b, G.Name, Maybe G.Description))
    collectColumnConfiguration columns TableConfig {..} = do
      let configByFieldName = mapKeys (fromCol @b) _tcColumnConfig
      Map.traverseWithKey
        (\fieldName -> pairColumnInfoAndConfig fieldName >=> extractColumnConfiguration fieldName)
        (align columns configByFieldName)

    pairColumnInfoAndConfig ::
      QErrM n =>
      FieldName ->
      These (RawColumnInfo b) ColumnConfig ->
      n (RawColumnInfo b, ColumnConfig)
    pairColumnInfoAndConfig fieldName = \case
      This column -> pure (column, mempty)
      These column config -> pure (column, config)
      That _ ->
        throw400 NotExists $
          "configuration was given for the column " <> fieldName <<> ", but no such column exists"

    extractColumnConfiguration ::
      QErrM n =>
      FieldName ->
      (RawColumnInfo b, ColumnConfig) ->
      n (RawColumnInfo b, G.Name, Maybe G.Description)
    extractColumnConfiguration fieldName (columnInfo, ColumnConfig {..}) = do
      name <- _ccfgCustomName `onNothing` textToName (getFieldNameTxt fieldName)
      pure (columnInfo, name, description)
      where
        description :: Maybe G.Description
        description = case _ccfgComment of
          Automatic -> rciDescription columnInfo
          (Explicit explicitDesc) -> G.Description . toTxt <$> explicitDesc

    processColumnInfo ::
      (QErrM n) =>
      Map.HashMap (Column b) (NonEmpty (EnumReference b)) ->
      TableName b ->
      (RawColumnInfo b, G.Name, Maybe G.Description) ->
      n (ColumnInfo b)
    processColumnInfo tableEnumReferences tableName (rawInfo, name, description) = do
      resolvedType <- resolveColumnType
      pure
        ColumnInfo
          { ciColumn = pgCol,
            ciName = name,
            ciPosition = rciPosition rawInfo,
            ciType = resolvedType,
            ciIsNullable = rciIsNullable rawInfo,
            ciDescription = description,
            ciMutability = rciMutability rawInfo
          }
      where
        pgCol = rciName rawInfo
        resolveColumnType =
          case Map.lookup pgCol tableEnumReferences of
            -- no references? not an enum
            Nothing -> pure $ ColumnScalar (rciType rawInfo)
            -- one reference? is an enum
            Just (enumReference :| []) -> pure $ ColumnEnumReference enumReference
            -- multiple referenced enums? the schema is strange, so let’s reject it
            Just enumReferences ->
              throw400 ConstraintViolation $
                "column " <> rciName rawInfo <<> " in table " <> tableName
                  <<> " references multiple enum tables ("
                  <> commaSeparated (map (dquote . erTable) $ toList enumReferences)
                  <> ")"

    assertNoDuplicateFieldNames columns =
      void $ flip Map.traverseWithKey (Map.groupOn ciName columns) \name columnsWithName ->
        case columnsWithName of
          one : two : more ->
            throw400 AlreadyExists $
              "the definitions of columns "
                <> englishList "and" (dquote . ciColumn <$> (one :| two : more))
                <> " are in conflict: they are mapped to the same field name, " <>> name
          _ -> pure ()

    buildDescription :: TableName b -> TableConfig b -> DBTableMetadata b -> Maybe PGDescription
    buildDescription tableName tableConfig tableMetadata =
      case _tcComment tableConfig of
        Automatic -> _ptmiDescription tableMetadata <|> Just autogeneratedDescription
        Explicit description -> PGDescription . toTxt <$> description
      where
        autogeneratedDescription =
          PGDescription $ "columns and relationships of " <>> tableName
