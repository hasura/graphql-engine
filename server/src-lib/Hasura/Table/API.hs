{-# LANGUAGE Arrows #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Description: Create/delete SQL tables to/from Hasura metadata.
module Hasura.Table.API
  ( TrackTable (..),
    runTrackTableQ,
    TrackTableV2 (..),
    runTrackTableV2Q,
    TrackTables (..),
    runTrackTablesQ,
    UntrackTable (..),
    runUntrackTableQ,
    UntrackTables (..),
    runUntrackTablesQ,
    dropTableInMetadata,
    SetTableIsEnum (..),
    runSetExistingTableIsEnumQ,
    SetTableCustomFields (..),
    runSetTableCustomFieldsQV2,
    SetTableCustomization (..),
    runSetTableCustomization,
    buildTableCache,
    checkConflictingNode,
    SetApolloFederationConfig (..),
    runSetApolloFederationConfig,
  )
where

import Control.Arrow.Extended
import Control.Arrow.Interpret
import Control.Lens hiding ((.=))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson.Ordered qualified as JO
import Data.Align (align)
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as S
import Data.Text.Casing (GQLNameIdentifier, fromCustomName)
import Data.Text.Extended
import Data.These (These (..))
import Data.Vector (Vector)
import Hasura.Backends.Postgres.SQL.Types (PGDescription (..), QualifiedTable)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Eventing.Backend (BackendEventTrigger, dropTriggerAndArchiveEvents)
import Hasura.GraphQL.Context
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.GraphQL.Schema.Common (textToGQLIdentifier)
import Hasura.Incremental qualified as Inc
import Hasura.LogicalModel.Metadata
import Hasura.LogicalModel.Types
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Enum (resolveEnumReferences)
import Hasura.RQL.DDL.Warnings
import Hasura.RQL.IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger (TriggerName)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization (applyFieldNameCaseIdentifier)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Utils
import Hasura.Table.Cache
import Hasura.Table.Metadata (mkTableMeta, tmApolloFederationConfig, tmConfiguration, tmIsEnum, tmLogicalModel)
import Language.GraphQL.Draft.Syntax qualified as G

data TrackTable b = TrackTable
  { tSource :: SourceName,
    tName :: TableName b,
    tIsEnum :: Bool,
    tApolloFedConfig :: Maybe ApolloFederationConfig,
    tLogicalModel :: Maybe LogicalModelName
  }

deriving instance (Backend b) => Show (TrackTable b)

deriving instance (Backend b) => Eq (TrackTable b)

instance (Backend b) => FromJSON (TrackTable b) where
  parseJSON v = withOptions v <|> withoutOptions
    where
      withOptions = withObject "TrackTable" \o ->
        TrackTable
          <$> o
          .:? "source"
          .!= defaultSource
          <*> o
          .: "table"
          <*> o
          .:? "is_enum"
          .!= False
          <*> o
          .:? "apollo_federation_config"
          <*> o
          .:? "logical_model"
      withoutOptions = TrackTable defaultSource <$> parseJSON v <*> pure False <*> pure Nothing <*> pure Nothing

data SetTableIsEnum b = SetTableIsEnum
  { stieSource :: SourceName,
    stieTable :: TableName b,
    stieIsEnum :: Bool
  }

deriving instance (Eq (TableName b)) => Eq (SetTableIsEnum b)

deriving instance (Show (TableName b)) => Show (SetTableIsEnum b)

instance (Backend b) => FromJSON (SetTableIsEnum b) where
  parseJSON = withObject "SetTableIsEnum" $ \o ->
    SetTableIsEnum
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> o
      .: "is_enum"

data UntrackTable b = UntrackTable
  { utSource :: SourceName,
    utTable :: TableName b,
    utCascade :: Bool
  }

deriving instance (Backend b) => Show (UntrackTable b)

deriving instance (Backend b) => Eq (UntrackTable b)

instance (Backend b) => FromJSON (UntrackTable b) where
  parseJSON = withObject "UntrackTable" $ \o ->
    UntrackTable
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> o
      .:? "cascade"
      .!= False

isTableTracked :: forall b. (Backend b) => SourceInfo b -> TableName b -> Bool
isTableTracked sourceInfo tableName =
  isJust $ HashMap.lookup tableName $ _siTables sourceInfo

-- | Track table/view, Phase 1:
-- Validate table tracking operation. Fails if table is already being tracked,
-- or if a function with the same name is being tracked.
trackExistingTableOrViewPhase1 ::
  forall b m.
  (QErrM m, CacheRWM m, Backend b, MetadataM m) =>
  SourceName ->
  TableName b ->
  m ()
trackExistingTableOrViewPhase1 source tableName = do
  sourceInfo <- askSourceInfo source
  when (isTableTracked @b sourceInfo tableName)
    $ throw400 AlreadyTracked
    $ "view/table already tracked: "
    <>> tableName
  let functionName = tableToFunction @b tableName
  when (isJust $ HashMap.lookup functionName $ _siFunctions @b sourceInfo)
    $ throw400 NotSupported
    $ "function with name "
    <> tableName
    <<> " already exists"

queryForExistingFieldNames :: SchemaCache -> Vector Text
queryForExistingFieldNames schemaCache = do
  let GQLContext queryParser _ _ = scUnauthenticatedGQLContext schemaCache
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
        [ G.SelectionField
            $ G.Field
              Nothing
              GName.___schema
              mempty
              []
              [ G.SelectionField
                  $ G.Field
                    Nothing
                    GName._queryType
                    mempty
                    []
                    [ G.SelectionField
                        $ G.Field
                          Nothing
                          GName._fields
                          mempty
                          []
                          [ G.SelectionField
                              $ G.Field
                                Nothing
                                GName._name
                                mempty
                                []
                                []
                          ]
                    ]
              ]
        ]
  case queryParser introspectionQuery of
    Left _ -> mempty
    Right results -> do
      case InsOrdHashMap.lookup (mkUnNamespacedRootFieldAlias GName.___schema) results of
        Just (RFRaw (JO.Object schema)) -> do
          let names = do
                JO.Object queryType <- JO.lookup "queryType" schema
                JO.Array fields <- JO.lookup "fields" queryType
                for fields \case
                  JO.Object field -> do
                    JO.String name <- JO.lookup "name" field
                    pure name
                  _ -> Nothing
          fromMaybe mempty $ names
        _ -> mempty

-- | Check whether a given name would conflict with the current schema by doing
-- an internal introspection
checkConflictingNode ::
  forall m.
  (MonadError QErr m) =>
  SchemaCache ->
  Text ->
  m ()
checkConflictingNode sc tnGQL = do
  let fieldNames = queryForExistingFieldNames sc
  when (tnGQL `elem` fieldNames)
    $ throw400 RemoteSchemaConflicts
    $ "node "
    <> tnGQL
    <> " already exists in current graphql schema"

findConflictingNodes ::
  SchemaCache ->
  (a -> Text) ->
  [a] ->
  [(a, QErr)]
findConflictingNodes sc extractName items = do
  let fieldNames = queryForExistingFieldNames sc
  flip foldMap items $ \item ->
    let name = extractName item
        err =
          err400 RemoteSchemaConflicts
            $ "node "
            <> name
            <> " already exists in current graphql schema"
     in [(item, err) | name `elem` fieldNames]

trackExistingTableOrViewPhase2 ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  TrackTableV2 b ->
  m EncJSON
trackExistingTableOrViewPhase2 trackTable@TrackTableV2 {ttv2Table = TrackTable {..}} = do
  sc <- askSchemaCache
  {-
  The next line does more than what it says on the tin.  Removing the following
  call to 'checkConflictingNode' causes memory usage to spike when newly
  tracking a large amount (~100) of tables.  The memory usage can be triggered
  by first creating a large amount of tables through SQL, without tracking the
  tables, and then clicking "track all" in the console.  Curiously, this high
  memory usage happens even when no substantial GraphQL schema is generated.
  -}
  checkConflictingNode sc $ snakeCaseTableName @b tName
  buildSchemaCacheFor
    ( MOSourceObjId tSource
        $ AB.mkAnyBackend
        $ SMOTable @b tName
    )
    $ mkTrackTableMetadataModifier trackTable
  pure successMsg

mkTrackTableMetadataModifier :: (Backend b) => TrackTableV2 b -> MetadataModifier
mkTrackTableMetadataModifier (TrackTableV2 (TrackTable source tableName isEnum apolloFedConfig logicalModel) config) =
  let metadata =
        mkTableMeta tableName isEnum config
          & tmApolloFederationConfig .~ apolloFedConfig
          & tmLogicalModel .~ logicalModel
   in MetadataModifier $ metaSources . ix source . toSourceMetadata . smTables %~ InsOrdHashMap.insert tableName metadata

runTrackTableQ ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  TrackTable b ->
  m EncJSON
runTrackTableQ trackTable@TrackTable {..} = do
  trackExistingTableOrViewPhase1 @b tSource tName
  trackExistingTableOrViewPhase2 @b (TrackTableV2 trackTable emptyTableConfig)

data TrackTableV2 b = TrackTableV2
  { ttv2Table :: TrackTable b,
    ttv2Configuration :: TableConfig b
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
runTrackTableV2Q trackTable@TrackTableV2 {ttv2Table = TrackTable {..}} = do
  trackExistingTableOrViewPhase1 @b tSource tName
  trackExistingTableOrViewPhase2 @b trackTable

data TrackTables b = TrackTables
  { _ttv2Tables :: [TrackTableV2 b],
    _ttv2AllowWarnings :: AllowWarnings
  }

instance (Backend b) => FromJSON (TrackTables b) where
  parseJSON = withObject "TrackTables" $ \o -> do
    TrackTables
      <$> o
      .: "tables"
      <*> o
      .:? "allow_warnings"
      .!= AllowWarnings

runTrackTablesQ ::
  forall b m.
  (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  TrackTables b ->
  m EncJSON
runTrackTablesQ TrackTables {..} = do
  unless (null duplicatedTables)
    $ let tables = commaSeparated $ (\(source, tableName) -> toTxt source <> "." <> toTxt tableName) <$> duplicatedTables
       in withPathK "tables" $ throw400 BadRequest ("The following tables occur more than once in the request: " <> tables)

  (successfulTables, metadataWarnings) <- runMetadataWarnings $ do
    phase1SuccessfulTables <- fmap mconcat . for _ttv2Tables $ \trackTable@TrackTableV2 {ttv2Table = TrackTable {..}} -> do
      (trackExistingTableOrViewPhase1 @b tSource tName $> [trackTable])
        `catchError` \qErr -> do
          let tableObjId = mkTrackTableV2ObjectId trackTable
          let message = qeError qErr
          warn $ MetadataWarning WCTrackTableFailed tableObjId message
          pure []

    trackExistingTablesOrViewsPhase2 phase1SuccessfulTables

  when (null successfulTables)
    $ throw400WithDetail InvalidConfiguration "all tables failed to track" (toJSON metadataWarnings)

  case _ttv2AllowWarnings of
    AllowWarnings -> pure ()
    DisallowWarnings ->
      unless (null metadataWarnings)
        $ throw400WithDetail (CustomCode "metadata-warnings") "failed due to metadata warnings" (toJSON metadataWarnings)

  pure $ mkSuccessResponseWithWarnings metadataWarnings
  where
    duplicatedTables :: [(SourceName, TableName b)]
    duplicatedTables =
      _ttv2Tables
        & fmap (\TrackTableV2 {ttv2Table = TrackTable {..}} -> (tSource, tName))
        & sort
        & group
        & mapMaybe
          ( \case
              duplicate : _ : _ -> Just duplicate
              _ -> Nothing
          )

mkTrackTableV2ObjectId :: forall b. (Backend b) => TrackTableV2 b -> MetadataObjId
mkTrackTableV2ObjectId TrackTableV2 {ttv2Table = TrackTable {..}} =
  MOSourceObjId tSource . AB.mkAnyBackend $ SMOTable @b tName

trackExistingTablesOrViewsPhase2 ::
  forall b m.
  (MonadError QErr m, MonadWarnings m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  [TrackTableV2 b] ->
  m [TrackTableV2 b]
trackExistingTablesOrViewsPhase2 tablesToTrack = do
  schemaCache <- askSchemaCache
  -- Find and create warnings for tables with conflicting table names
  let errorsFromConflictingTables = findConflictingNodes schemaCache (snakeCaseTableName @b . tName . ttv2Table) tablesToTrack
  for_ errorsFromConflictingTables $ \(trackTable, qErr) -> do
    let tableObjId = mkTrackTableV2ObjectId trackTable
    let message = qeError qErr
    warn $ MetadataWarning WCTrackTableFailed tableObjId message

  let conflictingTables = fst <$> errorsFromConflictingTables
  let nonConflictingTables = tablesToTrack & filter (`notElem` conflictingTables)

  -- Try tracking all non-conflicting tables
  let objectIds = HashMap.fromList $ (\t -> (mkTrackTableV2ObjectId t, t)) <$> nonConflictingTables
  successfulTables <- tryBuildSchemaCacheAndWarnOnFailingObjects (pure . mkTrackTableMetadataModifier) WCTrackTableFailed objectIds
  pure $ HashMap.elems successfulTables

data UntrackTables b = UntrackTables
  { _utTables :: [UntrackTable b],
    _utAllowWarnings :: AllowWarnings
  }

instance (Backend b) => FromJSON (UntrackTables b) where
  parseJSON = withObject "UntrackTables" $ \o -> do
    UntrackTables
      <$> o
      .: "tables"
      <*> o
      .:? "allow_warnings"
      .!= AllowWarnings

runUntrackTablesQ ::
  forall b m.
  (CacheRWM m, QErrM m, MetadataM m, BackendMetadata b, BackendEventTrigger b, MonadIO m) =>
  UntrackTables b ->
  m EncJSON
runUntrackTablesQ UntrackTables {..} = do
  unless (null duplicatedTables)
    $ let tables = commaSeparated $ (\(source, tableName) -> toTxt source <> "." <> toTxt tableName) <$> duplicatedTables
       in withPathK "tables" $ throw400 BadRequest ("The following tables occur more than once in the request: " <> tables)

  (successfulTables, metadataWarnings) <- runMetadataWarnings $ do
    phase1SuccessfulTables <- fmap mconcat . for _utTables $ \untrackTable -> do
      (untrackExistingTableOrViewPhase1 @b untrackTable $> [untrackTable])
        `catchError` \qErr -> do
          let tableObjId = mkUntrackTableObjectId untrackTable
          let message = qeError qErr
          warn $ MetadataWarning WCUntrackTableFailed tableObjId message
          pure []

    untrackExistingTablesOrViewsPhase2 phase1SuccessfulTables

  when (null successfulTables)
    $ throw400WithDetail InvalidConfiguration "all tables failed to untrack" (toJSON metadataWarnings)

  case _utAllowWarnings of
    AllowWarnings -> pure ()
    DisallowWarnings ->
      unless (null metadataWarnings)
        $ throw400WithDetail (CustomCode "metadata-warnings") "failed due to metadata warnings" (toJSON metadataWarnings)

  pure $ mkSuccessResponseWithWarnings metadataWarnings
  where
    duplicatedTables :: [(SourceName, TableName b)]
    duplicatedTables =
      _utTables
        & fmap (\UntrackTable {..} -> (utSource, utTable))
        & sort
        & group
        & mapMaybe
          ( \case
              duplicate : _ : _ -> Just duplicate
              _ -> Nothing
          )

mkUntrackTableObjectId :: forall b. (Backend b) => UntrackTable b -> MetadataObjId
mkUntrackTableObjectId UntrackTable {..} =
  MOSourceObjId utSource . AB.mkAnyBackend $ SMOTable @b utTable

untrackExistingTablesOrViewsPhase2 ::
  forall b m.
  (CacheRWM m, MonadWarnings m, MonadError QErr m, MetadataM m, BackendMetadata b, BackendEventTrigger b, MonadIO m) =>
  [UntrackTable b] ->
  m [UntrackTable b]
untrackExistingTablesOrViewsPhase2 untrackTables = do
  untrackableTables <- fmap (HashMap.fromList . catMaybes) . forM (HashMap.toList tablesToUntrack) $ \(tableObjId, untrackTable@UntrackTable {..}) -> do
    (indirectDeps, triggers) <- getTableUntrackingInfo untrackTable
    let indirectDepsNotAlreadyBeingUntracked = filter (not . isDepAlreadyGettingUntracked) indirectDeps
    -- If there are indirect dependencies to the table to untrack and we're not
    -- cascading the untrack, fail to untrack this table with a warning.
    -- But if the indirect dependencies are from tables we're already going to untrack
    -- then allow them, since they'll get untracked anyway.
    if null indirectDepsNotAlreadyBeingUntracked || utCascade
      then pure $ Just (tableObjId, (untrackTable, indirectDeps, triggers))
      else do
        let errorReasons = "cannot drop due to the following dependent objects: " <> reportSchemaObjs indirectDeps
        warn $ MetadataWarning WCUntrackTableFailed tableObjId errorReasons
        pure Nothing

  -- Untrack the tables and all their indirect dependencies
  let mkMetadataModifier (untrackTable, indirectDeps, _triggers) = mkUntrackTableMetadataModifier untrackTable indirectDeps
  successfullyUntrackedTables <- tryBuildSchemaCacheAndWarnOnFailingObjects mkMetadataModifier WCUntrackTableFailed untrackableTables

  -- drop all the hasura SQL triggers present on the untracked tables
  for_ successfullyUntrackedTables $ \(UntrackTable {..}, _indirectDeps, triggers) -> do
    sourceConfig <- askSourceConfig @b utSource
    for_ triggers $ \triggerName -> dropTriggerAndArchiveEvents @b sourceConfig triggerName utTable

  pure $ (\(untrackTable, _indirectDeps, _triggers) -> untrackTable) <$> HashMap.elems successfullyUntrackedTables
  where
    tablesToUntrack :: HashMap MetadataObjId (UntrackTable b)
    tablesToUntrack = HashMap.fromList $ (\tbl -> (mkUntrackTableObjectId tbl, tbl)) <$> untrackTables

    isDepAlreadyGettingUntracked :: SchemaObjId -> Bool
    isDepAlreadyGettingUntracked schemaObjDependency =
      case tryGetTableMetadataObjId schemaObjDependency of
        Just tableObjId -> HashMap.member tableObjId tablesToUntrack
        Nothing -> False

    tryGetTableMetadataObjId :: SchemaObjId -> Maybe MetadataObjId
    tryGetTableMetadataObjId = \case
      SOSourceObj sourceName sourceObj ->
        let tableObjMaybe = AB.traverseBackend @Backend sourceObj $ \case
              SOITable tableName -> Just $ SMOTable tableName
              SOITableObj tableName _tableObj -> Just $ SMOTable tableName
              _ -> Nothing
         in (MOSourceObjId sourceName) <$> tableObjMaybe
      _ -> Nothing

runSetExistingTableIsEnumQ :: forall b m. (MonadError QErr m, CacheRWM m, MetadataM m, BackendMetadata b) => SetTableIsEnum b -> m EncJSON
runSetExistingTableIsEnumQ (SetTableIsEnum source tableName isEnum) = do
  void $ askTableInfo @b source tableName -- assert that table is tracked
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable @b tableName)
    $ MetadataModifier
    $ tableMetadataSetter @b source tableName
    . tmIsEnum
    .~ isEnum
  return successMsg

data SetTableCustomization b = SetTableCustomization
  { _stcSource :: SourceName,
    _stcTable :: TableName b,
    _stcConfiguration :: TableConfig b
  }
  deriving (Show, Eq)

instance (Backend b) => FromJSON (SetTableCustomization b) where
  parseJSON = withObject "SetTableCustomization" $ \o ->
    SetTableCustomization
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> o
      .: "configuration"

data SetTableCustomFields = SetTableCustomFields
  { _stcfSource :: SourceName,
    _stcfTable :: QualifiedTable,
    _stcfCustomRootFields :: TableCustomRootFields,
    _stcfCustomColumnNames :: HashMap (Column ('Postgres 'Vanilla)) G.Name
  }
  deriving (Show, Eq)

instance FromJSON SetTableCustomFields where
  parseJSON = withObject "SetTableCustomFields" $ \o ->
    SetTableCustomFields
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> o
      .:? "custom_root_fields"
      .!= emptyCustomRootFields
      <*> o
      .:? "custom_column_names"
      .!= HashMap.empty

runSetTableCustomFieldsQV2 ::
  (QErrM m, CacheRWM m, MetadataM m) => SetTableCustomFields -> m EncJSON
runSetTableCustomFieldsQV2 (SetTableCustomFields source tableName rootFields columnNames) = do
  void $ askTableInfo @('Postgres 'Vanilla) source tableName -- assert that table is tracked
  let columnConfig = (\name -> mempty {_ccfgCustomName = Just name}) <$> columnNames
  let tableConfig = TableConfig @('Postgres 'Vanilla) rootFields columnConfig Nothing Automatic
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable @('Postgres 'Vanilla) tableName)
    $ MetadataModifier
    $ tableMetadataSetter source tableName
    . tmConfiguration
    .~ tableConfig
  return successMsg

runSetTableCustomization ::
  forall b m.
  (QErrM m, CacheRWM m, MetadataM m, Backend b) =>
  SetTableCustomization b ->
  m EncJSON
runSetTableCustomization (SetTableCustomization source table config) = do
  void $ askTableInfo @b source table
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable @b table)
    $ MetadataModifier
    $ tableMetadataSetter source table
    . tmConfiguration
    .~ config
  return successMsg

untrackExistingTableOrViewPhase1 ::
  forall b m.
  (CacheRM m, QErrM m, Backend b) =>
  UntrackTable b ->
  m ()
untrackExistingTableOrViewPhase1 (UntrackTable source vn _) = do
  schemaCache <- askSchemaCache
  void
    $ unsafeTableInfo @b source vn (scSources schemaCache)
    `onNothing` throw400 AlreadyUntracked ("view/table already untracked: " <>> vn)

untrackExistingTableOrViewPhase2 ::
  forall b m.
  (CacheRWM m, QErrM m, MetadataM m, BackendMetadata b, BackendEventTrigger b, MonadIO m) =>
  UntrackTable b ->
  m EncJSON
untrackExistingTableOrViewPhase2 untrackTable@(UntrackTable source tableName cascade) = do
  (indirectDeps, triggers) <- getTableUntrackingInfo untrackTable
  sourceConfig <- askSourceConfig @b source

  -- Report batch with an error if cascade is not set
  unless (null indirectDeps || cascade)
    $ reportDependentObjectsExist indirectDeps
  -- Purge all the dependents from state
  metadataModifier <- mkUntrackTableMetadataModifier untrackTable indirectDeps
  -- delete the table and its direct dependencies
  withNewInconsistentObjsCheck $ buildSchemaCache metadataModifier
  -- drop all the hasura SQL triggers present on the table
  for_ triggers $ \triggerName -> do
    dropTriggerAndArchiveEvents @b sourceConfig triggerName tableName
  pure successMsg

getTableUntrackingInfo ::
  forall b m.
  (Backend b, CacheRM m, MonadError QErr m) =>
  UntrackTable b ->
  m ([SchemaObjId], [TriggerName])
getTableUntrackingInfo UntrackTable {..} = do
  sc <- askSchemaCache
  sourceInfo <- askTableInfo @b utSource utTable
  let triggers = HashMap.keys $ _tiEventTriggerInfoMap sourceInfo
  -- Get relational, query template and function dependants
  let allDeps =
        getDependentObjs
          sc
          (SOSourceObj utSource $ AB.mkAnyBackend $ SOITable @b utTable)
  let indirectDeps = mapMaybe getIndirectDep allDeps
  pure (indirectDeps, triggers)
  where
    getIndirectDep :: SchemaObjId -> Maybe SchemaObjId
    getIndirectDep = \case
      sourceObj@(SOSourceObj s exists) ->
        -- If the dependency is to any other source, it automatically is an
        -- indirect dependency, hence the cast is safe here. However, we don't
        -- have these cross source dependencies yet
        AB.unpackAnyBackend @b exists >>= \case
          (SOITableObj dependentTableName _) ->
            if not (s == utSource && utTable == dependentTableName) then Just sourceObj else Nothing
          _ -> Just sourceObj
      -- A remote schema can have a remote relationship with a table. So when a
      -- table is dropped, the remote relationship in remote schema also needs to
      -- be removed.
      sourceObj@(SORemoteSchemaRemoteRelationship {}) -> Just sourceObj
      _ -> Nothing

-- | Creates a metadata modifier that untracks a table and the specified indirect dependencies
mkUntrackTableMetadataModifier ::
  forall b m.
  (Backend b, MonadError QErr m) =>
  UntrackTable b ->
  [SchemaObjId] ->
  m MetadataModifier
mkUntrackTableMetadataModifier UntrackTable {..} indirectDeps = execWriterT $ do
  traverse_ purgeSourceAndSchemaDependencies indirectDeps
  tell $ dropTableInMetadata @b utSource utTable

runUntrackTableQ ::
  forall b m.
  (CacheRWM m, QErrM m, MetadataM m, BackendMetadata b, BackendEventTrigger b, MonadIO m) =>
  UntrackTable b ->
  m EncJSON
runUntrackTableQ q = do
  untrackExistingTableOrViewPhase1 @b q
  untrackExistingTableOrViewPhase2 @b q

-- | Builds an initial table cache. Does not fill in permissions or event triggers, and the returned
-- @FieldInfoMap@s only contain columns, not relationships; those pieces of information are filled
-- in later.
buildTableCache ::
  forall arr m b.
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectItem) arr,
    Inc.ArrowCache m arr,
    MonadIO m,
    MonadBaseControl IO m,
    BackendMetadata b
  ) =>
  ( SourceName,
    SourceConfig b,
    DBTablesMetadata b,
    [TableBuildInput b],
    Inc.Dependency Inc.InvalidationKey,
    NamingCase,
    LogicalModels b
  )
    `arr` HashMap.HashMap (TableName b) (TableCoreInfoG b (StructuredColumnInfo b) (ColumnInfo b))
buildTableCache = Inc.cache proc (source, sourceConfig, dbTablesMeta, tableBuildInputs, reloadMetadataInvalidationKey, tCase, logicalModels) -> do
  rawTableInfos <-
    (|
      Inc.keyed
        ( \tableName tables ->
            (|
              withRecordInconsistency
                ( do
                    table <- noDuplicateTables -< tables
                    case HashMap.lookup (_tbiName table) dbTablesMeta of
                      Nothing ->
                        throwA
                          -<
                            err400 NotExists $ "no such table/view exists in source: " <>> _tbiName table
                      Just metadataTable ->
                        buildRawTableInfo -< (source, table, metadataTable, sourceConfig, reloadMetadataInvalidationKey, logicalModels)
                )
            |)
              (mkTableMetadataObject source tableName)
        )
      |)
      (HashMap.groupOnNE _tbiName tableBuildInputs)
  let rawTableCache = catMaybes rawTableInfos
      enumTables = flip mapMaybe rawTableCache \rawTableInfo ->
        (,,) <$> _tciPrimaryKey rawTableInfo <*> pure (_tciCustomConfig rawTableInfo) <*> _tciEnumValues rawTableInfo
  tableInfos <-
    interpretWriter
      -< for rawTableCache \table -> withRecordInconsistencyM (mkTableMetadataObject source (_tciName table)) do
        processTableInfo enumTables table tCase
  returnA -< catMaybes tableInfos
  where
    mkTableMetadataObject source name =
      MetadataObject
        ( MOSourceObjId source
            $ AB.mkAnyBackend
            $ SMOTable @b name
        )
        (toJSON name)

    noDuplicateTables = proc tables -> case tables of
      table :| [] -> returnA -< table
      _ -> throwA -< err400 AlreadyExists "duplication definition for table"

    -- Step 1: Build the raw table cache from metadata information.
    buildRawTableInfo ::
      ErrorA
        QErr
        arr
        ( SourceName,
          TableBuildInput b,
          DBTableMetadata b,
          SourceConfig b,
          Inc.Dependency Inc.InvalidationKey,
          LogicalModels b
        )
        (TableCoreInfoG b (RawColumnInfo b) (Column b))
    buildRawTableInfo = Inc.cache proc (sourceName, tableBuildInput, metadataTable, sourceConfig, reloadMetadataInvalidationKey, logicalModels) -> do
      let TableBuildInput name isEnum config apolloFedConfig mLogicalModelName = tableBuildInput
      columns <-
        liftEitherA
          -< case mLogicalModelName of
            Nothing ->
              -- No logical model specified: use columns from DB introspection
              pure $ _ptmiColumns metadataTable
            Just logicalModelName -> do
              -- A logical model was specified: use columns from the logical model
              --
              -- If the source does not support schemaless tables then we throw an error.
              -- This is not strictly necessary - the logical model could be used even with sources
              -- that always provide a table schema.  For now, we want to limit this functionality to
              -- databases such as MongoDB that don't always provide a schema. In future we may want
              -- to relax this.
              unless (sourceSupportsSchemalessTables @b sourceConfig)
                $ throw400 InvalidConfiguration ("The source " <> sourceName <<> " does not support schemaless tables")
              logicalModel <-
                InsOrdHashMap.lookup logicalModelName logicalModels
                  `onNothing` throw400 InvalidConfiguration ("The logical mode " <> logicalModelName <<> " could not be found")
              logicalModelToRawColumnInfos logicalModel
      let columnMap = mapFromL (FieldName . toTxt . rciName) columns
          primaryKey = _ptmiPrimaryKey metadataTable
          description = buildDescription name config metadataTable
      rawPrimaryKey <- liftEitherA -< traverse (resolvePrimaryKeyColumns columnMap) primaryKey
      enumValues <- do
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
              _tciFieldInfoMap = columnMap,
              _tciPrimaryKey = primaryKey,
              _tciUniqueConstraints = _ptmiUniqueConstraints metadataTable,
              _tciForeignKeys = S.map unForeignKeyMetadata $ _ptmiForeignKeys metadataTable,
              _tciViewInfo = _ptmiViewInfo metadataTable,
              _tciEnumValues = enumValues,
              _tciCustomConfig = config,
              _tciDescription = description,
              _tciExtraTableMetadata = _ptmiExtraTableMetadata metadataTable,
              _tciApolloFederationConfig = apolloFedConfig,
              _tciRawColumns = columns
            }

    logicalModelToRawColumnInfos :: LogicalModelMetadata b -> Either QErr [RawColumnInfo b]
    logicalModelToRawColumnInfos = traverse (uncurry logicalModelColumnFieldToRawColumnInfo) . zip [1 ..] . InsOrdHashMap.elems . _lmmFields

    logicalModelColumnFieldToRawColumnInfo :: Int -> LogicalModelField b -> Either QErr (RawColumnInfo b)
    logicalModelColumnFieldToRawColumnInfo position LogicalModelField {..} = do
      (rciType, rciIsNullable) <- logicalModelTypeToRawColumnType lmfType
      pure
        $ RawColumnInfo
          { rciName = lmfName,
            rciPosition = position,
            rciDescription = G.Description <$> lmfDescription,
            rciMutability = ColumnMutability False False, -- TODO
            ..
          }

    logicalModelTypeToRawColumnType :: LogicalModelType b -> Either QErr (RawColumnType b, Bool)
    logicalModelTypeToRawColumnType = \case
      LogicalModelTypeScalar LogicalModelTypeScalarC {..} ->
        pure (RawColumnTypeScalar lmtsScalar, lmtsNullable)
      LogicalModelTypeArray LogicalModelTypeArrayC {..} -> do
        supportsNestedObjects <- backendSupportsNestedObjects @b
        (nestedType, nestedIsNullable) <- logicalModelTypeToRawColumnType lmtaArray
        pure (RawColumnTypeArray supportsNestedObjects nestedType nestedIsNullable, lmtaNullable)
      LogicalModelTypeReference LogicalModelTypeReferenceC {..} -> do
        supportsNestedObjects <- backendSupportsNestedObjects @b
        pure (RawColumnTypeObject supportsNestedObjects (getLogicalModelName lmtrReference), lmtrNullable)

    -- Step 2: Process the raw table cache to replace Postgres column types with logical column
    -- types.
    processTableInfo ::
      (QErrM n) =>
      HashMap.HashMap (TableName b) (PrimaryKey b (Column b), TableConfig b, EnumValues) ->
      TableCoreInfoG b (RawColumnInfo b) (Column b) ->
      NamingCase ->
      n (TableCoreInfoG b (StructuredColumnInfo b) (ColumnInfo b))
    processTableInfo enumTables rawInfo tCase = do
      let columns = _tciFieldInfoMap rawInfo
          enumReferences = resolveEnumReferences enumTables (_tciForeignKeys rawInfo)
      columnInfoMap <-
        collectColumnConfiguration columns (_tciCustomConfig rawInfo)
          >>= traverse (processColumnInfo tCase enumReferences (_tciName rawInfo))
      assertNoDuplicateFieldNames (HashMap.elems columnInfoMap)

      primaryKey <- traverse (resolvePrimaryKeyColumns $ HashMap.mapMaybe toScalarColumnInfo columnInfoMap) (_tciPrimaryKey rawInfo)
      pure
        rawInfo
          { _tciFieldInfoMap = columnInfoMap,
            _tciPrimaryKey = primaryKey
          }

    resolvePrimaryKeyColumns ::
      forall n a. (QErrM n) => HashMap FieldName a -> PrimaryKey b (Column b) -> n (PrimaryKey b a)
    resolvePrimaryKeyColumns columnMap = traverseOf (pkColumns . traverse) \columnName ->
      HashMap.lookup (FieldName (toTxt columnName)) columnMap
        `onNothing` throw500 "column in primary key not in table!"

    collectColumnConfiguration ::
      (QErrM n) =>
      FieldInfoMap (RawColumnInfo b) ->
      TableConfig b ->
      n (FieldInfoMap (RawColumnInfo b, GQLNameIdentifier, Maybe G.Description))
    collectColumnConfiguration columns TableConfig {..} = do
      let configByFieldName = mapKeys (fromCol @b) _tcColumnConfig
      HashMap.traverseWithKey
        (\fieldName -> pairColumnInfoAndConfig fieldName >=> extractColumnConfiguration fieldName)
        (align columns configByFieldName)

    pairColumnInfoAndConfig ::
      (QErrM n) =>
      FieldName ->
      These (RawColumnInfo b) ColumnConfig ->
      n (RawColumnInfo b, ColumnConfig)
    pairColumnInfoAndConfig fieldName = \case
      This column -> pure (column, mempty)
      These column config -> pure (column, config)
      That _ ->
        throw400 NotExists
          $ "configuration was given for the column "
          <> fieldName
          <<> ", but no such column exists"

    extractColumnConfiguration ::
      (QErrM n) =>
      FieldName ->
      (RawColumnInfo b, ColumnConfig) ->
      n (RawColumnInfo b, GQLNameIdentifier, Maybe G.Description)
    extractColumnConfiguration fieldName (columnInfo, ColumnConfig {..}) = do
      name <- (fromCustomName <$> _ccfgCustomName) `onNothing` textToGQLIdentifier (getFieldNameTxt fieldName)
      pure (columnInfo, name, description)
      where
        description :: Maybe G.Description
        description = case _ccfgComment of
          Automatic -> rciDescription columnInfo
          (Explicit explicitDesc) -> G.Description . toTxt <$> explicitDesc

    processColumnInfo ::
      (QErrM n) =>
      NamingCase ->
      HashMap.HashMap (Column b) (NonEmpty (EnumReference b)) ->
      TableName b ->
      (RawColumnInfo b, GQLNameIdentifier, Maybe G.Description) ->
      n (StructuredColumnInfo b)
    processColumnInfo tCase tableEnumReferences tableName (rawInfo, name, description) =
      processRawColumnType (rciIsNullable rawInfo) $ rciType rawInfo
      where
        processRawColumnType isNullable = \case
          RawColumnTypeScalar scalarType -> do
            resolvedType <- resolveColumnType scalarType
            pure
              $ SCIScalarColumn
                ColumnInfo
                  { ciColumn = pgCol,
                    ciName = applyFieldNameCaseIdentifier tCase name,
                    ciPosition = rciPosition rawInfo,
                    ciType = resolvedType,
                    ciIsNullable = isNullable,
                    ciDescription = description,
                    ciMutability = rciMutability rawInfo
                  }
          RawColumnTypeObject supportsNestedObjects objectTypeName ->
            pure
              $ SCIObjectColumn @b
                NestedObjectInfo
                  { _noiSupportsNestedObjects = supportsNestedObjects,
                    _noiColumn = pgCol,
                    _noiName = applyFieldNameCaseIdentifier tCase name,
                    _noiType = LogicalModelName objectTypeName,
                    _noiIsNullable = isNullable,
                    _noiDescription = description,
                    _noiMutability = rciMutability rawInfo
                  }
          RawColumnTypeArray supportsNestedArrays rawColumnType isNullable' -> do
            nestedColumnInfo <- processRawColumnType isNullable' rawColumnType
            pure
              $ SCIArrayColumn @b
                NestedArrayInfo
                  { _naiSupportsNestedArrays = supportsNestedArrays,
                    _naiIsNullable = isNullable,
                    _naiColumnInfo = nestedColumnInfo
                  }
        pgCol = rciName rawInfo
        resolveColumnType scalarType =
          case HashMap.lookup pgCol tableEnumReferences of
            -- no references? not an enum
            Nothing -> pure $ ColumnScalar scalarType
            -- one reference? is an enum
            Just (enumReference :| []) -> pure $ ColumnEnumReference enumReference
            -- multiple referenced enums? the schema is strange, so let’s reject it
            Just enumReferences ->
              throw400 ConstraintViolation
                $ "column "
                <> rciName rawInfo
                <<> " in table "
                <> tableName
                <<> " references multiple enum tables ("
                <> commaSeparated (map (dquote . erTable) $ toList enumReferences)
                <> ")"

    assertNoDuplicateFieldNames columns =
      void $ flip HashMap.traverseWithKey (HashMap.groupOn structuredColumnInfoName columns) \name columnsWithName ->
        case columnsWithName of
          one : two : more ->
            throw400 AlreadyExists
              $ "the definitions of columns "
              <> englishList "and" (dquote . structuredColumnInfoColumn <$> (one :| two : more))
              <> " are in conflict: they are mapped to the same field name, "
              <>> name
          _ -> pure ()

    buildDescription :: TableName b -> TableConfig b -> DBTableMetadata b -> Maybe PGDescription
    buildDescription tableName tableConfig tableMetadata =
      case _tcComment tableConfig of
        Automatic -> _ptmiDescription tableMetadata <|> Just autogeneratedDescription
        Explicit description -> PGDescription . toTxt <$> description
      where
        autogeneratedDescription =
          PGDescription $ "columns and relationships of " <>> tableName

data SetApolloFederationConfig b = SetApolloFederationConfig
  { _safcSource :: SourceName,
    _safcTable :: TableName b,
    -- | Apollo Federation config for the table, setting `Nothing` would disable
    --   Apollo Federation support on the table.
    _safcApolloFederationConfig :: Maybe ApolloFederationConfig
  }

instance (Backend b) => FromJSON (SetApolloFederationConfig b) where
  parseJSON = withObject "SetApolloFederationConfig" $ \o ->
    SetApolloFederationConfig
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> o
      .:? "apollo_federation_config"

runSetApolloFederationConfig ::
  forall b m.
  (QErrM m, CacheRWM m, MetadataM m, Backend b) =>
  SetApolloFederationConfig b ->
  m EncJSON
runSetApolloFederationConfig (SetApolloFederationConfig source table apolloFedConfig) = do
  void $ askTableInfo @b source table
  buildSchemaCacheFor
    (MOSourceObjId source $ AB.mkAnyBackend $ SMOTable @b table)
    -- NOTE (paritosh): This API behaves like a PUT API now. In future, when
    -- the `ApolloFederationConfig` is complex, we should probably reconsider
    -- this approach of replacing the configuration everytime the API is called
    -- and maybe throw some error if the configuration is already there.
    $ MetadataModifier
    $ tableMetadataSetter @b source table
    . tmApolloFederationConfig
    .~ apolloFedConfig
  return successMsg
