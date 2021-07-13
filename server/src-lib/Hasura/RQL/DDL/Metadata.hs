module Hasura.RQL.DDL.Metadata
  ( runReplaceMetadata
  , runReplaceMetadataV2
  , runExportMetadata
  , runExportMetadataV2
  , runClearMetadata
  , runReloadMetadata
  , runDumpInternalState
  , runGetInconsistentMetadata
  , runDropInconsistentMetadata
  , runGetCatalogState
  , runSetCatalogState

  , runSetMetricsConfig
  , runRemoveMetricsConfig

  , module Hasura.RQL.DDL.Metadata.Types
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered                 as AO
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.HashSet                       as HS
import qualified Data.List                          as L

import           Control.Lens                       ((.~), (^?))
import           Data.Aeson
import           Data.Text.Extended                 ((<<>))

import qualified Hasura.SQL.AnyBackend              as AB

import           Hasura.Backends.Postgres.DDL.Table (delTriggerQ)
import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.CustomTypes
import           Hasura.RQL.DDL.Endpoint
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.InheritedRoles
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.ScheduledTrigger
import           Hasura.RQL.DDL.Schema

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Metadata.Types
import           Hasura.RQL.Types
import           Hasura.Server.Types                (ExperimentalFeature (..))


runClearMetadata
  :: ( MonadIO m
     , CacheRWM m
     , MetadataM m
     , HasServerConfigCtx m
     , MonadMetadataStorageQueryAPI m
     )
  => ClearMetadata -> m EncJSON
runClearMetadata _ = do
  metadata <- getMetadata
  -- We can infer whether the server is started with `--database-url` option
  -- (or corresponding env variable) by checking the existence of @'defaultSource'
  -- in current metadata.
  let maybeDefaultSourceMetadata = metadata ^? metaSources.ix defaultSource
      emptyMetadata' = case maybeDefaultSourceMetadata of
          Nothing -> emptyMetadata
          Just exists ->
            -- If default postgres source is defined, we need to set metadata
            -- which contains only default source without any tables and functions.
            let emptyDefaultSource =
                  AB.dispatchAnyBackend @Backend exists \(s :: SourceMetadata b) ->
                    AB.mkAnyBackend @b
                    $ SourceMetadata @b defaultSource mempty mempty
                    $ _smConfiguration @b s
            in emptyMetadata
               & metaSources %~ OMap.insert defaultSource emptyDefaultSource
  runReplaceMetadataV1 $ RMWithSources emptyMetadata'

{- Note [Clear postgres schema for dropped triggers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There was an issue (https://github.com/hasura/graphql-engine/issues/5461)
fixed (via https://github.com/hasura/graphql-engine/pull/6137) related to
event triggers while replacing metadata in the catalog prior to metadata
separation. The metadata separation solves the issue naturally, since the
'hdb_catalog.event_triggers' table is no more in use and new/updated event
triggers are processed in building schema cache. But we need to drop the
pg trigger and archive events for dropped event triggers. This is handled
explicitly in @'runReplaceMetadata' function.
-}

runReplaceMetadata
  :: ( CacheRWM m
     , MetadataM m
     , MonadIO m
     , MonadMetadataStorageQueryAPI m
     , HasServerConfigCtx m
     )
  => ReplaceMetadata -> m EncJSON
runReplaceMetadata = \case
  RMReplaceMetadataV1 v1args -> runReplaceMetadataV1 v1args
  RMReplaceMetadataV2 v2args -> runReplaceMetadataV2 v2args

runReplaceMetadataV1
  :: ( QErrM m
     , CacheRWM m
     , MetadataM m
     , MonadIO m
     , MonadMetadataStorageQueryAPI m
     , HasServerConfigCtx m
     )
  => ReplaceMetadataV1 -> m EncJSON
runReplaceMetadataV1 =
  (successMsg <$) . runReplaceMetadataV2 . ReplaceMetadataV2 NoAllowInconsistentMetadata

runReplaceMetadataV2
  :: forall m
   . ( QErrM m
     , CacheRWM m
     , MetadataM m
     , MonadIO m
     , HasServerConfigCtx m
     , MonadMetadataStorageQueryAPI m
     )
  => ReplaceMetadataV2 -> m EncJSON
runReplaceMetadataV2 ReplaceMetadataV2{..} = do
  -- we drop all the future cron trigger events before inserting the new metadata
  -- and re-populating future cron events below
  experimentalFeatures <- _sccExperimentalFeatures <$> askServerConfigCtx
  let inheritedRoles =
        case _rmv2Metadata of
          RMWithSources (Metadata { _metaInheritedRoles }) -> _metaInheritedRoles
          RMWithoutSources _                               -> mempty
      introspectionDisabledRoles =
        case _rmv2Metadata of
          RMWithSources m    -> _metaSetGraphqlIntrospectionOptions m
          RMWithoutSources _ -> mempty
  when (inheritedRoles /= mempty && (EFInheritedRoles `notElem` experimentalFeatures)) $
    throw400 ConstraintViolation $ "inherited_roles can only be added when it's enabled in the experimental features"

  oldMetadata <- getMetadata
  let (oldCronTriggersIncludedInMetadata, oldCronTriggersNotIncludedInMetadata) =
        ((OMap.filter ctIncludeInMetadata (_metaCronTriggers  oldMetadata))
        ,(OMap.filter (not . ctIncludeInMetadata) (_metaCronTriggers  oldMetadata)))
      newCronTriggers =
        case _rmv2Metadata of
          RMWithoutSources m -> _mnsCronTriggers m
          RMWithSources m    -> _metaCronTriggers m
  dropFutureCronEvents $ MetadataCronTriggers $ OMap.keys oldCronTriggersIncludedInMetadata
  cronTriggers <- do
    -- traverse over the new cron triggers and check if any of them
    -- already exists as a cron trigger with "included_in_metadata: false"
    for_ newCronTriggers $ \ct ->
      when (ctName ct `OMap.member` oldCronTriggersNotIncludedInMetadata) $
        throw400 AlreadyExists $
        "cron trigger with name "
        <> ctName ct
        <<> " already exists as a cron trigger with \"included_in_metadata\" as false"
    -- we add the old cron triggers with included_in_metadata set to false with the
    -- newly added cron triggers
    pure $ newCronTriggers <> oldCronTriggersNotIncludedInMetadata
  metadata <- case _rmv2Metadata of
    RMWithSources m -> pure $ m { _metaCronTriggers = cronTriggers }
    RMWithoutSources MetadataNoSources{..} -> do
      let maybeDefaultSourceMetadata = oldMetadata ^? metaSources.ix defaultSource.toSourceMetadata
      defaultSourceMetadata <- onNothing maybeDefaultSourceMetadata $
        throw400 NotSupported "cannot import metadata without sources since no default source is defined"
      let newDefaultSourceMetadata = AB.mkAnyBackend defaultSourceMetadata
                                     { _smTables = _mnsTables
                                     , _smFunctions = _mnsFunctions
                                     }
      pure $ Metadata (OMap.singleton defaultSource newDefaultSourceMetadata)
                        _mnsRemoteSchemas _mnsQueryCollections _mnsAllowlist
                        _mnsCustomTypes _mnsActions cronTriggers (_metaRestEndpoints oldMetadata)
                        emptyApiLimit emptyMetricsConfig mempty introspectionDisabledRoles
  putMetadata metadata

  case _rmv2AllowInconsistentMetadata of
    AllowInconsistentMetadata ->
      buildSchemaCache noMetadataModify
    NoAllowInconsistentMetadata ->
      buildSchemaCacheStrict

  -- populate future cron events for all the new cron triggers that are imported
  for_ newCronTriggers $ \(CronTriggerMetadata {..}) ->
    populateInitialCronTriggerEvents ctSchedule ctName

  -- See Note [Clear postgres schema for dropped triggers]
  dropPostgresTriggers (getOnlyPGSources oldMetadata) (getOnlyPGSources metadata)

  sc <- askSchemaCache
  pure $ encJFromJValue $ formatInconsistentObjs $ scInconsistentObjs sc
  where
    getOnlyPGSources :: Metadata -> InsOrdHashMap SourceName (SourceMetadata ('Postgres 'Vanilla))
    getOnlyPGSources = OMap.mapMaybe AB.unpackAnyBackend . _metaSources

    dropPostgresTriggers
      :: InsOrdHashMap SourceName (SourceMetadata ('Postgres 'Vanilla)) -- ^ old pg sources
      -> InsOrdHashMap SourceName (SourceMetadata ('Postgres 'Vanilla)) -- ^ new pg sources
      -> m ()
    dropPostgresTriggers oldSources newSources =
      for_ (OMap.toList newSources) $ \(source, newSourceCache) ->
        onJust (OMap.lookup source oldSources) $ \oldSourceCache -> do
          let oldTriggersMap = getPGTriggersMap oldSourceCache
              newTriggersMap = getPGTriggersMap newSourceCache
              droppedTriggers = OMap.keys $ oldTriggersMap `OMap.difference` newTriggersMap
              catcher e@QErr{ qeCode }
                | qeCode == Unexpected = pure () -- NOTE: This information should be returned by the inconsistent_metadata response, so doesn't need additional logging.
                | otherwise = throwError e -- rethrow other errors

          -- This will swallow Unexpected exceptions for sources if allow_inconsistent_metadata is enabled
          -- This should be ok since if the sources are already missing from the cache then they should
          -- not need to be removed.
          --
          -- TODO: Determine if any errors should be thrown from askSourceConfig at all if the errors are just being discarded
          flip catchError catcher do
            sourceConfig <- askSourceConfig @('Postgres 'Vanilla) source
            for_ droppedTriggers $
              \name -> do
                  liftIO $ runPgSourceWriteTx sourceConfig $ delTriggerQ name >> archiveEvents name
      where
        getPGTriggersMap = OMap.unions . map _tmEventTriggers . OMap.elems . _smTables

processExperimentalFeatures :: HasServerConfigCtx m => Metadata -> m Metadata
processExperimentalFeatures metadata = do
  experimentalFeatures <- _sccExperimentalFeatures <$> askServerConfigCtx
  let isInheritedRolesSet = EFInheritedRoles `elem` experimentalFeatures
  -- export inherited roles only when inherited_roles is set in the experimental features
  pure $ bool (metadata { _metaInheritedRoles = mempty }) metadata isInheritedRolesSet

-- | Only includes the cron triggers with `included_in_metadata` set to `True`
processCronTriggersMetadata :: Metadata -> Metadata
processCronTriggersMetadata metadata =
  let cronTriggersIncludedInMetadata = OMap.filter ctIncludeInMetadata $ _metaCronTriggers metadata
  in metadata { _metaCronTriggers = cronTriggersIncludedInMetadata }

processMetadata :: HasServerConfigCtx m => Metadata -> m Metadata
processMetadata metadata =
  processCronTriggersMetadata <$> processExperimentalFeatures metadata

runExportMetadata
  :: forall m . ( QErrM m, MetadataM m, HasServerConfigCtx m)
  => ExportMetadata -> m EncJSON
runExportMetadata ExportMetadata{} = do
  AO.toEncJSON . metadataToOrdJSON <$> (getMetadata >>= processMetadata)

runExportMetadataV2
  :: forall m . ( QErrM m, MetadataM m, HasServerConfigCtx m)
  => MetadataResourceVersion -> ExportMetadata -> m EncJSON
runExportMetadataV2 currentResourceVersion ExportMetadata{} = do
  exportMetadata <- processExperimentalFeatures =<< getMetadata
  pure $ AO.toEncJSON $ AO.object
    [ ("resource_version", AO.toOrdered currentResourceVersion)
    , ("metadata", metadataToOrdJSON exportMetadata)
    ]

runReloadMetadata :: (QErrM m, CacheRWM m, MetadataM m) => ReloadMetadata -> m EncJSON
runReloadMetadata (ReloadMetadata reloadRemoteSchemas reloadSources) = do
  metadata <- getMetadata
  let allSources = HS.fromList $ OMap.keys $ _metaSources metadata
      allRemoteSchemas = HS.fromList $ OMap.keys $ _metaRemoteSchemas metadata
      checkRemoteSchema name =
        unless (HS.member name allRemoteSchemas)
        $ throw400 NotExists
        $ "Remote schema with name " <> name <<> " not found in metadata"
      checkSource name =
        unless (HS.member name allSources)
        $ throw400 NotExists
        $ "Source with name " <> name <<> " not found in metadata"

  remoteSchemaInvalidations <- case reloadRemoteSchemas of
    RSReloadAll    -> pure allRemoteSchemas
    RSReloadList l -> mapM_ checkRemoteSchema l *> pure l
  pgSourcesInvalidations <- case reloadSources of
    RSReloadAll    -> pure allSources
    RSReloadList l -> mapM_ checkSource l *> pure l

  let cacheInvalidations = CacheInvalidations
                           { ciMetadata = True
                           , ciRemoteSchemas = remoteSchemaInvalidations
                           , ciSources = pgSourcesInvalidations
                           }

  buildSchemaCacheWithOptions CatalogUpdate cacheInvalidations metadata
  pure successMsg

runDumpInternalState
  :: (QErrM m, CacheRM m)
  => DumpInternalState -> m EncJSON
runDumpInternalState _ =
  encJFromJValue <$> askSchemaCache


runGetInconsistentMetadata
  :: (QErrM m, CacheRM m)
  => GetInconsistentMetadata -> m EncJSON
runGetInconsistentMetadata _ = do
  inconsObjs <- scInconsistentObjs <$> askSchemaCache
  return $ encJFromJValue $ formatInconsistentObjs inconsObjs

formatInconsistentObjs :: [InconsistentMetadata] -> Value
formatInconsistentObjs inconsObjs = object
  [ "is_consistent" .= null inconsObjs
  , "inconsistent_objects" .= inconsObjs
  ]

runDropInconsistentMetadata
  :: (QErrM m, CacheRWM m, MetadataM m)
  => DropInconsistentMetadata -> m EncJSON
runDropInconsistentMetadata _ = do
  sc <- askSchemaCache
  let inconsSchObjs = L.nub . concatMap imObjectIds $ scInconsistentObjs sc
  -- Note: when building the schema cache, we try to put dependents after their dependencies in the
  -- list of inconsistent objects, so reverse the list to start with dependents first. This is not
  -- perfect — a completely accurate solution would require performing a topological sort — but it
  -- seems to work well enough for now.
  metadataModifier <- execWriterT $ mapM_ (tell . purgeMetadataObj) (reverse inconsSchObjs)
  metadata <- getMetadata
  putMetadata $ unMetadataModifier metadataModifier metadata
  buildSchemaCacheStrict
  return successMsg

purgeMetadataObj :: MetadataObjId -> MetadataModifier
purgeMetadataObj = \case
  MOSource source                       -> MetadataModifier $ metaSources %~ OMap.delete source
  MOSourceObjId source exists           -> AB.dispatchAnyBackend @BackendMetadata exists $ handleSourceObj source
  MORemoteSchema rsn                    -> dropRemoteSchemaInMetadata rsn
  MORemoteSchemaPermissions rsName role -> dropRemoteSchemaPermissionInMetadata rsName role
  MOCustomTypes                         -> clearCustomTypesInMetadata
  MOAction action                       -> dropActionInMetadata action -- Nothing
  MOActionPermission action role        -> dropActionPermissionInMetadata action role
  MOCronTrigger ctName                  -> dropCronTriggerInMetadata ctName
  MOEndpoint epName                     -> dropEndpointInMetadata epName
  MOInheritedRole role                  -> dropInheritedRoleInMetadata role
  where
    handleSourceObj :: forall b. BackendMetadata b => SourceName -> SourceMetadataObjId b -> MetadataModifier
    handleSourceObj source = \case
      SMOTable qt                 -> dropTableInMetadata @b source qt
      SMOFunction qf              -> dropFunctionInMetadata @b source qf
      SMOFunctionPermission qf rn -> dropFunctionPermissionInMetadata @b source qf rn
      SMOTableObj qt tableObj     ->
        MetadataModifier
          $ tableMetadataSetter @b source qt %~ case tableObj of
            MTORel rn _              -> dropRelationshipInMetadata rn
            MTOPerm rn pt            -> dropPermissionInMetadata rn pt
            MTOTrigger trn           -> dropEventTriggerInMetadata trn
            MTOComputedField ccn     -> dropComputedFieldInMetadata ccn
            MTORemoteRelationship rn -> dropRemoteRelationshipInMetadata rn

runGetCatalogState
  :: (MonadMetadataStorageQueryAPI m) => GetCatalogState -> m EncJSON
runGetCatalogState _ =
  encJFromJValue <$> fetchCatalogState

runSetCatalogState
  :: (MonadMetadataStorageQueryAPI m) => SetCatalogState -> m EncJSON
runSetCatalogState SetCatalogState{..} = do
  updateCatalogState _scsType _scsState
  pure successMsg

runSetMetricsConfig
  :: (MonadIO m, CacheRWM m, MetadataM m, MonadError QErr m)
  => MetricsConfig -> m EncJSON
runSetMetricsConfig mc = do
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaMetricsConfig .~ mc
  pure successMsg

runRemoveMetricsConfig
  :: (MonadIO m, CacheRWM m, MetadataM m, MonadError QErr m)
  => m EncJSON
runRemoveMetricsConfig = do
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ MetadataModifier
    $ metaMetricsConfig .~ emptyMetricsConfig
  pure successMsg
