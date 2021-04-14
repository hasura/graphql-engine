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
import qualified Data.HashMap.Strict                as HM
import qualified Data.HashMap.Strict.InsOrd         as OMap
import qualified Data.HashSet                       as HS
import qualified Data.List                          as L

import           Control.Lens                       ((.~), (^?))
import           Data.Aeson

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

import           Hasura.EncJSON
import           Hasura.RQL.DDL.Metadata.Types
import           Hasura.RQL.Types
import           Hasura.Server.Types                (ExperimentalFeature (..))

runClearMetadata
  :: (MonadIO m, CacheRWM m, MetadataM m, MonadError QErr m, HasServerConfigCtx m)
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
                  AB.dispatchAnyBackend @Backend exists
                    $ AB.mkAnyBackend
                    . SourceMetadata defaultSource mempty mempty
                    . _smConfiguration
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
  :: ( QErrM m
     , CacheRWM m
     , MetadataM m
     , MonadIO m
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
     )
  => ReplaceMetadataV2 -> m EncJSON
runReplaceMetadataV2 ReplaceMetadataV2{..} = do
  experimentalFeatures <- _sccExperimentalFeatures <$> askServerConfigCtx
  let inheritedRoles =
        case _rmv2Metadata of
          RMWithSources (Metadata { _metaInheritedRoles }) -> _metaInheritedRoles
          RMWithoutSources _                               -> mempty
  when (inheritedRoles /= mempty && (EFInheritedRoles `notElem` experimentalFeatures)) $
    throw400 ConstraintViolation $ "inherited_roles can only be added when it's enabled in the experimental features"
  oldMetadata <- getMetadata
  metadata <- case _rmv2Metadata of
    RMWithSources m -> pure m
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
                        _mnsCustomTypes _mnsActions _mnsCronTriggers (_metaRestEndpoints oldMetadata)
                        emptyApiLimit emptyMetricsConfig mempty
  putMetadata metadata

  case _rmv2AllowInconsistentMetadata of
    AllowInconsistentMetadata ->
      buildSchemaCache noMetadataModify
    NoAllowInconsistentMetadata ->
      buildSchemaCacheStrict

  -- See Note [Clear postgres schema for dropped triggers]
  dropPostgresTriggers (getOnlyPGSources oldMetadata) (getOnlyPGSources metadata)

  sc <- askSchemaCache
  pure $ encJFromJValue $ formatInconsistentObjs $ scInconsistentObjs sc
  where
    getOnlyPGSources :: Metadata -> InsOrdHashMap SourceName (SourceMetadata 'Postgres)
    getOnlyPGSources = OMap.mapMaybe AB.unpackAnyBackend . _metaSources

    dropPostgresTriggers
      :: InsOrdHashMap SourceName (SourceMetadata 'Postgres) -- ^ old pg sources
      -> InsOrdHashMap SourceName (SourceMetadata 'Postgres) -- ^ new pg sources
      -> m ()
    dropPostgresTriggers oldSources newSources =
      for_ (OMap.toList newSources) $ \(source, newSourceCache) ->
        onJust (OMap.lookup source oldSources) $ \oldSourceCache -> do
          let oldTriggersMap = getPGTriggersMap oldSourceCache
              newTriggersMap = getPGTriggersMap newSourceCache
              droppedTriggers = OMap.keys $ oldTriggersMap `OMap.difference` newTriggersMap
          sourceConfig <- askSourceConfig source
          for_ droppedTriggers $
            \name -> liftIO $ runPgSourceWriteTx sourceConfig $ delTriggerQ name >> archiveEvents name
      where
        getPGTriggersMap = OMap.unions . map _tmEventTriggers . OMap.elems . _smTables

processExperimentalFeatures :: HasServerConfigCtx m => Metadata -> m Metadata
processExperimentalFeatures metadata = do
  experimentalFeatures <- _sccExperimentalFeatures <$> askServerConfigCtx
  let isInheritedRolesSet = EFInheritedRoles `elem` experimentalFeatures
  -- export inherited roles only when inherited_roles is set in the experimental features
  pure $ bool (metadata { _metaInheritedRoles = mempty }) metadata isInheritedRolesSet

runExportMetadata
  :: forall m . ( QErrM m, MetadataM m, HasServerConfigCtx m)
  => ExportMetadata -> m EncJSON
runExportMetadata ExportMetadata{} = do
  AO.toEncJSON . metadataToOrdJSON <$> (getMetadata >>= processExperimentalFeatures)

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
  sc <- askSchemaCache
  let remoteSchemaInvalidations = case reloadRemoteSchemas of
        RSReloadAll    -> HS.fromList $ getAllRemoteSchemas sc
        RSReloadList l -> l
      pgSourcesInvalidations = case reloadSources of
        RSReloadAll    -> HS.fromList $ HM.keys $ scSources sc
        RSReloadList l -> l
      cacheInvalidations = CacheInvalidations
                           { ciMetadata = True
                           , ciRemoteSchemas = remoteSchemaInvalidations
                           , ciSources = pgSourcesInvalidations
                           }
  metadata <- getMetadata
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
    handleSourceObj :: BackendMetadata b => SourceName -> SourceMetadataObjId b -> MetadataModifier
    handleSourceObj source = \case
      SMOTable qt                 -> dropTableInMetadata source qt
      SMOFunction qf              -> dropFunctionInMetadata source qf
      SMOFunctionPermission qf rn -> dropFunctionPermissionInMetadata source qf rn
      SMOTableObj qt tableObj     ->
        MetadataModifier
          $ tableMetadataSetter source qt %~ case tableObj of
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
