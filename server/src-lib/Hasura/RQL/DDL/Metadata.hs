module Hasura.RQL.DDL.Metadata
  ( runReplaceMetadata
  , runExportMetadata
  , runClearMetadata
  , runReloadMetadata
  , runDumpInternalState
  , runGetInconsistentMetadata
  , runDropInconsistentMetadata
  , runGetCatalogState
  , runSetCatalogState

  , module Hasura.RQL.DDL.Metadata.Types
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered                as AO
import qualified Data.HashMap.Strict               as HM
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Data.HashSet                      as HS
import qualified Data.List                         as L

import           Control.Lens                      ((^?))
import           Data.Aeson

import           Hasura.Metadata.Class
import           Hasura.RQL.DDL.Action
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.CustomTypes
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.ScheduledTrigger
import           Hasura.RQL.DDL.Schema

import           Hasura.EncJSON
import           Hasura.RQL.DDL.Metadata.Types
import           Hasura.RQL.Types

runClearMetadata
  :: (MonadIO m, CacheRWM m, MetadataM m, MonadError QErr m)
  => ClearMetadata -> m EncJSON
runClearMetadata _ = do
  metadata <- getMetadata
  -- We can infer whether the server is started with `--database-url` option
  -- (or corresponding env variable) by checking the existence of @'defaultSource'
  -- in current metadata.
  let maybeDefaultSourceMetadata = metadata ^? metaSources.ix defaultSource
      emptyMetadata' = case maybeDefaultSourceMetadata of
          Nothing -> emptyMetadata
          Just defaultSourceMetadata ->
            -- If default postgres source is defined, we need to set metadata
            -- which contains only default source without any tables and functions.
            let emptyDefaultSource = SourceMetadata defaultSource mempty mempty
                                     $ _smConfiguration defaultSourceMetadata
            in emptyMetadata
               & metaSources %~ OMap.insert defaultSource emptyDefaultSource
  runReplaceMetadata $ RMWithSources emptyMetadata'

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
     )
  => ReplaceMetadata -> m EncJSON
runReplaceMetadata replaceMetadata = do
  oldMetadata <- getMetadata
  metadata <- case replaceMetadata of
    RMWithSources m -> pure m
    RMWithoutSources MetadataNoSources{..} -> do
      defaultSourceMetadata <- onNothing (OMap.lookup defaultSource $ _metaSources oldMetadata) $
        throw400 NotSupported $ "cannot import metadata without sources since no default source is defined"
      let newDefaultSourceMetadata = defaultSourceMetadata
                                     { _smTables = _mnsTables
                                     , _smFunctions = _mnsFunctions
                                     }
      pure $ Metadata (OMap.singleton defaultSource newDefaultSourceMetadata)
                        _mnsRemoteSchemas _mnsQueryCollections _mnsAllowlist
                        _mnsCustomTypes _mnsActions _mnsCronTriggers
  putMetadata metadata
  buildSchemaCacheStrict
  -- See Note [Clear postgres schema for dropped triggers]
  for_ (OMap.toList $ _metaSources metadata) $ \(source, newSourceCache) ->
    onJust (OMap.lookup source $ _metaSources oldMetadata) $ \oldSourceCache -> do
      let getTriggersMap = OMap.unions . map _tmEventTriggers . OMap.elems . _smTables
          oldTriggersMap = getTriggersMap oldSourceCache
          newTriggersMap = getTriggersMap newSourceCache
          droppedTriggers = OMap.keys $ oldTriggersMap `OMap.difference` newTriggersMap
      sourceConfig <- _pcConfiguration <$> askPGSourceCache source
      for_ droppedTriggers $
        \name -> liftIO $ runPgSourceWriteTx sourceConfig $ delTriggerQ name >> archiveEvents name

  pure successMsg

runExportMetadata
  :: (MetadataM m)
  => ExportMetadata -> m EncJSON
runExportMetadata _ =
  AO.toEncJSON . metadataToOrdJSON <$> getMetadata

runReloadMetadata :: (QErrM m, CacheRWM m, MetadataM m) => ReloadMetadata -> m EncJSON
runReloadMetadata (ReloadMetadata reloadRemoteSchemas reloadSources) = do
  sc <- askSchemaCache
  let remoteSchemaInvalidations = case reloadRemoteSchemas of
        RSReloadAll    -> HS.fromList $ getAllRemoteSchemas sc
        RSReloadList l -> l
      pgSourcesInvalidations = case reloadSources of
        RSReloadAll    -> HS.fromList $ HM.keys $ scPostgres sc
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
  return $ encJFromJValue $ object
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
  putMetadata $ unMetadataModifier metadataModifier $ metadata
  buildSchemaCacheStrict
  return successMsg

purgeMetadataObj :: MetadataObjId -> MetadataModifier
purgeMetadataObj = \case
  MOSource source -> MetadataModifier $ metaSources %~ OMap.delete source
  MOSourceObjId source sourceObjId -> case sourceObjId of
    SMOTable qt                                 -> dropTableInMetadata source qt
    SMOTableObj qt tableObj -> MetadataModifier $
      tableMetadataSetter source qt %~ case tableObj of
        MTORel rn _              -> dropRelationshipInMetadata rn
        MTOPerm rn pt            -> dropPermissionInMetadata rn pt
        MTOTrigger trn           -> dropEventTriggerInMetadata trn
        MTOComputedField ccn     -> dropComputedFieldInMetadata ccn
        MTORemoteRelationship rn -> dropRemoteRelationshipInMetadata rn
    SMOFunction qf                              -> dropFunctionInMetadata source qf
  MORemoteSchema rsn                         -> dropRemoteSchemaInMetadata rsn
  MORemoteSchemaPermissions rsName role      -> dropRemoteSchemaPermissionInMetadata rsName role
  MOCustomTypes                              -> clearCustomTypesInMetadata
  MOAction action                            -> dropActionInMetadata action -- Nothing
  MOActionPermission action role             -> dropActionPermissionInMetadata action role
  MOCronTrigger ctName                       -> dropCronTriggerInMetadata ctName

runGetCatalogState
  :: (MonadMetadataStorageQueryAPI m) => GetCatalogState -> m EncJSON
runGetCatalogState _ =
  encJFromJValue <$> fetchCatalogState

runSetCatalogState
  :: (MonadMetadataStorageQueryAPI m) => SetCatalogState -> m EncJSON
runSetCatalogState SetCatalogState{..} = do
  updateCatalogState _scsType _scsState
  pure successMsg
