module Hasura.RQL.DDL.Metadata
  ( runReplaceMetadata
  , runExportMetadata
  , runClearMetadata
  , runReloadMetadata
  , runDumpInternalState
  , runGetInconsistentMetadata
  , runDropInconsistentMetadata

  , module Hasura.RQL.DDL.Metadata.Types
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered                as AO
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Data.HashSet                      as HS
import qualified Data.List                         as L

import           Data.Aeson

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
  :: (CacheRWM m, MetadataM m, MonadTx m)
  => ClearMetadata -> m EncJSON
runClearMetadata _ = do
  runReplaceMetadata emptyMetadata

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
     , MonadTx m
     )
  => Metadata -> m EncJSON
runReplaceMetadata metadata = do
  oldMetadata <- getMetadata
  putMetadata metadata
  buildSchemaCacheStrict
  -- See Note [Clear postgres schema for dropped triggers]
  let getTriggersMap = OMap.unions . map _tmEventTriggers . OMap.elems . _metaTables
      oldTriggersMap = getTriggersMap oldMetadata
      newTriggersMap = getTriggersMap metadata
      droppedTriggers = OMap.keys $ oldTriggersMap `OMap.difference` newTriggersMap
  for_ droppedTriggers $ \name -> liftTx $ delTriggerQ name >> archiveEvents name

  pure successMsg

runExportMetadata
  :: (MetadataM m)
  => ExportMetadata -> m EncJSON
runExportMetadata _ =
  AO.toEncJSON . metadataToOrdJSON <$> getMetadata

runReloadMetadata :: (QErrM m, CacheRWM m, MetadataM m) => ReloadMetadata -> m EncJSON
runReloadMetadata (ReloadMetadata reloadRemoteSchemas) = do
  sc <- askSchemaCache
  let remoteSchemaInvalidations =
        if reloadRemoteSchemas then HS.fromList (getAllRemoteSchemas sc) else mempty
      cacheInvalidations = CacheInvalidations
                           { ciMetadata = True
                           , ciRemoteSchemas = remoteSchemaInvalidations
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
  MOTable qt                                 -> dropTableInMetadata qt
  MOTableObj qt tableObj                     ->
    MetadataModifier $
    metaTables.ix qt %~ case tableObj of
        MTORel rn _              -> dropRelationshipInMetadata rn
        MTOPerm rn pt            -> dropPermissionInMetadata rn pt
        MTOTrigger trn           -> dropEventTriggerInMetadata trn
        MTOComputedField ccn     -> dropComputedFieldInMetadata ccn
        MTORemoteRelationship rn -> dropRemoteRelationshipInMetadata rn
  MOFunction qf                              -> dropFunctionInMetadata qf
  MORemoteSchema rsn                         -> dropRemoteSchemaInMetadata rsn
  MOCustomTypes                              -> clearCustomTypesInMetadata
  MOAction action                            -> dropActionInMetadata action
  MOActionPermission action role             -> dropActionPermissionInMetadata action role
  MOCronTrigger ctName                       -> dropCronTriggerInMetadata ctName
