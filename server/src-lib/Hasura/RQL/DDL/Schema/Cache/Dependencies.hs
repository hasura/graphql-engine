module Hasura.RQL.DDL.Schema.Cache.Dependencies
  ( resolveDependencies
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended       as M
import qualified Data.HashSet                       as HS

import           Control.Arrow.Extended
import           Control.Lens                       hiding ((.=))
import           Data.Aeson
import           Data.List                          (nub)
import           Data.Monoid                        (First)

import           Data.Text.Extended
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.Types

-- | Processes collected 'CIDependency' values into a 'DepMap', performing integrity checking to
-- ensure the dependencies actually exist. If a dependency is missing, its transitive dependents are
-- removed from the cache, and 'InconsistentMetadata's are returned.
resolveDependencies
  :: (ArrowKleisli m arr, QErrM m)
  => ( BuildOutputs 'Postgres
     , [(MetadataObject, SchemaObjId, SchemaDependency)]
     ) `arr` (BuildOutputs 'Postgres, [InconsistentMetadata], DepMap)
resolveDependencies = arrM \(cache, dependencies) -> do
  let dependencyMap = dependencies
        & M.groupOn (view _2)
        & fmap (map \(metadataObject, _, schemaDependency) -> (metadataObject, schemaDependency))
  performIteration 0 cache [] dependencyMap

-- Processes dependencies using an iterative process that alternates between two steps:
--
--   1. First, pruneDanglingDependents searches for any dependencies that do not exist in the
--      current cache and removes their dependents from the dependency map, returning an
--      InconsistentMetadata for each dependent that was removed. This step does not change
--      the schema cache in any way.
--
--   2. Second, deleteMetadataObject drops the pruned dependent objects from the cache. It does
--      not alter (or consult) the dependency map, so transitive dependents are /not/ removed.
--
-- By iterating the above process until pruneDanglingDependents does not discover any new
-- inconsistencies, all missing dependencies will eventually be removed, and since dependency
-- graphs between schema objects are unlikely to be very deep, it will usually terminate in just
-- a few iterations.
performIteration
  :: (QErrM m)
  => Int
  -> BuildOutputs 'Postgres
  -> [InconsistentMetadata]
  -> HashMap SchemaObjId [(MetadataObject, SchemaDependency)]
  -> m (BuildOutputs 'Postgres, [InconsistentMetadata], DepMap)
performIteration iterationNumber cache inconsistencies dependencies = do
  let (newInconsistencies, prunedDependencies) = pruneDanglingDependents cache dependencies
  case newInconsistencies of
    [] -> pure (cache, inconsistencies, HS.fromList . map snd <$> prunedDependencies)
    _ | iterationNumber < 100 -> do
          let inconsistentIds = nub $ concatMap imObjectIds newInconsistencies
              prunedCache = foldl' (flip deleteMetadataObject) cache inconsistentIds
              allInconsistencies = inconsistencies <> newInconsistencies
          performIteration (iterationNumber + 1) prunedCache allInconsistencies prunedDependencies
      | otherwise ->
          -- Running for 100 iterations without terminating is (hopefully) enormously unlikely
          -- unless we did something very wrong, so halt the process and abort with some
          -- debugging information.
          throwError (err500 Unexpected "schema dependency resolution failed to terminate")
            { qeInternal = Just $ object
                [ "inconsistent_objects" .= object
                  [ "old" .= inconsistencies
                  , "new" .= newInconsistencies ]
                , "pruned_dependencies" .= (map snd <$> prunedDependencies) ] }

pruneDanglingDependents
  :: BuildOutputs 'Postgres
  -> HashMap SchemaObjId [(MetadataObject, SchemaDependency)]
  -> ([InconsistentMetadata], HashMap SchemaObjId [(MetadataObject, SchemaDependency)])
pruneDanglingDependents cache = fmap (M.filter (not . null)) . traverse do
  partitionEithers . map \(metadataObject, dependency) -> case resolveDependency dependency of
    Right ()          -> Right (metadataObject, dependency)
    Left errorMessage -> Left (InconsistentObject errorMessage metadataObject)
  where
    resolveDependency :: SchemaDependency -> Either Text ()
    resolveDependency (SchemaDependency objectId _) = case objectId of
      SOSource source -> void $ M.lookup source (_boSources cache)
        `onNothing` Left ("no such source exists: " <>> source)
      SOSourceObj source sourceObjId -> case sourceObjId of
        SOITable tableName -> void $ resolveTable source tableName
        SOIFunction functionName -> void $
          unsafeFunctionInfo @'Postgres source functionName (_boSources cache)
          `onNothing` Left ("function " <> functionName <<> " is not tracked")
        SOITableObj tableName tableObjectId -> do
          tableInfo <- resolveTable source tableName
          case tableObjectId of
            TOCol columnName ->
              void $ resolveField tableInfo (fromCol @'Postgres columnName) _FIColumn "column"
            TORel relName ->
              void $ resolveField tableInfo (fromRel relName) _FIRelationship "relationship"
            TOComputedField fieldName ->
              void $ resolveField tableInfo (fromComputedField fieldName) _FIComputedField "computed field"
            TORemoteRel fieldName ->
              void $ resolveField tableInfo (fromRemoteRelationship fieldName) _FIRemoteRelationship "remote relationship"
            TOForeignKey constraintName -> do
              let foreignKeys = _tciForeignKeys $ _tiCoreInfo tableInfo
              unless (isJust $ find ((== constraintName) . _cName . _fkConstraint) foreignKeys) $
                Left $ "no foreign key constraint named " <> constraintName <<> " is "
                  <> "defined for table " <>> tableName
            TOPerm roleName permType -> withPermType permType \accessor -> do
              let permLens = permAccToLens accessor
              unless (has (tiRolePermInfoMap.ix roleName.permLens._Just) tableInfo) $
                Left $ "no " <> permTypeToCode permType <> " permission defined on table "
                  <> tableName <<> " for role " <>> roleName
            TOTrigger triggerName ->
              unless (M.member triggerName (_tiEventTriggerInfoMap tableInfo)) $ Left $
                "no event trigger named " <> triggerName <<> " is defined for table " <>> tableName
      SORemoteSchema remoteSchemaName -> unless (remoteSchemaName `M.member` _boRemoteSchemas cache) $
        Left $ "remote schema " <> remoteSchemaName <<> " is not found"
      SORemoteSchemaPermission remoteSchemaName roleName -> do
        remoteSchema <-
          onNothing (M.lookup remoteSchemaName $ _boRemoteSchemas cache)
            $ Left $ "remote schema " <> remoteSchemaName <<> " is not found"
        unless (roleName `M.member` _rscPermissions (fst remoteSchema)) $
          Left $ "no permission defined on remote schema " <> remoteSchemaName
                  <<> " for role " <>> roleName

    resolveTable source tableName =
      unsafeTableInfo source tableName (_boSources cache)
      `onNothing` Left ("table " <> tableName <<> " is not tracked")

    resolveField :: TableInfo 'Postgres -> FieldName -> Getting (First a) (FieldInfo 'Postgres) a -> Text -> Either Text a
    resolveField tableInfo fieldName fieldType fieldTypeName = do
      let coreInfo = _tiCoreInfo tableInfo
          tableName = _tciName coreInfo
      fieldInfo <- M.lookup fieldName (_tciFieldInfoMap coreInfo) `onNothing` Left
        ("table " <> tableName <<> " has no field named " <>> fieldName)
      (fieldInfo ^? fieldType) `onNothing` Left
        ("field " <> fieldName <<> "of table " <> tableName <<> " is not a " <> fieldTypeName)

deleteMetadataObject
  :: MetadataObjId -> BuildOutputs 'Postgres -> BuildOutputs 'Postgres
deleteMetadataObject = \case
  MOSource name                       -> boSources %~ M.delete name
  MOSourceObjId source sourceObjId    -> boSources %~ M.adjust (deleteObjId sourceObjId) source
  MORemoteSchema name                 -> boRemoteSchemas %~ M.delete name
  MORemoteSchemaPermissions name role -> boRemoteSchemas.ix name._1.rscPermissions %~ M.delete role
  MOCronTrigger name                  -> boCronTriggers %~ M.delete name
  MOCustomTypes                       -> boCustomTypes %~ const emptyAnnotatedCustomTypes
  MOAction name                       -> boActions %~ M.delete name
  MOEndpoint name                     -> boEndpoints %~ M.delete name
  MOActionPermission name role        -> boActions.ix name.aiPermissions %~ M.delete role
  where
    deleteObjId :: SourceMetadataObjId -> BackendSourceInfo -> BackendSourceInfo
    deleteObjId sourceObjId sourceInfo = maybe sourceInfo (BackendSourceInfo . deleteObjFn sourceObjId) $ unsafeSourceInfo sourceInfo
    deleteObjFn :: SourceMetadataObjId -> SourceInfo 'Postgres -> SourceInfo 'Postgres
    deleteObjFn = \case
      SMOTable    name -> siTables    %~ M.delete name
      SMOFunction name -> siFunctions %~ M.delete name
      SMOFunctionPermission functionName role ->
        siFunctions.ix functionName.fiPermissions %~ HS.delete role
      SMOTableObj tableName tableObjectId -> siTables.ix tableName %~ case tableObjectId of
        MTORel name _              -> tiCoreInfo.tciFieldInfoMap %~ M.delete (fromRel name)
        MTOComputedField name      -> tiCoreInfo.tciFieldInfoMap %~ M.delete (fromComputedField name)
        MTORemoteRelationship name -> tiCoreInfo.tciFieldInfoMap %~ M.delete (fromRemoteRelationship name)
        MTOTrigger  name           -> tiEventTriggerInfoMap %~ M.delete name
        MTOPerm roleName permType  -> withPermType permType \accessor ->
          tiRolePermInfoMap.ix roleName.permAccToLens accessor .~ Nothing
