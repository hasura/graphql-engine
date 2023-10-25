module Hasura.RQL.DDL.Schema.Cache.Dependencies
  ( resolveDependencies,
  )
where

import Control.Arrow.Extended
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as HS
import Data.List (nub)
import Data.Monoid (First)
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.Function.Lenses (fiPermissions)
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.Lenses (lmiPermissions)
import Hasura.LogicalModel.Types (LogicalModelLocation (..))
import Hasura.NativeQuery.Cache (NativeQueryInfo (_nqiReturns))
import Hasura.NativeQuery.Lenses (nqiRelationships)
import Hasura.Prelude
import Hasura.RQL.DDL.Permission.Internal (permissionIsDefined)
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RemoteSchema.SchemaCache (rscPermissions, rscRemoteRelationships)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.StoredProcedure.Cache (StoredProcedureInfo (_spiReturns))
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

-- | Processes collected 'CIDependency' values into a 'DepMap', performing integrity checking to
-- ensure the dependencies actually exist. If a dependency is missing, its transitive dependents are
-- removed from the cache, and 'InconsistentMetadata's are returned.
resolveDependencies ::
  (ArrowKleisli m arr, QErrM m) =>
  ( BuildOutputs,
    [MetadataDependency]
  )
    `arr` (BuildOutputs, [InconsistentMetadata], DepMap)
resolveDependencies = arrM \(cache, dependencies) -> do
  let dependencyMap =
        dependencies
          & HashMap.groupOn (\case MetadataDependency _ schemaObjId _ -> schemaObjId)
          & fmap (map \case MetadataDependency metadataObject _ schemaDependency -> (metadataObject, schemaDependency))
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
performIteration ::
  (QErrM m) =>
  Int ->
  BuildOutputs ->
  [InconsistentMetadata] ->
  HashMap SchemaObjId [(MetadataObject, SchemaDependency)] ->
  m (BuildOutputs, [InconsistentMetadata], DepMap)
performIteration iterationNumber cache inconsistencies dependencies = do
  let (newInconsistencies, prunedDependencies) = pruneDanglingDependents cache dependencies
  case newInconsistencies of
    [] -> pure (cache, inconsistencies, HS.fromList . map snd <$> prunedDependencies)
    _
      | iterationNumber < 100 -> do
          let inconsistentIds = nub $ concatMap imObjectIds newInconsistencies
              prunedCache = foldl' (flip deleteMetadataObject) cache inconsistentIds
              allInconsistencies = inconsistencies <> newInconsistencies
          performIteration (iterationNumber + 1) prunedCache allInconsistencies prunedDependencies
      | otherwise ->
          -- Running for 100 iterations without terminating is (hopefully) enormously unlikely
          -- unless we did something very wrong, so halt the process and abort with some
          -- debugging information.
          throwError
            (err500 Unexpected "schema dependency resolution failed to terminate")
              { qeInternal =
                  Just
                    $ ExtraInternal
                    $ object
                      [ "inconsistent_objects"
                          .= object
                            [ "old" .= inconsistencies,
                              "new" .= newInconsistencies
                            ],
                        "pruned_dependencies" .= (map snd <$> prunedDependencies)
                      ]
              }

pruneDanglingDependents ::
  BuildOutputs ->
  HashMap SchemaObjId [(MetadataObject, SchemaDependency)] ->
  ([InconsistentMetadata], HashMap SchemaObjId [(MetadataObject, SchemaDependency)])
pruneDanglingDependents cache =
  fmap (HashMap.filter (not . null)) . traverse do
    partitionEithers . map \(metadataObject, dependency) -> case resolveDependency dependency of
      Right () -> Right (metadataObject, dependency)
      Left errorMessage -> Left (InconsistentObject errorMessage Nothing metadataObject)
  where
    resolveDependency :: SchemaDependency -> Either Text ()
    resolveDependency (SchemaDependency objectId _) = case objectId of
      SOSource source ->
        void
          $ HashMap.lookup source (_boSources cache)
          `onNothing` Left ("no such source exists: " <>> source)
      SORemoteSchema remoteSchemaName ->
        unless (remoteSchemaName `HashMap.member` _boRemoteSchemas cache)
          $ Left
          $ "remote schema "
          <> remoteSchemaName
          <<> " is not found"
      SORemoteSchemaPermission remoteSchemaName roleName -> do
        remoteSchema <-
          onNothing (HashMap.lookup remoteSchemaName $ _boRemoteSchemas cache)
            $ Left
            $ "remote schema "
            <> remoteSchemaName
            <<> " is not found"
        unless (roleName `HashMap.member` _rscPermissions (fst remoteSchema))
          $ Left
          $ "no permission defined on remote schema "
          <> remoteSchemaName
          <<> " for role "
          <>> roleName
      SORemoteSchemaRemoteRelationship remoteSchemaName typeName relationshipName -> do
        remoteSchema <-
          fmap fst
            $ onNothing (HashMap.lookup remoteSchemaName $ _boRemoteSchemas cache)
            $ Left
            $ "remote schema "
            <> remoteSchemaName
            <<> " is not found"
        void
          $ onNothing
            (InsOrdHashMap.lookup typeName (_rscRemoteRelationships remoteSchema) >>= InsOrdHashMap.lookup relationshipName)
          $ Left
          $ "remote relationship "
          <> relationshipName
          <<> " on type "
          <> G.unName typeName
          <> " on "
          <> remoteSchemaName
          <<> " is not found"
      SOSourceObj source exists -> do
        AB.dispatchAnyBackend @Backend exists $ \(sourceObjId :: SourceObjId b) -> do
          sourceInfo <- castSourceInfo source sourceObjId
          case sourceObjId of
            SOITable tableName ->
              void $ resolveTable sourceInfo tableName
            SOIFunction functionName ->
              void
                $ HashMap.lookup functionName (_siFunctions sourceInfo)
                `onNothing` Left ("function " <> functionName <<> " is not tracked")
            SOILogicalModel logicalModelName ->
              void $ resolveLogicalModel sourceInfo logicalModelName
            SOILogicalModelObj logicalModelLocation logicalModelObjId -> do
              (logicalModelDesc, logicalModel) <- case logicalModelLocation of
                LMLLogicalModel logicalModelName ->
                  (,) ("logical model " <>> logicalModelName) <$> resolveLogicalModel sourceInfo logicalModelName
                LMLNativeQuery nativeQueryName ->
                  (,) ("logical model for native query " <>> nativeQueryName)
                    <$> resolveLogicalModelInNativeQuery sourceInfo nativeQueryName
              case logicalModelObjId of
                LMOReferencedLogicalModel inner ->
                  void $ resolveLogicalModel sourceInfo inner
                LMOPerm roleName permType -> do
                  let rolePermissions :: Maybe (RolePermInfo b)
                      rolePermissions = logicalModel ^? lmiPermissions . ix roleName

                  unless (any (permissionIsDefined permType) rolePermissions)
                    $ Left
                    $ "no "
                    <> permTypeToCode permType
                    <> " permission defined on "
                    <> logicalModelDesc
                    <<> " for role "
                    <>> roleName
                LMOCol column ->
                  unless (InsOrdHashMap.member column (_lmiFields logicalModel)) do
                    Left ("Could not find column " <> column <<> " in " <>> logicalModelDesc)
            SOINativeQuery nativeQueryName -> do
              void $ resolveNativeQuery sourceInfo nativeQueryName
            SOINativeQueryObj nativeQueryName nativeQueryObjId -> do
              nativeQueryInfo <- resolveNativeQuery sourceInfo nativeQueryName
              case nativeQueryObjId of
                NQOCol colName ->
                  unless (InsOrdHashMap.member colName (_lmiFields (_nqiReturns nativeQueryInfo)))
                    $ Left
                      ("native query " <> nativeQueryName <<> " has no field named " <>> colName)
            SOIStoredProcedure storedProcedureName -> do
              void $ resolveStoredProcedure sourceInfo storedProcedureName
            SOIStoredProcedureObj storedProcedureName storedProcedureObjId -> do
              storedProcedureInfo <- resolveStoredProcedure sourceInfo storedProcedureName
              case storedProcedureObjId of
                SPOCol colName ->
                  unless (InsOrdHashMap.member colName (_lmiFields (_spiReturns storedProcedureInfo)))
                    $ Left
                      ("stored procedure " <> storedProcedureName <<> " has no field named " <>> colName)
            SOITableObj tableName tableObjectId -> do
              tableInfo <- resolveTable sourceInfo tableName
              case tableObjectId of
                TOCol columnName ->
                  void $ resolveField tableInfo (columnToFieldName tableInfo columnName) _FIColumn "column"
                TORel relName ->
                  void $ resolveField tableInfo (fromRel relName) _FIRelationship "relationship"
                TOComputedField fieldName ->
                  void $ resolveField tableInfo (fromComputedField fieldName) _FIComputedField "computed field"
                TORemoteRel fieldName ->
                  void $ resolveField tableInfo (fromRemoteRelationship fieldName) _FIRemoteRelationship "remote relationship"
                TOForeignKey constraintName -> do
                  let foreignKeys = _tciForeignKeys $ _tiCoreInfo tableInfo
                  unless (isJust $ find ((== constraintName) . _cName . _fkConstraint) foreignKeys)
                    $ Left
                    $ "no foreign key constraint named "
                    <> constraintName
                    <<> " is "
                    <> "defined for table "
                    <>> tableName
                TOPerm roleName permType -> do
                  unless (any (permissionIsDefined permType) (tableInfo ^? (tiRolePermInfoMap . ix roleName)))
                    $ Left
                    $ "no "
                    <> permTypeToCode permType
                    <> " permission defined on table "
                    <> tableName
                    <<> " for role "
                    <>> roleName
                TOTrigger triggerName ->
                  unless (HashMap.member triggerName (_tiEventTriggerInfoMap tableInfo))
                    $ Left
                    $ "no event trigger named "
                    <> triggerName
                    <<> " is defined for table "
                    <>> tableName
      SORole roleName ->
        void
          $ (HashMap.lookup roleName (_boRoles cache))
          `onNothing` Left ("parent role " <> roleName <<> " does not exist")

    castSourceInfo ::
      (Backend b) => SourceName -> SourceObjId b -> Either Text (SourceInfo b)
    castSourceInfo sourceName _ =
      -- TODO: if the cast returns Nothing, we should be throwing an internal error
      -- the type of the dependency in sources is not as recorded
      (HashMap.lookup sourceName (_boSources cache) >>= unsafeSourceInfo)
        `onNothing` Left ("no such source found " <>> sourceName)

    resolveTable sourceInfo tableName =
      HashMap.lookup tableName (_siTables sourceInfo)
        `onNothing` Left ("table " <> tableName <<> " is not tracked")

    resolveNativeQuery sourceInfo nativeQueryName =
      HashMap.lookup nativeQueryName (_siNativeQueries sourceInfo)
        `onNothing` Left ("native query " <> nativeQueryName <<> " is not tracked")

    resolveStoredProcedure sourceInfo storedProcedureName =
      HashMap.lookup storedProcedureName (_siStoredProcedures sourceInfo)
        `onNothing` Left ("stored procedure " <> storedProcedureName <<> " is not tracked")

    resolveLogicalModel sourceInfo logicalModelName =
      HashMap.lookup logicalModelName (_siLogicalModels sourceInfo)
        `onNothing` Left ("logical model " <> logicalModelName <<> " is not tracked")

    resolveLogicalModelInNativeQuery sourceInfo nativeQueryName =
      resolveNativeQuery sourceInfo nativeQueryName <&> \nq -> _nqiReturns nq

    columnToFieldName :: forall b. (Backend b) => TableInfo b -> Column b -> FieldName
    columnToFieldName _ = fromCol @b

    resolveField ::
      (Backend b) =>
      TableInfo b ->
      FieldName ->
      Getting (First a) (FieldInfo b) a ->
      Text ->
      Either Text a
    resolveField tableInfo fieldName fieldType fieldTypeName = do
      let coreInfo = _tiCoreInfo tableInfo
          tableName = tableInfoName tableInfo
      fieldInfo <-
        HashMap.lookup fieldName (_tciFieldInfoMap coreInfo)
          `onNothing` Left
            ("table " <> tableName <<> " has no field named " <>> fieldName)
      (fieldInfo ^? fieldType)
        `onNothing` Left
          ("field " <> fieldName <<> "of table " <> tableName <<> " is not a " <> fieldTypeName)

deleteMetadataObject ::
  MetadataObjId -> BuildOutputs -> BuildOutputs
deleteMetadataObject = \case
  -- The objective here is to delete components of `BuildOutputs` that could
  -- freshly become inconsistent, due to it requiring another component of
  -- `BuildOutputs` that doesn't exist (e.g. because it has
  -- become inconsistent in a previous round of `performIteration`).
  MOSource name -> boSources %~ HashMap.delete name
  MOSourceObjId source exists -> AB.dispatchAnyBackend @Backend exists (\sourceObjId -> boSources %~ HashMap.adjust (deleteObjId sourceObjId) source)
  MORemoteSchema name -> boRemoteSchemas %~ HashMap.delete name
  MORemoteSchemaPermissions name role -> boRemoteSchemas . ix name . _1 . rscPermissions %~ HashMap.delete role
  MORemoteSchemaRemoteRelationship remoteSchema typeName relationshipName ->
    boRemoteSchemas . ix remoteSchema . _1 . rscRemoteRelationships . ix typeName %~ InsOrdHashMap.delete relationshipName
  MOCustomTypes -> boCustomTypes %~ const mempty
  MOAction name -> boActions %~ HashMap.delete name
  MOActionPermission name role -> boActions . ix name . aiPermissions %~ HashMap.delete role
  MOInheritedRole name -> boRoles %~ HashMap.delete name
  MODataConnectorAgent agentName ->
    boBackendCache
      %~ (BackendMap.modify @'DataConnector $ BackendInfoWrapper . HashMap.delete agentName . unBackendInfoWrapper)
  -- These parts of Metadata never become inconsistent as a result of
  -- inconsistencies elsewhere, i.e. they don't have metadata dependencies.  So
  -- we never need to prune them, and in fact don't even bother storing them in
  -- `BuildOutputs`.  For instance, Cron Triggers are an isolated feature that
  -- don't depend on e.g. DB sources, so their consistency is not dependent on
  -- the consistency of DB sources.
  --
  -- See also Note [Avoiding GraphQL schema rebuilds when changing irrelevant Metadata]
  MOCronTrigger _ -> id
  MOEndpoint _ -> id
  MOQueryCollectionsQuery _ _ -> id
  MOOpenTelemetry _ -> id
  where
    deleteObjId :: forall b. (Backend b) => SourceMetadataObjId b -> BackendSourceInfo -> BackendSourceInfo
    deleteObjId sourceObjId sourceInfo =
      maybe
        sourceInfo
        (AB.mkAnyBackend . deleteObjFn sourceObjId)
        $ unsafeSourceInfo sourceInfo

    deleteObjFn :: forall b. (Backend b) => SourceMetadataObjId b -> SourceInfo b -> SourceInfo b
    deleteObjFn = \case
      SMOTable name -> siTables %~ HashMap.delete name
      SMOFunction name -> siFunctions %~ HashMap.delete name
      SMOFunctionPermission functionName role ->
        siFunctions . ix functionName . fiPermissions %~ HashMap.delete role
      SMONativeQuery name -> siNativeQueries %~ HashMap.delete name
      SMONativeQueryObj nativeQueryName nativeQueryObjId ->
        siNativeQueries . ix nativeQueryName %~ case nativeQueryObjId of
          NQMORel name _ -> nqiRelationships %~ InsOrdHashMap.delete name
      SMOStoredProcedure name -> siStoredProcedures %~ HashMap.delete name
      SMOLogicalModel name ->
        -- TODO: if I'm inconsistent, delete everything that depends on me
        siLogicalModels %~ HashMap.delete name
      SMOLogicalModelObj (LMLLogicalModel logicalModelName) logicalModelObjectId ->
        siLogicalModels . ix logicalModelName %~ case logicalModelObjectId of
          LMMOPerm roleName PTSelect -> lmiPermissions . ix roleName . permSel .~ Nothing
          LMMOPerm roleName PTInsert -> lmiPermissions . ix roleName . permIns .~ Nothing
          LMMOPerm roleName PTUpdate -> lmiPermissions . ix roleName . permUpd .~ Nothing
          LMMOPerm roleName PTDelete -> lmiPermissions . ix roleName . permDel .~ Nothing
          LMMOReferencedLogicalModel _ -> id
      SMOLogicalModelObj (LMLNativeQuery _nativeQueryName) _logicalModelObjectId -> error "copy above pls"
      SMOTableObj tableName tableObjectId ->
        siTables . ix tableName %~ case tableObjectId of
          MTORel name _ -> tiCoreInfo . tciFieldInfoMap %~ HashMap.delete (fromRel name)
          MTOComputedField name -> tiCoreInfo . tciFieldInfoMap %~ HashMap.delete (fromComputedField name)
          MTORemoteRelationship name -> tiCoreInfo . tciFieldInfoMap %~ HashMap.delete (fromRemoteRelationship name)
          MTOTrigger name -> tiEventTriggerInfoMap %~ HashMap.delete name
          MTOPerm roleName PTSelect -> tiRolePermInfoMap . ix roleName . permSel .~ Nothing
          MTOPerm roleName PTInsert -> tiRolePermInfoMap . ix roleName . permIns .~ Nothing
          MTOPerm roleName PTUpdate -> tiRolePermInfoMap . ix roleName . permUpd .~ Nothing
          MTOPerm roleName PTDelete -> tiRolePermInfoMap . ix roleName . permDel .~ Nothing
