module Hasura.RQL.DDL.Schema.Cache.Dependencies
  ( resolveDependencies,
  )
where

import Control.Arrow.Extended
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.HashMap.Strict.Extended qualified as M
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as HS
import Data.List (nub)
import Data.Monoid (First)
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.DDL.Network
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

-- | Processes collected 'CIDependency' values into a 'DepMap', performing integrity checking to
-- ensure the dependencies actually exist. If a dependency is missing, its transitive dependents are
-- removed from the cache, and 'InconsistentMetadata's are returned.
resolveDependencies ::
  (ArrowKleisli m arr, QErrM m) =>
  ( BuildOutputs,
    [(MetadataObject, SchemaObjId, SchemaDependency)]
  )
    `arr` (BuildOutputs, [InconsistentMetadata], DepMap)
resolveDependencies = arrM \(cache, dependencies) -> do
  let dependencyMap =
        dependencies
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
                Just $
                  ExtraInternal $
                    object
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
  fmap (M.filter (not . null)) . traverse do
    partitionEithers . map \(metadataObject, dependency) -> case resolveDependency dependency of
      Right () -> Right (metadataObject, dependency)
      Left errorMessage -> Left (InconsistentObject errorMessage Nothing metadataObject)
  where
    resolveDependency :: SchemaDependency -> Either Text ()
    resolveDependency (SchemaDependency objectId _) = case objectId of
      SOSource source ->
        void $
          M.lookup source (_boSources cache)
            `onNothing` Left ("no such source exists: " <>> source)
      SORemoteSchema remoteSchemaName ->
        unless (remoteSchemaName `M.member` _boRemoteSchemas cache) $
          Left $ "remote schema " <> remoteSchemaName <<> " is not found"
      SORemoteSchemaPermission remoteSchemaName roleName -> do
        remoteSchema <-
          onNothing (M.lookup remoteSchemaName $ _boRemoteSchemas cache) $
            Left $ "remote schema " <> remoteSchemaName <<> " is not found"
        unless (roleName `M.member` _rscPermissions (fst remoteSchema)) $
          Left $
            "no permission defined on remote schema " <> remoteSchemaName
              <<> " for role " <>> roleName
      SORemoteSchemaRemoteRelationship remoteSchemaName typeName relationshipName -> do
        remoteSchema <-
          fmap fst $
            onNothing (M.lookup remoteSchemaName $ _boRemoteSchemas cache) $
              Left $ "remote schema " <> remoteSchemaName <<> " is not found"
        void $
          onNothing
            (OMap.lookup typeName (_rscRemoteRelationships remoteSchema) >>= OMap.lookup relationshipName)
            $ Left $
              "remote relationship " <> relationshipName
                <<> " on type " <> G.unName typeName <> " on " <> remoteSchemaName
                <<> " is not found"
      SOSourceObj source exists -> do
        AB.dispatchAnyBackend @Backend exists $ \sourceObjId -> do
          sourceInfo <- castSourceInfo source sourceObjId
          case sourceObjId of
            SOITable tableName -> do
              void $ resolveTable sourceInfo tableName
            SOIFunction functionName ->
              void $
                M.lookup functionName (_siFunctions sourceInfo)
                  `onNothing` Left ("function " <> functionName <<> " is not tracked")
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
                  unless (isJust $ find ((== constraintName) . _cName . _fkConstraint) foreignKeys) $
                    Left $
                      "no foreign key constraint named " <> constraintName <<> " is "
                        <> "defined for table " <>> tableName
                TOPerm roleName permType -> withPermType permType \accessor -> do
                  let permLens = permAccToLens accessor
                  unless (has (tiRolePermInfoMap . ix roleName . permLens . _Just) tableInfo) $
                    Left $
                      "no " <> permTypeToCode permType <> " permission defined on table "
                        <> tableName <<> " for role " <>> roleName
                TOTrigger triggerName ->
                  unless (M.member triggerName (_tiEventTriggerInfoMap tableInfo)) $
                    Left $
                      "no event trigger named " <> triggerName <<> " is defined for table " <>> tableName
      SORole roleName ->
        void $
          (M.lookup roleName (_boRoles cache))
            `onNothing` Left ("parent role " <> roleName <<> " does not exist")

    castSourceInfo ::
      (Backend b) => SourceName -> SourceObjId b -> Either Text (SourceInfo b)
    castSourceInfo sourceName _ =
      -- TODO: if the cast returns Nothing, we should be throwing an internal error
      -- the type of the dependency in sources is not as recorded
      (M.lookup sourceName (_boSources cache) >>= unsafeSourceInfo)
        `onNothing` Left ("no such source found " <>> sourceName)

    resolveTable sourceInfo tableName =
      M.lookup tableName (_siTables sourceInfo)
        `onNothing` Left ("table " <> tableName <<> " is not tracked")

    columnToFieldName :: forall b. (Backend b) => TableInfo b -> Column b -> FieldName
    columnToFieldName _ = fromCol @b

    resolveField ::
      Backend b =>
      TableInfo b ->
      FieldName ->
      Getting (First a) (FieldInfo b) a ->
      Text ->
      Either Text a
    resolveField tableInfo fieldName fieldType fieldTypeName = do
      let coreInfo = _tiCoreInfo tableInfo
          tableName = tableInfoName tableInfo
      fieldInfo <-
        M.lookup fieldName (_tciFieldInfoMap coreInfo)
          `onNothing` Left
            ("table " <> tableName <<> " has no field named " <>> fieldName)
      (fieldInfo ^? fieldType)
        `onNothing` Left
          ("field " <> fieldName <<> "of table " <> tableName <<> " is not a " <> fieldTypeName)

deleteMetadataObject ::
  MetadataObjId -> BuildOutputs -> BuildOutputs
deleteMetadataObject = \case
  MOSource name -> boSources %~ M.delete name
  MOSourceObjId source exists -> AB.dispatchAnyBackend @Backend exists (\sourceObjId -> boSources %~ M.adjust (deleteObjId sourceObjId) source)
  MORemoteSchema name -> boRemoteSchemas %~ M.delete name
  MORemoteSchemaPermissions name role -> boRemoteSchemas . ix name . _1 . rscPermissions %~ M.delete role
  MORemoteSchemaRemoteRelationship remoteSchema typeName relationshipName ->
    boRemoteSchemas . ix remoteSchema . _1 . rscRemoteRelationships . ix typeName %~ OMap.delete relationshipName
  MOCronTrigger name -> boCronTriggers %~ M.delete name
  MOCustomTypes -> boCustomTypes %~ const emptyAnnotatedCustomTypes
  MOAction name -> boActions %~ M.delete name
  MOEndpoint name -> boEndpoints %~ M.delete name
  MOActionPermission name role -> boActions . ix name . aiPermissions %~ M.delete role
  MOInheritedRole name -> boRoles %~ M.delete name
  MOHostTlsAllowlist host -> removeHostFromAllowList host
  MOQueryCollectionsQuery cName lq -> \bo@BuildOutputs {..} ->
    bo
      { _boEndpoints = removeEndpointsUsingQueryCollection lq _boEndpoints,
        _boAllowlist = removeFromAllowList lq _boAllowlist,
        _boQueryCollections = removeFromQueryCollections cName lq _boQueryCollections
      }
  where
    removeHostFromAllowList hst bo =
      bo
        { _boTlsAllowlist = filter (not . checkForHostnameInAllowlistObject hst) (_boTlsAllowlist bo)
        }

    deleteObjId :: forall b. (Backend b) => SourceMetadataObjId b -> BackendSourceInfo -> BackendSourceInfo
    deleteObjId sourceObjId sourceInfo =
      maybe
        sourceInfo
        (AB.mkAnyBackend . deleteObjFn sourceObjId)
        $ unsafeSourceInfo sourceInfo

    deleteObjFn :: (Backend b) => SourceMetadataObjId b -> SourceInfo b -> SourceInfo b
    deleteObjFn = \case
      SMOTable name -> siTables %~ M.delete name
      SMOFunction name -> siFunctions %~ M.delete name
      SMOFunctionPermission functionName role ->
        siFunctions . ix functionName . fiPermissions %~ M.delete role
      SMOTableObj tableName tableObjectId ->
        siTables . ix tableName %~ case tableObjectId of
          MTORel name _ -> tiCoreInfo . tciFieldInfoMap %~ M.delete (fromRel name)
          MTOComputedField name -> tiCoreInfo . tciFieldInfoMap %~ M.delete (fromComputedField name)
          MTORemoteRelationship name -> tiCoreInfo . tciFieldInfoMap %~ M.delete (fromRemoteRelationship name)
          MTOTrigger name -> tiEventTriggerInfoMap %~ M.delete name
          MTOPerm roleName permType -> withPermType permType \accessor ->
            tiRolePermInfoMap . ix roleName . permAccToLens accessor .~ Nothing

    removeFromQueryCollections :: CollectionName -> ListedQuery -> QueryCollections -> QueryCollections
    removeFromQueryCollections cName lq qc =
      let collectionModifier :: CreateCollection -> CreateCollection
          collectionModifier cc@CreateCollection {..} =
            cc
              { _ccDefinition =
                  let oldQueries = _cdQueries _ccDefinition
                   in _ccDefinition
                        { _cdQueries = filter (/= lq) oldQueries
                        }
              }
       in OMap.adjust collectionModifier cName qc

    removeEndpointsUsingQueryCollection :: ListedQuery -> HashMap EndpointName (EndpointMetadata GQLQueryWithText) -> HashMap EndpointName (EndpointMetadata GQLQueryWithText)
    removeEndpointsUsingQueryCollection lq endpointMap =
      case maybeEndpoint of
        Just (n, _) -> M.delete n endpointMap
        Nothing -> endpointMap
      where
        q = _lqQuery lq
        maybeEndpoint = find (\(_, def) -> (_edQuery . _ceDefinition) def == q) (M.toList endpointMap)

    removeFromAllowList :: ListedQuery -> InlinedAllowlist -> InlinedAllowlist
    removeFromAllowList lq aList =
      let oldAList = iaGlobal aList
          gqlQry = NormalizedQuery . unGQLQuery . getGQLQuery . _lqQuery $ lq
          newAList = HS.delete gqlQry oldAList
       in aList
            { iaGlobal = newAList
            }
