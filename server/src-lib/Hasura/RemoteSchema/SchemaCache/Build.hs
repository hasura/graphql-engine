{-# LANGUAGE Arrows #-}

module Hasura.RemoteSchema.SchemaCache.Build
  ( buildRemoteSchemas,
    addRemoteSchemaP2Setup,
  )
where

import Control.Arrow.Extended
import Control.Arrow.Interpret
import Control.Monad.Trans.Control
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Environment qualified as Env
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON (encJFromLBS)
import Hasura.GraphQL.RemoteServer
import Hasura.GraphQL.Schema.Common (SchemaSampledFeatureFlags)
import Hasura.Incremental qualified as Inc
import Hasura.Logging
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Cache.Permission
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Roles
import Hasura.RQL.Types.Roles.Internal (CheckPermission (..))
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache.Permission (resolveRoleBasedRemoteSchema)
import Hasura.RemoteSchema.SchemaCache.Types
import Hasura.Services
import Hasura.Tracing qualified as Tracing

-- Resolves a user specified `RemoteSchemaMetadata` into information rich `RemoteSchemaCtx`
-- However, given the nature of remote relationships, we cannot fully 'resolve' them, so
-- we resolve of remote relationships as much as possible.
buildRemoteSchemas ::
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectItem) arr,
    Inc.ArrowCache m arr,
    MonadIO m,
    MonadBaseControl IO m,
    Eq remoteRelationshipDefinition,
    ToJSON remoteRelationshipDefinition,
    MonadError QErr m,
    ProvidesNetwork m
  ) =>
  Logger Hasura ->
  Env.Environment ->
  ( (Inc.Dependency (HashMap RemoteSchemaName Inc.InvalidationKey), OrderedRoles, Maybe (HashMap RemoteSchemaName BL.ByteString), SchemaSampledFeatureFlags),
    [RemoteSchemaMetadataG remoteRelationshipDefinition]
  )
    `arr` HashMap RemoteSchemaName (PartiallyResolvedRemoteSchemaCtxG remoteRelationshipDefinition, MetadataObject)
buildRemoteSchemas logger env =
  buildInfoMapPreservingMetadata _rsmName mkRemoteSchemaMetadataObject buildRemoteSchema
  where
    -- We want to cache this call because it fetches the remote schema over
    -- HTTP, and we don’t want to re-run that if the remote schema definition
    -- hasn’t changed.
    buildRemoteSchema = Inc.cache proc ((invalidationKeys, orderedRoles, storedIntrospection, schemaSampledFeatureFlags), remoteSchema@(RemoteSchemaMetadata name defn _comment permissions relationships)) -> do
      Inc.dependOn -< Inc.selectKeyD name invalidationKeys
      let metadataObj = mkRemoteSchemaMetadataObject remoteSchema
      upstreamResponse <- bindA -< runExceptT (noopTrace $ addRemoteSchemaP2Setup name env schemaSampledFeatureFlags defn)
      remoteSchemaContextParts <-
        case upstreamResponse of
          Right upstream@(_, byteString, _) -> do
            -- Collect upstream introspection response to persist in the storage
            tellA -< pure (CollectStoredIntrospection $ RemoteSchemaIntrospectionItem name $ encJFromLBS byteString)
            returnA -< Just upstream
          Left upstreamError -> do
            -- If upstream is not available, try to lookup from stored introspection
            case (HashMap.lookup name =<< storedIntrospection) of
              Nothing ->
                -- If no stored introspection exist, re-throw the upstream exception
                (| withRecordInconsistency (throwA -< upstreamError) |) metadataObj
              Just storedRawIntrospection -> do
                processedIntrospection <-
                  bindA
                    -< runExceptT do
                      rsDef <- validateRemoteSchemaDef name env defn
                      (ir, rsi) <- stitchRemoteSchema schemaSampledFeatureFlags storedRawIntrospection rsDef
                      pure (ir, storedRawIntrospection, rsi)
                case processedIntrospection of
                  Right processed -> do
                    let inconsistencyMessage =
                          T.unwords
                            [ "remote schema " <>> name,
                              " is inconsistent because of stale remote schema introspection is used.",
                              "The remote schema couldn't be reached for a fresh introspection",
                              "because we got error: " <> qeError upstreamError
                            ]
                    -- Still record inconsistency to notify the user obout the usage of stored stale data
                    recordInconsistencies -< ((Just $ toJSON (qeInternal upstreamError), [metadataObj]), inconsistencyMessage)
                    bindA -< unLogger logger $ StoredIntrospectionLog ("Using stored introspection for remote schema " <>> name) upstreamError
                    returnA -< Just processed
                  Left _processError ->
                    -- Unable to process stored introspection, give up and re-throw upstream exception
                    (| withRecordInconsistency (throwA -< upstreamError) |) metadataObj
      case remoteSchemaContextParts of
        Nothing -> returnA -< Nothing
        Just (introspection, rawIntrospection, remoteSchemaInfo) -> do
          -- we then resolve permissions
          resolvedPermissions <- buildRemoteSchemaPermissions -< ((name, introspection, orderedRoles), fmap (name,) permissions)
          -- resolve remote relationships
          let transformedRelationships = relationships <&> \RemoteSchemaTypeRelationships {..} -> PartiallyResolvedRemoteRelationship _rstrsName <$> _rstrsRelationships
              remoteSchemaContext =
                RemoteSchemaCtx
                  { _rscName = name,
                    _rscIntroOriginal = introspection,
                    _rscInfo = remoteSchemaInfo,
                    _rscRawIntrospectionResult = rawIntrospection,
                    _rscPermissions = resolvedPermissions,
                    _rscRemoteRelationships = transformedRelationships
                  }
          returnA -< Just remoteSchemaContext

    -- TODO continue propagating MonadTrace up calls so that we can get tracing
    -- for remote schema introspection. This will require modifying CacheBuild.
    -- TODO(Antoine): do this when changing CacheBuild to be on top of the app's m.
    noopTrace = Tracing.ignoreTraceT

    mkRemoteSchemaMetadataObject remoteSchema =
      MetadataObject (MORemoteSchema (_rsmName remoteSchema)) (toJSON remoteSchema)

-- | Resolves a RemoteSchemaPermission metadata object into a 'GraphQL schema'.
buildRemoteSchemaPermissions ::
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectItem) arr,
    ArrowKleisli m arr,
    MonadError QErr m
  ) =>
  -- this ridiculous duplication of [(RemoteSchemaName, RemoteSchemaPermissionMetadata)]
  -- instead of just [RemoteSchemaName] is because buildInfoMap doesn't pass `e` to the
  -- mkMetadataObject function. However, that change is very invasive.
  ((RemoteSchemaName, IntrospectionResult, OrderedRoles), [(RemoteSchemaName, RemoteSchemaPermissionMetadata)]) `arr` HashMap.HashMap RoleName IntrospectionResult
buildRemoteSchemaPermissions = proc ((remoteSchemaName, originalIntrospection, orderedRoles), permissions) -> do
  metadataPermissionsMap <- do
    buildInfoMap (_rspmRole . snd) mkRemoteSchemaPermissionMetadataObject buildRemoteSchemaPermission
      -<
        (originalIntrospection, permissions)
  -- convert to the intermediate form `CheckPermission` whose `Semigroup`
  -- instance is used to combine permissions
  let metadataCheckPermissionsMap = CPDefined <$> metadataPermissionsMap
  allRolesUnresolvedPermissionsMap <-
    bindA
      -<
        foldM
          ( \accumulatedRolePermMap (Role roleName (ParentRoles parentRoles)) -> do
              rolePermission <- onNothing (HashMap.lookup roleName accumulatedRolePermMap) $ do
                parentRolePermissions <-
                  for (toList parentRoles) $ \role ->
                    onNothing (HashMap.lookup role accumulatedRolePermMap)
                      $ throw500
                      $ "remote schema permissions: bad ordering of roles, could not find the permission of role: "
                      <>> role
                let combinedPermission = sconcat <$> nonEmpty parentRolePermissions
                pure $ fromMaybe CPUndefined combinedPermission
              pure $ HashMap.insert roleName rolePermission accumulatedRolePermMap
          )
          metadataCheckPermissionsMap
          (_unOrderedRoles orderedRoles)
  -- traverse through `allRolesUnresolvedPermissionsMap` to record any inconsistencies (if exists)
  resolvedPermissions <-
    interpretWriter
      -< for (HashMap.toList allRolesUnresolvedPermissionsMap) \(roleName, checkPermission) -> do
        let inconsistentRoleEntity = InconsistentRemoteSchemaPermission remoteSchemaName
        resolvedCheckPermission <- resolveCheckPermission checkPermission roleName inconsistentRoleEntity
        return (roleName, resolvedCheckPermission)
  returnA -< catMaybes $ HashMap.fromList resolvedPermissions
  where
    buildRemoteSchemaPermission = proc (originalIntrospection, (remoteSchemaName, remoteSchemaPerm)) -> do
      let RemoteSchemaPermissionMetadata roleName defn _ = remoteSchemaPerm
          metadataObject = mkRemoteSchemaPermissionMetadataObject (remoteSchemaName, remoteSchemaPerm)
          schemaObject = SORemoteSchemaPermission remoteSchemaName roleName
          providedSchemaDoc = _rspdSchema defn
          addPermContext err = "in remote schema permission for role " <> roleName <<> ": " <> err
      (|
        withRecordInconsistency
          ( do
              (resolvedSchemaIntrospection, dependency) <-
                bindErrorA
                  -<
                    modifyErr addPermContext $ resolveRoleBasedRemoteSchema roleName remoteSchemaName originalIntrospection providedSchemaDoc
              recordDependencies -< (metadataObject, schemaObject, pure dependency)
              returnA -< resolvedSchemaIntrospection
          )
        |)
        metadataObject

    mkRemoteSchemaPermissionMetadataObject ::
      (RemoteSchemaName, RemoteSchemaPermissionMetadata) ->
      MetadataObject
    mkRemoteSchemaPermissionMetadataObject (rsName, (RemoteSchemaPermissionMetadata roleName defn _)) =
      let objectId = MORemoteSchemaPermissions rsName roleName
       in MetadataObject objectId $ toJSON defn

addRemoteSchemaP2Setup ::
  (QErrM m, MonadIO m, ProvidesNetwork m, Tracing.MonadTrace m) =>
  RemoteSchemaName ->
  Env.Environment ->
  SchemaSampledFeatureFlags ->
  RemoteSchemaDef ->
  m (IntrospectionResult, BL.ByteString, RemoteSchemaInfo)
addRemoteSchemaP2Setup name env schemaSampledFeatureFlags def = do
  rsi <- validateRemoteSchemaDef name env def
  fetchRemoteSchema env schemaSampledFeatureFlags rsi
