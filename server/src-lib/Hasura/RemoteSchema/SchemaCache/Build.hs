{-# LANGUAGE Arrows #-}

module Hasura.RemoteSchema.SchemaCache.Build
  ( buildRemoteSchemas,
    addRemoteSchemaP2Setup,
  )
where

import Control.Arrow.Extended
import Control.Arrow.Interpret
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Environment qualified as Env
import Data.HashMap.Strict.Extended qualified as M
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.GraphQL.RemoteServer (fetchRemoteSchema)
import Hasura.Incremental qualified as Inc
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
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client.Manager (HasHttpManagerM (..))

-- Resolves a user specified `RemoteSchemaMetadata` into information rich `RemoteSchemaCtx`
-- However, given the nature of remote relationships, we cannot fully 'resolve' them, so
-- we resolve of remote relationships as much as possible.
buildRemoteSchemas ::
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectedInfo) arr,
    Inc.ArrowCache m arr,
    MonadIO m,
    HasHttpManagerM m,
    Inc.Cacheable remoteRelationshipDefinition,
    ToJSON remoteRelationshipDefinition,
    MonadError QErr m
  ) =>
  Env.Environment ->
  ( (Inc.Dependency (HashMap RemoteSchemaName Inc.InvalidationKey), OrderedRoles),
    [RemoteSchemaMetadataG remoteRelationshipDefinition]
  )
    `arr` HashMap RemoteSchemaName (PartiallyResolvedRemoteSchemaCtxG remoteRelationshipDefinition, MetadataObject)
buildRemoteSchemas env =
  buildInfoMapPreservingMetadata _rsmName mkRemoteSchemaMetadataObject buildRemoteSchema
  where
    -- We want to cache this call because it fetches the remote schema over
    -- HTTP, and we don’t want to re-run that if the remote schema definition
    -- hasn’t changed.
    buildRemoteSchema = Inc.cache proc ((invalidationKeys, orderedRoles), remoteSchema@(RemoteSchemaMetadata name defn _comment permissions relationships)) -> do
      Inc.dependOn -< Inc.selectKeyD name invalidationKeys
      remoteSchemaContextParts <-
        (|
          withRecordInconsistency
            ( liftEitherA <<< bindA
                -<
                  runExceptT $ noopTrace $ addRemoteSchemaP2Setup env name defn
            )
          |) (mkRemoteSchemaMetadataObject remoteSchema)
      case remoteSchemaContextParts of
        Nothing -> returnA -< Nothing
        Just (introspection, rawIntrospection, remoteSchemaInfo) -> do
          -- we then resolve permissions
          resolvedPermissions <- buildRemoteSchemaPermissions -< ((name, introspection, orderedRoles), fmap (name,) permissions)
          -- resolve remote relationships
          let transformedRelationships = flip fmap relationships $ \RemoteSchemaTypeRelationships {..} -> fmap (PartiallyResolvedRemoteRelationship _rstrsName) _rstrsRelationships
          let remoteSchemaContext =
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
    noopTrace = Tracing.runTraceTWithReporter Tracing.noReporter "buildSchemaCacheRule"

    mkRemoteSchemaMetadataObject remoteSchema =
      MetadataObject (MORemoteSchema (_rsmName remoteSchema)) (toJSON remoteSchema)

-- | Resolves a RemoteSchemaPermission metadata object into a 'GraphQL schema'.
buildRemoteSchemaPermissions ::
  ( ArrowChoice arr,
    Inc.ArrowDistribute arr,
    ArrowWriter (Seq CollectedInfo) arr,
    Inc.ArrowCache m arr,
    MonadError QErr m
  ) =>
  -- this ridiculous duplication of [(RemoteSchemaName, RemoteSchemaPermissionMetadata)]
  -- instead of just [RemoteSchemaName] is because buildInfoMap doesn't pass `e` to the
  -- mkMetadataObject function. However, that change is very invasive.
  ((RemoteSchemaName, IntrospectionResult, OrderedRoles), [(RemoteSchemaName, RemoteSchemaPermissionMetadata)]) `arr` M.HashMap RoleName IntrospectionResult
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
              rolePermission <- onNothing (M.lookup roleName accumulatedRolePermMap) $ do
                parentRolePermissions <-
                  for (toList parentRoles) $ \role ->
                    onNothing (M.lookup role accumulatedRolePermMap) $
                      throw500 $
                        "remote schema permissions: bad ordering of roles, could not find the permission of role: " <>> role
                let combinedPermission = sconcat <$> nonEmpty parentRolePermissions
                pure $ fromMaybe CPUndefined combinedPermission
              pure $ M.insert roleName rolePermission accumulatedRolePermMap
          )
          metadataCheckPermissionsMap
          (_unOrderedRoles orderedRoles)
  -- traverse through `allRolesUnresolvedPermissionsMap` to record any inconsistencies (if exists)
  resolvedPermissions <-
    (|
      traverseA
        ( \(roleName, checkPermission) -> do
            let inconsistentRoleEntity = InconsistentRemoteSchemaPermission remoteSchemaName
            resolvedCheckPermission <- interpretWriter -< resolveCheckPermission checkPermission roleName inconsistentRoleEntity
            returnA -< (roleName, resolvedCheckPermission)
        )
      |) (M.toList allRolesUnresolvedPermissionsMap)
  returnA -< catMaybes $ M.fromList resolvedPermissions
  where
    buildRemoteSchemaPermission = proc (originalIntrospection, (remoteSchemaName, remoteSchemaPerm)) -> do
      let RemoteSchemaPermissionMetadata roleName defn _ = remoteSchemaPerm
          metadataObject = mkRemoteSchemaPermissionMetadataObject (remoteSchemaName, remoteSchemaPerm)
          schemaObject = SORemoteSchemaPermission remoteSchemaName roleName
          providedSchemaDoc = _rspdSchema defn
          addPermContext err = "in remote schema permission for role " <> roleName <<> ": " <> err
      (|
        withRecordInconsistency
          ( (|
              modifyErrA
                ( do
                    (resolvedSchemaIntrospection, dependencies) <-
                      liftEitherA <<< bindA
                        -<
                          runExceptT $ resolveRoleBasedRemoteSchema roleName remoteSchemaName originalIntrospection providedSchemaDoc
                    recordDependencies -< (metadataObject, schemaObject, dependencies)
                    returnA -< resolvedSchemaIntrospection
                )
            |) addPermContext
          )
        |) metadataObject

    mkRemoteSchemaPermissionMetadataObject ::
      (RemoteSchemaName, RemoteSchemaPermissionMetadata) ->
      MetadataObject
    mkRemoteSchemaPermissionMetadataObject (rsName, (RemoteSchemaPermissionMetadata roleName defn _)) =
      let objectId = MORemoteSchemaPermissions rsName roleName
       in MetadataObject objectId $ toJSON defn

addRemoteSchemaP2Setup ::
  (QErrM m, MonadIO m, HasHttpManagerM m, Tracing.MonadTrace m) =>
  Env.Environment ->
  RemoteSchemaName ->
  RemoteSchemaDef ->
  m (IntrospectionResult, BL.ByteString, RemoteSchemaInfo)
addRemoteSchemaP2Setup env name def = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef env def
  fetchRemoteSchema env httpMgr name rsi
