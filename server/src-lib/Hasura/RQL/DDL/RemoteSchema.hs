module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema,
    runRemoveRemoteSchema,
    dropRemoteSchemaInMetadata,
    runReloadRemoteSchema,
    addRemoteSchemaP1,
    addRemoteSchemaP2Setup,
    runIntrospectRemoteSchema,
    dropRemoteSchemaPermissionInMetadata,
    dropRemoteSchemaRemoteRelationshipInMetadata,
    runAddRemoteSchemaPermissions,
    runDropRemoteSchemaPermissions,
    runUpdateRemoteSchema,
  )
where

import Control.Lens ((^.))
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as S
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.RemoteServer
import Hasura.Prelude
import Hasura.RQL.DDL.RemoteSchema.Permission
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client.Manager (HasHttpManagerM (..))

runAddRemoteSchema ::
  ( QErrM m,
    CacheRWM m,
    MonadIO m,
    HasHttpManagerM m,
    MetadataM m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  AddRemoteSchemaQuery ->
  m EncJSON
runAddRemoteSchema env q@(AddRemoteSchemaQuery name defn comment) = do
  addRemoteSchemaP1 name
  -- addRemoteSchemaP2 env q
  void $ addRemoteSchemaP2Setup env q
  buildSchemaCacheFor (MORemoteSchema name) $
    MetadataModifier $ metaRemoteSchemas %~ OMap.insert name remoteSchemaMeta
  pure successMsg
  where
    -- NOTE: permissions here are empty, manipulated via a separate API with
    -- runAddRemoteSchemaPermissions below
    remoteSchemaMeta = RemoteSchemaMetadata name defn comment mempty mempty

doesRemoteSchemaPermissionExist :: Metadata -> RemoteSchemaName -> RoleName -> Bool
doesRemoteSchemaPermissionExist metadata remoteSchemaName roleName =
  any ((== roleName) . _rspmRole) $ metadata ^. (metaRemoteSchemas . ix remoteSchemaName . rsmPermissions)

runAddRemoteSchemaPermissions ::
  ( QErrM m,
    CacheRWM m,
    HasServerConfigCtx m,
    MetadataM m
  ) =>
  AddRemoteSchemaPermission ->
  m EncJSON
runAddRemoteSchemaPermissions q = do
  metadata <- getMetadata
  remoteSchemaPermsCtx <- _sccRemoteSchemaPermsCtx <$> askServerConfigCtx
  unless (remoteSchemaPermsCtx == RemoteSchemaPermsEnabled) $ do
    throw400 ConstraintViolation $
      "remote schema permissions can only be added when "
        <> "remote schema permissions are enabled in the graphql-engine"
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  remoteSchemaCtx <-
    onNothing (Map.lookup name remoteSchemaMap) $
      throw400 NotExists $ "remote schema " <> name <<> " doesn't exist"
  when (doesRemoteSchemaPermissionExist metadata name role) $
    throw400 AlreadyExists $
      "permissions for role: " <> role <<> " for remote schema:"
        <> name <<> " already exists"
  void $ resolveRoleBasedRemoteSchema providedSchemaDoc remoteSchemaCtx
  buildSchemaCacheFor (MORemoteSchemaPermissions name role) $
    MetadataModifier $ metaRemoteSchemas . ix name . rsmPermissions %~ (:) remoteSchemaPermMeta
  pure successMsg
  where
    AddRemoteSchemaPermission name role defn comment = q

    remoteSchemaPermMeta = RemoteSchemaPermissionMetadata role defn comment

    providedSchemaDoc = _rspdSchema defn

runDropRemoteSchemaPermissions ::
  ( QErrM m,
    CacheRWM m,
    MetadataM m
  ) =>
  DropRemoteSchemaPermissions ->
  m EncJSON
runDropRemoteSchemaPermissions (DropRemoteSchemaPermissions name roleName) = do
  metadata <- getMetadata
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  void $
    onNothing (Map.lookup name remoteSchemaMap) $
      throw400 NotExists $ "remote schema " <> name <<> " doesn't exist"
  unless (doesRemoteSchemaPermissionExist metadata name roleName) $
    throw400 NotExists $
      "permissions for role: " <> roleName <<> " for remote schema:"
        <> name <<> " doesn't exist"
  buildSchemaCacheFor (MORemoteSchemaPermissions name roleName) $
    dropRemoteSchemaPermissionInMetadata name roleName
  pure successMsg

addRemoteSchemaP1 ::
  (QErrM m, CacheRM m) =>
  RemoteSchemaName ->
  m ()
addRemoteSchemaP1 name = do
  remoteSchemaNames <- getAllRemoteSchemas <$> askSchemaCache
  when (name `elem` remoteSchemaNames) $
    throw400 AlreadyExists $
      "remote schema with name "
        <> name <<> " already exists"

addRemoteSchemaP2Setup ::
  (QErrM m, MonadIO m, HasHttpManagerM m, Tracing.MonadTrace m) =>
  Env.Environment ->
  AddRemoteSchemaQuery ->
  m RemoteSchemaCtx
addRemoteSchemaP2Setup env (AddRemoteSchemaQuery name def _) = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef env def
  fetchRemoteSchema env httpMgr name rsi

runRemoveRemoteSchema ::
  (QErrM m, UserInfoM m, CacheRWM m, MetadataM m) =>
  RemoteSchemaNameQuery ->
  m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn) = do
  void $ removeRemoteSchemaP1 rsn
  withNewInconsistentObjsCheck $
    buildSchemaCache $
      dropRemoteSchemaInMetadata rsn
  pure successMsg

removeRemoteSchemaP1 ::
  (UserInfoM m, QErrM m, CacheRM m) =>
  RemoteSchemaName ->
  m [RoleName]
removeRemoteSchemaP1 rsn = do
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  void $
    onNothing (Map.lookup rsn rmSchemas) $
      throw400 NotExists "no such remote schema"
  let depObjs = getDependentObjs sc remoteSchemaDepId
      roles = mapMaybe getRole depObjs
      nonPermDependentObjs = filter nonPermDependentObjPredicate depObjs
  -- report non permission dependencies (if any), this happens
  -- mostly when a remote relationship is defined with
  -- the current remote schema

  -- we only report the non permission dependencies because we
  -- drop the related permissions
  unless (null nonPermDependentObjs) $ reportDependentObjectsExist nonPermDependentObjs
  pure roles
  where
    remoteSchemaDepId = SORemoteSchema rsn

    getRole depObj =
      case depObj of
        SORemoteSchemaPermission _ role -> Just role
        _ -> Nothing

    nonPermDependentObjPredicate (SORemoteSchemaPermission _ _) = False
    nonPermDependentObjPredicate _ = True

runReloadRemoteSchema ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  RemoteSchemaNameQuery ->
  m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  remoteSchemas <- getAllRemoteSchemas <$> askSchemaCache
  unless (name `elem` remoteSchemas) $
    throw400 NotExists $
      "remote schema with name " <> name <<> " does not exist"

  let invalidations = mempty {ciRemoteSchemas = S.singleton name}
  metadata <- getMetadata
  withNewInconsistentObjsCheck $
    buildSchemaCacheWithOptions (CatalogUpdate Nothing) invalidations metadata
  pure successMsg

runIntrospectRemoteSchema ::
  (CacheRM m, QErrM m) => RemoteSchemaNameQuery -> m EncJSON
runIntrospectRemoteSchema (RemoteSchemaNameQuery rsName) = do
  sc <- askSchemaCache
  RemoteSchemaCtx {..} <-
    Map.lookup rsName (scRemoteSchemas sc) `onNothing` throw400 NotExists ("remote schema: " <> rsName <<> " not found")
  pure $ encJFromLBS _rscRawIntrospectionResult

runUpdateRemoteSchema ::
  ( QErrM m,
    CacheRWM m,
    MonadIO m,
    HasHttpManagerM m,
    MetadataM m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  AddRemoteSchemaQuery ->
  m EncJSON
runUpdateRemoteSchema env (AddRemoteSchemaQuery name defn comment) = do
  remoteSchemaNames <- getAllRemoteSchemas <$> askSchemaCache
  remoteSchemaMap <- _metaRemoteSchemas <$> getMetadata

  let metadataRMSchema = OMap.lookup name remoteSchemaMap
      metadataRMSchemaPerms = maybe mempty _rsmPermissions metadataRMSchema
      -- `metadataRMSchemaURL` and `metadataRMSchemaURLFromEnv` represent
      -- details that were stored within the metadata
      metadataRMSchemaURL = (_rsdUrl . _rsmDefinition) =<< metadataRMSchema
      metadataRMSchemaURLFromEnv = (_rsdUrlFromEnv . _rsmDefinition) =<< metadataRMSchema
      -- `currentRMSchemaURL` and `currentRMSchemaURLFromEnv` represent
      -- the details that were provided in the request
      currentRMSchemaURL = _rsdUrl defn
      currentRMSchemaURLFromEnv = _rsdUrlFromEnv defn

  unless (name `elem` remoteSchemaNames) $
    throw400 NotExists $ "remote schema with name " <> name <<> " doesn't exist"

  rsi <- validateRemoteSchemaDef env defn

  -- we only proceed to fetch the remote schema if the url has been updated
  unless
    ( (isJust metadataRMSchemaURL && isJust currentRMSchemaURL && metadataRMSchemaURL == currentRMSchemaURL)
        || (isJust metadataRMSchemaURLFromEnv && isJust currentRMSchemaURLFromEnv && metadataRMSchemaURLFromEnv == currentRMSchemaURLFromEnv)
    )
    $ do
      httpMgr <- askHttpManager
      void $ fetchRemoteSchema env httpMgr name rsi

  -- This will throw an error if the new schema fetched in incompatible
  -- with the existing permissions and relations
  withNewInconsistentObjsCheck $
    buildSchemaCacheFor (MORemoteSchema name) $
      MetadataModifier $ metaRemoteSchemas %~ OMap.insert name (remoteSchemaMeta metadataRMSchemaPerms)

  pure successMsg
  where
    remoteSchemaMeta perms = RemoteSchemaMetadata name defn comment perms mempty
