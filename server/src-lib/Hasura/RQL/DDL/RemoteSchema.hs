module Hasura.RQL.DDL.RemoteSchema
  ( runAddRemoteSchema
  , runRemoveRemoteSchema
  , dropRemoteSchemaInMetadata
  , runReloadRemoteSchema
  , addRemoteSchemaP1
  , addRemoteSchemaP2Setup
  , runIntrospectRemoteSchema
  , dropRemoteSchemaPermissionInMetadata
  , runAddRemoteSchemaPermissions
  , runDropRemoteSchemaPermissions
  ) where

import           Hasura.Prelude
import           Hasura.RQL.DDL.RemoteSchema.Permission

import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as S

import           Control.Monad.Unique
import           Data.Text.Extended

import           Hasura.EncJSON
import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)

import           Hasura.Session

runAddRemoteSchema
  :: ( HasVersion
     , QErrM m
     , CacheRWM m
     , MonadIO m
     , MonadUnique m
     , HasHttpManager m
     , MetadataM m
     )
  => Env.Environment
  -> AddRemoteSchemaQuery
  -> m EncJSON
runAddRemoteSchema env q@(AddRemoteSchemaQuery name defn comment) = do
  addRemoteSchemaP1 name
  -- addRemoteSchemaP2 env q
  void $ addRemoteSchemaP2Setup env q
  buildSchemaCacheFor (MORemoteSchema name) $
    MetadataModifier $ metaRemoteSchemas %~ OMap.insert name remoteSchemaMeta
  pure successMsg
  where
    remoteSchemaMeta = RemoteSchemaMetadata name defn comment mempty

runAddRemoteSchemaPermissions
  :: ( QErrM m
     , CacheRWM m
     , HasRemoteSchemaPermsCtx m
     , MetadataM m
     )
  => AddRemoteSchemaPermissions
  -> m EncJSON
runAddRemoteSchemaPermissions q = do
  remoteSchemaPermsCtx <- askRemoteSchemaPermsCtx
  unless (remoteSchemaPermsCtx == RemoteSchemaPermsEnabled) $ do
    throw400 ConstraintViolation
      $ "remote schema permissions can only be added when "
      <> "remote schema permissions are enabled in the graphql-engine"
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  remoteSchemaCtx <-
    onNothing (Map.lookup name remoteSchemaMap) $
      throw400 NotExists $ "remote schema " <> name <<> " doesn't exist"
  onJust (Map.lookup role $ _rscPermissions remoteSchemaCtx) $ \_ ->
    throw400 AlreadyExists $ "permissions for role: " <> role <<> " for remote schema:"
      <> name <<> " already exists"
  resolveRoleBasedRemoteSchema providedSchemaDoc remoteSchemaCtx
  buildSchemaCacheFor (MORemoteSchemaPermissions name role) $
    MetadataModifier $ metaRemoteSchemas.ix name.rsmPermissions %~ (:) remoteSchemaPermMeta
  pure successMsg
  where
    AddRemoteSchemaPermissions name role defn comment = q

    remoteSchemaPermMeta = RemoteSchemaPermissionMetadata role defn comment

    providedSchemaDoc = _rspdSchema defn

runDropRemoteSchemaPermissions
  :: ( QErrM m
     , CacheRWM m
     , MetadataM m
     )
  => DropRemoteSchemaPermissions
  -> m EncJSON
runDropRemoteSchemaPermissions (DropRemoteSchemaPermissions name roleName) = do
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  RemoteSchemaCtx _ _ _ _ _ perms <-
    onNothing (Map.lookup name remoteSchemaMap) $
      throw400 NotExists $ "remote schema " <> name <<> " doesn't exist"
  onNothing (Map.lookup roleName perms) $
    throw400 NotExists $ "permissions for role: " <> roleName <<> " for remote schema:"
     <> name <<> " doesn't exist"
  buildSchemaCacheFor (MORemoteSchemaPermissions name roleName) $
    dropRemoteSchemaPermissionInMetadata name roleName
  pure successMsg

addRemoteSchemaP1
  :: (QErrM m, CacheRM m)
  => RemoteSchemaName -> m ()
addRemoteSchemaP1 name = do
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  onJust (Map.lookup name remoteSchemaMap) $ const $
    throw400 AlreadyExists $ "remote schema with name "
    <> name <<> " already exists"

addRemoteSchemaP2Setup
  :: (HasVersion, QErrM m, MonadIO m, MonadUnique m, HasHttpManager m)
  => Env.Environment
  -> AddRemoteSchemaQuery -> m RemoteSchemaCtx
addRemoteSchemaP2Setup env (AddRemoteSchemaQuery name def _) = do
  httpMgr <- askHttpManager
  rsi <- validateRemoteSchemaDef env def
  fetchRemoteSchema env httpMgr name rsi

runRemoveRemoteSchema
  :: (QErrM m, UserInfoM m, CacheRWM m, MetadataM m)
  => RemoteSchemaNameQuery -> m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn) = do
  removeRemoteSchemaP1 rsn
  withNewInconsistentObjsCheck $ buildSchemaCache $
    dropRemoteSchemaInMetadata rsn
  pure successMsg

removeRemoteSchemaP1
  :: (UserInfoM m, QErrM m, CacheRM m)
  => RemoteSchemaName -> m [RoleName]
removeRemoteSchemaP1 rsn = do
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  void $ onNothing (Map.lookup rsn rmSchemas) $
    throw400 NotExists "no such remote schema"
  let depObjs = getDependentObjs sc remoteSchemaDepId
      roles = mapMaybe getRole depObjs
      nonPermDependentObjs = filter nonPermDependentObjPredicate depObjs
  -- report non permission dependencies (if any), this happens
  -- mostly when a remote relationship is defined with
  -- the current remote schema

  -- we only report the non permission dependencies because we
  -- drop the related permissions
  when (nonPermDependentObjs /= []) $ reportDeps nonPermDependentObjs
  pure roles
  where
    remoteSchemaDepId = SORemoteSchema rsn

    getRole depObj =
      case depObj of
        SORemoteSchemaPermission _ role -> Just role
        _                               -> Nothing

    nonPermDependentObjPredicate (SORemoteSchemaPermission _ _) = False
    nonPermDependentObjPredicate _                              = True

runReloadRemoteSchema
  :: (QErrM m, CacheRWM m, MetadataM m)
  => RemoteSchemaNameQuery -> m EncJSON
runReloadRemoteSchema (RemoteSchemaNameQuery name) = do
  remoteSchemas <- getAllRemoteSchemas <$> askSchemaCache
  unless (name `elem` remoteSchemas) $ throw400 NotExists $
    "remote schema with name " <> name <<> " does not exist"

  let invalidations = mempty { ciRemoteSchemas = S.singleton name }
  metadata <- getMetadata
  withNewInconsistentObjsCheck $
    buildSchemaCacheWithOptions CatalogUpdate invalidations metadata
  pure successMsg

dropRemoteSchemaInMetadata :: RemoteSchemaName -> MetadataModifier
dropRemoteSchemaInMetadata name =
  MetadataModifier $ metaRemoteSchemas %~ OMap.delete name

dropRemoteSchemaPermissionInMetadata :: RemoteSchemaName -> RoleName -> MetadataModifier
dropRemoteSchemaPermissionInMetadata remoteSchemaName roleName =
  MetadataModifier $ metaRemoteSchemas.ix remoteSchemaName.rsmPermissions %~ filter ((/=) roleName . _rspmRole)

runIntrospectRemoteSchema
  :: (CacheRM m, QErrM m) => RemoteSchemaNameQuery -> m EncJSON
runIntrospectRemoteSchema (RemoteSchemaNameQuery rsName) = do
  sc <- askSchemaCache
  RemoteSchemaCtx _ _ _ introspectionByteString _ _ <-
    Map.lookup rsName (scRemoteSchemas sc) `onNothing` throw400 NotExists ("remote schema: " <> rsName <<> " not found")
  pure $ encJFromLBS introspectionByteString
