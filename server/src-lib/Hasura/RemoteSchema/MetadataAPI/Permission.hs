{-# LANGUAGE TemplateHaskell #-}

module Hasura.RemoteSchema.MetadataAPI.Permission
  ( AddRemoteSchemaPermission (..),
    DropRemoteSchemaPermissions (..),
    runDropRemoteSchemaPermissions,
    runAddRemoteSchemaPermissions,
  )
where

import Control.Lens ((^.))
import Data.Aeson.TH qualified as J
import Data.HashMap.Strict qualified as Map
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache.Permission
import Hasura.Server.Types
import Hasura.Session

data AddRemoteSchemaPermission = AddRemoteSchemaPermission
  { _arspRemoteSchema :: RemoteSchemaName,
    _arspRole :: RoleName,
    _arspDefinition :: RemoteSchemaPermissionDefinition,
    _arspComment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance NFData AddRemoteSchemaPermission

instance Cacheable AddRemoteSchemaPermission

$(J.deriveJSON hasuraJSON ''AddRemoteSchemaPermission)

data DropRemoteSchemaPermissions = DropRemoteSchemaPermissions
  { _drspRemoteSchema :: RemoteSchemaName,
    _drspRole :: RoleName
  }
  deriving (Show, Eq, Generic)

instance NFData DropRemoteSchemaPermissions

instance Cacheable DropRemoteSchemaPermissions

$(J.deriveJSON hasuraJSON ''DropRemoteSchemaPermissions)

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
  unless (remoteSchemaPermsCtx == Options.EnableRemoteSchemaPermissions) $ do
    throw400 ConstraintViolation $
      "remote schema permissions can only be added when "
        <> "remote schema permissions are enabled in the graphql-engine"
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  remoteSchemaCtx <-
    onNothing (Map.lookup name remoteSchemaMap) $
      throw400 NotExists $
        "remote schema " <> name <<> " doesn't exist"
  when (doesRemoteSchemaPermissionExist metadata name role) $
    throw400 AlreadyExists $
      "permissions for role: "
        <> role <<> " for remote schema:"
        <> name <<> " already exists"
  void $ resolveRoleBasedRemoteSchema role name (_rscIntroOriginal remoteSchemaCtx) providedSchemaDoc
  buildSchemaCacheFor (MORemoteSchemaPermissions name role) $
    MetadataModifier $
      metaRemoteSchemas . ix name . rsmPermissions %~ (:) remoteSchemaPermMeta
  pure successMsg
  where
    AddRemoteSchemaPermission name role defn comment = q

    remoteSchemaPermMeta = RemoteSchemaPermissionMetadata role defn comment

    providedSchemaDoc = _rspdSchema defn

doesRemoteSchemaPermissionExist :: Metadata -> RemoteSchemaName -> RoleName -> Bool
doesRemoteSchemaPermissionExist metadata remoteSchemaName roleName =
  any ((== roleName) . _rspmRole) $ metadata ^. (metaRemoteSchemas . ix remoteSchemaName . rsmPermissions)

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
      throw400 NotExists $
        "remote schema " <> name <<> " doesn't exist"
  unless (doesRemoteSchemaPermissionExist metadata name roleName) $
    throw400 NotExists $
      "permissions for role: "
        <> roleName <<> " for remote schema:"
        <> name <<> " doesn't exist"
  buildSchemaCacheFor (MORemoteSchemaPermissions name roleName) $
    dropRemoteSchemaPermissionInMetadata name roleName
  pure successMsg
