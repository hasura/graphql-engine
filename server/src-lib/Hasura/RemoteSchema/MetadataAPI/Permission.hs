module Hasura.RemoteSchema.MetadataAPI.Permission
  ( AddRemoteSchemaPermission (..),
    DropRemoteSchemaPermissions (..),
    runDropRemoteSchemaPermissions,
    runAddRemoteSchemaPermissions,
  )
where

import Control.Lens ((^.))
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache.Permission

data AddRemoteSchemaPermission = AddRemoteSchemaPermission
  { _arspRemoteSchema :: RemoteSchemaName,
    _arspRole :: RoleName,
    _arspDefinition :: RemoteSchemaPermissionDefinition,
    _arspComment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance NFData AddRemoteSchemaPermission

instance J.FromJSON AddRemoteSchemaPermission where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON AddRemoteSchemaPermission where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

data DropRemoteSchemaPermissions = DropRemoteSchemaPermissions
  { _drspRemoteSchema :: RemoteSchemaName,
    _drspRole :: RoleName
  }
  deriving (Show, Eq, Generic)

instance NFData DropRemoteSchemaPermissions

instance J.FromJSON DropRemoteSchemaPermissions where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON DropRemoteSchemaPermissions where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

runAddRemoteSchemaPermissions ::
  ( QErrM m,
    CacheRWM m,
    MetadataM m
  ) =>
  Options.RemoteSchemaPermissions ->
  AddRemoteSchemaPermission ->
  m EncJSON
runAddRemoteSchemaPermissions remoteSchemaPermsCtx q = do
  metadata <- getMetadata
  unless (remoteSchemaPermsCtx == Options.EnableRemoteSchemaPermissions) $ do
    throw400 ConstraintViolation
      $ "remote schema permissions can only be added when "
      <> "remote schema permissions are enabled in the graphql-engine"
  remoteSchemaMap <- scRemoteSchemas <$> askSchemaCache
  remoteSchemaCtx <-
    onNothing (HashMap.lookup name remoteSchemaMap)
      $ throw400 NotExists
      $ "remote schema "
      <> name
      <<> " doesn't exist"
  when (doesRemoteSchemaPermissionExist metadata name role)
    $ throw400 AlreadyExists
    $ "permissions for role: "
    <> role
    <<> " for remote schema:"
    <> name
    <<> " already exists"
  void $ resolveRoleBasedRemoteSchema role name (_rscIntroOriginal remoteSchemaCtx) providedSchemaDoc
  buildSchemaCacheFor (MORemoteSchemaPermissions name role)
    $ MetadataModifier
    $ metaRemoteSchemas
    . ix name
    . rsmPermissions
    %~ (:) remoteSchemaPermMeta
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
  void
    $ onNothing (HashMap.lookup name remoteSchemaMap)
    $ throw400 NotExists
    $ "remote schema "
    <> name
    <<> " doesn't exist"
  unless (doesRemoteSchemaPermissionExist metadata name roleName)
    $ throw400 NotExists
    $ "permissions for role: "
    <> roleName
    <<> " for remote schema:"
    <> name
    <<> " doesn't exist"
  buildSchemaCacheFor (MORemoteSchemaPermissions name roleName)
    $ dropRemoteSchemaPermissionInMetadata name roleName
  pure successMsg
