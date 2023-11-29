module Hasura.RemoteSchema.MetadataAPI.Core
  ( AddRemoteSchemaQuery (..),
    RemoteSchemaNameQuery (..),
    runAddRemoteSchema,
    runRemoveRemoteSchema,
    dropRemoteSchemaInMetadata,
    runReloadRemoteSchema,
    runIntrospectRemoteSchema,
    dropRemoteSchemaPermissionInMetadata,
    dropRemoteSchemaRemoteRelationshipInMetadata,
    runUpdateRemoteSchema,
  )
where

import Data.Aeson qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as S
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.RemoteServer
import Hasura.GraphQL.Schema.Common (SchemaSampledFeatureFlags)
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache.Build (addRemoteSchemaP2Setup)
import Hasura.RemoteSchema.SchemaCache.Types
import Hasura.Services
import Hasura.Session (UserInfoM)
import Hasura.Tracing qualified as Tracing

-- | The payload for 'add_remote_schema', and a component of 'Metadata'.
data AddRemoteSchemaQuery = AddRemoteSchemaQuery
  { -- | An internal identifier for this remote schema.
    _arsqName :: RemoteSchemaName,
    _arsqDefinition :: RemoteSchemaDef,
    -- | An opaque description or comment. We might display this in the UI, for instance.
    _arsqComment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance NFData AddRemoteSchemaQuery

instance J.FromJSON AddRemoteSchemaQuery where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON AddRemoteSchemaQuery where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

newtype RemoteSchemaNameQuery = RemoteSchemaNameQuery
  { _rsnqName :: RemoteSchemaName
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON RemoteSchemaNameQuery where
  parseJSON = J.genericParseJSON hasuraJSON

instance J.ToJSON RemoteSchemaNameQuery where
  toJSON = J.genericToJSON hasuraJSON
  toEncoding = J.genericToEncoding hasuraJSON

runAddRemoteSchema ::
  ( QErrM m,
    CacheRWM m,
    MonadIO m,
    ProvidesNetwork m,
    MetadataM m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  SchemaSampledFeatureFlags ->
  AddRemoteSchemaQuery ->
  m EncJSON
runAddRemoteSchema env schemaSampledFeatureFlags (AddRemoteSchemaQuery name defn comment) = do
  addRemoteSchemaP1 name
  void $ addRemoteSchemaP2Setup name env schemaSampledFeatureFlags defn
  buildSchemaCacheFor (MORemoteSchema name)
    $ MetadataModifier
    $ metaRemoteSchemas
    %~ InsOrdHashMap.insert name remoteSchemaMeta
  pure successMsg
  where
    -- NOTE: permissions here are empty, manipulated via a separate API with
    -- runAddRemoteSchemaPermissions below
    remoteSchemaMeta = RemoteSchemaMetadata name defn comment mempty mempty

addRemoteSchemaP1 ::
  (QErrM m, CacheRM m) =>
  RemoteSchemaName ->
  m ()
addRemoteSchemaP1 name = do
  remoteSchemaNames <- getAllRemoteSchemas <$> askSchemaCache
  when (name `elem` remoteSchemaNames)
    $ throw400 AlreadyExists
    $ "remote schema with name "
    <> name
    <<> " already exists"

runRemoveRemoteSchema ::
  (QErrM m, UserInfoM m, CacheRWM m, MetadataM m) =>
  RemoteSchemaNameQuery ->
  m EncJSON
runRemoveRemoteSchema (RemoteSchemaNameQuery rsn) = do
  void $ removeRemoteSchemaP1 rsn
  withNewInconsistentObjsCheck
    $ buildSchemaCache
    $ dropRemoteSchemaInMetadata rsn
  pure successMsg

removeRemoteSchemaP1 ::
  (UserInfoM m, QErrM m, CacheRM m) =>
  RemoteSchemaName ->
  m [RoleName]
removeRemoteSchemaP1 rsn = do
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  void
    $ onNothing (HashMap.lookup rsn rmSchemas)
    $ throw400 NotExists "no such remote schema"
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
  unless (name `elem` remoteSchemas)
    $ throw400 NotExists
    $ "remote schema with name "
    <> name
    <<> " does not exist"

  let invalidations = mempty {ciRemoteSchemas = S.singleton name}
  metadata <- getMetadata
  withNewInconsistentObjsCheck
    $ buildSchemaCacheWithOptions (CatalogUpdate Nothing) invalidations metadata Nothing
  pure successMsg

runIntrospectRemoteSchema ::
  (CacheRM m, QErrM m) => RemoteSchemaNameQuery -> m EncJSON
runIntrospectRemoteSchema (RemoteSchemaNameQuery rsName) = do
  sc <- askSchemaCache
  RemoteSchemaCtx {..} <-
    HashMap.lookup rsName (scRemoteSchemas sc) `onNothing` throw400 NotExists ("remote schema: " <> rsName <<> " not found")
  pure $ encJFromLBS _rscRawIntrospectionResult

runUpdateRemoteSchema ::
  ( QErrM m,
    CacheRWM m,
    MonadIO m,
    ProvidesNetwork m,
    MetadataM m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  SchemaSampledFeatureFlags ->
  AddRemoteSchemaQuery ->
  m EncJSON
runUpdateRemoteSchema env schemaSampledFeatureFlags (AddRemoteSchemaQuery name defn comment) = do
  remoteSchemaNames <- getAllRemoteSchemas <$> askSchemaCache
  remoteSchemaMap <- _metaRemoteSchemas <$> getMetadata

  let metadataRMSchema = InsOrdHashMap.lookup name remoteSchemaMap
      metadataRMSchemaPerms = maybe mempty _rsmPermissions metadataRMSchema
      -- `metadataRMSchemaURL` and `metadataRMSchemaURLFromEnv` represent
      -- details that were stored within the metadata
      metadataRMSchemaURL = (_rsdUrl . _rsmDefinition) =<< metadataRMSchema
      metadataRMSchemaURLFromEnv = (_rsdUrlFromEnv . _rsmDefinition) =<< metadataRMSchema
      -- `currentRMSchemaURL` and `currentRMSchemaURLFromEnv` represent
      -- the details that were provided in the request
      currentRMSchemaURL = _rsdUrl defn
      currentRMSchemaURLFromEnv = _rsdUrlFromEnv defn

  unless (name `elem` remoteSchemaNames)
    $ throw400 NotExists
    $ "remote schema with name "
    <> name
    <<> " doesn't exist"

  rsi <- validateRemoteSchemaDef name env defn

  -- we only proceed to fetch the remote schema if the url has been updated
  unless
    ( (isJust metadataRMSchemaURL && isJust currentRMSchemaURL && metadataRMSchemaURL == currentRMSchemaURL)
        || (isJust metadataRMSchemaURLFromEnv && isJust currentRMSchemaURLFromEnv && metadataRMSchemaURLFromEnv == currentRMSchemaURLFromEnv)
    )
    $ void
    $ fetchRemoteSchema env schemaSampledFeatureFlags rsi

  -- This will throw an error if the new schema fetched in incompatible
  -- with the existing permissions and relations
  withNewInconsistentObjsCheck
    $ buildSchemaCacheFor (MORemoteSchema name)
    $ MetadataModifier
    $ metaRemoteSchemas
    %~ InsOrdHashMap.insert name (remoteSchemaMeta metadataRMSchemaPerms)

  pure successMsg
  where
    remoteSchemaMeta perms = RemoteSchemaMetadata name defn comment perms mempty
