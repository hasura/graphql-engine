{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.SchemaCache
  ( SchemaCache (..),
    TableConfig (..),
    emptyTableConfig,
    getAllRemoteSchemas,
    unsafeFunctionCache,
    unsafeFunctionInfo,
    unsafeTableCache,
    unsafeTableInfo,
    askSourceInfo,
    askSourceInfoMaybe,
    askSourceConfig,
    askSourceConfigMaybe,
    askTableCache,
    askTableInfo,
    askTableCoreInfo,
    askTableFieldInfoMap,
    askLogicalModelCache,
    askTableMetadata,
    askFunctionInfo,
    askFieldInfoMapSource,
    TableCoreCache,
    TableCache,
    ActionCache,
    InheritedRolesCache,
    TableCoreInfoG (..),
    TableCoreInfo,
    tciName,
    tciDescription,
    tciFieldInfoMap,
    tciPrimaryKey,
    tciUniqueConstraints,
    tciForeignKeys,
    tciViewInfo,
    tciEnumValues,
    tciCustomConfig,
    tciUniqueOrPrimaryKeyConstraints,
    TableInfo (..),
    tiCoreInfo,
    tiRolePermInfoMap,
    tiEventTriggerInfoMap,
    ViewInfo (..),
    isMutable,
    IntrospectionResult (..),
    RemoteSchemaCustomizer (..),
    remoteSchemaCustomizeTypeName,
    remoteSchemaCustomizeFieldName,
    RemoteSchemaRelationships,
    RemoteSchemaCtxG (..),
    PartiallyResolvedRemoteSchemaCtx,
    RemoteSchemaCtx,
    PartiallyResolvedRemoteSchemaMap,
    RemoteSchemaMap,
    DepMap,
    WithDeps,
    TableCoreInfoRM (..),
    TableCoreCacheRT (..),
    TableInfoRM (..),
    TableCacheRT (..),
    CacheRM (..),
    FieldInfoMap,
    FieldInfo (..),
    _FIColumn,
    _FIRelationship,
    _FIComputedField,
    fieldInfoName,
    fieldInfoGraphQLNames,
    getCols,
    getRels,
    getComputedFieldInfos,
    RolePermInfoMap,
    InsPermInfo (..),
    UpdPermInfo (..),
    DelPermInfo (..),
    PreSetColsPartial,
    EventTriggerInfo (..),
    EventTriggerInfoMap,
    TableObjId (..),
    SchemaObjId (..),
    reportSchemaObj,
    reportSchemaObjs,
    DependencyReason (..),
    SchemaDependency (..),
    mkParentDep,
    mkLogicalModelParentDep,
    mkColDep,
    mkLogicalModelColDep,
    mkComputedFieldDep,
    getDependentObjs,
    getDependentObjsWith,
    getRemoteDependencies,
    FunctionVolatility (..),
    FunctionArgName (..),
    FunctionInfo (..),
    FunctionCache,
    CronTriggerInfo (..),
    MetadataResourceVersion (..),
    showMetadataResourceVersion,
    initialResourceVersion,
    MetadataWithResourceVersion (..),
    getLogicalModelBoolExpDeps,
    getBoolExpDeps,
    InlinedAllowlist,
    BoolExpM (..),
    BoolExpCtx (..),
    getOpExpDeps,
    BackendInfoWrapper (..),
    BackendCache,
    getBackendInfo,
  )
where

import Control.Lens (Traversal', at, preview, (^.))
import Data.Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HS
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Extended ((<<>))
import Data.Text.Extended qualified as T
import Database.MSSQL.Transaction qualified as MSSQL
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection qualified as Postgres
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.GraphQL.Context (GQLContext, RoleContext)
import Hasura.LogicalModel.Cache (LogicalModelCache)
import Hasura.LogicalModel.Fields (LogicalModelFieldsLookupRT)
import Hasura.LogicalModel.Types (LogicalModelLocation (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.BoolExp.RemoteRelationshipPredicate (rrrfweColumnFieldName)
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (HasTag (backendTag), reify)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.OpenTelemetry (OpenTelemetryInfo)
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Session (UserInfoM)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Webhook.Transform
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache.Types
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.BackendMap (BackendMap)
import Hasura.SQL.BackendMap qualified as BackendMap
import Hasura.Table.Cache
import Hasura.Table.Metadata (TableMetadata (..))
import Hasura.Tracing (TraceT)
import Language.GraphQL.Draft.Syntax qualified as G
import Network.Types.Extended (TlsAllow)
import System.Cron.Types

newtype MetadataResourceVersion = MetadataResourceVersion
  { getMetadataResourceVersion :: Int64
  }
  deriving (Eq, Num, FromJSON, ToJSON)

initialResourceVersion :: MetadataResourceVersion
initialResourceVersion = MetadataResourceVersion 0

showMetadataResourceVersion :: MetadataResourceVersion -> Text
showMetadataResourceVersion (MetadataResourceVersion version) = tshow version

instance Show MetadataResourceVersion where
  show = T.unpack . showMetadataResourceVersion

data MetadataWithResourceVersion = MetadataWithResourceVersion
  { _mwrvMetadata :: Metadata,
    _mwrvResourceVersion :: MetadataResourceVersion
  }
  deriving (Eq)

mkParentDep ::
  forall b.
  (Backend b) =>
  SourceName ->
  TableName b ->
  SchemaDependency
mkParentDep s tn =
  SchemaDependency (SOSourceObj s $ AB.mkAnyBackend @b (SOITable tn)) DRTable

-- | When we depend on anything to do with logical models, we also declare that
-- we depend on the logical model as a whole. This is the "parent" dependency
-- in the dependency tree for a given logical model.
mkLogicalModelParentDep ::
  forall b.
  (Backend b) =>
  SourceName ->
  LogicalModelLocation ->
  SchemaDependency
mkLogicalModelParentDep source logicalModelLocation = do
  let sourceObject :: SchemaObjId
      sourceObject =
        case logicalModelLocation of
          LMLLogicalModel logicalModelName ->
            SOSourceObj source
              $ AB.mkAnyBackend @b
              $ SOILogicalModel logicalModelName
          LMLNativeQuery nativeQueryName ->
            SOSourceObj source
              $ AB.mkAnyBackend @b
              $ SOINativeQuery nativeQueryName

  SchemaDependency sourceObject DRTable

mkColDep ::
  forall b.
  (Backend b) =>
  DependencyReason ->
  SourceName ->
  TableName b ->
  Column b ->
  SchemaDependency
mkColDep reason source tn col =
  flip SchemaDependency reason
    . SOSourceObj source
    . AB.mkAnyBackend
    . SOITableObj @b tn
    $ TOCol @b col

-- | Declare a dependency on a particular column of a logical model
mkLogicalModelColDep ::
  forall b.
  (Backend b) =>
  DependencyReason ->
  SourceName ->
  LogicalModelLocation ->
  Column b ->
  SchemaDependency
mkLogicalModelColDep reason source logicalModelLocation column = do
  let sourceObject :: SchemaObjId
      sourceObject =
        SOSourceObj source
          $ AB.mkAnyBackend
          $ SOILogicalModelObj @b logicalModelLocation
          $ LMOCol @b column

  SchemaDependency sourceObject reason

mkComputedFieldDep ::
  forall b.
  (Backend b) =>
  DependencyReason ->
  SourceName ->
  TableName b ->
  ComputedFieldName ->
  SchemaDependency
mkComputedFieldDep reason s tn computedField =
  flip SchemaDependency reason
    . SOSourceObj s
    . AB.mkAnyBackend
    . SOITableObj @b tn
    $ TOComputedField computedField

type WithDeps a = (a, Seq SchemaDependency)

type RemoteSchemaRelationships = RemoteSchemaRelationshipsG (RemoteFieldInfo G.Name)

type RemoteSchemaCtx = RemoteSchemaCtxG (RemoteFieldInfo G.Name)

type RemoteSchemaMap = HashMap.HashMap RemoteSchemaName RemoteSchemaCtx

type PartiallyResolvedRemoteSchemaCtx =
  RemoteSchemaCtxG
    (PartiallyResolvedRemoteRelationship RemoteRelationshipDefinition)

type PartiallyResolvedRemoteSchemaMap =
  HashMap.HashMap RemoteSchemaName PartiallyResolvedRemoteSchemaCtx

type DepMap = HashMap.HashMap SchemaObjId (HS.HashSet SchemaDependency)

data CronTriggerInfo = CronTriggerInfo
  { ctiName :: TriggerName,
    ctiSchedule :: CronSchedule,
    ctiPayload :: Maybe Value,
    ctiRetryConf :: STRetryConf,
    ctiWebhookInfo :: EnvRecord ResolvedWebhook,
    ctiHeaders :: [EventHeaderInfo],
    ctiComment :: Maybe Text,
    ctiRequestTransform :: Maybe RequestTransform,
    ctiResponseTransform :: Maybe MetadataResponseTransform
  }
  deriving (Show, Eq, Generic)

instance ToJSON CronTriggerInfo where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

type ActionCache = HashMap.HashMap ActionName ActionInfo -- info of all actions

type InheritedRolesCache = HashMap.HashMap RoleName (HashSet RoleName)

-------------------------------------------------------------------------------

-- | Retrieves the source info for a given source name.
--
-- This function retrieves the schema cache from the monadic context, and
-- attempts to look the corresponding source up in the source cache. This
-- function must be used with a _type annotation_, such as `askSourceInfo
-- @('Postgres 'Vanilla)`. It throws an error if:
-- 1. The function fails to find the named source at all
-- 2. The named source exists but does not match the expected type
-- 3. The named source exists, and is of the expected type, but is inconsistent
askSourceInfo ::
  forall b m.
  (CacheRM m, MetadataM m, MonadError QErr m, Backend b) =>
  SourceName ->
  m (SourceInfo b)
askSourceInfo sourceName = do
  sources <- scSources <$> askSchemaCache
  -- find any matching source info by name
  case HashMap.lookup sourceName sources of
    -- 1. The function fails to find the named source at all
    Nothing -> throw400 NotExists $ "source with name " <> sourceName <<> " does not exist"
    Just matchingNameSourceInfo -> do
      -- find matching source info for backend type @b
      onNothing (unsafeSourceInfo @b matchingNameSourceInfo) do
        metadata <- getMetadata
        maybe
          -- 2. The named source exists but does not match the expected type
          ( throw400 NotExists
              $ "source with name "
              <> sourceName
              <<> " has backend type "
              <> T.toTxt (AB.lowerTag matchingNameSourceInfo)
              <<> " which does not match the expected type "
              <> T.toTxt (reify $ backendTag @b)
          )
          -- 3. The named source exists, and is of the expected type, but is inconsistent
          ( const
              $ throw400 Unexpected
              $ "source with name "
              <> sourceName
              <<> " is inconsistent"
          )
          (metadata ^. metaSources . at sourceName)

askSourceInfoMaybe ::
  forall b m.
  (CacheRM m, Backend b) =>
  SourceName ->
  m (Maybe (SourceInfo b))
askSourceInfoMaybe sourceName = do
  sources <- scSources <$> askSchemaCache
  pure (unsafeSourceInfo @b =<< HashMap.lookup sourceName sources)

-- | Retrieves the source config for a given source name.
--
-- This function relies on 'askSourceInfo' and similarly throws an error if the
-- source isn't found.
askSourceConfig ::
  forall b m.
  (CacheRM m, MonadError QErr m, Backend b, MetadataM m) =>
  SourceName ->
  m (SourceConfig b)
askSourceConfig = fmap _siConfiguration . askSourceInfo @b

askSourceConfigMaybe ::
  forall b m.
  (CacheRM m, Backend b) =>
  SourceName ->
  m (Maybe (SourceConfig b))
askSourceConfigMaybe =
  fmap (fmap _siConfiguration) . askSourceInfoMaybe @b

-- | Retrieves the table cache for a given source cache and source name.
--
-- This function must be used with a _type annotation_, such as
-- `unsafeTableCache @('Postgres 'Vanilla)`. It returns @Nothing@ if it fails to
-- find that source or if the kind of the source does not match the type
-- annotation, and does not distinguish between the two cases.
unsafeTableCache ::
  forall b. (Backend b) => SourceName -> SourceCache -> Maybe (TableCache b)
unsafeTableCache sourceName cache = do
  unsafeSourceTables @b =<< HashMap.lookup sourceName cache

-- | Retrieves the table cache for a given source name.
--
-- This function retrieves the schema cache from the monadic context, and
-- attempts to look the corresponding source up in the source cache. It must be
-- used with a _type annotation_, such as `unsafeTableCache @('Postgres
-- 'Vanilla)`. It returns @Nothing@ if it fails to find that source or if the
-- kind of the source does not match the type annotation, and does not
-- distinguish between the two cases.
askTableCache ::
  forall b m.
  (Backend b, CacheRM m) =>
  SourceName ->
  m (Maybe (TableCache b))
askTableCache sourceName = do
  sources <- scSources <$> askSchemaCache
  pure $ unsafeSourceTables =<< HashMap.lookup sourceName sources

-- | Retrieves the information about a table from the source cache, the source
-- name, and the table name.
--
-- This function returns @Nothing@ if it fails to find that source or if the
-- kind of the source does not match the type annotation, and does not
-- distinguish between the two cases.
unsafeTableInfo ::
  forall b. (Backend b) => SourceName -> TableName b -> SourceCache -> Maybe (TableInfo b)
unsafeTableInfo sourceName tableName cache =
  HashMap.lookup tableName =<< unsafeTableCache @b sourceName cache

-- | Retrieves the information about a table for a given source name and table
-- name.
--
-- This function retrieves the schema cache from the monadic context, and
-- attempts to look the corresponding source up in the source cache. it throws
-- an error if it fails to find that source, in which case it looks that source
-- up in the metadata, to differentiate between the source not existing or the
-- type of the source not matching.
askTableInfo ::
  forall b m.
  (QErrM m, CacheRM m, Backend b) =>
  SourceName ->
  TableName b ->
  m (TableInfo b)
askTableInfo sourceName tableName = do
  rawSchemaCache <- askSchemaCache
  onNothing (unsafeTableInfo sourceName tableName $ scSources rawSchemaCache)
    $ throw400 NotExists
    $ "table "
    <> tableName
    <<> " does not exist in source: "
    <> sourceNameToText sourceName

-- | Similar to 'askTableInfo', but drills further down to extract the
-- underlying core info.
askTableCoreInfo ::
  forall b m.
  (QErrM m, CacheRM m, Backend b) =>
  SourceName ->
  TableName b ->
  m (TableCoreInfo b)
askTableCoreInfo sourceName tableName =
  _tiCoreInfo <$> askTableInfo sourceName tableName

-- | Similar to 'askTableCoreInfo', but drills further down to extract the
-- underlying field info map.
askTableFieldInfoMap ::
  forall b m.
  (QErrM m, CacheRM m, Backend b) =>
  SourceName ->
  TableName b ->
  m (FieldInfoMap (FieldInfo b))
askTableFieldInfoMap sourceName tableName =
  _tciFieldInfoMap <$> askTableCoreInfo sourceName tableName

askLogicalModelCache ::
  forall b m.
  (Backend b, CacheRM m) =>
  SourceName ->
  m (Maybe (LogicalModelCache b))
askLogicalModelCache sourceName = do
  sources <- scSources <$> askSchemaCache
  pure $ unsafeSourceLogicalModels =<< HashMap.lookup sourceName sources

-- | Retrieves the metadata information about a table for a given source name
-- and table name.
--
-- Unlike most other @ask@ functions in this module, this function does not
-- drill through the schema cache, and instead inspects the metadata. Like most
-- others, it throws an error if it fails to find that source, in which case it
-- looks that source up in the metadata, to differentiate between the source not
-- existing or the type of the source not matching.
askTableMetadata ::
  forall b m.
  (QErrM m, MetadataM m, Backend b) =>
  SourceName ->
  TableName b ->
  m (TableMetadata b)
askTableMetadata sourceName tableName = do
  onNothingM (getMetadata <&> preview focusTableMetadata)
    $ throw400 NotExists
    $ "table "
    <> tableName
    <<> " does not exist in source: "
    <> sourceNameToText sourceName
  where
    focusTableMetadata :: Traversal' Metadata (TableMetadata b)
    focusTableMetadata =
      metaSources
        . ix sourceName
        . toSourceMetadata @b
        . smTables
        . ix tableName

-- | Retrieves the function cache for a given source cache and source name.
--
-- This function must be used with a _type annotation_, such as
-- `unsafeFunctionCache @('Postgres 'Vanilla)`. It returns @Nothing@ if it fails
-- to find that source or if the kind of the source does not match the type
-- annotation, and does not distinguish between the two cases.
unsafeFunctionCache ::
  forall b. (Backend b) => SourceName -> SourceCache -> Maybe (FunctionCache b)
unsafeFunctionCache sourceName cache =
  unsafeSourceFunctions @b =<< HashMap.lookup sourceName cache

-- | Retrieves the information about a function from the source cache, the
-- source name, and the function name.
--
-- This function returns @Nothing@ if it fails to find that source or if the
-- kind of the source does not match the type annotation, and does not
-- distinguish between the two cases.
unsafeFunctionInfo ::
  forall b. (Backend b) => SourceName -> FunctionName b -> SourceCache -> Maybe (FunctionInfo b)
unsafeFunctionInfo sourceName functionName cache =
  HashMap.lookup functionName =<< unsafeFunctionCache @b sourceName cache

-- | Retrieves the information about a function cache for a given source name
-- and function name.
--
-- This function retrieves the schema cache from the monadic context, and
-- attempts to look the corresponding source up in the source cache. It throws
-- an error if it fails to find that source, in which case it looks that source
-- up in the metadata, to differentiate between the source not existing or the
-- type of the source not matching.
askFunctionInfo ::
  forall b m.
  (QErrM m, CacheRM m, Backend b) =>
  SourceName ->
  FunctionName b ->
  m (FunctionInfo b)
askFunctionInfo sourceName functionName = do
  rawSchemaCache <- askSchemaCache
  onNothing (unsafeFunctionInfo sourceName functionName $ scSources rawSchemaCache)
    $ throw400 NotExists
    $ "function "
    <> functionName
    <<> " does not exist in source: "
    <> sourceNameToText sourceName

-------------------------------------------------------------------------------

newtype BackendInfoWrapper (b :: BackendType) = BackendInfoWrapper {unBackendInfoWrapper :: BackendInfo b}

deriving newtype instance (ToJSON (BackendInfo b)) => ToJSON (BackendInfoWrapper b)

deriving newtype instance (Semigroup (BackendInfo b)) => Semigroup (BackendInfoWrapper b)

deriving newtype instance (Monoid (BackendInfo b)) => Monoid (BackendInfoWrapper b)

type BackendCache = BackendMap BackendInfoWrapper

getBackendInfo :: forall b m. (CacheRM m, HasTag b) => m (Maybe (BackendInfo b))
getBackendInfo = askSchemaCache <&> fmap unBackendInfoWrapper . BackendMap.lookup @b . scBackendCache

-------------------------------------------------------------------------------

data SchemaCache = SchemaCache
  { scSources :: SourceCache,
    scActions :: ActionCache,
    scRemoteSchemas :: RemoteSchemaMap,
    scAllowlist :: InlinedAllowlist,
    scAdminIntrospection :: G.SchemaIntrospection,
    scGQLContext :: HashMap RoleName (RoleContext GQLContext),
    scUnauthenticatedGQLContext :: GQLContext,
    scRelayContext :: HashMap RoleName (RoleContext GQLContext),
    scUnauthenticatedRelayContext :: GQLContext,
    scDepMap :: DepMap,
    scInconsistentObjs :: [InconsistentMetadata],
    scCronTriggers :: HashMap.HashMap TriggerName CronTriggerInfo,
    scEndpoints :: EndpointTrie GQLQueryWithText,
    scApiLimits :: ApiLimit,
    scMetricsConfig :: MetricsConfig,
    scMetadataResourceVersion :: MetadataResourceVersion,
    scSetGraphqlIntrospectionOptions :: SetGraphqlIntrospectionOptions,
    scTlsAllowlist :: [TlsAllow],
    scQueryCollections :: QueryCollections,
    scBackendCache :: BackendCache,
    scSourceHealthChecks :: SourceHealthCheckCache,
    scSourcePingConfig :: SourcePingCache,
    scOpenTelemetryConfig :: OpenTelemetryInfo
  }

-- WARNING: this can only be used for debug purposes, as it loses all
-- backend-specific information in the process!
instance ToJSON SchemaCache where
  toJSON SchemaCache {..} =
    object
      [ "sources" .= toJSON (AB.debugAnyBackendToJSON <$> scSources),
        "actions" .= toJSON scActions,
        "remote_schemas" .= toJSON scRemoteSchemas,
        "allowlist" .= toJSON scAllowlist,
        "g_q_l_context" .= toJSON scGQLContext,
        "unauthenticated_g_q_l_context" .= toJSON scUnauthenticatedGQLContext,
        "relay_context" .= toJSON scRelayContext,
        "unauthenticated_relay_context" .= toJSON scUnauthenticatedRelayContext,
        "dep_map" .= toJSON scDepMap,
        "inconsistent_objs" .= toJSON scInconsistentObjs,
        "cron_triggers" .= toJSON scCronTriggers,
        "endpoints" .= toJSON scEndpoints,
        "api_limits" .= toJSON scApiLimits,
        "metrics_config" .= toJSON scMetricsConfig,
        "metadata_resource_version" .= toJSON scMetadataResourceVersion,
        "set_graphql_introspection_options" .= toJSON scSetGraphqlIntrospectionOptions,
        "tls_allowlist" .= toJSON scTlsAllowlist,
        "query_collection" .= toJSON scQueryCollections,
        "backend_cache" .= toJSON scBackendCache
      ]

getAllRemoteSchemas :: SchemaCache -> [RemoteSchemaName]
getAllRemoteSchemas sc =
  let consistentRemoteSchemas = HashMap.keys $ scRemoteSchemas sc
      inconsistentRemoteSchemas =
        getInconsistentRemoteSchemas $ scInconsistentObjs sc
   in consistentRemoteSchemas <> inconsistentRemoteSchemas

-- | A more limited version of 'CacheRM' that is used when building the schema cache, since the
-- entire schema cache has not been built yet.
class (Monad m) => TableCoreInfoRM b m where
  lookupTableCoreInfo :: TableName b -> m (Maybe (TableCoreInfo b))

instance (TableCoreInfoRM b m) => TableCoreInfoRM b (ReaderT r m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

instance (TableCoreInfoRM b m) => TableCoreInfoRM b (StateT s m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

instance (Monoid w, TableCoreInfoRM b m) => TableCoreInfoRM b (WriterT w m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

instance (TableCoreInfoRM b m) => TableCoreInfoRM b (TraceT m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

instance (TableCoreInfoRM b m) => TableCoreInfoRM b (LogicalModelFieldsLookupRT b m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

newtype TableCoreCacheRT b m a = TableCoreCacheRT {runTableCoreCacheRT :: TableCoreCache b -> m a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, Postgres.MonadTx)
    via (ReaderT (TableCoreCache b) m)
  deriving (MonadTrans) via (ReaderT (TableCoreCache b))

instance (MonadReader r m) => MonadReader r (TableCoreCacheRT b m) where
  ask = lift ask
  local f m = TableCoreCacheRT (local f . runTableCoreCacheRT m)

instance (Monad m, Backend b) => TableCoreInfoRM b (TableCoreCacheRT b m) where
  lookupTableCoreInfo tableName =
    TableCoreCacheRT (pure . HashMap.lookup tableName)

-- | All our RQL DML queries operate over a single source. This typeclass facilitates that.
class (TableCoreInfoRM b m) => TableInfoRM b m where
  lookupTableInfo :: TableName b -> m (Maybe (TableInfo b))

instance (TableInfoRM b m) => TableInfoRM b (ReaderT r m) where
  lookupTableInfo tableName = lift $ lookupTableInfo tableName

instance (TableInfoRM b m) => TableInfoRM b (StateT s m) where
  lookupTableInfo tableName = lift $ lookupTableInfo tableName

instance (Monoid w, TableInfoRM b m) => TableInfoRM b (WriterT w m) where
  lookupTableInfo tableName = lift $ lookupTableInfo tableName

instance (TableInfoRM b m) => TableInfoRM b (TraceT m) where
  lookupTableInfo tableName = lift $ lookupTableInfo tableName

instance (TableInfoRM b m) => TableInfoRM b (LogicalModelFieldsLookupRT b m) where
  lookupTableInfo = lift . lookupTableInfo

newtype TableCacheRT b m a = TableCacheRT {runTableCacheRT :: TableCache b -> m a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, Postgres.MonadTx, UserInfoM)
    via (ReaderT (TableCache b) m)
  deriving (MonadTrans) via (ReaderT (TableCache b))

instance (Monad m, Backend b) => TableCoreInfoRM b (TableCacheRT b m) where
  lookupTableCoreInfo tableName =
    TableCacheRT (pure . fmap _tiCoreInfo . HashMap.lookup tableName)

instance (Monad m, Backend b) => TableInfoRM b (TableCacheRT b m) where
  lookupTableInfo tableName =
    TableCacheRT (pure . HashMap.lookup tableName)

class (Monad m) => CacheRM m where
  askSchemaCache :: m SchemaCache

instance (CacheRM m) => CacheRM (ReaderT r m) where
  askSchemaCache = lift askSchemaCache

instance (CacheRM m) => CacheRM (StateT s m) where
  askSchemaCache = lift askSchemaCache

instance (Monoid w, CacheRM m) => CacheRM (WriterT w m) where
  askSchemaCache = lift askSchemaCache

instance (CacheRM m) => CacheRM (TraceT m) where
  askSchemaCache = lift askSchemaCache

instance (CacheRM m) => CacheRM (PG.TxET QErr m) where
  askSchemaCache = lift askSchemaCache

instance (CacheRM m) => CacheRM (MSSQL.TxET e m) where
  askSchemaCache = lift askSchemaCache

getDependentObjs :: SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjs = getDependentObjsWith (const True)

getDependentObjsWith ::
  (DependencyReason -> Bool) -> SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjsWith f sc objId =
  map fst $ filter (isDependency . snd) $ HashMap.toList $ scDepMap sc
  where
    isDependency deps = not
      $ HS.null
      $ flip HS.filter deps
      $ \(SchemaDependency depId reason) -> objId `induces` depId && f reason
    -- induces a b : is b dependent on a
    induces (SOSource s1) (SOSource s2) = s1 == s2
    induces (SOSource s1) (SOSourceObj s2 _) = s1 == s2
    induces o1@(SOSourceObj s1 e1) o2@(SOSourceObj s2 e2) =
      s1 == s2 && fromMaybe (o1 == o2) (AB.composeAnyBackend @Backend go e1 e2 Nothing)
    induces o1 o2 = o1 == o2

    go (SOITable tn1) (SOITable tn2) = Just $ tn1 == tn2
    go (SOITable tn1) (SOITableObj tn2 _) = Just $ tn1 == tn2
    go _ _ = Nothing

-- | Compute all remote dependencies on a source.
--
-- Given a source name, this function computes all of its dependencies, direct
-- or indirect, and returns all of the dependencies that are not "local" to the
-- source, i.e. that belong to another source or to a remote schema, here dubbed
-- "remote dependencies".
--
-- This functions returns a 'SchemaObjId' for each such dependency, but makes no
-- attempt at extracting the underlying `SourceObjId` (if any), for two reasons:
--   1. a `SourceObjId` no longer contains the source name, which most callers
--      need to identify where the corresponding dependency is
--   2. this would prevent us from returning remote schema dependencies, which
--      by definition do not have a corresponding `SourceObjId`
getRemoteDependencies ::
  SchemaCache ->
  SourceName ->
  [SchemaObjId]
getRemoteDependencies schemaCache sourceName =
  filter isRemoteDep $ getDependentObjs schemaCache (SOSource sourceName)
  where
    isRemoteDep = \case
      SOSourceObj sn _
        -- only true if the dependency is in another source
        | sn /= sourceName -> True
        | otherwise -> False
      SORemoteSchemaRemoteRelationship {} -> True
      -- those relationshipss either do not exist or do not qualify as remote
      SOSource {} -> False
      SORemoteSchema {} -> False
      SORemoteSchemaPermission {} -> False
      SORole {} -> False

-- | What schema dependencies does a given row permission for a logical model
-- have? This will almost certainly involve some number of dependencies on
-- logical models, but may also involve dependencies on tables. Although we
-- can't relate tables and logical models yet, we can still declare permissions
-- like, "you can only see this logical model if your user ID exists in this
-- table".
getLogicalModelBoolExpDeps ::
  forall b.
  (GetAggregationPredicatesDeps b) =>
  SourceName ->
  LogicalModelLocation ->
  AnnBoolExpPartialSQL b ->
  [SchemaDependency]
getLogicalModelBoolExpDeps source logicalModelLocation = \case
  BoolAnd exps -> concatMap (getLogicalModelBoolExpDeps source logicalModelLocation) exps
  BoolOr exps -> concatMap (getLogicalModelBoolExpDeps source logicalModelLocation) exps
  BoolNot e -> getLogicalModelBoolExpDeps source logicalModelLocation e
  BoolField fld -> getLogicalModelColExpDeps source logicalModelLocation fld
  BoolExists (GExists refqt whereExp) -> do
    let table :: SchemaObjId
        table = SOSourceObj source $ AB.mkAnyBackend $ SOITable @b refqt

    SchemaDependency table DRRemoteTable : getBoolExpDeps source refqt whereExp

-- | What schema dependencies does this row permission for a particular column
-- within a logical model have? This is a fairly simple function at the moment
-- as there's only one type of column: columns! As a result, we have no
-- dependencies from relationships, computed fields, or aggregation predicates,
-- as none of these things are supported.
getLogicalModelColExpDeps ::
  forall b.
  (GetAggregationPredicatesDeps b) =>
  SourceName ->
  LogicalModelLocation ->
  AnnBoolExpFld b (PartialSQLExp b) ->
  [SchemaDependency]
getLogicalModelColExpDeps source logicalModelLocation = \case
  AVRelationship {} -> []
  AVComputedField _ -> []
  AVAggregationPredicates _ -> []
  AVNestedObject NestedObjectInfo {..} boolExp ->
    getLogicalModelBoolExpDeps source (LMLLogicalModel _noiType) boolExp
  AVColumn colInfo _redactionExp opExps -> do
    let columnName :: Column b
        columnName = ciColumn colInfo

        -- Do we depend on /any/ arbitrary SQL expression, or are /all/ our
        -- dependencies on session variables?
        colDepReason :: DependencyReason
        colDepReason = bool DRSessionVariable DROnType (any hasStaticExp opExps)

        colDep :: SchemaDependency
        colDep = mkLogicalModelColDep @b colDepReason source logicalModelLocation columnName

    colDep : getLogicalModelOpExpDeps source logicalModelLocation opExps
  AVRemoteRelationship {} -> []

-- | Discover the schema dependencies of an @AnnBoolExpPartialSQL@.
getBoolExpDeps ::
  forall b.
  (GetAggregationPredicatesDeps b) =>
  SourceName ->
  TableName b ->
  AnnBoolExpPartialSQL b ->
  [SchemaDependency]
getBoolExpDeps source tableName =
  runBoolExpM (BoolExpCtx {source = source, currTable = tableName, rootTable = tableName}) . getBoolExpDeps'

getBoolExpDeps' ::
  forall b.
  (GetAggregationPredicatesDeps b) =>
  AnnBoolExpPartialSQL b ->
  BoolExpM b [SchemaDependency]
getBoolExpDeps' = \case
  BoolAnd exps -> procExps exps
  BoolOr exps -> procExps exps
  BoolNot e -> getBoolExpDeps' e
  BoolField fld -> getColExpDeps fld
  BoolExists (GExists refqt whereExp) -> do
    BoolExpCtx {source} <- ask
    let tableDep =
          SchemaDependency
            ( SOSourceObj source
                $ AB.mkAnyBackend
                $ SOITable @b refqt
            )
            DRRemoteTable
    (tableDep :) <$> local (\e -> e {currTable = refqt}) (getBoolExpDeps' whereExp)
  where
    procExps :: [AnnBoolExpPartialSQL b] -> BoolExpM b [SchemaDependency]
    procExps = fmap concat . mapM getBoolExpDeps'

getColExpDeps ::
  forall b.
  (GetAggregationPredicatesDeps b) =>
  AnnBoolExpFld b (PartialSQLExp b) ->
  BoolExpM b [SchemaDependency]
getColExpDeps bexp = do
  BoolExpCtx {source, currTable} <- ask
  case bexp of
    AVColumn colInfo _redactionExp opExps ->
      let columnName = ciColumn colInfo
          colDepReason = bool DRSessionVariable DROnType $ any hasStaticExp opExps
          colDep = mkColDep @b colDepReason source currTable columnName
       in (colDep :) <$> getOpExpDeps opExps
    AVNestedObject NestedObjectInfo {..} boolExp ->
      pure $ getLogicalModelBoolExpDeps source (LMLLogicalModel _noiType) boolExp
    AVRelationship relInfo RelationshipFilters {rfTargetTablePermissions, rfFilter} ->
      case riTarget relInfo of
        RelTargetNativeQuery _ -> error "getColExpDeps RelTargetNativeQuery"
        RelTargetTable relationshipTable ->
          let relationshipName = riName relInfo
              schemaDependency =
                SchemaDependency
                  ( SOSourceObj source
                      $ AB.mkAnyBackend
                      $ SOITableObj @b currTable (TORel relationshipName)
                  )
                  DROnType
           in do
                boolExpDeps <- local (\e -> e {currTable = relationshipTable}) (getBoolExpDeps' rfFilter)
                permDeps <-
                  local
                    ( \e ->
                        e
                          { currTable = relationshipTable,
                            rootTable = relationshipTable
                          }
                    )
                    (getBoolExpDeps' rfTargetTablePermissions)
                return (schemaDependency : boolExpDeps <> permDeps)
    AVComputedField computedFieldBoolExp ->
      let mkComputedFieldDep' r =
            mkComputedFieldDep @b r source currTable $ _acfbName computedFieldBoolExp
       in case _acfbBoolExp computedFieldBoolExp of
            CFBEScalar _redactionExp opExps ->
              let computedFieldDep =
                    mkComputedFieldDep'
                      $ bool DRSessionVariable DROnType
                      $ any hasStaticExp opExps
               in (computedFieldDep :) <$> getOpExpDeps opExps
            CFBETable cfTable cfTableBoolExp ->
              (mkComputedFieldDep' DROnType :) <$> local (\e -> e {currTable = cfTable}) (getBoolExpDeps' cfTableBoolExp)
    AVAggregationPredicates aggPreds -> getAggregationPredicateDeps aggPreds
    AVRemoteRelationship remoteRelPermBoolExp -> do
      sourceName <- AB.dispatchAnyBackend @Backend (rhsFetchInfo remoteRelPermBoolExp) \(RemoteRelRHSFetchInfo {..}) -> pure rrrfiSource
      let sourceColumnObject =
            AB.mapBackend
              (rhsFetchInfo remoteRelPermBoolExp)
              ( \RemoteRelRHSFetchInfo {..} ->
                  SOITableObj rrrfiTable (TOCol (rrrfweColumnFieldName rrrfiWhere))
              )
      pure
        $ [SchemaDependency (SOSourceObj sourceName sourceColumnObject) DRRemoteRelationship]

getOpExpDeps ::
  forall b.
  (Backend b) =>
  [OpExpG b (PartialSQLExp b)] ->
  BoolExpM b [SchemaDependency]
getOpExpDeps opExps = do
  BoolExpCtx {source, rootTable, currTable} <- ask
  pure $ do
    RootOrCurrentColumn rootOrCol col <- mapMaybe opExpDepCol opExps
    let table = case rootOrCol of
          IsRoot -> rootTable
          IsCurrent -> currTable
    pure $ mkColDep @b DROnType source table col

-- | What dependencies does this row permission for a logical model have? This
-- is really a utility function for the tree of dependency traversals under
-- 'getLogicalModelBoolExpDeps', specifically focusing on boolean operators.
getLogicalModelOpExpDeps ::
  forall b.
  (Backend b) =>
  SourceName ->
  LogicalModelLocation ->
  [OpExpG b (PartialSQLExp b)] ->
  [SchemaDependency]
getLogicalModelOpExpDeps source logicalModelLocation operatorExpressions = do
  RootOrCurrentColumn _ column <- mapMaybe opExpDepCol operatorExpressions
  pure (mkLogicalModelColDep @b DROnType source logicalModelLocation column)

-- | Asking for a table's fields info without explicit @'SourceName' argument.
-- The source name is implicitly inferred from @'SourceM' via @'TableCoreInfoRM'.
askFieldInfoMapSource ::
  (QErrM m, Backend b, TableCoreInfoRM b m) =>
  TableName b ->
  m (FieldInfoMap (FieldInfo b))
askFieldInfoMapSource tableName = do
  fmap _tciFieldInfoMap
    $ onNothingM (lookupTableCoreInfo tableName)
    $ throw400 NotExists
    $ "table "
    <> tableName
    <<> " does not exist"
