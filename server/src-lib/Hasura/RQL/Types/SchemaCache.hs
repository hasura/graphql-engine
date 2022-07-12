{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
-- As of GHC 8.6, a use of DefaultSignatures in this module triggers a false positive for this
-- warning, so don’t treat it as an error even if -Werror is enabled.
--
-- TODO: Restructure code so this error can be downgraded to a warning for a
-- much smaller module footprint.
{-# OPTIONS_GHC -Wwarn=redundant-constraints #-}

module Hasura.RQL.Types.SchemaCache
  ( SchemaCache (..),
    SchemaCacheVer,
    initSchemaCacheVer,
    incSchemaCacheVer,
    TableConfig (..),
    emptyTableConfig,
    getAllRemoteSchemas,
    unsafeFunctionCache,
    unsafeFunctionInfo,
    unsafeTableCache,
    unsafeTableInfo,
    askSourceInfo,
    askSourceConfig,
    askTableCache,
    askTableInfo,
    askTableCoreInfo,
    askTableFieldInfoMap,
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
    tciSystemDefined,
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
    RemoteSchemaCtx (..),
    getIntrospectionResult,
    rscName,
    rscInfo,
    rscIntroOriginal,
    rscRawIntrospectionResult,
    rscPermissions,
    rscRemoteRelationships,
    RemoteSchemaMap,
    DepMap,
    WithDeps,
    SourceM (..),
    SourceT (..),
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
    RelInfo (..),
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
    mkColDep,
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
    initialResourceVersion,
    getBoolExpDeps,
    InlinedAllowlist,
  )
where

import Control.Lens (Traversal', at, makeLenses, preview, (^.))
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as HS
import Data.Int (Int64)
import Data.Text.Extended
import Database.MSSQL.Transaction qualified as MSSQL
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Base.Error
import Hasura.GraphQL.Context (GQLContext, RoleContext)
import Hasura.Incremental
  ( Cacheable,
    Dependency,
    MonadDepend (..),
    selectKeyD,
  )
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.ApiLimit
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Network (TlsAllow)
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.ScheduledTrigger
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Types
import Hasura.Session
import Hasura.Tracing (TraceT)
import Language.GraphQL.Draft.Syntax qualified as G
import System.Cron.Types

newtype MetadataResourceVersion = MetadataResourceVersion
  { getMetadataResourceVersion :: Int64
  }
  deriving (Show, Eq, Num, FromJSON, ToJSON)

initialResourceVersion :: MetadataResourceVersion
initialResourceVersion = MetadataResourceVersion 0

mkParentDep ::
  forall b.
  Backend b =>
  SourceName ->
  TableName b ->
  SchemaDependency
mkParentDep s tn =
  SchemaDependency (SOSourceObj s $ AB.mkAnyBackend @b (SOITable tn)) DRTable

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

type WithDeps a = (a, [SchemaDependency])

data IntrospectionResult = IntrospectionResult
  { irDoc :: !RemoteSchemaIntrospection,
    irQueryRoot :: !G.Name,
    irMutationRoot :: !(Maybe G.Name),
    irSubscriptionRoot :: !(Maybe G.Name)
  }
  deriving (Show, Eq, Generic)

instance Cacheable IntrospectionResult

type RemoteSchemaRelationships =
  InsOrdHashMap G.Name (InsOrdHashMap RelName (RemoteFieldInfo G.Name))

-- | See 'fetchRemoteSchema'.
data RemoteSchemaCtx = RemoteSchemaCtx
  { _rscName :: !RemoteSchemaName,
    -- | Original remote schema without customizations
    _rscIntroOriginal :: !IntrospectionResult,
    _rscInfo :: !RemoteSchemaInfo,
    -- | The raw response from the introspection query against the remote server.
    -- We store this so we can efficiently service 'introspect_remote_schema'.
    _rscRawIntrospectionResult :: !BL.ByteString,
    _rscPermissions :: !(M.HashMap RoleName IntrospectionResult),
    _rscRemoteRelationships :: RemoteSchemaRelationships
  }

getIntrospectionResult :: RemoteSchemaPermsCtx -> RoleName -> RemoteSchemaCtx -> Maybe IntrospectionResult
getIntrospectionResult remoteSchemaPermsCtx role remoteSchemaContext =
  if
      | -- admin doesn't have a custom annotated introspection, defaulting to the original one
        role == adminRoleName ->
        pure $ _rscIntroOriginal remoteSchemaContext
      | -- if permissions are disabled, the role map will be empty, defaulting to the original one
        remoteSchemaPermsCtx == RemoteSchemaPermsDisabled ->
        pure $ _rscIntroOriginal remoteSchemaContext
      | -- otherwise, look the role up in the map; if we find nothing, then the role doesn't have access
        otherwise ->
        M.lookup role (_rscPermissions remoteSchemaContext)

$(makeLenses ''RemoteSchemaCtx)

instance ToJSON RemoteSchemaCtx where
  toJSON RemoteSchemaCtx {..} =
    object $
      [ "name" .= _rscName,
        "info" .= toJSON _rscInfo
      ]

type RemoteSchemaMap = M.HashMap RemoteSchemaName RemoteSchemaCtx

type DepMap = M.HashMap SchemaObjId (HS.HashSet SchemaDependency)

data CronTriggerInfo = CronTriggerInfo
  { ctiName :: !TriggerName,
    ctiSchedule :: !CronSchedule,
    ctiPayload :: !(Maybe Value),
    ctiRetryConf :: !STRetryConf,
    ctiWebhookInfo :: !(EnvRecord ResolvedWebhook),
    ctiHeaders :: ![EventHeaderInfo],
    ctiComment :: !(Maybe Text),
    ctiRequestTransform :: !(Maybe RequestTransform),
    ctiResponseTransform :: !(Maybe MetadataResponseTransform)
  }
  deriving (Show, Eq)

$(deriveToJSON hasuraJSON ''CronTriggerInfo)

newtype SchemaCacheVer = SchemaCacheVer {unSchemaCacheVer :: Word64}
  deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON)

initSchemaCacheVer :: SchemaCacheVer
initSchemaCacheVer = SchemaCacheVer 0

incSchemaCacheVer :: SchemaCacheVer -> SchemaCacheVer
incSchemaCacheVer (SchemaCacheVer prev) =
  SchemaCacheVer $ prev + 1

type ActionCache = M.HashMap ActionName ActionInfo -- info of all actions

type InheritedRolesCache = M.HashMap RoleName (HashSet RoleName)

-------------------------------------------------------------------------------

-- | Retrieves the source info for a given source name.
--
-- This function retrieves the schema cache from the monadic context, and
-- attempts to look the corresponding source up in the source cache. This
-- function must be used with a _type annotation_, such as `askSourceInfo
-- @('Postgres 'Vanilla)`. It throws an error if it fails to find that source,
-- in which case it looks that source up in the metadata, to differentiate
-- between the source not existing or the type of the source not matching.
askSourceInfo ::
  forall b m.
  (CacheRM m, MetadataM m, MonadError QErr m, Backend b) =>
  SourceName ->
  m (SourceInfo b)
askSourceInfo sourceName = do
  sources <- scSources <$> askSchemaCache
  onNothing (unsafeSourceInfo @b =<< M.lookup sourceName sources) do
    metadata <- getMetadata
    case metadata ^. metaSources . at sourceName of
      Nothing ->
        throw400 NotExists $ "source with name " <> sourceName <<> " does not exist"
      Just _ ->
        throw400 Unexpected $ "source with name " <> sourceName <<> " is inconsistent"

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

-- | Retrieves the table cache for a given source cache and source name.
--
-- This function must be used with a _type annotation_, such as
-- `unsafeTableCache @('Postgres 'Vanilla)`. It returns @Nothing@ if it fails to
-- find that source or if the kind of the source does not match the type
-- annotation, and does not distinguish between the two cases.
unsafeTableCache ::
  forall b. Backend b => SourceName -> SourceCache -> Maybe (TableCache b)
unsafeTableCache sourceName cache = do
  unsafeSourceTables @b =<< M.lookup sourceName cache

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
  pure $ unsafeSourceTables =<< M.lookup sourceName sources

-- | Retrieves the information about a table from the source cache, the source
-- name, and the table name.
--
-- This function returns @Nothing@ if it fails to find that source or if the
-- kind of the source does not match the type annotation, and does not
-- distinguish between the two cases.
unsafeTableInfo ::
  forall b. Backend b => SourceName -> TableName b -> SourceCache -> Maybe (TableInfo b)
unsafeTableInfo sourceName tableName cache =
  M.lookup tableName =<< unsafeTableCache @b sourceName cache

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
  onNothing (unsafeTableInfo sourceName tableName $ scSources rawSchemaCache) $
    throw400 NotExists $ "table " <> tableName <<> " does not exist in source: " <> sourceNameToText sourceName

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
  onNothingM (getMetadata <&> preview focusTableMetadata) $
    throw400 NotExists $ "table " <> tableName <<> " does not exist in source: " <> sourceNameToText sourceName
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
  forall b. Backend b => SourceName -> SourceCache -> Maybe (FunctionCache b)
unsafeFunctionCache sourceName cache =
  unsafeSourceFunctions @b =<< M.lookup sourceName cache

-- | Retrieves the information about a function from the source cache, the
-- source name, and the function name.
--
-- This function returns @Nothing@ if it fails to find that source or if the
-- kind of the source does not match the type annotation, and does not
-- distinguish between the two cases.
unsafeFunctionInfo ::
  forall b. Backend b => SourceName -> FunctionName b -> SourceCache -> Maybe (FunctionInfo b)
unsafeFunctionInfo sourceName functionName cache =
  M.lookup functionName =<< unsafeFunctionCache @b sourceName cache

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
  onNothing (unsafeFunctionInfo sourceName functionName $ scSources rawSchemaCache) $
    throw400 NotExists $ "function " <> functionName <<> " does not exist in source: " <> sourceNameToText sourceName

-------------------------------------------------------------------------------

data SchemaCache = SchemaCache
  { scSources :: !SourceCache,
    scActions :: !ActionCache,
    scRemoteSchemas :: !RemoteSchemaMap,
    scAllowlist :: !InlinedAllowlist,
    scAdminIntrospection :: !G.SchemaIntrospection,
    scGQLContext :: !(HashMap RoleName (RoleContext GQLContext)),
    scUnauthenticatedGQLContext :: !GQLContext,
    scRelayContext :: !(HashMap RoleName (RoleContext GQLContext)),
    scUnauthenticatedRelayContext :: !GQLContext,
    scDepMap :: !DepMap,
    scInconsistentObjs :: ![InconsistentMetadata],
    scCronTriggers :: !(M.HashMap TriggerName CronTriggerInfo),
    scEndpoints :: !(EndpointTrie GQLQueryWithText),
    scApiLimits :: !ApiLimit,
    scMetricsConfig :: !MetricsConfig,
    scMetadataResourceVersion :: !(Maybe MetadataResourceVersion),
    scSetGraphqlIntrospectionOptions :: !SetGraphqlIntrospectionOptions,
    scTlsAllowlist :: ![TlsAllow],
    scQueryCollections :: !QueryCollections
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
        "query_collection" .= toJSON scQueryCollections
      ]

getAllRemoteSchemas :: SchemaCache -> [RemoteSchemaName]
getAllRemoteSchemas sc =
  let consistentRemoteSchemas = M.keys $ scRemoteSchemas sc
      inconsistentRemoteSchemas =
        getInconsistentRemoteSchemas $ scInconsistentObjs sc
   in consistentRemoteSchemas <> inconsistentRemoteSchemas

class (Monad m) => SourceM m where
  askCurrentSource :: m SourceName

instance (SourceM m) => SourceM (ReaderT r m) where
  askCurrentSource = lift askCurrentSource

instance (SourceM m) => SourceM (StateT s m) where
  askCurrentSource = lift askCurrentSource

instance (Monoid w, SourceM m) => SourceM (WriterT w m) where
  askCurrentSource = lift askCurrentSource

instance (SourceM m) => SourceM (TraceT m) where
  askCurrentSource = lift askCurrentSource

newtype SourceT m a = SourceT {runSourceT :: SourceName -> m a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, PG.MonadTx, TableCoreInfoRM b, CacheRM)
    via (ReaderT SourceName m)
  deriving (MonadTrans) via (ReaderT SourceName)

instance (Monad m) => SourceM (SourceT m) where
  askCurrentSource = SourceT pure

-- | A more limited version of 'CacheRM' that is used when building the schema cache, since the
-- entire schema cache has not been built yet.
class (SourceM m) => TableCoreInfoRM b m where
  lookupTableCoreInfo :: TableName b -> m (Maybe (TableCoreInfo b))

instance (TableCoreInfoRM b m) => TableCoreInfoRM b (ReaderT r m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

instance (TableCoreInfoRM b m) => TableCoreInfoRM b (StateT s m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

instance (Monoid w, TableCoreInfoRM b m) => TableCoreInfoRM b (WriterT w m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

instance (TableCoreInfoRM b m) => TableCoreInfoRM b (TraceT m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

newtype TableCoreCacheRT b m a = TableCoreCacheRT {runTableCoreCacheRT :: (SourceName, Dependency (TableCoreCache b)) -> m a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, PG.MonadTx)
    via (ReaderT (SourceName, Dependency (TableCoreCache b)) m)
  deriving (MonadTrans) via (ReaderT (SourceName, Dependency (TableCoreCache b)))

instance (MonadReader r m) => MonadReader r (TableCoreCacheRT b m) where
  ask = lift ask
  local f m = TableCoreCacheRT (local f . runTableCoreCacheRT m)

instance (Monad m) => SourceM (TableCoreCacheRT b m) where
  askCurrentSource =
    TableCoreCacheRT (pure . fst)

instance (MonadDepend m, Backend b) => TableCoreInfoRM b (TableCoreCacheRT b m) where
  lookupTableCoreInfo tableName =
    TableCoreCacheRT (dependOnM . selectKeyD tableName . snd)

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

newtype TableCacheRT b m a = TableCacheRT {runTableCacheRT :: (SourceName, TableCache b) -> m a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, PG.MonadTx)
    via (ReaderT (SourceName, TableCache b) m)
  deriving (MonadTrans) via (ReaderT (SourceName, TableCache b))

instance (UserInfoM m) => UserInfoM (TableCacheRT b m) where
  askUserInfo = lift askUserInfo

instance (Monad m) => SourceM (TableCacheRT b m) where
  askCurrentSource =
    TableCacheRT (pure . fst)

instance (Monad m, Backend b) => TableCoreInfoRM b (TableCacheRT b m) where
  lookupTableCoreInfo tableName =
    TableCacheRT (pure . fmap _tiCoreInfo . M.lookup tableName . snd)

instance (Monad m, Backend b) => TableInfoRM b (TableCacheRT b m) where
  lookupTableInfo tableName =
    TableCacheRT (pure . M.lookup tableName . snd)

instance (HasServerConfigCtx m) => HasServerConfigCtx (TableCacheRT b m) where
  askServerConfigCtx = lift askServerConfigCtx

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

instance (CacheRM m) => CacheRM (Q.TxET QErr m) where
  askSchemaCache = lift askSchemaCache

instance (CacheRM m) => CacheRM (MSSQL.TxET e m) where
  askSchemaCache = lift askSchemaCache

getDependentObjs :: SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjs = getDependentObjsWith (const True)

getDependentObjsWith ::
  (DependencyReason -> Bool) -> SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjsWith f sc objId =
  map fst $ filter (isDependency . snd) $ M.toList $ scDepMap sc
  where
    isDependency deps = not $
      HS.null $
        flip HS.filter deps $
          \(SchemaDependency depId reason) -> objId `induces` depId && f reason
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

-- | The table type context of schema dependency discovery. Boolean expressions
-- may refer to a so-called 'root table' (identified by a '$'-sign in the
-- expression input syntax) or the 'current' table.
data BoolExpCtx b = BoolExpCtx
  { source :: SourceName,
    -- | Reference to the 'current' table type.
    currTable :: TableName b,
    -- | Reference to the 'root' table type.
    rootTable :: TableName b
  }

-- | Discover the schema dependencies of an @AnnBoolExpPartialSQL@.
getBoolExpDeps ::
  forall b.
  Backend b =>
  SourceName ->
  TableName b ->
  AnnBoolExpPartialSQL b ->
  [SchemaDependency]
getBoolExpDeps source tableName =
  runBoolExpM (BoolExpCtx {source = source, currTable = tableName, rootTable = tableName}) . getBoolExpDeps'

-- | The monad for doing schema dependency discovery for boolean expressions.
-- maintains the table context of the expressions being translated.
newtype BoolExpM b a = BoolExpM {unBoolExpM :: Reader (BoolExpCtx b) a}
  deriving (Functor, Applicative, Monad, MonadReader (BoolExpCtx b))

runBoolExpM :: BoolExpCtx b -> BoolExpM b a -> a
runBoolExpM ctx = flip runReader ctx . unBoolExpM

getBoolExpDeps' ::
  forall b.
  Backend b =>
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
            ( SOSourceObj source $
                AB.mkAnyBackend $
                  SOITable @b refqt
            )
            DRRemoteTable
    (tableDep :) <$> local (\e -> e {currTable = refqt}) (getBoolExpDeps' whereExp)
  where
    procExps :: [AnnBoolExpPartialSQL b] -> BoolExpM b [SchemaDependency]
    procExps = fmap concat . mapM getBoolExpDeps'

getColExpDeps ::
  forall b.
  Backend b =>
  AnnBoolExpFld b (PartialSQLExp b) ->
  BoolExpM b [SchemaDependency]
getColExpDeps bexp = do
  BoolExpCtx {source, currTable} <- ask
  case bexp of
    AVColumn colInfo opExps ->
      let columnName = ciColumn colInfo
          colDepReason = bool DRSessionVariable DROnType $ any hasStaticExp opExps
          colDep = mkColDep @b colDepReason source currTable columnName
       in (colDep :) <$> mkOpExpDeps opExps
    AVRelationship relInfo relBoolExp ->
      let relationshipName = riName relInfo
          relationshipTable = riRTable relInfo
          schemaDependency =
            SchemaDependency
              ( SOSourceObj source $
                  AB.mkAnyBackend $
                    SOITableObj @b currTable (TORel relationshipName)
              )
              DROnType
       in (schemaDependency :) <$> local (\e -> e {currTable = relationshipTable}) (getBoolExpDeps' relBoolExp)
    AVComputedField computedFieldBoolExp ->
      let mkComputedFieldDep' r =
            mkComputedFieldDep @b r source currTable $ _acfbName computedFieldBoolExp
       in case _acfbBoolExp computedFieldBoolExp of
            CFBEScalar opExps ->
              let computedFieldDep =
                    mkComputedFieldDep' $
                      bool DRSessionVariable DROnType $ any hasStaticExp opExps
               in (computedFieldDep :) <$> mkOpExpDeps opExps
            CFBETable cfTable cfTableBoolExp ->
              (mkComputedFieldDep' DROnType :) <$> local (\e -> e {currTable = cfTable}) (getBoolExpDeps' cfTableBoolExp)
  where
    mkOpExpDeps :: [OpExpG b (PartialSQLExp b)] -> BoolExpM b [SchemaDependency]
    mkOpExpDeps opExps = do
      BoolExpCtx {source, rootTable, currTable} <- ask
      pure $ do
        RootOrCurrentColumn rootOrCol col <- mapMaybe opExpDepCol opExps
        let table = case rootOrCol of
              IsRoot -> rootTable
              IsCurrent -> currTable
        pure $ mkColDep @b DROnType source table col

-- | Asking for a table's fields info without explicit @'SourceName' argument.
-- The source name is implicitly inferred from @'SourceM' via @'TableCoreInfoRM'.
askFieldInfoMapSource ::
  (QErrM m, Backend b, TableCoreInfoRM b m) =>
  TableName b ->
  m (FieldInfoMap (FieldInfo b))
askFieldInfoMapSource tableName = do
  fmap _tciFieldInfoMap $
    onNothingM (lookupTableCoreInfo tableName) $
      throw400 NotExists $ "table " <> tableName <<> " does not exist"
