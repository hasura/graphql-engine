-- As of GHC 8.6, a use of DefaultSignatures in this module triggers a false positive for this
-- warning, so don’t treat it as an error even if -Werror is enabled.
{-# OPTIONS_GHC -Wwarn=redundant-constraints #-}

{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.SchemaCache
  ( SchemaCache(..)
  , SchemaCacheVer
  , initSchemaCacheVer
  , incSchemaCacheVer
  , TableConfig(..)
  , emptyTableConfig
  , getAllRemoteSchemas
  , unsafeFunctionCache
  , unsafeFunctionInfo
  , unsafeTableCache
  , unsafeTableInfo

  , TableCoreCache
  , TableCache
  , ActionCache

  , TypeRelationship(..)
  , trName, trType, trRemoteTable, trFieldMapping
  , TableCoreInfoG(..)
  , TableCoreInfo
  , tciName
  , tciDescription
  , tciSystemDefined
  , tciFieldInfoMap
  , tciPrimaryKey
  , tciUniqueConstraints
  , tciForeignKeys
  , tciViewInfo
  , tciEnumValues
  , tciCustomConfig
  , tciUniqueOrPrimaryKeyConstraints

  , TableInfo(..)
  , tiCoreInfo
  , tiRolePermInfoMap
  , tiEventTriggerInfoMap

  , ViewInfo(..)
  , isMutable
  , mutableView

  , IntrospectionResult(..)
  , ParsedIntrospection(..)
  , RemoteSchemaCtx(..)
  , rscName
  , rscInfo
  , rscIntro
  , rscParsed
  , rscRawIntrospectionResult
  , rscPermissions
  , RemoteSchemaMap

  , DepMap
  , WithDeps

  , SourceM(..)
  , SourceT(..)
  , TableCoreInfoRM(..)
  , TableCoreCacheRT(..)
  , TableInfoRM(..)
  , TableCacheRT(..)
  , CacheRM(..)

  , FieldInfoMap
  , FieldInfo(..)
  , _FIColumn
  , _FIRelationship
  , _FIComputedField
  , fieldInfoName
  , fieldInfoGraphQLNames
  , getCols
  , getRels
  , getComputedFieldInfos

  , isPGColInfo
  , RelInfo(..)

  , RolePermInfo(..)
  , mkRolePermInfo
  , permIns
  , permSel
  , permUpd
  , permDel
  , PermAccessor(..)
  , permAccToLens
  , permAccToType
  , withPermType
  , RolePermInfoMap

  , InsPermInfo(..)
  , SelPermInfo(..)
  , getSelectPermissionInfoM
  , UpdPermInfo(..)
  , DelPermInfo(..)
  , PreSetColsPartial

  , EventTriggerInfo(..)
  , EventTriggerInfoMap

  , TableObjId(..)
  , SchemaObjId(..)
  , reportSchemaObj
  , reportSchemaObjs
  , DependencyReason(..)
  , SchemaDependency(..)
  , mkParentDep
  , mkColDep
  , mkComputedFieldDep
  , getDependentObjs
  , getDependentObjsWith

  , FunctionVolatility(..)
  , FunctionArg(..)
  , FunctionArgName(..)
--  , FunctionName(..)
  , FunctionInfo(..)
  , FunctionCache
  , CronTriggerInfo(..)
  ) where

import           Hasura.Prelude

import qualified Data.ByteString.Lazy                as BL
import qualified Data.HashMap.Strict                 as M
import qualified Data.HashSet                        as HS
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Lens                        (makeLenses)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text.Extended
import           Data.Typeable                       (cast)
import           System.Cron.Types

import qualified Hasura.Backends.Postgres.Connection as PG
import qualified Hasura.GraphQL.Parser               as P

import           Hasura.GraphQL.Context              (GQLContext, RemoteField, RoleContext)
import           Hasura.Incremental                  (Cacheable, Dependency, MonadDepend (..),
                                                      selectKeyD)
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.ApiLimit
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.Endpoint
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Metadata.Object
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.Relationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.ScheduledTrigger
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.RQL.Types.Source
import           Hasura.RQL.Types.Table
import           Hasura.Session
import           Hasura.Tracing                      (TraceT)


reportSchemaObjs :: [SchemaObjId] -> Text
reportSchemaObjs = commaSeparated . sort . map reportSchemaObj

mkParentDep :: forall b. (Backend b) => SourceName -> TableName b -> SchemaDependency
mkParentDep s tn = SchemaDependency (SOSourceObj @b s $ SOITable tn) DRTable

mkColDep
  :: forall b
  . (Backend b)
  => DependencyReason -> SourceName -> TableName b -> Column b -> SchemaDependency
mkColDep reason source tn col =
  flip SchemaDependency reason . SOSourceObj @b source . SOITableObj tn $ TOCol col

mkComputedFieldDep
  :: forall b. (Backend b)
  => DependencyReason -> SourceName -> TableName b -> ComputedFieldName -> SchemaDependency
mkComputedFieldDep reason s tn computedField =
  flip SchemaDependency reason . SOSourceObj @b s . SOITableObj tn $ TOComputedField computedField

type WithDeps a = (a, [SchemaDependency])

data IntrospectionResult
  = IntrospectionResult
  { irDoc              :: RemoteSchemaIntrospection
  , irQueryRoot        :: G.Name
  , irMutationRoot     :: Maybe G.Name
  , irSubscriptionRoot :: Maybe G.Name
  } deriving (Show, Eq, Generic)
instance Cacheable IntrospectionResult

data ParsedIntrospection
  = ParsedIntrospection
  { piQuery        :: [P.FieldParser (P.ParseT Identity) RemoteField]
  , piMutation     :: Maybe [P.FieldParser (P.ParseT Identity) RemoteField]
  , piSubscription :: Maybe [P.FieldParser (P.ParseT Identity) RemoteField]
  }

-- | See 'fetchRemoteSchema'.
data RemoteSchemaCtx
  = RemoteSchemaCtx
  { _rscName                   :: !RemoteSchemaName
  , _rscIntro                  :: !IntrospectionResult
  , _rscInfo                   :: !RemoteSchemaInfo
  , _rscRawIntrospectionResult :: !BL.ByteString
  -- ^ The raw response from the introspection query against the remote server.
  -- We store this so we can efficiently service 'introspect_remote_schema'.
  , _rscParsed                 ::  ParsedIntrospection
  , _rscPermissions            :: !(M.HashMap RoleName IntrospectionResult)
  }
$(makeLenses ''RemoteSchemaCtx)

instance ToJSON RemoteSchemaCtx where
  toJSON (RemoteSchemaCtx name _ info _ _ _) =
    object $
      [ "name" .= name
      , "info" .= toJSON info
      ]

type RemoteSchemaMap = M.HashMap RemoteSchemaName RemoteSchemaCtx

type DepMap = M.HashMap SchemaObjId (HS.HashSet SchemaDependency)

data CronTriggerInfo
 = CronTriggerInfo
   { ctiName        :: !TriggerName
   , ctiSchedule    :: !CronSchedule
   , ctiPayload     :: !(Maybe Value)
   , ctiRetryConf   :: !STRetryConf
   , ctiWebhookInfo :: !ResolvedWebhook
   , ctiHeaders     :: ![EventHeaderInfo]
   , ctiComment     :: !(Maybe Text)
   } deriving (Show, Eq)

$(deriveToJSON hasuraJSON ''CronTriggerInfo)

newtype SchemaCacheVer
  = SchemaCacheVer { unSchemaCacheVer :: Word64 }
  deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON)

initSchemaCacheVer :: SchemaCacheVer
initSchemaCacheVer = SchemaCacheVer 0

incSchemaCacheVer :: SchemaCacheVer -> SchemaCacheVer
incSchemaCacheVer (SchemaCacheVer prev) =
  SchemaCacheVer $ prev + 1

type ActionCache = M.HashMap ActionName ActionInfo -- info of all actions

unsafeFunctionCache
  :: forall b. Backend b => SourceName -> SourceCache -> Maybe (FunctionCache b)
unsafeFunctionCache sourceName cache =
  unsafeSourceFunctions =<< M.lookup sourceName cache

unsafeFunctionInfo
  :: forall b. Backend b => SourceName -> FunctionName b -> SourceCache -> Maybe (FunctionInfo b)
unsafeFunctionInfo sourceName functionName cache =
  M.lookup functionName =<< unsafeFunctionCache sourceName cache

unsafeTableCache
  :: forall b. Backend b => SourceName -> SourceCache -> Maybe (TableCache b)
unsafeTableCache sourceName cache = do
  unsafeSourceTables =<< M.lookup sourceName cache

unsafeTableInfo
  :: forall b. Backend b => SourceName -> TableName b -> SourceCache -> Maybe (TableInfo b)
unsafeTableInfo sourceName tableName cache =
  M.lookup tableName =<< unsafeTableCache sourceName cache

data SchemaCache
  = SchemaCache
  { scPostgres                    :: !SourceCache
  , scActions                     :: !ActionCache
  , scRemoteSchemas               :: !RemoteSchemaMap
  , scAllowlist                   :: !(HS.HashSet GQLQuery)
  , scGQLContext                  :: !(HashMap RoleName (RoleContext GQLContext))
  , scUnauthenticatedGQLContext   :: !GQLContext
  , scRelayContext                :: !(HashMap RoleName (RoleContext GQLContext))
  , scUnauthenticatedRelayContext :: !GQLContext
  -- , scCustomTypes       :: !(NonObjectTypeMap, AnnotatedObjects)
  , scDepMap                      :: !DepMap
  , scInconsistentObjs            :: ![InconsistentMetadata]
  , scCronTriggers                :: !(M.HashMap TriggerName CronTriggerInfo)
  , scEndpoints                   :: !(EndpointTrie GQLQueryWithText)
  , scApiLimits                   :: !ApiLimit
  , scMetricsConfig               :: !MetricsConfig
  }
$(deriveToJSON hasuraJSON ''SchemaCache)

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

newtype SourceT m a
  = SourceT { runSourceT :: SourceName -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, PG.MonadTx, TableCoreInfoRM b, CacheRM)
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

newtype TableCoreCacheRT b m a
  = TableCoreCacheRT { runTableCoreCacheRT :: (SourceName, Dependency (TableCoreCache b)) -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, PG.MonadTx)
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

newtype TableCacheRT b m a
  = TableCacheRT { runTableCacheRT :: (SourceName, TableCache b) -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, PG.MonadTx)
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
instance (CacheRM m) => CacheRM (PG.LazyTxT QErr m) where
  askSchemaCache = lift askSchemaCache

getDependentObjs :: SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjs = getDependentObjsWith (const True)

getDependentObjsWith
  :: (DependencyReason -> Bool) -> SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjsWith f sc objId =
  -- [ sdObjId sd | sd <- filter (f . sdReason) allDeps]
  map fst $ filter (isDependency . snd) $ M.toList $ scDepMap sc
  where
    isDependency deps = not $ HS.null $ flip HS.filter deps $
      \(SchemaDependency depId reason) -> objId `induces` depId && f reason
    -- induces a b : is b dependent on a
    induces (SOSource s1)                   (SOSource s2)                        = s1 == s2
    induces (SOSource s1)                   (SOSourceObj s2 _)                   = s1 == s2
    induces (SOSourceObj s1 (SOITable tn1)) (SOSourceObj s2 (SOITable tn2))      = s1 == s2 && Just tn1 == (cast tn2)
    induces (SOSourceObj s1 (SOITable tn1)) (SOSourceObj s2 (SOITableObj tn2 _)) = s1 == s2 && Just tn1 == (cast tn2)
    induces objId1 objId2                                                        = objId1 == objId2
    -- allDeps = toList $ fromMaybe HS.empty $ M.lookup objId $ scDepMap sc
