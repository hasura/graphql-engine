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

  , TableCoreCache
  , TableCache
  , ActionCache

  , TypeRelationship(..)
  , trName, trType, trRemoteTable, trFieldMapping
  , TableCoreInfoG(..)
  , TableRawInfo
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
  , RemoteSchemaMap

  , DepMap
  , WithDeps

  , TableInfoRM(..)
  , TableCacheRT(..)
  , TableCoreInfoRM(..)
  , TableCoreCacheRT(..)
  , CacheRM(..)
  -- , CacheRT(..)

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
  , SourceObjId(..)
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

  , FunctionType(..)
  , FunctionArg(..)
  , FunctionArgName(..)
  , FunctionName(..)
  , FunctionInfo(..)
  , FunctionCache
  , getFuncsOfTable
  , askFunctionInfo
  , CronTriggerInfo(..)

  , PGSourceSchemaCache(..)
  , SourceName(..)
  , defaultSource
  , SourceM(..)
  , SourceT(..)
  , PGSourcesCache
  , getPGTableInfo
  , getPGFunctionInfo
  ) where

import           Hasura.Db
import           Hasura.GraphQL.Context            (GQLContext, RoleContext)
import qualified Hasura.GraphQL.Parser             as P
import           Hasura.Incremental                (Cacheable, Dependency, MonadDepend (..),
                                                    selectKeyD)
import           Hasura.Prelude
import           Hasura.RQL.Types.Action
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.CustomTypes
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Metadata
--import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.RemoteSchema

import           Hasura.RQL.Types.ScheduledTrigger
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.RQL.Types.Table
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.Tracing                    (TraceT)

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax        (Lift)
import           System.Cron.Types

import qualified Data.ByteString.Lazy              as BL
import qualified Data.HashMap.Strict               as M
import qualified Data.HashSet                      as HS
import qualified Data.Text                         as T
import qualified Language.GraphQL.Draft.Syntax     as G

reportSchemaObjs :: [SchemaObjId] -> T.Text
reportSchemaObjs = T.intercalate ", " . sort . map reportSchemaObj

mkParentDep :: SourceName -> QualifiedTable -> SchemaDependency
mkParentDep s tn = SchemaDependency (SOSourceObj s $ SOITable tn) DRTable

mkColDep :: DependencyReason -> SourceName -> QualifiedTable -> PGCol -> SchemaDependency
mkColDep reason source tn col =
  flip SchemaDependency reason . SOSourceObj source . SOITableObj tn $ TOCol col

mkComputedFieldDep
  :: DependencyReason -> SourceName -> QualifiedTable -> ComputedFieldName -> SchemaDependency
mkComputedFieldDep reason s tn computedField =
  flip SchemaDependency reason . SOSourceObj s . SOITableObj tn $ TOComputedField computedField

type WithDeps a = (a, [SchemaDependency])

data IntrospectionResult
  = IntrospectionResult
  { irDoc              :: G.SchemaIntrospection
  , irQueryRoot        :: G.Name
  , irMutationRoot     :: Maybe G.Name
  , irSubscriptionRoot :: Maybe G.Name
  }

data ParsedIntrospection
  = ParsedIntrospection
  { piQuery        :: [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  , piMutation     :: Maybe [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  , piSubscription :: Maybe [P.FieldParser (P.ParseT Identity) (RemoteSchemaInfo, G.Field G.NoFragments P.Variable)]
  }

data RemoteSchemaCtx
  = RemoteSchemaCtx
  { rscName                   :: !RemoteSchemaName
  , rscIntro                  :: !IntrospectionResult
  , rscInfo                   :: !RemoteSchemaInfo
  , rscRawIntrospectionResult :: !BL.ByteString
  , rscParsed                 :: ParsedIntrospection
  }

instance ToJSON RemoteSchemaCtx where
  toJSON = toJSON . rscInfo

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

$(deriveToJSON (aesonDrop 3 snakeCase) ''CronTriggerInfo)

newtype SchemaCacheVer
  = SchemaCacheVer { unSchemaCacheVer :: Word64 }
  deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON)

initSchemaCacheVer :: SchemaCacheVer
initSchemaCacheVer = SchemaCacheVer 0

incSchemaCacheVer :: SchemaCacheVer -> SchemaCacheVer
incSchemaCacheVer (SchemaCacheVer prev) =
  SchemaCacheVer $ prev + 1

type FunctionCache = M.HashMap QualifiedFunction FunctionInfo -- info of all functions
type ActionCache = M.HashMap ActionName ActionInfo -- info of all actions

data PGSourceSchemaCache
  = PGSourceSchemaCache
  { _pcTables    :: !TableCache
  , _pcFunctions :: !FunctionCache
  }
$(deriveToJSON (aesonDrop 3 snakeCase) ''PGSourceSchemaCache)

getPGFunctionInfo :: SourceName -> QualifiedFunction -> PGSourcesCache -> Maybe FunctionInfo
getPGFunctionInfo sourceName qualifiedFunction m =
  M.lookup sourceName m >>= M.lookup qualifiedFunction . _pcFunctions

getPGTableInfo :: SourceName -> QualifiedTable -> PGSourcesCache -> Maybe TableInfo
getPGTableInfo sourceName qualifiedTable m =
  M.lookup sourceName m >>= M.lookup qualifiedTable . _pcTables

type PGSourcesCache = M.HashMap SourceName PGSourceSchemaCache

data SchemaCache
  = SchemaCache
  { scPostgres                    :: !PGSourcesCache
  -- { scTables                      :: !TableCache
  , scActions                     :: !ActionCache
  -- , scFunctions                   :: !FunctionCache
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
  }
$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaCache)

getFuncsOfTable :: QualifiedTable -> FunctionCache -> [FunctionInfo]
getFuncsOfTable qt fc = flip filter allFuncs $ \f -> qt == fiReturnType f
  where
    allFuncs = M.elems fc

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
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, UserInfoM, MonadTx, TableCoreInfoRM, TableInfoRM, CacheRM)
    via (ReaderT SourceName m)
  deriving (MonadTrans) via (ReaderT SourceName)

instance (Monad m) => SourceM (SourceT m) where
  askCurrentSource = SourceT pure

-- | A more limited version of 'CacheRM' that is used when building the schema cache, since the
-- entire schema cache has not been built yet.
class (SourceM m) => TableCoreInfoRM m where
  -- lookupTableCoreInfo :: SourceName -> QualifiedTable -> m (Maybe TableCoreInfo)
  -- default lookupTableCoreInfo :: (CacheRM m) => SourceName -> QualifiedTable -> m (Maybe TableCoreInfo)
  -- lookupTableCoreInfo sourceName tableName = do
  --   schemaCache <- askSchemaCache
    -- pure $ fmap _tiCoreInfo $ getPGTableInfo sourceName tableName $ scPostgres schemaCache
  lookupTableCoreInfo :: QualifiedTable -> m (Maybe TableCoreInfo)
  -- default lookupTableCoreInfo :: (CacheRM m) => QualifiedTable -> m (Maybe TableCoreInfo)
  -- lookupTableCoreInfo tableName = do
  --   schemaCache <- askSchemaCache
  --   sourceName <- askCurrentSource
  --   pure $ fmap _tiCoreInfo $ getPGTableInfo sourceName tableName $ scPostgres schemaCache

-- instance (TableCoreInfoRM m) => TableCoreInfoRM (ReaderT r m) where
--   lookupTableCoreInfo sourceName tableName = lift $ lookupTableCoreInfo sourceName tableName
-- instance (TableCoreInfoRM m) => TableCoreInfoRM (StateT s m) where
--   lookupTableCoreInfo sourceName tableName = lift $ lookupTableCoreInfo sourceName tableName
-- instance (Monoid w, TableCoreInfoRM m) => TableCoreInfoRM (WriterT w m) where
--   lookupTableCoreInfo sourceName tableName = lift $ lookupTableCoreInfo sourceName tableName
-- instance (TableCoreInfoRM m) => TableCoreInfoRM (TraceT m) where
  -- lookupTableCoreInfo sourceName tableName = lift $ lookupTableCoreInfo sourceName tableName
instance (TableCoreInfoRM m) => TableCoreInfoRM (ReaderT r m) where
  lookupTableCoreInfo tableName = lift $ lookupTableCoreInfo tableName
instance (TableCoreInfoRM m) => TableCoreInfoRM (StateT s m) where
  lookupTableCoreInfo tableName = lift $ lookupTableCoreInfo tableName
instance (Monoid w, TableCoreInfoRM m) => TableCoreInfoRM (WriterT w m) where
  lookupTableCoreInfo tableName = lift $ lookupTableCoreInfo tableName
instance (TableCoreInfoRM m) => TableCoreInfoRM (TraceT m) where
  lookupTableCoreInfo tableName = lift $ lookupTableCoreInfo tableName

newtype TableCoreCacheRT m a
  = TableCoreCacheRT { runTableCoreCacheRT :: (SourceName, Dependency TableCoreCache) -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, MonadTx)
    via (ReaderT (SourceName, Dependency TableCoreCache) m)
  deriving (MonadTrans) via (ReaderT (SourceName, Dependency TableCoreCache))

instance (MonadReader r m) => MonadReader r (TableCoreCacheRT m) where
  ask = lift ask
  local f m = TableCoreCacheRT (local f . runTableCoreCacheRT m)

instance (Monad m) => SourceM (TableCoreCacheRT m) where
  askCurrentSource =
    TableCoreCacheRT (pure . fst)

instance (MonadDepend m) => TableCoreInfoRM (TableCoreCacheRT m) where
  lookupTableCoreInfo tableName =
    TableCoreCacheRT (dependOnM . selectKeyD tableName . snd)

-- | Most of our queries operate over a single source. This typeclass
-- facilitates that.
class (TableCoreInfoRM m) => TableInfoRM m where
  lookupTableInfo :: QualifiedTable -> m (Maybe TableInfo)

instance (TableInfoRM m) => TableInfoRM (ReaderT r m) where
  lookupTableInfo tableName = lift $ lookupTableInfo tableName
instance (TableInfoRM m) => TableInfoRM (StateT s m) where
  lookupTableInfo tableName = lift $ lookupTableInfo tableName
instance (Monoid w, TableInfoRM m) => TableInfoRM (WriterT w m) where
  lookupTableInfo tableName = lift $ lookupTableInfo tableName
instance (TableInfoRM m) => TableInfoRM (TraceT m) where
  lookupTableInfo tableName = lift $ lookupTableInfo tableName

newtype TableCacheRT m a
  = TableCacheRT { runTableCacheRT :: (SourceName, TableCache) -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, MonadTx, UserInfoM)
    via (ReaderT (SourceName, TableCache) m)
  deriving (MonadTrans) via (ReaderT (SourceName, TableCache))
class (Monad m) => CacheRM m where
  askSchemaCache :: m SchemaCache

instance (Monad m) => SourceM (TableCacheRT m) where
  askCurrentSource =
    TableCacheRT (pure . fst)

instance (Monad m) => TableCoreInfoRM (TableCacheRT m) where
  lookupTableCoreInfo tableName =
    TableCacheRT (pure . fmap _tiCoreInfo . M.lookup tableName . snd)

instance (Monad m) => TableInfoRM (TableCacheRT m) where
  lookupTableInfo tableName =
    TableCacheRT (pure . M.lookup tableName . snd)

instance (CacheRM m) => CacheRM (ReaderT r m) where
  askSchemaCache = lift askSchemaCache
instance (CacheRM m) => CacheRM (StateT s m) where
  askSchemaCache = lift askSchemaCache
instance (Monoid w, CacheRM m) => CacheRM (WriterT w m) where
  askSchemaCache = lift askSchemaCache
instance (CacheRM m) => CacheRM (TraceT m) where
  askSchemaCache = lift askSchemaCache

-- newtype CacheRT m a = CacheRT { runCacheRT :: SchemaCache -> m a }
--   deriving (Functor, Applicative, Monad, MonadError e, MonadWriter w) via (ReaderT SchemaCache m)
--   deriving (MonadTrans) via (ReaderT SchemaCache)
-- instance (Monad m) => TableCoreInfoRM (CacheRT m)
-- instance (Monad m) => CacheRM (CacheRT m) where
--   askSchemaCache = CacheRT pure

askFunctionInfo
  :: (CacheRM m, QErrM m)
  => SourceName -> QualifiedFunction ->  m FunctionInfo
askFunctionInfo sourceName qf = do
  sc <- askSchemaCache
  maybe throwNoFn return $ getPGFunctionInfo sourceName qf $ scPostgres sc
  where
    throwNoFn = throw400 NotExists $ "function not found in cache " <>> qf

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
    induces (SOSourceObj s1 (SOITable tn1)) (SOSourceObj s2 (SOITable tn2))      = s1 == s2 && tn1 == tn2
    induces (SOSourceObj s1 (SOITable tn1)) (SOSourceObj s2 (SOITableObj tn2 _)) = s1 == s2 && tn1 == tn2
    induces objId1 objId2                                                        = objId1 == objId2
    -- allDeps = toList $ fromMaybe HS.empty $ M.lookup objId $ scDepMap sc
