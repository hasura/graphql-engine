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

  , TableCoreInfoRM(..)
  , TableCoreCacheRT(..)
  , CacheRM(..)
  , CacheRT(..)

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

  , FunctionType(..)
  , FunctionArg(..)
  , FunctionArgName(..)
  , FunctionName(..)
  , FunctionInfo(..)
  , FunctionCache
  , getFuncsOfTable
  , askFunctionInfo
  , CronTriggerInfo(..)
  ) where

import           Hasura.Prelude

import qualified Data.ByteString.Lazy                as BL
import qualified Data.HashMap.Strict                 as M
import qualified Data.HashSet                        as HS
import qualified Language.GraphQL.Draft.Syntax       as G

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended
import           System.Cron.Types

import qualified Hasura.GraphQL.Parser               as P

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.GraphQL.Context              (GQLContext, RemoteField, RoleContext)
import           Hasura.Incremental                  (Dependency, MonadDepend (..), selectKeyD)
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Action
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
import           Hasura.SQL.Backend
import           Hasura.Session
import           Hasura.Tracing                      (TraceT)


reportSchemaObjs :: [SchemaObjId] -> Text
reportSchemaObjs = commaSeparated . sort . map reportSchemaObj

mkParentDep :: QualifiedTable -> SchemaDependency
mkParentDep tn = SchemaDependency (SOTable tn) DRTable

mkColDep :: DependencyReason -> QualifiedTable -> PGCol -> SchemaDependency
mkColDep reason tn col =
  flip SchemaDependency reason . SOTableObj tn $ TOCol col

mkComputedFieldDep
  :: DependencyReason -> QualifiedTable -> ComputedFieldName -> SchemaDependency
mkComputedFieldDep reason tn computedField =
  flip SchemaDependency reason . SOTableObj tn $ TOComputedField computedField

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
  { piQuery        :: [P.FieldParser (P.ParseT Identity) RemoteField]
  , piMutation     :: Maybe [P.FieldParser (P.ParseT Identity) RemoteField]
  , piSubscription :: Maybe [P.FieldParser (P.ParseT Identity) RemoteField]
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
type ActionCache = M.HashMap ActionName (ActionInfo 'Postgres) -- info of all actions

data SchemaCache
  = SchemaCache
  { scTables                      :: !TableCache
  , scActions                     :: !ActionCache
  , scFunctions                   :: !FunctionCache
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

-- | A more limited version of 'CacheRM' that is used when building the schema cache, since the
-- entire schema cache has not been built yet.
class (Monad m) => TableCoreInfoRM m where
  lookupTableCoreInfo :: QualifiedTable -> m (Maybe (TableCoreInfo 'Postgres))
  default lookupTableCoreInfo :: (CacheRM m) => QualifiedTable -> m (Maybe (TableCoreInfo 'Postgres))
  lookupTableCoreInfo tableName = fmap _tiCoreInfo . M.lookup tableName . scTables <$> askSchemaCache

instance (TableCoreInfoRM m) => TableCoreInfoRM (ReaderT r m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo
instance (TableCoreInfoRM m) => TableCoreInfoRM (StateT s m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo
instance (Monoid w, TableCoreInfoRM m) => TableCoreInfoRM (WriterT w m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo
instance (TableCoreInfoRM m) => TableCoreInfoRM (TraceT m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo

newtype TableCoreCacheRT m a
  = TableCoreCacheRT { runTableCoreCacheRT :: Dependency TableCoreCache -> m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, MonadTx)
    via (ReaderT (Dependency TableCoreCache) m)
  deriving (MonadTrans) via (ReaderT (Dependency TableCoreCache))

instance (MonadReader r m) => MonadReader r (TableCoreCacheRT m) where
  ask = lift ask
  local f m = TableCoreCacheRT (local f . runTableCoreCacheRT m)
instance (MonadDepend m) => TableCoreInfoRM (TableCoreCacheRT m) where
  lookupTableCoreInfo tableName = TableCoreCacheRT (dependOnM . selectKeyD tableName)

class (TableCoreInfoRM m) => CacheRM m where
  askSchemaCache :: m SchemaCache

instance (CacheRM m) => CacheRM (ReaderT r m) where
  askSchemaCache = lift askSchemaCache
instance (CacheRM m) => CacheRM (StateT s m) where
  askSchemaCache = lift askSchemaCache
instance (Monoid w, CacheRM m) => CacheRM (WriterT w m) where
  askSchemaCache = lift askSchemaCache
instance (CacheRM m) => CacheRM (TraceT m) where
  askSchemaCache = lift askSchemaCache

newtype CacheRT m a = CacheRT { runCacheRT :: SchemaCache -> m a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadWriter w) via (ReaderT SchemaCache m)
  deriving (MonadTrans) via (ReaderT SchemaCache)
instance (Monad m) => TableCoreInfoRM (CacheRT m)
instance (Monad m) => CacheRM (CacheRT m) where
  askSchemaCache = CacheRT pure

askFunctionInfo
  :: (CacheRM m, QErrM m)
  => QualifiedFunction ->  m FunctionInfo
askFunctionInfo qf = do
  sc <- askSchemaCache
  onNothing (M.lookup qf $ scFunctions sc) throwNoFn
  where
    throwNoFn = throw400 NotExists $
      "function not found in cache " <>> qf

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
    induces (SOTable tn1) (SOTable tn2)      = tn1 == tn2
    induces (SOTable tn1) (SOTableObj tn2 _) = tn1 == tn2
    induces objId1 objId2                    = objId1 == objId2
    -- allDeps = toList $ fromMaybe HS.empty $ M.lookup objId $ scDepMap sc
