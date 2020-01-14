-- As of GHC 8.6, a use of DefaultSignatures in this module triggers a false positive for this
-- warning, so don’t treat it as an error even if -Werror is enabled.
{-# OPTIONS_GHC -Wwarn=redundant-constraints #-}

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.SchemaCache
  ( SchemaCache(..)
  , SchemaCacheVer
  , initSchemaCacheVer
  , incSchemaCacheVer
  , TableConfig(..)
  , emptyTableConfig

  , TableCoreCache
  , TableCache

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
  , checkForFieldConflict
  , isMutable
  , mutableView

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
  , emptyRolePermInfo
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
  ) where

import qualified Hasura.GraphQL.Context            as GC

import           Hasura.Db
import           Hasura.Incremental                (Cacheable, Dependency, MonadDepend (..),
                                                    selectKeyD)
import           Hasura.Prelude
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Metadata
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.QueryCollection
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.SQL.Types

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax        (Lift)

import qualified Data.HashMap.Strict               as M
import qualified Data.HashSet                      as HS
import qualified Data.Text                         as T
import qualified Language.GraphQL.Draft.Syntax     as G

reportSchemaObjs :: [SchemaObjId] -> T.Text
reportSchemaObjs = T.intercalate ", " . sort . map reportSchemaObj

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

data FieldInfo
  = FIColumn !PGColumnInfo
  | FIRelationship !RelInfo
  | FIComputedField !ComputedFieldInfo
  deriving (Show, Eq, Generic)
instance Cacheable FieldInfo
$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "detail"
                 }
  ''FieldInfo)
$(makePrisms ''FieldInfo)

type FieldInfoMap = M.HashMap FieldName

fieldInfoName :: FieldInfo -> FieldName
fieldInfoName = \case
  FIColumn info -> fromPGCol $ pgiColumn info
  FIRelationship info -> fromRel $ riName info
  FIComputedField info -> fromComputedField $ _cfiName info

-- | Returns all the field names created for the given field. Columns, object relationships, and
-- computed fields only ever produce a single field, but array relationships also contain an
-- @_aggregate@ field.
fieldInfoGraphQLNames :: FieldInfo -> [G.Name]
fieldInfoGraphQLNames = \case
  FIColumn info -> [pgiName info]
  FIRelationship info ->
    let name = G.Name . relNameToTxt $ riName info
    in case riType info of
      ObjRel -> [name]
      ArrRel -> [name, name <> "_aggregate"]
  FIComputedField info -> [G.Name . computedFieldNameToText $ _cfiName info]

getCols :: FieldInfoMap FieldInfo -> [PGColumnInfo]
getCols = mapMaybe (^? _FIColumn) . M.elems

getRels :: FieldInfoMap FieldInfo -> [RelInfo]
getRels = mapMaybe (^? _FIRelationship) . M.elems

getComputedFieldInfos :: FieldInfoMap FieldInfo -> [ComputedFieldInfo]
getComputedFieldInfos = mapMaybe (^? _FIComputedField) . M.elems

isPGColInfo :: FieldInfo -> Bool
isPGColInfo (FIColumn _) = True
isPGColInfo _            = False

data InsPermInfo
  = InsPermInfo
  { ipiCols            :: !(HS.HashSet PGCol)
  , ipiView            :: !QualifiedTable
  , ipiCheck           :: !AnnBoolExpPartialSQL
  , ipiSet             :: !PreSetColsPartial
  , ipiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq, Generic)
instance NFData InsPermInfo
instance Cacheable InsPermInfo
$(deriveToJSON (aesonDrop 3 snakeCase) ''InsPermInfo)

data SelPermInfo
  = SelPermInfo
  { spiCols                 :: !(HS.HashSet PGCol)
  , spiScalarComputedFields :: !(HS.HashSet ComputedFieldName)
  , spiTable                :: !QualifiedTable
  , spiFilter               :: !AnnBoolExpPartialSQL
  , spiLimit                :: !(Maybe Int)
  , spiAllowAgg             :: !Bool
  , spiRequiredHeaders      :: ![T.Text]
  } deriving (Show, Eq, Generic)
instance NFData SelPermInfo
instance Cacheable SelPermInfo
$(deriveToJSON (aesonDrop 3 snakeCase) ''SelPermInfo)

data UpdPermInfo
  = UpdPermInfo
  { upiCols            :: !(HS.HashSet PGCol)
  , upiTable           :: !QualifiedTable
  , upiFilter          :: !AnnBoolExpPartialSQL
  , upiSet             :: !PreSetColsPartial
  , upiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq, Generic)
instance NFData UpdPermInfo
instance Cacheable UpdPermInfo
$(deriveToJSON (aesonDrop 3 snakeCase) ''UpdPermInfo)

data DelPermInfo
  = DelPermInfo
  { dpiTable           :: !QualifiedTable
  , dpiFilter          :: !AnnBoolExpPartialSQL
  , dpiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq, Generic)
instance NFData DelPermInfo
instance Cacheable DelPermInfo
$(deriveToJSON (aesonDrop 3 snakeCase) ''DelPermInfo)

emptyRolePermInfo :: RolePermInfo
emptyRolePermInfo = RolePermInfo Nothing Nothing Nothing Nothing

data RolePermInfo
  = RolePermInfo
  { _permIns :: !(Maybe InsPermInfo)
  , _permSel :: !(Maybe SelPermInfo)
  , _permUpd :: !(Maybe UpdPermInfo)
  , _permDel :: !(Maybe DelPermInfo)
  } deriving (Show, Eq, Generic)
instance NFData RolePermInfo
$(deriveToJSON (aesonDrop 5 snakeCase) ''RolePermInfo)

makeLenses ''RolePermInfo

type RolePermInfoMap = M.HashMap RoleName RolePermInfo

data EventTriggerInfo
 = EventTriggerInfo
   { etiName        :: !TriggerName
   , etiOpsDef      :: !TriggerOpsDef
   , etiRetryConf   :: !RetryConf
   , etiWebhookInfo :: !WebhookConfInfo
   , etiHeaders     :: ![EventHeaderInfo]
   } deriving (Show, Eq, Generic)
instance NFData EventTriggerInfo
$(deriveToJSON (aesonDrop 3 snakeCase) ''EventTriggerInfo)

type EventTriggerInfoMap = M.HashMap TriggerName EventTriggerInfo

data ViewInfo
  = ViewInfo
  { viIsUpdatable  :: !Bool
  , viIsDeletable  :: !Bool
  , viIsInsertable :: !Bool
  } deriving (Show, Eq, Generic)
instance NFData ViewInfo
instance Cacheable ViewInfo
$(deriveJSON (aesonDrop 2 snakeCase) ''ViewInfo)

isMutable :: (ViewInfo -> Bool) -> Maybe ViewInfo -> Bool
isMutable _ Nothing   = True
isMutable f (Just vi) = f vi

mutableView :: (MonadError QErr m) => QualifiedTable
            -> (ViewInfo -> Bool) -> Maybe ViewInfo
            -> T.Text -> m ()
mutableView qt f mVI operation =
  unless (isMutable f mVI) $ throw400 NotSupported $
  "view " <> qt <<> " is not " <> operation

data TableConfig
  = TableConfig
  { _tcCustomRootFields  :: !GC.TableCustomRootFields
  , _tcCustomColumnNames :: !CustomColumnNames
  } deriving (Show, Eq, Lift, Generic)
instance NFData TableConfig
instance Cacheable TableConfig
$(deriveToJSON (aesonDrop 3 snakeCase) ''TableConfig)

emptyTableConfig :: TableConfig
emptyTableConfig =
  TableConfig GC.emptyCustomRootFields M.empty

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $ \obj ->
    TableConfig
    <$> obj .:? "custom_root_fields" .!= GC.emptyCustomRootFields
    <*> obj .:? "custom_column_names" .!= M.empty

-- | The @field@ and @primaryKeyColumn@ type parameters vary as the schema cache is built and more
-- information is accumulated. See 'TableRawInfo' and 'TableCoreInfo'.
data TableCoreInfoG field primaryKeyColumn
  = TableCoreInfo
  { _tciName              :: !QualifiedTable
  , _tciDescription       :: !(Maybe PGDescription)
  , _tciSystemDefined     :: !SystemDefined
  , _tciFieldInfoMap      :: !(FieldInfoMap field)
  , _tciPrimaryKey        :: !(Maybe (PrimaryKey primaryKeyColumn))
  , _tciUniqueConstraints :: !(HashSet Constraint)
  -- ^ Does /not/ include the primary key; use 'tciUniqueOrPrimaryKeyConstraints' if you need both.
  , _tciForeignKeys       :: !(HashSet ForeignKey)
  , _tciViewInfo          :: !(Maybe ViewInfo)
  , _tciEnumValues        :: !(Maybe EnumValues)
  , _tciCustomConfig      :: !TableConfig
  } deriving (Show, Eq, Generic)
instance (Cacheable a, Cacheable b) => Cacheable (TableCoreInfoG a b)
$(deriveToJSON (aesonDrop 4 snakeCase) ''TableCoreInfoG)
$(makeLenses ''TableCoreInfoG)

-- | The result of the initial processing step for table info. Includes all basic information, but
-- is missing non-column fields.
type TableRawInfo = TableCoreInfoG PGColumnInfo PGColumnInfo
-- | Fully-processed table info that includes non-column fields.
type TableCoreInfo = TableCoreInfoG FieldInfo PGColumnInfo

tciUniqueOrPrimaryKeyConstraints :: TableCoreInfoG a b -> [Constraint]
tciUniqueOrPrimaryKeyConstraints info =
  maybeToList (_pkConstraint <$> _tciPrimaryKey info) <> toList (_tciUniqueConstraints info)

data TableInfo
  = TableInfo
  { _tiCoreInfo            :: TableCoreInfo
  , _tiRolePermInfoMap     :: !RolePermInfoMap
  , _tiEventTriggerInfoMap :: !EventTriggerInfoMap
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 3 snakeCase) ''TableInfo)
$(makeLenses ''TableInfo)

checkForFieldConflict
  :: (MonadError QErr m)
  => TableCoreInfoG a b
  -> FieldName
  -> m ()
checkForFieldConflict tableInfo f =
  case M.lookup f (_tciFieldInfoMap tableInfo) of
    Just _ -> throw400 AlreadyExists $ mconcat
      [ "column/relationship/computed field " <>> f
      , " of table " <>> _tciName tableInfo
      , " already exists"
      ]
    Nothing -> return ()

type TableCoreCache = M.HashMap QualifiedTable TableCoreInfo
type TableCache = M.HashMap QualifiedTable TableInfo -- info of all tables
type FunctionCache = M.HashMap QualifiedFunction FunctionInfo -- info of all functions

data RemoteSchemaCtx
  = RemoteSchemaCtx
  { rscName :: !RemoteSchemaName
  , rscGCtx :: !GC.RemoteGCtx
  , rscInfo :: !RemoteSchemaInfo
  } deriving (Show, Eq)

instance ToJSON RemoteSchemaCtx where
  toJSON = toJSON . rscInfo

type RemoteSchemaMap = M.HashMap RemoteSchemaName RemoteSchemaCtx

type DepMap = M.HashMap SchemaObjId (HS.HashSet SchemaDependency)

newtype SchemaCacheVer
  = SchemaCacheVer { unSchemaCacheVer :: Word64 }
  deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON)

initSchemaCacheVer :: SchemaCacheVer
initSchemaCacheVer = SchemaCacheVer 0

incSchemaCacheVer :: SchemaCacheVer -> SchemaCacheVer
incSchemaCacheVer (SchemaCacheVer prev) =
  SchemaCacheVer $ prev + 1

data SchemaCache
  = SchemaCache
  { scTables            :: !TableCache
  , scFunctions         :: !FunctionCache
  , scRemoteSchemas     :: !RemoteSchemaMap
  , scAllowlist         :: !(HS.HashSet GQLQuery)
  , scGCtxMap           :: !GC.GCtxMap
  , scDefaultRemoteGCtx :: !GC.GCtx
  , scDepMap            :: !DepMap
  , scInconsistentObjs  :: ![InconsistentMetadata]
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaCache)

getFuncsOfTable :: QualifiedTable -> FunctionCache -> [FunctionInfo]
getFuncsOfTable qt fc = flip filter allFuncs $ \f -> qt == fiReturnType f
  where
    allFuncs = M.elems fc

-- | A more limited version of 'CacheRM' that is used when building the schema cache, since the
-- entire schema cache has not been built yet.
class (Monad m) => TableCoreInfoRM m where
  lookupTableCoreInfo :: QualifiedTable -> m (Maybe TableCoreInfo)
  default lookupTableCoreInfo :: (CacheRM m) => QualifiedTable -> m (Maybe TableCoreInfo)
  lookupTableCoreInfo tableName = fmap _tiCoreInfo . M.lookup tableName . scTables <$> askSchemaCache

instance (TableCoreInfoRM m) => TableCoreInfoRM (ReaderT r m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo
instance (TableCoreInfoRM m) => TableCoreInfoRM (StateT s m) where
  lookupTableCoreInfo = lift . lookupTableCoreInfo
instance (Monoid w, TableCoreInfoRM m) => TableCoreInfoRM (WriterT w m) where
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

newtype CacheRT m a = CacheRT { runCacheRT :: SchemaCache -> m a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadWriter w) via (ReaderT SchemaCache m)
  deriving (MonadTrans) via (ReaderT SchemaCache)
instance (Monad m) => TableCoreInfoRM (CacheRT m)
instance (Monad m) => CacheRM (CacheRT m) where
  askSchemaCache = CacheRT pure

data PermAccessor a where
  PAInsert :: PermAccessor InsPermInfo
  PASelect :: PermAccessor SelPermInfo
  PAUpdate :: PermAccessor UpdPermInfo
  PADelete :: PermAccessor DelPermInfo

permAccToLens :: PermAccessor a -> Lens' RolePermInfo (Maybe a)
permAccToLens PAInsert = permIns
permAccToLens PASelect = permSel
permAccToLens PAUpdate = permUpd
permAccToLens PADelete = permDel

permAccToType :: PermAccessor a -> PermType
permAccToType PAInsert = PTInsert
permAccToType PASelect = PTSelect
permAccToType PAUpdate = PTUpdate
permAccToType PADelete = PTDelete

withPermType :: PermType -> (forall a. PermAccessor a -> b) -> b
withPermType PTInsert f = f PAInsert
withPermType PTSelect f = f PASelect
withPermType PTUpdate f = f PAUpdate
withPermType PTDelete f = f PADelete

askFunctionInfo
  :: (CacheRM m, QErrM m)
  => QualifiedFunction ->  m FunctionInfo
askFunctionInfo qf = do
  sc <- askSchemaCache
  maybe throwNoFn return $ M.lookup qf $ scFunctions sc
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
