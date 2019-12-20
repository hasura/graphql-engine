{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Hasura.RQL.Types.SchemaCache
       ( SchemaCache(..)
       , SchemaCacheVer
       , initSchemaCacheVer
       , incSchemaCacheVer
       , emptySchemaCache
       , TableConfig(..)
       , emptyTableConfig

       , TableCache
       , modTableCache
       , addTableToCache
       , modTableInCache
       , delTableFromCache

       , TableInfo(..)
       , tiName
       , tiDescription
       , tiSystemDefined
       , tiFieldInfoMap
       , tiRolePermInfoMap
       , tiUniqOrPrimConstraints
       , tiPrimaryKeyCols
       , tiViewInfo
       , tiEventTriggerInfoMap
       , tiEnumValues
       , tiCustomConfig

       , TableConstraint(..)
       , ConstraintType(..)
       , ViewInfo(..)
       , checkForFieldConflict
       , isMutable
       , mutableView
       , isUniqueOrPrimary
       , isForeignKey

       , RemoteSchemaCtx(..)
       , RemoteSchemaMap
       , addRemoteSchemaToCache
       , delRemoteSchemaFromCache

       , WithDeps

       , CacheRM(..)
       , CacheRWM(..)

       , FieldInfoMap
       , FieldInfo(..)
       , _FIColumn
       , _FIRelationship
       , getCols
       , getRels
       , getComputedFieldInfos
       , possibleNonColumnGraphQLFields

       , isPGColInfo
       , RelInfo(..)
       , addColToCache
       , addRelToCache
       , addComputedFieldToCache

       , delColFromCache
       , updColInCache
       , delRelFromCache
       , deleteComputedFieldFromCache

       , RolePermInfo(..)
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
       , addPermToCache
       , delPermFromCache
       , PreSetColsPartial

       , addEventTriggerToCache
       , delEventTriggerFromCache
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
       , addFunctionToCache
       , askFunctionInfo
       , delFunctionFromCache
       , updateFunctionDescription

       , replaceAllowlist
       ) where

import qualified Hasura.GraphQL.Context            as GC

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
reportSchemaObjs = T.intercalate ", " . map reportSchemaObj

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

data FieldInfo columnInfo
  = FIColumn !columnInfo
  | FIRelationship !RelInfo
  | FIComputedField !ComputedFieldInfo
  deriving (Show, Eq)
$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "detail"
                 }
  ''FieldInfo)
$(makePrisms ''FieldInfo)

type FieldInfoMap columnInfo = M.HashMap FieldName (FieldInfo columnInfo)

possibleNonColumnGraphQLFields :: FieldInfoMap PGColumnInfo -> [G.Name]
possibleNonColumnGraphQLFields fields =
  flip concatMap (M.toList fields) $ \case
    (_, FIColumn _)             -> []
    (_, FIRelationship relInfo) ->
      let relationshipName = G.Name $ relNameToTxt $ riName relInfo
      in case riType relInfo of
           ObjRel -> [relationshipName]
           ArrRel -> [relationshipName, relationshipName <> "_aggregate"]
    (_, FIComputedField info) ->
      pure $ G.Name $ computedFieldNameToText $ _cfiName info

getCols :: FieldInfoMap columnInfo -> [columnInfo]
getCols = mapMaybe (^? _FIColumn) . M.elems

getRels :: FieldInfoMap columnInfo -> [RelInfo]
getRels = mapMaybe (^? _FIRelationship) . M.elems

getComputedFieldInfos :: FieldInfoMap columnInfo -> [ComputedFieldInfo]
getComputedFieldInfos = mapMaybe (^? _FIComputedField) . M.elems

isPGColInfo :: FieldInfo columnInfo -> Bool
isPGColInfo (FIColumn _) = True
isPGColInfo _            = False

data InsPermInfo
  = InsPermInfo
  { ipiCols            :: !(HS.HashSet PGCol)
  , ipiView            :: !QualifiedTable
  , ipiCheck           :: !AnnBoolExpPartialSQL
  , ipiSet             :: !PreSetColsPartial
  , ipiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq)

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
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''SelPermInfo)

data UpdPermInfo
  = UpdPermInfo
  { upiCols            :: !(HS.HashSet PGCol)
  , upiTable           :: !QualifiedTable
  , upiFilter          :: !AnnBoolExpPartialSQL
  , upiSet             :: !PreSetColsPartial
  , upiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''UpdPermInfo)

data DelPermInfo
  = DelPermInfo
  { dpiTable           :: !QualifiedTable
  , dpiFilter          :: !AnnBoolExpPartialSQL
  , dpiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''DelPermInfo)

mkRolePermInfo :: RolePermInfo
mkRolePermInfo = RolePermInfo Nothing Nothing Nothing Nothing

data RolePermInfo
  = RolePermInfo
  { _permIns :: !(Maybe InsPermInfo)
  , _permSel :: !(Maybe SelPermInfo)
  , _permUpd :: !(Maybe UpdPermInfo)
  , _permDel :: !(Maybe DelPermInfo)
  } deriving (Show, Eq)

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
   } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''EventTriggerInfo)

type EventTriggerInfoMap = M.HashMap TriggerName EventTriggerInfo

data ConstraintType
  = CTCHECK
  | CTFOREIGNKEY
  | CTPRIMARYKEY
  | CTUNIQUE
  deriving Eq

constraintTyToTxt :: ConstraintType -> T.Text
constraintTyToTxt ty = case ty of
  CTCHECK      -> "CHECK"
  CTFOREIGNKEY -> "FOREIGN KEY"
  CTPRIMARYKEY -> "PRIMARY KEY"
  CTUNIQUE     -> "UNIQUE"

instance Show ConstraintType where
  show = T.unpack . constraintTyToTxt

instance FromJSON ConstraintType where
  parseJSON = withText "ConstraintType" $ \case
    "CHECK"       -> return CTCHECK
    "FOREIGN KEY" -> return CTFOREIGNKEY
    "PRIMARY KEY" -> return CTPRIMARYKEY
    "UNIQUE"      -> return CTUNIQUE
    c             -> fail $ "unexpected ConstraintType: " <> T.unpack c

instance ToJSON ConstraintType where
  toJSON = String . constraintTyToTxt

isUniqueOrPrimary :: ConstraintType -> Bool
isUniqueOrPrimary = \case
  CTPRIMARYKEY -> True
  CTUNIQUE     -> True
  _            -> False

isForeignKey :: ConstraintType -> Bool
isForeignKey = \case
  CTFOREIGNKEY -> True
  _            -> False

data TableConstraint
  = TableConstraint
  { tcType :: !ConstraintType
  , tcName :: !ConstraintName
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase) ''TableConstraint)

data ViewInfo
  = ViewInfo
  { viIsUpdatable  :: !Bool
  , viIsDeletable  :: !Bool
  , viIsInsertable :: !Bool
  } deriving (Show, Eq)

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
$(deriveToJSON (aesonDrop 3 snakeCase) ''TableConfig)

emptyTableConfig :: TableConfig
emptyTableConfig =
  TableConfig GC.emptyCustomRootFields M.empty

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $ \obj ->
    TableConfig
    <$> obj .:? "custom_root_fields" .!= GC.emptyCustomRootFields
    <*> obj .:? "custom_column_names" .!= M.empty

data TableInfo columnInfo
  = TableInfo
  { _tiName                  :: !QualifiedTable
  , _tiDescription           :: !(Maybe PGDescription)
  , _tiSystemDefined         :: !SystemDefined
  , _tiFieldInfoMap          :: !(FieldInfoMap columnInfo)
  , _tiRolePermInfoMap       :: !RolePermInfoMap
  , _tiUniqOrPrimConstraints :: ![ConstraintName]
  , _tiPrimaryKeyCols        :: ![PGCol]
  , _tiViewInfo              :: !(Maybe ViewInfo)
  , _tiEventTriggerInfoMap   :: !EventTriggerInfoMap
  , _tiEnumValues            :: !(Maybe EnumValues)
  , _tiCustomConfig          :: !TableConfig
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 3 snakeCase) ''TableInfo)
$(makeLenses ''TableInfo)

checkForFieldConflict
  :: (MonadError QErr m)
  => TableInfo PGColumnInfo
  -> FieldName
  -> m ()
checkForFieldConflict tableInfo f = do
  case M.lookup f fieldInfoMap of
    Just _ -> throw400 AlreadyExists $ mconcat
      [ "column/relationship/computed field " <>> f
      , " of table " <>> tableName
      , " already exists"
      ]
    Nothing -> return ()
  when (f `elem` customColumnFields) $
    throw400 AlreadyExists $
    "custom column name " <> f <<> " of table " <> tableName <<> " already exists"
  where
    tableName = _tiName tableInfo
    fieldInfoMap = _tiFieldInfoMap tableInfo
    customColumnFields =
      map (FieldName . G.unName . pgiName) $ getCols fieldInfoMap

type TableCache columnInfo = M.HashMap QualifiedTable (TableInfo columnInfo) -- info of all tables
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

addToDepMap :: SchemaObjId -> [SchemaDependency] -> DepMap -> DepMap
addToDepMap schObj deps =
  M.insert schObj (HS.fromList deps)

removeFromDepMap :: SchemaObjId -> DepMap -> DepMap
removeFromDepMap =
  M.delete

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
  { scTables            :: !(TableCache PGColumnInfo)
  , scFunctions         :: !FunctionCache
  , scRemoteSchemas     :: !RemoteSchemaMap
  , scAllowlist         :: !(HS.HashSet GQLQuery)
  , scGCtxMap           :: !GC.GCtxMap
  , scDefaultRemoteGCtx :: !GC.GCtx
  , scDepMap            :: !DepMap
  , scInconsistentObjs  :: ![InconsistentMetadataObj]
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaCache)

getFuncsOfTable :: QualifiedTable -> FunctionCache -> [FunctionInfo]
getFuncsOfTable qt fc = flip filter allFuncs $ \f -> qt == fiReturnType f
  where
    allFuncs = M.elems fc

modDepMapInCache :: (CacheRWM m) => (DepMap -> DepMap) -> m ()
modDepMapInCache f = do
  sc <- askSchemaCache
  writeSchemaCache $ sc { scDepMap = f (scDepMap sc)}

class (Monad m) => CacheRM m where
  askSchemaCache :: m SchemaCache

instance (CacheRM m) => CacheRM (ReaderT r m) where
  askSchemaCache = lift askSchemaCache

instance (Monad m) => CacheRM (StateT SchemaCache m) where
  askSchemaCache = get

class (CacheRM m) => CacheRWM m where
  writeSchemaCache :: SchemaCache -> m ()

instance (CacheRWM m) => CacheRWM (ReaderT r m) where
  writeSchemaCache = lift . writeSchemaCache

instance (Monad m) => CacheRWM (StateT SchemaCache m) where
  writeSchemaCache = put

emptySchemaCache :: SchemaCache
emptySchemaCache =
  SchemaCache M.empty M.empty M.empty
              HS.empty M.empty GC.emptyGCtx mempty []

modTableCache :: (CacheRWM m) => TableCache PGColumnInfo -> m ()
modTableCache tc = do
  sc <- askSchemaCache
  writeSchemaCache $ sc { scTables = tc }

addTableToCache :: (QErrM m, CacheRWM m)
                => TableInfo PGColumnInfo -> m ()
addTableToCache ti = do
  sc <- askSchemaCache
  assertTableNotExists tn sc
  modTableCache $ M.insert tn ti $ scTables sc
  where
    tn = _tiName ti

delTableFromCache :: (QErrM m, CacheRWM m)
                  => QualifiedTable -> m ()
delTableFromCache tn = do
  sc <- askSchemaCache
  void $ getTableInfoFromCache tn sc
  modTableCache $ M.delete tn $ scTables sc
  modDepMapInCache (M.filterWithKey notThisTableObj)
  where
    notThisTableObj (SOTableObj depTn _) _ = depTn /= tn
    notThisTableObj _                    _ = True

getTableInfoFromCache :: (QErrM m)
                      => QualifiedTable
                      -> SchemaCache
                      -> m (TableInfo PGColumnInfo)
getTableInfoFromCache tn sc =
  case M.lookup tn (scTables sc) of
    Nothing -> throw500 $ "table not found in cache : " <>> tn
    Just ti -> return ti

assertTableNotExists :: (QErrM m)
                     => QualifiedTable
                     -> SchemaCache
                     -> m ()
assertTableNotExists tn sc =
  case M.lookup tn (scTables sc) of
    Nothing -> return ()
    Just _  -> throw500 $ "table exists in cache : " <>> tn

modTableInCache :: (QErrM m, CacheRWM m)
                => (TableInfo PGColumnInfo -> m (TableInfo PGColumnInfo))
                -> QualifiedTable
                -> m ()
modTableInCache f tn = do
  sc <- askSchemaCache
  ti <- getTableInfoFromCache tn sc
  newTi <- f ti
  modTableCache $ M.insert tn newTi $ scTables sc

addColToCache
  :: (QErrM m, CacheRWM m)
  => PGCol -> PGColumnInfo
  -> QualifiedTable -> m ()
addColToCache cn ci =
  addFldToCache (fromPGCol cn) (FIColumn ci)

addRelToCache
  :: (QErrM m, CacheRWM m)
  => RelName -> RelInfo -> [SchemaDependency]
  -> QualifiedTable -> m ()
addRelToCache rn ri deps tn = do
  addFldToCache (fromRel rn) (FIRelationship ri)  tn
  modDepMapInCache (addToDepMap schObjId deps)
  where
    schObjId = SOTableObj tn $ TORel $ riName ri

addComputedFieldToCache
  :: (QErrM m, CacheRWM m)
  => QualifiedTable -> ComputedFieldInfo -> m ()
addComputedFieldToCache table computedFieldInfo =
  addFldToCache computedField (FIComputedField computedFieldInfo) table
  where
    computedField = fromComputedField $ _cfiName computedFieldInfo

addFldToCache
  :: (QErrM m, CacheRWM m)
  => FieldName -> FieldInfo PGColumnInfo
  -> QualifiedTable -> m ()
addFldToCache fn fi =
  modTableInCache modFieldInfoMap
  where
    modFieldInfoMap ti = do
      let fim = _tiFieldInfoMap ti
      case M.lookup fn fim of
        Just _  -> throw500 "field already exists "
        Nothing -> return $
          ti { _tiFieldInfoMap = M.insert fn fi fim }

delFldFromCache :: (QErrM m, CacheRWM m)
                => FieldName -> QualifiedTable -> m ()
delFldFromCache fn =
  modTableInCache modFieldInfoMap
  where
    modFieldInfoMap ti = do
      let fim = _tiFieldInfoMap ti
      case M.lookup fn fim of
        Just _  -> return $
          ti { _tiFieldInfoMap = M.delete fn fim }
        Nothing -> throw500 "field does not exist"

delColFromCache :: (QErrM m, CacheRWM m)
                => PGCol -> QualifiedTable -> m ()
delColFromCache cn =
  delFldFromCache (fromPGCol cn)

delRelFromCache :: (QErrM m, CacheRWM m)
                => RelName -> QualifiedTable -> m ()
delRelFromCache rn tn = do
  delFldFromCache (fromRel rn) tn
  modDepMapInCache (removeFromDepMap schObjId)
  where
    schObjId = SOTableObj tn $ TORel rn

deleteComputedFieldFromCache
  :: (QErrM m, CacheRWM m)
  => QualifiedTable -> ComputedFieldName -> m ()
deleteComputedFieldFromCache table computedField =
  delFldFromCache (fromComputedField computedField) table

updColInCache
  :: (QErrM m, CacheRWM m)
  => PGCol -> PGColumnInfo
  -> QualifiedTable -> m ()
updColInCache cn ci tn = do
  delColFromCache cn tn
  addColToCache cn ci tn

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

addEventTriggerToCache
  :: (QErrM m, CacheRWM m)
  => QualifiedTable
  -> EventTriggerInfo
  -> [SchemaDependency]
  -> m ()
addEventTriggerToCache qt eti deps = do
  modTableInCache modEventTriggerInfo qt
  modDepMapInCache (addToDepMap schObjId deps)
  where
    trn = etiName eti
    modEventTriggerInfo ti = do
      let etim = _tiEventTriggerInfoMap ti
      return $ ti { _tiEventTriggerInfoMap = M.insert trn eti etim}
    schObjId = SOTableObj qt $ TOTrigger trn

delEventTriggerFromCache
  :: (QErrM m, CacheRWM m)
  => QualifiedTable
  -> TriggerName
  -> m ()
delEventTriggerFromCache qt trn = do
  modTableInCache modEventTriggerInfo qt
  modDepMapInCache (removeFromDepMap schObjId)
  where
    modEventTriggerInfo ti = do
      let etim = _tiEventTriggerInfoMap ti
      return $ ti { _tiEventTriggerInfoMap = M.delete trn etim }
    schObjId = SOTableObj qt $ TOTrigger trn

addFunctionToCache
  :: (QErrM m, CacheRWM m)
  => FunctionInfo -> [SchemaDependency] -> m ()
addFunctionToCache fi deps = do
  sc <- askSchemaCache
  let functionCache = scFunctions sc
  case M.lookup fn functionCache of
    Just _ -> throw500 $ "function already exists in cache " <>> fn
    Nothing -> do
      let newFunctionCache = M.insert fn fi functionCache
      writeSchemaCache $ sc {scFunctions = newFunctionCache}
  modDepMapInCache (addToDepMap objId deps)
  where
    fn = fiName fi
    objId = SOFunction $ fiName fi

askFunctionInfo
  :: (CacheRM m, QErrM m)
  => QualifiedFunction ->  m FunctionInfo
askFunctionInfo qf = do
  sc <- askSchemaCache
  maybe throwNoFn return $ M.lookup qf $ scFunctions sc
  where
    throwNoFn = throw400 NotExists $
      "function not found in cache " <>> qf

delFunctionFromCache
  :: (QErrM m, CacheRWM m)
  => QualifiedFunction -> m ()
delFunctionFromCache qf = do
  void $ askFunctionInfo qf
  sc <- askSchemaCache
  let functionCache = scFunctions sc
      newFunctionCache = M.delete qf functionCache
  writeSchemaCache $ sc {scFunctions = newFunctionCache}
  modDepMapInCache (removeFromDepMap objId)
  where
    objId = SOFunction qf

updateFunctionDescription
  :: (QErrM m, CacheRWM m)
  => QualifiedFunction -> Maybe PGDescription -> m ()
updateFunctionDescription qf descM = do
  fi <- askFunctionInfo qf
  sc <- askSchemaCache
  let newFuncInfo = fi{fiDescription = descM}
      newFuncCache = M.insert qf newFuncInfo $ scFunctions sc
  writeSchemaCache sc{scFunctions = newFuncCache}

addPermToCache
  :: (QErrM m, CacheRWM m)
  => QualifiedTable
  -> RoleName
  -> PermAccessor a
  -> a
  -> [SchemaDependency]
  -> m ()
addPermToCache tn rn pa i deps = do
  modTableInCache modRolePermInfo tn
  modDepMapInCache (addToDepMap schObjId deps)
  where
    paL = permAccToLens pa
    modRolePermInfo ti = do
      let rpim = _tiRolePermInfoMap ti
          rpi  = fromMaybe mkRolePermInfo $ M.lookup rn rpim
          newRPI = rpi & paL ?~ i
      assertPermNotExists pa rpi
      return $ ti { _tiRolePermInfoMap = M.insert rn newRPI rpim }
    schObjId = SOTableObj tn $ TOPerm rn $ permAccToType pa

assertPermNotExists
  :: (QErrM m)
  => PermAccessor a
  -> RolePermInfo -> m ()
assertPermNotExists f rpi =
  when (isJust $ rpi ^. permAccToLens f) $ throw500 "permission exists"

assertPermExists
  :: (QErrM m)
  => PermAccessor a
  -> RolePermInfo -> m ()
assertPermExists f rpi =
  unless (isJust $ rpi ^. permAccToLens f) $ throw500 "permission does not exist"

delPermFromCache
  :: (QErrM m, CacheRWM m)
  => PermAccessor a
  -> RoleName
  -> QualifiedTable
  -> m ()
delPermFromCache pa rn tn = do
  modTableInCache modRolePermInfo tn
  modDepMapInCache (removeFromDepMap schObjId)
  where
    paL = permAccToLens pa
    modRolePermInfo ti = do
      let rpim = _tiRolePermInfoMap ti
          rpi  = fromMaybe mkRolePermInfo $ M.lookup rn rpim
      assertPermExists pa rpi
      let newRPI = rpi & paL .~ Nothing
      return $ ti { _tiRolePermInfoMap = M.insert rn newRPI rpim }
    schObjId = SOTableObj tn $ TOPerm rn $ permAccToType pa

addRemoteSchemaToCache
  :: (QErrM m, CacheRWM m) => RemoteSchemaCtx -> m ()
addRemoteSchemaToCache rmCtx = do
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
      name = rscName rmCtx
  -- ideally, remote schema shouldn't present in cache
  -- if present unexpected 500 is thrown
  onJust (M.lookup name rmSchemas) $ const $
    throw500 $ "remote schema with name " <> name
    <<> " already found in cache"
  writeSchemaCache sc
    {scRemoteSchemas = M.insert name rmCtx rmSchemas}

delRemoteSchemaFromCache
  :: (QErrM m, CacheRWM m) => RemoteSchemaName -> m ()
delRemoteSchemaFromCache name = do
  sc <- askSchemaCache
  let rmSchemas = scRemoteSchemas sc
  -- ideally, remote schema should be present in cache
  -- if not present unexpected 500 is thrown
  void $ onNothing (M.lookup name rmSchemas) $
    throw500 $ "remote schema with name " <> name
    <<> " not found in cache"
  writeSchemaCache sc {scRemoteSchemas = M.delete name rmSchemas}

replaceAllowlist
  :: (CacheRWM m)
  => QueryList -> m ()
replaceAllowlist qList = do
  sc <- askSchemaCache
  let allowlist = HS.fromList $
        map (queryWithoutTypeNames . getGQLQuery . _lqQuery) qList
  writeSchemaCache sc{scAllowlist = allowlist}

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
