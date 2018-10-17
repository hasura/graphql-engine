{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.RQL.Types.SchemaCache
       ( TableCache
       , SchemaCache(..)
       , emptySchemaCache
       , TableInfo(..)
       , TableConstraint(..)
       , ConstraintType(..)
       , ViewInfo(..)
       , isMutable
       , mutableView
       , onlyIntCols
       , onlyJSONBCols
       , isUniqueOrPrimary
       , mkTableInfo
       , addTableToCache
       , modTableInCache
       , delTableFromCache

       , CacheRM(..)
       , CacheRWM(..)

       , FieldInfoMap
       , FieldInfo(..)
       , fieldInfoToEither
       , partitionFieldInfos
       , partitionFieldInfosWith
       , getCols
       , getRels

       , PGColInfo(..)
       , isPGColInfo
       , getColInfos
       , RelInfo(..)
       , addFldToCache
       , delFldFromCache

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

       , QueryTemplateInfo(..)
       , addQTemplateToCache
       , delQTemplateFromCache
       , TemplateParamInfo(..)

       , addEventTriggerToCache
       , delEventTriggerFromCache
       , getOpInfo
       , EventTriggerInfo(..)
       , EventTriggerInfoMap
       , OpTriggerInfo(..)

       , TableObjId(..)
       , SchemaObjId(..)
       , reportSchemaObj
       , reportSchemaObjs
       , SchemaDependency(..)
       , mkParentDep
       , mkColDep
       , getDependentObjs
       , getDependentObjsWith
       , getDependentObjsOfTable
       , getDependentObjsOfQTemplateCache
       , getDependentPermsOfTable
       , getDependentRelsOfTable
       , getDependentTriggersOfTable
       , isDependentOn
       ) where

import qualified Database.PG.Query           as Q
import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.DML
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.Subscribe
import qualified Hasura.SQL.DML              as S
import           Hasura.SQL.Types

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           GHC.Generics                (Generic)

import qualified Data.HashMap.Strict         as M
import qualified Data.HashSet                as HS
import qualified Data.Text                   as T
import qualified PostgreSQL.Binary.Decoding  as PD

data TableObjId
  = TOCol !PGCol
  | TORel !RelName
  | TOCons !ConstraintName
  | TOPerm !RoleName !PermType
  | TOTrigger !TriggerName
  deriving (Show, Eq, Generic)

instance Hashable TableObjId

data SchemaObjId
  = SOTable !QualifiedTable
  | SOQTemplate !TQueryName
  | SOTableObj !QualifiedTable !TableObjId
   deriving (Eq, Generic)

instance Hashable SchemaObjId

reportSchemaObj :: SchemaObjId -> T.Text
reportSchemaObj (SOTable tn) = "table " <> qualTableToTxt tn
reportSchemaObj (SOQTemplate qtn) =
  "query-template " <> getTQueryName qtn
reportSchemaObj (SOTableObj tn (TOCol cn)) =
  "column " <> qualTableToTxt tn <> "." <> getPGColTxt cn
reportSchemaObj (SOTableObj tn (TORel cn)) =
  "relationship " <> qualTableToTxt tn <> "." <> getRelTxt cn
reportSchemaObj (SOTableObj tn (TOCons cn)) =
  "constraint " <> qualTableToTxt tn <> "." <> getConstraintTxt cn
reportSchemaObj (SOTableObj tn (TOPerm rn pt)) =
  "permission " <> qualTableToTxt tn <> "." <> getRoleTxt rn
  <> "." <> permTypeToCode pt
reportSchemaObj (SOTableObj tn (TOTrigger trn )) =
  "event-trigger " <> qualTableToTxt tn <> "." <> trn

reportSchemaObjs :: [SchemaObjId] -> T.Text
reportSchemaObjs = T.intercalate ", " . map reportSchemaObj

instance Show SchemaObjId where
  show soi = T.unpack $ reportSchemaObj soi

instance ToJSON SchemaObjId where
  toJSON = String . reportSchemaObj

data SchemaDependency
  = SchemaDependency
  { sdObjId  :: !SchemaObjId
  , sdReason :: !T.Text
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaDependency)

mkParentDep :: QualifiedTable -> SchemaDependency
mkParentDep tn = SchemaDependency (SOTable tn) "table"

mkColDep :: T.Text -> QualifiedTable -> PGCol -> SchemaDependency
mkColDep reason tn col =
  flip SchemaDependency reason . SOTableObj tn $ TOCol col

class CachedSchemaObj a where
  dependsOn :: a -> [SchemaDependency]

isDependentOn :: (CachedSchemaObj a) => (T.Text -> Bool) -> SchemaObjId -> a -> Bool
isDependentOn reasonFn objId = any compareFn . dependsOn
  where
    compareFn (SchemaDependency depObjId rsn) = induces objId depObjId && reasonFn rsn
    induces (SOTable tn1) (SOTable tn2)      = tn1 == tn2
    induces (SOTable tn1) (SOTableObj tn2 _) = tn1 == tn2
    induces objId1 objId2                    = objId1 == objId2

data QueryTemplateInfo
  = QueryTemplateInfo
  { qtiName  :: !TQueryName
  , qtiQuery :: !QueryT
  , qtiDeps  :: ![SchemaDependency]
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''QueryTemplateInfo)

instance CachedSchemaObj QueryTemplateInfo where
  dependsOn = qtiDeps

type QTemplateCache = M.HashMap TQueryName QueryTemplateInfo

data PGColInfo
  = PGColInfo
  { pgiName       :: !PGCol
  , pgiType       :: !PGColType
  , pgiIsNullable :: !Bool
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''PGColInfo)

onlyIntCols :: [PGColInfo] -> [PGColInfo]
onlyIntCols = filter (isIntegerType . pgiType)

onlyJSONBCols :: [PGColInfo] -> [PGColInfo]
onlyJSONBCols = filter (isJSONBType . pgiType)

getColInfos :: [PGCol] -> [PGColInfo] -> [PGColInfo]
getColInfos cols allColInfos = flip filter allColInfos $ \ci ->
  pgiName ci `elem` cols

data RelInfo
  = RelInfo
  { riName     :: !RelName
  , riType     :: !RelType
  , riMapping  :: ![(PGCol, PGCol)]
  , riRTable   :: !QualifiedTable
  , riDeps     :: ![SchemaDependency]
  , riIsManual :: !Bool
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''RelInfo)

instance CachedSchemaObj RelInfo where
  dependsOn = riDeps

data FieldInfo
  = FIColumn !PGColInfo
  | FIRelationship !RelInfo
  deriving (Show, Eq)

$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "detail"
                 }
  ''FieldInfo)

fieldInfoToEither :: FieldInfo -> Either PGColInfo RelInfo
fieldInfoToEither (FIColumn l)       = Left l
fieldInfoToEither (FIRelationship r) = Right r

partitionFieldInfos :: [FieldInfo] -> ([PGColInfo], [RelInfo])
partitionFieldInfos = partitionFieldInfosWith (id, id)

partitionFieldInfosWith :: (PGColInfo -> a, RelInfo -> b)
                        -> [FieldInfo] -> ([a], [b])
partitionFieldInfosWith fns =
  partitionEithers . map (biMapEither fns . fieldInfoToEither)
  where
    biMapEither (f1, f2) = either (Left . f1) (Right . f2)

type FieldInfoMap = M.HashMap FieldName FieldInfo

getCols :: FieldInfoMap -> [PGColInfo]
getCols fim = lefts $ map fieldInfoToEither $ M.elems fim

getRels :: FieldInfoMap -> [RelInfo]
getRels fim = rights $ map fieldInfoToEither $ M.elems fim

isPGColInfo :: FieldInfo -> Bool
isPGColInfo (FIColumn _) = True
isPGColInfo _            = False

instance ToJSON S.BoolExp where
  toJSON = String . T.pack . show

data InsPermInfo
  = InsPermInfo
  { ipiView            :: !QualifiedTable
  , ipiCheck           :: !S.BoolExp
  , ipiAllowUpsert     :: !Bool
  , ipiDeps            :: ![SchemaDependency]
  , ipiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''InsPermInfo)

instance CachedSchemaObj InsPermInfo where
  dependsOn = ipiDeps

data SelPermInfo
  = SelPermInfo
  { spiCols            :: !(HS.HashSet PGCol)
  , spiTable           :: !QualifiedTable
  , spiFilter          :: !S.BoolExp
  , spiLimit           :: !(Maybe Int)
  , spiDeps            :: ![SchemaDependency]
  , spiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''SelPermInfo)

instance CachedSchemaObj SelPermInfo where
  dependsOn = spiDeps

data UpdPermInfo
  = UpdPermInfo
  { upiCols            :: !(HS.HashSet PGCol)
  , upiTable           :: !QualifiedTable
  , upiFilter          :: !S.BoolExp
  , upiDeps            :: ![SchemaDependency]
  , upiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''UpdPermInfo)

instance CachedSchemaObj UpdPermInfo where
  dependsOn = upiDeps

data DelPermInfo
  = DelPermInfo
  { dpiTable           :: !QualifiedTable
  , dpiFilter          :: !S.BoolExp
  , dpiDeps            :: ![SchemaDependency]
  , dpiRequiredHeaders :: ![T.Text]
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''DelPermInfo)

instance CachedSchemaObj DelPermInfo where
  dependsOn = dpiDeps

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

data OpTriggerInfo
  = OpTriggerInfo
  { otiTable       :: !QualifiedTable
  , otiTriggerName :: !TriggerName
  , otiCols        :: !SubscribeOpSpec
  , otiDeps        :: ![SchemaDependency]
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 3 snakeCase) ''OpTriggerInfo)

instance CachedSchemaObj OpTriggerInfo where
  dependsOn = otiDeps

data EventTriggerInfo
 = EventTriggerInfo
   { etiId        :: !TriggerId
   , etiName      :: !TriggerName
   , etiInsert    :: !(Maybe OpTriggerInfo)
   , etiUpdate    :: !(Maybe OpTriggerInfo)
   , etiDelete    :: !(Maybe OpTriggerInfo)
   , etiRetryConf :: !RetryConf
   , etiWebhook   :: !T.Text
   , etiHeaders   :: ![(HeaderName, T.Text)]
   } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase) ''EventTriggerInfo)

type EventTriggerInfoMap = M.HashMap TriggerName EventTriggerInfo

getTriggers :: EventTriggerInfoMap -> [OpTriggerInfo]
getTriggers etim = toOpTriggerInfo $ M.elems etim
  where
    toOpTriggerInfo etis = catMaybes $ foldl (\acc eti -> acc ++ [etiInsert eti, etiUpdate eti, etiDelete eti]) [] etis


data ConstraintType
  = CTCHECK
  | CTFOREIGNKEY
  | CTPRIMARYKEY
  | CTUNIQUE
  deriving Eq

$(deriveToJSON defaultOptions{constructorTagModifier = drop 2} ''ConstraintType)

constraintTyToTxt :: ConstraintType -> T.Text
constraintTyToTxt ty = case ty of
  CTCHECK      -> "CHECK"
  CTFOREIGNKEY -> "FOREIGN KEY"
  CTPRIMARYKEY -> "PRIMARY KEY"
  CTUNIQUE     -> "UNIQUE"

instance Show ConstraintType where
  show = T.unpack . constraintTyToTxt

instance Q.FromCol ConstraintType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "CHECK"       -> Just CTCHECK
    "FOREIGN KEY" -> Just CTFOREIGNKEY
    "PRIMARY KEY" -> Just CTPRIMARYKEY
    "UNIQUE"      -> Just CTUNIQUE
    _             -> Nothing

data TableConstraint
  = TableConstraint
  { tcType :: !ConstraintType
  , tcName :: !ConstraintName
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''TableConstraint)

isUniqueOrPrimary :: TableConstraint -> Bool
isUniqueOrPrimary (TableConstraint ty _) = case ty of
  CTCHECK      -> False
  CTFOREIGNKEY -> False
  CTPRIMARYKEY -> True
  CTUNIQUE     -> True

data ViewInfo
  = ViewInfo
  { viIsUpdatable  :: !Bool
  , viIsDeletable  :: !Bool
  , viIsInsertable :: !Bool
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''ViewInfo)

isMutable :: (ViewInfo -> Bool) -> Maybe ViewInfo -> Bool
isMutable _ Nothing = True
isMutable f (Just vi) = f vi

mutableView :: (MonadError QErr m) => QualifiedTable
            -> (ViewInfo -> Bool) -> Maybe ViewInfo
            -> T.Text -> m ()
mutableView qt f mVI operation =
  unless (isMutable f mVI) $ throw400 NotSupported $
  "view " <> qt <<> " is not " <> operation

data TableInfo
  = TableInfo
  { tiName                :: !QualifiedTable
  , tiSystemDefined       :: !Bool
  , tiFieldInfoMap        :: !FieldInfoMap
  , tiRolePermInfoMap     :: !RolePermInfoMap
  , tiConstraints         :: ![TableConstraint]
  , tiPrimaryKeyCols      :: ![PGCol]
  , tiViewInfo            :: !(Maybe ViewInfo)
  , tiEventTriggerInfoMap :: !EventTriggerInfoMap
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''TableInfo)

mkTableInfo :: QualifiedTable -> Bool -> [(ConstraintType, ConstraintName)]
            -> [(PGCol, PGColType, Bool)] -> [PGCol]
            -> Maybe ViewInfo -> TableInfo
mkTableInfo tn isSystemDefined rawCons cols pcols mVI =
  TableInfo tn isSystemDefined colMap (M.fromList []) constraints pcols mVI (M.fromList [])
  where
    constraints = flip map rawCons $ uncurry TableConstraint
    colMap     = M.fromList $ map f cols
    f (cn, ct, b) = (fromPGCol cn, FIColumn $ PGColInfo cn ct b)

type TableCache = M.HashMap QualifiedTable TableInfo -- info of all tables

data SchemaCache
  = SchemaCache
  { scTables     :: !TableCache
  , scQTemplates :: !QTemplateCache
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaCache)

class (Monad m) => CacheRM m where

  -- Get the schema cache
  askSchemaCache :: m SchemaCache

instance (Monad m) => CacheRM (StateT SchemaCache m) where
  askSchemaCache = get

class (CacheRM m) => CacheRWM m where

  -- Get the schema cache
  writeSchemaCache :: SchemaCache -> m ()

instance (Monad m) => CacheRWM (StateT SchemaCache m) where
  writeSchemaCache = put

addQTemplateToCache :: (QErrM m, CacheRWM m)
                    => QueryTemplateInfo -> m ()
addQTemplateToCache qti = do
  sc <- askSchemaCache
  let templateCache = scQTemplates sc
  case M.lookup qtn templateCache of
    Just _ -> throw500 $ "template already exists in cache " <>> qtn
    Nothing -> do
      let newTemplateCache = M.insert qtn qti templateCache
      writeSchemaCache $ sc {scQTemplates = newTemplateCache}
  where
    qtn = qtiName qti

delQTemplateFromCache :: (QErrM m, CacheRWM m)
                      => TQueryName -> m ()
delQTemplateFromCache qtn = do
  sc <- askSchemaCache
  let templateCache = scQTemplates sc
  case M.lookup qtn templateCache of
    Nothing -> throw500 $ "template does not exist in cache " <>> qtn
    Just _ -> do
      let newTemplateCache = M.delete qtn templateCache
      writeSchemaCache $ sc {scQTemplates = newTemplateCache}

-- instance CacheRM  where
--   askSchemaCache = get

emptySchemaCache :: SchemaCache
emptySchemaCache = SchemaCache (M.fromList []) (M.fromList [])

modTableCache :: (CacheRWM m) => TableCache -> m ()
modTableCache tc = do
  sc <- askSchemaCache
  writeSchemaCache $ sc { scTables = tc }

addTableToCache :: (QErrM m, CacheRWM m)
                => TableInfo -> m ()
addTableToCache ti = do
  sc <- askSchemaCache
  assertTableNotExists tn sc
  modTableCache $ M.insert tn ti $ scTables sc
  where
    tn = tiName ti

delTableFromCache :: (QErrM m, CacheRWM m)
                  => QualifiedTable -> m ()
delTableFromCache tn = do
  sc <- askSchemaCache
  void $ getTableInfoFromCache tn sc
  modTableCache $ M.delete tn $ scTables sc

getTableInfoFromCache :: (QErrM m)
                      => QualifiedTable
                      -> SchemaCache
                      -> m TableInfo
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
                => (TableInfo -> m TableInfo)
                -> QualifiedTable
                -> m ()
modTableInCache f tn = do
  sc <- askSchemaCache
  ti <- getTableInfoFromCache tn sc
  newTi <- f ti
  modTableCache $ M.insert tn newTi $ scTables sc

addFldToCache :: (QErrM m, CacheRWM m)
              => FieldName -> FieldInfo
              -> QualifiedTable -> m ()
addFldToCache fn fi =
  modTableInCache modFieldInfoMap
  where
    modFieldInfoMap ti = do
      let fim = tiFieldInfoMap ti
      case M.lookup fn fim of
        Just _  -> throw500 "field already exists "
        Nothing -> return $
          ti { tiFieldInfoMap = M.insert fn fi fim }

delFldFromCache :: (QErrM m, CacheRWM m)
                => FieldName -> QualifiedTable -> m ()
delFldFromCache fn =
  modTableInCache modFieldInfoMap
  where
    modFieldInfoMap ti = do
      let fim = tiFieldInfoMap ti
      case M.lookup fn fim of
        Just _  -> return $
          ti { tiFieldInfoMap = M.delete fn fim }
        Nothing -> throw500 "field does not exist"

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
  -> TriggerId
  -> TriggerName
  -> TriggerOpsDef
  -> RetryConf
  -> T.Text
  -> [(HeaderName, T.Text)]
  -> m ()
addEventTriggerToCache qt trid trn tdef rconf webhook headers =
  modTableInCache modEventTriggerInfo qt
  where
    modEventTriggerInfo ti = do
      let eti = EventTriggerInfo
                trid
                trn
                (getOpInfo trn ti $ tdInsert tdef)
                (getOpInfo trn ti $ tdUpdate tdef)
                (getOpInfo trn ti $ tdDelete tdef)
                rconf
                webhook
                headers
          etim = tiEventTriggerInfoMap ti
      -- fail $ show (toJSON eti)
      return $ ti { tiEventTriggerInfoMap = M.insert trn eti etim}

delEventTriggerFromCache
  :: (QErrM m, CacheRWM m)
  => QualifiedTable
  -> TriggerName
  -> m ()
delEventTriggerFromCache qt trn =
  modTableInCache modEventTriggerInfo qt
  where
    modEventTriggerInfo ti = do
      let etim = tiEventTriggerInfoMap ti
      return $ ti { tiEventTriggerInfoMap = M.delete trn etim }

addPermToCache
  :: (QErrM m, CacheRWM m)
  => QualifiedTable
  -> RoleName
  -> PermAccessor a
  -> a
  -> m ()
addPermToCache tn rn pa i =
  modTableInCache modRolePermInfo tn
  where
    paL = permAccToLens pa
    modRolePermInfo ti = do
      let rpim = tiRolePermInfoMap ti
          rpi  = fromMaybe mkRolePermInfo $ M.lookup rn rpim
          newRPI = rpi & paL .~ Just i
      assertPermNotExists pa rpi
      return $ ti { tiRolePermInfoMap = M.insert rn newRPI rpim }

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
delPermFromCache pa rn =
  modTableInCache modRolePermInfo
  where
    paL = permAccToLens pa
    modRolePermInfo ti = do
      let rpim = tiRolePermInfoMap ti
          rpi  = fromMaybe mkRolePermInfo $ M.lookup rn rpim
      assertPermExists pa rpi
      let newRPI = rpi & paL .~ Nothing
      return $ ti { tiRolePermInfoMap = M.insert rn newRPI rpim }

data TemplateParamInfo
  = TemplateParamInfo
  { tpiName    :: !TemplateParam
  , tpiDefault :: !(Maybe Value)
  } deriving (Show, Eq)

getDependentObjs :: SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjs = getDependentObjsWith (const True)

getDependentObjsWith :: (T.Text -> Bool) -> SchemaCache -> SchemaObjId -> [SchemaObjId]
getDependentObjsWith f sc objId =
  HS.toList $ getDependentObjsRWith f HS.empty sc objId

getDependentObjsRWith :: (T.Text -> Bool)
                      -> HS.HashSet SchemaObjId
                      -> SchemaCache -> SchemaObjId
                      -> HS.HashSet SchemaObjId
getDependentObjsRWith f visited sc objId =
  foldr go visited thisLevelDeps
  where
    thisLevelDeps = concatMap (getDependentObjsOfTableWith f objId) (scTables sc)
                    <> getDependentObjsOfQTemplateCache objId (scQTemplates sc)
    go lObjId vis =
      if HS.member lObjId vis
      then vis
      else getDependentObjsRWith f (HS.insert lObjId vis) sc lObjId

getDependentObjsOfQTemplateCache :: SchemaObjId -> QTemplateCache -> [SchemaObjId]
getDependentObjsOfQTemplateCache objId qtc =
  map (SOQTemplate . qtiName) $ filter (isDependentOn (const True) objId) $
  M.elems qtc

getDependentObjsOfTable :: SchemaObjId -> TableInfo -> [SchemaObjId]
getDependentObjsOfTable objId ti =
  rels ++ perms ++ triggers
  where
    rels  = getDependentRelsOfTable (const True) objId ti
    perms = getDependentPermsOfTable (const True) objId ti
    triggers = getDependentTriggersOfTable (const True) objId ti


getDependentObjsOfTableWith :: (T.Text -> Bool) -> SchemaObjId -> TableInfo -> [SchemaObjId]
getDependentObjsOfTableWith f objId ti =
  rels ++ perms ++ triggers
  where
    rels  = getDependentRelsOfTable f objId ti
    perms = getDependentPermsOfTable f objId ti
    triggers = getDependentTriggersOfTable f objId ti

getDependentRelsOfTable :: (T.Text -> Bool) -> SchemaObjId
                        -> TableInfo -> [SchemaObjId]
getDependentRelsOfTable rsnFn objId (TableInfo tn _ fim _ _ _ _ _) =
    map (SOTableObj tn . TORel . riName) $
    filter (isDependentOn rsnFn objId) $ getRels fim

getDependentPermsOfTable :: (T.Text -> Bool) -> SchemaObjId
                         -> TableInfo -> [SchemaObjId]
getDependentPermsOfTable rsnFn objId (TableInfo tn _ _ rpim _ _ _ _) =
  concat $ flip M.mapWithKey rpim $
  \rn rpi -> map (SOTableObj tn . TOPerm rn) $ getDependentPerms' rsnFn objId rpi

getDependentPerms' :: (T.Text -> Bool) -> SchemaObjId -> RolePermInfo -> [PermType]
getDependentPerms' rsnFn objId (RolePermInfo mipi mspi mupi mdpi) =
  mapMaybe join
  [ forM mipi $ toPermRow PTInsert
  , forM mspi $ toPermRow PTSelect
  , forM mupi $ toPermRow PTUpdate
  , forM mdpi $ toPermRow PTDelete
  ]
  where
    toPermRow :: forall a. (CachedSchemaObj a) => PermType -> a -> Maybe PermType
    toPermRow pt =
      bool Nothing (Just pt) . isDependentOn rsnFn objId

getDependentTriggersOfTable :: (T.Text -> Bool) -> SchemaObjId
                         -> TableInfo -> [SchemaObjId]
getDependentTriggersOfTable rsnFn objId (TableInfo tn _ _ _ _ _ _ et) =
  map (SOTableObj tn . TOTrigger . otiTriggerName ) $ filter (isDependentOn rsnFn objId) $ getTriggers et

getOpInfo :: TriggerName -> TableInfo -> Maybe SubscribeOpSpec -> Maybe OpTriggerInfo
getOpInfo trn ti mos= fromSubscrOpSpec <$> mos
  where
    fromSubscrOpSpec :: SubscribeOpSpec -> OpTriggerInfo
    fromSubscrOpSpec os =
      let qt = tiName ti
          cols = getColsFromSub $ sosColumns os
          schemaDeps = SchemaDependency (SOTable qt) "event trigger is dependent on table"
            : map (\col -> SchemaDependency (SOTableObj qt (TOCol col)) "event trigger is dependent on column") (toList cols)
        in OpTriggerInfo qt trn os schemaDeps
        where
          getColsFromSub sc = case sc of
            SubCStar         -> HS.fromList $ map pgiName $ getCols $ tiFieldInfoMap ti
            SubCArray pgcols -> HS.fromList pgcols
