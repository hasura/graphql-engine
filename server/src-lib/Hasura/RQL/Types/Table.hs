{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Hasura.RQL.Types.Table
       ( TableConfig(..)
       , emptyTableConfig

       , TableCoreCache
       , TableCache

       , TableInfo(..)
       , tiCoreInfo
       , tiRolePermInfoMap
       , tiEventTriggerInfoMap

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

       -- , TableConstraint(..)
       -- , ConstraintType(..)
       , ViewInfo(..)
       , isMutable
       , mutableView

       , FieldInfoMap
       , FieldInfo(..)
       , _FIColumn
       , _FIRelationship
       , _FIComputedField
       , fieldInfoName
       , fieldInfoGraphQLNames
       , getFieldInfoM
       , getPGColumnInfoM
       , getCols
       , sortCols
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
       , TableCustomRootFields(..)
       , emptyCustomRootFields

       ) where

-- import qualified Hasura.GraphQL.Context            as GC

import           Hasura.GraphQL.Utils           (showNames)
import           Hasura.Incremental             (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.Server.Utils            (duplicates)
import           Hasura.SQL.Types

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax     (Lift)

import qualified Data.HashMap.Strict            as M
import qualified Data.HashSet                   as HS
import qualified Data.Text                      as T
import qualified Language.GraphQL.Draft.Syntax  as G

data TableCustomRootFields
  = TableCustomRootFields
  { _tcrfSelect          :: !(Maybe G.Name)
  , _tcrfSelectByPk      :: !(Maybe G.Name)
  , _tcrfSelectAggregate :: !(Maybe G.Name)
  , _tcrfInsert          :: !(Maybe G.Name)
  , _tcrfInsertOne       :: !(Maybe G.Name)
  , _tcrfUpdate          :: !(Maybe G.Name)
  , _tcrfUpdateByPk      :: !(Maybe G.Name)
  , _tcrfDelete          :: !(Maybe G.Name)
  , _tcrfDeleteByPk      :: !(Maybe G.Name)
  } deriving (Show, Eq, Lift, Generic)
instance NFData TableCustomRootFields
instance Cacheable TableCustomRootFields
$(deriveToJSON (aesonDrop 5 snakeCase){omitNothingFields=True} ''TableCustomRootFields)

instance FromJSON TableCustomRootFields where
  parseJSON = withObject "Object" $ \obj -> do
    select <- obj .:? "select"
    selectByPk <- obj .:? "select_by_pk"
    selectAggregate <- obj .:? "select_aggregate"
    insert <- obj .:? "insert"
    insertOne <- obj .:? "insert_one"
    update <- obj .:? "update"
    updateByPk <- obj .:? "update_by_pk"
    delete <- obj .:? "delete"
    deleteByPk <- obj .:? "delete_by_pk"

    let duplicateRootFields = duplicates $
                              catMaybes [ select, selectByPk, selectAggregate
                                        , insert, insertOne
                                        , update, updateByPk
                                        , delete, deleteByPk
                                        ]
    when (not $ null duplicateRootFields) $ fail $ T.unpack $
      "the following custom root field names are duplicated: "
      <> showNames duplicateRootFields

    pure $ TableCustomRootFields select selectByPk selectAggregate
                                 insert insertOne update updateByPk delete deleteByPk
emptyCustomRootFields :: TableCustomRootFields
emptyCustomRootFields =
  TableCustomRootFields
  { _tcrfSelect          = Nothing
  , _tcrfSelectByPk      = Nothing
  , _tcrfSelectAggregate = Nothing
  , _tcrfInsert          = Nothing
  , _tcrfInsertOne       = Nothing
  , _tcrfUpdate          = Nothing
  , _tcrfUpdateByPk      = Nothing
  , _tcrfDelete          = Nothing
  , _tcrfDeleteByPk      = Nothing
  }

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

-- | Sort columns based on their ordinal position
sortCols :: [PGColumnInfo] -> [PGColumnInfo]
sortCols = sortBy (\l r -> compare (pgiPosition l) (pgiPosition r))

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
  , upiCheck           :: !(Maybe AnnBoolExpPartialSQL)
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

mkRolePermInfo :: RolePermInfo
mkRolePermInfo = RolePermInfo Nothing Nothing Nothing Nothing

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
   -- ^ The HTTP(s) URL which will be called with the event payload on configured operation.
   -- Must be a POST handler. This URL can be entered manually or can be picked up from an
   -- environment variable (the environment variable needs to be set before using it for
   -- this configuration).
   , etiHeaders     :: ![EventHeaderInfo]
   -- ^ Custom headers can be added to an event trigger. Each webhook request will have these
   -- headers added.
   } deriving (Show, Eq, Generic)
instance NFData EventTriggerInfo
$(deriveToJSON (aesonDrop 3 snakeCase) ''EventTriggerInfo)

type EventTriggerInfoMap = M.HashMap TriggerName EventTriggerInfo

-- data ConstraintType
--   = CTCHECK
--   | CTFOREIGNKEY
--   | CTPRIMARYKEY
--   | CTUNIQUE
--   deriving Eq

-- constraintTyToTxt :: ConstraintType -> T.Text
-- constraintTyToTxt ty = case ty of
--   CTCHECK      -> "CHECK"
--   CTFOREIGNKEY -> "FOREIGN KEY"
--   CTPRIMARYKEY -> "PRIMARY KEY"
--   CTUNIQUE     -> "UNIQUE"

-- instance Show ConstraintType where
--   show = T.unpack . constraintTyToTxt

-- instance FromJSON ConstraintType where
--   parseJSON = withText "ConstraintType" $ \case
--     "CHECK"       -> return CTCHECK
--     "FOREIGN KEY" -> return CTFOREIGNKEY
--     "PRIMARY KEY" -> return CTPRIMARYKEY
--     "UNIQUE"      -> return CTUNIQUE
--     c             -> fail $ "unexpected ConstraintType: " <> T.unpack c

-- instance ToJSON ConstraintType where
--   toJSON = String . constraintTyToTxt

-- isUniqueOrPrimary :: ConstraintType -> Bool
-- isUniqueOrPrimary = \case
--   CTPRIMARYKEY -> True
--   CTUNIQUE     -> True
--   _            -> False

-- isForeignKey :: ConstraintType -> Bool
-- isForeignKey = \case
--   CTFOREIGNKEY -> True
--   _            -> False

-- data TableConstraint
--   = TableConstraint
--   { tcType :: !ConstraintType
--   , tcName :: !ConstraintName
--   } deriving (Show, Eq)

-- $(deriveJSON (aesonDrop 2 snakeCase) ''TableConstraint)

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
  { _tcCustomRootFields  :: !TableCustomRootFields
  , _tcCustomColumnNames :: !CustomColumnNames
  } deriving (Show, Eq, Lift, Generic)
instance NFData TableConfig
instance Cacheable TableConfig
$(deriveToJSON (aesonDrop 3 snakeCase) ''TableConfig)

emptyTableConfig :: TableConfig
emptyTableConfig =
  TableConfig emptyCustomRootFields M.empty

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $ \obj ->
    TableConfig
    <$> obj .:? "custom_root_fields" .!= emptyCustomRootFields
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

type TableCoreCache = M.HashMap QualifiedTable TableCoreInfo
type TableCache = M.HashMap QualifiedTable TableInfo -- info of all tables

getFieldInfoM
  :: TableInfo -> FieldName -> Maybe FieldInfo
getFieldInfoM tableInfo fieldName
  = tableInfo ^. tiCoreInfo.tciFieldInfoMap.at fieldName

getPGColumnInfoM
  :: TableInfo -> FieldName -> Maybe PGColumnInfo
getPGColumnInfoM tableInfo fieldName =
  (^? _FIColumn) =<< getFieldInfoM tableInfo fieldName

getSelectPermissionInfoM
  :: TableInfo -> RoleName -> Maybe SelPermInfo
getSelectPermissionInfoM tableInfo roleName =
  join $ tableInfo ^? tiRolePermInfoMap.at roleName._Just.permSel

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
