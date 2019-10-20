{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Hasura.RQL.Types.Table
       ( TableConfig(..)
       , emptyTableConfig

       , TableCache

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

       -- , TableConstraint(..)
       -- , ConstraintType(..)
       , ViewInfo(..)
       , isMutable
       , mutableView

       , FieldInfoMap
       , FieldInfo(..)
       , _FIColumn
       , _FIRelationship
       , getFieldInfoM
       , getPGColumnInfoM
       , fieldInfoToEither
       , partitionFieldInfos
       , partitionFieldInfosWith
       , getCols
       , getRels

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

import           Hasura.Prelude
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.HashMap.Strict           as M
import qualified Data.HashSet                  as HS
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

data TableCustomRootFields
  = TableCustomRootFields
  { _tcrfSelect          :: !(Maybe G.Name)
  , _tcrfSelectByPk      :: !(Maybe G.Name)
  , _tcrfSelectAggregate :: !(Maybe G.Name)
  , _tcrfInsert          :: !(Maybe G.Name)
  , _tcrfUpdate          :: !(Maybe G.Name)
  , _tcrfDelete          :: !(Maybe G.Name)
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 5 snakeCase) ''TableCustomRootFields)

emptyCustomRootFields :: TableCustomRootFields
emptyCustomRootFields =
  TableCustomRootFields
  { _tcrfSelect          = Nothing
  , _tcrfSelectByPk      = Nothing
  , _tcrfSelectAggregate = Nothing
  , _tcrfInsert          = Nothing
  , _tcrfUpdate          = Nothing
  , _tcrfDelete          = Nothing
  }

data FieldInfo columnInfo
  = FIColumn !columnInfo
  | FIRelationship !RelInfo
  deriving (Show, Eq)
$(deriveToJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "detail"
                 }
  ''FieldInfo)
$(makePrisms ''FieldInfo)

fieldInfoToEither :: FieldInfo columnInfo -> Either columnInfo RelInfo
fieldInfoToEither (FIColumn l)       = Left l
fieldInfoToEither (FIRelationship r) = Right r

partitionFieldInfos :: [FieldInfo columnInfo] -> ([columnInfo], [RelInfo])
partitionFieldInfos = partitionFieldInfosWith (id, id)

partitionFieldInfosWith :: (columnInfo -> a, RelInfo -> b)
                        -> [FieldInfo columnInfo] -> ([a], [b])
partitionFieldInfosWith fns =
  partitionEithers . map (biMapEither fns . fieldInfoToEither)
  where
    biMapEither (f1, f2) = either (Left . f1) (Right . f2)

type FieldInfoMap columnInfo = M.HashMap FieldName (FieldInfo columnInfo)

getCols :: FieldInfoMap columnInfo -> [columnInfo]
getCols fim = lefts $ map fieldInfoToEither $ M.elems fim

getRels :: FieldInfoMap columnInfo -> [RelInfo]
getRels fim = rights $ map fieldInfoToEither $ M.elems fim

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
  { spiCols            :: !(HS.HashSet PGCol)
  , spiTable           :: !QualifiedTable
  , spiFilter          :: !AnnBoolExpPartialSQL
  , spiLimit           :: !(Maybe Int)
  , spiAllowAgg        :: !Bool
  , spiRequiredHeaders :: ![T.Text]
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
  { _tcCustomRootFields  :: !TableCustomRootFields
  , _tcCustomColumnNames :: !CustomColumnNames
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 3 snakeCase) ''TableConfig)

emptyTableConfig :: TableConfig
emptyTableConfig =
  TableConfig emptyCustomRootFields M.empty

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $ \obj ->
    TableConfig
    <$> obj .:? "custom_root_fields" .!= emptyCustomRootFields
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
$(deriveToJSON (aesonDrop 2 snakeCase) ''TableInfo)
$(makeLenses ''TableInfo)

getFieldInfoM
  :: TableInfo columnInfo -> FieldName -> Maybe (FieldInfo columnInfo)
getFieldInfoM tableInfo fieldName
  = tableInfo ^. tiFieldInfoMap.at fieldName

getPGColumnInfoM
  :: TableInfo columnInfo -> FieldName -> Maybe columnInfo
getPGColumnInfoM tableInfo fieldName =
  (^? _FIColumn) =<< getFieldInfoM tableInfo fieldName

getSelectPermissionInfoM
  :: TableInfo columnInfo -> RoleName -> Maybe SelPermInfo
getSelectPermissionInfoM tableInfo roleName =
  join $ tableInfo ^? tiRolePermInfoMap.at roleName._Just.permSel

type TableCache columnInfo = M.HashMap QualifiedTable (TableInfo columnInfo) -- info of all tables

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
