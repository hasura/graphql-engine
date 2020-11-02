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
       , _FIRemoteRelationship
       , fieldInfoName
       , fieldInfoGraphQLName
       , fieldInfoGraphQLNames
       , getFieldInfoM
       , getColumnInfoM
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

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M
import qualified Data.HashSet                        as HS
import qualified Data.List.NonEmpty                  as NE
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended
import           Language.Haskell.TH.Syntax          (Lift)

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Incremental                  (Cacheable)
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.Server.Utils                 (duplicates, englishList)
import           Hasura.Session
import           Hasura.SQL.Backend


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
    for_ (nonEmpty duplicateRootFields) \duplicatedFields -> fail $ T.unpack $
      "the following custom root field names are duplicated: "
      <> englishList "and" (toTxt <$> duplicatedFields)

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

data FieldInfo (b :: Backend)
  = FIColumn !(ColumnInfo b)
  | FIRelationship !RelInfo
  | FIComputedField !(ComputedFieldInfo b)
  | FIRemoteRelationship !(RemoteFieldInfo b)
  deriving (Generic)
deriving instance Eq (FieldInfo 'Postgres)
instance Cacheable (FieldInfo 'Postgres)
instance ToJSON (FieldInfo 'Postgres) where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = snakeCase . drop 2
                   , sumEncoding = TaggedObject "type" "detail"
                   }
$(makePrisms ''FieldInfo)

type FieldInfoMap = M.HashMap FieldName

fieldInfoName :: FieldInfo 'Postgres -> FieldName
fieldInfoName = \case
  FIColumn info             -> fromPGCol $ pgiColumn info
  FIRelationship info       -> fromRel $ riName info
  FIComputedField info      -> fromComputedField $ _cfiName info
  FIRemoteRelationship info -> fromRemoteRelationship $ _rfiName info

fieldInfoGraphQLName :: FieldInfo 'Postgres -> Maybe G.Name
fieldInfoGraphQLName = \case
  FIColumn info             -> Just $ pgiName info
  FIRelationship info       -> G.mkName $ relNameToTxt $ riName info
  FIComputedField info      -> G.mkName $ computedFieldNameToText $ _cfiName info
  FIRemoteRelationship info -> G.mkName $ remoteRelationshipNameToText $ _rfiName info

-- | Returns all the field names created for the given field. Columns, object relationships, and
-- computed fields only ever produce a single field, but array relationships also contain an
-- @_aggregate@ field.
fieldInfoGraphQLNames :: FieldInfo 'Postgres -> [G.Name]
fieldInfoGraphQLNames info = case info of
  FIColumn _ -> maybeToList $ fieldInfoGraphQLName info
  FIRelationship relationshipInfo -> fold do
    name <- fieldInfoGraphQLName info
    pure $ case riType relationshipInfo of
      ObjRel -> [name]
      ArrRel -> [name, name <> $$(G.litName "_aggregate")]
  FIComputedField _ -> maybeToList $ fieldInfoGraphQLName info
  FIRemoteRelationship _ -> maybeToList $ fieldInfoGraphQLName info

getCols :: FieldInfoMap (FieldInfo backend) -> [ColumnInfo backend]
getCols = mapMaybe (^? _FIColumn) . M.elems

-- | Sort columns based on their ordinal position
sortCols :: [ColumnInfo backend] -> [ColumnInfo backend]
sortCols = sortBy (\l r -> compare (pgiPosition l) (pgiPosition r))

getRels :: FieldInfoMap (FieldInfo backend) -> [RelInfo]
getRels = mapMaybe (^? _FIRelationship) . M.elems

getComputedFieldInfos :: FieldInfoMap (FieldInfo backend) -> [ComputedFieldInfo backend]
getComputedFieldInfos = mapMaybe (^? _FIComputedField) . M.elems

isPGColInfo :: FieldInfo backend -> Bool
isPGColInfo (FIColumn _) = True
isPGColInfo _            = False

data InsPermInfo (b :: Backend)
  = InsPermInfo
  { ipiCols            :: !(HS.HashSet (Column b))
  , ipiCheck           :: !(AnnBoolExpPartialSQL b)
  , ipiSet             :: !(PreSetColsPartial b)
  , ipiBackendOnly     :: !Bool
  , ipiRequiredHeaders :: ![Text]
  } deriving (Generic)
instance NFData (InsPermInfo 'Postgres)
deriving instance Eq (InsPermInfo 'Postgres)
instance Cacheable (InsPermInfo 'Postgres)
instance ToJSON (InsPermInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase

data SelPermInfo (b :: Backend)
  = SelPermInfo
  { spiCols                 :: !(HS.HashSet (Column b))
  , spiScalarComputedFields :: !(HS.HashSet ComputedFieldName)
  , spiFilter               :: !(AnnBoolExpPartialSQL b)
  , spiLimit                :: !(Maybe Int)
  , spiAllowAgg             :: !Bool
  , spiRequiredHeaders      :: ![Text]
  } deriving (Generic)
instance NFData (SelPermInfo 'Postgres)
deriving instance Eq (SelPermInfo 'Postgres)
instance Cacheable (SelPermInfo 'Postgres)
instance ToJSON (SelPermInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase

data UpdPermInfo (b :: Backend)
  = UpdPermInfo
  { upiCols            :: !(HS.HashSet (Column b))
  , upiTable           :: !QualifiedTable
  , upiFilter          :: !(AnnBoolExpPartialSQL b)
  , upiCheck           :: !(Maybe (AnnBoolExpPartialSQL b))
  , upiSet             :: !(PreSetColsPartial b)
  , upiRequiredHeaders :: ![Text]
  } deriving (Generic)
instance NFData (UpdPermInfo 'Postgres)
deriving instance Eq (UpdPermInfo 'Postgres)
instance Cacheable (UpdPermInfo 'Postgres)
instance ToJSON (UpdPermInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase

data DelPermInfo (b :: Backend)
  = DelPermInfo
  { dpiTable           :: !QualifiedTable
  , dpiFilter          :: !(AnnBoolExpPartialSQL b)
  , dpiRequiredHeaders :: ![Text]
  } deriving (Generic)
instance NFData (DelPermInfo 'Postgres)
deriving instance Eq (DelPermInfo 'Postgres)
instance Cacheable (DelPermInfo 'Postgres)
instance ToJSON (DelPermInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase

mkRolePermInfo :: RolePermInfo backend
mkRolePermInfo = RolePermInfo Nothing Nothing Nothing Nothing

data RolePermInfo (b :: Backend)
  = RolePermInfo
  { _permIns :: !(Maybe (InsPermInfo b))
  , _permSel :: !(Maybe (SelPermInfo b))
  , _permUpd :: !(Maybe (UpdPermInfo b))
  , _permDel :: !(Maybe (DelPermInfo b))
  } deriving (Generic)
instance NFData (RolePermInfo 'Postgres)
instance ToJSON (RolePermInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 5 snakeCase

makeLenses ''RolePermInfo

type RolePermInfoMap b = M.HashMap RoleName (RolePermInfo b)

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

-- constraintTyToTxt :: ConstraintType -> Text
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
            -> Text -> m ()
mutableView qt f mVI operation =
  unless (isMutable f mVI) $ throw400 NotSupported $
  "view " <> qt <<> " is not " <> operation

data TableConfig
  = TableConfig
  { _tcCustomRootFields  :: !TableCustomRootFields
  , _tcCustomColumnNames :: !CustomColumnNames
  , _tcCustomName        :: !(Maybe G.Name)
  } deriving (Show, Eq, Lift, Generic)
instance NFData TableConfig
instance Cacheable TableConfig
$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''TableConfig)

emptyTableConfig :: TableConfig
emptyTableConfig =
  TableConfig emptyCustomRootFields M.empty Nothing

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $ \obj ->
    TableConfig
    <$> obj .:? "custom_root_fields" .!= emptyCustomRootFields
    <*> obj .:? "custom_column_names" .!= M.empty
    <*> obj .:? "custom_name"

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
type TableRawInfo b = TableCoreInfoG (ColumnInfo b) (ColumnInfo b)
-- | Fully-processed table info that includes non-column fields.
type TableCoreInfo b = TableCoreInfoG (FieldInfo b) (ColumnInfo b)

tciUniqueOrPrimaryKeyConstraints :: TableCoreInfoG a b -> Maybe (NonEmpty Constraint)
tciUniqueOrPrimaryKeyConstraints info = NE.nonEmpty $
  maybeToList (_pkConstraint <$> _tciPrimaryKey info)
  <> toList (_tciUniqueConstraints info)

data TableInfo (b :: Backend)
  = TableInfo
  { _tiCoreInfo            :: TableCoreInfo b
  , _tiRolePermInfoMap     :: !(RolePermInfoMap b)
  , _tiEventTriggerInfoMap :: !EventTriggerInfoMap
  } deriving (Generic)
instance ToJSON (TableInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase
$(makeLenses ''TableInfo)

type TableCoreCache = M.HashMap QualifiedTable (TableCoreInfo 'Postgres)
type TableCache = M.HashMap QualifiedTable (TableInfo 'Postgres) -- info of all tables

getFieldInfoM
  :: TableInfo b -> FieldName -> Maybe (FieldInfo b)
getFieldInfoM tableInfo fieldName
  = tableInfo ^. tiCoreInfo.tciFieldInfoMap.at fieldName

getColumnInfoM
  :: TableInfo b -> FieldName -> Maybe (ColumnInfo b)
getColumnInfoM tableInfo fieldName =
  (^? _FIColumn) =<< getFieldInfoM tableInfo fieldName

getSelectPermissionInfoM
  :: TableInfo b -> RoleName -> Maybe (SelPermInfo b)
getSelectPermissionInfoM tableInfo roleName =
  join $ tableInfo ^? tiRolePermInfoMap.at roleName._Just.permSel

data PermAccessor b a where
  PAInsert :: PermAccessor b (InsPermInfo b)
  PASelect :: PermAccessor b (SelPermInfo b)
  PAUpdate :: PermAccessor b (UpdPermInfo b)
  PADelete :: PermAccessor b (DelPermInfo b)

permAccToLens :: PermAccessor b a -> Lens' (RolePermInfo b) (Maybe a)
permAccToLens PAInsert = permIns
permAccToLens PASelect = permSel
permAccToLens PAUpdate = permUpd
permAccToLens PADelete = permDel

permAccToType :: PermAccessor b a -> PermType
permAccToType PAInsert = PTInsert
permAccToType PASelect = PTSelect
permAccToType PAUpdate = PTUpdate
permAccToType PADelete = PTDelete

withPermType :: PermType -> (forall a. PermAccessor backend a -> b) -> b
withPermType PTInsert f = f PAInsert
withPermType PTSelect f = f PASelect
withPermType PTUpdate f = f PAUpdate
withPermType PTDelete f = f PADelete
