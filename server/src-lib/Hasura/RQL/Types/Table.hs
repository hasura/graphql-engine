{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Table where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M
import qualified Data.HashSet                        as HS
import qualified Data.List.NonEmpty                  as NE
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Control.Lens                        hiding ((.=))
import           Data.Aeson.Casing
import           Data.Aeson.Extended
import           Data.Aeson.TH
import           Data.List.Extended                  (duplicates)
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.Types  as PG (PGDescription)

import           Hasura.Incremental                  (Cacheable)
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.Relationship
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.SQL.Backend
import           Hasura.Server.Utils                 (englishList)
import           Hasura.Session


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
  } deriving (Show, Eq, Generic)
instance NFData TableCustomRootFields
instance Cacheable TableCustomRootFields
$(deriveToJSON hasuraJSON{omitNothingFields=True} ''TableCustomRootFields)

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

    let duplicateRootFields = HS.toList $ duplicates $
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

data FieldInfo (b :: BackendType)
  = FIColumn !(ColumnInfo b)
  | FIRelationship !(RelInfo b)
  | FIComputedField !(ComputedFieldInfo b)
  | FIRemoteRelationship !(RemoteFieldInfo b)
  deriving (Generic)
deriving instance Backend b => Show (FieldInfo b)
deriving instance Backend b => Eq (FieldInfo b)
instance Backend b => Cacheable (FieldInfo b)
instance Backend b => ToJSON (FieldInfo b) where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = snakeCase . drop 2
                   , sumEncoding = TaggedObject "type" "detail"
                   }
$(makePrisms ''FieldInfo)

type FieldInfoMap = M.HashMap FieldName

fieldInfoName :: forall b. Backend b => FieldInfo b -> FieldName
fieldInfoName = \case
  FIColumn info             -> fromCol @b $ pgiColumn info
  FIRelationship info       -> fromRel $ riName info
  FIComputedField info      -> fromComputedField $ _cfiName info
  FIRemoteRelationship info -> fromRemoteRelationship $ _rfiName info

fieldInfoGraphQLName :: FieldInfo b -> Maybe G.Name
fieldInfoGraphQLName = \case
  FIColumn info             -> Just $ pgiName info
  FIRelationship info       -> G.mkName $ relNameToTxt $ riName info
  FIComputedField info      -> G.mkName $ computedFieldNameToText $ _cfiName info
  FIRemoteRelationship info -> G.mkName $ remoteRelationshipNameToText $ _rfiName info

-- | Returns all the field names created for the given field. Columns, object relationships, and
-- computed fields only ever produce a single field, but array relationships also contain an
-- @_aggregate@ field.
fieldInfoGraphQLNames :: FieldInfo b -> [G.Name]
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

getRels :: FieldInfoMap (FieldInfo backend) -> [RelInfo backend]
getRels = mapMaybe (^? _FIRelationship) . M.elems

getComputedFieldInfos :: FieldInfoMap (FieldInfo backend) -> [ComputedFieldInfo backend]
getComputedFieldInfos = mapMaybe (^? _FIComputedField) . M.elems

isPGColInfo :: FieldInfo backend -> Bool
isPGColInfo (FIColumn _) = True
isPGColInfo _            = False

data InsPermInfo (b :: BackendType)
  = InsPermInfo
  { ipiCols            :: !(HS.HashSet (Column b))
  , ipiCheck           :: !(AnnBoolExpPartialSQL b)
  , ipiSet             :: !(PreSetColsPartial b)
  , ipiBackendOnly     :: !Bool
  , ipiRequiredHeaders :: ![Text]
  } deriving (Generic)

deriving instance
  ( Backend b
  , Eq (BooleanOperators b (PartialSQLExp b))
  ) => Eq (InsPermInfo b)

instance
  ( Backend b
  , NFData (BooleanOperators b (PartialSQLExp b))
  ) => NFData (InsPermInfo b)

instance
  ( Backend b
  , Hashable  (BooleanOperators b (PartialSQLExp b))
  , Cacheable (BooleanOperators b (PartialSQLExp b))
  ) => Cacheable (InsPermInfo b)

instance
  ( Backend b
  , ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) => ToJSON (InsPermInfo b) where
  toJSON = genericToJSON hasuraJSON


data SelPermInfo (b :: BackendType)
  = SelPermInfo
  { spiCols                 :: !(M.HashMap (Column b) (Maybe (AnnColumnCaseBoolExpPartialSQL b)))
  -- ^ HashMap of accessible columns to the role, the `Column` may be mapped to
  -- an `AnnColumnCaseBoolExpPartialSQL`, which happens only in the case of an
  -- inherited role, for a non-inherited role, it will be `Nothing`. The above
  -- bool exp will determine if the column should be nullified in a row, when
  -- there aren't requisite permissions.
  , spiScalarComputedFields :: !(M.HashMap ComputedFieldName (Maybe (AnnColumnCaseBoolExpPartialSQL b)))
  -- ^ HashMap of accessible scalar computed fields to the role, mapped to
  -- `AnnColumnCaseBoolExpPartialSQL`, simililar to `spiCols`
  , spiFilter               :: !(AnnBoolExpPartialSQL b)
  , spiLimit                :: !(Maybe Int)
  , spiAllowAgg             :: !Bool
  , spiRequiredHeaders      :: ![Text]
  } deriving (Generic)

deriving instance
  ( Backend b
  , Eq (BooleanOperators b (PartialSQLExp b))
  ) => Eq (SelPermInfo b)

instance
  ( Backend b
  , NFData (BooleanOperators b (PartialSQLExp b))
  ) => NFData (SelPermInfo b)

instance
  ( Backend b
  , Hashable  (BooleanOperators b (PartialSQLExp b))
  , Cacheable (BooleanOperators b (PartialSQLExp b))
  ) => Cacheable (SelPermInfo b)

instance
  ( Backend b
  , ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) => ToJSON (SelPermInfo b) where
  toJSON = genericToJSON hasuraJSON


data UpdPermInfo (b :: BackendType)
  = UpdPermInfo
  { upiCols            :: !(HS.HashSet (Column b))
  , upiTable           :: !(TableName b)
  , upiFilter          :: !(AnnBoolExpPartialSQL b)
  , upiCheck           :: !(Maybe (AnnBoolExpPartialSQL b))
  , upiSet             :: !(PreSetColsPartial b)
  , upiRequiredHeaders :: ![Text]
  } deriving (Generic)

deriving instance
  ( Backend b
  , Eq (BooleanOperators b (PartialSQLExp b))
  ) => Eq (UpdPermInfo b)

instance
  ( Backend b
  , NFData (BooleanOperators b (PartialSQLExp b))
  ) => NFData (UpdPermInfo b)

instance
  ( Backend b
  , Hashable  (BooleanOperators b (PartialSQLExp b))
  , Cacheable (BooleanOperators b (PartialSQLExp b))
  ) => Cacheable (UpdPermInfo b)

instance
  ( Backend b
  , ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) => ToJSON (UpdPermInfo b) where
  toJSON = genericToJSON hasuraJSON


data DelPermInfo (b :: BackendType)
  = DelPermInfo
  { dpiTable           :: !(TableName b)
  , dpiFilter          :: !(AnnBoolExpPartialSQL b)
  , dpiRequiredHeaders :: ![Text]
  } deriving (Generic)

deriving instance
  ( Backend b
  , Eq (BooleanOperators b (PartialSQLExp b))
  ) => Eq (DelPermInfo b)

instance
  ( Backend b
  , NFData (BooleanOperators b (PartialSQLExp b))
  ) => NFData (DelPermInfo b)

instance
  ( Backend b
  , Hashable  (BooleanOperators b (PartialSQLExp b))
  , Cacheable (BooleanOperators b (PartialSQLExp b))
  ) => Cacheable (DelPermInfo b)

instance
  ( Backend b
  , ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) => ToJSON (DelPermInfo b) where
  toJSON = genericToJSON hasuraJSON


data RolePermInfo (b :: BackendType)
  = RolePermInfo
  { _permIns :: !(Maybe (InsPermInfo b))
  , _permSel :: !(Maybe (SelPermInfo b))
  , _permUpd :: !(Maybe (UpdPermInfo b))
  , _permDel :: !(Maybe (DelPermInfo b))
  } deriving (Generic)
instance (Backend b, NFData (BooleanOperators b (PartialSQLExp b))) => NFData (RolePermInfo b)
instance (Backend b, ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))) => ToJSON (RolePermInfo b) where
  toJSON = genericToJSON hasuraJSON

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
   } deriving (Generic, Show, Eq)
instance NFData EventTriggerInfo

instance ToJSON EventTriggerInfo where
  toJSON EventTriggerInfo{..} =
    object [ "name" .= etiName
           , "ops_def" .= etiOpsDef
           , "retry_conf" .= etiRetryConf
           , "webhook_info" .= etiWebhookInfo
           , "headers" .= etiHeaders
           ]

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

-- $(deriveJSON hasuraJSON ''TableConstraint)

data ViewInfo
  = ViewInfo
  { viIsUpdatable  :: !Bool
  , viIsDeletable  :: !Bool
  , viIsInsertable :: !Bool
  } deriving (Show, Eq, Generic)
instance NFData ViewInfo
instance Cacheable ViewInfo
$(deriveJSON hasuraJSON ''ViewInfo)

isMutable :: (ViewInfo -> Bool) -> Maybe ViewInfo -> Bool
isMutable _ Nothing   = True
isMutable f (Just vi) = f vi

type CustomColumnNames b = HashMap (Column b) G.Name

data TableConfig b
  = TableConfig
  { _tcCustomRootFields  :: !TableCustomRootFields
  , _tcCustomColumnNames :: !(CustomColumnNames b)
  , _tcCustomName        :: !(Maybe G.Name)
  } deriving (Generic)
deriving instance (Backend b) => Eq (TableConfig b)
deriving instance (Backend b) => Show (TableConfig b)
instance (Backend b) => NFData (TableConfig b)
instance (Backend b) => Cacheable (TableConfig b)
instance Backend b => ToJSON (TableConfig b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields = True}
$(makeLenses ''TableConfig)

emptyTableConfig :: (TableConfig b)
emptyTableConfig =
  TableConfig emptyCustomRootFields M.empty Nothing

instance (Backend b) => FromJSON (TableConfig b) where
  parseJSON = withObject "TableConfig" $ \obj ->
    TableConfig
    <$> obj .:? "custom_root_fields" .!= emptyCustomRootFields
    <*> obj .:? "custom_column_names" .!= M.empty
    <*> obj .:? "custom_name"

data Constraint (b :: BackendType)
  = Constraint
  { _cName :: !(ConstraintName b)
  , _cOid  :: !OID
  } deriving (Generic)
deriving instance Backend b => Eq (Constraint b)
deriving instance Backend b => Show (Constraint b)
instance Backend b => NFData (Constraint b)
instance Backend b => Hashable (Constraint b)
instance Backend b => Cacheable (Constraint b)
instance Backend b => ToJSON (Constraint b) where
  toJSON = genericToJSON hasuraJSON
instance Backend b => FromJSON (Constraint b) where
  parseJSON = genericParseJSON hasuraJSON

data PrimaryKey (b :: BackendType) a
  = PrimaryKey
  { _pkConstraint :: !(Constraint b)
  , _pkColumns    :: !(NESeq a)
  } deriving (Generic, Foldable)
deriving instance (Backend b, Eq a) => Eq (PrimaryKey b a)
deriving instance (Backend b, Show a) => Show (PrimaryKey b a)
instance (Backend b, NFData a) => NFData (PrimaryKey b a)
instance (Backend b, Hashable (NESeq a)) => Hashable (PrimaryKey b a)
instance (Backend b, Cacheable a) => Cacheable (PrimaryKey b a)
instance (Backend b, ToJSON a) => ToJSON (PrimaryKey b a) where
  toJSON = genericToJSON hasuraJSON
instance (Backend b, FromJSON a) => FromJSON (PrimaryKey b a) where
  parseJSON = genericParseJSON hasuraJSON
$(makeLenses ''PrimaryKey)

data ForeignKey (b :: BackendType)
  = ForeignKey
  { _fkConstraint    :: !(Constraint b)
  , _fkForeignTable  :: !(TableName b)
  , _fkColumnMapping :: !(HashMap (Column b) (Column b))
  } deriving (Generic)
deriving instance Backend b => Eq (ForeignKey b)
deriving instance Backend b => Show (ForeignKey b)
instance Backend b => NFData (ForeignKey b)
instance Backend b => Hashable (ForeignKey b)
instance Backend b => Cacheable (ForeignKey b)
instance Backend b => ToJSON (ForeignKey b) where
  toJSON = genericToJSON hasuraJSON
instance Backend b => FromJSON (ForeignKey b) where
  parseJSON = genericParseJSON hasuraJSON

-- | The @field@ and @primaryKeyColumn@ type parameters vary as the schema cache is built and more
-- information is accumulated. See also 'TableCoreInfo'.
data TableCoreInfoG (b :: BackendType) field primaryKeyColumn
  = TableCoreInfo
  { _tciName              :: !(TableName b)
  , _tciDescription       :: !(Maybe PG.PGDescription) -- TODO make into type family?
  , _tciSystemDefined     :: !SystemDefined
  , _tciFieldInfoMap      :: !(FieldInfoMap field)
  , _tciPrimaryKey        :: !(Maybe (PrimaryKey b primaryKeyColumn))
  , _tciUniqueConstraints :: !(HashSet (Constraint b))
  -- ^ Does /not/ include the primary key; use 'tciUniqueOrPrimaryKeyConstraints' if you need both.
  , _tciForeignKeys       :: !(HashSet (ForeignKey b))
  , _tciViewInfo          :: !(Maybe ViewInfo)
  , _tciEnumValues        :: !(Maybe EnumValues)
  , _tciCustomConfig      :: !(TableConfig b)
  } deriving (Generic)
deriving instance (Eq field, Eq pkCol, Backend b) => Eq (TableCoreInfoG b field pkCol)
instance (Cacheable field, Cacheable pkCol, Backend b) => Cacheable (TableCoreInfoG b field pkCol)
instance (Backend b, Generic pkCol, ToJSON field, ToJSON pkCol) => ToJSON (TableCoreInfoG b field pkCol) where
  toJSON = genericToJSON hasuraJSON
$(makeLenses ''TableCoreInfoG)

-- | Fully-processed table info that includes non-column fields.
type TableCoreInfo b = TableCoreInfoG b (FieldInfo b) (ColumnInfo b)

tciUniqueOrPrimaryKeyConstraints
  :: TableCoreInfoG b f pkCol -> Maybe (NonEmpty (Constraint b))
tciUniqueOrPrimaryKeyConstraints info = NE.nonEmpty $
  maybeToList (_pkConstraint <$> _tciPrimaryKey info)
  <> toList (_tciUniqueConstraints info)

data TableInfo (b :: BackendType)
  = TableInfo
  { _tiCoreInfo            :: TableCoreInfo b
  , _tiRolePermInfoMap     :: !(RolePermInfoMap b)
  , _tiEventTriggerInfoMap :: !EventTriggerInfoMap
  } deriving (Generic)
instance (Backend b, ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))) => ToJSON (TableInfo b) where
  toJSON = genericToJSON hasuraJSON
$(makeLenses ''TableInfo)

type TableCoreCache b = M.HashMap (TableName b) (TableCoreInfo b)
type TableCache b = M.HashMap (TableName b) (TableInfo b) -- info of all tables

-- | Metadata of a Postgres foreign key constraint which is being
-- extracted from database via 'src-rsr/pg_table_metadata.sql'
newtype ForeignKeyMetadata (b :: BackendType)
  = ForeignKeyMetadata
  { unForeignKeyMetadata :: ForeignKey b
  } deriving (Show, Eq, NFData, Hashable, Cacheable)

instance Backend b => FromJSON (ForeignKeyMetadata b) where
  parseJSON = withObject "ForeignKeyMetadata" \o -> do
    constraint <- o .: "constraint"
    foreignTable <- o .: "foreign_table"

    columns <- o .: "columns"
    foreignColumns <- o .: "foreign_columns"
    if (length columns == length foreignColumns) then
      pure $ ForeignKeyMetadata ForeignKey
        { _fkConstraint = constraint
        , _fkForeignTable = foreignTable
        , _fkColumnMapping = M.fromList $ zip columns foreignColumns
        }
    else fail "columns and foreign_columns differ in length"


-- | Metadata of a Postgres table which is being extracted from
-- database via 'src-rsr/pg_table_metadata.sql'
data DBTableMetadata (b :: BackendType)
  = DBTableMetadata
  { _ptmiOid               :: !OID
  , _ptmiColumns           :: ![RawColumnInfo b]
  , _ptmiPrimaryKey        :: !(Maybe (PrimaryKey b (Column b)))
  , _ptmiUniqueConstraints :: !(HashSet (Constraint b))
  -- ^ Does /not/ include the primary key!
  , _ptmiForeignKeys       :: !(HashSet (ForeignKeyMetadata b))
  , _ptmiViewInfo          :: !(Maybe ViewInfo)
  , _ptmiDescription       :: !(Maybe PG.PGDescription)
  } deriving (Generic)
deriving instance Backend b => Eq (DBTableMetadata b)
deriving instance Backend b => Show (DBTableMetadata b)
instance Backend b => NFData (DBTableMetadata b)
instance Backend b => Cacheable (DBTableMetadata b)
instance Backend b => FromJSON (DBTableMetadata b) where
  parseJSON = genericParseJSON hasuraJSON

type DBTablesMetadata b = HashMap (TableName b) (DBTableMetadata b)

getFieldInfoM
  :: TableInfo b -> FieldName -> Maybe (FieldInfo b)
getFieldInfoM tableInfo fieldName
  = tableInfo ^. tiCoreInfo.tciFieldInfoMap.at fieldName

getColumnInfoM
  :: TableInfo b -> FieldName -> Maybe (ColumnInfo b)
getColumnInfoM tableInfo fieldName =
  (^? _FIColumn) =<< getFieldInfoM tableInfo fieldName

data PermAccessor (b :: BackendType) a where
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

askFieldInfo :: (MonadError QErr m)
             => FieldInfoMap fieldInfo
             -> FieldName
             -> m fieldInfo
askFieldInfo m f =
  onNothing (M.lookup f m) $ throw400 NotExists (f <<> " does not exist")

askColumnType
  :: (MonadError QErr m, Backend backend)
  => FieldInfoMap (FieldInfo backend)
  -> Column backend
  -> Text
  -> m (ColumnType backend)
askColumnType m c msg =
  pgiType <$> askColInfo m c msg

askColInfo
  :: forall m backend
  . (MonadError QErr m, Backend backend)
  => FieldInfoMap (FieldInfo backend)
  -> Column backend
  -> Text
  -> m (ColumnInfo backend)
askColInfo m c msg = do
  fieldInfo <- modifyErr ("column " <>) $
             askFieldInfo m (fromCol @backend c)
  case fieldInfo of
    (FIColumn pgColInfo)     -> pure pgColInfo
    (FIRelationship   _)     -> throwErr "relationship"
    (FIComputedField _)      -> throwErr "computed field"
    (FIRemoteRelationship _) -> throwErr "remote relationship"
  where
    throwErr fieldType =
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a postgres column; but, "
      , c <<> " is a " <> fieldType <> "; "
      , msg
      ]
