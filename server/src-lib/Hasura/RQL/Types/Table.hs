{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Table
  ( CombinedSelPermInfo (..),
    Constraint (..),
    ColumnConfig (..),
    CustomRootField (..),
    DBTableMetadata (..),
    DBTablesMetadata,
    DelPermInfo (..),
    FieldInfo (..),
    FieldInfoMap,
    ForeignKey (..),
    ForeignKeyMetadata (..),
    InsPermInfo (..),
    PrimaryKey (..),
    RolePermInfo (..),
    RolePermInfoMap,
    SelPermInfo (..),
    TableCache,
    TableConfig (..),
    TableCoreCache,
    TableCoreInfo,
    TableEventTriggers,
    TableCoreInfoG (..),
    TableCustomRootFields (..),
    TableInfo (..),
    UniqueConstraint (..),
    UpdPermInfo (..),
    ViewInfo (..),
    askColInfo,
    askColumnType,
    askFieldInfo,
    assertColumnExists,
    askRelType,
    askComputedFieldInfo,
    askRemoteRel,
    combinedSelPermInfoToSelPermInfo,
    emptyCustomRootFields,
    emptyTableConfig,
    fieldInfoGraphQLName,
    fieldInfoGraphQLNames,
    fieldInfoName,
    getAllCustomRootFields,
    getCols,
    getColumnInfoM,
    getComputedFieldInfos,
    getFieldInfoM,
    getRels,
    getRemoteFieldInfoName,
    isMutable,
    mkAdminRolePermInfo,
    permDel,
    permIns,
    permSel,
    permUpd,
    pkColumns,
    pkConstraint,
    sortCols,
    tableInfoName,
    getRolePermInfo,
    tcCustomName,
    tcCustomRootFields,
    tcComment,
    tcColumnConfig,
    tciCustomConfig,
    tciDescription,
    tciApolloFederationConfig,
    tciEnumValues,
    tciExtraTableMetadata,
    tciFieldInfoMap,
    tciForeignKeys,
    tciName,
    tciPrimaryKey,
    tciUniqueConstraints,
    tciUniqueOrPrimaryKeyConstraints,
    tciViewInfo,
    tiAdminRolePermInfo,
    tiCoreInfo,
    tiEventTriggerInfoMap,
    tiName,
    tiRolePermInfoMap,
    _FIColumn,
    _FIComputedField,
    _FIRelationship,
    _FIRemoteRelationship,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson.Casing
import Data.Aeson.Extended
import Data.Aeson.TH
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.HashMap.Strict qualified as M
import Data.HashMap.Strict.Extended qualified as M
import Data.HashMap.Strict.NonEmpty (NEHashMap)
import Data.HashMap.Strict.NonEmpty qualified as NEHashMap
import Data.HashSet qualified as HS
import Data.List.Extended (duplicates)
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Any (..), Max (..))
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types qualified as PG (PGDescription)
import Hasura.Base.Error
import Hasura.Incremental (Cacheable)
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Permission (AllowedRootFields (..), QueryRootFieldType (..), SubscriptionRootFieldType (..))
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.SQL.AnyBackend (runBackend)
import Hasura.SQL.Backend
import Hasura.Server.Utils (englishList)
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

data CustomRootField = CustomRootField
  { _crfName :: Maybe G.Name,
    _crfComment :: Comment
  }
  deriving (Show, Eq, Generic)

instance NFData CustomRootField

instance Cacheable CustomRootField

instance FromJSON CustomRootField where
  parseJSON = \case
    Null -> pure $ CustomRootField Nothing Automatic
    String text -> pure $ CustomRootField (G.mkName text) Automatic
    Object obj ->
      CustomRootField
        <$> (obj .:? "name")
        <*> (obj .:? "comment" .!= Automatic)
    val -> prependFailure "parsing CustomRootField failed, " (typeMismatch "Object, String or Null" val)

instance ToJSON CustomRootField where
  toJSON (CustomRootField Nothing Automatic) = Null
  toJSON (CustomRootField (Just name) Automatic) = String $ G.unName name
  toJSON (CustomRootField name comment) =
    object
      [ "name" .= name,
        "comment" .= comment
      ]

defaultCustomRootField :: CustomRootField
defaultCustomRootField = CustomRootField Nothing Automatic

data TableCustomRootFields = TableCustomRootFields
  { _tcrfSelect :: CustomRootField,
    _tcrfSelectByPk :: CustomRootField,
    _tcrfSelectAggregate :: CustomRootField,
    _tcrfSelectStream :: CustomRootField,
    _tcrfInsert :: CustomRootField,
    _tcrfInsertOne :: CustomRootField,
    _tcrfUpdate :: CustomRootField,
    _tcrfUpdateByPk :: CustomRootField,
    _tcrfUpdateMany :: CustomRootField,
    _tcrfDelete :: CustomRootField,
    _tcrfDeleteByPk :: CustomRootField
  }
  deriving (Show, Eq, Generic)

instance NFData TableCustomRootFields

instance Cacheable TableCustomRootFields

instance ToJSON TableCustomRootFields where
  toJSON TableCustomRootFields {..} =
    object $
      filter
        ((/= Null) . snd)
        [ "select" .= _tcrfSelect,
          "select_by_pk" .= _tcrfSelectByPk,
          "select_aggregate" .= _tcrfSelectAggregate,
          "select_stream" .= _tcrfSelectStream,
          "insert" .= _tcrfInsert,
          "insert_one" .= _tcrfInsertOne,
          "update" .= _tcrfUpdate,
          "update_by_pk" .= _tcrfUpdateByPk,
          "update_many" .= _tcrfUpdateMany,
          "delete" .= _tcrfDelete,
          "delete_by_pk" .= _tcrfDeleteByPk
        ]

instance FromJSON TableCustomRootFields where
  parseJSON = withObject "Object" $ \obj -> do
    tableCustomRootFields <-
      TableCustomRootFields
        <$> (obj .:? "select" .!= defaultCustomRootField)
        <*> (obj .:? "select_by_pk" .!= defaultCustomRootField)
        <*> (obj .:? "select_aggregate" .!= defaultCustomRootField)
        <*> (obj .:? "select_stream" .!= defaultCustomRootField)
        <*> (obj .:? "insert" .!= defaultCustomRootField)
        <*> (obj .:? "insert_one" .!= defaultCustomRootField)
        <*> (obj .:? "update" .!= defaultCustomRootField)
        <*> (obj .:? "update_by_pk" .!= defaultCustomRootField)
        <*> (obj .:? "update_many" .!= defaultCustomRootField)
        <*> (obj .:? "delete" .!= defaultCustomRootField)
        <*> (obj .:? "delete_by_pk" .!= defaultCustomRootField)

    let duplicateRootFields = HS.toList . duplicates . mapMaybe _crfName $ getAllCustomRootFields tableCustomRootFields
    for_ (nonEmpty duplicateRootFields) \duplicatedFields ->
      fail . T.unpack $
        "the following custom root field names are duplicated: "
          <> englishList "and" (toTxt <$> duplicatedFields)

    pure tableCustomRootFields

emptyCustomRootFields :: TableCustomRootFields
emptyCustomRootFields =
  TableCustomRootFields
    { _tcrfSelect = defaultCustomRootField,
      _tcrfSelectByPk = defaultCustomRootField,
      _tcrfSelectStream = defaultCustomRootField,
      _tcrfSelectAggregate = defaultCustomRootField,
      _tcrfInsert = defaultCustomRootField,
      _tcrfInsertOne = defaultCustomRootField,
      _tcrfUpdate = defaultCustomRootField,
      _tcrfUpdateByPk = defaultCustomRootField,
      _tcrfUpdateMany = defaultCustomRootField,
      _tcrfDelete = defaultCustomRootField,
      _tcrfDeleteByPk = defaultCustomRootField
    }

getAllCustomRootFields :: TableCustomRootFields -> [CustomRootField]
getAllCustomRootFields TableCustomRootFields {..} =
  [ _tcrfSelect,
    _tcrfSelectByPk,
    _tcrfSelectAggregate,
    _tcrfSelectStream,
    _tcrfInsert,
    _tcrfInsertOne,
    _tcrfUpdate,
    _tcrfUpdateByPk,
    _tcrfUpdateMany,
    _tcrfDelete,
    _tcrfDeleteByPk
  ]

data FieldInfo (b :: BackendType)
  = FIColumn (ColumnInfo b)
  | FIRelationship (RelInfo b)
  | FIComputedField (ComputedFieldInfo b)
  | FIRemoteRelationship (RemoteFieldInfo (DBJoinField b))
  deriving (Generic)

deriving instance Backend b => Eq (FieldInfo b)

instance Backend b => Cacheable (FieldInfo b)

instance Backend b => ToJSON (FieldInfo b) where
  toJSON =
    genericToJSON $
      defaultOptions
        { constructorTagModifier = snakeCase . drop 2,
          sumEncoding = TaggedObject "type" "detail"
        }

$(makePrisms ''FieldInfo)

type FieldInfoMap = M.HashMap FieldName

fieldInfoName :: forall b. Backend b => FieldInfo b -> FieldName
fieldInfoName = \case
  FIColumn info -> fromCol @b $ ciColumn info
  FIRelationship info -> fromRel $ riName info
  FIComputedField info -> fromComputedField $ _cfiName info
  FIRemoteRelationship info -> fromRemoteRelationship $ getRemoteFieldInfoName info

fieldInfoGraphQLName :: FieldInfo b -> Maybe G.Name
fieldInfoGraphQLName = \case
  FIColumn info -> Just $ ciName info
  FIRelationship info -> G.mkName $ relNameToTxt $ riName info
  FIComputedField info -> G.mkName $ computedFieldNameToText $ _cfiName info
  FIRemoteRelationship info -> G.mkName $ relNameToTxt $ getRemoteFieldInfoName info

getRemoteFieldInfoName :: RemoteFieldInfo lhsJoinField -> RelName
getRemoteFieldInfoName RemoteFieldInfo {_rfiRHS} = case _rfiRHS of
  RFISchema schema -> _rrfiName schema
  RFISource source -> runBackend source _rsfiName

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
      ArrRel -> [name, name <> Name.__aggregate]
  FIComputedField _ -> maybeToList $ fieldInfoGraphQLName info
  FIRemoteRelationship _ -> maybeToList $ fieldInfoGraphQLName info

getCols :: FieldInfoMap (FieldInfo backend) -> [ColumnInfo backend]
getCols = mapMaybe (^? _FIColumn) . M.elems

-- | Sort columns based on their ordinal position
sortCols :: [ColumnInfo backend] -> [ColumnInfo backend]
sortCols = sortBy (\l r -> compare (ciPosition l) (ciPosition r))

getRels :: FieldInfoMap (FieldInfo backend) -> [RelInfo backend]
getRels = mapMaybe (^? _FIRelationship) . M.elems

getComputedFieldInfos :: FieldInfoMap (FieldInfo backend) -> [ComputedFieldInfo backend]
getComputedFieldInfos = mapMaybe (^? _FIComputedField) . M.elems

data InsPermInfo (b :: BackendType) = InsPermInfo
  { ipiCols :: HS.HashSet (Column b),
    ipiCheck :: AnnBoolExpPartialSQL b,
    ipiSet :: PreSetColsPartial b,
    ipiBackendOnly :: Bool,
    ipiRequiredHeaders :: HS.HashSet Text
  }
  deriving (Generic)

deriving instance
  ( Backend b,
    Eq (BooleanOperators b (PartialSQLExp b)),
    Eq (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Eq (InsPermInfo b)

deriving instance
  ( Backend b,
    Show (BooleanOperators b (PartialSQLExp b)),
    Show (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Show (InsPermInfo b)

instance
  ( Backend b,
    NFData (BooleanOperators b (PartialSQLExp b)),
    NFData (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  NFData (InsPermInfo b)

instance
  ( Backend b,
    Hashable (BooleanOperators b (PartialSQLExp b)),
    Cacheable (BooleanOperators b (PartialSQLExp b)),
    Cacheable (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Cacheable (InsPermInfo b)

instance
  ( Backend b,
    ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) =>
  ToJSON (InsPermInfo b)
  where
  toJSON = genericToJSON hasuraJSON

-- | This type is only used as an intermediate type
--   to combine more than one select permissions for inherited roles.
data CombinedSelPermInfo (b :: BackendType) = CombinedSelPermInfo
  { cspiCols :: [(M.HashMap (Column b) (Maybe (AnnColumnCaseBoolExpPartialSQL b)))],
    cspiComputedFields :: [(M.HashMap ComputedFieldName (Maybe (AnnColumnCaseBoolExpPartialSQL b)))],
    cspiFilter :: [(AnnBoolExpPartialSQL b)],
    cspiLimit :: Maybe (Max Int),
    cspiAllowAgg :: Any,
    cspiRequiredHeaders :: HS.HashSet Text,
    cspiAllowedQueryRootFieldTypes :: AllowedRootFields QueryRootFieldType,
    cspiAllowedSubscriptionRootFieldTypes :: AllowedRootFields SubscriptionRootFieldType
  }

instance (Backend b) => Semigroup (CombinedSelPermInfo b) where
  CombinedSelPermInfo colsL scalarComputedFieldsL filterL limitL allowAggL reqHeadersL allowedQueryRFTypesL allowedSubsRFTypesL
    <> CombinedSelPermInfo colsR scalarComputedFieldsR filterR limitR allowAggR reqHeadersR allowedQueryRFTypesR allowedSubsRFTypesR =
      CombinedSelPermInfo
        (colsL <> colsR)
        (scalarComputedFieldsL <> scalarComputedFieldsR)
        (filterL <> filterR)
        (limitL <> limitR)
        (allowAggL <> allowAggR)
        (reqHeadersL <> reqHeadersR)
        (allowedQueryRFTypesL <> allowedQueryRFTypesR)
        (allowedSubsRFTypesL <> allowedSubsRFTypesR)

combinedSelPermInfoToSelPermInfo ::
  Backend b =>
  Int ->
  CombinedSelPermInfo b ->
  SelPermInfo b
combinedSelPermInfoToSelPermInfo selPermsCount CombinedSelPermInfo {..} =
  SelPermInfo
    (mergeColumnsWithBoolExp <$> M.unionsAll cspiCols)
    (mergeColumnsWithBoolExp <$> M.unionsAll cspiComputedFields)
    (BoolOr cspiFilter)
    (getMax <$> cspiLimit)
    (getAny cspiAllowAgg)
    cspiRequiredHeaders
    cspiAllowedQueryRootFieldTypes
    cspiAllowedSubscriptionRootFieldTypes
  where
    mergeColumnsWithBoolExp ::
      NonEmpty (Maybe (GBoolExp b (AnnColumnCaseBoolExpField b (PartialSQLExp b)))) ->
      Maybe (GBoolExp b (AnnColumnCaseBoolExpField b (PartialSQLExp b)))
    mergeColumnsWithBoolExp booleanExpressions
      -- when all the parent roles have a select permission, then we set
      -- the case boolean expression to `Nothing`. Suppose this were not done, then
      -- the resulting boolean expression will an expression which will always evaluate to
      -- `True`. So, to avoid additional computations, we just set the case boolean expression
      -- to `Nothing`.
      --
      -- Suppose, an inherited role, `inherited_role` inherits from two roles `role1` and `role2`.
      -- `role1` has the filter: `{"published": {"eq": true}}` and `role2` has the filter:
      -- `{"early_preview": {"eq": true}}` then the filter boolean expression of the inherited select permission will be
      -- the boolean OR of the parent roles filters.

      --  Now, let's say both `role1` and `role2` allow access to
      -- the `title` column of the table, the case boolean expression of the `title` column will be
      -- the boolean OR of the parent roles filters i.e. same as the filter of the select permission. Since,
      -- the column case boolean expression is equal to the row filter boolean expression, the column
      -- case boolean expression will always evaluate to `True`, since the column case boolean expression
      -- will always evaluate to `True`, we simply remove the boolean case expression when for a column all
      -- the select permissions exists.
      | selPermsCount == length booleanExpressions = Nothing
      | otherwise =
        let nonNothingBoolExps = catMaybes $ toList booleanExpressions
         in bool (Just $ BoolOr nonNothingBoolExps) Nothing $ null nonNothingBoolExps

data SelPermInfo (b :: BackendType) = SelPermInfo
  { -- | HashMap of accessible columns to the role, the `Column` may be mapped to
    -- an `AnnColumnCaseBoolExpPartialSQL`, which happens only in the case of an
    -- inherited role, for a non-inherited role, it will be `Nothing`. The above
    -- bool exp will determine if the column should be nullified in a row, when
    -- there aren't requisite permissions.
    spiCols :: M.HashMap (Column b) (Maybe (AnnColumnCaseBoolExpPartialSQL b)),
    -- | HashMap of accessible computed fields to the role, mapped to
    -- `AnnColumnCaseBoolExpPartialSQL`, simililar to `spiCols`.
    -- These computed fields do not return rows of existing table.
    spiComputedFields :: M.HashMap ComputedFieldName (Maybe (AnnColumnCaseBoolExpPartialSQL b)),
    spiFilter :: AnnBoolExpPartialSQL b,
    spiLimit :: Maybe Int,
    spiAllowAgg :: Bool,
    spiRequiredHeaders :: HashSet Text,
    -- | allowed root field types to be exposed in the query_root
    spiAllowedQueryRootFields :: AllowedRootFields QueryRootFieldType,
    -- | allowed root field types to be exposed in the subscription_root
    spiAllowedSubscriptionRootFields :: AllowedRootFields SubscriptionRootFieldType
  }
  deriving (Generic)

deriving instance
  ( Backend b,
    Eq (BooleanOperators b (PartialSQLExp b)),
    Eq (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Eq (SelPermInfo b)

deriving instance
  ( Backend b,
    Show (BooleanOperators b (PartialSQLExp b)),
    Show (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Show (SelPermInfo b)

instance
  ( Backend b,
    NFData (BooleanOperators b (PartialSQLExp b)),
    NFData (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  NFData (SelPermInfo b)

instance
  ( Backend b,
    Hashable (BooleanOperators b (PartialSQLExp b)),
    Cacheable (BooleanOperators b (PartialSQLExp b)),
    Cacheable (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Cacheable (SelPermInfo b)

instance
  ( Backend b,
    ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) =>
  ToJSON (SelPermInfo b)
  where
  toJSON = genericToJSON hasuraJSON

data UpdPermInfo (b :: BackendType) = UpdPermInfo
  { upiCols :: HS.HashSet (Column b),
    upiTable :: TableName b,
    upiFilter :: AnnBoolExpPartialSQL b,
    upiCheck :: Maybe (AnnBoolExpPartialSQL b),
    upiSet :: PreSetColsPartial b,
    upiBackendOnly :: Bool,
    upiRequiredHeaders :: HashSet Text
  }
  deriving (Generic)

deriving instance
  ( Backend b,
    Eq (BooleanOperators b (PartialSQLExp b)),
    Eq (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Eq (UpdPermInfo b)

deriving instance
  ( Backend b,
    Show (BooleanOperators b (PartialSQLExp b)),
    Show (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Show (UpdPermInfo b)

instance
  ( Backend b,
    NFData (BooleanOperators b (PartialSQLExp b)),
    NFData (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  NFData (UpdPermInfo b)

instance
  ( Backend b,
    Hashable (BooleanOperators b (PartialSQLExp b)),
    Cacheable (BooleanOperators b (PartialSQLExp b)),
    Cacheable (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Cacheable (UpdPermInfo b)

instance
  ( Backend b,
    ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) =>
  ToJSON (UpdPermInfo b)
  where
  toJSON = genericToJSON hasuraJSON

data DelPermInfo (b :: BackendType) = DelPermInfo
  { dpiTable :: TableName b,
    dpiFilter :: AnnBoolExpPartialSQL b,
    dpiBackendOnly :: !Bool,
    dpiRequiredHeaders :: HashSet Text
  }
  deriving (Generic)

deriving instance
  ( Backend b,
    Eq (BooleanOperators b (PartialSQLExp b)),
    Eq (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Eq (DelPermInfo b)

deriving instance
  ( Backend b,
    Show (BooleanOperators b (PartialSQLExp b)),
    Show (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Show (DelPermInfo b)

instance
  ( Backend b,
    NFData (BooleanOperators b (PartialSQLExp b)),
    NFData (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  NFData (DelPermInfo b)

instance
  ( Backend b,
    Hashable (BooleanOperators b (PartialSQLExp b)),
    Cacheable (BooleanOperators b (PartialSQLExp b)),
    Cacheable (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  Cacheable (DelPermInfo b)

instance
  ( Backend b,
    ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) =>
  ToJSON (DelPermInfo b)
  where
  toJSON = genericToJSON hasuraJSON

data RolePermInfo (b :: BackendType) = RolePermInfo
  { _permIns :: Maybe (InsPermInfo b),
    _permSel :: Maybe (SelPermInfo b),
    _permUpd :: Maybe (UpdPermInfo b),
    _permDel :: Maybe (DelPermInfo b)
  }
  deriving (Generic)

instance
  ( Backend b,
    NFData (BooleanOperators b (PartialSQLExp b)),
    NFData (FunctionArgumentExp b (PartialSQLExp b))
  ) =>
  NFData (RolePermInfo b)

instance
  ( Backend b,
    ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) =>
  ToJSON (RolePermInfo b)
  where
  toJSON = genericToJSON hasuraJSON

makeLenses ''RolePermInfo

type RolePermInfoMap b = M.HashMap RoleName (RolePermInfo b)

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
--   { tcType :: ConstraintType
--   , tcName :: ConstraintName
--   } deriving (Show, Eq)

-- $(deriveJSON hasuraJSON ''TableConstraint)

data ViewInfo = ViewInfo
  { viIsUpdatable :: Bool,
    viIsDeletable :: Bool,
    viIsInsertable :: Bool
  }
  deriving (Show, Eq, Generic)

instance NFData ViewInfo

instance Cacheable ViewInfo

$(deriveJSON hasuraJSON ''ViewInfo)

isMutable :: (ViewInfo -> Bool) -> Maybe ViewInfo -> Bool
isMutable _ Nothing = True
isMutable f (Just vi) = f vi

data ColumnConfig = ColumnConfig
  { _ccfgCustomName :: Maybe G.Name,
    _ccfgComment :: Comment
  }
  deriving stock (Eq, Show, Generic)

instance NFData ColumnConfig

instance Cacheable ColumnConfig

instance ToJSON ColumnConfig where
  toJSON ColumnConfig {..} =
    object $
      filter
        ((/= Null) . snd)
        [ "custom_name" .= _ccfgCustomName,
          "comment" .= _ccfgComment
        ]

instance FromJSON ColumnConfig where
  parseJSON = withObject "ColumnConfig" $ \obj ->
    ColumnConfig
      <$> obj .:? "custom_name"
      <*> obj .:? "comment" .!= Automatic

instance Semigroup ColumnConfig where
  a <> b = ColumnConfig customName comment
    where
      customName = _ccfgCustomName a <|> _ccfgCustomName b
      comment = case (_ccfgComment a, _ccfgComment b) of
        (Automatic, explicit) -> explicit
        (explicit, _) -> explicit

instance Monoid ColumnConfig where
  mempty = ColumnConfig Nothing Automatic

data TableConfig b = TableConfig
  { _tcCustomRootFields :: TableCustomRootFields,
    _tcColumnConfig :: HashMap (Column b) ColumnConfig,
    _tcCustomName :: Maybe G.Name,
    _tcComment :: Comment
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (TableConfig b)

deriving instance (Backend b) => Show (TableConfig b)

instance (Backend b) => NFData (TableConfig b)

instance (Backend b) => Cacheable (TableConfig b)

$(makeLenses ''TableConfig)

emptyTableConfig :: TableConfig b
emptyTableConfig =
  TableConfig emptyCustomRootFields M.empty Nothing Automatic

instance (Backend b) => FromJSON (TableConfig b) where
  parseJSON = withObject "TableConfig" $ \obj -> do
    TableConfig
      <$> obj .:? "custom_root_fields" .!= emptyCustomRootFields
      <*> parseColumnConfig obj
      <*> obj .:? "custom_name"
      <*> obj .:? "comment" .!= Automatic
    where
      -- custom_column_names is a deprecated property that has been replaced by column_config.
      -- We merge custom_column_names into column_config transparently to maintain backwards
      -- compatibility (with both the metadata API and the metadata JSON saved in the HGE DB)
      -- custom_column_names can be removed once the deprecation period has expired and we get rid of it
      parseColumnConfig :: Object -> Parser (HashMap (Column b) ColumnConfig)
      parseColumnConfig obj = do
        columnConfig <- obj .:? "column_config" .!= M.empty
        legacyCustomColumnNames <- obj .:? "custom_column_names" .!= M.empty
        let legacyColumnConfig = (\name -> ColumnConfig (Just name) Automatic) <$> legacyCustomColumnNames
        pure $ M.unionWith (<>) columnConfig legacyColumnConfig -- columnConfig takes precedence over legacy

instance (Backend b) => ToJSON (TableConfig b) where
  toJSON TableConfig {..} =
    object $
      filter
        ((/= Null) . snd)
        [ "custom_root_fields" .= _tcCustomRootFields,
          -- custom_column_names is a deprecated property that has been replaced by column_config.
          -- We are retaining it here, sourcing its values from column_config, for backwards-compatibility
          -- custom_column_names can be removed once the deprecation period has expired and we get rid of it
          "custom_column_names" .= mapMaybe _ccfgCustomName _tcColumnConfig,
          "column_config" .= M.filter (/= mempty) _tcColumnConfig,
          "custom_name" .= _tcCustomName,
          "comment" .= _tcComment
        ]

data Constraint (b :: BackendType) = Constraint
  { _cName :: ConstraintName b,
    _cOid :: OID
  }
  deriving (Generic)

deriving instance Backend b => Eq (Constraint b)

deriving instance Backend b => Show (Constraint b)

instance Backend b => NFData (Constraint b)

instance Backend b => Hashable (Constraint b)

instance Backend b => Cacheable (Constraint b)

instance Backend b => ToJSON (Constraint b) where
  toJSON = genericToJSON hasuraJSON

instance Backend b => FromJSON (Constraint b) where
  parseJSON = genericParseJSON hasuraJSON

data PrimaryKey (b :: BackendType) a = PrimaryKey
  { _pkConstraint :: Constraint b,
    _pkColumns :: NESeq a
  }
  deriving (Generic, Foldable)

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

-- | Data type modelling uniqueness constraints. Occasionally this will include
-- primary keys, although those are tracked separately in `TableCoreInfoG`.
--
-- For more information about unique constraints, visit the postgresql documentation:
-- <https://www.postgresql.org/docs/current/ddl-constraints.html#DDL-CONSTRAINTS-UNIQUE-CONSTRAINTS>.
data UniqueConstraint (b :: BackendType) = UniqueConstraint
  { -- | The postgresql name and object id of a unique constraint.
    _ucConstraint :: Constraint b,
    -- | The set of columns which should be unique for this particular constraint.
    --   Used for permissions calculation.
    _ucColumns :: HashSet (Column b)
  }
  deriving (Generic)

deriving instance Backend b => Eq (UniqueConstraint b)

deriving instance Backend b => Show (UniqueConstraint b)

instance Backend b => NFData (UniqueConstraint b)

instance Backend b => Hashable (UniqueConstraint b)

instance Backend b => Cacheable (UniqueConstraint b)

instance Backend b => ToJSON (UniqueConstraint b) where
  toJSON = genericToJSON hasuraJSON

instance Backend b => FromJSON (UniqueConstraint b) where
  parseJSON = genericParseJSON hasuraJSON

data ForeignKey (b :: BackendType) = ForeignKey
  { _fkConstraint :: Constraint b,
    _fkForeignTable :: TableName b,
    _fkColumnMapping :: NEHashMap (Column b) (Column b)
  }
  deriving (Generic)

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
data TableCoreInfoG (b :: BackendType) field primaryKeyColumn = TableCoreInfo
  { _tciName :: TableName b,
    _tciDescription :: Maybe PG.PGDescription, -- TODO make into type family?
    _tciFieldInfoMap :: FieldInfoMap field,
    _tciPrimaryKey :: Maybe (PrimaryKey b primaryKeyColumn),
    -- | Does /not/ include the primary key; use 'tciUniqueOrPrimaryKeyConstraints' if you need both.
    _tciUniqueConstraints :: HashSet (UniqueConstraint b),
    _tciForeignKeys :: HashSet (ForeignKey b),
    _tciViewInfo :: Maybe ViewInfo,
    _tciEnumValues :: Maybe EnumValues,
    _tciCustomConfig :: TableConfig b,
    _tciExtraTableMetadata :: ExtraTableMetadata b,
    _tciApolloFederationConfig :: Maybe ApolloFederationConfig
  }
  deriving (Generic)

deriving instance (Eq field, Eq pkCol, Backend b) => Eq (TableCoreInfoG b field pkCol)

instance (Cacheable field, Cacheable pkCol, Backend b) => Cacheable (TableCoreInfoG b field pkCol)

instance (Backend b, Generic pkCol, ToJSON field, ToJSON pkCol) => ToJSON (TableCoreInfoG b field pkCol) where
  toJSON = genericToJSON hasuraJSON

$(makeLenses ''TableCoreInfoG)

-- | Fully-processed table info that includes non-column fields.
type TableCoreInfo b = TableCoreInfoG b (FieldInfo b) (ColumnInfo b)

tciUniqueOrPrimaryKeyConstraints ::
  forall b f.
  (Eq (Column b), Hashable (Column b)) =>
  TableCoreInfoG b f (ColumnInfo b) ->
  Maybe (NonEmpty (UniqueConstraint b))
tciUniqueOrPrimaryKeyConstraints info =
  NE.nonEmpty $
    maybeToList (primaryToUnique <$> _tciPrimaryKey info)
      <> (toList (_tciUniqueConstraints info))
  where
    primaryToUnique :: PrimaryKey b (ColumnInfo b) -> UniqueConstraint b
    primaryToUnique pk = UniqueConstraint (_pkConstraint pk) (HS.fromList . fmap ciColumn . toList $ _pkColumns pk)

data TableInfo (b :: BackendType) = TableInfo
  { _tiCoreInfo :: TableCoreInfo b,
    _tiRolePermInfoMap :: RolePermInfoMap b,
    _tiEventTriggerInfoMap :: EventTriggerInfoMap b,
    _tiAdminRolePermInfo :: RolePermInfo b
  }
  deriving (Generic)

instance
  ( Backend b,
    ToJSONKeyValue (BooleanOperators b (PartialSQLExp b))
  ) =>
  ToJSON (TableInfo b)
  where
  toJSON = genericToJSON hasuraJSON

$(makeLenses ''TableInfo)

tiName :: Lens' (TableInfo b) (TableName b)
tiName = tiCoreInfo . tciName

tableInfoName :: TableInfo b -> TableName b
tableInfoName = view tiName

getRolePermInfo :: RoleName -> TableInfo b -> RolePermInfo b
getRolePermInfo role tableInfo
  | role == adminRoleName = _tiAdminRolePermInfo tableInfo
  | otherwise =
    fromMaybe
      (RolePermInfo Nothing Nothing Nothing Nothing)
      (M.lookup role $ _tiRolePermInfoMap tableInfo)

type TableCoreCache b = M.HashMap (TableName b) (TableCoreInfo b)

type TableCache b = M.HashMap (TableName b) (TableInfo b) -- info of all tables

-- map of all event triggers on the table
type TableEventTriggers b = M.HashMap (TableName b) [TriggerName]

-- | Metadata of a Postgres foreign key constraint which is being
-- extracted from database via 'src-rsr/pg_table_metadata.sql'
newtype ForeignKeyMetadata (b :: BackendType) = ForeignKeyMetadata
  { unForeignKeyMetadata :: ForeignKey b
  }
  deriving (Show, Eq, NFData, Hashable, Cacheable)

instance Backend b => FromJSON (ForeignKeyMetadata b) where
  parseJSON = withObject "ForeignKeyMetadata" \o -> do
    constraint <- o .: "constraint"
    foreignTable <- o .: "foreign_table"

    columns <-
      o .: "columns" >>= \case
        x : xs -> pure (x :| xs)
        [] -> fail "columns must be non-empty"

    foreignColumns <-
      o .: "foreign_columns" >>= \case
        x : xs -> pure (x :| xs)
        [] -> fail "foreign_columns must be non-empty"

    unless (length columns == length foreignColumns) do
      fail "columns and foreign_columns differ in length"

    pure $
      ForeignKeyMetadata
        ForeignKey
          { _fkConstraint = constraint,
            _fkForeignTable = foreignTable,
            _fkColumnMapping =
              NEHashMap.fromNonEmpty $
                NE.zip columns foreignColumns
          }

-- | Metadata of any Backend table which is being extracted from source database
data DBTableMetadata (b :: BackendType) = DBTableMetadata
  { _ptmiOid :: OID,
    _ptmiColumns :: [RawColumnInfo b],
    _ptmiPrimaryKey :: Maybe (PrimaryKey b (Column b)),
    -- | Does /not/ include the primary key
    _ptmiUniqueConstraints :: HashSet (UniqueConstraint b),
    _ptmiForeignKeys :: HashSet (ForeignKeyMetadata b),
    _ptmiViewInfo :: Maybe ViewInfo,
    _ptmiDescription :: Maybe PG.PGDescription,
    _ptmiExtraTableMetadata :: ExtraTableMetadata b
  }
  deriving (Generic)

deriving instance Backend b => Eq (DBTableMetadata b)

deriving instance Backend b => Show (DBTableMetadata b)

instance Backend b => NFData (DBTableMetadata b)

instance Backend b => Cacheable (DBTableMetadata b)

instance Backend b => FromJSON (DBTableMetadata b) where
  parseJSON = genericParseJSON hasuraJSON

type DBTablesMetadata b = HashMap (TableName b) (DBTableMetadata b)

getFieldInfoM ::
  TableInfo b -> FieldName -> Maybe (FieldInfo b)
getFieldInfoM tableInfo fieldName =
  tableInfo ^. tiCoreInfo . tciFieldInfoMap . at fieldName

getColumnInfoM ::
  TableInfo b -> FieldName -> Maybe (ColumnInfo b)
getColumnInfoM tableInfo fieldName =
  (^? _FIColumn) =<< getFieldInfoM tableInfo fieldName

askFieldInfo ::
  (MonadError QErr m) =>
  FieldInfoMap fieldInfo ->
  FieldName ->
  m fieldInfo
askFieldInfo m f =
  onNothing (M.lookup f m) $ throw400 NotExists (f <<> " does not exist")

askColumnType ::
  (MonadError QErr m, Backend backend) =>
  FieldInfoMap (FieldInfo backend) ->
  Column backend ->
  Text ->
  m (ColumnType backend)
askColumnType m c msg =
  ciType <$> askColInfo m c msg

askColInfo ::
  forall m backend.
  (MonadError QErr m, Backend backend) =>
  FieldInfoMap (FieldInfo backend) ->
  Column backend ->
  Text ->
  m (ColumnInfo backend)
askColInfo m c msg = do
  fieldInfo <-
    modifyErr ("column " <>) $
      askFieldInfo m (fromCol @backend c)
  case fieldInfo of
    (FIColumn colInfo) -> pure colInfo
    (FIRelationship _) -> throwErr "relationship"
    (FIComputedField _) -> throwErr "computed field"
    (FIRemoteRelationship _) -> throwErr "remote relationship"
  where
    throwErr fieldType =
      throwError $
        err400 UnexpectedPayload $
          "expecting a database column; but, "
            <> c <<> " is a "
            <> fieldType
            <> "; "
            <> msg

askComputedFieldInfo ::
  (MonadError QErr m) =>
  FieldInfoMap (FieldInfo backend) ->
  ComputedFieldName ->
  m (ComputedFieldInfo backend)
askComputedFieldInfo fields computedField = do
  fieldInfo <-
    modifyErr ("computed field " <>) $
      askFieldInfo fields $ fromComputedField computedField
  case fieldInfo of
    (FIColumn _) -> throwErr "column"
    (FIRelationship _) -> throwErr "relationship"
    (FIRemoteRelationship _) -> throwErr "remote relationship"
    (FIComputedField cci) -> pure cci
  where
    throwErr fieldType =
      throwError $
        err400 UnexpectedPayload $
          "expecting a computed field; but, "
            <> computedField <<> " is a "
            <> fieldType

assertColumnExists ::
  forall backend m.
  (MonadError QErr m, Backend backend) =>
  FieldInfoMap (FieldInfo backend) ->
  Text ->
  Column backend ->
  m ()
assertColumnExists m msg c = do
  void $ askColInfo m c msg

askRelType ::
  (MonadError QErr m) =>
  FieldInfoMap (FieldInfo backend) ->
  RelName ->
  Text ->
  m (RelInfo backend)
askRelType m r msg = do
  colInfo <-
    modifyErr ("relationship " <>) $
      askFieldInfo m (fromRel r)
  case colInfo of
    (FIRelationship relInfo) -> return relInfo
    _ ->
      throwError $
        err400 UnexpectedPayload $
          "expecting a relationship; but, "
            <> r <<> " is a postgres column; "
            <> msg

askRemoteRel ::
  (MonadError QErr m) =>
  FieldInfoMap (FieldInfo backend) ->
  RelName ->
  m (RemoteFieldInfo (DBJoinField backend))
askRemoteRel fieldInfoMap relName = do
  fieldInfo <- askFieldInfo fieldInfoMap (fromRemoteRelationship relName)
  case fieldInfo of
    (FIRemoteRelationship remoteFieldInfo) -> return remoteFieldInfo
    _ ->
      throw400 UnexpectedPayload "expecting a remote relationship"

mkAdminRolePermInfo :: Backend b => TableCoreInfo b -> RolePermInfo b
mkAdminRolePermInfo tableInfo =
  RolePermInfo (Just i) (Just s) (Just u) (Just d)
  where
    fields = _tciFieldInfoMap tableInfo
    pgCols = map ciColumn $ getCols fields
    pgColsWithFilter = M.fromList $ map (,Nothing) pgCols
    computedFields =
      -- Fetch the list of computed fields not returning rows of existing table.
      -- For other computed fields returning existing table rows, the admin role can query them
      -- as their permissions are derived from returning table permissions.
      HS.fromList $ map _cfiName $ removeComputedFieldsReturningExistingTable $ getComputedFieldInfos fields
    computedFields' = HS.toMap computedFields $> Nothing

    tableName = _tciName tableInfo
    i = InsPermInfo (HS.fromList pgCols) annBoolExpTrue M.empty False mempty
    s = SelPermInfo pgColsWithFilter computedFields' annBoolExpTrue Nothing True mempty ARFAllowAllRootFields ARFAllowAllRootFields
    u = UpdPermInfo (HS.fromList pgCols) tableName annBoolExpTrue Nothing M.empty False mempty
    d = DelPermInfo tableName annBoolExpTrue False mempty
