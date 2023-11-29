{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Table.Cache
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
    GraphQLType (..),
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
    isListType,
    isMutable,
    isNullableType,
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
    tableArrayRelationships,
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
    tciRawColumns,
    tiAdminRolePermInfo,
    tiCoreInfo,
    tiEventTriggerInfoMap,
    tiName,
    tiRolePermInfoMap,
    _FIColumn,
    _FIComputedField,
    _FIRelationship,
    _FIRemoteRelationship,
    setFieldNameCase,
  )
where

import Autodocodec
  ( HasCodec (codec),
    dimapCodec,
    disjointEitherCodec,
    hashMapCodec,
    nullCodec,
    optionalFieldOrNullWith',
    optionalFieldOrNullWithOmittedDefault',
    optionalFieldWithDefault',
    optionalFieldWithDefaultWith',
    optionalFieldWithOmittedDefault',
  )
import Autodocodec qualified as AC
import Autodocodec.Extended (graphQLFieldNameCodec)
import Control.Lens hiding ((.=))
import Data.Aeson qualified as J
import Data.Aeson.Casing
import Data.Aeson.Extended
import Data.Aeson.TH
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.NonEmpty (NEHashMap)
import Data.HashMap.Strict.NonEmpty qualified as NEHashMap
import Data.HashSet qualified as HS
import Data.List.Extended (duplicates)
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (Any (..), Max (..))
import Data.Text qualified as T
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types qualified as Postgres (PGDescription)
import Hasura.Base.Error
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.BoolExp.Lenses (_RedactIfFalse)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Permission (AllowedRootFields (..), QueryRootFieldType (..), SubscriptionRootFieldType (..), ValidateInput (..))
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Roles (RoleName, adminRoleName)
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend (runBackend)
import Language.GraphQL.Draft.Parser qualified as GParse
import Language.GraphQL.Draft.Printer qualified as GPrint
import Language.GraphQL.Draft.Syntax qualified as G
import Text.Builder qualified as T

-- | A wrapper around 'G.GType' which allows us to define custom JSON
-- instances.
--
-- TODO: this name is ambiguous, and conflicts with
-- Hasura.RQL.DDL.RemoteSchema.Permission.GraphQLType; it should perhaps be
-- renamed, made internal to this module, or removed altogether?
newtype GraphQLType = GraphQLType {unGraphQLType :: G.GType}
  deriving (Show, Eq, Ord, Generic, NFData)

instance HasCodec GraphQLType where
  codec = AC.bimapCodec dec enc codec
    where
      dec t = case GParse.parseGraphQLType t of
        Left _ -> Left $ "not a valid GraphQL type: " <> T.unpack t
        Right a -> Right $ GraphQLType a
      enc = T.run . GPrint.graphQLType . unGraphQLType

instance J.ToJSON GraphQLType where
  toJSON = J.toJSON . T.run . GPrint.graphQLType . unGraphQLType

instance J.FromJSON GraphQLType where
  parseJSON =
    J.withText "GraphQLType" $ \t ->
      case GParse.parseGraphQLType t of
        Left _ -> fail $ "not a valid GraphQL type: " <> T.unpack t
        Right a -> return $ GraphQLType a

isListType :: GraphQLType -> Bool
isListType = coerce G.isListType

isNullableType :: GraphQLType -> Bool
isNullableType = coerce G.isNullable

data CustomRootField = CustomRootField
  { _crfName :: Maybe G.Name,
    _crfComment :: Comment
  }
  deriving (Show, Eq, Generic)

instance NFData CustomRootField

instance HasCodec CustomRootField where
  codec =
    dimapCodec dec enc
      $ disjointEitherCodec nullCodec
      $ disjointEitherCodec (codec @Text) nameAndComment
    where
      nameAndComment =
        AC.object "CustomRootField"
          $ CustomRootField
          <$> optionalFieldOrNullWith' "name" graphQLFieldNameCodec
          AC..= _crfName
            <*> optionalFieldOrNullWithOmittedDefault' "comment" Automatic
          AC..= _crfComment

      dec = \case
        Left _ -> CustomRootField Nothing Automatic
        Right (Left text) -> CustomRootField (G.mkName text) Automatic
        Right (Right obj) -> obj

      enc = \case
        (CustomRootField Nothing Automatic) -> Left ()
        (CustomRootField (Just name) Automatic) -> Right $ Left $ G.unName name
        obj -> Right $ Right obj

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

instance HasCodec TableCustomRootFields where
  codec =
    AC.object "TableCustomRootFields"
      $ TableCustomRootFields
      <$> field "select"
      AC..= _tcrfSelect
        <*> field "select_by_pk"
      AC..= _tcrfSelectByPk
        <*> field "select_aggregate"
      AC..= _tcrfSelectAggregate
        <*> field "select_stream"
      AC..= _tcrfSelectStream
        <*> field "insert"
      AC..= _tcrfInsert
        <*> field "insert_one"
      AC..= _tcrfInsertOne
        <*> field "update"
      AC..= _tcrfUpdate
        <*> field "update_by_pk"
      AC..= _tcrfUpdateByPk
        <*> field "update_many"
      AC..= _tcrfUpdateMany
        <*> field "delete"
      AC..= _tcrfDelete
        <*> field "delete_by_pk"
      AC..= _tcrfDeleteByPk
    where
      field name = optionalFieldWithOmittedDefault' name defaultCustomRootField

instance ToJSON TableCustomRootFields where
  toJSON TableCustomRootFields {..} =
    object
      $ filter
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
      fail
        . T.unpack
        $ "the following custom root field names are duplicated: "
        <> englishList "and" (toTxt <$> duplicatedFields)

    pure tableCustomRootFields
    where
      englishList :: Text -> NonEmpty Text -> Text
      englishList joiner = \case
        one :| [] -> one
        one :| [two] -> one <> " " <> joiner <> " " <> two
        several ->
          let final :| initials = NE.reverse several
           in commaSeparated (reverse initials) <> ", " <> joiner <> " " <> final

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
  = FIColumn (StructuredColumnInfo b)
  | FIRelationship (RelInfo b)
  | FIComputedField (ComputedFieldInfo b)
  | FIRemoteRelationship (RemoteFieldInfo (DBJoinField b))
  deriving (Generic)

deriving instance (Backend b) => Eq (FieldInfo b)

instance (Backend b) => ToJSON (FieldInfo b) where
  toJSON =
    genericToJSON
      $ defaultOptions
        { constructorTagModifier = snakeCase . drop 2,
          sumEncoding = TaggedObject "type" "detail"
        }

$(makePrisms ''FieldInfo)

type FieldInfoMap = HashMap.HashMap FieldName

fieldInfoName :: forall b. (Backend b) => FieldInfo b -> FieldName
fieldInfoName = \case
  FIColumn info -> fromCol @b $ structuredColumnInfoColumn info
  FIRelationship info -> fromRel $ riName info
  FIComputedField info -> fromComputedField $ _cfiName info
  FIRemoteRelationship info -> fromRemoteRelationship $ getRemoteFieldInfoName info

fieldInfoGraphQLName :: FieldInfo b -> Maybe G.Name
fieldInfoGraphQLName = \case
  FIColumn info -> Just $ structuredColumnInfoName info
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
      ArrRel -> addAggregateFields [name]
  FIComputedField _ -> maybeToList $ fieldInfoGraphQLName info
  FIRemoteRelationship _ -> maybeToList $ fieldInfoGraphQLName info
  where
    addAggregateFields :: [G.Name] -> [G.Name]
    addAggregateFields names = do
      name <- names
      [name, name <> Name.__aggregate]

getCols :: FieldInfoMap (FieldInfo backend) -> [StructuredColumnInfo backend]
getCols = mapMaybe (^? _FIColumn) . HashMap.elems

-- | Sort columns based on their ordinal position
sortCols :: [ColumnInfo backend] -> [ColumnInfo backend]
sortCols = sortBy (\l r -> compare (ciPosition l) (ciPosition r))

getRels :: FieldInfoMap (FieldInfo backend) -> [RelInfo backend]
getRels = mapMaybe (^? _FIRelationship) . HashMap.elems

getComputedFieldInfos :: FieldInfoMap (FieldInfo backend) -> [ComputedFieldInfo backend]
getComputedFieldInfos = mapMaybe (^? _FIComputedField) . HashMap.elems

data InsPermInfo (b :: BackendType) = InsPermInfo
  { ipiCols :: HS.HashSet (Column b),
    ipiCheck :: AnnBoolExpPartialSQL b,
    ipiSet :: PreSetColsPartial b,
    ipiBackendOnly :: Bool,
    ipiRequiredHeaders :: HS.HashSet Text,
    ipiValidateInput :: Maybe (ValidateInput ResolvedWebhook)
  }
  deriving (Generic)

deriving instance
  ( Backend b,
    Eq (AnnBoolExpPartialSQL b)
  ) =>
  Eq (InsPermInfo b)

deriving instance
  ( Backend b,
    Show (AnnBoolExpPartialSQL b)
  ) =>
  Show (InsPermInfo b)

instance
  ( Backend b,
    NFData (AnnBoolExpPartialSQL b),
    NFData (PreSetColsPartial b)
  ) =>
  NFData (InsPermInfo b)

instance
  ( Backend b,
    ToJSON (AnnBoolExpPartialSQL b)
  ) =>
  ToJSON (InsPermInfo b)
  where
  toJSON = genericToJSON hasuraJSON

-- | This type is only used as an intermediate type
--   to combine more than one select permissions for inherited roles.
data CombinedSelPermInfo (b :: BackendType) = CombinedSelPermInfo
  { cspiCols :: [(HashMap.HashMap (Column b) (AnnRedactionExpPartialSQL b))],
    cspiComputedFields :: [(HashMap.HashMap ComputedFieldName (AnnRedactionExpPartialSQL b))],
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
  (Backend b) =>
  Int ->
  CombinedSelPermInfo b ->
  SelPermInfo b
combinedSelPermInfoToSelPermInfo selPermsCount CombinedSelPermInfo {..} =
  SelPermInfo
    (mergeColumnsWithBoolExp <$> HashMap.unionsAll cspiCols)
    (mergeColumnsWithBoolExp <$> HashMap.unionsAll cspiComputedFields)
    (BoolOr cspiFilter)
    (getMax <$> cspiLimit)
    (getAny cspiAllowAgg)
    cspiRequiredHeaders
    cspiAllowedQueryRootFieldTypes
    cspiAllowedSubscriptionRootFieldTypes
  where
    mergeColumnsWithBoolExp ::
      NonEmpty (AnnRedactionExp b (PartialSQLExp b)) ->
      AnnRedactionExp b (PartialSQLExp b)
    mergeColumnsWithBoolExp redactionExpressions
      -- when all the parent roles have a select permission, then we set
      -- the redaction expression to `NoRedaction`. Suppose this were not done, then
      -- the resulting boolean expression will be an expression which will always evaluate to
      -- `True`. So, to avoid additional computations, we just set the case boolean expression
      -- to `NoRedaction`.
      --
      -- Suppose, an inherited role, `inherited_role` inherits from two roles `role1` and `role2`.
      -- `role1` has the filter: `{"published": {"eq": true}}` and `role2` has the filter:
      -- `{"early_preview": {"eq": true}}` then the filter boolean expression of the inherited select permission will be
      -- the boolean OR of the parent roles filters.

      -- Now, let's say both `role1` and `role2` allow access to
      -- the `title` column of the table, the `RedactIfFalse` boolean expression of the `title` column will be
      -- the boolean OR of the parent roles filters i.e. same as the filter of the select permission. Since,
      -- the `RedactIfFalse` boolean expression is equal to the row filter boolean expression, the `RedactIfFalse`
      -- boolean expression will always evaluate to `True`; and since the `RedactIfFalse` boolean expression
      -- will always evaluate to `True`, we simply change the `RedactIfFalse` to a `NoRedaction` redaction expression
      -- when for a column all the select permissions exists.
      | selPermsCount == length redactionExpressions = NoRedaction
      | otherwise =
          let redactionBoolExps = mapMaybe (^? _RedactIfFalse) $ toList redactionExpressions
           in bool (RedactIfFalse $ BoolOr redactionBoolExps) NoRedaction $ null redactionBoolExps

data SelPermInfo (b :: BackendType) = SelPermInfo
  { -- | HashMap of accessible columns to the role, the `Column` may be mapped to
    -- an `AnnRedactionExpPartialSQL`, which is `RedactIfFalse` only in the case of an
    -- inherited role, for a non-inherited role, it will always be `NoRedaction`. The `RedactIfFalse`
    -- bool exp will determine if the column should be nullified in a row, when
    -- there aren't requisite permissions.
    spiCols :: HashMap.HashMap (Column b) (AnnRedactionExpPartialSQL b),
    -- | HashMap of accessible computed fields to the role, mapped to
    -- `AnnRedactionExpPartialSQL`, simililar to `spiCols`.
    -- These computed fields do not return rows of existing table.
    spiComputedFields :: HashMap.HashMap ComputedFieldName (AnnRedactionExpPartialSQL b),
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
    Eq (AnnBoolExpPartialSQL b)
  ) =>
  Eq (SelPermInfo b)

deriving instance
  ( Backend b,
    Show (AnnBoolExpPartialSQL b)
  ) =>
  Show (SelPermInfo b)

instance
  ( Backend b,
    NFData (AnnBoolExpPartialSQL b)
  ) =>
  NFData (SelPermInfo b)

instance
  ( Backend b,
    ToJSON (AnnBoolExpPartialSQL b)
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
    upiRequiredHeaders :: HashSet Text,
    upiValidateInput :: Maybe (ValidateInput ResolvedWebhook)
  }
  deriving (Generic)

deriving instance
  ( Backend b,
    Eq (AnnBoolExpPartialSQL b)
  ) =>
  Eq (UpdPermInfo b)

deriving instance
  ( Backend b,
    Show (AnnBoolExpPartialSQL b)
  ) =>
  Show (UpdPermInfo b)

instance
  ( Backend b,
    NFData (AnnBoolExpPartialSQL b),
    NFData (PreSetColsPartial b)
  ) =>
  NFData (UpdPermInfo b)

instance
  ( Backend b,
    ToJSON (AnnBoolExpPartialSQL b)
  ) =>
  ToJSON (UpdPermInfo b)
  where
  toJSON = genericToJSON hasuraJSON

data DelPermInfo (b :: BackendType) = DelPermInfo
  { dpiTable :: TableName b,
    dpiFilter :: AnnBoolExpPartialSQL b,
    dpiBackendOnly :: !Bool,
    dpiRequiredHeaders :: HashSet Text,
    dpiValidateInput :: Maybe (ValidateInput ResolvedWebhook)
  }
  deriving (Generic)

deriving instance
  ( Backend b,
    Eq (AnnBoolExpPartialSQL b)
  ) =>
  Eq (DelPermInfo b)

deriving instance
  ( Backend b,
    Show (AnnBoolExpPartialSQL b)
  ) =>
  Show (DelPermInfo b)

instance
  ( Backend b,
    NFData (AnnBoolExpPartialSQL b)
  ) =>
  NFData (DelPermInfo b)

instance
  ( Backend b,
    ToJSON (AnnBoolExpPartialSQL b)
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

instance Show (RolePermInfo b) where
  show _ = "Debugging Show instance for data RolePermInfo (b :: BackendType) = RolePermInfo"

instance
  ( Backend b,
    NFData (InsPermInfo b),
    NFData (SelPermInfo b),
    NFData (UpdPermInfo b),
    NFData (DelPermInfo b)
  ) =>
  NFData (RolePermInfo b)

instance
  ( Backend b,
    ToJSON (InsPermInfo b),
    ToJSON (SelPermInfo b),
    ToJSON (UpdPermInfo b),
    ToJSON (DelPermInfo b)
  ) =>
  ToJSON (RolePermInfo b)
  where
  toJSON = genericToJSON hasuraJSON

makeLenses ''RolePermInfo

type RolePermInfoMap b = HashMap.HashMap RoleName (RolePermInfo b)

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

instance HasCodec ColumnConfig where
  codec =
    AC.object "ColumnConfig"
      $ ColumnConfig
      <$> optionalFieldOrNullWith' "custom_name" graphQLFieldNameCodec
      AC..= _ccfgCustomName
        <*> optionalFieldWithOmittedDefault' "comment" Automatic
      AC..= _ccfgComment

instance ToJSON ColumnConfig where
  toJSON ColumnConfig {..} =
    object
      $ filter
        ((/= Null) . snd)
        [ "custom_name" .= _ccfgCustomName,
          "comment" .= _ccfgComment
        ]

instance FromJSON ColumnConfig where
  parseJSON = withObject "ColumnConfig" $ \obj ->
    ColumnConfig
      <$> obj
      .:? "custom_name"
      <*> obj
      .:? "comment"
      .!= Automatic

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

$(makeLenses ''TableConfig)

emptyTableConfig :: TableConfig b
emptyTableConfig =
  TableConfig emptyCustomRootFields HashMap.empty Nothing Automatic

instance (Backend b) => HasCodec (TableConfig b) where
  codec =
    AC.object (backendPrefix @b <> "TableConfig")
      $ TableConfig
      <$> optionalFieldWithDefault' "custom_root_fields" emptyCustomRootFields
      AC..= _tcCustomRootFields
        <*> columnConfigCodec
      AC..= _tcColumnConfig
        <*> optionalFieldOrNullWith' "custom_name" graphQLFieldNameCodec
      AC..= _tcCustomName
        <*> optionalFieldWithOmittedDefault' "comment" Automatic
      AC..= _tcComment
    where
      -- custom_column_names is a deprecated property that has been replaced by column_config.
      -- We merge custom_column_names into column_config transparently to maintain backwards
      -- compatibility (with both the metadata API and the metadata JSON saved in the HGE DB)
      -- custom_column_names can be removed once the deprecation period has expired and we get rid of it
      --
      -- This codec translates between a single column config value on the
      -- Haskell side, and a pair of object properties on the JSON side that are
      -- merged into the object codec above. When encoding the value for
      -- @custom_column_names@ is derived from @_tcColumnConfig@. When decoding
      -- values from @column_config@ and @custom_column_names@ are merged
      -- produce one value for @_tcColumnConfig@.
      columnConfigCodec =
        dimapCodec dec enc
          $ (,)
          <$> optionalFieldWithDefault' "column_config" HashMap.empty
          AC..= fst
            <*> optionalFieldWithDefaultWith' "custom_column_names" (hashMapCodec graphQLFieldNameCodec) HashMap.empty
          AC..= snd

      -- if @custom_column_names@ was given then merge its value during decoding
      -- to get a complete value for _tcColumnConfig
      dec (columnConfig, legacyCustomColumnNames) =
        let legacyColumnConfig = (\name -> ColumnConfig (Just name) Automatic) <$> legacyCustomColumnNames
         in HashMap.unionWith (<>) columnConfig legacyColumnConfig -- columnConfig takes precedence over legacy

      -- encode value from _tcColumnConfig for @column_config@, and for the
      -- legacy representation for @custom_column_names@.
      enc columnConfig =
        let outputColumnConfig = HashMap.filter (/= mempty) columnConfig
            legacyCustomColumnNames = mapMaybe _ccfgCustomName columnConfig
         in (outputColumnConfig, legacyCustomColumnNames)

instance (Backend b) => FromJSON (TableConfig b) where
  parseJSON = withObject "TableConfig" $ \obj -> do
    TableConfig
      <$> obj
      .:? "custom_root_fields"
      .!= emptyCustomRootFields
      <*> parseColumnConfig obj
      <*> obj
      .:? "custom_name"
      <*> obj
      .:? "comment"
      .!= Automatic
    where
      -- custom_column_names is a deprecated property that has been replaced by column_config.
      -- We merge custom_column_names into column_config transparently to maintain backwards
      -- compatibility (with both the metadata API and the metadata JSON saved in the HGE DB)
      -- custom_column_names can be removed once the deprecation period has expired and we get rid of it
      parseColumnConfig :: Object -> Parser (HashMap (Column b) ColumnConfig)
      parseColumnConfig obj = do
        columnConfig <- obj .:? "column_config" .!= HashMap.empty
        legacyCustomColumnNames <- obj .:? "custom_column_names" .!= HashMap.empty
        let legacyColumnConfig = (\name -> ColumnConfig (Just name) Automatic) <$> legacyCustomColumnNames
        pure $ HashMap.unionWith (<>) columnConfig legacyColumnConfig -- columnConfig takes precedence over legacy

instance (Backend b) => ToJSON (TableConfig b) where
  toJSON TableConfig {..} =
    object
      $ filter
        ((/= Null) . snd)
        [ "custom_root_fields" .= _tcCustomRootFields,
          -- custom_column_names is a deprecated property that has been replaced by column_config.
          -- We are retaining it here, sourcing its values from column_config, for backwards-compatibility
          -- custom_column_names can be removed once the deprecation period has expired and we get rid of it
          "custom_column_names" .= mapMaybe _ccfgCustomName _tcColumnConfig,
          "column_config" .= HashMap.filter (/= mempty) _tcColumnConfig,
          "custom_name" .= _tcCustomName,
          "comment" .= _tcComment
        ]

data Constraint (b :: BackendType) = Constraint
  { _cName :: ConstraintName b,
    _cOid :: OID
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (Constraint b)

deriving instance (Backend b) => Show (Constraint b)

instance (Backend b) => NFData (Constraint b)

instance (Backend b) => Hashable (Constraint b)

instance (Backend b) => ToJSON (Constraint b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (Constraint b) where
  parseJSON = genericParseJSON hasuraJSON

data PrimaryKey (b :: BackendType) a = PrimaryKey
  { _pkConstraint :: Constraint b,
    _pkColumns :: NESeq a
  }
  deriving (Generic, Foldable)

deriving instance (Backend b, Eq a) => Eq (PrimaryKey b a)

deriving instance (Backend b, Show a) => Show (PrimaryKey b a)

instance (Backend b, NFData a) => NFData (PrimaryKey b a)

instance (Eq a, Backend b, Hashable (NESeq a)) => Hashable (PrimaryKey b a)

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

deriving instance (Backend b) => Eq (UniqueConstraint b)

deriving instance (Backend b) => Show (UniqueConstraint b)

instance (Backend b) => NFData (UniqueConstraint b)

instance (Backend b) => Hashable (UniqueConstraint b)

instance (Backend b) => ToJSON (UniqueConstraint b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (UniqueConstraint b) where
  parseJSON = genericParseJSON hasuraJSON

data ForeignKey (b :: BackendType) = ForeignKey
  { _fkConstraint :: Constraint b,
    _fkForeignTable :: TableName b,
    _fkColumnMapping :: NEHashMap (ColumnPath b) (ColumnPath b)
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (ForeignKey b)

deriving instance (Backend b) => Show (ForeignKey b)

instance (Backend b) => NFData (ForeignKey b)

instance (Backend b) => Hashable (ForeignKey b)

instance (Backend b) => ToJSON (ForeignKey b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (ForeignKey b) where
  parseJSON = genericParseJSON hasuraJSON

-- | The @field@ and @primaryKeyColumn@ type parameters vary as the schema cache is built and more
-- information is accumulated. See also 'TableCoreInfo'.
data TableCoreInfoG (b :: BackendType) field primaryKeyColumn = TableCoreInfo
  { _tciName :: TableName b,
    _tciDescription :: Maybe Postgres.PGDescription, -- TODO make into type family?
    _tciFieldInfoMap :: FieldInfoMap field,
    _tciPrimaryKey :: Maybe (PrimaryKey b primaryKeyColumn),
    -- | Does /not/ include the primary key; use 'tciUniqueOrPrimaryKeyConstraints' if you need both.
    _tciUniqueConstraints :: HashSet (UniqueConstraint b),
    _tciForeignKeys :: HashSet (ForeignKey b),
    _tciViewInfo :: Maybe ViewInfo,
    _tciEnumValues :: Maybe EnumValues,
    _tciCustomConfig :: TableConfig b,
    _tciExtraTableMetadata :: ExtraTableMetadata b,
    _tciApolloFederationConfig :: Maybe ApolloFederationConfig,
    _tciRawColumns :: [RawColumnInfo b]
  }
  deriving (Generic)

deriving instance (Eq field, Eq pkCol, Backend b) => Eq (TableCoreInfoG b field pkCol)

instance (Backend b, Generic pkCol, ToJSON field, ToJSON pkCol) => ToJSON (TableCoreInfoG b field pkCol) where
  toJSON = genericToJSON hasuraJSON

$(makeLenses ''TableCoreInfoG)

-- | Fully-processed table info that includes non-column fields.
type TableCoreInfo b = TableCoreInfoG b (FieldInfo b) (ColumnInfo b)

tciUniqueOrPrimaryKeyConstraints ::
  forall b f.
  (Hashable (Column b)) =>
  TableCoreInfoG b f (ColumnInfo b) ->
  Maybe (NonEmpty (UniqueConstraint b))
tciUniqueOrPrimaryKeyConstraints info =
  NE.nonEmpty
    $ maybeToList (primaryToUnique <$> _tciPrimaryKey info)
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
    ToJSON (EventTriggerInfoMap b),
    ToJSON (RolePermInfo b),
    ToJSON (RolePermInfoMap b),
    ToJSON (TableCoreInfo b)
  ) =>
  ToJSON (TableInfo b)
  where
  toJSON = genericToJSON hasuraJSON

$(makeLenses ''TableInfo)

tiName :: Lens' (TableInfo b) (TableName b)
tiName = tiCoreInfo . tciName

tableInfoName :: TableInfo b -> TableName b
tableInfoName = view tiName

tableArrayRelationships :: TableInfo b -> [RelInfo b]
tableArrayRelationships ti = [rel | rel <- getRels . _tciFieldInfoMap . _tiCoreInfo $ ti, riType rel == ArrRel]

getRolePermInfo :: RoleName -> TableInfo b -> RolePermInfo b
getRolePermInfo role tableInfo
  | role == adminRoleName = _tiAdminRolePermInfo tableInfo
  | otherwise =
      fromMaybe
        (RolePermInfo Nothing Nothing Nothing Nothing)
        (HashMap.lookup role $ _tiRolePermInfoMap tableInfo)

type TableCoreCache b = HashMap.HashMap (TableName b) (TableCoreInfo b)

type TableCache b = HashMap.HashMap (TableName b) (TableInfo b) -- info of all tables

-- map of all event triggers on the table
type TableEventTriggers b = HashMap.HashMap (TableName b) [TriggerName]

-- | Metadata of a Postgres foreign key constraint which is being
-- extracted from database via 'src-rsr/pg_table_metadata.sql'
newtype ForeignKeyMetadata (b :: BackendType) = ForeignKeyMetadata
  { unForeignKeyMetadata :: ForeignKey b
  }
  deriving (Show, Eq, NFData, Hashable)

instance (Backend b) => FromJSON (ForeignKeyMetadata b) where
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

    pure
      $ ForeignKeyMetadata
        ForeignKey
          { _fkConstraint = constraint,
            _fkForeignTable = foreignTable,
            _fkColumnMapping =
              NEHashMap.fromNonEmpty
                $ NE.zip columns foreignColumns
          }

instance (Backend b) => ToJSON (ForeignKeyMetadata b) where
  toJSON (ForeignKeyMetadata (ForeignKey constraint foreignTable columnMapping)) =
    let (columns, foreignColumns) = NE.unzip $ NEHashMap.toList columnMapping
     in object
          [ "constraint" .= constraint,
            "foreign_table" .= foreignTable,
            "columns" .= columns,
            "foreign_columns" .= foreignColumns
          ]

-- | Metadata of any Backend table which is being extracted from source database
data DBTableMetadata (b :: BackendType) = DBTableMetadata
  { _ptmiOid :: OID,
    _ptmiColumns :: [RawColumnInfo b],
    _ptmiPrimaryKey :: Maybe (PrimaryKey b (Column b)),
    -- | Does /not/ include the primary key
    _ptmiUniqueConstraints :: HashSet (UniqueConstraint b),
    _ptmiForeignKeys :: HashSet (ForeignKeyMetadata b),
    _ptmiViewInfo :: Maybe ViewInfo,
    _ptmiDescription :: Maybe Postgres.PGDescription,
    _ptmiExtraTableMetadata :: ExtraTableMetadata b
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (DBTableMetadata b)

deriving instance (Backend b) => Show (DBTableMetadata b)

instance (Backend b) => NFData (DBTableMetadata b)

instance (Backend b) => FromJSON (DBTableMetadata b) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => ToJSON (DBTableMetadata b) where
  toJSON = genericToJSON hasuraJSON

type DBTablesMetadata b = HashMap (TableName b) (DBTableMetadata b)

getFieldInfoM ::
  TableInfo b -> FieldName -> Maybe (FieldInfo b)
getFieldInfoM tableInfo fieldName =
  tableInfo ^. tiCoreInfo . tciFieldInfoMap . at fieldName

getColumnInfoM ::
  TableInfo b -> FieldName -> Maybe (ColumnInfo b)
getColumnInfoM tableInfo fieldName =
  (^? _FIColumn . _SCIScalarColumn) =<< getFieldInfoM tableInfo fieldName

askFieldInfo ::
  (MonadError QErr m) =>
  FieldInfoMap fieldInfo ->
  FieldName ->
  m fieldInfo
askFieldInfo m f =
  onNothing (HashMap.lookup f m) $ throw400 NotExists (f <<> " does not exist")

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
    modifyErr ("column " <>)
      $ askFieldInfo m (fromCol @backend c)
  case fieldInfo of
    (FIColumn (SCIScalarColumn colInfo)) -> pure colInfo
    (FIColumn (SCIObjectColumn _)) -> throwErr "object"
    (FIColumn (SCIArrayColumn _)) -> throwErr "array"
    (FIRelationship _) -> throwErr "relationship"
    (FIComputedField _) -> throwErr "computed field"
    (FIRemoteRelationship _) -> throwErr "remote relationship"
  where
    throwErr fieldType =
      throwError
        $ err400 UnexpectedPayload
        $ "expecting a database column; but, "
        <> c
        <<> " is a "
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
    modifyErr ("computed field " <>)
      $ askFieldInfo fields
      $ fromComputedField computedField
  case fieldInfo of
    (FIColumn _) -> throwErr "column"
    (FIRelationship _) -> throwErr "relationship"
    (FIRemoteRelationship _) -> throwErr "remote relationship"
    (FIComputedField cci) -> pure cci
  where
    throwErr fieldType =
      throwError
        $ err400 UnexpectedPayload
        $ "expecting a computed field; but, "
        <> computedField
        <<> " is a "
        <> fieldType

assertColumnExists ::
  forall backend m.
  (MonadError QErr m, Backend backend) =>
  FieldInfoMap (FieldInfo backend) ->
  Text ->
  Column backend ->
  m ()
assertColumnExists m msg c = do
  fieldInfo <-
    modifyErr ("column " <>)
      $ askFieldInfo m (fromCol @backend c)
  case fieldInfo of
    (FIColumn _) -> pure ()
    (FIRelationship _) -> throwErr "relationship"
    (FIComputedField _) -> throwErr "computed field"
    (FIRemoteRelationship _) -> throwErr "remote relationship"
  where
    throwErr fieldType =
      throwError
        $ err400 UnexpectedPayload
        $ "expecting a database column; but, "
        <> c
        <<> " is a "
        <> fieldType
        <> "; "
        <> msg

askRelType ::
  (MonadError QErr m) =>
  FieldInfoMap (FieldInfo backend) ->
  RelName ->
  Text ->
  m (RelInfo backend)
askRelType m r msg = do
  colInfo <-
    modifyErr ("relationship " <>)
      $ askFieldInfo m (fromRel r)
  case colInfo of
    (FIRelationship relInfo) -> return relInfo
    _ ->
      throwError
        $ err400 UnexpectedPayload
        $ "expecting a relationship; but, "
        <> r
        <<> " is a postgres column; "
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

mkAdminRolePermInfo :: (Backend b) => TableCoreInfo b -> RolePermInfo b
mkAdminRolePermInfo tableInfo =
  RolePermInfo (Just i) (Just s) (Just u) (Just d)
  where
    fields = _tciFieldInfoMap tableInfo
    pgCols = map structuredColumnInfoColumn $ getCols fields
    pgColsWithFilter = HashMap.fromList $ map (,NoRedaction) pgCols
    computedFields =
      -- Fetch the list of computed fields not returning rows of existing table.
      -- For other computed fields returning existing table rows, the admin role can query them
      -- as their permissions are derived from returning table permissions.
      HS.fromList $ map _cfiName $ removeComputedFieldsReturningExistingTable $ getComputedFieldInfos fields
    computedFields' = HS.toMap computedFields $> NoRedaction

    tableName = _tciName tableInfo
    i = InsPermInfo (HS.fromList pgCols) annBoolExpTrue HashMap.empty False mempty Nothing
    s = SelPermInfo pgColsWithFilter computedFields' annBoolExpTrue Nothing True mempty ARFAllowAllRootFields ARFAllowAllRootFields
    u = UpdPermInfo (HS.fromList pgCols) tableName annBoolExpTrue Nothing HashMap.empty False mempty Nothing
    d = DelPermInfo tableName annBoolExpTrue False mempty Nothing

-- | Builds field name with proper case. Please note that this is a pure
--   function as all the validation has already been done while preparing
--   @GQLNameIdentifier@.
setFieldNameCase ::
  NamingCase ->
  TableInfo b ->
  CustomRootField ->
  (C.GQLNameIdentifier -> C.GQLNameIdentifier) ->
  C.GQLNameIdentifier ->
  G.Name
setFieldNameCase tCase tInfo crf getFieldName tableName =
  (applyFieldNameCaseIdentifier tCase fieldIdentifier)
  where
    tccName = fmap C.fromCustomName . _tcCustomName . _tciCustomConfig . _tiCoreInfo $ tInfo
    crfName = fmap C.fromCustomName (_crfName crf)
    fieldIdentifier = fromMaybe (getFieldName (fromMaybe tableName tccName)) crfName
