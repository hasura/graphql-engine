{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Column
  ( ColumnType (..),
    _ColumnScalar,
    _ColumnEnumReference,
    isEnumColumn,
    isScalarColumnWhere,
    ValueParser,
    onlyNumCols,
    isNumCol,
    onlyComparableCols,
    parseScalarValueColumnType,
    parseScalarValuesColumnType,
    ColumnValue (..),
    ColumnMutability (..),
    ColumnInfo (..),
    NestedObjectInfo (..),
    RawColumnType (..),
    RawColumnInfo (..),
    PrimaryKeyColumns,
    getColInfos,
    EnumReference (..),
    EnumValues,
    EnumValue (..),
    EnumValueInfo (..),
    fromCol,
    ColumnValues,
    ColumnReference (..),
    columnReferenceType,
    NestedArrayInfo (..),
    StructuredColumnInfo (..),
    _SCIScalarColumn,
    _SCIObjectColumn,
    _SCIArrayColumn,
    structuredColumnInfoName,
    structuredColumnInfoColumn,
    structuredColumnInfoMutability,
    toScalarColumnInfo,
  )
where

import Autodocodec
import Control.Lens.TH
import Data.Aeson hiding ((.=))
import Data.Aeson.TH
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.LogicalModel.Types (LogicalModelName)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField.Name (ComputedFieldName)
import Hasura.SQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

newtype EnumValue = EnumValue {getEnumValue :: G.Name}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NFData, Hashable, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

newtype EnumValueInfo = EnumValueInfo
  { evComment :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NFData, Hashable)

$(deriveJSON hasuraJSON ''EnumValueInfo)

type EnumValues = HashMap.HashMap EnumValue EnumValueInfo

-- | Represents a reference to an “enum table,” a single-column Postgres table that is referenced
-- via foreign key.
data EnumReference (b :: BackendType) = EnumReference
  { erTable :: TableName b,
    erValues :: EnumValues,
    erTableCustomName :: Maybe G.Name
  }
  deriving (Generic)

deriving instance (Backend b) => Show (EnumReference b)

deriving instance (Backend b) => Eq (EnumReference b)

deriving instance (Backend b) => Ord (EnumReference b)

instance (Backend b) => NFData (EnumReference b)

instance (Backend b) => Hashable (EnumReference b)

instance (Backend b) => FromJSON (EnumReference b) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => ToJSON (EnumReference b) where
  toJSON = genericToJSON hasuraJSON

-- | The type we use for columns, which are currently always “scalars” (though
-- see the note about 'CollectableType'). Unlike 'ScalarType', which represents
-- a type that a backend knows about, this type characterizes distinctions we
-- make but the backend doesn’t.
data ColumnType (b :: BackendType)
  = -- | Ordinary Postgres columns.
    ColumnScalar (ScalarType b)
  | -- | Columns that reference enum tables (see "Hasura.RQL.Schema.Enum"). This is not actually a
    -- distinct type from the perspective of Postgres (at the time of this writing, we ensure they
    -- always have type @text@), but we really want to distinguish this case, since we treat it
    -- /completely/ differently in the GraphQL schema.
    ColumnEnumReference (EnumReference b)
  deriving (Show, Eq, Ord, Generic)

instance (Backend b) => NFData (ColumnType b)

instance (Backend b) => Hashable (ColumnType b)

instance (Backend b) => ToJSON (ColumnType b) where
  toJSON = genericToJSON $ defaultOptions {constructorTagModifier = drop 6}

$(makePrisms ''ColumnType)

instance (Backend b) => ToTxt (ColumnType b) where
  toTxt = \case
    ColumnScalar scalar -> toTxt scalar
    ColumnEnumReference (EnumReference tableName _ tableCustomName) ->
      let tableTxtName = toTxt tableName
       in maybe tableTxtName toTxt tableCustomName

-- | A parser to parse a json value with enforcing column type
type ValueParser b m v =
  CollectableType (ColumnType b) -> Value -> m v

data ColumnValue (b :: BackendType) = ColumnValue
  { cvType :: ColumnType b,
    cvValue :: ScalarValue b
  }

deriving instance (Backend b) => Eq (ColumnValue b)

deriving instance (Backend b) => Show (ColumnValue b)

isScalarColumnWhere :: (ScalarType b -> Bool) -> ColumnType b -> Bool
isScalarColumnWhere f = \case
  ColumnScalar scalar -> f scalar
  ColumnEnumReference _ -> False

isEnumColumn :: ColumnType b -> Bool
isEnumColumn (ColumnEnumReference _) = True
isEnumColumn _ = False

-- | Note: Unconditionally accepts null values and returns 'PGNull'.
parseScalarValueColumnType ::
  forall m b.
  (MonadError QErr m, Backend b) =>
  ColumnType b ->
  Value ->
  m (ScalarValue b)
parseScalarValueColumnType columnType value = case columnType of
  ColumnScalar scalarType -> liftEither $ parseScalarValue @b scalarType value
  ColumnEnumReference (EnumReference tableName enumValues _) ->
    parseEnumValue =<< decodeValue value
    where
      parseEnumValue :: Maybe G.Name -> m (ScalarValue b)
      parseEnumValue enumValueName = do
        for_ enumValueName \evn -> do
          let enums = map getEnumValue $ HashMap.keys enumValues
          unless (evn `elem` enums)
            $ throw400 UnexpectedPayload
            $ "expected one of the values "
            <> dquoteList (sort enums)
            <> " for type "
            <> snakeCaseTableName @b tableName
            <<> ", given "
            <>> evn
        pure $ textToScalarValue @b $ G.unName <$> enumValueName

parseScalarValuesColumnType ::
  (MonadError QErr m, Backend b) =>
  ColumnType b ->
  [Value] ->
  m [ScalarValue b]
parseScalarValuesColumnType columnType =
  indexedMapM (parseScalarValueColumnType columnType)

data RawColumnType (b :: BackendType)
  = RawColumnTypeScalar (ScalarType b)
  | RawColumnTypeObject (XNestedObjects b) G.Name
  | RawColumnTypeArray (XNestedObjects b) (RawColumnType b) Bool
  deriving stock (Generic)

deriving instance (Backend b) => Eq (RawColumnType b)

deriving instance (Backend b) => Ord (RawColumnType b)

deriving anyclass instance (Backend b) => Hashable (RawColumnType b)

deriving instance (Backend b) => Show (RawColumnType b)

instance (Backend b) => NFData (RawColumnType b)

-- For backwards compatibility we want to serialize and deserialize
-- RawColumnTypeScalar as a ScalarType
instance (Backend b) => ToJSON (RawColumnType b) where
  toJSON = \case
    RawColumnTypeScalar scalar -> toJSON scalar
    other -> genericToJSON hasuraJSON other

instance (Backend b) => FromJSON (RawColumnType b) where
  parseJSON v = (RawColumnTypeScalar <$> parseJSON v) <|> genericParseJSON hasuraJSON v

-- Ideally we'd derive ToJSON and FromJSON instances from the HasCodec instance, rather than the other way around.
-- Unfortunately, I'm not sure if it's possible to write a proper HasCodec instance in the presence
-- of the (XNestedObjects b) type family, which may be Void.
instance (Backend b) => HasCodec (RawColumnType b) where
  codec = codecViaAeson "RawColumnType"

-- | “Raw” column info, as stored in the catalog (but not in the schema cache). Instead of
-- containing a 'PGColumnType', it only contains a 'PGScalarType', which is combined with the
-- 'pcirReferences' field and other table data to eventually resolve the type to a 'PGColumnType'.
data RawColumnInfo (b :: BackendType) = RawColumnInfo
  { rciName :: Column b,
    -- | The “ordinal position” of the column according to Postgres. Numbering starts at 1 and
    -- increases. Dropping a column does /not/ cause the columns to be renumbered, so a column can be
    -- consistently identified by its position.
    rciPosition :: Int,
    rciType :: RawColumnType b,
    rciIsNullable :: Bool,
    rciDescription :: Maybe G.Description,
    rciMutability :: ColumnMutability
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (RawColumnInfo b)

deriving instance (Backend b) => Show (RawColumnInfo b)

instance (Backend b) => NFData (RawColumnInfo b)

instance (Backend b) => ToJSON (RawColumnInfo b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (RawColumnInfo b) where
  parseJSON = genericParseJSON hasuraJSON

-- | Indicates whether a column may participate in certain mutations.
--
-- For example, identity columns may sometimes be insertable but rarely
-- updatable, depending on the backend and how they're declared.
--
-- This guides the schema parsers such that they only generate fields for
-- columns where they're valid without having to model the exact circumstances
-- which cause a column to appear or not.
--
-- See <https://github.com/hasura/graphql-engine/blob/master/rfcs/column-mutability.md>.
data ColumnMutability = ColumnMutability
  { _cmIsInsertable :: Bool,
    _cmIsUpdatable :: Bool
  }
  deriving (Eq, Ord, Generic, Show)

instance NFData ColumnMutability

instance Hashable ColumnMutability

instance FromJSON ColumnMutability where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON ColumnMutability where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

-- | “Resolved” column info, produced from a 'RawColumnInfo' value that has been combined with
-- other schema information to produce a 'PGColumnType'.
data ColumnInfo (b :: BackendType) = ColumnInfo
  { ciColumn :: Column b,
    -- | field name exposed in GraphQL interface
    ciName :: G.Name,
    ciPosition :: Int,
    ciType :: ColumnType b,
    ciIsNullable :: Bool,
    ciDescription :: Maybe G.Description,
    ciMutability :: ColumnMutability
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (ColumnInfo b)

deriving instance (Backend b) => Ord (ColumnInfo b)

deriving instance (Backend b) => Show (ColumnInfo b)

instance (Backend b) => NFData (ColumnInfo b)

instance (Backend b) => Hashable (ColumnInfo b)

instance (Backend b) => ToJSON (ColumnInfo b) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data NestedObjectInfo b = NestedObjectInfo
  { _noiSupportsNestedObjects :: XNestedObjects b,
    _noiColumn :: Column b,
    _noiName :: G.Name,
    _noiType :: LogicalModelName,
    _noiIsNullable :: Bool,
    _noiDescription :: Maybe G.Description,
    _noiMutability :: ColumnMutability
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (NestedObjectInfo b)

deriving instance (Backend b) => Ord (NestedObjectInfo b)

deriving instance (Backend b) => Show (NestedObjectInfo b)

instance (Backend b) => NFData (NestedObjectInfo b)

instance (Backend b) => Hashable (NestedObjectInfo b)

instance (Backend b) => ToJSON (NestedObjectInfo b) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data NestedArrayInfo b = NestedArrayInfo
  { _naiSupportsNestedArrays :: XNestedObjects b,
    _naiIsNullable :: Bool,
    _naiColumnInfo :: StructuredColumnInfo b
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (NestedArrayInfo b)

deriving instance (Backend b) => Ord (NestedArrayInfo b)

deriving instance (Backend b) => Show (NestedArrayInfo b)

instance (Backend b) => NFData (NestedArrayInfo b)

instance (Backend b) => Hashable (NestedArrayInfo b)

instance (Backend b) => ToJSON (NestedArrayInfo b) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

data StructuredColumnInfo b
  = SCIScalarColumn (ColumnInfo b)
  | SCIObjectColumn (NestedObjectInfo b)
  | SCIArrayColumn (NestedArrayInfo b)
  deriving (Generic)

deriving instance (Backend b) => Eq (StructuredColumnInfo b)

deriving instance (Backend b) => Ord (StructuredColumnInfo b)

deriving instance (Backend b) => Show (StructuredColumnInfo b)

instance (Backend b) => NFData (StructuredColumnInfo b)

instance (Backend b) => Hashable (StructuredColumnInfo b)

instance (Backend b) => ToJSON (StructuredColumnInfo b) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

structuredColumnInfoName :: StructuredColumnInfo b -> G.Name
structuredColumnInfoName = \case
  SCIScalarColumn ColumnInfo {..} -> ciName
  SCIObjectColumn NestedObjectInfo {..} -> _noiName
  SCIArrayColumn NestedArrayInfo {..} -> structuredColumnInfoName _naiColumnInfo

structuredColumnInfoColumn :: StructuredColumnInfo b -> Column b
structuredColumnInfoColumn = \case
  SCIScalarColumn ColumnInfo {..} -> ciColumn
  SCIObjectColumn NestedObjectInfo {..} -> _noiColumn
  SCIArrayColumn NestedArrayInfo {..} -> structuredColumnInfoColumn _naiColumnInfo

structuredColumnInfoMutability :: StructuredColumnInfo b -> ColumnMutability
structuredColumnInfoMutability = \case
  SCIScalarColumn ColumnInfo {..} -> ciMutability
  SCIObjectColumn NestedObjectInfo {..} -> _noiMutability
  SCIArrayColumn NestedArrayInfo {..} -> structuredColumnInfoMutability _naiColumnInfo

toScalarColumnInfo :: StructuredColumnInfo b -> Maybe (ColumnInfo b)
toScalarColumnInfo = \case
  SCIScalarColumn ci -> Just ci
  _ -> Nothing

$(makePrisms ''StructuredColumnInfo)

type PrimaryKeyColumns b = NESeq (ColumnInfo b)

onlyNumCols :: forall b. (Backend b) => [ColumnInfo b] -> [ColumnInfo b]
onlyNumCols = filter isNumCol

isNumCol :: forall b. (Backend b) => ColumnInfo b -> Bool
isNumCol = isScalarColumnWhere (isNumType @b) . ciType

onlyComparableCols :: forall b. (Backend b) => [ColumnInfo b] -> [ColumnInfo b]
onlyComparableCols = filter (isScalarColumnWhere (isComparableType @b) . ciType)

getColInfos :: (Backend b) => [Column b] -> [ColumnInfo b] -> [ColumnInfo b]
getColInfos cols allColInfos =
  flip filter allColInfos $ \ci -> ciColumn ci `elem` cols

fromCol :: (Backend b) => Column b -> FieldName
fromCol = FieldName . toTxt

type ColumnValues b a = HashMap (Column b) a

-- | Represents a reference to a source column, possibly casted an arbitrary
-- number of times. Used within 'parseBoolExpOperations' for bookkeeping.
data ColumnReference (b :: BackendType)
  = ColumnReferenceColumn (ColumnInfo b)
  | ColumnReferenceComputedField ComputedFieldName (ScalarType b)
  | ColumnReferenceCast (ColumnReference b) (ColumnType b)

columnReferenceType :: ColumnReference backend -> ColumnType backend
columnReferenceType = \case
  ColumnReferenceColumn column -> ciType column
  ColumnReferenceComputedField _ scalarType -> ColumnScalar scalarType
  ColumnReferenceCast _ targetType -> targetType

instance (Backend b) => ToTxt (ColumnReference b) where
  toTxt = \case
    ColumnReferenceColumn column -> toTxt $ ciColumn column
    ColumnReferenceComputedField name _ -> toTxt name
    ColumnReferenceCast reference targetType ->
      toTxt reference <> "::" <> toTxt targetType
