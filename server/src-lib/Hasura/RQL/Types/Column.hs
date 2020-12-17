module Hasura.RQL.Types.Column
  ( ColumnType(..)
  , _ColumnScalar
  , _ColumnEnumReference
  , isScalarColumnWhere

  , onlyNumCols
  , onlyJSONBCols
  , onlyComparableCols

  , parsePGScalarValue
  , parsePGScalarValues
  , unsafePGColumnToBackend
  , parseTxtEncodedPGValue

  , ColumnValue(..)

  , ColumnInfo(..)
  , RawColumnInfo(..)
  , PrimaryKeyColumns
  , getColInfos

  , EnumReference(..)
  , EnumValues
  , EnumValue(..)
  , EnumValueInfo(..)
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                as M
import qualified Language.GraphQL.Draft.Syntax      as G

import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended

import           Hasura.Backends.Postgres.SQL.Types hiding (TableName, isNumType, isComparableType)
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.Instances               ()
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend


newtype EnumValue
  = EnumValue { getEnumValue :: G.Name }
  deriving (Show, Eq, Ord, NFData, Hashable, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Cacheable)

newtype EnumValueInfo
  = EnumValueInfo
  { evComment :: Maybe Text
  } deriving (Show, Eq, Ord, NFData, Hashable, Cacheable)
$(deriveJSON (aesonDrop 2 snakeCase) ''EnumValueInfo)

type EnumValues = M.HashMap EnumValue EnumValueInfo

-- | Represents a reference to an “enum table,” a single-column Postgres table that is referenced
-- via foreign key.
data EnumReference (b :: BackendType)
  = EnumReference
  { erTable  :: !(TableName b)
  , erValues :: !EnumValues
  } deriving (Generic)
deriving instance (Backend b) => Show (EnumReference b)
deriving instance (Backend b) => Eq (EnumReference b)
deriving instance (Backend b) => Ord (EnumReference b)
instance (Backend b) => NFData (EnumReference b)
instance (Backend b) => Hashable (EnumReference b)
instance (Backend b) => Cacheable (EnumReference b)

instance Backend b => FromJSON (EnumReference b) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance Backend b => ToJSON (EnumReference b) where
  toJSON = genericToJSON $ aesonPrefix snakeCase

-- | The type we use for columns, which are currently always “scalars” (though
-- see the note about 'CollectableType'). Unlike 'ScalarType', which represents
-- a type that a backend knows about, this type characterizes distinctions we
-- make but the backend doesn’t.
data ColumnType (b :: BackendType)
  -- | Ordinary Postgres columns.
  = ColumnScalar !(ScalarType b)
  -- | Columns that reference enum tables (see "Hasura.RQL.Schema.Enum"). This is not actually a
  -- distinct type from the perspective of Postgres (at the time of this writing, we ensure they
  -- always have type @text@), but we really want to distinguish this case, since we treat it
  -- /completely/ differently in the GraphQL schema.
  | ColumnEnumReference !(EnumReference b)
  deriving (Show, Eq, Ord, Generic)
instance (Backend b) => NFData (ColumnType b)
instance (Backend b) => Hashable (ColumnType b)
instance (Backend b) => Cacheable (ColumnType b)

instance Backend b => ToJSON (ColumnType b) where
  toJSON = genericToJSON $ defaultOptions{constructorTagModifier = drop 6}

$(makePrisms ''ColumnType)

instance Backend b => ToTxt (ColumnType b) where
  toTxt = \case
    ColumnScalar scalar                             -> toTxt scalar
    ColumnEnumReference (EnumReference tableName _) -> toTxt tableName

data ColumnValue (b :: BackendType) = ColumnValue
  { cvType  :: ColumnType b
  , cvValue :: ScalarValue b
  }

isScalarColumnWhere :: (ScalarType b -> Bool) -> ColumnType b -> Bool
isScalarColumnWhere f = \case
  ColumnScalar scalar   -> f scalar
  ColumnEnumReference _ -> False

-- | Gets the representation type associated with a 'ColumnType'. Avoid using this if possible.
-- Prefer 'parsePGScalarValue', 'parsePGScalarValues', or
-- 'Hasura.RQL.Types.BoolExp.mkTypedSessionVar'.
unsafePGColumnToBackend :: ColumnType 'Postgres -> ScalarType 'Postgres
unsafePGColumnToBackend = \case
  ColumnScalar scalarType -> scalarType
  ColumnEnumReference _   -> PGText

-- | Note: Unconditionally accepts null values and returns 'PGNull'.
parsePGScalarValue
  :: forall m. (MonadError QErr m) => ColumnType 'Postgres -> Value -> m PGScalarValue
parsePGScalarValue columnType value = case columnType of
  ColumnScalar scalarType -> runAesonParser (parsePGValue scalarType) value
  ColumnEnumReference (EnumReference tableName enumValues) ->
    maybe (pure $ PGNull PGText) parseEnumValue =<< decodeValue value
    where
      parseEnumValue :: G.Name -> m PGScalarValue
      parseEnumValue enumValueName = do
        let enums = map getEnumValue $ M.keys enumValues
        unless (enumValueName `elem` enums) $ throw400 UnexpectedPayload
          $ "expected one of the values " <> dquoteList enums
          <> " for type " <> snakeCaseQualifiedObject tableName <<> ", given " <>> enumValueName
        pure $ PGValText $ G.unName enumValueName

parsePGScalarValues
  :: (MonadError QErr m)
  => ColumnType 'Postgres -> [Value] -> m [PGScalarValue]
parsePGScalarValues columnType values =
  indexedMapM (parsePGScalarValue columnType) values

parseTxtEncodedPGValue
  :: (MonadError QErr m)
  => ColumnType 'Postgres -> TxtEncodedPGVal -> m PGScalarValue
parseTxtEncodedPGValue colTy val =
  parsePGScalarValue colTy $ case val of
    TENull  -> Null
    TELit t -> String t

-- | “Raw” column info, as stored in the catalog (but not in the schema cache). Instead of
-- containing a 'PGColumnType', it only contains a 'PGScalarType', which is combined with the
-- 'pcirReferences' field and other table data to eventually resolve the type to a 'PGColumnType'.
data RawColumnInfo (b :: BackendType)
  = RawColumnInfo
  { prciName        :: !(Column b)
  , prciPosition    :: !Int
  -- ^ The “ordinal position” of the column according to Postgres. Numbering starts at 1 and
  -- increases. Dropping a column does /not/ cause the columns to be renumbered, so a column can be
  -- consistently identified by its position.
  , prciType        :: !(ScalarType b)
  , prciIsNullable  :: !Bool
  , prciDescription :: !(Maybe G.Description)
  } deriving (Generic)
deriving instance Backend b => Eq (RawColumnInfo b)
deriving instance Backend b => Show (RawColumnInfo b)
instance Backend b => NFData (RawColumnInfo b)
instance Backend b => Cacheable (RawColumnInfo b)
instance Backend b => ToJSON (RawColumnInfo b) where
  toJSON = genericToJSON $ aesonDrop 4 snakeCase
instance Backend b => FromJSON (RawColumnInfo b) where
  parseJSON = genericParseJSON $ aesonDrop 4 snakeCase

-- | “Resolved” column info, produced from a 'RawColumnInfo' value that has been combined with
-- other schema information to produce a 'PGColumnType'.
data ColumnInfo (b :: BackendType)
  = ColumnInfo
  { pgiColumn      :: !(Column b)
  , pgiName        :: !G.Name
  -- ^ field name exposed in GraphQL interface
  , pgiPosition    :: !Int
  , pgiType        :: !(ColumnType b)
  , pgiIsNullable  :: !Bool
  , pgiDescription :: !(Maybe G.Description)
  } deriving (Generic)
deriving instance Backend b => Eq (ColumnInfo b)
instance Backend b => Cacheable (ColumnInfo b)
instance Backend b => NFData (ColumnInfo b)
instance Backend b => Hashable (ColumnInfo b)
instance Backend b => ToJSON (ColumnInfo b) where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase
  toEncoding = genericToEncoding $ aesonDrop 3 snakeCase

type PrimaryKeyColumns b = NESeq (ColumnInfo b)

onlyNumCols :: forall b . Backend b => [ColumnInfo b] -> [ColumnInfo b]
onlyNumCols = filter (isScalarColumnWhere (isNumType @b) . pgiType)

onlyJSONBCols :: [ColumnInfo 'Postgres] -> [ColumnInfo 'Postgres]
onlyJSONBCols = filter (isScalarColumnWhere (== PGJSONB) . pgiType)

onlyComparableCols :: forall b. Backend b => [ColumnInfo b] -> [ColumnInfo b]
onlyComparableCols = filter (isScalarColumnWhere (isComparableType @b) . pgiType)

getColInfos :: Backend b => [Column b] -> [ColumnInfo b] -> [ColumnInfo b]
getColInfos cols allColInfos =
  flip filter allColInfos $ \ci -> pgiColumn ci `elem` cols
