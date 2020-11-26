module Hasura.RQL.Types.Column
  ( ColumnType(..)
  , _ColumnScalar
  , _ColumnEnumReference
  , isScalarColumnWhere

  , onlyIntCols
  , onlyNumCols
  , onlyJSONBCols
  , onlyComparableCols

  , parsePGScalarValue
  , parsePGScalarValues
  , unsafePGColumnToBackend
  , parseTxtEncodedPGValue

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

import           Hasura.Backends.Postgres.SQL.Types hiding (TableName)
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

-- | The type we use for columns, which are currently always “scalars” (though see the note about
-- 'PGType'). Unlike 'PGScalarType', which represents a type that /Postgres/ knows about, this type
-- characterizes distinctions we make but Postgres doesn’t.
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

isScalarColumnWhere :: (ScalarType b -> Bool) -> ColumnType b -> Bool
isScalarColumnWhere f = \case
  ColumnScalar scalar   -> f scalar
  ColumnEnumReference _ -> False

-- | Gets the representation type associated with a 'ColumnType'. Avoid using this if possible.
-- Prefer 'parsePGScalarValue', 'parsePGScalarValues', or
-- 'Hasura.RQL.Types.BoolExp.mkTypedSessionVar'.
unsafePGColumnToBackend :: ColumnType 'Postgres -> PGScalarType
unsafePGColumnToBackend = \case
  ColumnScalar scalarType -> scalarType
  ColumnEnumReference _   -> PGText

-- | Note: Unconditionally accepts null values and returns 'PGNull'.
parsePGScalarValue
  :: forall m. (MonadError QErr m) => ColumnType 'Postgres -> Value -> m (WithScalarType PGScalarValue)
parsePGScalarValue columnType value = case columnType of
  ColumnScalar scalarType ->
    WithScalarType scalarType <$> runAesonParser (parsePGValue scalarType) value
  ColumnEnumReference (EnumReference tableName enumValues) ->
    WithScalarType PGText <$> (maybe (pure $ PGNull PGText) parseEnumValue =<< decodeValue value)
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
  => ColumnType 'Postgres -> [Value] -> m (WithScalarType [PGScalarValue])
parsePGScalarValues columnType values = do
  scalarValues <- indexedMapM (fmap pstValue . parsePGScalarValue columnType) values
  pure $ WithScalarType (unsafePGColumnToBackend columnType) scalarValues

parseTxtEncodedPGValue
  :: (MonadError QErr m)
  => ColumnType 'Postgres -> TxtEncodedPGVal -> m (WithScalarType PGScalarValue)
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
deriving instance Eq (RawColumnInfo 'Postgres)
instance NFData (RawColumnInfo 'Postgres)
instance Cacheable (RawColumnInfo 'Postgres)
instance ToJSON (RawColumnInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 4 snakeCase
instance FromJSON (RawColumnInfo 'Postgres) where
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
deriving instance Eq (ColumnInfo 'Postgres)
instance NFData (ColumnInfo 'Postgres)
instance Cacheable (ColumnInfo 'Postgres)
instance Hashable (ColumnInfo 'Postgres)
instance ToJSON (ColumnInfo 'Postgres) where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase
  toEncoding = genericToEncoding $ aesonDrop 3 snakeCase

type PrimaryKeyColumns b = NESeq (ColumnInfo b)

onlyIntCols :: [ColumnInfo 'Postgres] -> [ColumnInfo 'Postgres]
onlyIntCols = filter (isScalarColumnWhere isIntegerType . pgiType)

onlyNumCols :: [ColumnInfo 'Postgres] -> [ColumnInfo 'Postgres]
onlyNumCols = filter (isScalarColumnWhere isNumType . pgiType)

onlyJSONBCols :: [ColumnInfo 'Postgres] -> [ColumnInfo 'Postgres]
onlyJSONBCols = filter (isScalarColumnWhere (== PGJSONB) . pgiType)

onlyComparableCols :: [ColumnInfo 'Postgres] -> [ColumnInfo 'Postgres]
onlyComparableCols = filter (isScalarColumnWhere isComparableType . pgiType)

getColInfos :: Eq (Column backend) => [Column backend] -> [ColumnInfo backend] -> [ColumnInfo backend]
getColInfos cols allColInfos =
  flip filter allColInfos $ \ci -> pgiColumn ci `elem` cols
