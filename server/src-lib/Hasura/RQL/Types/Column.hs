module Hasura.RQL.Types.Column
  ( ColumnType(..)
  , _ColumnScalar
  , _ColumnEnumReference
  , isScalarColumnWhere
  , ValueParser

  , onlyNumCols
  , onlyComparableCols

  , parseScalarValueColumnType
  , parseScalarValuesColumnType

  , ColumnValue(..)

  , ColumnInfo(..)
  , RawColumnInfo(..)
  , PrimaryKeyColumns
  , getColInfos

  , EnumReference(..)
  , EnumValues
  , EnumValue(..)
  , EnumValueInfo(..)

  , fromCol
  , ColumnValues
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as M
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text.Extended

import           Hasura.Incremental            (Cacheable)
import           Hasura.RQL.Instances          ()
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend
import           Hasura.SQL.Types


newtype EnumValue
  = EnumValue { getEnumValue :: G.Name }
  deriving (Show, Eq, Ord, NFData, Hashable, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Cacheable)

newtype EnumValueInfo
  = EnumValueInfo
  { evComment :: Maybe Text
  } deriving (Show, Eq, Ord, NFData, Hashable, Cacheable)
$(deriveJSON hasuraJSON ''EnumValueInfo)

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
instance (Backend b) => FromJSON (EnumReference b) where
  parseJSON = genericParseJSON hasuraJSON
instance (Backend b) => ToJSON (EnumReference b) where
  toJSON = genericToJSON hasuraJSON

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
instance (Backend b) => ToJSON (ColumnType b) where
  toJSON = genericToJSON $ defaultOptions{constructorTagModifier = drop 6}

$(makePrisms ''ColumnType)

instance Backend b => ToTxt (ColumnType b) where
  toTxt = \case
    ColumnScalar scalar                             -> toTxt scalar
    ColumnEnumReference (EnumReference tableName _) -> toTxt tableName

-- | A parser to parse a json value with enforcing column type
type ValueParser b m v =
  CollectableType (ColumnType b) -> Value -> m v

data ColumnValue (b :: BackendType) = ColumnValue
  { cvType  :: ColumnType b
  , cvValue :: ScalarValue b
  }

isScalarColumnWhere :: (ScalarType b -> Bool) -> ColumnType b -> Bool
isScalarColumnWhere f = \case
  ColumnScalar scalar   -> f scalar
  ColumnEnumReference _ -> False

-- | Note: Unconditionally accepts null values and returns 'PGNull'.
parseScalarValueColumnType
  :: forall m b
  . (MonadError QErr m, Backend b)
  => ColumnType b -> Value -> m (ScalarValue b)
parseScalarValueColumnType columnType value = case columnType of
  ColumnScalar scalarType -> liftEither $ parseScalarValue @b scalarType value
  ColumnEnumReference (EnumReference tableName enumValues) ->
    -- maybe (pure $ PGNull PGText) parseEnumValue =<< decodeValue value
    parseEnumValue =<< decodeValue value
    where
      parseEnumValue :: Maybe G.Name -> m (ScalarValue b)
      parseEnumValue enumValueName = do
        onJust enumValueName \evn -> do
          let enums = map getEnumValue $ M.keys enumValues
          unless (evn `elem` enums) $ throw400 UnexpectedPayload
            $ "expected one of the values " <> dquoteList enums
            <> " for type " <> snakeCaseTableName tableName <<> ", given " <>> evn
        pure $ textToScalarValue @b $ G.unName <$> enumValueName

parseScalarValuesColumnType
  :: (MonadError QErr m, Backend b)
  => ColumnType b -> [Value] -> m [ScalarValue b]
parseScalarValuesColumnType columnType values =
  indexedMapM (parseScalarValueColumnType columnType) values

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
  toJSON = genericToJSON hasuraJSON
instance Backend b => FromJSON (RawColumnInfo b) where
  parseJSON = genericParseJSON hasuraJSON

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
deriving instance Backend b => Show (ColumnInfo b)
instance Backend b => Cacheable (ColumnInfo b)
instance Backend b => NFData (ColumnInfo b)
instance Backend b => Hashable (ColumnInfo b)
instance Backend b => ToJSON (ColumnInfo b) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

type PrimaryKeyColumns b = NESeq (ColumnInfo b)

onlyNumCols :: forall b . Backend b => [ColumnInfo b] -> [ColumnInfo b]
onlyNumCols = filter (isScalarColumnWhere (isNumType @b) . pgiType)

onlyComparableCols :: forall b. Backend b => [ColumnInfo b] -> [ColumnInfo b]
onlyComparableCols = filter (isScalarColumnWhere (isComparableType @b) . pgiType)

getColInfos :: Backend b => [Column b] -> [ColumnInfo b] -> [ColumnInfo b]
getColInfos cols allColInfos =
  flip filter allColInfos $ \ci -> pgiColumn ci `elem` cols

fromCol :: Backend b => Column b -> FieldName
fromCol = FieldName . toTxt

type ColumnValues b a = HashMap (Column b) a
