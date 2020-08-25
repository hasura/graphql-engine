module Hasura.RQL.Types.Column
  ( PGColumnType(..)
  , _PGColumnScalar
  , _PGColumnEnumReference
  , isScalarColumnWhere

  , onlyIntCols
  , onlyNumCols
  , onlyJSONBCols
  , onlyComparableCols

  , parsePGScalarValue
  , parsePGScalarValues
  , unsafePGColumnToRepresentation
  , parseTxtEncodedPGValue

  , PGColumnInfo(..)
  , PGRawColumnInfo(..)
  , PrimaryKeyColumns
  , getColInfos

  , EnumReference(..)
  , EnumValues
  , EnumValue(..)
  , EnumValueInfo(..)
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as M
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.Incremental            (Cacheable)
import           Hasura.RQL.Instances          ()
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Types
import           Hasura.SQL.Value
import           Language.Haskell.TH.Syntax    (Lift)

newtype EnumValue
  = EnumValue { getEnumValue :: G.Name }
  deriving (Show, Eq, Ord, Lift, NFData, Hashable, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Cacheable)

newtype EnumValueInfo
  = EnumValueInfo
  { evComment :: Maybe T.Text
  } deriving (Show, Eq, Ord, Lift, NFData, Hashable, Cacheable)
$(deriveJSON (aesonDrop 2 snakeCase) ''EnumValueInfo)

type EnumValues = M.HashMap EnumValue EnumValueInfo

-- | Represents a reference to an “enum table,” a single-column Postgres table that is referenced
-- via foreign key.
data EnumReference
  = EnumReference
  { erTable  :: !QualifiedTable
  , erValues :: !EnumValues
  } deriving (Show, Eq, Ord, Generic, Lift)
instance NFData EnumReference
instance Hashable EnumReference
instance Cacheable EnumReference
$(deriveJSON (aesonDrop 2 snakeCase) ''EnumReference)

-- | The type we use for columns, which are currently always “scalars” (though see the note about
-- 'PGType'). Unlike 'PGScalarType', which represents a type that /Postgres/ knows about, this type
-- characterizes distinctions we make but Postgres doesn’t.
data PGColumnType
  -- | Ordinary Postgres columns.
  = PGColumnScalar !PGScalarType
  -- | Columns that reference enum tables (see "Hasura.RQL.Schema.Enum"). This is not actually a
  -- distinct type from the perspective of Postgres (at the time of this writing, we ensure they
  -- always have type @text@), but we really want to distinguish this case, since we treat it
  -- /completely/ differently in the GraphQL schema.
  | PGColumnEnumReference !EnumReference
  deriving (Show, Eq, Ord, Generic)
instance NFData PGColumnType
instance Hashable PGColumnType
instance Cacheable PGColumnType
$(deriveToJSON defaultOptions{constructorTagModifier = drop 8} ''PGColumnType)
$(makePrisms ''PGColumnType)

instance DQuote PGColumnType where
  dquoteTxt = \case
    PGColumnScalar scalar -> dquoteTxt scalar
    PGColumnEnumReference (EnumReference tableName _) -> dquoteTxt tableName

isScalarColumnWhere :: (PGScalarType -> Bool) -> PGColumnType -> Bool
isScalarColumnWhere f = \case
  PGColumnScalar scalar -> f scalar
  PGColumnEnumReference _ -> False

-- | Gets the representation type associated with a 'PGColumnType'. Avoid using this if possible.
-- Prefer 'parsePGScalarValue', 'parsePGScalarValues', or
-- 'Hasura.RQL.Types.BoolExp.mkTypedSessionVar'.
unsafePGColumnToRepresentation :: PGColumnType -> PGScalarType
unsafePGColumnToRepresentation = \case
  PGColumnScalar scalarType -> scalarType
  PGColumnEnumReference _ -> PGText

-- | Note: Unconditionally accepts null values and returns 'PGNull'.
parsePGScalarValue
  :: forall m. (MonadError QErr m) => PGColumnType -> Value -> m (WithScalarType PGScalarValue)
parsePGScalarValue columnType value = case columnType of
  PGColumnScalar scalarType ->
    WithScalarType scalarType <$> runAesonParser (parsePGValue scalarType) value
  PGColumnEnumReference (EnumReference tableName enumValues) ->
    WithScalarType PGText <$> (maybe (pure $ PGNull PGText) parseEnumValue =<< decodeValue value)
    where
      parseEnumValue :: G.Name -> m PGScalarValue
      parseEnumValue enumValueName = do
        let enums = map getEnumValue $ M.keys enumValues
        unless (enumValueName `elem` enums) $ throw400 UnexpectedPayload
          $ "expected one of the values " <> T.intercalate ", " (map dquote enums)
          <> " for type " <> snakeCaseQualObject tableName <<> ", given " <>> enumValueName
        pure $ PGValText $ G.unName enumValueName

parsePGScalarValues
  :: (MonadError QErr m)
  => PGColumnType -> [Value] -> m (WithScalarType [PGScalarValue])
parsePGScalarValues columnType values = do
  scalarValues <- indexedMapM (fmap pstValue . parsePGScalarValue columnType) values
  pure $ WithScalarType (unsafePGColumnToRepresentation columnType) scalarValues

parseTxtEncodedPGValue
  :: (MonadError QErr m)
  => PGColumnType -> TxtEncodedPGVal -> m (WithScalarType PGScalarValue)
parseTxtEncodedPGValue colTy val =
  parsePGScalarValue colTy $ case val of
    TENull  -> Null
    TELit t -> String t


-- | “Raw” column info, as stored in the catalog (but not in the schema cache). Instead of
-- containing a 'PGColumnType', it only contains a 'PGScalarType', which is combined with the
-- 'pcirReferences' field and other table data to eventually resolve the type to a 'PGColumnType'.
data PGRawColumnInfo
  = PGRawColumnInfo
  { prciName        :: !PGCol
  , prciPosition    :: !Int
  -- ^ The “ordinal position” of the column according to Postgres. Numbering starts at 1 and
  -- increases. Dropping a column does /not/ cause the columns to be renumbered, so a column can be
  -- consistently identified by its position.
  , prciType        :: !PGScalarType
  , prciIsNullable  :: !Bool
  , prciDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Generic)
instance NFData PGRawColumnInfo
instance Cacheable PGRawColumnInfo
$(deriveJSON (aesonDrop 4 snakeCase) ''PGRawColumnInfo)

-- | “Resolved” column info, produced from a 'PGRawColumnInfo' value that has been combined with
-- other schema information to produce a 'PGColumnType'.
data PGColumnInfo
  = PGColumnInfo
  { pgiColumn      :: !PGCol
  , pgiName        :: !G.Name
  -- ^ field name exposed in GraphQL interface
  , pgiPosition    :: !Int
  , pgiType        :: !PGColumnType
  , pgiIsNullable  :: !Bool
  , pgiDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Generic)
instance NFData PGColumnInfo
instance Cacheable PGColumnInfo
instance Hashable PGColumnInfo
$(deriveToJSON (aesonDrop 3 snakeCase) ''PGColumnInfo)

type PrimaryKeyColumns = NESeq PGColumnInfo

onlyIntCols :: [PGColumnInfo] -> [PGColumnInfo]
onlyIntCols = filter (isScalarColumnWhere isIntegerType . pgiType)

onlyNumCols :: [PGColumnInfo] -> [PGColumnInfo]
onlyNumCols = filter (isScalarColumnWhere isNumType . pgiType)

onlyJSONBCols :: [PGColumnInfo] -> [PGColumnInfo]
onlyJSONBCols = filter (isScalarColumnWhere (== PGJSONB) . pgiType)

onlyComparableCols :: [PGColumnInfo] -> [PGColumnInfo]
onlyComparableCols = filter (isScalarColumnWhere isComparableType . pgiType)

getColInfos :: [PGCol] -> [PGColumnInfo] -> [PGColumnInfo]
getColInfos cols allColInfos =
  flip filter allColInfos $ \ci -> pgiColumn ci `elem` cols
