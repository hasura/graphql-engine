module Hasura.RQL.Types.Column
  ( PGColumnType(..)
  , _PGColumnScalar
  , _PGColumnEnumReference
  , isScalarColumnWhere

  , parsePGScalarValue
  , parsePGScalarValues
  , unsafePGColumnToRepresentation

  , PGColumnInfo(..)
  , PGRawColumnInfo(..)
  , onlyIntCols
  , onlyNumCols
  , onlyJSONBCols
  , onlyComparableCols
  , getColInfos

  , EnumReference(..)
  , EnumValues
  , EnumValue(..)
  , EnumValueInfo(..)
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict        as M
import qualified Data.Text                  as T

import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax (Lift)

import           Hasura.RQL.Instances       ()
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Types
import           Hasura.SQL.Value

newtype EnumValue
  = EnumValue { getEnumValue :: T.Text }
  deriving (Show, Eq, Lift, Hashable, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

newtype EnumValueInfo
  = EnumValueInfo
  { evComment :: Maybe T.Text
  } deriving (Show, Eq, Lift, Hashable)
$(deriveJSON (aesonDrop 2 snakeCase) ''EnumValueInfo)

type EnumValues = M.HashMap EnumValue EnumValueInfo

-- | Represents a reference to an “enum table,” a single-column Postgres table that is referenced
-- via foreign key.
data EnumReference
  = EnumReference
  { erTable  :: !QualifiedTable
  , erValues :: !EnumValues
  } deriving (Show, Eq, Generic, Lift)
instance Hashable EnumReference
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
  deriving (Show, Eq, Generic)
instance Hashable PGColumnType
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

parsePGScalarValue :: (MonadError QErr m) => PGColumnType -> Value -> m (WithScalarType PGScalarValue)
parsePGScalarValue columnType value = case columnType of
  PGColumnScalar scalarType ->
    WithScalarType scalarType <$> runAesonParser (parsePGValue scalarType) value
  PGColumnEnumReference (EnumReference tableName enumValues) -> do
    let typeName = snakeCaseQualObject tableName
    flip runAesonParser value . withText (T.unpack typeName) $ \textValue -> do
      let enumTextValues = map getEnumValue $ M.keys enumValues
      unless (textValue `elem` enumTextValues) $
        fail . T.unpack
          $ "expected one of the values " <> T.intercalate ", " (map dquote enumTextValues)
          <> " for type " <> typeName <<> ", given " <>> textValue
      pure $ WithScalarType PGText (PGValText textValue)

parsePGScalarValues
  :: (MonadError QErr m)
  => PGColumnType -> [Value] -> m (WithScalarType [PGScalarValue])
parsePGScalarValues columnType values = do
  scalarValues <- indexedMapM (fmap pstValue . parsePGScalarValue columnType) values
  pure $ WithScalarType (unsafePGColumnToRepresentation columnType) scalarValues

-- | “Raw” column info, as stored in the catalog (but not in the schema cache). Instead of
-- containing a 'PGColumnType', it only contains a 'PGScalarType', which is combined with the
-- 'pcirReferences' field and other table data to eventually resolve the type to a 'PGColumnType'.
data PGRawColumnInfo
  = PGRawColumnInfo
  { prciName       :: !PGCol
  , prciType       :: !PGScalarType
  , prciIsNullable :: !Bool
  , prciReferences :: ![QualifiedTable]
  -- ^ only stores single-column references to primary key of foreign tables (used for detecting
  -- references to enum tables)
  } deriving (Show, Eq)
$(deriveJSON (aesonDrop 4 snakeCase) ''PGRawColumnInfo)

-- | “Resolved” column info, produced from a 'PGRawColumnInfo' value that has been combined with other
-- schema information to produce a 'PGColumnType'.
data PGColumnInfo
  = PGColumnInfo
  { pgiName       :: !PGCol
  , pgiType       :: !PGColumnType
  , pgiIsNullable :: !Bool
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 3 snakeCase) ''PGColumnInfo)

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
  flip filter allColInfos $ \ci -> pgiName ci `elem` cols
