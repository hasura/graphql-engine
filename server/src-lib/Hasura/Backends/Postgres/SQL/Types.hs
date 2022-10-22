{-# LANGUAGE TemplateHaskell #-}

-- | Postgres SQL Types
--
-- Combinators and helpers for dealing with Postgres types such as strings, numerals,
-- geography and geometry, etc.
module Hasura.Backends.Postgres.SQL.Types
  ( pgFmtLit,
    pgFmtIdentifier,
    isView,
    QualifiedTable,
    QualifiedFunction,
    PGDescription (..),
    PGCol,
    unsafePGCol,
    getPGColTxt,
    showPGCols,
    isNumType,
    stringTypes,
    isStringType,
    isJSONType,
    isComparableType,
    isBigNum,
    geoTypes,
    isGeoType,
    IsIdentifier (..),
    Identifier (..),
    SchemaName (..),
    publicSchema,
    hdbCatalogSchema,
    TableName (..),
    FunctionName (..),
    ConstraintName (..),
    QualifiedObject (..),
    getIdentifierQualifiedObject,
    qualifiedObjectToText,
    snakeCaseQualifiedObject,
    namingConventionSupport,
    qualifiedObjectToName,
    PGScalarType (..),
    textToPGScalarType,
    pgScalarTypeToText,
    PGTypeKind (..),
    QualifiedPGType (..),
    isBaseType,
    typeToTable,
    mkFunctionArgScalarType,
    PGRawFunctionInfo (..),
    mkScalarTypeName,
  )
where

import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Aeson.TH
import Data.Aeson.Types (toJSONKeyText)
import Data.List (uncons)
import Data.Text qualified as T
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Database.PG.Query qualified as Q
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend (SupportedNamingCase (..))
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Function
import Hasura.SQL.Types
import Language.GraphQL.Draft.Syntax qualified as G
import PostgreSQL.Binary.Decoding qualified as PD
import Text.Builder qualified as TB

newtype Identifier = Identifier {getIdenTxt :: Text}
  deriving (Show, Eq, NFData, FromJSON, ToJSON, Hashable, Semigroup, Data, Cacheable)

instance ToSQL Identifier where
  toSQL (Identifier t) =
    TB.text $ pgFmtIdentifier t

class IsIdentifier a where
  toIdentifier :: a -> Identifier

instance IsIdentifier Identifier where
  toIdentifier = id

pgFmtIdentifier :: Text -> Text
pgFmtIdentifier x =
  "\"" <> T.replace "\"" "\"\"" (trimNullChars x) <> "\""

pgFmtLit :: Text -> Text
pgFmtLit x =
  let trimmed = trimNullChars x
      escaped = "'" <> T.replace "'" "''" trimmed <> "'"
      slashed = T.replace "\\" "\\\\" escaped
   in if "\\" `T.isInfixOf` escaped
        then "E" <> slashed
        else slashed

trimNullChars :: Text -> Text
trimNullChars = T.takeWhile (/= '\x0')

newtype TableName = TableName {getTableTxt :: Text}
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      Hashable,
      Q.ToPrepArg,
      Q.FromCol,
      Data,
      Generic,
      NFData,
      Cacheable,
      IsString
    )

instance IsIdentifier TableName where
  toIdentifier (TableName t) = Identifier t

instance ToTxt TableName where
  toTxt (TableName t) = t

instance ToSQL TableName where
  toSQL = toSQL . toIdentifier

data TableType
  = TTBaseTable
  | TTView
  | TTForeignTable
  | TTLocalTemporary
  deriving (Eq)

instance Q.FromCol TableType where
  fromCol bs = flip Q.fromColHelper bs $
    PD.enum $ \case
      "BASE TABLE" -> Just TTBaseTable
      "VIEW" -> Just TTView
      "FOREIGN TABLE" -> Just TTForeignTable
      "LOCAL TEMPORARY" -> Just TTLocalTemporary
      _ -> Nothing

isView :: TableType -> Bool
isView TTView = True
isView _ = False

newtype ConstraintName = ConstraintName {getConstraintTxt :: Text}
  deriving (Show, Eq, ToTxt, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, NFData, Cacheable)

instance IsIdentifier ConstraintName where
  toIdentifier (ConstraintName t) = Identifier t

instance ToSQL ConstraintName where
  toSQL = toSQL . toIdentifier

newtype FunctionName = FunctionName {getFunctionTxt :: Text}
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, Data, Generic, NFData, Cacheable)

instance IsIdentifier FunctionName where
  toIdentifier (FunctionName t) = Identifier t

instance ToTxt FunctionName where
  toTxt = getFunctionTxt

instance ToSQL FunctionName where
  toSQL = toSQL . toIdentifier

newtype SchemaName = SchemaName {getSchemaTxt :: Text}
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      Hashable,
      Q.ToPrepArg,
      Q.FromCol,
      Data,
      Generic,
      NFData,
      Cacheable,
      IsString
    )

publicSchema :: SchemaName
publicSchema = SchemaName "public"

hdbCatalogSchema :: SchemaName
hdbCatalogSchema = SchemaName "hdb_catalog"

instance IsIdentifier SchemaName where
  toIdentifier (SchemaName t) = Identifier t

instance ToSQL SchemaName where
  toSQL = toSQL . toIdentifier

data QualifiedObject a = QualifiedObject
  { qSchema :: !SchemaName,
    qName :: !a
  }
  deriving (Show, Eq, Functor, Ord, Generic, Data)

instance (NFData a) => NFData (QualifiedObject a)

instance (Cacheable a) => Cacheable (QualifiedObject a)

instance (FromJSON a) => FromJSON (QualifiedObject a) where
  parseJSON v@(String _) =
    QualifiedObject publicSchema <$> parseJSON v
  parseJSON (Object o) =
    QualifiedObject
      <$> o .:? "schema" .!= publicSchema
      <*> o .: "name"
  parseJSON _ =
    fail "expecting a string/object for QualifiedObject"

instance (ToJSON a) => ToJSON (QualifiedObject a) where
  toJSON (QualifiedObject sn o) =
    object
      [ "schema" .= sn,
        "name" .= o
      ]

instance (ToJSON a, ToTxt a) => ToJSONKey (QualifiedObject a) where
  toJSONKey = ToJSONKeyText qualifiedObjectToText (text . qualifiedObjectToText)

instance (ToTxt a) => ToTxt (QualifiedObject a) where
  toTxt = qualifiedObjectToText

instance (Hashable a) => Hashable (QualifiedObject a)

instance (ToSQL a) => ToSQL (QualifiedObject a) where
  toSQL (QualifiedObject sn o) =
    toSQL sn <> "." <> toSQL o

qualifiedObjectToText :: ToTxt a => QualifiedObject a -> Text
qualifiedObjectToText (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "." <> toTxt o

snakeCaseQualifiedObject :: ToTxt a => QualifiedObject a -> Text
snakeCaseQualifiedObject (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "_" <> toTxt o

getIdentifierQualifiedObject :: ToTxt a => QualifiedObject a -> Either QErr C.GQLNameIdentifier
getIdentifierQualifiedObject obj@(QualifiedObject sn o) = do
  let tLst =
        if sn == publicSchema
          then C.fromSnake $ toTxt o
          else C.fromSnake (getSchemaTxt sn) <> C.fromSnake (toTxt o)
      gqlIdents = do
        (pref, suffs) <- uncons tLst
        prefName <- G.mkName pref
        suffNames <- traverse G.mkNameSuffix suffs
        pure $ C.Identifier prefName suffNames
  gqlIdents
    `onNothing` throw400
      ValidationFailed
      ( "cannot include " <> obj <<> " in the GraphQL schema because " <> C.toSnakeT tLst
          <<> " is not a valid GraphQL identifier"
      )

namingConventionSupport :: SupportedNamingCase
namingConventionSupport = AllConventions

qualifiedObjectToName :: (ToTxt a, MonadError QErr m) => QualifiedObject a -> m G.Name
qualifiedObjectToName objectName = do
  let textName = snakeCaseQualifiedObject objectName
  onNothing (G.mkName textName) $
    throw400 ValidationFailed $
      "cannot include " <> objectName <<> " in the GraphQL schema because " <> textName
        <<> " is not a valid GraphQL identifier"

type QualifiedTable = QualifiedObject TableName

type QualifiedFunction = QualifiedObject FunctionName

newtype PGDescription = PGDescription {getPGDescription :: Text}
  deriving (Show, Eq, FromJSON, ToJSON, Q.FromCol, NFData, Cacheable, Hashable)

newtype PGCol = PGCol {getPGColTxt :: Text}
  deriving
    ( Show,
      Eq,
      Ord,
      FromJSON,
      ToJSON,
      Hashable,
      Q.ToPrepArg,
      Q.FromCol,
      ToJSONKey,
      FromJSONKey,
      Data,
      Generic,
      NFData,
      Cacheable,
      IsString
    )

instance IsIdentifier PGCol where
  toIdentifier (PGCol t) = Identifier t

instance ToSQL PGCol where
  toSQL = toSQL . toIdentifier

instance ToTxt PGCol where
  toTxt = getPGColTxt

unsafePGCol :: Text -> PGCol
unsafePGCol = PGCol

showPGCols :: (Foldable t, Functor t) => t PGCol -> T.Text
showPGCols = dquoteList . fmap getPGColTxt

data PGScalarType
  = PGSmallInt
  | PGInteger
  | PGBigInt
  | PGSerial
  | PGBigSerial
  | PGFloat
  | PGDouble
  | PGNumeric
  | PGMoney
  | PGBoolean
  | PGChar
  | PGVarchar
  | PGText
  | PGCitext
  | PGDate
  | PGTimeStamp
  | PGTimeStampTZ
  | PGTimeTZ
  | PGJSON
  | PGJSONB
  | PGGeometry
  | PGGeography
  | PGRaster
  | PGUUID
  | PGLtree
  | PGLquery
  | PGLtxtquery
  | PGUnknown !Text
  | PGCompositeScalar !Text
  | PGEnumScalar !Text
  deriving (Show, Eq, Ord, Generic, Data)

instance NFData PGScalarType

instance Hashable PGScalarType

instance Cacheable PGScalarType

pgScalarTypeToText :: PGScalarType -> Text
pgScalarTypeToText = \case
  PGSmallInt -> "smallint"
  PGInteger -> "integer"
  PGBigInt -> "bigint"
  PGSerial -> "serial"
  PGBigSerial -> "bigserial"
  PGFloat -> "real"
  PGDouble -> "float8"
  PGNumeric -> "numeric"
  PGMoney -> "money"
  PGBoolean -> "boolean"
  PGChar -> "bpchar"
  PGVarchar -> "varchar"
  PGText -> "text"
  PGCitext -> "citext"
  PGDate -> "date"
  PGTimeStamp -> "timestamp"
  PGTimeStampTZ -> "timestamptz"
  PGTimeTZ -> "timetz"
  PGJSON -> "json"
  PGJSONB -> "jsonb"
  PGGeometry -> "geometry"
  PGGeography -> "geography"
  PGRaster -> "raster"
  PGUUID -> "uuid"
  PGLtree -> "ltree"
  PGLquery -> "lquery"
  PGLtxtquery -> "ltxtquery"
  PGUnknown t -> t
  PGCompositeScalar t -> t
  PGEnumScalar t -> t

instance ToSQL PGScalarType where
  toSQL =
    TB.text . \case
      -- Format enum type names as identifiers to preserve case sensitivity
      -- https://github.com/hasura/graphql-engine/issues/4014
      PGEnumScalar t -> pgFmtIdentifier t
      scalarType -> pgScalarTypeToText scalarType

instance ToJSON PGScalarType where
  toJSON = String . pgScalarTypeToText

instance ToJSONKey PGScalarType where
  toJSONKey = toJSONKeyText pgScalarTypeToText

instance ToTxt PGScalarType where
  toTxt = pgScalarTypeToText

textToPGScalarType :: Text -> PGScalarType
textToPGScalarType t = fromMaybe (PGUnknown t) (lookup t pgScalarTranslations)

-- Inlining this results in pretty terrible Core being generated by GHC.

{-# NOINLINE pgScalarTranslations #-}
pgScalarTranslations :: [(Text, PGScalarType)]
pgScalarTranslations =
  [ ("serial", PGSerial),
    ("bigserial", PGBigSerial),
    ("smallint", PGSmallInt),
    ("int2", PGSmallInt),
    ("integer", PGInteger),
    ("int4", PGInteger),
    ("bigint", PGBigInt),
    ("int8", PGBigInt),
    ("real", PGFloat),
    ("float4", PGFloat),
    ("double precision", PGDouble),
    ("float8", PGDouble),
    ("numeric", PGNumeric),
    ("decimal", PGNumeric),
    ("money", PGMoney),
    ("boolean", PGBoolean),
    ("bool", PGBoolean),
    ("bpchar", PGChar),
    ("char", PGChar),
    ("character", PGChar),
    ("varchar", PGVarchar),
    ("character varying", PGVarchar),
    ("text", PGText),
    ("citext", PGCitext),
    ("date", PGDate),
    ("timestamp", PGTimeStamp),
    ("timestamp without time zone", PGTimeStamp),
    ("timestamptz", PGTimeStampTZ),
    ("timestamp with time zone", PGTimeStampTZ),
    ("timetz", PGTimeTZ),
    ("time with time zone", PGTimeTZ),
    ("json", PGJSON),
    ("jsonb", PGJSONB),
    ("geometry", PGGeometry),
    ("geography", PGGeography),
    ("raster", PGRaster),
    ("uuid", PGUUID),
    ("ltree", PGLtree),
    ("lquery", PGLquery),
    ("ltxtquery", PGLtxtquery)
  ]

instance FromJSON PGScalarType where
  parseJSON (String t) = return $ textToPGScalarType t
  parseJSON (Object o) = do
    typeType <- o .: "type"
    typeName <- o .: "name"
    pure $
      case typeType of
        PGKindEnum -> PGEnumScalar typeName
        PGKindComposite -> PGCompositeScalar typeName
        _ -> textToPGScalarType typeName
  parseJSON _ = fail "Expecting a string or object for PGScalarType"

isNumType :: PGScalarType -> Bool
isNumType PGInteger = True
isNumType PGSmallInt = True
isNumType PGBigInt = True
isNumType PGFloat = True
isNumType PGDouble = True
isNumType PGNumeric = True
isNumType PGMoney = True
isNumType _ = False

stringTypes :: [PGScalarType]
stringTypes = [PGVarchar, PGText, PGCitext, PGChar]

isStringType :: PGScalarType -> Bool
isStringType = (`elem` stringTypes)

jsonTypes :: [PGScalarType]
jsonTypes = [PGJSON, PGJSONB]

isJSONType :: PGScalarType -> Bool
isJSONType = (`elem` jsonTypes)

isComparableType :: PGScalarType -> Bool
isComparableType PGJSON = False
isComparableType PGJSONB = False
isComparableType PGGeometry = False
isComparableType PGGeography = False
isComparableType PGBoolean = False
isComparableType (PGUnknown _) = False
isComparableType _ = True

isBigNum :: PGScalarType -> Bool
isBigNum = \case
  PGBigInt -> True
  PGBigSerial -> True
  PGNumeric -> True
  PGDouble -> True
  PGMoney -> True
  _ -> False

geoTypes :: [PGScalarType]
geoTypes = [PGGeometry, PGGeography]

isGeoType :: PGScalarType -> Bool
isGeoType = (`elem` geoTypes)

data PGTypeKind
  = PGKindBase
  | PGKindComposite
  | PGKindDomain
  | PGKindEnum
  | PGKindRange
  | PGKindPseudo
  | PGKindUnknown !Text
  deriving (Show, Eq, Generic)

instance NFData PGTypeKind

instance Hashable PGTypeKind

instance Cacheable PGTypeKind

instance FromJSON PGTypeKind where
  parseJSON = withText "postgresTypeKind" $
    \t -> pure $ case t of
      "b" -> PGKindBase
      "c" -> PGKindComposite
      "d" -> PGKindDomain
      "e" -> PGKindEnum
      "r" -> PGKindRange
      "p" -> PGKindPseudo
      _ -> PGKindUnknown t

instance ToJSON PGTypeKind where
  toJSON = \case
    PGKindBase -> "b"
    PGKindComposite -> "c"
    PGKindDomain -> "d"
    PGKindEnum -> "e"
    PGKindRange -> "r"
    PGKindPseudo -> "p"
    PGKindUnknown t -> String t

data QualifiedPGType = QualifiedPGType
  { _qptSchema :: !SchemaName,
    _qptName :: !PGScalarType,
    _qptType :: !PGTypeKind
  }
  deriving (Show, Eq, Generic)

instance NFData QualifiedPGType

instance Hashable QualifiedPGType

instance Cacheable QualifiedPGType

$(deriveJSON hasuraJSON ''QualifiedPGType)

isBaseType :: QualifiedPGType -> Bool
isBaseType (QualifiedPGType _ n ty) =
  notUnknown && (ty == PGKindBase)
  where
    notUnknown = case n of
      PGUnknown _ -> False
      _ -> True

typeToTable :: QualifiedPGType -> QualifiedTable
typeToTable (QualifiedPGType sch n _) =
  QualifiedObject sch $ TableName $ pgScalarTypeToText n

mkFunctionArgScalarType :: QualifiedPGType -> PGScalarType
mkFunctionArgScalarType (QualifiedPGType _schema name type') =
  case type' of
    -- The suffix `_scalar` is added in
    -- the @mkScalarTypeName@ function.
    PGKindComposite -> PGCompositeScalar $ toTxt name
    _ -> name

-- | Metadata describing SQL functions at the DB level, i.e. below the GraphQL layer.
data PGRawFunctionInfo = PGRawFunctionInfo
  { rfiOid :: !OID,
    rfiHasVariadic :: !Bool,
    rfiFunctionType :: !FunctionVolatility,
    rfiReturnTypeSchema :: !SchemaName,
    rfiReturnTypeName :: !PGScalarType,
    rfiReturnTypeType :: !PGTypeKind,
    rfiReturnsSet :: !Bool,
    rfiInputArgTypes :: ![QualifiedPGType],
    rfiInputArgNames :: ![FunctionArgName],
    rfiDefaultArgs :: !Int,
    rfiReturnsTable :: !Bool,
    rfiDescription :: !(Maybe PGDescription)
  }
  deriving (Show, Eq, Generic)

instance NFData PGRawFunctionInfo

instance Cacheable PGRawFunctionInfo

$(deriveJSON hasuraJSON ''PGRawFunctionInfo)

mkScalarTypeName :: MonadError QErr m => PGScalarType -> m G.Name
mkScalarTypeName PGInteger = pure intScalar
mkScalarTypeName PGBoolean = pure boolScalar
mkScalarTypeName PGFloat = pure floatScalar
mkScalarTypeName PGText = pure stringScalar
mkScalarTypeName PGVarchar = pure stringScalar
mkScalarTypeName (PGCompositeScalar compositeScalarType) =
  -- When the function argument is a row type argument
  -- then it's possible that there can be an object type
  -- with the table name depending upon whether the table
  -- is tracked or not. As a result, we get a conflict between
  -- both these types (scalar and object type with same name).
  -- To avoid this, we suffix the table name with `_scalar`
  -- and create a new scalar type
  (<> G.__scalar) <$> G.mkName compositeScalarType
    `onNothing` throw400
      ValidationFailed
      ( "cannot use SQL type " <> compositeScalarType <<> " in the GraphQL schema because its name is not a "
          <> "valid GraphQL identifier"
      )
mkScalarTypeName scalarType =
  G.mkName (pgScalarTypeToText scalarType)
    `onNothing` throw400
      ValidationFailed
      ( "cannot use SQL type " <> scalarType <<> " in the GraphQL schema because its name is not a "
          <> "valid GraphQL identifier"
      )

instance IsIdentifier RelName where
  toIdentifier rn = Identifier $ relNameToTxt rn

instance IsIdentifier FieldName where
  toIdentifier (FieldName f) = Identifier f
