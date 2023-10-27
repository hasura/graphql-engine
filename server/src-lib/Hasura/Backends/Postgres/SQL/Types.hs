{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

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
    ColumnIdentifier (..),
    TableIdentifier (..),
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
    pgScalarTranslations,
    pgScalarTypeToText,
    PGTypeKind (..),
    QualifiedPGType (..),
    isBaseType,
    typeToTable,
    mkFunctionArgScalarType,
    PGRawFunctionInfo (..),
    mkScalarTypeName,
    pgTypeOid,
    tableIdentifierToIdentifier,
    identifierToTableIdentifier,
    PGExtraTableMetadata (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec, optionalFieldWithDefault', parseAlternative, requiredField')
import Autodocodec qualified as AC
import Autodocodec.Extended (typeableName)
import Data.Aeson
import Data.Aeson.Encoding (text)
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (toJSONKeyText)
import Data.Int
import Data.List (uncons)
import Data.String
import Data.Text qualified as T
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Data.Text.NonEmpty (NonEmptyText (unNonEmptyText))
import Data.Typeable (Typeable)
import Database.PG.Query qualified as PG
import Database.PG.Query.PTI qualified as PTI
import Database.PostgreSQL.LibPQ qualified as PQ
import Hasura.Base.Error
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue
import Hasura.Function.Cache
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.Types.Backend (SupportedNamingCase (..))
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField.Name (ComputedFieldName (..))
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Source.TableType (SourceTableType (..))
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.Types
import Language.GraphQL.Draft.Syntax qualified as G
import PostgreSQL.Binary.Decoding qualified as PD
import Text.Builder qualified as TB
import Text.Regex.TDFA ((=~))

{- Note [About identifier types]

In order to better be able to reason about values representing SQL binders and
variables we are in the process of retiring the generic 'Identifier' type in
favor of the more specific types 'TableIdentifier' and 'ColumnIdentifier'.

Likewise, we distinguish binders of names from uses of names: The types
'TableAlias' and `ColumnAlias` are used to for binders, whereas
`TableIdentifier` and `ColumnIdentifier` represent usages or references of
previously bound names.

We want to ensure these are handled in an hygenic way:
\* 'TableAlias'es and 'ColumnAlias'es can be constructed freely, but
\* 'TableIdentifier' can only be constructed from a 'TableAlias' via
  'tableAliasToIdentifier', and
\* 'ColumnIdentifier's can only be constructed from a 'ColumnAlias', and
  potentially be qualified with a 'TableIdentifier'.

-}

newtype Identifier = Identifier {getIdenTxt :: Text}
  deriving stock (Data, Eq, Show)
  deriving newtype (NFData, FromJSON, ToJSON, Hashable, Semigroup)

instance ToSQL Identifier where
  toSQL (Identifier t) =
    TB.text $ pgFmtIdentifier t

class IsIdentifier a where
  toIdentifier :: a -> Identifier

instance IsIdentifier Identifier where
  toIdentifier = id

instance IsIdentifier ComputedFieldName where
  toIdentifier = Identifier . unNonEmptyText . unComputedFieldName

-- | The type of identifiers representing tabular values.
-- While we are transitioning away from 'Identifier' we provisionally export
-- the value constructor.
newtype TableIdentifier = TableIdentifier {unTableIdentifier :: Text}
  deriving stock (Data, Eq, Show)
  deriving newtype (NFData, FromJSON, ToJSON, Hashable, Semigroup)

-- | Temporary conversion function, to be removed once 'Identifier' has been
-- entirely split into 'TableIdentifier' and 'ColumnIdentifier'.
tableIdentifierToIdentifier :: TableIdentifier -> Identifier
tableIdentifierToIdentifier = Identifier . unTableIdentifier

-- | Temporary conversion function, to be removed once 'Identifier' has been
-- entirely split into 'TableIdentifier' and 'ColumnIdentifier'.
identifierToTableIdentifier :: Identifier -> TableIdentifier
identifierToTableIdentifier = TableIdentifier . getIdenTxt

instance ToSQL TableIdentifier where
  toSQL (TableIdentifier t) =
    TB.text $ pgFmtIdentifier t

-- | The type of identifiers representing scalar values
newtype ColumnIdentifier = ColumnIdentifier {unColumnIdentifier :: Text}
  deriving stock (Data, Eq, Show)
  deriving newtype (NFData, FromJSON, ToJSON, Hashable, Semigroup)

instance ToSQL ColumnIdentifier where
  toSQL (ColumnIdentifier t) =
    TB.text $ pgFmtIdentifier t

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
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON, Hashable, PG.ToPrepArg, PG.FromCol, NFData, IsString)

instance HasCodec TableName where
  codec = dimapCodec TableName getTableTxt codec

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

instance PG.FromCol TableType where
  fromCol bs = flip PG.fromColHelper bs
    $ PD.enum
    $ \case
      "BASE TABLE" -> Just TTBaseTable
      "VIEW" -> Just TTView
      "FOREIGN TABLE" -> Just TTForeignTable
      "LOCAL TEMPORARY" -> Just TTLocalTemporary
      _ -> Nothing

isView :: TableType -> Bool
isView TTView = True
isView _ = False

newtype ConstraintName = ConstraintName {getConstraintTxt :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, NFData, ToTxt, PG.ToPrepArg, PG.FromCol)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey)

instance IsIdentifier ConstraintName where
  toIdentifier (ConstraintName t) = Identifier t

instance ToSQL ConstraintName where
  toSQL = toSQL . toIdentifier

instance ToErrorValue ConstraintName where
  toErrorValue = ErrorValue.squote . getConstraintTxt

newtype FunctionName = FunctionName {getFunctionTxt :: Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON, PG.ToPrepArg, PG.FromCol, Hashable, NFData)

instance HasCodec FunctionName where
  codec = dimapCodec FunctionName getFunctionTxt codec

instance IsIdentifier FunctionName where
  toIdentifier (FunctionName t) = Identifier t

instance ToSQL FunctionName where
  toSQL = toSQL . toIdentifier

instance ToTxt FunctionName where
  toTxt = getFunctionTxt

instance ToErrorValue FunctionName where
  toErrorValue = ErrorValue.squote . getFunctionTxt

newtype SchemaName = SchemaName {getSchemaTxt :: Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON, Hashable, PG.ToPrepArg, PG.FromCol, NFData, IsString)

instance HasCodec SchemaName where
  codec = dimapCodec SchemaName getSchemaTxt codec

publicSchema :: SchemaName
publicSchema = SchemaName "public"

hdbCatalogSchema :: SchemaName
hdbCatalogSchema = SchemaName "hdb_catalog"

instance IsIdentifier SchemaName where
  toIdentifier (SchemaName t) = Identifier t

instance ToSQL SchemaName where
  toSQL = toSQL . toIdentifier

data QualifiedObject a = QualifiedObject
  { qSchema :: SchemaName,
    qName :: a
  }
  deriving (Show, Eq, Functor, Ord, Generic, Data)

instance (NFData a) => NFData (QualifiedObject a)

instance (HasCodec a, Typeable a) => HasCodec (QualifiedObject a) where
  codec = parseAlternative objCodec strCodec
    where
      objCodec =
        AC.object ("PostgresQualified_" <> typeableName @a)
          $ QualifiedObject
          <$> optionalFieldWithDefault' "schema" publicSchema
          AC..= qSchema
            <*> requiredField' "name"
          AC..= qName
      strCodec = QualifiedObject publicSchema <$> codec @a

instance (FromJSON a) => FromJSON (QualifiedObject a) where
  parseJSON v@(String _) =
    QualifiedObject publicSchema <$> parseJSON v
  parseJSON (Object o) =
    QualifiedObject
      <$> o
      .:? "schema"
      .!= publicSchema
      <*> o
      .: "name"
  parseJSON _ =
    fail "expecting a string/object for QualifiedObject"

instance (ToJSON a) => ToJSON (QualifiedObject a) where
  toJSON (QualifiedObject sn o) =
    object
      [ "schema" .= sn,
        "name" .= o
      ]

instance (ToJSON a, ToTxt a) => ToJSONKey (QualifiedObject a) where
  toJSONKey = ToJSONKeyText (K.fromText . qualifiedObjectToText) (text . qualifiedObjectToText)

instance (ToTxt a) => ToTxt (QualifiedObject a) where
  toTxt = qualifiedObjectToText

instance (ToTxt a) => ToErrorValue (QualifiedObject a) where
  toErrorValue (QualifiedObject sn o) = ErrorValue.squote $ getSchemaTxt sn <> "." <> toTxt o

instance (Hashable a) => Hashable (QualifiedObject a)

instance (ToSQL a) => ToSQL (QualifiedObject a) where
  toSQL (QualifiedObject sn o) =
    toSQL sn <> "." <> toSQL o

qualifiedObjectToText :: (ToTxt a) => QualifiedObject a -> Text
qualifiedObjectToText (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "." <> toTxt o

snakeCaseQualifiedObject :: (ToTxt a) => QualifiedObject a -> Text
snakeCaseQualifiedObject (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "_" <> toTxt o

getIdentifierQualifiedObject :: (ToTxt a) => QualifiedObject a -> Either QErr C.GQLNameIdentifier
getIdentifierQualifiedObject obj@(QualifiedObject sn o) = do
  let tLst =
        if sn == publicSchema
          then C.fromSnake $ toTxt o
          else C.fromSnake (getSchemaTxt sn) <> C.fromSnake (toTxt o)
      gqlIdents = do
        (pref, suffs) <- uncons tLst
        prefName <- G.mkName pref
        suffNames <- traverse G.mkNameSuffix suffs
        pure $ C.fromAutogeneratedTuple (prefName, suffNames)
  gqlIdents
    `onNothing` throw400
      ValidationFailed
      ( "cannot include "
          <> obj
          <<> " in the GraphQL schema because "
          <> C.toSnakeT tLst
          <<> " is not a valid GraphQL identifier"
      )

namingConventionSupport :: SupportedNamingCase
namingConventionSupport = AllConventions

qualifiedObjectToName :: (ToTxt a, MonadError QErr m) => QualifiedObject a -> m G.Name
qualifiedObjectToName objectName = do
  let textName = snakeCaseQualifiedObject objectName
  onNothing (G.mkName textName)
    $ throw400 ValidationFailed
    $ "cannot include "
    <> objectName
    <<> " in the GraphQL schema because "
    <> textName
    <<> " is not a valid GraphQL identifier"

-- | Represents a database table qualified with the schema name.
type QualifiedTable = QualifiedObject TableName

type QualifiedFunction = QualifiedObject FunctionName

newtype PGDescription = PGDescription {getPGDescription :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON, PG.FromCol, NFData, Hashable)

newtype PGCol = PGCol {getPGColTxt :: Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON, Hashable, PG.ToPrepArg, PG.FromCol, ToJSONKey, FromJSONKey, NFData, IsString)

instance HasCodec PGCol where
  codec = dimapCodec PGCol getPGColTxt codec

instance IsIdentifier PGCol where
  toIdentifier (PGCol t) = Identifier t

instance ToSQL PGCol where
  toSQL = toSQL . toIdentifier

instance ToTxt PGCol where
  toTxt = getPGColTxt

instance ToErrorValue PGCol where
  toErrorValue = ErrorValue.squote . getPGColTxt

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
  | PGArray PGScalarType
  | PGUnknown Text
  | PGCompositeScalar Text
  | PGEnumScalar Text
  deriving (Show, Eq, Ord, Generic, Data)

instance NFData PGScalarType

instance Hashable PGScalarType

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
  PGArray t -> pgScalarTypeToText t <> "[]"
  PGUnknown t -> t
  PGCompositeScalar t -> t
  PGEnumScalar t -> t

-- | Used for logical models validation.
instance HasCodec PGScalarType where
  codec =
    AC.bimapCodec
      decodePGScalarType
      pgScalarTypeToText
      AC.textCodec
      AC.<?> "Postgres Scalar Types"
    where
      -- We check that the types are one of the ones described in our docs
      -- <https://hasura.io/docs/latest/schema/postgres/postgresql-types>.
      decodePGScalarType :: Text -> Either String PGScalarType
      decodePGScalarType t =
        maybe
          (Left $ "Did not recognize scalar type '" <> T.unpack t <> "'")
          Right
          -- For tables, etc. We accept all types. For native queries we want to be a bit more conservatives.
          (lookup typ (pgScalarTranslations <> pgKnownUnknowns))
        where
          typ = massage t
          massage = stripPrecision . T.toLower
          stripPrecision usertype =
            fromMaybe usertype
              $ listToMaybe
              $ [ prectype
                  | prectype <- typesWithPrecision,
                    usertype =~ ("^" <> prectype <> " *\\([0-9]+\\)$")
                ]
              <> [ prectype
                   | prectype <- typesWithPrecision2,
                     usertype =~ ("^" <> prectype <> " *\\([0-9]+ *, *[0-9]+\\)$")
                 ]

          typesWithPrecision :: [Text]
          typesWithPrecision =
            [ "bit",
              "bit varying",
              "varbit",
              "char",
              "character",
              "varchar",
              "character varying"
            ]
          typesWithPrecision2 :: [Text]
          typesWithPrecision2 =
            [ "numeric",
              "decimal"
            ]
          -- Types we describe as PGUnknown internally.
          pgKnownUnknowns =
            map (,PGUnknown typ)
              $ [ "bit varying",
                  "bit",
                  "box",
                  "bytea",
                  "cidr",
                  "circle",
                  "inet",
                  "interval",
                  "line",
                  "lseg",
                  "macaddr",
                  "macaddr8",
                  "path",
                  "pg_lsn",
                  "point",
                  "polygon",
                  "serial2",
                  "serial4",
                  "smallserial",
                  "time without time zone",
                  "time",
                  "tsquery",
                  "tsvector",
                  "txid_snapshot",
                  "varbit",
                  "xml"
                ]

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

instance ToErrorValue PGScalarType where
  toErrorValue = ErrorValue.squote . pgScalarTypeToText

textToPGScalarType :: Text -> PGScalarType
textToPGScalarType =
  parse
  where
    lookupName txt =
      fromMaybe
        (PGUnknown txt)
        (lookup (T.toLower txt) pgScalarTranslations)
    parse = \case
      txt
        | T.takeEnd 2 txt == "[]" ->
            PGArray $ lookupName (T.dropEnd 2 txt)
      txt -> lookupName txt

-- Inlining this results in pretty terrible Core being generated by GHC.

{-# NOINLINE pgScalarTranslations #-}
pgScalarTranslations :: [(Text, PGScalarType)]
pgScalarTranslations =
  [ ("serial", PGSerial),
    ("bigserial", PGBigSerial),
    ("smallint", PGSmallInt),
    ("int2", PGSmallInt),
    ("int", PGInteger),
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
    pure
      $ case typeType of
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
  | PGKindUnknown Text
  deriving (Show, Eq, Ord, Generic)

instance NFData PGTypeKind

instance Hashable PGTypeKind

instance FromJSON PGTypeKind where
  parseJSON = withText "postgresTypeKind"
    $ \t -> pure $ case t of
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
  { _qptSchema :: SchemaName,
    _qptName :: PGScalarType,
    _qptType :: PGTypeKind
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData QualifiedPGType

instance Hashable QualifiedPGType

instance FromJSON QualifiedPGType where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON QualifiedPGType where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

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
  { rfiOid :: OID,
    rfiHasVariadic :: Bool,
    rfiFunctionType :: FunctionVolatility,
    rfiReturnTypeSchema :: SchemaName,
    rfiReturnTypeName :: PGScalarType,
    rfiReturnTypeType :: PGTypeKind,
    rfiReturnsSet :: Bool,
    rfiInputArgTypes :: [QualifiedPGType],
    rfiInputArgNames :: [FunctionArgName],
    rfiDefaultArgs :: Int,
    rfiReturnsTable :: Bool,
    rfiDescription :: Maybe PGDescription
  }
  deriving (Show, Eq, Generic)

instance NFData PGRawFunctionInfo

instance FromJSON PGRawFunctionInfo where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON PGRawFunctionInfo where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

mkScalarTypeName :: (MonadError QErr m) => NamingCase -> PGScalarType -> m G.Name
mkScalarTypeName tCase typ = applyTypeNameCaseCust tCase <$> go typ
  where
    go :: (MonadError QErr m) => PGScalarType -> m G.Name
    go PGInteger = pure GName._Int
    go PGBoolean = pure GName._Boolean
    go PGFloat = pure GName._Float
    go PGText = pure GName._String
    go PGVarchar = pure GName._String
    go (PGCompositeScalar compositeScalarType) =
      -- When the function argument is a row type argument
      -- then it's possible that there can be an object type
      -- with the table name depending upon whether the table
      -- is tracked or not. As a result, we get a conflict between
      -- both these types (scalar and object type with same name).
      -- To avoid this, we suffix the table name with `_scalar`
      -- and create a new scalar type
      (<> Name.__scalar)
        <$> G.mkName compositeScalarType
        `onNothing` throw400
          ValidationFailed
          ( "cannot use SQL type "
              <> compositeScalarType
              <<> " in the GraphQL schema because its name is not a "
              <> "valid GraphQL identifier"
          )
    go (PGArray innerScalarType) =
      -- previous to Postgres array changes, an array of a type was called `_thing`, and this made
      -- nice GraphQL names, so maintaining this
      G.mkName ("_" <> pgScalarTypeToText innerScalarType)
        `onNothing` throw400
          ValidationFailed
          ( "cannot use SQL type "
              <> innerScalarType
              <<> " in the GraphQL schema because its name is not a "
              <> "valid GraphQL identifier"
          )
    go scalarType =
      G.mkName (pgScalarTypeToText scalarType)
        `onNothing` throw400
          ValidationFailed
          ( "cannot use SQL type "
              <> scalarType
              <<> " in the GraphQL schema because its name is not a "
              <> "valid GraphQL identifier"
          )

instance IsIdentifier RelName where
  toIdentifier rn = Identifier $ relNameToTxt rn

instance IsIdentifier FieldName where
  toIdentifier (FieldName f) = Identifier f

pgTypeOid :: PGScalarType -> PQ.Oid
pgTypeOid = \case
  PGSmallInt -> PTI.int2
  PGInteger -> PTI.int4
  PGBigInt -> PTI.int8
  PGSerial -> PTI.int4
  PGBigSerial -> PTI.int8
  PGFloat -> PTI.float4
  PGDouble -> PTI.float8
  PGNumeric -> PTI.numeric
  PGMoney -> PTI.numeric
  PGBoolean -> PTI.bool
  PGChar -> PTI.char
  PGVarchar -> PTI.varchar
  PGText -> PTI.text
  PGCitext -> PTI.text -- Explict type cast to citext needed, See also Note [Type casting prepared params]
  PGDate -> PTI.date
  PGTimeStamp -> PTI.timestamp
  PGTimeStampTZ -> PTI.timestamptz
  PGTimeTZ -> PTI.timetz
  PGJSON -> PTI.json
  PGJSONB -> PTI.jsonb
  PGGeometry -> PTI.text -- we are using the ST_GeomFromGeoJSON($i) instead of $i
  PGGeography -> PTI.text
  PGRaster -> PTI.text -- we are using the ST_RastFromHexWKB($i) instead of $i
  PGUUID -> PTI.uuid
  PGLtree -> PTI.text
  PGLquery -> PTI.text
  PGLtxtquery -> PTI.text
  PGUnknown _ -> PTI.auto
  PGCompositeScalar _ -> PTI.auto
  PGEnumScalar _ -> PTI.auto
  PGArray _ -> PTI.auto

--  Extra metadata for vanilla Postgres
data PGExtraTableMetadata = PGExtraTableMetadata
  { _petmTableType :: SourceTableType
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

instance ToJSON PGExtraTableMetadata where
  toJSON PGExtraTableMetadata {..} =
    object ["table_type" .= _petmTableType]

instance FromJSON PGExtraTableMetadata where
  parseJSON = withObject "PGExtraTableMetadata" \obj -> do
    _petmTableType <- obj .: "table_type"
    pure PGExtraTableMetadata {..}
