module Hasura.SQL.Types
  ( ToSQL(..)
  , toSQLTxt

  , (<+>)
  , (<<>)
  , (<>>)

  , pgFmtLit
  , pgFmtIden
  , isView

  , QualifiedTable
  , QualifiedFunction

  , PGDescription(..)

  , PGCol
  , unsafePGCol
  , getPGColTxt
  , showPGCols

  , isIntegerType
  , isNumType
  , stringTypes
  , isStringType
  , isJSONType
  , isComparableType
  , isBigNum
  , geoTypes
  , isGeoType

  , DQuote(..)
  , dquote
  , dquoteList

  , IsIden(..)
  , Iden(..)

  , ToTxt(..)

  , SchemaName(..)
  , publicSchema
  , hdbCatalogSchema

  , TableName(..)
  , FunctionName(..)
  , ConstraintName(..)

  , QualifiedObject(..)
  , qualObjectToText
  , snakeCaseQualObject

  , PGScalarType(..)
  , WithScalarType(..)
  , PGType(..)
  , textToPGScalarType
  , pgTypeOid

  , PGTypeKind(..)
  , QualifiedPGType(..)
  , isBaseType
  , typeToTable
  )
where

import qualified Database.PG.Query             as Q
import qualified Database.PG.Query.PTI         as PTI

import           Hasura.Prelude

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Encoding           (text)
import           Data.Aeson.TH
import           Data.Aeson.Types              (toJSONKeyText)
import           Instances.TH.Lift             ()
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.Text.Extended            as T
import qualified Database.PostgreSQL.LibPQ     as PQ
import qualified Language.GraphQL.Draft.Syntax as G
import qualified PostgreSQL.Binary.Decoding    as PD
import qualified Text.Builder                  as TB

import           Hasura.Incremental            (Cacheable)

class ToSQL a where
  toSQL :: a -> TB.Builder

instance ToSQL TB.Builder where
  toSQL x = x

toSQLTxt :: (ToSQL a) => a -> T.Text
toSQLTxt = TB.run . toSQL

infixr 6 <+>
(<+>) :: (ToSQL a) => T.Text -> [a] -> TB.Builder
(<+>) _ [] = mempty
(<+>) kat (x:xs) =
  toSQL x <> mconcat [ TB.text kat <> toSQL x' | x' <- xs ]
{-# INLINE (<+>) #-}

newtype Iden
  = Iden { getIdenTxt :: T.Text }
  deriving (Show, Eq, NFData, FromJSON, ToJSON, Hashable, Semigroup, Data, Cacheable)

instance ToSQL Iden where
  toSQL (Iden t) =
    TB.text $ pgFmtIden t

class IsIden a where
  toIden :: a -> Iden

instance IsIden Iden where
  toIden = id

class DQuote a where
  dquoteTxt :: a -> T.Text

instance DQuote T.Text where
  dquoteTxt = id
  {-# INLINE dquoteTxt #-}

deriving instance DQuote G.NamedType
deriving instance DQuote G.Name
deriving instance DQuote G.EnumValue

dquote :: (DQuote a) => a -> T.Text
dquote = T.dquote . dquoteTxt
{-# INLINE dquote #-}

dquoteList :: (DQuote a, Foldable t) => t a -> T.Text
dquoteList = T.intercalate ", " . map dquote . toList
{-# INLINE dquoteList #-}

infixr 6 <>>
(<>>) :: (DQuote a) => T.Text -> a -> T.Text
(<>>) lTxt a = lTxt <> dquote a
{-# INLINE (<>>) #-}

infixr 6 <<>
(<<>) :: (DQuote a) => a -> T.Text -> T.Text
(<<>) a rTxt = dquote a <> rTxt
{-# INLINE (<<>) #-}

pgFmtIden :: T.Text -> T.Text
pgFmtIden x =
  "\"" <> T.replace "\"" "\"\"" (trimNullChars x) <> "\""

pgFmtLit :: T.Text -> T.Text
pgFmtLit x =
 let trimmed = trimNullChars x
     escaped = "'" <> T.replace "'" "''" trimmed <> "'"
     slashed = T.replace "\\" "\\\\" escaped in
 if "\\" `T.isInfixOf` escaped
   then "E" <> slashed
   else slashed

trimNullChars :: T.Text -> T.Text
trimNullChars = T.takeWhile (/= '\x0')

instance (ToSQL a) => ToSQL (Maybe a) where
  toSQL (Just a) = toSQL a
  toSQL Nothing  = mempty

class ToTxt a where
  toTxt :: a -> T.Text

newtype TableName
  = TableName { getTableTxt :: T.Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift, Data
           , Generic, Arbitrary, NFData, Cacheable, IsString )

instance IsIden TableName where
  toIden (TableName t) = Iden t

instance DQuote TableName where
  dquoteTxt (TableName t) = t

instance ToSQL TableName where
  toSQL = toSQL . toIden

instance ToTxt TableName where
  toTxt = getTableTxt

data TableType
  = TTBaseTable
  | TTView
  | TTForeignTable
  | TTLocalTemporary
  deriving (Eq)

tableTyToTxt :: TableType -> T.Text
tableTyToTxt TTBaseTable      = "BASE TABLE"
tableTyToTxt TTView           = "VIEW"
tableTyToTxt TTForeignTable   = "FOREIGN TABLE"
tableTyToTxt TTLocalTemporary = "LOCAL TEMPORARY"

instance Show TableType where
  show = T.unpack . tableTyToTxt

instance Q.FromCol TableType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "BASE TABLE"      -> Just TTBaseTable
    "VIEW"            -> Just TTView
    "FOREIGN TABLE"   -> Just TTForeignTable
    "LOCAL TEMPORARY" -> Just TTLocalTemporary
    _                 -> Nothing

isView :: TableType -> Bool
isView TTView = True
isView _      = False

newtype ConstraintName
  = ConstraintName { getConstraintTxt :: T.Text }
  deriving (Show, Eq, DQuote, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, Lift, NFData, Cacheable)

instance IsIden ConstraintName where
  toIden (ConstraintName t) = Iden t

instance ToSQL ConstraintName where
  toSQL = toSQL . toIden

newtype FunctionName
  = FunctionName { getFunctionTxt :: T.Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, Lift, Data, Generic, Arbitrary, NFData, Cacheable)

instance IsIden FunctionName where
  toIden (FunctionName t) = Iden t

instance DQuote FunctionName where
  dquoteTxt (FunctionName t) = t

instance ToSQL FunctionName where
  toSQL = toSQL . toIden

instance ToTxt FunctionName where
  toTxt = getFunctionTxt

newtype SchemaName
  = SchemaName { getSchemaTxt :: T.Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift, Data, Generic
           , Arbitrary, NFData, Cacheable, IsString )

publicSchema :: SchemaName
publicSchema = SchemaName "public"

hdbCatalogSchema :: SchemaName
hdbCatalogSchema = SchemaName "hdb_catalog"

instance IsIden SchemaName where
  toIden (SchemaName t) = Iden t

instance ToSQL SchemaName where
  toSQL = toSQL . toIden

data QualifiedObject a
  = QualifiedObject
  { qSchema :: !SchemaName
  , qName   :: !a
  } deriving (Show, Eq, Functor, Ord, Generic, Lift, Data)
instance (NFData a) => NFData (QualifiedObject a)
instance (Cacheable a) => Cacheable (QualifiedObject a)

instance (FromJSON a) => FromJSON (QualifiedObject a) where
  parseJSON v@(String _) =
    QualifiedObject publicSchema <$> parseJSON v
  parseJSON (Object o) =
    QualifiedObject <$>
    o .:? "schema" .!= publicSchema <*>
    o .: "name"
  parseJSON _ =
    fail "expecting a string/object for QualifiedObject"

instance (ToJSON a) => ToJSON (QualifiedObject a) where
  toJSON (QualifiedObject sn o) =
    object [ "schema" .= sn
           , "name"  .= o
           ]

instance (ToJSON a, ToTxt a) => ToJSONKey (QualifiedObject a) where
  toJSONKey = ToJSONKeyText qualObjectToText (text . qualObjectToText)

instance (ToTxt a) => DQuote (QualifiedObject a) where
  dquoteTxt = qualObjectToText

instance (Hashable a) => Hashable (QualifiedObject a)

instance (ToSQL a) => ToSQL (QualifiedObject a) where
  toSQL (QualifiedObject sn o) =
    toSQL sn <> "." <> toSQL o

qualObjectToText :: ToTxt a => QualifiedObject a -> T.Text
qualObjectToText (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "." <> toTxt o

snakeCaseQualObject :: ToTxt a => QualifiedObject a -> T.Text
snakeCaseQualObject (QualifiedObject sn o)
  | sn == publicSchema = toTxt o
  | otherwise = getSchemaTxt sn <> "_" <> toTxt o

type QualifiedTable = QualifiedObject TableName

type QualifiedFunction = QualifiedObject FunctionName

newtype PGDescription
  = PGDescription { getPGDescription :: T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, Q.FromCol, NFData, Cacheable, Hashable)

newtype PGCol
  = PGCol { getPGColTxt :: T.Text }
  deriving ( Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, ToJSONKey
           , FromJSONKey, Lift, Data, Generic, Arbitrary, NFData, Cacheable, IsString )

instance IsIden PGCol where
  toIden (PGCol t) = Iden t

instance ToSQL PGCol where
  toSQL = toSQL . toIden

instance DQuote PGCol where
  dquoteTxt (PGCol t) = t

unsafePGCol :: Text -> PGCol
unsafePGCol = PGCol

showPGCols :: (Foldable t) => t PGCol -> T.Text
showPGCols cols =
  T.intercalate ", " $ map (T.dquote . getPGColTxt) $ toList cols

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
  | PGUnknown !T.Text
  deriving (Show, Eq, Lift, Generic, Data)
instance NFData PGScalarType
instance Hashable PGScalarType
instance Cacheable PGScalarType

instance ToSQL PGScalarType where
  toSQL = \case
    PGSmallInt    -> "smallint"
    PGInteger     -> "integer"
    PGBigInt      -> "bigint"
    PGSerial      -> "serial"
    PGBigSerial   -> "bigserial"
    PGFloat       -> "real"
    PGDouble      -> "float8"
    PGNumeric     -> "numeric"
    PGMoney       -> "money"
    PGBoolean     -> "boolean"
    PGChar        -> "character"
    PGVarchar     -> "varchar"
    PGText        -> "text"
    PGCitext      -> "citext"
    PGDate        -> "date"
    PGTimeStamp   -> "timestamp"
    PGTimeStampTZ -> "timestamptz"
    PGTimeTZ      -> "timetz"
    PGJSON        -> "json"
    PGJSONB       -> "jsonb"
    PGGeometry    -> "geometry"
    PGGeography   -> "geography"
    PGRaster      -> "raster"
    PGUUID        -> "uuid"
    PGUnknown t   -> TB.text t

instance ToJSON PGScalarType where
  toJSON = String . toSQLTxt

instance ToJSONKey PGScalarType where
  toJSONKey = toJSONKeyText toSQLTxt

instance DQuote PGScalarType where
  dquoteTxt = toSQLTxt

textToPGScalarType :: Text -> PGScalarType
textToPGScalarType t = case t of
  "serial"                   -> PGSerial
  "bigserial"                -> PGBigSerial

  "smallint"                 -> PGSmallInt
  "int2"                     -> PGSmallInt

  "integer"                  -> PGInteger
  "int4"                     -> PGInteger

  "bigint"                   -> PGBigInt
  "int8"                     -> PGBigInt

  "real"                     -> PGFloat
  "float4"                   -> PGFloat

  "double precision"         -> PGDouble
  "float8"                   -> PGDouble

  "numeric"                  -> PGNumeric
  "decimal"                  -> PGNumeric

  "money"                    -> PGMoney

  "boolean"                  -> PGBoolean
  "bool"                     -> PGBoolean

  "character"                -> PGChar

  "varchar"                  -> PGVarchar
  "character varying"        -> PGVarchar

  "text"                     -> PGText
  "citext"                   -> PGCitext

  "date"                     -> PGDate

  "timestamp"                -> PGTimeStamp
  "timestamp without time zone" -> PGTimeStamp

  "timestamptz"              -> PGTimeStampTZ
  "timestamp with time zone" -> PGTimeStampTZ

  "timetz"                   -> PGTimeTZ
  "time with time zone"      -> PGTimeTZ

  "json"                     -> PGJSON
  "jsonb"                    -> PGJSONB

  "geometry"                 -> PGGeometry
  "geography"                -> PGGeography

  "raster"                   -> PGRaster
  "uuid"                     -> PGUUID
  _                          -> PGUnknown t


instance FromJSON PGScalarType where
  parseJSON (String t) = return $ textToPGScalarType t
  parseJSON _          = fail "Expecting a string for PGScalarType"

pgTypeOid :: PGScalarType -> PQ.Oid
pgTypeOid PGSmallInt    = PTI.int2
pgTypeOid PGInteger     = PTI.int4
pgTypeOid PGBigInt      = PTI.int8
pgTypeOid PGSerial      = PTI.int4
pgTypeOid PGBigSerial   = PTI.int8
pgTypeOid PGFloat       = PTI.float4
pgTypeOid PGDouble      = PTI.float8
pgTypeOid PGNumeric     = PTI.numeric
pgTypeOid PGMoney       = PTI.numeric
pgTypeOid PGBoolean     = PTI.bool
pgTypeOid PGChar        = PTI.char
pgTypeOid PGVarchar     = PTI.varchar
pgTypeOid PGText        = PTI.text
pgTypeOid PGCitext      = PTI.text -- Explict type cast to citext needed, See also Note [Type casting prepared params]
pgTypeOid PGDate        = PTI.date
pgTypeOid PGTimeStamp   = PTI.timestamp
pgTypeOid PGTimeStampTZ = PTI.timestamptz
pgTypeOid PGTimeTZ      = PTI.timetz
pgTypeOid PGJSON        = PTI.json
pgTypeOid PGJSONB       = PTI.jsonb
pgTypeOid PGGeometry    = PTI.text -- we are using the ST_GeomFromGeoJSON($i) instead of $i
pgTypeOid PGGeography   = PTI.text
pgTypeOid PGRaster      = PTI.text -- we are using the ST_RastFromHexWKB($i) instead of $i
pgTypeOid PGUUID        = PTI.uuid
pgTypeOid (PGUnknown _) = PTI.auto

isIntegerType :: PGScalarType -> Bool
isIntegerType PGInteger  = True
isIntegerType PGSmallInt = True
isIntegerType PGBigInt   = True
isIntegerType _          = False

isNumType :: PGScalarType -> Bool
isNumType PGFloat   = True
isNumType PGDouble  = True
isNumType PGNumeric = True
isNumType PGMoney   = True
isNumType ty        = isIntegerType ty

stringTypes :: [PGScalarType]
stringTypes = [PGVarchar, PGText, PGCitext]

isStringType :: PGScalarType -> Bool
isStringType = (`elem` stringTypes)

jsonTypes :: [PGScalarType]
jsonTypes = [PGJSON, PGJSONB]

isJSONType :: PGScalarType -> Bool
isJSONType = (`elem` jsonTypes)

isComparableType :: PGScalarType -> Bool
isComparableType PGJSON        = False
isComparableType PGJSONB       = False
isComparableType PGGeometry    = False
isComparableType PGGeography   = False
isComparableType PGBoolean     = False
isComparableType (PGUnknown _) = False
isComparableType _             = True

isBigNum :: PGScalarType -> Bool
isBigNum = \case
  PGBigInt    -> True
  PGBigSerial -> True
  PGNumeric   -> True
  PGDouble    -> True
  PGMoney     -> True
  _           -> False

geoTypes :: [PGScalarType]
geoTypes = [PGGeometry, PGGeography]

isGeoType :: PGScalarType -> Bool
isGeoType = (`elem` geoTypes)

data WithScalarType a
  = WithScalarType
  { pstType  :: !PGScalarType
  , pstValue :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | The type of all Postgres types (i.e. scalars and arrays). This type is parameterized so that
-- we can have both @'PGType' 'PGScalarType'@ and @'PGType' 'Hasura.RQL.Types.PGColumnType'@, for
-- when we care about the distinction made by 'Hasura.RQL.Types.PGColumnType'. If we ever change
-- 'Hasura.RQL.Types.PGColumnType' to handle arrays, not just scalars, then the parameterization can
-- go away.
--
-- TODO: This is incorrect modeling, as 'PGScalarType' will capture anything (under 'PGUnknown').
-- This should be fixed when support for all types is merged.
data PGType a
  = PGTypeScalar !a
  | PGTypeArray !a
  deriving (Show, Eq, Generic, Data, Functor)
instance (NFData a) => NFData (PGType a)
instance (Cacheable a) => Cacheable (PGType a)
$(deriveJSON defaultOptions{constructorTagModifier = drop 6} ''PGType)

instance (ToSQL a) => ToSQL (PGType a) where
  toSQL = \case
    PGTypeScalar ty -> toSQL ty
    -- typename array is an sql standard way of declaring types
    PGTypeArray ty -> toSQL ty <> " array"

data PGTypeKind
  = PGKindBase
  | PGKindComposite
  | PGKindDomain
  | PGKindEnum
  | PGKindRange
  | PGKindPseudo
  | PGKindUnknown !T.Text
  deriving (Show, Eq, Generic)
instance NFData PGTypeKind
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
      _   -> PGKindUnknown t

instance ToJSON PGTypeKind where
  toJSON = \case
    PGKindBase      -> "b"
    PGKindComposite -> "c"
    PGKindDomain    -> "d"
    PGKindEnum      -> "e"
    PGKindRange     -> "r"
    PGKindPseudo    -> "p"
    PGKindUnknown t -> String t

data QualifiedPGType
  = QualifiedPGType
  { _qptSchema :: !SchemaName
  , _qptName   :: !PGScalarType
  , _qptType   :: !PGTypeKind
  } deriving (Show, Eq, Generic)
instance NFData QualifiedPGType
instance Cacheable QualifiedPGType
$(deriveJSON (aesonDrop 4 snakeCase) ''QualifiedPGType)

isBaseType :: QualifiedPGType -> Bool
isBaseType (QualifiedPGType _ n ty) =
  notUnknown && (ty == PGKindBase)
  where
    notUnknown = case n of
      PGUnknown _ -> False
      _           -> True

typeToTable :: QualifiedPGType -> QualifiedTable
typeToTable (QualifiedPGType sch n _) =
  QualifiedObject sch $ TableName $ toSQLTxt n
