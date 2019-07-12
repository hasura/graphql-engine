module Hasura.SQL.Types where

import qualified Database.PG.Query          as Q
import qualified Database.PG.Query.PTI      as PTI

import           Hasura.Prelude

import           Data.Aeson
import           Data.Aeson.Encoding        (text)
import           Data.String                (fromString)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Text.Extended         as T
import qualified Database.PostgreSQL.LibPQ  as PQ
import qualified PostgreSQL.Binary.Decoding as PD
import qualified Text.Builder               as TB

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
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, Semigroup)

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

infixr 6 <>>
(<>>) :: (DQuote a) => T.Text -> a -> T.Text
(<>>) lTxt a =
  lTxt <> T.dquote (dquoteTxt a)
{-# INLINE (<>>) #-}

infixr 6 <<>
(<<>) :: (DQuote a) => a -> T.Text -> T.Text
(<<>) a rTxt =
  T.dquote (dquoteTxt a) <> rTxt
{-# INLINE (<<>) #-}

instance (ToSQL a) => ToSQL (Maybe a) where
  toSQL (Just a) = toSQL a
  toSQL Nothing  = mempty

class ToTxt a where
  toTxt :: a -> T.Text

newtype TableName
  = TableName { getTableTxt :: T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift)

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
  deriving (Show, Eq, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, Lift)

instance IsIden ConstraintName where
  toIden (ConstraintName t) = Iden t

instance ToSQL ConstraintName where
  toSQL = toSQL . toIden

newtype FunctionName
  = FunctionName { getFunctionTxt :: T.Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, Lift)

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
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift)

publicSchema :: SchemaName
publicSchema = SchemaName "public"

hdbViewsSchema :: SchemaName
hdbViewsSchema = SchemaName "hdb_views"

instance IsIden SchemaName where
  toIden (SchemaName t) = Iden t

instance ToSQL SchemaName where
  toSQL = toSQL . toIden

data QualifiedObject a
  = QualifiedObject
  { qSchema :: !SchemaName
  , qName   :: !a
  } deriving (Show, Eq, Functor, Ord, Generic, Lift)

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
  toJSON (QualifiedObject (SchemaName "public") o) = toJSON o
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

snakeCaseTable :: QualifiedObject TableName -> T.Text
snakeCaseTable (QualifiedObject sn tn) =
  getSchemaTxt sn <> "_" <> getTableTxt tn

type QualifiedFunction = QualifiedObject FunctionName

newtype PGCol
  = PGCol { getPGColTxt :: T.Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, ToJSONKey, FromJSONKey, Lift)

instance IsIden PGCol where
  toIden (PGCol t) = Iden t

instance ToSQL PGCol where
  toSQL = toSQL . toIden

instance DQuote PGCol where
  dquoteTxt (PGCol t) = t

showPGCols :: (Foldable t) => t PGCol -> T.Text
showPGCols cols =
  T.intercalate ", " $ map (T.dquote . getPGColTxt) $ toList cols

data PGColType
  = PGSmallInt
  | PGInteger
  | PGBigInt
  | PGSerial
  | PGBigSerial
  | PGFloat
  | PGDouble
  | PGNumeric
  | PGBoolean
  | PGChar
  | PGVarchar
  | PGText
  | PGDate
  | PGTimeStampTZ
  | PGTimeTZ
  | PGJSON
  | PGJSONB
  | PGGeometry
  | PGGeography
  | PGUnknown !T.Text
  deriving (Eq, Lift, Generic)

instance Hashable PGColType

instance Show PGColType where
  show PGSmallInt    = "smallint"
  show PGInteger     = "integer"
  show PGBigInt      = "bigint"
  show PGSerial      = "serial"
  show PGBigSerial   = "bigserial"
  show PGFloat       = "real"
  show PGDouble      = "float8"
  show PGNumeric     = "numeric"
  show PGBoolean     = "boolean"
  show PGChar        = "character"
  show PGVarchar     = "varchar"
  show PGText        = "text"
  show PGDate        = "date"
  show PGTimeStampTZ = "timestamptz"
  show PGTimeTZ      = "timetz"
  show PGJSON        = "json"
  show PGJSONB       = "jsonb"
  show PGGeometry    = "geometry"
  show PGGeography   = "geography"
  show (PGUnknown t) = T.unpack t

instance ToJSON PGColType where
  toJSON pct = String $ T.pack $ show pct

instance ToSQL PGColType where
  toSQL pct = fromString $ show pct


txtToPgColTy :: Text -> PGColType
txtToPgColTy t = case t of
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

  "boolean"                  -> PGBoolean
  "bool"                     -> PGBoolean

  "character"                -> PGChar

  "varchar"                  -> PGVarchar
  "character varying"        -> PGVarchar

  "text"                     -> PGText
  "citext"                   -> PGText

  "date"                     -> PGDate

  "timestamptz"              -> PGTimeStampTZ
  "timestamp with time zone" -> PGTimeStampTZ

  "timetz"                   -> PGTimeTZ
  "time with time zone"      -> PGTimeTZ

  "json"                     -> PGJSON
  "jsonb"                    -> PGJSONB

  "geometry"                 -> PGGeometry
  "geography"                -> PGGeography
  _                          -> PGUnknown t


instance FromJSON PGColType where
  parseJSON (String t) = return $ txtToPgColTy t
  parseJSON _          = fail "Expecting a string for PGColType"

pgTypeOid :: PGColType -> PQ.Oid
pgTypeOid PGSmallInt    = PTI.int2
pgTypeOid PGInteger     = PTI.int4
pgTypeOid PGBigInt      = PTI.int8
pgTypeOid PGSerial      = PTI.int4
pgTypeOid PGBigSerial   = PTI.int8
pgTypeOid PGFloat       = PTI.float4
pgTypeOid PGDouble      = PTI.float8
pgTypeOid PGNumeric     = PTI.numeric
pgTypeOid PGBoolean     = PTI.bool
pgTypeOid PGChar        = PTI.char
pgTypeOid PGVarchar     = PTI.varchar
pgTypeOid PGText        = PTI.text
pgTypeOid PGDate        = PTI.date
pgTypeOid PGTimeStampTZ = PTI.timestamptz
pgTypeOid PGTimeTZ      = PTI.timetz
pgTypeOid PGJSON        = PTI.json
pgTypeOid PGJSONB       = PTI.jsonb
-- we are using the ST_GeomFromGeoJSON($i) instead of $i
pgTypeOid PGGeometry    = PTI.text
pgTypeOid PGGeography   = PTI.text
pgTypeOid (PGUnknown _) = PTI.auto

-- TODO: This is incorrect modelling as PGColType
-- will capture anything under PGUnknown
-- This should be fixed when support for
-- all types is merged.

data PgType
  = PgTypeSimple !PGColType
  | PgTypeArray !PGColType
  deriving (Eq)

instance Show PgType where
  show = \case
    PgTypeSimple ty -> show ty
    -- typename array is an sql standard way
    -- of declaring types
    PgTypeArray ty -> show ty <> " array"

instance ToJSON PgType where
  toJSON = toJSON . show

isIntegerType :: PGColType -> Bool
isIntegerType PGInteger  = True
isIntegerType PGSmallInt = True
isIntegerType PGBigInt   = True
isIntegerType _          = False

isNumType :: PGColType -> Bool
isNumType PGFloat   = True
isNumType PGDouble  = True
isNumType PGNumeric = True
isNumType ty        = isIntegerType ty

isJSONBType :: PGColType -> Bool
isJSONBType PGJSONB = True
isJSONBType _       = False

isComparableType :: PGColType -> Bool
isComparableType PGJSON        = False
isComparableType PGJSONB       = False
isComparableType PGGeometry    = False
isComparableType PGGeography   = False
isComparableType PGBoolean     = False
isComparableType (PGUnknown _) = False
isComparableType _             = True

isBigNum :: PGColType -> Bool
isBigNum = \case
  PGBigInt    -> True
  PGBigSerial -> True
  PGNumeric   -> True
  PGDouble    -> True
  _           -> False

isGeoType :: PGColType -> Bool
isGeoType = \case
  PGGeometry  -> True
  PGGeography -> True
  _           -> False
