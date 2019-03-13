{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module Hasura.SQL.Types where

import qualified Database.PG.Query          as Q
import qualified Database.PG.Query.PTI      as PTI

import           Hasura.Prelude
import           Hasura.RQL.Instances       ()

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Casing
import           Data.Aeson.Encoding        (text)
import           Data.String                (fromString)
import           Instances.TH.Lift          ()
import qualified Language.Haskell.TH.Syntax as TH

import qualified Data.HashMap.Strict.InsOrd as OMap
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
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, TH.Lift)

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
  deriving (Show, Eq, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, TH.Lift)

instance IsIden ConstraintName where
  toIden (ConstraintName t) = Iden t

instance ToSQL ConstraintName where
  toSQL = toSQL . toIden

newtype FunctionName
  = FunctionName { getFunctionTxt :: T.Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Hashable, TH.Lift)

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
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, TH.Lift)

publicSchema :: SchemaName
publicSchema = SchemaName "public"

catalogSchema :: SchemaName
catalogSchema = SchemaName "catalog"

instance IsIden SchemaName where
  toIden (SchemaName t) = Iden t

instance ToSQL SchemaName where
  toSQL = toSQL . toIden

data QualifiedObject a
  = QualifiedObject
  { qSchema :: !SchemaName
  , qName   :: !a
  } deriving (Show, Eq, Ord, Generic, TH.Lift)

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
  | sn == catalogSchema = toTxt o
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
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, ToJSONKey, FromJSONKey, TH.Lift)

instance IsIden PGCol where
  toIden (PGCol t) = Iden t

instance ToSQL PGCol where
  toSQL = toSQL . toIden

instance DQuote PGCol where
  dquoteTxt (PGCol t) = t

showPGCols :: (Foldable t) => t PGCol -> T.Text
showPGCols cols =
  T.intercalate ", " $ map (T.dquote . getPGColTxt) $ toList cols

newtype AnnType
  = AnnType {unAnnType :: T.Text}
  deriving (Show, Eq, Generic, TH.Lift, ToJSON, FromJSON)

instance Hashable AnnType

intType :: AnnType
intType = AnnType "int"

textType :: AnnType
textType = AnnType "text"

textArrType :: AnnType
textArrType = AnnType "text[]"

jsonType :: AnnType
jsonType = AnnType "json"

jsonArrType :: AnnType
jsonArrType = AnnType "json[]"

jsonbType :: AnnType
jsonbType = AnnType "jsonb"

newtype PGTyFldName = PGTyFldName { getTyFldText :: T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable, Q.ToPrepArg, Q.FromCol, TH.Lift)


newtype EnumVal = EnumVal { getEnumVal :: T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, TH.Lift)

newtype PGTyName = PGTyName { getTyText :: T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, TH.Lift)

instance ToTxt PGTyName where
  toTxt = getTyText

type QualifiedType = QualifiedObject PGTyName

data PGBaseColType
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
  deriving (Eq, TH.Lift, Generic)

instance Hashable PGBaseColType

instance ToJSON PGBaseColType where
  toJSON pct = String $ T.pack $ show pct

instance ToSQL PGBaseColType where
  toSQL pct = fromString $ show pct

txtToPgBaseColTy :: Text -> PGBaseColType
txtToPgBaseColTy t = case t of
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

instance FromJSON PGBaseColType where
  parseJSON (String t) = return $ txtToPgBaseColTy t
  parseJSON _          = fail "Expecting a string for PGBaseColType"

instance Show PGBaseColType where
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

data PGColTyDetails
  = PGTyComposite !(OMap.InsOrdHashMap PGTyFldName PGColType)
  | PGTyArray  !PGColType
  | PGTyDomain !PGColType
  | PGTyBase   !PGBaseColType
  | PGTyEnum   ![EnumVal]
  | PGTyRange
  | PGTyPseudo
  deriving (Show, Eq, TH.Lift, Generic)

instance Hashable PGColTyDetails

getArrayBaseTy :: PGColType -> Maybe PGColType
getArrayBaseTy x = case pgColTyDetails x of
  PGTyArray b -> case pgColTyDetails b of
    PGTyArray{} -> getArrayBaseTy b
    _ -> Just b
  _ -> Nothing

getPGTyArrDim :: PGColType -> Int
getPGTyArrDim colTy = case pgColTyDetails  colTy of
  PGTyArray bTy  -> 1 + getPGTyArrDim bTy
  PGTyDomain bTy -> getPGTyArrDim bTy
  _              -> 0

data PGColType
  = PGColType
  { pgColTyName    :: !QualifiedType
  , pgColTySqlName :: !AnnType
  , pgColTyOid     :: !PQ.Oid
  , pgColTyDetails :: !PGColTyDetails
  } deriving (Show, Eq, TH.Lift, Generic)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 4
                 , sumEncoding = TaggedObject "type" "detail"
                 }
  ''PGColTyDetails)
$(deriveJSON (aesonDrop 7 camelCase) ''PGColType)

baseTy :: PGBaseColType -> PGColType
baseTy b = PGColType qualfdType (AnnType name) (pgTypeOid b)$ PGTyBase b
  where
    qualfdType = QualifiedObject (SchemaName "pg_catalog") (PGTyName name)
    name = T.pack $ show b

instance Hashable PGColType

pgTypeOid :: PGBaseColType -> PQ.Oid
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


isIntegerType :: PGColType -> Bool
isIntegerType = onBaseUDT False isIntegerType'

isNumType :: PGColType -> Bool
isNumType = onBaseUDT False isNumType'

isJSONBType :: PGColType -> Bool
isJSONBType = onBaseUDT False isJSONBType'

pattern PGGeomTy :: QualifiedType -> AnnType -> PQ.Oid -> PGColType
pattern PGGeomTy a b c = PGColType a b c (PGTyBase PGGeometry)

pattern PGJSONTy :: QualifiedType -> AnnType -> PQ.Oid -> PGColType
pattern PGJSONTy a b c = PGColType a b c (PGTyBase PGJSON)

--any numeric, string, date/time, network, or enum type, or arrays of these types
isComparableType :: PGColType -> Bool
isComparableType t = case pgColTyDetails t of
  PGTyArray a  ->  isComparableType a
  PGTyDomain a -> isComparableType a
  PGTyBase   b -> isComparableType' b
  PGTyEnum{}   -> True
  _            -> False

-- Apply the function if the underlying data type is a base data type. Otherwise return the default value
onBaseUDT :: a -> (PGBaseColType -> a) -> PGColType -> a
onBaseUDT def f t = case pgColTyDetails t of
  PGTyBase   b -> f b
  PGTyDomain a -> onBaseUDT def f a
  _            -> def

isIntegerType' :: PGBaseColType -> Bool
isIntegerType' PGInteger  = True
isIntegerType' PGSmallInt = True
isIntegerType' PGBigInt   = True
isIntegerType' _          = False

isNumType' :: PGBaseColType -> Bool
isNumType' PGFloat   = True
isNumType' PGDouble  = True
isNumType' PGNumeric = True
isNumType' ty        = isIntegerType' ty

isJSONBType' :: PGBaseColType -> Bool
isJSONBType' PGJSONB = True
isJSONBType' _       = False

isComparableType' :: PGBaseColType -> Bool
isComparableType' PGJSON        = False
isComparableType' PGJSONB       = False
isComparableType' PGGeometry    = False
isComparableType' PGGeography   = False
isComparableType' PGBoolean     = False
isComparableType' (PGUnknown _) = False
isComparableType' _             = True

isBigNum' :: PGBaseColType -> Bool
isBigNum' = \case
  PGBigInt    -> True
  PGBigSerial -> True
  PGNumeric   -> True
  PGDouble    -> True
  _           -> False
