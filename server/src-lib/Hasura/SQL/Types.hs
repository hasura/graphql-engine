{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

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

newtype TableName
  = TableName { getTableTxt :: T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift)

instance IsIden TableName where
  toIden (TableName t) = Iden t

instance DQuote TableName where
  dquoteTxt (TableName t) = t

instance ToSQL TableName where
  toSQL = toSQL . toIden

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

newtype SchemaName
  = SchemaName { getSchemaTxt :: T.Text }
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, Lift)

publicSchema :: SchemaName
publicSchema = SchemaName "public"

instance IsIden SchemaName where
  toIden (SchemaName t) = Iden t

instance ToSQL SchemaName where
  toSQL = toSQL . toIden

data QualifiedTable
  = QualifiedTable
  { qtSchema :: !SchemaName
  , qtTable  :: !TableName
  } deriving (Show, Eq, Generic, Lift)

instance FromJSON QualifiedTable where
  parseJSON v@(String _) =
    QualifiedTable publicSchema <$> parseJSON v
  parseJSON (Object o) =
    QualifiedTable <$>
    o .:? "schema" .!= publicSchema <*>
    o .: "name"
  parseJSON _ =
    fail "expecting a string/object for table"

instance ToJSON QualifiedTable where
  toJSON (QualifiedTable (SchemaName "public") tn) = toJSON tn
  toJSON (QualifiedTable sn tn) =
    object [ "schema" .= sn
           , "name"  .= tn
           ]

instance ToJSONKey QualifiedTable where
  toJSONKey = ToJSONKeyText qualTableToTxt (text . qualTableToTxt)

instance DQuote QualifiedTable where
  dquoteTxt = qualTableToTxt

instance Hashable QualifiedTable

instance ToSQL QualifiedTable where
  toSQL (QualifiedTable sn tn) =
    toSQL sn <> "." <> toSQL tn

qualTableToTxt :: QualifiedTable -> T.Text
qualTableToTxt (QualifiedTable (SchemaName "public") tn) =
  getTableTxt tn
qualTableToTxt (QualifiedTable sn tn) =
  getSchemaTxt sn <> "." <> getTableTxt tn

snakeCaseTable :: QualifiedTable -> T.Text
snakeCaseTable (QualifiedTable sn tn) =
  getSchemaTxt sn <> "_" <> getTableTxt tn

newtype PGCol
  = PGCol { getPGColTxt :: T.Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, Hashable, Q.ToPrepArg, Q.FromCol, ToJSONKey, FromJSONKey, Lift)

instance IsIden PGCol where
  toIden (PGCol t) = Iden t

instance ToSQL PGCol where
  toSQL = toSQL . toIden

instance DQuote PGCol where
  dquoteTxt (PGCol t) = t

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

instance FromJSON PGColType where
  parseJSON (String "serial")      = return PGSerial
  parseJSON (String "bigserial")   = return PGBigSerial

  parseJSON (String "smallint")    = return PGSmallInt
  parseJSON (String "int2")    = return PGSmallInt

  parseJSON (String "integer")     = return PGInteger
  parseJSON (String "int4")        = return PGInteger

  parseJSON (String "bigint")      = return PGBigInt
  parseJSON (String "int8")      = return PGBigInt

  parseJSON (String "real")        = return PGFloat
  parseJSON (String "float4")      = return PGFloat

  parseJSON (String "double precision")      = return PGDouble
  parseJSON (String "float8")      = return PGDouble

  parseJSON (String "numeric")     = return PGNumeric
  parseJSON (String "decimal")     = return PGNumeric

  parseJSON (String "boolean")     = return PGBoolean
  parseJSON (String "bool")        = return PGBoolean

  parseJSON (String "character")   = return PGChar

  parseJSON (String "varchar")     = return PGVarchar
  parseJSON (String "character varying")     = return PGVarchar

  parseJSON (String "text")        = return PGText
  parseJSON (String "citext")      = return PGText

  parseJSON (String "date")        = return PGDate

  parseJSON (String "timestamptz") = return PGTimeStampTZ
  parseJSON (String "timestamp with time zone") = return PGTimeStampTZ

  parseJSON (String "timetz")      = return PGTimeTZ
  parseJSON (String "time with time zone") = return PGTimeTZ

  parseJSON (String "json") = return PGJSON
  parseJSON (String "jsonb") = return PGJSONB

  parseJSON (String "geometry") = return PGGeometry
  parseJSON (String "geography") = return PGGeography

  parseJSON (String t)             = return $ PGUnknown t
  parseJSON _                      =
    fail "Expecting a string for PGColType"

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

isIntegerType :: PGColType -> Bool
isIntegerType PGInteger  = True
isIntegerType PGSmallInt = True
isIntegerType PGBigInt   = True
isIntegerType _          = False

isJSONBType :: PGColType -> Bool
isJSONBType PGJSONB = True
isJSONBType _       = False
