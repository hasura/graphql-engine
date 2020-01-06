module Hasura.SQL.Value
  ( PGScalarValue(..)
  , pgColValueToInt
  , withConstructorFn
  , parsePGValue

  , TxtEncodedPGVal(..)
  , txtEncodedPGVal

  , binEncoder
  , txtEncoder
  , toBinaryValue
  , toTxtValue
  , toPrepParam
  ) where

import           Hasura.SQL.GeoJSON
import           Hasura.SQL.Time
import           Hasura.SQL.Types

import qualified Database.PG.Query          as Q
import qualified Database.PG.Query.PTI      as PTI
import qualified Hasura.SQL.DML             as S

import           Data.Aeson
import           Data.Int
import           Data.Scientific
import           Data.Time
import           Hasura.Prelude

import qualified Data.Aeson.Text            as AE
import qualified Data.Aeson.Types           as AT
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import qualified Data.Text.Conversions      as TC
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL

import qualified Database.PostgreSQL.LibPQ  as PQ
import qualified PostgreSQL.Binary.Encoding as PE

newtype RasterWKB
  = RasterWKB { getRasterWKB :: TC.Base16 B.ByteString }
  deriving (Show, Eq)

instance FromJSON RasterWKB where
  parseJSON = \case
    String t -> case TC.fromText t of
      Just v  -> return $ RasterWKB v
      Nothing -> fail
        "invalid hexadecimal representation of raster well known binary format"
    _        -> fail "expecting String for raster"

--  Binary value. Used in prepared sq
data PGScalarValue
  = PGValInteger !Int32
  | PGValSmallInt !Int16
  | PGValBigInt !Int64
  | PGValFloat !Float
  | PGValDouble !Double
  | PGValNumeric !Scientific
  | PGValBoolean !Bool
  | PGValChar !Char
  | PGValVarchar !T.Text
  | PGValText !T.Text
  | PGValDate !Day
  | PGValTimeStampTZ !UTCTime
  | PGValTimeTZ !ZonedTimeOfDay
  | PGNull !PGScalarType
  | PGValJSON !Q.JSON
  | PGValJSONB !Q.JSONB
  | PGValGeo !GeometryWithCRS
  | PGValRaster !RasterWKB
  | PGValUnknown !T.Text
  deriving (Show, Eq)

pgColValueToInt :: PGScalarValue -> Maybe Int
pgColValueToInt (PGValInteger i)  = Just $ fromIntegral i
pgColValueToInt (PGValSmallInt i) = Just $ fromIntegral i
pgColValueToInt (PGValBigInt i)   = Just $ fromIntegral i
pgColValueToInt _                 = Nothing

withConstructorFn :: PGScalarType -> S.SQLExp -> S.SQLExp
withConstructorFn ty v
  | isGeoType ty = S.SEFnApp "ST_GeomFromGeoJSON" [v] Nothing
  | ty == PGRaster = S.SEFnApp "ST_RastFromHexWKB" [v] Nothing
  | otherwise = v

parsePGValue :: PGScalarType -> Value -> AT.Parser PGScalarValue
parsePGValue ty val = case (ty, val) of
  (_          , Null)     -> pure $ PGNull ty
  (PGUnknown _, String t) -> pure $ PGValUnknown t
  (PGRaster   , _)        -> parseTyped -- strictly parse raster value
  (_          , String t) -> parseTyped <|> pure (PGValUnknown t)
  (_          , _)        -> parseTyped
  where
    parseTyped = case ty of
      PGSmallInt -> PGValSmallInt <$> parseJSON val
      PGInteger -> PGValInteger <$> parseJSON val
      PGBigInt -> PGValBigInt <$> parseJSON val
      PGSerial -> PGValInteger <$> parseJSON val
      PGBigSerial -> PGValBigInt <$> parseJSON val
      PGFloat -> PGValFloat <$> parseJSON val
      PGDouble -> PGValDouble <$> parseJSON val
      PGNumeric -> PGValNumeric <$> parseJSON val
      PGBoolean -> PGValBoolean <$> parseJSON val
      PGChar -> PGValChar <$> parseJSON val
      PGVarchar -> PGValVarchar <$> parseJSON val
      PGText -> PGValText <$> parseJSON val
      PGDate -> PGValDate <$> parseJSON val
      PGTimeStampTZ -> PGValTimeStampTZ <$> parseJSON val
      PGTimeTZ -> PGValTimeTZ <$> parseJSON val
      PGJSON -> PGValJSON . Q.JSON <$> parseJSON val
      PGJSONB -> PGValJSONB . Q.JSONB <$> parseJSON val
      PGGeometry -> PGValGeo <$> parseJSON val
      PGGeography -> PGValGeo <$> parseJSON val
      PGRaster -> PGValRaster <$> parseJSON val
      PGUnknown tyName ->
        fail $ "A string is expected for type : " ++ T.unpack tyName

data TxtEncodedPGVal
  = TENull
  | TELit !Text
  deriving (Show, Eq, Generic)

instance Hashable TxtEncodedPGVal

instance ToJSON TxtEncodedPGVal where
  toJSON = \case
    TENull  -> Null
    TELit t -> String t

instance FromJSON TxtEncodedPGVal where
  parseJSON Null       = pure TENull
  parseJSON (String t) = pure $ TELit t
  parseJSON v          = AT.typeMismatch "String" v

txtEncodedPGVal :: PGScalarValue -> TxtEncodedPGVal
txtEncodedPGVal colVal = case colVal of
  PGValInteger i  -> TELit $ T.pack $ show i
  PGValSmallInt i -> TELit $ T.pack $ show i
  PGValBigInt i   -> TELit $ T.pack $ show i
  PGValFloat f    -> TELit $ T.pack $ show f
  PGValDouble d   -> TELit $ T.pack $ show d
  PGValNumeric sc -> TELit $ T.pack $ show sc
  PGValBoolean b  -> TELit $ bool "false" "true" b
  PGValChar t     -> TELit $ T.pack $ show t
  PGValVarchar t  -> TELit t
  PGValText t     -> TELit t
  PGValDate d     -> TELit $ T.pack $ showGregorian d
  PGValTimeStampTZ u ->
    TELit $ T.pack $ formatTime defaultTimeLocale "%FT%T%QZ" u
  PGValTimeTZ (ZonedTimeOfDay tod tz) ->
    TELit $ T.pack (show tod ++ timeZoneOffsetString tz)
  PGNull _ ->
    TENull
  PGValJSON (Q.JSON j)    -> TELit $ TL.toStrict $
    AE.encodeToLazyText j
  PGValJSONB (Q.JSONB j)  -> TELit $ TL.toStrict $
    AE.encodeToLazyText j
  PGValGeo o    -> TELit $ TL.toStrict $
    AE.encodeToLazyText o
  PGValRaster r -> TELit $ TC.toText $ getRasterWKB r
  PGValUnknown t -> TELit t

binEncoder :: PGScalarValue -> Q.PrepArg
binEncoder colVal = case colVal of
  PGValInteger i -> Q.toPrepVal i
  PGValSmallInt i -> Q.toPrepVal i
  PGValBigInt i -> Q.toPrepVal i
  PGValFloat f -> Q.toPrepVal f
  PGValDouble d -> Q.toPrepVal d
  PGValNumeric sc -> Q.toPrepVal sc
  PGValBoolean b -> Q.toPrepVal b
  PGValChar t -> Q.toPrepVal t
  PGValVarchar t -> Q.toPrepVal t
  PGValText t -> Q.toPrepVal t
  PGValDate d -> Q.toPrepVal d
  PGValTimeStampTZ u -> Q.toPrepVal u
  PGValTimeTZ (ZonedTimeOfDay t z) -> Q.toPrepValHelper PTI.timetz PE.timetz_int (t, z)
  PGNull ty -> (pgTypeOid ty, Nothing)
  PGValJSON u -> Q.toPrepVal u
  PGValJSONB u -> Q.toPrepVal u
  PGValGeo o -> Q.toPrepVal $ TL.toStrict $ AE.encodeToLazyText o
  PGValRaster r -> Q.toPrepVal $ TC.toText $ getRasterWKB r
  PGValUnknown t -> (PTI.auto, Just (TE.encodeUtf8 t, PQ.Text))

txtEncoder :: PGScalarValue -> S.SQLExp
txtEncoder colVal = case txtEncodedPGVal colVal of
  TENull  -> S.SENull
  TELit t -> S.SELit t

toPrepParam :: Int -> PGScalarType -> S.SQLExp
toPrepParam i ty = withConstructorFn ty $ S.SEPrep i

toBinaryValue :: WithScalarType PGScalarValue -> Q.PrepArg
toBinaryValue = binEncoder . pstValue

toTxtValue :: WithScalarType PGScalarValue -> S.SQLExp
toTxtValue (WithScalarType ty val) =
  S.withTyAnn ty . withConstructorFn ty $ txtEncoder val
