module Hasura.SQL.Value where

import           Hasura.SQL.GeoJSON
import           Hasura.SQL.Time
import           Hasura.SQL.Types

import qualified Database.PG.Query          as Q
import qualified Database.PG.Query.PTI      as PTI
import qualified Hasura.SQL.DML             as S

import           Data.Aeson
import           Data.Aeson.Internal
import           Data.Int
import           Data.Scientific
import           Data.Time
import           Hasura.Prelude

import qualified Data.Aeson.Text            as AE
import qualified Data.Aeson.Types           as AT
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL

import qualified Database.PostgreSQL.LibPQ  as PQ
import qualified PostgreSQL.Binary.Encoding as PE

--  Binary value. Used in prepared sq
data PGColValue
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
  | PGNull !PGColType
  | PGValJSON !Q.JSON
  | PGValJSONB !Q.JSONB
  | PGValGeo !GeometryWithCRS
  | PGValUnknown !T.Text
  deriving (Show, Eq)

txtEncoder :: PGColValue -> S.SQLExp
txtEncoder colVal = case colVal of
  PGValInteger i  -> S.SELit $ T.pack $ show i
  PGValSmallInt i -> S.SELit $ T.pack $ show i
  PGValBigInt i   -> S.SELit $ T.pack $ show i
  PGValFloat f    -> S.SELit $ T.pack $ show f
  PGValDouble d   -> S.SELit $ T.pack $ show d
  PGValNumeric sc -> S.SELit $ T.pack $ show sc
  PGValBoolean b  -> S.SELit $ bool "false" "true" b
  PGValChar t     -> S.SELit $ T.pack $ show t
  PGValVarchar t  -> S.SELit t
  PGValText t     -> S.SELit t
  PGValDate d     -> S.SELit $ T.pack $ showGregorian d
  PGValTimeStampTZ u ->
    S.SELit $ T.pack $ formatTime defaultTimeLocale "%FT%T%QZ" u
  PGValTimeTZ (ZonedTimeOfDay tod tz) ->
    S.SELit $ T.pack (show tod ++ timeZoneOffsetString tz)
  PGNull _ ->
    S.SEUnsafe "NULL"
  PGValJSON (Q.JSON j)    -> S.SELit $ TL.toStrict $
    AE.encodeToLazyText j
  PGValJSONB (Q.JSONB j)  -> S.SELit $ TL.toStrict $
    AE.encodeToLazyText j
  PGValGeo o    -> S.SELit $ TL.toStrict $
    AE.encodeToLazyText o
  PGValUnknown t -> S.SELit t

binEncoder :: PGColValue -> Q.PrepArg
binEncoder colVal = case colVal of
  PGValInteger i ->
    Q.toPrepVal i
  PGValSmallInt i ->
    Q.toPrepVal i
  PGValBigInt i ->
    Q.toPrepVal i
  PGValFloat f ->
    Q.toPrepVal f
  PGValDouble d ->
    Q.toPrepVal d
  PGValNumeric sc ->
    Q.toPrepVal sc
  PGValBoolean b ->
    Q.toPrepVal b
  PGValChar t ->
    Q.toPrepVal t
  PGValVarchar t ->
    Q.toPrepVal t
  PGValText t ->
    Q.toPrepVal t
  PGValDate d ->
    Q.toPrepVal d
  PGValTimeStampTZ u ->
    Q.toPrepVal u
  PGValTimeTZ (ZonedTimeOfDay t z) ->
    Q.toPrepValHelper PTI.timetz PE.timetz_int (t, z)
  PGNull ty ->
    (pgTypeOid ty, Nothing)
  PGValJSON u ->
    Q.toPrepVal u
  PGValJSONB u ->
    Q.toPrepVal u
  PGValGeo o ->
    Q.toPrepVal $ TL.toStrict $ AE.encodeToLazyText o
  PGValUnknown t ->
    (PTI.auto, Just (TE.encodeUtf8 t, PQ.Text))

parsePGValue' :: PGColType
             -> Value
             -> AT.Parser PGColValue
parsePGValue' ty Null =
  return $ PGNull ty
parsePGValue' PGSmallInt val =
  PGValSmallInt <$> parseJSON val
parsePGValue' PGInteger val =
  PGValInteger <$> parseJSON val
parsePGValue' PGBigInt val =
  PGValBigInt <$> parseJSON val
parsePGValue' PGSerial val =
  PGValInteger <$> parseJSON val
parsePGValue' PGBigSerial val =
  PGValBigInt <$> parseJSON val
parsePGValue' PGFloat val =
  PGValFloat <$> parseJSON val
parsePGValue' PGDouble val =
  PGValDouble <$> parseJSON val
parsePGValue' PGNumeric val =
  PGValNumeric <$> parseJSON val
parsePGValue' PGBoolean val =
  PGValBoolean <$> parseJSON val
parsePGValue' PGChar val =
  PGValChar <$> parseJSON val
parsePGValue' PGVarchar val =
  PGValVarchar <$> parseJSON val
parsePGValue' PGText val =
  PGValText <$> parseJSON val
parsePGValue' PGDate val =
  PGValDate <$> parseJSON val
parsePGValue' PGTimeStampTZ val =
  PGValTimeStampTZ <$> parseJSON val
parsePGValue' PGTimeTZ val =
  PGValTimeTZ <$> parseJSON val
parsePGValue' PGJSON val =
  PGValJSON . Q.JSON <$> parseJSON val
parsePGValue' PGJSONB val =
  PGValJSONB . Q.JSONB <$> parseJSON val
parsePGValue' PGGeometry val =
  PGValGeo <$> parseJSON val
parsePGValue' PGGeography val =
  PGValGeo <$> parseJSON val
parsePGValue' (PGUnknown _) (String t) =
  return $ PGValUnknown t
parsePGValue' (PGUnknown tyName) _ =
  fail $ "A string is expected for type : " ++ T.unpack tyName

parsePGValue :: PGColType -> Value -> AT.Parser PGColValue
parsePGValue pct val =
  case val of
    String t -> parsePGValue' pct val <|> return (PGValUnknown t)
    _        -> parsePGValue' pct val

convToBin :: PGColType
          -> Value
          -> AT.Parser Q.PrepArg
convToBin ty val =
  binEncoder <$> parsePGValue ty val

convToTxt :: PGColType
          -> Value
          -> AT.Parser S.SQLExp
convToTxt ty val =
  txtEncoder <$> parsePGValue ty val

readEitherTxt :: (Read a) => T.Text -> Either String a
readEitherTxt = readEither . T.unpack

iresToEither :: IResult a -> Either String a
iresToEither (IError _ msg) = Left msg
iresToEither (ISuccess a)   = return a

pgValFromJVal :: (FromJSON a) => Value -> Either String a
pgValFromJVal = iresToEither . ifromJSON

applyGeomFromGeoJson :: S.SQLExp -> S.SQLExp
applyGeomFromGeoJson v =
  S.SEFnApp "ST_GeomFromGeoJSON" [v] Nothing

isGeoTy :: PGColType -> Bool
isGeoTy = \case
  PGGeometry  -> True
  PGGeography -> True
  _           -> False

toPrepParam :: Int -> PGColType -> S.SQLExp
toPrepParam i =
  bool prepVal (applyGeomFromGeoJson prepVal) . isGeoTy
  where
    prepVal = S.SEPrep i

toTxtValue :: PGColType -> PGColValue -> S.SQLExp
toTxtValue ty val =
  S.annotateExp txtVal ty
  where
    txtVal = withGeoVal $ txtEncoder val
    withGeoVal v =
      bool v (applyGeomFromGeoJson v) $ isGeoTy ty

pgColValueToInt :: PGColValue -> Maybe Int
pgColValueToInt (PGValInteger i)  = Just $ fromIntegral i
pgColValueToInt (PGValSmallInt i) = Just $ fromIntegral i
pgColValueToInt (PGValBigInt i)   = Just $ fromIntegral i
pgColValueToInt _                 = Nothing
