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
  | PGValUnknown !T.Text
  deriving (Show, Eq)

data TxtEncodedPGVal
  = TENull
  | TELit !Text
  deriving (Show, Eq, Generic)

instance Hashable TxtEncodedPGVal

instance ToJSON TxtEncodedPGVal where
  toJSON = \case
    TENull  -> Null
    TELit t -> String t

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
  PGValUnknown t -> TELit t

txtEncoder :: PGScalarValue -> S.SQLExp
txtEncoder colVal = case txtEncodedPGVal colVal of
  TENull  -> S.SEUnsafe "NULL"
  TELit t -> S.SELit t

binEncoder :: PGScalarValue -> Q.PrepArg
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
    textToPrepVal t

textToPrepVal :: Text -> Q.PrepArg
textToPrepVal t =
  (PTI.auto, Just (TE.encodeUtf8 t, PQ.Text))

parsePGValue' :: PGScalarType
             -> Value
             -> AT.Parser PGScalarValue
parsePGValue' ty v = case (ty, v) of
  (_, Null) -> return $ PGNull ty
  (PGSmallInt, val) -> PGValSmallInt <$> parseJSON val
  (PGInteger, val) -> PGValInteger <$> parseJSON val
  (PGBigInt, val) -> PGValBigInt <$> parseJSON val
  (PGSerial, val) -> PGValInteger <$> parseJSON val
  (PGBigSerial, val) -> PGValBigInt <$> parseJSON val
  (PGFloat, val) -> PGValFloat <$> parseJSON val
  (PGDouble, val) -> PGValDouble <$> parseJSON val
  (PGNumeric, val) -> PGValNumeric <$> parseJSON val
  (PGBoolean, val) -> PGValBoolean <$> parseJSON val
  (PGChar, val) -> PGValChar <$> parseJSON val
  (PGVarchar, val) -> PGValVarchar <$> parseJSON val
  (PGText, val) -> PGValText <$> parseJSON val
  (PGDate, val) -> PGValDate <$> parseJSON val
  (PGTimeStampTZ, val) -> PGValTimeStampTZ <$> parseJSON val
  (PGTimeTZ, val) -> PGValTimeTZ <$> parseJSON val
  (PGJSON, val) -> PGValJSON . Q.JSON <$> parseJSON val
  (PGJSONB, val) -> PGValJSONB . Q.JSONB <$> parseJSON val
  (PGGeometry, val) -> PGValGeo <$> parseJSON val
  (PGGeography, val) -> PGValGeo <$> parseJSON val
  (PGUnknown _, String t) -> return $ PGValUnknown t
  (PGUnknown tyName, _) -> fail $ "A string is expected for type : " ++ T.unpack tyName

parsePGValue :: PGScalarType -> Value -> AT.Parser PGScalarValue
parsePGValue pct val =
  case val of
    String t -> parsePGValue' pct val <|> return (PGValUnknown t)
    _        -> parsePGValue' pct val

readEitherTxt :: (Read a) => T.Text -> Either String a
readEitherTxt = readEither . T.unpack

iresToEither :: IResult a -> Either String a
iresToEither (IError _ msg) = Left msg
iresToEither (ISuccess a)   = return a

pgValFromJVal :: (FromJSON a) => Value -> Either String a
pgValFromJVal = iresToEither . ifromJSON

withGeoVal :: PGScalarType -> S.SQLExp -> S.SQLExp
withGeoVal ty v
  | isGeoType ty = S.SEFnApp "ST_GeomFromGeoJSON" [v] Nothing
  | otherwise = v

toPrepParam :: Int -> PGScalarType -> S.SQLExp
toPrepParam i ty =
  withGeoVal ty $ S.SEPrep i

toBinaryValue :: WithScalarType PGScalarValue -> Q.PrepArg
toBinaryValue = binEncoder . pstValue

toTxtValue :: WithScalarType PGScalarValue -> S.SQLExp
toTxtValue (WithScalarType ty val) = S.withTyAnn ty . withGeoVal ty $ txtEncoder val

pgColValueToInt :: PGScalarValue -> Maybe Int
pgColValueToInt (PGValInteger i)  = Just $ fromIntegral i
pgColValueToInt (PGValSmallInt i) = Just $ fromIntegral i
pgColValueToInt (PGValBigInt i)   = Just $ fromIntegral i
pgColValueToInt _                 = Nothing
