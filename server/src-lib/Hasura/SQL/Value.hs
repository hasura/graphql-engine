module Hasura.SQL.Value
  ( PGScalarValue(..)
  , pgColValueToInt
  , pgScalarValueToJson
  , withConstructorFn
  , parsePGValue
  , mkWithScalarType

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
import qualified Data.UUID                  as UUID

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

instance ToJSON RasterWKB where
  toJSON = toJSON . TC.toText . getRasterWKB

--  Binary value. Used in prepared sq
data PGScalarValue
  = PGValInteger !Int32
  | PGValSmallInt !Int16
  | PGValBigInt !Int64
  | PGValFloat !Float
  | PGValDouble !Double
  | PGValNumeric !Scientific
  | PGValMoney !Scientific
  | PGValBoolean !Bool
  | PGValChar !Char
  | PGValVarchar !T.Text
  | PGValText !T.Text
  | PGValCitext !T.Text
  | PGValDate !Day
  | PGValTimeStamp !LocalTime
  | PGValTimeStampTZ !UTCTime
  | PGValTimeTZ !ZonedTimeOfDay
  | PGNull !PGScalarType
  | PGValJSON !Q.JSON
  | PGValJSONB !Q.JSONB
  | PGValGeo !GeometryWithCRS
  | PGValRaster !RasterWKB
  | PGValUUID !UUID.UUID
  | PGValUnknown !T.Text
  deriving (Show, Eq)

mkWithScalarType :: PGScalarValue -> WithScalarType PGScalarValue
mkWithScalarType = \case
  x@(PGValInteger _) -> WithScalarType PGInteger x
  x@(PGValSmallInt _) -> WithScalarType PGSmallInt x
  x@(PGValBigInt _) -> WithScalarType PGBigInt x
  x@(PGValFloat _) -> WithScalarType PGFloat x
  x@(PGValDouble _) -> WithScalarType PGDouble x
  x@(PGValNumeric _) -> WithScalarType PGNumeric x
  x@(PGValMoney _) -> WithScalarType PGMoney x
  x@(PGValBoolean _) -> WithScalarType PGBoolean x
  x@(PGValChar _) -> WithScalarType PGChar x
  x@(PGValVarchar _) -> WithScalarType PGVarchar x
  x@(PGValText _) -> WithScalarType PGText x
  x@(PGValCitext _) -> WithScalarType PGCitext x
  x@(PGValDate _) -> WithScalarType PGDate x
  x@(PGValTimeStampTZ _) -> WithScalarType PGTimeStampTZ x
  x@(PGValTimeTZ _) -> WithScalarType PGTimeTZ x
  x@(PGNull t) -> WithScalarType t x
  x@(PGValJSON _) -> WithScalarType PGJSON x
  x@(PGValJSONB _) -> WithScalarType PGJSONB x
  x@(PGValGeo _) -> WithScalarType PGGeography x
  x@(PGValRaster _) -> WithScalarType PGRaster x
  x@(PGValUUID _) -> WithScalarType PGUUID x
  x@(PGValUnknown t) -> WithScalarType (PGUnknown t) x

pgScalarValueToJson :: PGScalarValue -> Value
pgScalarValueToJson = \case
  PGValInteger i  -> toJSON i
  PGValSmallInt i -> toJSON i
  PGValBigInt i   -> toJSON i
  PGValFloat f    -> toJSON f
  PGValDouble d   -> toJSON d
  PGValNumeric sc -> toJSON sc
  PGValMoney m    -> toJSON m
  PGValBoolean b  -> toJSON b
  PGValChar t     -> toJSON t
  PGValVarchar t  -> toJSON t
  PGValText t     -> toJSON t
  PGValCitext t   -> toJSON t
  PGValDate d     -> toJSON d
  PGValTimeStamp u ->
    toJSON $ formatTime defaultTimeLocale "%FT%T%QZ" u
  PGValTimeStampTZ u ->
    toJSON $ formatTime defaultTimeLocale "%FT%T%QZ" u
  PGValTimeTZ (ZonedTimeOfDay tod tz) ->
    toJSON (show tod ++ timeZoneOffsetString tz)
  PGNull _ -> Null
  PGValJSON (Q.JSON j)    -> j
  PGValJSONB (Q.JSONB j)  -> j
  PGValGeo o    -> toJSON o
  PGValRaster r -> toJSON r
  PGValUUID u -> toJSON u
  PGValUnknown t -> toJSON t

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
    parseBoundedInt :: forall i. (Integral i, Bounded i) => Value -> AT.Parser i
    parseBoundedInt val' =
      withScientific
        ("Integer expected for input type: " ++ show ty)
        go
        val'
      where
        go num = case toBoundedInteger num of
          Just parsed -> return parsed
          Nothing     -> fail $ "The value " ++ show num ++ " lies outside the "
                         ++ "bounds or is not an integer.  Maybe it is a "
                         ++ "float, or is there integer overflow?"
    parseBoundedFloat :: forall a. (RealFloat a) => Value -> AT.Parser a
    parseBoundedFloat val' =
      withScientific
        ("Float expected for input type: " ++ show ty)
        go
        val'
      where
        go num = case toBoundedRealFloat num of
          Left _       -> fail $ "The value " ++ show num ++ " lies outside the "
                          ++ "bounds.  Is it overflowing the float bounds?"
          Right parsed -> return parsed
    parseTyped = case ty of
      PGSmallInt -> PGValSmallInt <$> parseBoundedInt val
      PGInteger -> PGValInteger <$> parseBoundedInt val
      PGBigInt -> PGValBigInt <$> parseBoundedInt val
      PGSerial -> PGValInteger <$> parseBoundedInt val
      PGBigSerial -> PGValBigInt <$> parseBoundedInt val
      PGFloat -> PGValFloat <$> parseBoundedFloat val
      PGDouble -> PGValDouble <$> parseBoundedFloat val
      PGNumeric -> PGValNumeric <$> parseJSON val
      PGMoney -> PGValMoney <$> parseJSON val
      PGBoolean -> PGValBoolean <$> parseJSON val
      PGChar -> PGValChar <$> parseJSON val
      PGVarchar -> PGValVarchar <$> parseJSON val
      PGText -> PGValText <$> parseJSON val
      PGCitext -> PGValCitext <$> parseJSON val
      PGDate -> PGValDate <$> parseJSON val
      PGTimeStamp -> PGValTimeStamp <$> parseJSON val
      PGTimeStampTZ -> PGValTimeStampTZ <$> parseJSON val
      PGTimeTZ -> PGValTimeTZ <$> parseJSON val
      PGJSON -> PGValJSON . Q.JSON <$> parseJSON val
      PGJSONB -> PGValJSONB . Q.JSONB <$> parseJSON val
      PGGeometry -> PGValGeo <$> parseJSON val
      PGGeography -> PGValGeo <$> parseJSON val
      PGRaster -> PGValRaster <$> parseJSON val
      PGUUID -> PGValUUID <$> parseJSON val
      PGUnknown tyName ->
        fail $ "A string is expected for type: " ++ T.unpack tyName

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
  PGValMoney m    -> TELit $ T.pack $ show m
  PGValBoolean b  -> TELit $ bool "false" "true" b
  PGValChar t     -> TELit $ T.pack $ show t
  PGValVarchar t  -> TELit t
  PGValText t     -> TELit t
  PGValCitext t   -> TELit t
  PGValDate d     -> TELit $ T.pack $ showGregorian d
  PGValTimeStamp u ->
    TELit $ T.pack $ formatTime defaultTimeLocale "%FT%T%QZ" u
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
  PGValUUID u -> TELit $ UUID.toText u
  PGValUnknown t -> TELit t

binEncoder :: PGScalarValue -> Q.PrepArg
binEncoder colVal = case colVal of
  PGValInteger i                   -> Q.toPrepVal i
  PGValSmallInt i                  -> Q.toPrepVal i
  PGValBigInt i                    -> Q.toPrepVal i
  PGValFloat f                     -> Q.toPrepVal f
  PGValDouble d                    -> Q.toPrepVal d
  PGValNumeric sc                  -> Q.toPrepVal sc
  PGValMoney m                     -> Q.toPrepVal m
  PGValBoolean b                   -> Q.toPrepVal b
  PGValChar t                      -> Q.toPrepVal t
  PGValVarchar t                   -> Q.toPrepVal t
  PGValText t                      -> Q.toPrepVal t
  PGValCitext t                    -> Q.toPrepVal t
  PGValDate d                      -> Q.toPrepVal d
  PGValTimeStamp u                 -> Q.toPrepVal u
  PGValTimeStampTZ u               -> Q.toPrepVal u
  PGValTimeTZ (ZonedTimeOfDay t z) -> Q.toPrepValHelper PTI.timetz PE.timetz_int (t, z)
  PGNull ty                        -> (pgTypeOid ty, Nothing)
  PGValJSON u                      -> Q.toPrepVal u
  PGValJSONB u                     -> Q.toPrepVal u
  PGValGeo o                       -> Q.toPrepVal $ TL.toStrict $ AE.encodeToLazyText o
  PGValRaster r                    -> Q.toPrepVal $ TC.toText $ getRasterWKB r
  PGValUUID u                      -> Q.toPrepVal u
  PGValUnknown t                   -> (PTI.auto, Just (TE.encodeUtf8 t, PQ.Text))

txtEncoder :: PGScalarValue -> S.SQLExp
txtEncoder colVal = case txtEncodedPGVal colVal of
  TENull  -> S.SENull
  TELit t -> S.SELit t

{- Note [Type casting prepared params]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Prepared values are passed to Postgres via text encoding. Explicit type cast for prepared params
is needed to distinguish the column types. For example, the parameter for citext column type is
generated as ($i)::citext where 'i' is parameter position (integer).

Also see https://github.com/hasura/graphql-engine/issues/2818
-}

toPrepParam :: Int -> PGScalarType -> S.SQLExp
toPrepParam i ty =
  -- See Note [Type casting prepared params] above
  S.withTyAnn ty . withConstructorFn ty $ S.SEPrep i

toBinaryValue :: WithScalarType PGScalarValue -> Q.PrepArg
toBinaryValue = binEncoder . pstValue

toTxtValue :: WithScalarType PGScalarValue -> S.SQLExp
toTxtValue (WithScalarType ty val) =
  S.withTyAnn ty . withConstructorFn ty $ txtEncoder val
