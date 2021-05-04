module Hasura.Backends.Postgres.SQL.Value
  ( PGScalarValue(..)
  , pgScalarValueToJson
  , withConstructorFn
  , parsePGValue
  , scientificToInteger
  , scientificToFloat

  , TxtEncodedVal(..)
  , txtEncodedVal

  , binEncoder
  , txtEncoder
  , toPrepParam
  ) where

import           Hasura.Prelude

import           Hasura.SQL.Value                   (TxtEncodedVal (..))

import qualified Data.Aeson.Text                    as AE
import qualified Data.Aeson.Types                   as AT
import qualified Data.ByteString                    as B
import qualified Data.Text                          as T
import qualified Data.Text.Conversions              as TC
import qualified Data.Text.Encoding                 as TE
import qualified Data.Text.Lazy                     as TL
import qualified Data.UUID                          as UUID
import qualified Database.PG.Query                  as Q
import qualified Database.PG.Query.PTI              as PTI
import qualified Database.PostgreSQL.LibPQ          as PQ
import qualified PostgreSQL.Binary.Encoding         as PE

import           Data.Aeson
import           Data.Int
import           Data.Scientific
import           Data.Time

import qualified Hasura.Backends.Postgres.SQL.DML   as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.SQL.GeoJSON
import           Hasura.SQL.Time

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

newtype Ltree = Ltree Text
  deriving (Show, Eq)

instance ToJSON Ltree where
    toJSON (Ltree t) = toJSON t

instance FromJSON Ltree where
    parseJSON = \case
        String t ->
            if any T.null $ T.splitOn (T.pack ".") t
                then fail message
                else pure $ Ltree t
        _ -> fail message
        where
            message = "Expecting label path: a sequence of zero or more labels separated by dots, for example L1.L2.L3"

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
  | PGValVarchar !Text
  | PGValText !Text
  | PGValCitext !Text
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
  | PGValLtree !Ltree
  | PGValLquery !Text
  | PGValLtxtquery !Text
  | PGValUnknown !Text
  deriving (Show, Eq)

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
  PGValLtree t -> toJSON t
  PGValLquery t -> toJSON t
  PGValLtxtquery t -> toJSON t
  PGValUnknown t -> toJSON t

withConstructorFn :: PGScalarType -> S.SQLExp -> S.SQLExp
withConstructorFn ty v
  | isGeoType ty = S.SEFnApp "ST_GeomFromGeoJSON" [v] Nothing
  | ty == PGRaster = S.SEFnApp "ST_RastFromHexWKB" [v] Nothing
  | otherwise = v


scientificToInteger :: (Integral i, Bounded i) => Scientific -> AT.Parser i
scientificToInteger num =
  toBoundedInteger num
  `onNothing`
   fail ("The value " ++ show num ++ " lies outside the "
      ++ "bounds or is not an integer.  Maybe it is a "
      ++ "float, or is there integer overflow?")

scientificToFloat :: (RealFloat f) => Scientific -> AT.Parser f
scientificToFloat num =
  toBoundedRealFloat num
  `onLeft` \ _ ->
  fail ("The value " ++ show num ++ " lies outside the "
     ++ "bounds.  Is it overflowing the float bounds?")

parsePGValue :: PGScalarType -> Value -> AT.Parser PGScalarValue
parsePGValue ty val = case (ty, val) of
  (_          , Null)     -> pure $ PGNull ty
  (PGUnknown _, String t) -> pure $ PGValUnknown t
  (PGRaster   , _)        -> parseTyped -- strictly parse raster value
  (PGLtree    , _)        -> parseTyped
  (_          , String t) -> parseTyped <|> pure (PGValUnknown t)
  (_          , _)        -> parseTyped
  where
    parseBoundedInt :: forall i. (Integral i, Bounded i) => Value -> AT.Parser i
    parseBoundedInt = withScientific ("Integer expected for input type: " ++ show ty) scientificToInteger

    parseBoundedFloat :: forall a. (RealFloat a) => Value -> AT.Parser a
    parseBoundedFloat = withScientific ("Float expected for input type: " ++ show ty) scientificToFloat

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
      PGLtree -> PGValLtree <$> parseJSON val
      PGLquery -> PGValLquery <$> parseJSON val
      PGLtxtquery -> PGValLtxtquery <$> parseJSON val
      PGUnknown tyName ->
        fail $ "A string is expected for type: " ++ T.unpack tyName

txtEncodedVal :: PGScalarValue -> TxtEncodedVal
txtEncodedVal = \case
  PGValInteger i  -> TELit $ tshow i
  PGValSmallInt i -> TELit $ tshow i
  PGValBigInt i   -> TELit $ tshow i
  PGValFloat f    -> TELit $ tshow f
  PGValDouble d   -> TELit $ tshow d
  PGValNumeric sc -> TELit $ tshow sc
  -- PostgreSQL doesn't like scientific notation for money, so pass it
  -- with 2 decimal places.
  PGValMoney m    -> TELit $ T.pack $ formatScientific Fixed (Just 2) m
  PGValBoolean b  -> TELit $ bool "false" "true" b
  PGValChar t     -> TELit $ tshow t
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
  PGValLtree (Ltree t) -> TELit t
  PGValLquery t -> TELit t
  PGValLtxtquery t -> TELit t
  PGValUnknown t -> TELit t

pgTypeOid :: PGScalarType -> PQ.Oid
pgTypeOid = \case
  PGSmallInt    -> PTI.int2
  PGInteger     -> PTI.int4
  PGBigInt      -> PTI.int8
  PGSerial      -> PTI.int4
  PGBigSerial   -> PTI.int8
  PGFloat       -> PTI.float4
  PGDouble      -> PTI.float8
  PGNumeric     -> PTI.numeric
  PGMoney       -> PTI.numeric
  PGBoolean     -> PTI.bool
  PGChar        -> PTI.char
  PGVarchar     -> PTI.varchar
  PGText        -> PTI.text
  PGCitext      -> PTI.text -- Explict type cast to citext needed, See also Note [Type casting prepared params]
  PGDate        -> PTI.date
  PGTimeStamp   -> PTI.timestamp
  PGTimeStampTZ -> PTI.timestamptz
  PGTimeTZ      -> PTI.timetz
  PGJSON        -> PTI.json
  PGJSONB       -> PTI.jsonb
  PGGeometry    -> PTI.text -- we are using the ST_GeomFromGeoJSON($i) instead of $i
  PGGeography   -> PTI.text
  PGRaster      -> PTI.text -- we are using the ST_RastFromHexWKB($i) instead of $i
  PGUUID        -> PTI.uuid
  PGLtree       -> PTI.text
  PGLquery      -> PTI.text
  PGLtxtquery   -> PTI.text
  (PGUnknown _) -> PTI.auto

binEncoder :: PGScalarValue -> Q.PrepArg
binEncoder = \case
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
  PGValLtree (Ltree t)             -> Q.toPrepVal t
  PGValLquery t                    -> Q.toPrepVal t
  PGValLtxtquery t                 -> Q.toPrepVal t
  PGValUnknown t                   -> (PTI.auto, Just (TE.encodeUtf8 t, PQ.Text))

txtEncoder :: PGScalarValue -> S.SQLExp
txtEncoder colVal = case txtEncodedVal colVal of
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
