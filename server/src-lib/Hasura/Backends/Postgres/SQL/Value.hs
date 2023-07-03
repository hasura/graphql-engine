-- | Postgres SQL Value
--
-- Deals with Postgres scalar values, converting them to and from 'Text', and to
-- JSON 'Value'.
module Hasura.Backends.Postgres.SQL.Value
  ( PGScalarValue (..),
    pgScalarValueToJson,
    withConstructorFn,
    parsePGValue,
    scientificToInteger,
    scientificToFloat,
    textToScalarValue,
    TxtEncodedVal (..),
    txtEncodedVal,
    binEncoder,
    txtEncoder,
    toPrepParam,
    withScalarTypeAnn,
    withTypeAnn,
  )
where

import Data.Aeson
import Data.Aeson.Text qualified as AE
import Data.Aeson.Types qualified as AT
import Data.ByteString qualified as B
import Data.Int
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Conversions qualified as TC
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.UUID qualified as UUID
import Database.PG.Query qualified as PG
import Database.PG.Query.PTI qualified as PTI
import Database.PostgreSQL.LibPQ qualified as PQ
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Prelude
import Hasura.SQL.GeoJSON
import Hasura.SQL.Time
import Hasura.SQL.Types
import Hasura.SQL.Value (TxtEncodedVal (..))
import PostgreSQL.Binary.Encoding qualified as PE

newtype RasterWKB = RasterWKB {getRasterWKB :: TC.Base16 B.ByteString}
  deriving (Show, Eq)

instance FromJSON RasterWKB where
  parseJSON = \case
    String t -> case TC.fromText t of
      Just v -> return $ RasterWKB v
      Nothing ->
        fail
          "invalid hexadecimal representation of raster well known binary format"
    _ -> fail "expecting String for raster"

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

-- @PGScalarValue@ represents any value that can be a column in a Postgres table
data PGScalarValue
  = PGValInteger Int32
  | PGValSmallInt Int16
  | PGValBigInt Int64
  | PGValFloat Float
  | PGValDouble Double
  | PGValNumeric Scientific
  | PGValMoney Scientific
  | PGValBoolean Bool
  | PGValChar Char
  | PGValVarchar Text
  | PGValText Text
  | PGValCitext Text
  | PGValDate Day
  | PGValTimeStamp LocalTime
  | PGValTimeStampTZ UTCTime
  | PGValTimeTZ ZonedTimeOfDay
  | PGNull PGScalarType
  | PGValJSON PG.JSON
  | PGValJSONB PG.JSONB
  | PGValGeo GeometryWithCRS
  | PGValRaster RasterWKB
  | PGValUUID UUID.UUID
  | PGValLtree Ltree
  | PGValLquery Text
  | PGValLtxtquery Text
  | PGValUnknown Text
  | PGValArray [PGScalarValue]
  deriving (Show, Eq)

pgScalarValueToJson :: PGScalarValue -> Value
pgScalarValueToJson = \case
  PGValInteger i -> toJSON i
  PGValSmallInt i -> toJSON i
  PGValBigInt i -> toJSON i
  PGValFloat f -> toJSON f
  PGValDouble d -> toJSON d
  PGValNumeric sc -> toJSON sc
  PGValMoney m -> toJSON m
  PGValBoolean b -> toJSON b
  PGValChar t -> toJSON t
  PGValVarchar t -> toJSON t
  PGValText t -> toJSON t
  PGValCitext t -> toJSON t
  PGValDate d -> toJSON d
  PGValTimeStamp u -> String $ formatTimestamp u
  PGValTimeStampTZ u -> String $ formatTimestamp u
  PGValTimeTZ (ZonedTimeOfDay tod tz) ->
    toJSON (show tod ++ timeZoneOffsetString tz)
  PGNull _ -> Null
  PGValJSON (PG.JSON j) -> j
  PGValJSONB (PG.JSONB j) -> j
  PGValGeo o -> toJSON o
  PGValRaster r -> toJSON r
  PGValUUID u -> toJSON u
  PGValLtree t -> toJSON t
  PGValLquery t -> toJSON t
  PGValLtxtquery t -> toJSON t
  PGValUnknown t -> toJSON t
  PGValArray a -> toJSON (map pgScalarValueToJson a)

textToScalarValue :: Maybe Text -> PGScalarValue
textToScalarValue = maybe (PGNull PGText) PGValText

withConstructorFn :: PGScalarType -> S.SQLExp -> S.SQLExp
withConstructorFn ty v
  | isGeoType ty = S.SEFnApp "ST_GeomFromGeoJSON" [v] Nothing
  | ty == PGRaster = S.SEFnApp "ST_RastFromHexWKB" [v] Nothing
  | otherwise = v

-- FIXME: shouldn't this also use 'withConstructorFn'?
withScalarTypeAnn :: PGScalarType -> S.SQLExp -> S.SQLExp
withScalarTypeAnn colTy v = S.SETyAnn v . S.mkTypeAnn $ CollectableTypeScalar colTy

withTypeAnn :: CollectableType PGScalarType -> S.SQLExp -> S.SQLExp
withTypeAnn ty expr = flip S.SETyAnn (S.mkTypeAnn ty)
  $ case ty of
    CollectableTypeScalar baseTy -> withConstructorFn baseTy expr
    CollectableTypeArray _ -> expr

-- TODO: those two functions are useful outside of Postgres, and
-- should be moved to a common place of the code. Perhaps the Prelude?
scientificToInteger :: (Integral i, Bounded i) => Scientific -> AT.Parser i
scientificToInteger num =
  toBoundedInteger num
    `onNothing` fail
      ( "The value "
          ++ show num
          ++ " lies outside the "
          ++ "bounds or is not an integer. Maybe it is a "
          ++ "float, or is there integer overflow?"
      )

scientificToFloat :: (RealFloat f) => Scientific -> AT.Parser f
scientificToFloat num =
  toBoundedRealFloat num
    `onLeft` \_ ->
      fail
        ( "The value "
            ++ show num
            ++ " lies outside the "
            ++ "bounds. Is it overflowing the float bounds?"
        )

parsePGValue :: PGScalarType -> Value -> AT.Parser PGScalarValue
parsePGValue ty val = case (ty, val) of
  (_, Null) -> pure $ PGNull ty
  (PGUnknown _, String t) -> pure $ PGValUnknown t
  (PGRaster, _) -> parseTyped -- strictly parse raster value
  (PGLtree, _) -> parseTyped
  (_, String t) -> parseTyped <|> pure (PGValUnknown t)
  (_, _) -> parseTyped
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
      PGJSON -> PGValJSON . PG.JSON <$> parseJSON val
      PGJSONB -> PGValJSONB . PG.JSONB <$> parseJSON val
      PGGeometry -> PGValGeo <$> parseJSON val
      PGGeography -> PGValGeo <$> parseJSON val
      PGRaster -> PGValRaster <$> parseJSON val
      PGUUID -> PGValUUID <$> parseJSON val
      PGLtree -> PGValLtree <$> parseJSON val
      PGLquery -> PGValLquery <$> parseJSON val
      PGLtxtquery -> PGValLtxtquery <$> parseJSON val
      PGUnknown tyName ->
        fail $ "A string is expected for type: " ++ T.unpack tyName
      PGCompositeScalar tyName ->
        fail $ "A string is expected for type: " ++ T.unpack tyName
      PGEnumScalar tyName ->
        fail $ "A string is expected for type: " ++ T.unpack tyName
      PGArray s -> parseJSON val >>= fmap PGValArray . traverse (parsePGValue s)

txtEncodedVal :: PGScalarValue -> TxtEncodedVal
txtEncodedVal = \case
  PGValInteger i -> TELit $ tshow i
  PGValSmallInt i -> TELit $ tshow i
  PGValBigInt i -> TELit $ tshow i
  PGValFloat f -> TELit $ tshow f
  PGValDouble d -> TELit $ tshow d
  PGValNumeric sc -> TELit $ tshow sc
  -- PostgreSQL doesn't like scientific notation for money, so pass it
  -- with 2 decimal places.
  PGValMoney m -> TELit $ T.pack $ formatScientific Fixed (Just 2) m
  PGValBoolean b -> TELit $ bool "false" "true" b
  PGValChar t -> TELit $ T.singleton t
  PGValVarchar t -> TELit t
  PGValText t -> TELit t
  PGValCitext t -> TELit t
  PGValDate d -> TELit $ T.pack $ showGregorian d
  PGValTimeStamp u -> TELit $ formatTimestamp u
  PGValTimeStampTZ u -> TELit $ formatTimestamp u
  PGValTimeTZ (ZonedTimeOfDay tod tz) ->
    TELit $ T.pack (show tod ++ timeZoneOffsetString tz)
  PGNull _ ->
    TENull
  PGValJSON (PG.JSON j) ->
    TELit
      $ TL.toStrict
      $ AE.encodeToLazyText j
  PGValJSONB (PG.JSONB j) ->
    TELit
      $ TL.toStrict
      $ AE.encodeToLazyText j
  PGValGeo o ->
    TELit
      $ TL.toStrict
      $ AE.encodeToLazyText o
  PGValRaster r -> TELit $ TC.toText $ getRasterWKB r
  PGValUUID u -> TELit $ UUID.toText u
  PGValLtree (Ltree t) -> TELit t
  PGValLquery t -> TELit t
  PGValLtxtquery t -> TELit t
  PGValUnknown t -> TELit t
  PGValArray ts -> TELit $ buildArrayLiteral ts

binEncoder :: PGScalarValue -> PG.PrepArg
binEncoder = \case
  PGValInteger i -> PG.toPrepVal i
  PGValSmallInt i -> PG.toPrepVal i
  PGValBigInt i -> PG.toPrepVal i
  PGValFloat f -> PG.toPrepVal f
  PGValDouble d -> PG.toPrepVal d
  PGValNumeric sc -> PG.toPrepVal sc
  PGValMoney m -> PG.toPrepVal m
  PGValBoolean b -> PG.toPrepVal b
  PGValChar t -> PG.toPrepVal t
  PGValVarchar t -> PG.toPrepVal t
  PGValText t -> PG.toPrepVal t
  PGValCitext t -> PG.toPrepVal t
  PGValDate d -> PG.toPrepVal d
  PGValTimeStamp u -> PG.toPrepVal u
  PGValTimeStampTZ u -> PG.toPrepVal u
  PGValTimeTZ (ZonedTimeOfDay t z) -> PG.toPrepValHelper PTI.timetz PE.timetz_int (t, z)
  PGNull ty -> (pgTypeOid ty, Nothing)
  PGValJSON u -> PG.toPrepVal u
  PGValJSONB u -> PG.toPrepVal u
  PGValGeo o -> PG.toPrepVal $ TL.toStrict $ AE.encodeToLazyText o
  PGValRaster r -> PG.toPrepVal $ TC.toText $ getRasterWKB r
  PGValUUID u -> PG.toPrepVal u
  PGValLtree (Ltree t) -> PG.toPrepVal t
  PGValLquery t -> PG.toPrepVal t
  PGValLtxtquery t -> PG.toPrepVal t
  PGValUnknown t -> (PTI.auto, Just (TE.encodeUtf8 t, PQ.Text))
  PGValArray s -> (PTI.auto, Just (TE.encodeUtf8 $ buildArrayLiteral s, PQ.Text))

formatTimestamp :: (FormatTime t) => t -> Text
formatTimestamp = T.pack . formatTime defaultTimeLocale "%0Y-%m-%dT%T%QZ"

txtEncoder :: PGScalarValue -> S.SQLExp
txtEncoder colVal = case txtEncodedVal colVal of
  TENull -> S.SENull
  TELit t -> S.SELit t

-- arrays are sufficiently complicated, e.g. in the case of empty and unknown element arrays,
-- for us to default to text encoding in all cases, and defer to Postgres' handling of them
--
-- FIXME: this will fail if we ever introduce the box type as a @PGScalarValue@,
-- which uses a different seperator https://www.postgresql.org/docs/current/arrays.html#ARRAYS-INPUT
-- https://github.com/hasura/graphql-engine-mono/issues/4892
buildArrayLiteral :: [PGScalarValue] -> Text
buildArrayLiteral ts =
  T.concat ["{", T.intercalate "," (map (inner . encodeElement) ts), "}"]
  where
    -- present text elements as json strings
    escape = TL.toStrict . AE.encodeToLazyText
    encodeElement = \case
      PGValChar t -> TELit $ escape $ T.singleton t
      PGValVarchar t -> TELit $ escape t
      PGValText t -> TELit $ escape t
      PGValCitext t -> TELit $ escape t
      PGValLquery t -> TELit $ escape t
      PGValLtxtquery t -> TELit $ escape t
      PGValUnknown t -> TELit $ escape t
      PGValJSON (PG.JSON j) -> case j of
        -- this is delicate - we want to encode JSON
        -- that is provided to HGE as raw JSON literals provided via variables,
        -- and in stringified form as received when
        -- inlined in a query. Therefore we need to check whether any string
        -- receive is a genuine JSON string value, or a stringified rich value.
        String s -> case decode (txtToLbs s) of
          Just jv -> fromJson jv -- it was some actual JSON in disguise! encode it like usual
          Nothing -> TELit $ escape (escape s) -- it's an actual JSON string, so add quotes again
        _ -> fromJson j
      PGValJSONB (PG.JSONB j) -> case j of
        -- we do the same for JSONB as JSON
        String s -> case decode (txtToLbs s) of
          Just jv -> fromJsonb jv -- it was some actual JSON in disguise! encode it like usual
          Nothing -> TELit $ escape (escape s) -- it's an actual JSON string, so add quotes again
        _ -> fromJsonb j
      other -> txtEncodedVal other

    fromJson = TELit . escape . bsToTxt . PE.encodingBytes . PE.json_ast
    fromJsonb = TELit . escape . bsToTxt . PE.encodingBytes . PE.jsonb_ast

    inner = \case
      TENull -> "null"
      TELit t -> t

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
  withScalarTypeAnn ty . withConstructorFn ty $ S.SEPrep i
