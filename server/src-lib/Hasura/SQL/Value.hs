{-# LANGUAGE PatternSynonyms #-}
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
import           Foreign.C.Types
import           Hasura.Prelude

import qualified Data.Aeson.Text            as AE
import qualified Data.Aeson.Types           as AT
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Vector                as V

import qualified Database.PostgreSQL.LibPQ  as PQ
import qualified PostgreSQL.Binary.Encoding as PE


data PGColValue = PGColValue !PQ.Oid PGColValueDet
  deriving (Show, Eq)

type PGElemOid = PQ.Oid

pattern PGBoolVal :: PQ.Oid -> Bool -> PGColValue
pattern PGBoolVal o b <- PGColValue o (PGValBase (PGValKnown (PGValBoolean b)))

pattern PGTxtVal :: PQ.Oid -> Text -> PGColValue
pattern PGTxtVal o x = PGColValue o (PGValBase (PGValKnown (PGValText x)))

pattern PGVarcharVal :: PQ.Oid -> Text -> PGColValue
pattern PGVarcharVal o x = PGColValue o (PGValBase (PGValKnown (PGValVarchar x)))

data PGColValueDet
  = PGValBase      !PGBaseColValue
  | PGValDomain    !PGColValue
  | PGValArray     !PGElemOid !(V.Vector PGColValue)
  | PGValEnum      !Text
  | PGValRange     !Text
  -- TODO Change this to HashMap, field -> maybe PGColValue
  | PGValComposite !Text
  | PGNull
  deriving (Show, Eq)

--  Binary value. Used in prepared sq
data PGBaseColValue
  = PGValKnown !PGBCKnown
  | PGValUnknown !T.Text
  deriving (Show, Eq)

data PGBCKnown
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
  | PGValJSON !Q.JSON
  | PGValJSONB !Q.JSONB
  | PGValGeo !GeometryWithCRS
  deriving (Show, Eq)

data PGColValueBin = PGColValueBin PQ.Oid PGColValueBinDet

type ElemOid = PQ.Oid

data PGColValueBinDet
  = PGValBaseBin   !PGBCKnown
  | PGValDomainBin !PGColValueBin
  | PGValArrayBin  ElemOid !(V.Vector PGColValueBin)
  | PGNullBin

toPGBinVal :: PGColValue -> Maybe PGColValueBin
toPGBinVal (PGColValue oid x) = fmap (PGColValueBin oid) $ case x of
  PGNull             -> Just PGNullBin
  PGValComposite _   -> Nothing
  PGValRange _       -> Nothing
  PGValEnum _        -> Nothing
  PGValDomain b      -> PGValDomainBin <$> toPGBinVal b
  PGValArray eOid v  -> PGValArrayBin eOid <$> mapM toPGBinVal v
  PGValBase b        -> case b of
                        PGValKnown kb  -> Just (PGValBaseBin kb)
                        PGValUnknown{} -> Nothing

--binTyM :: PGColValue -> Maybe PGColValueBin
--binTyM

data TxtEncodedPGVal
  = TENull
  | TELit !Text
  | TEArray ![TxtEncodedPGVal]
  deriving (Show, Eq, Generic)

instance Hashable TxtEncodedPGVal

instance ToJSON TxtEncodedPGVal where
  toJSON = \case
    TENull  -> Null
    TELit t -> String t
    TEArray l -> toJSON l

fromEncPGVal :: TxtEncodedPGVal -> S.SQLExp
fromEncPGVal = \case
  TENull -> S.SEUnsafe "NULL"
  TELit t -> S.SELit t
  TEArray l -> S.SEArray $ map fromEncPGVal l

txtEncoder :: PGColValue -> S.SQLExp
txtEncoder = fromEncPGVal . txtEncodePGVal

txtEncodePGValG
  :: (PGBaseColValue -> TxtEncodedPGVal) -> PGColValue -> TxtEncodedPGVal
txtEncodePGValG f (PGColValue _ x) = case x of
  PGValBase b      -> f b
  PGValDomain b    -> txtEncodePGVal b
  PGValComposite a -> TELit a
  PGValRange a     -> TELit a
  PGValEnum a      -> TELit a
  PGValArray _ as  -> TEArray $ map (txtEncodePGValG f) $ V.toList as
  PGNull           -> TENull

txtEncodePGVal :: PGColValue -> TxtEncodedPGVal
txtEncodePGVal = txtEncodePGValG txtEncodePGValBase

txtEncodePGValBase :: PGBaseColValue -> TxtEncodedPGVal
txtEncodePGValBase (PGValUnknown t)    = TELit t
txtEncodePGValBase (PGValKnown colVal) = case colVal of
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
  PGValJSON (Q.JSON j)    -> TELit $ TL.toStrict $
    AE.encodeToLazyText j
  PGValJSONB (Q.JSONB j)  -> TELit $ TL.toStrict $
    AE.encodeToLazyText j
  PGValGeo o    -> TELit $ TL.toStrict $
    AE.encodeToLazyText o

paTxtEncBase :: PGBCKnown -> (PQ.Oid, T.Text)
paTxtEncBase c = case c of
  PGValInteger i     -> (oidBuiltIn i, T.pack $ show i)
  PGValSmallInt i    -> (oidBuiltIn i, T.pack $ show i)
  PGValBigInt i      -> (oidBuiltIn i, T.pack $ show i)
  PGValFloat i       -> (oidBuiltIn i, T.pack $ show i)
  PGValDouble i      -> (oidBuiltIn i, T.pack $ show i)
  PGValNumeric i     -> (oidBuiltIn i, T.pack $ show i)
  PGValBoolean i     -> (oidBuiltIn i, T.pack $ show i)
  PGValChar i        -> (oidBuiltIn i, T.pack [i])
  PGValVarchar t     -> (oidBuiltIn t, t)
  PGValText t        -> (oidBuiltIn t, t)
  PGValDate d        -> (oidBuiltIn d, T.pack $ showGregorian d)
  PGValTimeStampTZ i -> (oidBuiltIn i, T.pack $ show i)
  PGValTimeTZ (ZonedTimeOfDay tod tz) ->
    (PTI.timetz  , T.pack (show tod ++ timeZoneOffsetString tz))
  PGValJSON t@(Q.JSON j)   -> (oidBuiltIn t, TL.toStrict $ AE.encodeToLazyText j)
  PGValJSONB t@(Q.JSONB j) -> (oidBuiltIn t, TL.toStrict $ AE.encodeToLazyText j)
  PGValGeo o               -> paTxtEncBase $ PGValText $ TL.toStrict $ AE.encodeToLazyText o

data TxtEncInfo
  = TxtEncInfo
  { teiOid           :: PQ.Oid
  -- Should be double quoted if this encoding is for an element of array/composite etc
  , teiToDoubleQuote :: Bool
  , teiEnc           :: Text
  }

paTxtEnc :: PGColValue -> TxtEncInfo
paTxtEnc (PGColValue oid v) = case v of
  PGValBase (PGValKnown x)   -> let y = paTxtEncBase x in TxtEncInfo (fst y) True (snd y)
  PGValBase (PGValUnknown x) -> TxtEncInfo oid True  x
  PGValDomain x              -> paTxtEnc x
  PGValComposite x           -> TxtEncInfo oid True x
  PGValRange x               -> TxtEncInfo oid True x
  PGValEnum x                -> TxtEncInfo oid True x
  PGNull                     -> TxtEncInfo oid False "NULL"
  PGValArray _ x             -> TxtEncInfo oid True $ asPGArr $ V.toList x
  where
    asPGArr a = curly $ T.intercalate "," $ map encAndDoubleQuote a
    encAndDoubleQuote x =
      let TxtEncInfo _ q enc = paTxtEnc x in
          bool id doubleQuoted q enc
    doubleQuoted a = "\"" <> escaped a  <> "\""
    escaped a = T.replace "\"" "\\\"" $ T.replace "\\" "\\\\" a
    curly a = "{" <> a <> "}"


binEncKnown :: PGBCKnown -> (PQ.Oid, Maybe PE.Encoding)
binEncKnown c = case c of
  PGValInteger i     -> paBinEncBuiltIn i
  PGValSmallInt i    -> paBinEncBuiltIn i
  PGValBigInt i      -> paBinEncBuiltIn i
  PGValFloat i       -> paBinEncBuiltIn i
  PGValDouble i      -> paBinEncBuiltIn i
  PGValNumeric i     -> paBinEncBuiltIn i
  PGValBoolean i     -> paBinEncBuiltIn i
  PGValChar t        -> paBinEncBuiltIn t
  PGValVarchar t     -> paBinEncBuiltIn t
  PGValText t        -> paBinEncBuiltIn t
  PGValDate d        -> paBinEncBuiltIn d
  PGValTimeStampTZ d -> paBinEncBuiltIn d
  PGValTimeTZ (ZonedTimeOfDay t z) ->
    (PTI.timetz  , Just $ PE.timetz_int (t,z))
  PGValJSON u        -> paBinEncBuiltIn u
  PGValJSONB u       -> paBinEncBuiltIn u
  PGValGeo o         -> paBinEncBuiltIn $ TL.toStrict $ AE.encodeToLazyText o

binEnc :: PGColValueBin -> (PQ.Oid, Maybe PE.Encoding)
binEnc x@(PGColValueBin oid c) = case c of
  PGNullBin               -> (oid, Nothing)
  PGValBaseBin b          -> binEncKnown b
  PGValDomainBin b        -> binEnc b
  PGValArrayBin elemOid _ -> (oid, Just $ PE.array (toWord32 elemOid) $ arrEnc x)
    where
      toWord32 (PQ.Oid (CUInt z)) = z
      arrEnc :: PGColValueBin -> PE.Array
      arrEnc (PGColValueBin _ z) = case z of
        (PGValArrayBin _ y) -> PE.dimensionArray foldl' arrEnc y
        PGNullBin         -> PE.nullArray
        PGValDomainBin b  -> arrEnc b
        PGValBaseBin t    -> maybe PE.nullArray PE.encodingArray $ snd $ binEncKnown t

paBinEncBuiltIn :: Q.BinaryEncBuiltInTy a => a -> (PQ.Oid, Maybe PE.Encoding)
paBinEncBuiltIn x = (o,encF x)
  where (PTI.ElemOid o, _,encF) = Q.btBinaryEncInfo

oidBuiltIn :: Q.BinaryEncBuiltInTy a => a -> PQ.Oid
oidBuiltIn = fst . paBinEncBuiltIn

parseKnownValAs :: FromJSON a => (a -> PGBCKnown) -> Value -> AT.Parser PGBaseColValue
parseKnownValAs a v =  PGValKnown . a <$> parseJSON v

parsePGValue' :: PGBaseColType
             -> Value
             -> AT.Parser PGBaseColValue
parsePGValue' PGSmallInt val =
  parseKnownValAs PGValSmallInt val
parsePGValue' PGInteger val =
  parseKnownValAs PGValInteger val
parsePGValue' PGBigInt val =
  parseKnownValAs PGValBigInt val
parsePGValue' PGSerial val =
  parseKnownValAs PGValInteger val
parsePGValue' PGBigSerial val =
  parseKnownValAs PGValBigInt val
parsePGValue' PGFloat val =
  parseKnownValAs PGValFloat val
parsePGValue' PGDouble val =
  parseKnownValAs PGValDouble val
parsePGValue' PGNumeric val =
  parseKnownValAs PGValNumeric val
parsePGValue' PGBoolean val =
  parseKnownValAs PGValBoolean val
parsePGValue' PGChar val =
  parseKnownValAs PGValChar val
parsePGValue' PGVarchar val =
  parseKnownValAs PGValVarchar val
parsePGValue' PGText val =
  parseKnownValAs PGValText val
parsePGValue' PGDate val =
  parseKnownValAs PGValDate val
parsePGValue' PGTimeStampTZ val =
  parseKnownValAs PGValTimeStampTZ val
parsePGValue' PGTimeTZ val =
  parseKnownValAs PGValTimeTZ val
parsePGValue' PGJSON val =
  parseKnownValAs (PGValJSON . Q.JSON) val
parsePGValue' PGJSONB val =
  parseKnownValAs (PGValJSONB . Q.JSONB) val
parsePGValue' PGGeometry val =
  parseKnownValAs PGValGeo val
parsePGValue' PGGeography val =
  parseKnownValAs PGValGeo val
parsePGValue' (PGUnknown _) (String t) =
  return $ PGValUnknown t
parsePGValue' (PGUnknown tyName) _ =
  fail $ "A string is expected for type : " ++ T.unpack tyName

parsePGValue :: PGColType -> Value -> AT.Parser PGColValue
parsePGValue pct Null = return $ PGColValue (pgColTyOid pct) PGNull
parsePGValue pct val = case pgColTyDetails pct of
  PGTyPseudo{}    -> fail "Column types do not return psuedo types"
  PGTyArray pbct  -> parseAsArray pbct val
  PGTyEnum{}      -> parseAsEnum val
  PGTyDomain dom  -> parsePGValue dom val
  PGTyComposite{} -> parseAsComposite val
  PGTyRange{}     -> parseAsRange val
  PGTyBase pbct   -> parseAsBase pbct val
  where
    parseAsVal g v =
      let oid = pgColTyOid pct
          asVal = PGColValue oid . g in
      asVal <$> parseJSON v

    parseAsComposite = parseAsVal PGValComposite

    parseAsEnum      = parseAsVal PGValEnum

    parseAsRange     = parseAsVal PGValRange

    parseAsArray bct v =
      allowPGEncStr $ (flip $ withArray "[PGColValue]") v $ \arr -> do
        let elemOid = maybe (pgColTyOid bct) pgTyOid $ getArrayBaseTy pct
            asArr = PGColValue (pgTyOid pct) . PGValArray elemOid
        asArr <$> mapM (parsePGValue bct) arr

    asUnknown bct v = PGColValue (pgColTyOid bct) $ PGValBase $ PGValUnknown v

    parseAsBase bct v = allowPGEncStr $
      let asBaseColVal = PGColValue (pgTyOid pct) . PGValBase in
      asBaseColVal <$> parsePGValue' bct v

    allowPGEncStr :: AT.Parser PGColValue -> AT.Parser PGColValue
    allowPGEncStr f = case val of
      String t -> f <|> return (asUnknown pct t)
      _        -> f


convToBin :: PGColType
          -> Value
          -> AT.Parser Q.PrepArg
convToBin ty val = do
  colVal <- parsePGValue ty val
  return $ binEncoder colVal

binEncoder :: PGColValue -> Q.PrepArg
binEncoder colVal= maybe (asTxtPrepArg colVal) asBinPrepArg $ toPGBinVal colVal
  where
    asTxtPrepArg v
      = let TxtEncInfo oid _ enc = paTxtEnc v in
      (oid, Just (TE.encodeUtf8 enc, PQ.Text))
    asBinPrepArg vb
      = let (oid, enc) = binEnc vb in
      (oid, fmap (\x -> (PE.encodingBytes x,PQ.Binary)) enc )

convToTxt :: PGColType
          -> Value
          -> AT.Parser S.SQLExp
convToTxt ty val =
  toTxtValue ty <$> parsePGValue ty val

readEitherTxt :: (Read a) => T.Text -> Either String a
readEitherTxt = readEither . T.unpack

iresToEither :: IResult a -> Either String a
iresToEither (IError _ msg) = Left msg
iresToEither (ISuccess a)   = return a

pgValFromJVal :: (FromJSON a) => Value -> Either String a
pgValFromJVal = iresToEither . ifromJSON

pattern PGGeogVal :: GeometryWithCRS -> PGBaseColValue
pattern PGGeogVal x = PGValKnown (PGValGeo x)

applyGeomFromGeoJson :: S.SQLExp -> S.SQLExp
applyGeomFromGeoJson v = S.SEFnApp "ST_GeomFromGeoJSON" [v] Nothing

applyAsGeoJSON :: S.SQLExp -> S.SQLExp
applyAsGeoJSON expn =
  S.SEFnApp "ST_AsGeoJSON"
  [ expn
  , S.SEUnsafe "15" -- max decimal digits
  , S.SEUnsafe "4"  -- to print out crs
  ] Nothing
  `S.SETyAnn` jsonType

withGeoJSONArr :: (S.SQLExp -> S.SQLExp) -> S.SQLExp -> S.SQLExp
withGeoJSONArr f v =
  S.SESelect S.mkSelect
  { S.selExtr = pure $ flip S.Extractor Nothing $ S.SEFnApp "array_agg"
                [f $ S.mkQIdenExp unnestTab unnestCol]
                Nothing
  , S.selFrom = Just $ S.FromExp $ pure $ S.FIUnnest [v]
                (S.Alias unnestTab) [S.SEIden unnestCol]
  }
  where
    unnestCol = Iden "unnest_col"
    unnestTab = Iden "unnest_tab"

applyAsGeoJSONArr :: S.SQLExp -> S.SQLExp
applyAsGeoJSONArr =
  flip S.SETyAnn jsonArrType . withGeoJSONArr applyAsGeoJSON

toPrepParam :: Int -> PGColType -> S.SQLExp
toPrepParam i ty = withGeoVal ty $ S.SEPrep i

withGeoVal :: PGColType -> S.SQLExp -> S.SQLExp
withGeoVal ty = case dets of
  PGTyBase x  -> bool id applyGeomFromGeoJson $ isBaseTyGeo x
  PGTyArray{} -> case getArrayBaseTy ty of
    Just (PGColType _ _ _ (PGTyBase b)) -> bool id applyArrGeomFromGeoJson $
                                           isBaseTyGeo b
    _                                   -> id
  _           -> id
  where
    dets = pgColTyDetails ty
    isBaseTyGeo b =
      case b of
        PGGeometry  -> True
        PGGeography -> True
        _           -> False

    applyArrGeomFromGeoJson = withGeoJSONArr applyGeomFromGeoJson

toTxtValue :: PGColType -> PGColValue -> S.SQLExp
toTxtValue ty val =
  S.annotateExp ty txtVal
  where
    txtVal = withGeoVal ty $ txtEncoder val

pgColValueToInt :: PGColValue -> Maybe Int
pgColValueToInt (PGColValue _ x) = case x of
  (PGValBase i)   -> pgColValueToInt' i
  (PGValDomain i) -> pgColValueToInt i
  _               -> Nothing

pgColValueToInt' :: PGBaseColValue -> Maybe Int
pgColValueToInt' PGValUnknown{} = Nothing
pgColValueToInt' (PGValKnown x)   = case x of
  (PGValInteger i)  -> Just $ fromIntegral i
  (PGValSmallInt i) -> Just $ fromIntegral i
  (PGValBigInt i)   -> Just $ fromIntegral i
  _                 -> Nothing
