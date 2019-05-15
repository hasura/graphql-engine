module Hasura.RQL.GBoolExp
  ( toSQLBoolExp
  , getBoolExpDeps
  , annBoolExp
  , txtRHSBuilder
  , pgValParser
  ) where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML      as S

import           Data.Aeson

import qualified Data.HashMap.Strict as M
import qualified Data.Text.Extended  as T

parseOpExp
  :: (MonadError QErr m)
  => ValueParser m a
  -> FieldInfoMap
  -> PGColInfo
  -> (T.Text, Value) -> m (OpExpG a)
parseOpExp parser fim (PGColInfo cn colTy _) (opStr, val) = withErrPath $
  case opStr of
    "$eq"            -> parseEq
    "_eq"            -> parseEq

    "$ne"            -> parseNe
    "_ne"            -> parseNe
    "$neq"           -> parseNe
    "_neq"           -> parseNe

    "$in"            -> parseIn
    "_in"            -> parseIn

    "$nin"           -> parseNin
    "_nin"           -> parseNin

    "$gt"            -> parseGt
    "_gt"            -> parseGt

    "$lt"            -> parseLt
    "_lt"            -> parseLt

    "$gte"           -> parseGte
    "_gte"           -> parseGte

    "$lte"           -> parseLte
    "_lte"           -> parseLte

    "$like"          -> parseLike
    "_like"          -> parseLike

    "$nlike"         -> parseNlike
    "_nlike"         -> parseNlike

    "$ilike"         -> parseIlike
    "_ilike"         -> parseIlike

    "$nilike"        -> parseNilike
    "_nilike"        -> parseNilike

    "$similar"       -> parseSimilar
    "_similar"       -> parseSimilar
    "$nsimilar"      -> parseNsimilar
    "_nsimilar"      -> parseNsimilar

    "$is_null"       -> parseIsNull
    "_is_null"       -> parseIsNull

    -- jsonb type
    "_contains"      -> jsonbOnlyOp $ AContains <$> parseOne
    "$contains"      -> jsonbOnlyOp $ AContains <$> parseOne
    "_contained_in"  -> jsonbOnlyOp $ AContainedIn <$> parseOne
    "$contained_in"  -> jsonbOnlyOp $ AContainedIn <$> parseOne
    "_has_key"       -> jsonbOnlyOp $ AHasKey <$> parseWithTy PGText
    "$has_key"       -> jsonbOnlyOp $ AHasKey <$> parseWithTy PGText

    --FIXME:- Parse a session variable as text array values
    --TODO:- Add following commented operators after fixing above said
    -- "_has_keys_any"  -> jsonbOnlyOp $ AHasKeysAny <$> parseVal
    -- "$has_keys_any"  -> jsonbOnlyOp $ AHasKeysAny <$> parseVal
    -- "_has_keys_all"  -> jsonbOnlyOp $ AHasKeysAll <$> parseVal
    -- "$has_keys_all"  -> jsonbOnlyOp $ AHasKeysAll <$> parseVal

    -- geometry types
    "_st_contains"   -> parseGeometryOp ASTContains
    "$st_contains"   -> parseGeometryOp ASTContains
    "_st_crosses"    -> parseGeometryOp ASTCrosses
    "$st_crosses"    -> parseGeometryOp ASTCrosses
    "_st_equals"     -> parseGeometryOp ASTEquals
    "$st_equals"     -> parseGeometryOp ASTEquals
    "_st_overlaps"   -> parseGeometryOp ASTOverlaps
    "$st_overlaps"   -> parseGeometryOp ASTOverlaps
    "_st_touches"    -> parseGeometryOp ASTTouches
    "$st_touches"    -> parseGeometryOp ASTTouches
    "_st_within"     -> parseGeometryOp ASTWithin
    "$st_within"     -> parseGeometryOp ASTWithin
    -- geometry and geography types
    "_st_intersects" -> parseGeometryOrGeographyOp ASTIntersects
    "$st_intersects" -> parseGeometryOrGeographyOp ASTIntersects
    "_st_d_within"   -> parseSTDWithinObj
    "$st_d_within"   -> parseSTDWithinObj

    "$ceq"           -> parseCeq
    "_ceq"           -> parseCeq

    "$cne"           -> parseCne
    "_cne"           -> parseCne
    "$cneq"          -> parseCne
    "_cneq"          -> parseCne

    "$cgt"           -> parseCgt
    "_cgt"           -> parseCgt

    "$clt"           -> parseClt
    "_clt"           -> parseClt

    "$cgte"          -> parseCgte
    "_cgte"          -> parseCgte

    "$clte"          -> parseClte
    "_clte"          -> parseClte

    x                -> throw400 UnexpectedPayload $ "Unknown operator : " <> x
  where
    withErrPath = withPathK (getPGColTxt cn) . withPathK opStr

    parseEq       = AEQ False <$> parseOne -- equals
    parseNe       = ANE False <$> parseOne -- <>
    parseIn       = AIN <$> parseMany -- in an array
    parseNin      = ANIN <$> parseMany -- not in an array
    parseGt       = AGT <$> parseOne -- >
    parseLt       = ALT <$> parseOne -- <
    parseGte      = AGTE <$> parseOne -- >=
    parseLte      = ALTE <$> parseOne -- <=
    parseLike     = textOnlyOp colTy >> ALIKE <$> parseOne
    parseNlike    = textOnlyOp colTy >> ANLIKE <$> parseOne
    parseIlike    = textOnlyOp colTy >> AILIKE <$> parseOne
    parseNilike   = textOnlyOp colTy >> ANILIKE <$> parseOne
    parseSimilar  = textOnlyOp colTy >> ASIMILAR <$> parseOne
    parseNsimilar = textOnlyOp colTy >> ANSIMILAR <$> parseOne

    parseIsNull   = bool ANISNOTNULL ANISNULL -- is null
                    <$> parseVal

    parseCeq      = CEQ <$> decodeAndValidateRhsCol
    parseCne      = CNE <$> decodeAndValidateRhsCol
    parseCgt      = CGT <$> decodeAndValidateRhsCol
    parseClt      = CLT <$> decodeAndValidateRhsCol
    parseCgte     = CGTE <$> decodeAndValidateRhsCol
    parseClte     = CLTE <$> decodeAndValidateRhsCol

    jsonbOnlyOp m = case colTy of
      PGJSONB -> m
      ty      -> throwError $ buildMsg ty [PGJSONB]

    parseGeometryOp f =
      geometryOp colTy >> f <$> parseOne
    parseGeometryOrGeographyOp f =
      geometryOrGeographyOp colTy >> f <$> parseOne

    parseSTDWithinObj = case colTy of
      PGGeometry -> do
        DWithinGeomOp distVal fromVal <- parseVal
        dist <- withPathK "distance" $ parser PGFloat distVal
        from <- withPathK "from" $ parser colTy fromVal
        return $ ASTDWithinGeom $ DWithinGeomOp dist from
      PGGeography -> do
        DWithinGeogOp distVal fromVal sphVal <- parseVal
        dist <- withPathK "distance" $ parser PGFloat distVal
        from <- withPathK "from" $ parser colTy fromVal
        useSpheroid <- withPathK "use_spheroid" $ parser PGBoolean sphVal
        return $ ASTDWithinGeog $ DWithinGeogOp dist from useSpheroid
      _ -> throwError $ buildMsg colTy [PGGeometry, PGGeography]

    decodeAndValidateRhsCol =
      parseVal >>= validateRhsCol

    validateRhsCol rhsCol = do
      let errMsg = "column operators can only compare postgres columns"
      rhsType <- askPGType fim rhsCol errMsg
      if colTy /= rhsType
        then throw400 UnexpectedPayload $
             "incompatible column types : " <> cn <<> ", " <>> rhsCol
        else return rhsCol

    geometryOp PGGeometry = return ()
    geometryOp ty =
      throwError $ buildMsg ty [PGGeometry]
    geometryOrGeographyOp PGGeometry = return ()
    geometryOrGeographyOp PGGeography = return ()
    geometryOrGeographyOp ty =
      throwError $ buildMsg ty [PGGeometry, PGGeography]

    parseWithTy ty = parser ty val
    parseOne = parseWithTy colTy
    parseMany = do
      vals <- runAesonParser parseJSON val
      indexedForM vals (parser colTy)

    parseVal :: (FromJSON a, QErrM m) => m a
    parseVal = decodeValue val

parseOpExps
  :: (MonadError QErr m)
  => ValueParser m a
  -> FieldInfoMap
  -> PGColInfo
  -> Value
  -> m [OpExpG a]
parseOpExps valParser cim colInfo = \case
  (Object o) -> mapM (parseOpExp valParser cim colInfo)(M.toList o)
  val        -> pure . AEQ False <$> valParser (pgiType colInfo) val

type ValueParser m a = PGColType -> Value -> m a

buildMsg :: PGColType -> [PGColType] -> QErr
buildMsg ty expTys =
  err400 UnexpectedPayload $ mconcat
  [ " is of type " <> T.pack (show ty)
  , "; this operator works "
  , "only on columns of type "
  , T.intercalate "/" $ map (T.dquote . T.pack . show) expTys
  ]

textOnlyOp :: (MonadError QErr m) => PGColType -> m ()
textOnlyOp PGText    = return ()
textOnlyOp PGVarchar = return ()
textOnlyOp ty =
  throwError $ buildMsg ty [PGVarchar, PGText]

-- This convoluted expression instead of col = val
-- to handle the case of col : null
equalsBoolExpBuilder :: S.SQLExp -> S.SQLExp -> S.BoolExp
equalsBoolExpBuilder qualColExp rhsExp =
  S.BEBin S.OrOp (S.BECompare S.SEQ qualColExp rhsExp)
    (S.BEBin S.AndOp
      (S.BENull qualColExp)
      (S.BENull rhsExp))

notEqualsBoolExpBuilder :: S.SQLExp -> S.SQLExp -> S.BoolExp
notEqualsBoolExpBuilder qualColExp rhsExp =
  S.BEBin S.OrOp (S.BECompare S.SNE qualColExp rhsExp)
    (S.BEBin S.AndOp
      (S.BENotNull qualColExp)
      (S.BENull rhsExp))

annBoolExp
  :: (QErrM m, CacheRM m)
  => ValueParser m a
  -> FieldInfoMap
  -> BoolExp
  -> m (AnnBoolExp a)
annBoolExp valParser fim (BoolExp boolExp) =
  traverse (annColExp valParser fim) boolExp

annColExp
  :: (QErrM m, CacheRM m)
  => ValueParser m a
  -> FieldInfoMap
  -> ColExp
  -> m (AnnBoolExpFld a)
annColExp valueParser colInfoMap (ColExp fieldName colVal) = do
  colInfo <- askFieldInfo colInfoMap fieldName
  case colInfo of
    FIColumn (PGColInfo _ PGJSON _) ->
      throwError (err400 UnexpectedPayload "JSON column can not be part of where clause")
    -- FIColumn (PGColInfo _ PGJSONB _) ->
    --   throwError (err400 UnexpectedPayload "JSONB column can not be part of where clause")
    FIColumn pgi ->
      AVCol pgi <$> parseOpExps valueParser colInfoMap pgi colVal
    FIRelationship relInfo -> do
      relBoolExp      <- decodeValue colVal
      relFieldInfoMap <- askFieldInfoMap $ riRTable relInfo
      annRelBoolExp   <- annBoolExp valueParser relFieldInfoMap relBoolExp
      return $ AVRel relInfo annRelBoolExp

toSQLBoolExp
  :: S.Qual -> AnnBoolExpSQL -> S.BoolExp
toSQLBoolExp tq e =
  evalState (convBoolRhs' tq e) 0

convBoolRhs'
  :: S.Qual -> AnnBoolExpSQL -> State Word64 S.BoolExp
convBoolRhs' tq =
  foldBoolExp (convColRhs tq)

convColRhs
  :: S.Qual -> AnnBoolExpFldSQL -> State Word64 S.BoolExp
convColRhs tableQual = \case
  AVCol (PGColInfo cn _ _) opExps -> do
    let bExps = map (mkColCompExp tableQual cn) opExps
    return $ foldr (S.BEBin S.AndOp) (S.BELit True) bExps

  AVRel (RelInfo _ _ colMapping relTN _) nesAnn -> do
    -- Convert the where clause on the relationship
    curVarNum <- get
    put $ curVarNum + 1
    let newIden  = Iden $ "_be_" <> T.pack (show curVarNum) <> "_"
                   <> snakeCaseTable relTN
        newIdenQ = S.QualIden newIden
    annRelBoolExp <- convBoolRhs' newIdenQ nesAnn
    let backCompExp = foldr (S.BEBin S.AndOp) (S.BELit True) $
          flip map colMapping $ \(lCol, rCol) ->
          S.BECompare S.SEQ
          (mkQCol (S.QualIden newIden) rCol)
          (mkQCol tableQual lCol)
        innerBoolExp = S.BEBin S.AndOp backCompExp annRelBoolExp
    return $ S.mkExists (S.FISimple relTN $ Just $ S.Alias newIden) innerBoolExp
  where
    mkQCol q = S.SEQIden . S.QIden q . toIden

pgValParser
  :: (MonadError QErr m)
  => PGColType -> Value -> m PGColValue
pgValParser ty =
  runAesonParser (parsePGValue ty)

txtRHSBuilder
  :: (MonadError QErr m)
  => PGColType -> Value -> m S.SQLExp
txtRHSBuilder ty val =
  toTxtValue ty <$> pgValParser ty val

mkColCompExp
  :: S.Qual -> PGCol -> OpExpG S.SQLExp -> S.BoolExp
mkColCompExp qual lhsCol = \case
  AEQ False val    -> equalsBoolExpBuilder lhs val
  AEQ True val     -> S.BECompare S.SEQ lhs val
  ANE False val    -> notEqualsBoolExpBuilder lhs val
  ANE True  val    -> S.BECompare S.SNE lhs val
  AIN vals         -> handleEmptyIn vals
  ANIN vals        -> S.BENot $ handleEmptyIn vals
  AGT val          -> S.BECompare S.SGT lhs val
  ALT val          -> S.BECompare S.SLT lhs val
  AGTE val         -> S.BECompare S.SGTE lhs val
  ALTE val         -> S.BECompare S.SLTE lhs val
  ALIKE val        -> S.BECompare S.SLIKE lhs val
  ANLIKE val       -> S.BECompare S.SNLIKE lhs val
  AILIKE val       -> S.BECompare S.SILIKE lhs val
  ANILIKE val      -> S.BECompare S.SNILIKE lhs val
  ASIMILAR val     -> S.BECompare S.SSIMILAR lhs val
  ANSIMILAR val    -> S.BECompare S.SNSIMILAR lhs val
  AContains val    -> S.BECompare S.SContains lhs val
  AContainedIn val -> S.BECompare S.SContainedIn lhs val
  AHasKey val      -> S.BECompare S.SHasKey lhs val
  AHasKeysAny keys -> S.BECompare S.SHasKeysAny lhs $ toTextArray keys
  AHasKeysAll keys -> S.BECompare S.SHasKeysAll lhs $ toTextArray keys

  ASTContains val   -> mkGeomOpBe "ST_Contains" val
  ASTCrosses val    -> mkGeomOpBe "ST_Crosses" val
  ASTEquals val     -> mkGeomOpBe "ST_Equals" val
  ASTIntersects val -> mkGeomOpBe "ST_Intersects" val
  ASTOverlaps val   -> mkGeomOpBe "ST_Overlaps" val
  ASTTouches val    -> mkGeomOpBe "ST_Touches" val
  ASTWithin val     -> mkGeomOpBe "ST_Within" val

  ASTDWithinGeom (DWithinGeomOp r val)      -> applySQLFn "ST_DWithin" [lhs, val, r]
  ASTDWithinGeog (DWithinGeogOp r val sph)  -> applySQLFn "ST_DWithin" [lhs, val, r, sph]

  ANISNULL         -> S.BENull lhs
  ANISNOTNULL      -> S.BENotNull lhs
  CEQ rhsCol       -> S.BECompare S.SEQ lhs $ mkQCol rhsCol
  CNE rhsCol       -> S.BECompare S.SNE lhs $ mkQCol rhsCol
  CGT rhsCol       -> S.BECompare S.SGT lhs $ mkQCol rhsCol
  CLT rhsCol       -> S.BECompare S.SLT lhs $ mkQCol rhsCol
  CGTE rhsCol      -> S.BECompare S.SGTE lhs $ mkQCol rhsCol
  CLTE rhsCol      -> S.BECompare S.SLTE lhs $ mkQCol rhsCol
  where
    mkQCol = S.SEQIden . S.QIden qual . toIden
    lhs = mkQCol lhsCol

    toTextArray arr =
      S.SETyAnn (S.SEArray $ map (txtEncoder . PGValText) arr) S.textArrType

    mkGeomOpBe fn v = applySQLFn fn [lhs, v]

    applySQLFn f exps = S.BEExp $ S.SEFnApp f exps Nothing

    handleEmptyIn []   = S.BELit False
    handleEmptyIn vals = S.BEIN lhs vals

getColExpDeps :: QualifiedTable -> AnnBoolExpFld a -> [SchemaDependency]
getColExpDeps tn = \case
  AVCol colInfo opExps ->
    let cn = pgiName colInfo
        depColsInOpExp = mapMaybe opExpDepCol opExps
        allDepCols = cn:depColsInOpExp
    in map (mkColDep "on_type" tn) allDepCols
  AVRel relInfo relBoolExp ->
    let rn = riName relInfo
        relTN = riRTable relInfo
        pd = SchemaDependency (SOTableObj tn (TORel rn)) "on_type"
    in pd : getBoolExpDeps relTN relBoolExp

getBoolExpDeps :: QualifiedTable -> AnnBoolExp a -> [SchemaDependency]
getBoolExpDeps tn =
  foldr (\annFld deps -> getColExpDeps tn annFld <> deps) []
