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
parseOpExp parser fim (PGColInfo cn colTy _) (opStr, val) = case opStr of
  "$eq"       -> parseEq
  "_eq"       -> parseEq

  "$ne"       -> parseNe
  "_ne"       -> parseNe
  "$neq"      -> parseNe
  "_neq"      -> parseNe

  "$in"       -> parseIn
  "_in"       -> parseIn

  "$nin"      -> parseNin
  "_nin"      -> parseNin

  "$gt"       -> parseGt
  "_gt"       -> parseGt

  "$lt"       -> parseLt
  "_lt"       -> parseLt

  "$gte"      -> parseGte
  "_gte"      -> parseGte

  "$lte"      -> parseLte
  "_lte"      -> parseLte

  "$like"     -> parseLike
  "_like"     -> parseLike

  "$nlike"    -> parseNlike
  "_nlike"    -> parseNlike

  "$ilike"    -> parseIlike
  "_ilike"    -> parseIlike

  "$nilike"   -> parseNilike
  "_nilike"   -> parseNilike

  "$similar"  -> parseSimilar
  "_similar"  -> parseSimilar
  "$nsimilar" -> parseNsimilar
  "_nsimilar" -> parseNsimilar

  "$is_null"  -> parseIsNull
  "_is_null"  -> parseIsNull

  "$ceq"      -> parseCeq
  "_ceq"      -> parseCeq

  "$cne"      -> parseCne
  "_cne"      -> parseCne
  "$cneq"     -> parseCne
  "_cneq"     -> parseCne

  "$cgt"      -> parseCgt
  "_cgt"      -> parseCgt

  "$clt"      -> parseClt
  "_clt"      -> parseClt

  "$cgte"     -> parseCgte
  "_cgte"     -> parseCgte

  "$clte"     -> parseClte
  "_clte"     -> parseClte

  x           -> throw400 UnexpectedPayload $ "Unknown operator : " <> x
  where
    parseEq       = AEQ <$> parseOne -- equals
    parseNe       = ANE <$> parseOne -- <>
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
                    <$> decodeValue val
    parseCeq      = CEQ <$> decodeAndValidateRhsCol
    parseCne      = CNE <$> decodeAndValidateRhsCol
    parseCgt      = CGT <$> decodeAndValidateRhsCol
    parseClt      = CLT <$> decodeAndValidateRhsCol
    parseCgte     = CGTE <$> decodeAndValidateRhsCol
    parseClte     = CLTE <$> decodeAndValidateRhsCol

    decodeAndValidateRhsCol =
      decodeValue val >>= validateRhsCol

    validateRhsCol rhsCol = do
      let errMsg = "column operators can only compare postgres columns"
      rhsType <- askPGType fim rhsCol errMsg
      if colTy /= rhsType
        then throw400 UnexpectedPayload $
             "incompatible column types : " <> cn <<> ", " <>> rhsCol
        else return rhsCol

    parseOne = parser colTy val
    parseMany = do
      vals <- runAesonParser parseJSON val
      indexedForM vals (parser colTy)

parseOpExps
  :: (MonadError QErr m)
  => ValueParser m a
  -> FieldInfoMap
  -> PGColInfo
  -> Value
  -> m [OpExpG a]
parseOpExps valParser cim colInfo = \case
  (Object o) -> mapM (parseOpExp valParser cim colInfo)(M.toList o)
  val        -> pure . AEQ <$> valParser (pgiType colInfo) val

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
    FIColumn (PGColInfo _ PGJSONB _) ->
      throwError (err400 UnexpectedPayload "JSONB column can not be part of where clause")
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
  txtEncoder <$> pgValParser ty val

mkColCompExp
  :: S.Qual -> PGCol -> OpExpG S.SQLExp -> S.BoolExp
mkColCompExp qual lhsCol = \case
  AEQ val          -> equalsBoolExpBuilder lhs val
  ANE val          -> notEqualsBoolExpBuilder lhs val
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
  ASTDWithin r val  -> applySQLFn "ST_DWithin" [lhs, val, r]
  ASTEquals val     -> mkGeomOpBe "ST_Equals" val
  ASTIntersects val -> mkGeomOpBe "ST_Intersects" val
  ASTOverlaps val   -> mkGeomOpBe "ST_Overlaps" val
  ASTTouches val    -> mkGeomOpBe "ST_Touches" val
  ASTWithin val     -> mkGeomOpBe "ST_Within" val

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
  AVCol colInfo _ ->
    let cn = pgiName colInfo
    in [SchemaDependency (SOTableObj tn (TOCol cn)) "on_type"]
  AVRel relInfo relBoolExp ->
    let rn = riName relInfo
        relTN = riRTable relInfo
        pd = SchemaDependency (SOTableObj tn (TORel rn)) "on_type"
    in pd : getBoolExpDeps relTN relBoolExp

getBoolExpDeps :: QualifiedTable -> AnnBoolExp a -> [SchemaDependency]
getBoolExpDeps tn =
  foldr (\annFld deps -> getColExpDeps tn annFld <> deps) []
