module Hasura.RQL.GBoolExp
  ( toSQLBoolExp
  , getBoolExpDeps
  , annBoolExp
  ) where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML      as S

import           Control.Lens        (filtered, has)
import           Data.Aeson
import           Data.Data.Lens      (template)

import qualified Data.HashMap.Strict as M
import qualified Data.Text.Extended  as T

type OpRhsParser m v =
  PGType PGColumnType -> Value -> m v

-- | Represents a reference to a Postgres column, possibly casted an arbitrary
-- number of times. Used within 'parseOperationsExpression' for bookkeeping.
data ColumnReference
  = ColumnReferenceColumn !PGColumnInfo
  | ColumnReferenceCast !ColumnReference !PGColumnType
  deriving (Show, Eq)

columnReferenceType :: ColumnReference -> PGColumnType
columnReferenceType = \case
  ColumnReferenceColumn column -> pgiType column
  ColumnReferenceCast _ targetType -> targetType

instance DQuote ColumnReference where
  dquoteTxt = \case
    ColumnReferenceColumn column ->
      getPGColTxt $ pgiName column
    ColumnReferenceCast reference targetType ->
      dquoteTxt reference <> "::" <> dquoteTxt targetType

parseOperationsExpression
  :: forall m v
   . (MonadError QErr m)
  => OpRhsParser m v
  -> FieldInfoMap PGColumnInfo
  -> PGColumnInfo
  -> Value
  -> m [OpExpG v]
parseOperationsExpression rhsParser fim columnInfo =
  withPathK (getPGColTxt $ pgiName columnInfo) .
    parseOperations (ColumnReferenceColumn columnInfo)
  where
    parseOperations :: ColumnReference -> Value -> m [OpExpG v]
    parseOperations column = \case
      Object o -> mapM (parseOperation column) (M.toList o)
      val      -> pure . AEQ False <$> rhsParser columnType val
      where
        columnType = PGTypeScalar $ columnReferenceType column

    parseOperation :: ColumnReference -> (T.Text, Value) -> m (OpExpG v)
    parseOperation column (opStr, val) = withPathK opStr $
      case opStr of
        "$cast"          -> parseCast
        "_cast"          -> parseCast

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
        "_contains"      -> guardType [PGJSONB] >> AContains <$> parseOne
        "$contains"      -> guardType [PGJSONB] >> AContains <$> parseOne
        "_contained_in"  -> guardType [PGJSONB] >> AContainedIn <$> parseOne
        "$contained_in"  -> guardType [PGJSONB] >> AContainedIn <$> parseOne
        "_has_key"       -> guardType [PGJSONB] >> AHasKey <$> parseWithTy (PGColumnScalar PGText)
        "$has_key"       -> guardType [PGJSONB] >> AHasKey <$> parseWithTy (PGColumnScalar PGText)

        "_has_keys_any"  -> guardType [PGJSONB] >> AHasKeysAny <$> parseManyWithType (PGColumnScalar PGText)
        "$has_keys_any"  -> guardType [PGJSONB] >> AHasKeysAny <$> parseManyWithType (PGColumnScalar PGText)
        "_has_keys_all"  -> guardType [PGJSONB] >> AHasKeysAll <$> parseManyWithType (PGColumnScalar PGText)
        "$has_keys_all"  -> guardType [PGJSONB] >> AHasKeysAll <$> parseManyWithType (PGColumnScalar PGText)

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
        colTy = columnReferenceType column

        parseEq       = AEQ False <$> parseOne -- equals
        parseNe       = ANE False <$> parseOne -- <>
        parseIn       = AIN <$> parseManyWithType colTy -- in an array
        parseNin      = ANIN <$> parseManyWithType colTy -- not in an array
        parseGt       = AGT <$> parseOne -- >
        parseLt       = ALT <$> parseOne -- <
        parseGte      = AGTE <$> parseOne -- >=
        parseLte      = ALTE <$> parseOne -- <=
        parseLike     = guardType stringTypes >> ALIKE <$> parseOne
        parseNlike    = guardType stringTypes >> ANLIKE <$> parseOne
        parseIlike    = guardType stringTypes >> AILIKE <$> parseOne
        parseNilike   = guardType stringTypes >> ANILIKE <$> parseOne
        parseSimilar  = guardType stringTypes >> ASIMILAR <$> parseOne
        parseNsimilar = guardType stringTypes >> ANSIMILAR <$> parseOne

        parseIsNull   = bool ANISNOTNULL ANISNULL -- is null
                        <$> parseVal

        parseCeq      = CEQ <$> decodeAndValidateRhsCol
        parseCne      = CNE <$> decodeAndValidateRhsCol
        parseCgt      = CGT <$> decodeAndValidateRhsCol
        parseClt      = CLT <$> decodeAndValidateRhsCol
        parseCgte     = CGTE <$> decodeAndValidateRhsCol
        parseClte     = CLTE <$> decodeAndValidateRhsCol

        parseCast = do
          castOperations <- parseVal
          parsedCastOperations <-
            forM (M.toList castOperations) $ \(targetTypeName, castedComparisons) -> do
              let targetType = txtToPgColTy targetTypeName
                  castedColumn = ColumnReferenceCast column (PGColumnScalar targetType)
              checkValidCast targetType
              parsedCastedComparisons <- withPathK targetTypeName $
                parseOperations castedColumn castedComparisons
              return (targetType, parsedCastedComparisons)
          return . ACast $ M.fromList parsedCastOperations

        checkValidCast targetType = case (colTy, targetType) of
          (PGColumnScalar PGGeometry, PGGeography) -> return ()
          (PGColumnScalar PGGeography, PGGeometry) -> return ()
          _ -> throw400 UnexpectedPayload $
            "cannot cast column of type " <> colTy <<> " to type " <>> targetType

        parseGeometryOp f =
          guardType [PGGeometry] >> f <$> parseOneNoSess colTy val
        parseGeometryOrGeographyOp f =
          guardType geoTypes >> f <$> parseOneNoSess colTy val

        parseSTDWithinObj = case colTy of
          PGColumnScalar PGGeometry -> do
            DWithinGeomOp distVal fromVal <- parseVal
            dist <- withPathK "distance" $ parseOneNoSess (PGColumnScalar PGFloat) distVal
            from <- withPathK "from" $ parseOneNoSess colTy fromVal
            return $ ASTDWithinGeom $ DWithinGeomOp dist from
          PGColumnScalar PGGeography -> do
            DWithinGeogOp distVal fromVal sphVal <- parseVal
            dist <- withPathK "distance" $ parseOneNoSess (PGColumnScalar PGFloat) distVal
            from <- withPathK "from" $ parseOneNoSess colTy fromVal
            useSpheroid <- withPathK "use_spheroid" $ parseOneNoSess (PGColumnScalar PGBoolean) sphVal
            return $ ASTDWithinGeog $ DWithinGeogOp dist from useSpheroid
          _ -> throwError $ buildMsg colTy [PGGeometry, PGGeography]

        decodeAndValidateRhsCol =
          parseVal >>= validateRhsCol

        validateRhsCol rhsCol = do
          let errMsg = "column operators can only compare postgres columns"
          rhsType <- askPGType fim rhsCol errMsg
          if colTy /= rhsType
            then throw400 UnexpectedPayload $
                 "incompatible column types : " <> column <<> ", " <>> rhsCol
            else return rhsCol

        parseWithTy ty = rhsParser (PGTypeScalar ty) val

        -- parse one with the column's type
        parseOne = parseWithTy colTy
        parseOneNoSess ty = rhsParser (PGTypeScalar ty)

        parseManyWithType ty = rhsParser (PGTypeArray ty) val

        guardType validTys = unless (isScalarColumnWhere (`elem` validTys) colTy) $
          throwError $ buildMsg colTy validTys
        buildMsg ty expTys = err400 UnexpectedPayload
          $ " is of type " <> ty <<> "; this operator works only on columns of type "
          <> T.intercalate "/" (map dquote expTys)

        parseVal :: (FromJSON a) => m a
        parseVal = decodeValue val

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
  => OpRhsParser m v
  -> FieldInfoMap PGColumnInfo
  -> BoolExp
  -> m (AnnBoolExp v)
annBoolExp rhsParser fim (BoolExp boolExp) =
  traverse (annColExp rhsParser fim) boolExp

annColExp
  :: (QErrM m, CacheRM m)
  => OpRhsParser m v
  -> FieldInfoMap PGColumnInfo
  -> ColExp
  -> m (AnnBoolExpFld v)
annColExp rhsParser colInfoMap (ColExp fieldName colVal) = do
  colInfo <- askFieldInfo colInfoMap fieldName
  case colInfo of
    FIColumn (PGColumnInfo _ (PGColumnScalar PGJSON) _) ->
      throwError (err400 UnexpectedPayload "JSON column can not be part of where clause")
    FIColumn pgi ->
      AVCol pgi <$> parseOperationsExpression rhsParser colInfoMap pgi colVal
    FIRelationship relInfo -> do
      relBoolExp      <- decodeValue colVal
      relFieldInfoMap <- askFieldInfoMap $ riRTable relInfo
      annRelBoolExp   <- annBoolExp rhsParser relFieldInfoMap relBoolExp
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
  AVCol (PGColumnInfo cn _ _) opExps -> do
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

mkColCompExp
  :: S.Qual -> PGCol -> OpExpG S.SQLExp -> S.BoolExp
mkColCompExp qual lhsCol = mkCompExp (mkQCol lhsCol)
  where
    mkQCol = S.SEQIden . S.QIden qual . toIden

    mkCompExp :: S.SQLExp -> OpExpG S.SQLExp -> S.BoolExp
    mkCompExp lhs = \case
      ACast casts      -> mkCastsExp casts
      AEQ False val    -> equalsBoolExpBuilder lhs val
      AEQ True val     -> S.BECompare S.SEQ lhs val
      ANE False val    -> notEqualsBoolExpBuilder lhs val
      ANE True  val    -> S.BECompare S.SNE lhs val

      AIN val          -> S.BECompareAny S.SEQ lhs val
      ANIN val         -> S.BENot $ S.BECompareAny S.SEQ lhs val

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

      AHasKeysAny val  -> S.BECompare S.SHasKeysAny lhs val
      AHasKeysAll val  -> S.BECompare S.SHasKeysAll lhs val

      ASTContains val   -> mkGeomOpBe "ST_Contains" val
      ASTCrosses val    -> mkGeomOpBe "ST_Crosses" val
      ASTEquals val     -> mkGeomOpBe "ST_Equals" val
      ASTIntersects val -> mkGeomOpBe "ST_Intersects" val
      ASTOverlaps val   -> mkGeomOpBe "ST_Overlaps" val
      ASTTouches val    -> mkGeomOpBe "ST_Touches" val
      ASTWithin val     -> mkGeomOpBe "ST_Within" val
      ASTDWithinGeom (DWithinGeomOp r val)     ->
        applySQLFn "ST_DWithin" [lhs, val, r]
      ASTDWithinGeog (DWithinGeogOp r val sph) ->
        applySQLFn "ST_DWithin" [lhs, val, r, sph]

      ANISNULL         -> S.BENull lhs
      ANISNOTNULL      -> S.BENotNull lhs
      CEQ rhsCol       -> S.BECompare S.SEQ lhs $ mkQCol rhsCol
      CNE rhsCol       -> S.BECompare S.SNE lhs $ mkQCol rhsCol
      CGT rhsCol       -> S.BECompare S.SGT lhs $ mkQCol rhsCol
      CLT rhsCol       -> S.BECompare S.SLT lhs $ mkQCol rhsCol
      CGTE rhsCol      -> S.BECompare S.SGTE lhs $ mkQCol rhsCol
      CLTE rhsCol      -> S.BECompare S.SLTE lhs $ mkQCol rhsCol
      where
        mkGeomOpBe fn v = applySQLFn fn [lhs, v]

        applySQLFn f exps = S.BEExp $ S.SEFnApp f exps Nothing

        mkCastsExp casts =
          sqlAll . flip map (M.toList casts) $ \(targetType, operations) ->
            let targetAnn = S.mkTypeAnn $ PGTypeScalar targetType
            in sqlAll $ map (mkCompExp (S.SETyAnn lhs targetAnn)) operations

        sqlAll = foldr (S.BEBin S.AndOp) (S.BELit True)

hasStaticExp :: OpExpG PartialSQLExp -> Bool
hasStaticExp = has (template . filtered isStaticValue)

getColExpDeps
  :: QualifiedTable -> AnnBoolExpFldPartialSQL -> [SchemaDependency]
getColExpDeps tn = \case
  AVCol colInfo opExps ->
    let cn = pgiName colInfo
        colDepReason = bool DRSessionVariable DROnType $ any hasStaticExp opExps
        colDep = mkColDep colDepReason tn cn
        depColsInOpExp = mapMaybe opExpDepCol opExps
        colDepsInOpExp = map (mkColDep DROnType tn) depColsInOpExp
    in colDep:colDepsInOpExp
  AVRel relInfo relBoolExp ->
    let rn = riName relInfo
        relTN = riRTable relInfo
        pd = SchemaDependency (SOTableObj tn (TORel rn)) DROnType
    in pd : getBoolExpDeps relTN relBoolExp

getBoolExpDeps :: QualifiedTable -> AnnBoolExpPartialSQL -> [SchemaDependency]
getBoolExpDeps tn =
  foldr (\annFld deps -> getColExpDeps tn annFld <> deps) []
