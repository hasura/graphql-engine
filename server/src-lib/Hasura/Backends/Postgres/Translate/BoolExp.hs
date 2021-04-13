{-# LANGUAGE PartialTypeSignatures #-}
module Hasura.Backends.Postgres.Translate.BoolExp
  ( toSQLBoolExp
  , getBoolExpDeps
  , annBoolExp
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                      as M

import qualified Hasura.Backends.Postgres.SQL.DML         as S

import           Hasura.Backends.Postgres.Instances.Types ()
import           Hasura.Backends.Postgres.SQL.Types       hiding (TableName)
import           Hasura.Backends.Postgres.Types.BoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Types


-- This convoluted expression instead of col = val
-- to handle the case of col : null
equalsBoolExpBuilder :: SQLExpression 'Postgres -> SQLExpression 'Postgres -> S.BoolExp
equalsBoolExpBuilder qualColExp rhsExp =
  S.BEBin S.OrOp (S.BECompare S.SEQ qualColExp rhsExp)
    (S.BEBin S.AndOp
      (S.BENull qualColExp)
      (S.BENull rhsExp))

notEqualsBoolExpBuilder :: SQLExpression 'Postgres -> SQLExpression 'Postgres -> S.BoolExp
notEqualsBoolExpBuilder qualColExp rhsExp =
  S.BEBin S.OrOp (S.BECompare S.SNE qualColExp rhsExp)
    (S.BEBin S.AndOp
      (S.BENotNull qualColExp)
      (S.BENull rhsExp))

annBoolExp
  :: (QErrM m, TableCoreInfoRM b m, BackendMetadata b)
  => ValueParser b m v
  -> FieldInfoMap (FieldInfo b)
  -> GBoolExp b ColExp
  -> m (AnnBoolExp b v)
annBoolExp rhsParser fim boolExp =
  case boolExp of
    BoolAnd exps -> BoolAnd <$> procExps exps
    BoolOr exps  -> BoolOr <$> procExps exps
    BoolNot e    -> BoolNot <$> annBoolExp rhsParser fim e
    BoolExists (GExists refqt whereExp) ->
      withPathK "_exists" $ do
        refFields <- withPathK "_table" $ askFieldInfoMapSource refqt
        annWhereExp <- withPathK "_where" $
                       annBoolExp rhsParser refFields whereExp
        return $ BoolExists $ GExists refqt annWhereExp
    BoolFld fld -> BoolFld <$> annColExp rhsParser fim fld
  where
    procExps = mapM (annBoolExp rhsParser fim)

annColExp
  :: (QErrM m, TableCoreInfoRM b m, BackendMetadata b)
  => ValueParser b m v
  -> FieldInfoMap (FieldInfo b)
  -> ColExp
  -> m (AnnBoolExpFld b v)
annColExp rhsParser colInfoMap (ColExp fieldName colVal) = do
  colInfo <- askFieldInfo colInfoMap fieldName
  case colInfo of
    FIColumn pgi -> AVCol pgi <$> parseBoolExpOperations rhsParser colInfoMap pgi colVal
    FIRelationship relInfo -> do
      relBoolExp      <- decodeValue colVal
      relFieldInfoMap <- askFieldInfoMapSource $ riRTable relInfo
      annRelBoolExp   <- annBoolExp rhsParser relFieldInfoMap $
                         unBoolExp relBoolExp
      return $ AVRel relInfo annRelBoolExp
    FIComputedField _ ->
      throw400 UnexpectedPayload "Computed columns can not be part of the where clause"
    -- TODO Rakesh (from master)
    FIRemoteRelationship{} ->
      throw400 UnexpectedPayload "remote field unsupported"

toSQLBoolExp
  :: S.Qual -> AnnBoolExpSQL 'Postgres -> S.BoolExp
toSQLBoolExp tq e =
  evalState (convBoolRhs' tq e) 0

convBoolRhs'
  :: S.Qual -> AnnBoolExpSQL 'Postgres -> State Word64 S.BoolExp
convBoolRhs' tq =
  foldBoolExp (convColRhs tq)

convColRhs
  :: S.Qual -> AnnBoolExpFldSQL 'Postgres -> State Word64 S.BoolExp
convColRhs tableQual = \case
  AVCol colInfo opExps -> do
    let colFld = fromCol @'Postgres $ pgiColumn colInfo
        bExps = map (mkFieldCompExp tableQual colFld) opExps
    return $ foldr (S.BEBin S.AndOp) (S.BELit True) bExps

  AVRel (RelInfo _ _ colMapping relTN _ _ _) nesAnn -> do
    -- Convert the where clause on the relationship
    curVarNum <- get
    put $ curVarNum + 1
    let newIdentifier  = Identifier $ "_be_" <> tshow curVarNum <> "_"
                   <> snakeCaseQualifiedObject relTN
        newIdenQ = S.QualifiedIdentifier newIdentifier Nothing
    annRelBoolExp <- convBoolRhs' newIdenQ nesAnn
    let backCompExp = foldr (S.BEBin S.AndOp) (S.BELit True) $
          flip map (M.toList colMapping) $ \(lCol, rCol) ->
            S.BECompare S.SEQ
            (mkQCol (S.QualifiedIdentifier newIdentifier Nothing) rCol)
            (mkQCol tableQual lCol)
        innerBoolExp = S.BEBin S.AndOp backCompExp annRelBoolExp
    return $ S.mkExists (S.FISimple relTN $ Just $ S.Alias newIdentifier) innerBoolExp
  where
    mkQCol q = S.SEQIdentifier . S.QIdentifier q . toIdentifier

foldExists :: GExists 'Postgres (AnnBoolExpFldSQL 'Postgres) -> State Word64 S.BoolExp
foldExists (GExists qt wh) = do
  whereExp <- foldBoolExp (convColRhs (S.QualTable qt)) wh
  return $ S.mkExists (S.FISimple qt Nothing) whereExp

foldBoolExp
  :: (AnnBoolExpFldSQL 'Postgres -> State Word64 S.BoolExp)
  -> AnnBoolExpSQL 'Postgres
  -> State Word64 S.BoolExp
foldBoolExp f = \case
  BoolAnd bes           -> do
    sqlBExps <- mapM (foldBoolExp f) bes
    return $ foldr (S.BEBin S.AndOp) (S.BELit True) sqlBExps

  BoolOr bes           -> do
    sqlBExps <- mapM (foldBoolExp f) bes
    return $ foldr (S.BEBin S.OrOp) (S.BELit False) sqlBExps

  BoolNot notExp       -> S.BENot <$> foldBoolExp f notExp
  BoolExists existsExp -> foldExists existsExp
  BoolFld ce           -> f ce

mkFieldCompExp
  :: S.Qual -> FieldName -> OpExpG 'Postgres S.SQLExp -> S.BoolExp
mkFieldCompExp qual lhsField = mkCompExp (mkQField lhsField)
  where
    mkQCol = S.SEQIdentifier . S.QIdentifier qual . toIdentifier
    mkQField = S.SEQIdentifier . S.QIdentifier qual . Identifier . getFieldNameTxt

    mkCompExp :: SQLExpression 'Postgres -> OpExpG 'Postgres (SQLExpression 'Postgres) -> S.BoolExp
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

      CEQ rhsCol       -> S.BECompare S.SEQ lhs $ mkQCol rhsCol
      CNE rhsCol       -> S.BECompare S.SNE lhs $ mkQCol rhsCol
      CGT rhsCol       -> S.BECompare S.SGT lhs $ mkQCol rhsCol
      CLT rhsCol       -> S.BECompare S.SLT lhs $ mkQCol rhsCol
      CGTE rhsCol      -> S.BECompare S.SGTE lhs $ mkQCol rhsCol
      CLTE rhsCol      -> S.BECompare S.SLTE lhs $ mkQCol rhsCol

      ANISNULL         -> S.BENull lhs
      ANISNOTNULL      -> S.BENotNull lhs

      ABackendSpecific (AILIKE       val)     -> S.BECompare S.SILIKE lhs val
      ABackendSpecific (ANILIKE      val)     -> S.BECompare S.SNILIKE lhs val
      ABackendSpecific (ASIMILAR     val)     -> S.BECompare S.SSIMILAR lhs val
      ABackendSpecific (ANSIMILAR    val)     -> S.BECompare S.SNSIMILAR lhs val
      ABackendSpecific (AREGEX       val)     -> S.BECompare S.SREGEX lhs val
      ABackendSpecific (AIREGEX      val)     -> S.BECompare S.SIREGEX lhs val
      ABackendSpecific (ANREGEX      val)     -> S.BECompare S.SNREGEX lhs val
      ABackendSpecific (ANIREGEX     val)     -> S.BECompare S.SNIREGEX lhs val
      ABackendSpecific (AContains    val)     -> S.BECompare S.SContains lhs val
      ABackendSpecific (AContainedIn val)     -> S.BECompare S.SContainedIn lhs val

      ABackendSpecific (AHasKey     val)      -> S.BECompare S.SHasKey lhs val
      ABackendSpecific (AHasKeysAny val)      -> S.BECompare S.SHasKeysAny lhs val
      ABackendSpecific (AHasKeysAll val)      -> S.BECompare S.SHasKeysAll lhs val

      ABackendSpecific (AAncestor        val) -> S.BECompare S.SContains lhs val
      ABackendSpecific (AAncestorAny     val) -> S.BECompare S.SContains lhs val
      ABackendSpecific (ADescendant      val) -> S.BECompare S.SContainedIn lhs val
      ABackendSpecific (ADescendantAny   val) -> S.BECompare S.SContainedIn lhs val
      ABackendSpecific (AMatches         val) -> S.BECompare S.SREGEX lhs val
      ABackendSpecific (AMatchesAny      val) -> S.BECompare S.SHasKey lhs val
      ABackendSpecific (AMatchesFulltext val) -> S.BECompare S.SMatchesFulltext lhs val

      ABackendSpecific (ASTContains   val)    -> mkGeomOpBe "ST_Contains" val
      ABackendSpecific (ASTCrosses    val)    -> mkGeomOpBe "ST_Crosses" val
      ABackendSpecific (ASTEquals     val)    -> mkGeomOpBe "ST_Equals" val
      ABackendSpecific (ASTIntersects val)    -> mkGeomOpBe "ST_Intersects" val
      ABackendSpecific (ASTOverlaps   val)    -> mkGeomOpBe "ST_Overlaps" val
      ABackendSpecific (ASTTouches    val)    -> mkGeomOpBe "ST_Touches" val
      ABackendSpecific (ASTWithin     val)    -> mkGeomOpBe "ST_Within" val

      ABackendSpecific (ASTDWithinGeom (DWithinGeomOp r val)     ) ->
        applySQLFn "ST_DWithin" [lhs, val, r]
      ABackendSpecific (ASTDWithinGeog (DWithinGeogOp r val sph) ) ->
        applySQLFn "ST_DWithin" [lhs, val, r, sph]

      ABackendSpecific (ASTIntersectsRast val ) ->
        applySTIntersects [lhs, val]
      ABackendSpecific (ASTIntersectsNbandGeom (STIntersectsNbandGeommin nband geommin) ) ->
        applySTIntersects [lhs, nband, geommin]
      ABackendSpecific (ASTIntersectsGeomNband (STIntersectsGeomminNband geommin mNband)) ->
        applySTIntersects [lhs, geommin, withSQLNull mNband]

      where
        mkGeomOpBe fn v = applySQLFn fn [lhs, v]

        applySQLFn f exps = S.BEExp $ S.SEFnApp f exps Nothing

        applySTIntersects = applySQLFn "ST_Intersects"

        withSQLNull = fromMaybe S.SENull

        mkCastsExp casts =
          sqlAll . flip map (M.toList casts) $ \(targetType, operations) ->
            let targetAnn = S.mkTypeAnn $ CollectableTypeScalar targetType
            in sqlAll $ map (mkCompExp (S.SETyAnn lhs targetAnn)) operations

        sqlAll = foldr (S.BEBin S.AndOp) (S.BELit True)
