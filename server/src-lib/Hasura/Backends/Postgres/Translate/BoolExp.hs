{-# LANGUAGE PartialTypeSignatures #-}

-- | Postgres Translate BoolExp
--
-- Convert IR boolean expressions to Postgres-specific SQL expressions.
module Hasura.Backends.Postgres.Translate.BoolExp
  ( toSQLBoolExp,
    annBoolExp,
  )
where

import Data.HashMap.Strict qualified as M
import Data.Text.Extended (ToTxt)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Backends.Postgres.Types.BoolExp
import Hasura.Backends.Postgres.Types.Function (onArgumentExp)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BoolExp
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache hiding (BoolExpCtx (..), BoolExpM (..))
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend
import Hasura.SQL.Types

-- This convoluted expression instead of col = val
-- to handle the case of col : null
equalsBoolExpBuilder :: SQLExpression ('Postgres pgKind) -> SQLExpression ('Postgres pgKind) -> S.BoolExp
equalsBoolExpBuilder qualColExp rhsExp =
  S.BEBin
    S.OrOp
    (S.BECompare S.SEQ qualColExp rhsExp)
    ( S.BEBin
        S.AndOp
        (S.BENull qualColExp)
        (S.BENull rhsExp)
    )

notEqualsBoolExpBuilder :: SQLExpression ('Postgres pgKind) -> SQLExpression ('Postgres pgKind) -> S.BoolExp
notEqualsBoolExpBuilder qualColExp rhsExp =
  S.BEBin
    S.OrOp
    (S.BECompare S.SNE qualColExp rhsExp)
    ( S.BEBin
        S.AndOp
        (S.BENotNull qualColExp)
        (S.BENull rhsExp)
    )

annBoolExp ::
  (QErrM m, TableCoreInfoRM b m, BackendMetadata b) =>
  BoolExpRHSParser b m v ->
  TableName b ->
  FieldInfoMap (FieldInfo b) ->
  GBoolExp b ColExp ->
  m (AnnBoolExp b v)
annBoolExp rhsParser rootTable fim boolExp =
  case boolExp of
    BoolAnd exps -> BoolAnd <$> procExps exps
    BoolOr exps -> BoolOr <$> procExps exps
    BoolNot e -> BoolNot <$> annBoolExp rhsParser rootTable fim e
    BoolExists (GExists refqt whereExp) ->
      withPathK "_exists" $ do
        refFields <- withPathK "_table" $ askFieldInfoMapSource refqt
        annWhereExp <- withPathK "_where" $ annBoolExp rhsParser rootTable refFields whereExp
        return $ BoolExists $ GExists refqt annWhereExp
    BoolField fld -> BoolField <$> annColExp rhsParser rootTable fim fld
  where
    procExps = mapM (annBoolExp rhsParser rootTable fim)

annColExp ::
  (QErrM m, TableCoreInfoRM b m, BackendMetadata b) =>
  BoolExpRHSParser b m v ->
  TableName b ->
  FieldInfoMap (FieldInfo b) ->
  ColExp ->
  m (AnnBoolExpFld b v)
annColExp rhsParser rootTable colInfoMap (ColExp fieldName colVal) = do
  colInfo <- askFieldInfo colInfoMap fieldName
  case colInfo of
    FIColumn pgi -> AVColumn pgi <$> parseBoolExpOperations (_berpValueParser rhsParser) rootTable colInfoMap (ColumnReferenceColumn pgi) colVal
    FIRelationship relInfo -> do
      relBoolExp <- decodeValue colVal
      relFieldInfoMap <- askFieldInfoMapSource $ riRTable relInfo
      annRelBoolExp <- annBoolExp rhsParser rootTable relFieldInfoMap $ unBoolExp relBoolExp
      return $ AVRelationship relInfo annRelBoolExp
    FIComputedField computedFieldInfo ->
      AVComputedField <$> buildComputedFieldBooleanExp (BoolExpResolver annBoolExp) rhsParser rootTable colInfoMap computedFieldInfo colVal
    -- Using remote fields in the boolean expression is not supported.
    FIRemoteRelationship {} ->
      throw400 UnexpectedPayload "remote field unsupported"

-- | Translate an IR boolean expression to an SQL boolean expression. References
-- to columns etc are relative to the given 'rootReference'.
toSQLBoolExp ::
  forall pgKind.
  Backend ('Postgres pgKind) =>
  -- | The name of the tabular value in query scope that the boolean expression
  -- applies to
  S.Qual ->
  -- | The boolean expression to translate
  AnnBoolExpSQL ('Postgres pgKind) ->
  S.BoolExp
toSQLBoolExp rootReference e =
  evalState
    ( runReaderT
        (unBoolExpM (translateBoolExp e))
        initialCtx
    )
    0
  where
    initialCtx =
      BoolExpCtx
        { currTableReference = rootReference,
          rootReference = rootReference
        }

-- | The table context of boolean expression translation. This is used to
-- resolve references to fields, as those may refer to the so-called 'root
-- table' (identified by a '$'-sign in the expression input syntax) or the
-- 'current' table.
data BoolExpCtx = BoolExpCtx
  { -- | Reference to the current tabular value.
    currTableReference :: S.Qual,
    -- | Reference to the root tabular value.
    rootReference :: S.Qual
  }

-- | The monad that carries the translation of boolean expressions. This
-- supports the generation of fresh names for aliasing sub-expressions and
-- maintains the table context of the expressions being translated.
newtype BoolExpM a = BoolExpM {unBoolExpM :: ReaderT BoolExpCtx (State Word64) a}
  deriving (Functor, Applicative, Monad, MonadReader BoolExpCtx, MonadState Word64)

-- | Translate a 'GBoolExp' with annotated SQLExpressions in the leaves into a
-- bare SQL Boolean Expression.
translateBoolExp ::
  forall pgKind.
  (Backend ('Postgres pgKind)) =>
  AnnBoolExpSQL ('Postgres pgKind) ->
  BoolExpM S.BoolExp
translateBoolExp = \case
  BoolAnd bes -> do
    sqlBExps <- mapM translateBoolExp bes
    return $ foldr (S.BEBin S.AndOp) (S.BELit True) sqlBExps
  BoolOr bes -> do
    sqlBExps <- mapM translateBoolExp bes
    return $ foldr (S.BEBin S.OrOp) (S.BELit False) sqlBExps
  BoolNot notExp -> S.BENot <$> translateBoolExp notExp
  BoolExists (GExists currTableReference wh) -> do
    whereExp <- recCurrentTable (S.QualTable currTableReference) wh
    return $ S.mkExists (S.FISimple currTableReference Nothing) whereExp
  BoolField boolExp -> case boolExp of
    AVColumn colInfo opExps -> do
      BoolExpCtx {rootReference, currTableReference} <- ask
      let colFld = fromCol @('Postgres pgKind) $ ciColumn colInfo
          bExps = map (mkFieldCompExp rootReference currTableReference $ LColumn colFld) opExps
      return $ foldr (S.BEBin S.AndOp) (S.BELit True) bExps
    AVRelationship (RelInfo _ _ colMapping relTN _ _) nesAnn -> do
      -- Convert the where clause on the relationship
      aliasRelTN <- freshIdentifier relTN
      annRelBoolExp <- recCurrentTable (S.QualifiedIdentifier aliasRelTN Nothing) nesAnn
      BoolExpCtx {currTableReference} <- ask
      let backCompExp = foldr (S.BEBin S.AndOp) (S.BELit True) $
            flip map (M.toList colMapping) $ \(lCol, rCol) ->
              S.BECompare
                S.SEQ
                (mkQCol (S.QualifiedIdentifier aliasRelTN Nothing) rCol)
                (mkQCol currTableReference lCol)
          innerBoolExp = S.BEBin S.AndOp backCompExp annRelBoolExp
      return $ S.mkExists (S.FISimple relTN $ Just $ S.toTableAlias aliasRelTN) innerBoolExp
    AVComputedField (AnnComputedFieldBoolExp _ _ function sessionArgPresence cfBoolExp) -> do
      case cfBoolExp of
        CFBEScalar opExps -> do
          BoolExpCtx {rootReference, currTableReference} <- ask
          -- Convert the where clause on scalar computed field
          let bExps = map (mkFieldCompExp rootReference currTableReference $ LComputedField function sessionArgPresence) opExps
          pure $ foldr (S.BEBin S.AndOp) (S.BELit True) bExps
        CFBETable _ be -> do
          -- Convert the where clause on table computed field
          BoolExpCtx {currTableReference} <- ask
          aliasFunction <- freshIdentifier function
          let functionExp =
                mkComputedFieldFunctionExp currTableReference function sessionArgPresence $
                  Just $ S.toTableAlias aliasFunction
          S.mkExists (S.FIFunc functionExp) <$> recCurrentTable (S.QualifiedIdentifier aliasFunction Nothing) be
  where
    mkQCol :: forall a. IsIdentifier a => S.Qual -> a -> S.SQLExp
    mkQCol q = S.SEQIdentifier . S.QIdentifier q . toIdentifier

    -- Draw a fresh identifier intended to alias the given object.
    freshIdentifier :: forall a. ToTxt a => QualifiedObject a -> BoolExpM Identifier
    freshIdentifier obj = do
      curVarNum <- get
      put $ curVarNum + 1
      let newIdentifier =
            Identifier $
              "_be_" <> tshow curVarNum <> "_"
                <> snakeCaseQualifiedObject obj

      return newIdentifier

    -- Call recursively using the given identifier for the 'current' table.
    recCurrentTable :: S.Qual -> AnnBoolExpSQL ('Postgres pgKind) -> BoolExpM S.BoolExp
    recCurrentTable curr = local (\e -> e {currTableReference = curr}) . translateBoolExp

data LHSField b
  = LColumn FieldName
  | LComputedField QualifiedFunction (FunctionArgsExp b (SQLExpression b))

mkComputedFieldFunctionExp ::
  S.Qual ->
  QualifiedFunction ->
  FunctionArgsExp ('Postgres pgKind) (SQLExpression ('Postgres pgKind)) ->
  Maybe S.TableAlias ->
  S.FunctionExp
mkComputedFieldFunctionExp qual function functionArgs alias =
  -- "function_schema"."function_name"("qual".*)
  let tableRowInput = S.SEStar $ Just qual
      resolvedFunctionArgs =
        let FunctionArgsExp {..} = fmap (onArgumentExp tableRowInput (S.SEIdentifier . Identifier)) functionArgs
         in S.FunctionArgs _faePositional _faeNamed
   in S.FunctionExp function resolvedFunctionArgs $ flip S.FunctionAlias Nothing <$> alias

mkFieldCompExp ::
  S.Qual -> S.Qual -> LHSField ('Postgres pgKind) -> OpExpG ('Postgres pgKind) S.SQLExp -> S.BoolExp
mkFieldCompExp rootReference currTableReference lhsField = mkCompExp qLhsField
  where
    qLhsField = case lhsField of
      LColumn fieldName ->
        -- "qual"."column" =
        S.SEQIdentifier $ S.QIdentifier currTableReference $ Identifier $ getFieldNameTxt fieldName
      LComputedField function sessionArgPresence ->
        -- "function_schema"."function_name"("qual".*) =
        S.SEFunction $ mkComputedFieldFunctionExp currTableReference function sessionArgPresence Nothing

    mkQCol :: RootOrCurrentColumn ('Postgres pgKind) -> S.SQLExp
    mkQCol (RootOrCurrentColumn IsRoot col) = S.SEQIdentifier $ S.QIdentifier rootReference $ toIdentifier col
    mkQCol (RootOrCurrentColumn IsCurrent col) = S.SEQIdentifier $ S.QIdentifier currTableReference $ toIdentifier col

    mkCompExp :: SQLExpression ('Postgres pgKind) -> OpExpG ('Postgres pgKind) (SQLExpression ('Postgres pgKind)) -> S.BoolExp
    mkCompExp lhs = \case
      ACast casts -> mkCastsExp casts
      AEQ False val -> equalsBoolExpBuilder lhs val
      AEQ True val -> S.BECompare S.SEQ lhs val
      ANE False val -> notEqualsBoolExpBuilder lhs val
      ANE True val -> S.BECompare S.SNE lhs val
      AIN val -> S.BECompareAny S.SEQ lhs val
      ANIN val -> S.BENot $ S.BECompareAny S.SEQ lhs val
      AGT val -> S.BECompare S.SGT lhs val
      ALT val -> S.BECompare S.SLT lhs val
      AGTE val -> S.BECompare S.SGTE lhs val
      ALTE val -> S.BECompare S.SLTE lhs val
      ALIKE val -> S.BECompare S.SLIKE lhs val
      ANLIKE val -> S.BECompare S.SNLIKE lhs val
      CEQ rhsCol -> S.BECompare S.SEQ lhs $ mkQCol rhsCol
      CNE rhsCol -> S.BECompare S.SNE lhs $ mkQCol rhsCol
      CGT rhsCol -> S.BECompare S.SGT lhs $ mkQCol rhsCol
      CLT rhsCol -> S.BECompare S.SLT lhs $ mkQCol rhsCol
      CGTE rhsCol -> S.BECompare S.SGTE lhs $ mkQCol rhsCol
      CLTE rhsCol -> S.BECompare S.SLTE lhs $ mkQCol rhsCol
      ANISNULL -> S.BENull lhs
      ANISNOTNULL -> S.BENotNull lhs
      ABackendSpecific op -> case op of
        AILIKE val -> S.BECompare S.SILIKE lhs val
        ANILIKE val -> S.BECompare S.SNILIKE lhs val
        ASIMILAR val -> S.BECompare S.SSIMILAR lhs val
        ANSIMILAR val -> S.BECompare S.SNSIMILAR lhs val
        AREGEX val -> S.BECompare S.SREGEX lhs val
        AIREGEX val -> S.BECompare S.SIREGEX lhs val
        ANREGEX val -> S.BECompare S.SNREGEX lhs val
        ANIREGEX val -> S.BECompare S.SNIREGEX lhs val
        AContains val -> S.BECompare S.SContains lhs val
        AContainedIn val -> S.BECompare S.SContainedIn lhs val
        AHasKey val -> S.BECompare S.SHasKey lhs val
        AHasKeysAny val -> S.BECompare S.SHasKeysAny lhs val
        AHasKeysAll val -> S.BECompare S.SHasKeysAll lhs val
        AAncestor val -> S.BECompare S.SContains lhs val
        AAncestorAny val -> S.BECompare S.SContains lhs val
        ADescendant val -> S.BECompare S.SContainedIn lhs val
        ADescendantAny val -> S.BECompare S.SContainedIn lhs val
        AMatches val -> S.BECompare S.SREGEX lhs val
        AMatchesAny val -> S.BECompare S.SHasKey lhs val
        AMatchesFulltext val -> S.BECompare S.SMatchesFulltext lhs val
        ASTContains val -> mkGeomOpBe "ST_Contains" val
        ASTCrosses val -> mkGeomOpBe "ST_Crosses" val
        ASTEquals val -> mkGeomOpBe "ST_Equals" val
        ASTIntersects val -> mkGeomOpBe "ST_Intersects" val
        AST3DIntersects val -> mkGeomOpBe "ST_3DIntersects" val
        ASTOverlaps val -> mkGeomOpBe "ST_Overlaps" val
        ASTTouches val -> mkGeomOpBe "ST_Touches" val
        ASTWithin val -> mkGeomOpBe "ST_Within" val
        AST3DDWithinGeom (DWithinGeomOp r val) -> applySQLFn "ST_3DDWithin" [lhs, val, r]
        ASTDWithinGeom (DWithinGeomOp r val) -> applySQLFn "ST_DWithin" [lhs, val, r]
        ASTDWithinGeog (DWithinGeogOp r val sph) -> applySQLFn "ST_DWithin" [lhs, val, r, sph]
        ASTIntersectsRast val -> applySTIntersects [lhs, val]
        ASTIntersectsNbandGeom (STIntersectsNbandGeommin nband geommin) -> applySTIntersects [lhs, nband, geommin]
        ASTIntersectsGeomNband (STIntersectsGeomminNband geommin mNband) -> applySTIntersects [lhs, geommin, withSQLNull mNband]
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
