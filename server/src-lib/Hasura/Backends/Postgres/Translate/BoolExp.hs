{-# LANGUAGE PartialTypeSignatures #-}

-- | Postgres Translate BoolExp
--
-- Convert IR boolean expressions to Postgres-specific SQL expressions.
module Hasura.Backends.Postgres.Translate.BoolExp
  ( toSQLBoolExp,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended (ToTxt)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Backends.Postgres.Types.BoolExp
import Hasura.Backends.Postgres.Types.Function (onArgumentExp)
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.BoolExp.AggregationPredicates (AggregationPredicate (..), AggregationPredicateArguments (..), AggregationPredicatesImplementation (..))
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.Local
import Hasura.SQL.Types
import Hasura.Table.Cache ()

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

-- | Translate an IR boolean expression to an SQL boolean expression. References
-- to columns etc are relative to the given 'rootReference'.
toSQLBoolExp ::
  forall pgKind.
  (Backend ('Postgres pgKind)) =>
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
    return $ sqlAnd sqlBExps
  BoolOr bes -> do
    sqlBExps <- mapM translateBoolExp bes
    return $ foldr (S.BEBin S.OrOp) (S.BELit False) sqlBExps
  BoolNot notExp -> S.BENot <$> translateBoolExp notExp
  BoolExists (GExists currTableReference wh) -> do
    fresh <- state \identifier -> (identifier, identifier + 1)

    let alias :: S.TableAlias
        alias = S.toTableAlias (Identifier ("_exists_table_" <> tshow fresh))

        identifier :: TableIdentifier
        identifier = S.tableAliasToIdentifier alias

    whereExp <- withCurrentTable (S.QualifiedIdentifier identifier Nothing) (translateBoolExp wh)
    return $ S.mkExists (S.FISimple currTableReference (Just alias)) whereExp
  BoolField boolExp -> case boolExp of
    AVColumn colInfo opExps -> do
      BoolExpCtx {rootReference, currTableReference} <- ask
      let colFld = fromCol @('Postgres pgKind) $ ciColumn colInfo
          bExps = map (mkFieldCompExp rootReference currTableReference $ LColumn colFld) opExps
      return $ sqlAnd bExps
    AVRelationship
      (RelInfo {riTarget = RelTargetNativeQuery _})
      _ -> error "translateBoolExp RelTargetNativeQuery"
    AVRelationship
      (RelInfo {riMapping = colMapping, riTarget = RelTargetTable relTN})
      RelationshipFilters
        { rfTargetTablePermissions,
          rfFilter
        } -> do
        -- Convert the where clause on the relationship
        relTNAlias <- S.toTableAlias <$> freshIdentifier relTN
        let relTNIdentifier = S.tableAliasToIdentifier relTNAlias
            relTNQual = S.QualifiedIdentifier relTNIdentifier Nothing

        -- '$' references in permissions of the relationship target table refer to that table, so we
        -- reset both here.
        permBoolExp <-
          local
            ( \e ->
                e
                  { currTableReference = relTNQual,
                    rootReference = relTNQual
                  }
            )
            (translateBoolExp rfTargetTablePermissions)
        annRelBoolExp <- withCurrentTable relTNQual (translateBoolExp rfFilter)
        tableRelExp <- translateTableRelationship colMapping relTNIdentifier
        let innerBoolExp = S.BEBin S.AndOp tableRelExp (S.BEBin S.AndOp permBoolExp annRelBoolExp)
        return $ S.mkExists (S.FISimple relTN $ Just $ relTNAlias) innerBoolExp
    AVComputedField (AnnComputedFieldBoolExp _ _ function sessionArgPresence cfBoolExp) -> do
      case cfBoolExp of
        CFBEScalar opExps -> do
          BoolExpCtx {rootReference, currTableReference} <- ask
          -- Convert the where clause on scalar computed field
          let bExps = map (mkFieldCompExp rootReference currTableReference $ LComputedField function sessionArgPresence) opExps
          pure $ sqlAnd bExps
        CFBETable _ be -> do
          -- Convert the where clause on table computed field
          BoolExpCtx {currTableReference} <- ask
          functionAlias <- S.toTableAlias <$> freshIdentifier function
          let functionIdentifier = S.tableAliasToIdentifier functionAlias
              functionExp =
                mkComputedFieldFunctionExp currTableReference function sessionArgPresence
                  $ Just
                  $ functionAlias
          S.mkExists (S.FIFunc functionExp) <$> withCurrentTable (S.QualifiedIdentifier functionIdentifier Nothing) (translateBoolExp be)
    AVAggregationPredicates aggPreds -> translateAVAggregationPredicates aggPreds

-- | Call a given translation action recursively using the given identifier for the 'current' table.
withCurrentTable :: forall a. S.Qual -> BoolExpM a -> BoolExpM a
withCurrentTable curr = local (\e -> e {currTableReference = curr})

-- | Draw a fresh identifier intended to alias the given object.
freshIdentifier :: forall a. (ToTxt a) => QualifiedObject a -> BoolExpM Identifier
freshIdentifier obj = do
  curVarNum <- get
  put $ curVarNum + 1
  let newIdentifier =
        Identifier
          $ "_be_"
          <> tshow curVarNum
          <> "_"
          <> snakeCaseQualifiedObject obj
  return newIdentifier

identifierWithSuffix :: (ToTxt a) => QualifiedObject a -> Text -> Identifier
identifierWithSuffix relTableName name =
  Identifier (snakeCaseQualifiedObject relTableName <> "_" <> name)

-- | Given a GraphQL aggregation filter of the form:
-- > { where: {<table>_aggregate: {<aggregate_field>: {<bool_op>: <value>} } } }
--
-- Produces SQL of the form:
-- > EXISTS (
-- >   SELECT
-- >     1
-- >     FROM
-- >       (
-- >         SELECT
-- >           <aggregate_function> AS <aggregate_function_alias> -- Note: we can have multiple aggregate expressions here
-- >         FROM
-- >           <array_relationship_table> AS <array_relationship_table_alias>
-- >         WHERE
-- >           <relationship_table_key>
-- >             AND <row_permissions>
-- >             AND <filters>
-- >       ) AS "_sub"
-- >    WHERE
-- >      "_sub".<aggregate_function_alias> <bool_op> <value>
-- > )
translateAVAggregationPredicates ::
  forall pgKind.
  (Backend ('Postgres pgKind)) =>
  AggregationPredicatesImplementation ('Postgres pgKind) (SQLExpression ('Postgres pgKind)) ->
  BoolExpM S.BoolExp
translateAVAggregationPredicates
  api@(AggregationPredicatesImplementation (RelInfo {riTarget = RelTargetTable relTableName, riMapping = colMapping}) _rowPermissions predicate) = do
    -- e.g. __be_0_<schema>_<table_name>
    relTableNameAlias <- S.toTableAlias <$> freshIdentifier relTableName
    let relTableNameIdentifier = S.tableAliasToIdentifier relTableNameAlias
    tableRelExp <- translateTableRelationship colMapping relTableNameIdentifier
    let subselectAlias = S.mkTableAlias "_sub"
        subselectIdentifier = S.tableAliasToIdentifier subselectAlias
        relTable = S.QualifiedIdentifier relTableNameIdentifier Nothing
    subselect <-
      local
        (\e -> e {currTableReference = relTable, rootReference = relTable})
        $ translateAggPredsSubselect subselectAlias relTableNameAlias tableRelExp api
    outerWhereFrag <- translateAggPredBoolExp relTableName subselectIdentifier predicate
    pure $ S.mkExists subselect outerWhereFrag
translateAVAggregationPredicates (AggregationPredicatesImplementation (RelInfo {riTarget = RelTargetNativeQuery _}) _rowPermissions _predicate) =
  error "translateAVAggregationPredicates RelTargetNativeQuery"

translateAggPredBoolExp ::
  forall pgKind.
  TableName ('Postgres pgKind) ->
  TableIdentifier ->
  AggregationPredicate ('Postgres pgKind) S.SQLExp ->
  BoolExpM S.BoolExp
translateAggPredBoolExp
  relTableName
  subselectIdentifier
  (AggregationPredicate {aggPredFunctionName, aggPredPredicate}) = do
    BoolExpCtx {rootReference} <- ask
    let (Identifier aggAlias) = identifierWithSuffix relTableName aggPredFunctionName
        boolExps =
          map
            (mkFieldCompExp rootReference (S.QualifiedIdentifier subselectIdentifier Nothing) $ LColumn (FieldName aggAlias))
            aggPredPredicate
    pure $ sqlAnd boolExps

translateAggPredsSubselect ::
  forall pgKind.
  (Backend ('Postgres pgKind)) =>
  S.TableAlias ->
  S.TableAlias ->
  S.BoolExp ->
  AggregationPredicatesImplementation ('Postgres pgKind) S.SQLExp ->
  BoolExpM S.FromItem
translateAggPredsSubselect
  _subselectAlias
  _relTableNameAlias
  _tableRelExp
  ( AggregationPredicatesImplementation
      RelInfo {riTarget = RelTargetNativeQuery _}
      _rowPermissions
      _predicate
    ) = error "translateAggPredsSubselect RelTargetNativeQuery"
translateAggPredsSubselect
  subselectAlias
  relTableNameAlias
  tableRelExp
  ( AggregationPredicatesImplementation
      RelInfo {riTarget = RelTargetTable relTableName}
      rowPermissions
      predicate
    ) = do
    mFilter <- traverse translateBoolExp (aggPredFilter predicate)
    rowPermExp <- translateBoolExp rowPermissions
    let relTableNameIdentifier = S.tableAliasToIdentifier relTableNameAlias
        -- SELECT <aggregate_function> AS <aggregate_function_alias>
        extractorsExp = translateAggPredExtractor relTableNameIdentifier relTableName predicate
        -- FROM <array_relationship_table> AS <array_relationship_table_alias>
        fromExp = pure $ S.FISimple relTableName $ Just $ S.toTableAlias relTableNameAlias
        -- WHERE <relationship_table_key> AND <row_permissions> AND <mFilter>
        whereExp = sqlAnd $ [tableRelExp, rowPermExp] ++ maybeToList mFilter
    pure
      $ S.mkSelFromItem
        S.mkSelect
          { S.selExtr = [extractorsExp],
            S.selFrom = Just $ S.FromExp fromExp,
            S.selWhere = Just $ S.WhereFrag whereExp
          }
        subselectAlias

translateAggPredExtractor ::
  forall pgKind field.
  TableIdentifier ->
  TableName ('Postgres pgKind) ->
  AggregationPredicate ('Postgres pgKind) field ->
  S.Extractor
translateAggPredExtractor relTableNameIdentifier relTableName (AggregationPredicate {aggPredFunctionName, aggPredArguments}) =
  let predArgsExp = toList $ translateAggPredArguments aggPredArguments relTableNameIdentifier
      aggAlias = S.toColumnAlias $ identifierWithSuffix relTableName aggPredFunctionName
   in S.Extractor (S.SEFnApp aggPredFunctionName predArgsExp Nothing) (Just aggAlias)

translateAggPredArguments ::
  forall pgKind.
  AggregationPredicateArguments ('Postgres pgKind) ->
  TableIdentifier ->
  NonEmpty S.SQLExp
translateAggPredArguments predArgs relTableNameIdentifier =
  case predArgs of
    AggregationPredicateArgumentsStar -> pure $ S.SEStar Nothing
    (AggregationPredicateArguments cols) ->
      S.SEQIdentifier . S.mkQIdentifier relTableNameIdentifier <$> cols

translateTableRelationship :: HashMap PGCol PGCol -> TableIdentifier -> BoolExpM S.BoolExp
translateTableRelationship colMapping relTableNameIdentifier = do
  BoolExpCtx {currTableReference} <- ask
  pure
    $ sqlAnd
    $ flip map (HashMap.toList colMapping)
    $ \(lCol, rCol) ->
      S.BECompare
        S.SEQ
        (S.mkIdentifierSQLExp (S.QualifiedIdentifier relTableNameIdentifier Nothing) rCol)
        (S.mkIdentifierSQLExp currTableReference lCol)

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

sqlAnd :: [S.BoolExp] -> S.BoolExp
sqlAnd = foldr (S.BEBin S.AndOp) (S.BELit True)

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
    mkQCol (RootOrCurrentColumn IsRoot col) = S.mkIdentifierSQLExp rootReference col
    mkQCol (RootOrCurrentColumn IsCurrent col) = S.mkIdentifierSQLExp currTableReference col

    mkCompExp :: SQLExpression ('Postgres pgKind) -> OpExpG ('Postgres pgKind) (SQLExpression ('Postgres pgKind)) -> S.BoolExp
    mkCompExp lhs = \case
      ACast casts -> mkCastsExp casts
      AEQ NullableComparison val -> equalsBoolExpBuilder lhs val
      AEQ NonNullableComparison val -> S.BECompare S.SEQ lhs val
      ANE NullableComparison val -> notEqualsBoolExpBuilder lhs val
      ANE NonNullableComparison val -> S.BECompare S.SNE lhs val
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
          sqlAnd . flip map (HashMap.toList casts) $ \(targetType, operations) ->
            let targetAnn = S.mkTypeAnn $ CollectableTypeScalar targetType
             in sqlAnd $ map (mkCompExp (S.SETyAnn lhs targetAnn)) operations
