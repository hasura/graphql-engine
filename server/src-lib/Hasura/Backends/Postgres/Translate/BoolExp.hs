{-# LANGUAGE PartialTypeSignatures #-}

-- | Postgres Translate BoolExp
--
-- Convert IR boolean expressions to Postgres-specific SQL expressions.
module Hasura.Backends.Postgres.Translate.BoolExp
  ( toSQLBoolExp,
    withRedactionExp,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended (ToTxt, toTxt)
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types hiding (TableName)
import Hasura.Backends.Postgres.Types.BoolExp
import Hasura.Backends.Postgres.Types.Function (onArgumentExp)
import Hasura.Base.Error (QErr)
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.BoolExp.AggregationPredicates (AggregationPredicate (..), AggregationPredicateArguments (..), AggregationPredicatesImplementation (..))
import Hasura.RQL.IR.BoolExp.RemoteRelationshipPredicate
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Session (UserInfo (..))
import Hasura.SQL.AnyBackend qualified as AB
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
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  UserInfo ->
  -- | The name of the tabular value in query scope that the boolean expression
  -- applies to
  S.Qual ->
  -- | The boolean expression to translate
  AnnBoolExpSQL ('Postgres pgKind) ->
  m S.BoolExp
toSQLBoolExp userInfo rootReference e =
  evalStateT
    ( runReaderT
        (unBoolExpM (translateBoolExp userInfo e))
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
newtype BoolExpM m a = BoolExpM {unBoolExpM :: ReaderT BoolExpCtx (StateT Word64 m) a}
  deriving (Functor, Applicative, Monad, MonadReader BoolExpCtx, MonadState Word64)

instance MonadTrans BoolExpM where
  lift = BoolExpM . lift . lift

-- | Translate a 'GBoolExp' with annotated SQLExpressions in the leaves into a
-- bare SQL Boolean Expression.
translateBoolExp ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  UserInfo ->
  AnnBoolExpSQL ('Postgres pgKind) ->
  BoolExpM m S.BoolExp
translateBoolExp userInfo = \case
  BoolAnd bes -> do
    sqlBExps <- mapM (translateBoolExp userInfo) bes
    return $ sqlAnd sqlBExps
  BoolOr bes -> do
    sqlBExps <- mapM (translateBoolExp userInfo) bes
    return $ foldr (S.BEBin S.OrOp) (S.BELit False) sqlBExps
  BoolNot notExp -> S.BENot <$> translateBoolExp userInfo notExp
  BoolExists (GExists currTableReference wh) -> do
    fresh <- state \identifier -> (identifier, identifier + 1)

    let alias :: S.TableAlias
        alias = S.toTableAlias (Identifier ("_exists_table_" <> tshow fresh))

        identifier :: TableIdentifier
        identifier = S.tableAliasToIdentifier alias

    whereExp <- withCurrentTable (S.QualifiedIdentifier identifier Nothing) (translateBoolExp userInfo wh)
    return $ S.mkExists (S.FISimple currTableReference (Just alias)) whereExp
  BoolField boolExp -> case boolExp of
    AVColumn colInfo redactionExp opExps -> do
      BoolExpCtx {rootReference, currTableReference} <- ask
      let colFld = fromCol @('Postgres pgKind) $ ciColumn colInfo
      bExps <- traverse (lift . mkFieldCompExp rootReference currTableReference redactionExp (LColumn colFld) userInfo) opExps
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
            (translateBoolExp userInfo rfTargetTablePermissions)
        annRelBoolExp <- withCurrentTable relTNQual (translateBoolExp userInfo rfFilter)
        tableRelExp <- translateTableRelationship colMapping relTNIdentifier
        let innerBoolExp = S.BEBin S.AndOp tableRelExp (S.BEBin S.AndOp permBoolExp annRelBoolExp)
        return $ S.mkExists (S.FISimple relTN $ Just $ relTNAlias) innerBoolExp
    AVComputedField (AnnComputedFieldBoolExp _ _ function sessionArgPresence cfBoolExp) -> do
      case cfBoolExp of
        CFBEScalar redactionExp opExps -> do
          BoolExpCtx {rootReference, currTableReference} <- ask
          -- Convert the where clause on scalar computed field
          bExps <- traverse (lift . mkFieldCompExp rootReference currTableReference redactionExp (LComputedField function sessionArgPresence) userInfo) opExps
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
          S.mkExists (S.FIFunc functionExp) <$> withCurrentTable (S.QualifiedIdentifier functionIdentifier Nothing) (translateBoolExp userInfo be)
    AVAggregationPredicates aggPreds -> translateAVAggregationPredicates userInfo aggPreds
    AVRemoteRelationship (RemoteRelPermBoolExp _rawRelBoolExp (lhsCol, _rawRelBoolExplhsColType) rhsFetchInfo) -> do
      {-
        The permission is the following:

        relName : {
          remoteFieldName : {
            _eq : remoteFieldEqExp
          }
        }

        Now, we need to do the following steps:
        Step 1: get the data by running the query:
          SELECT <target_field_name> FROM <target_table_name> WHERE <remoteFieldName> = <remoteFieldEqExp>
        Step 2: Make a IN statement:
          <origin_field_name> IN (<data from step 3>)
      -}
      let colFld = fromCol @('Postgres pgKind) $ lhsCol
      lookupLst <-
        AB.dispatchAnyBackend @Backend rhsFetchInfo \((RemoteRelRHSFetchInfo (colType, col) tableName filterExp sourceName sourceConfig) :: RemoteRelRHSFetchInfo f b) -> do
          let colFieldName = rrrfweColumnFieldName filterExp
              colFieldBoolExpressions = rrrfweBoolExp filterExp
          result <- lift $ getColVals @b (_uiSession userInfo) sourceName sourceConfig tableName (colType, col) (colFieldName, colFieldBoolExpressions)
          let typeAnn = S.mkTypeAnn (CollectableTypeScalar (textToPGScalarType (toTxt colType)))
          if null result
            then pure $ [S.SENull]
            else pure (fmap ((`S.SETyAnn` typeAnn) . S.SELit) result)
      pure $ S.BEIN (S.SEIdentifier (Identifier (toTxt colFld))) (lookupLst)

-- | Call a given translation action recursively using the given identifier for the 'current' table.
withCurrentTable :: forall m a. (Monad m) => S.Qual -> BoolExpM m a -> BoolExpM m a
withCurrentTable curr = local (\e -> e {currTableReference = curr})

-- | Draw a fresh identifier intended to alias the given object.
freshIdentifier :: forall a m. (ToTxt a, Monad m) => QualifiedObject a -> BoolExpM m Identifier
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
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  UserInfo ->
  AggregationPredicatesImplementation ('Postgres pgKind) (SQLExpression ('Postgres pgKind)) ->
  BoolExpM m S.BoolExp
translateAVAggregationPredicates
  userInfo
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
        $ translateAggPredsSubselect userInfo subselectAlias relTableNameAlias tableRelExp api
    outerWhereFrag <- translateAggPredBoolExp relTableName subselectIdentifier userInfo predicate
    pure $ S.mkExists subselect outerWhereFrag
translateAVAggregationPredicates _userInfo (AggregationPredicatesImplementation (RelInfo {riTarget = RelTargetNativeQuery _}) _rowPermissions _predicate) =
  error "translateAVAggregationPredicates RelTargetNativeQuery"

translateAggPredBoolExp ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  TableName ('Postgres pgKind) ->
  TableIdentifier ->
  UserInfo ->
  AggregationPredicate ('Postgres pgKind) S.SQLExp ->
  BoolExpM m S.BoolExp
translateAggPredBoolExp
  relTableName
  subselectIdentifier
  userInfo
  (AggregationPredicate {aggPredFunctionName, aggPredPredicate}) = do
    BoolExpCtx {rootReference} <- ask
    let (Identifier aggAlias) = identifierWithSuffix relTableName aggPredFunctionName
    boolExps <-
      traverse
        (lift . mkFieldCompExp rootReference (S.QualifiedIdentifier subselectIdentifier Nothing) NoRedaction (LColumn (FieldName aggAlias)) userInfo)
        aggPredPredicate
    pure $ S.simplifyBoolExp $ sqlAnd boolExps

translateAggPredsSubselect ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  UserInfo ->
  S.TableAlias ->
  S.TableAlias ->
  S.BoolExp ->
  AggregationPredicatesImplementation ('Postgres pgKind) S.SQLExp ->
  BoolExpM m S.FromItem
translateAggPredsSubselect
  _userInfo
  _subselectAlias
  _relTableNameAlias
  _tableRelExp
  ( AggregationPredicatesImplementation
      RelInfo {riTarget = RelTargetNativeQuery _}
      _rowPermissions
      _predicate
    ) = error "translateAggPredsSubselect RelTargetNativeQuery"
translateAggPredsSubselect
  userInfo
  subselectAlias
  relTableNameAlias
  tableRelExp
  ( AggregationPredicatesImplementation
      RelInfo {riTarget = RelTargetTable relTableName}
      rowPermissions
      predicate
    ) = do
    mFilter <- traverse (translateBoolExp userInfo) (aggPredFilter predicate)
    rowPermExp <- translateBoolExp userInfo rowPermissions
    let relTableNameIdentifier = S.tableAliasToIdentifier relTableNameAlias
    -- SELECT <aggregate_function> AS <aggregate_function_alias>
    extractorsExp <- lift $ translateAggPredExtractor relTableNameIdentifier relTableName userInfo predicate
    -- FROM <array_relationship_table> AS <array_relationship_table_alias>
    let fromExp = pure $ S.FISimple relTableName $ Just $ S.toTableAlias relTableNameAlias
        -- WHERE <relationship_table_key> AND <row_permissions> AND <mFilter>
        whereExp = sqlAnd $ [tableRelExp, rowPermExp] ++ maybeToList mFilter
    pure
      $ S.mkSelFromItem
        S.mkSelect
          { S.selExtr = [extractorsExp],
            S.selFrom = Just $ S.FromExp fromExp,
            S.selWhere = Just $ S.WhereFrag $ S.simplifyBoolExp whereExp
          }
        subselectAlias

translateAggPredExtractor ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  TableIdentifier ->
  TableName ('Postgres pgKind) ->
  UserInfo ->
  AggregationPredicate ('Postgres pgKind) S.SQLExp ->
  m S.Extractor
translateAggPredExtractor relTableNameIdentifier relTableName userInfo (AggregationPredicate {aggPredFunctionName, aggPredArguments}) = do
  predArgsExp <- toList <$> translateAggPredArguments aggPredArguments relTableNameIdentifier userInfo
  let aggAlias = S.toColumnAlias $ identifierWithSuffix relTableName aggPredFunctionName
  pure $ S.Extractor (S.SEFnApp aggPredFunctionName predArgsExp Nothing) (Just aggAlias)

translateAggPredArguments ::
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  AggregationPredicateArguments ('Postgres pgKind) S.SQLExp ->
  TableIdentifier ->
  UserInfo ->
  m (NonEmpty S.SQLExp)
translateAggPredArguments predArgs relTableNameIdentifier userInfo =
  case predArgs of
    AggregationPredicateArgumentsStar -> pure (pure $ S.SEStar Nothing)
    (AggregationPredicateArguments cols) -> do
      traverse
        ( \(column, redactionExp) ->
            withRedactionExp (S.QualifiedIdentifier relTableNameIdentifier Nothing) redactionExp userInfo
              $ S.mkQIdenExp relTableNameIdentifier column
        )
        cols

translateTableRelationship :: (Monad m) => RelMapping ('Postgres pgKind) -> TableIdentifier -> BoolExpM m S.BoolExp
translateTableRelationship colMapping relTableNameIdentifier = do
  BoolExpCtx {currTableReference} <- ask
  pure
    $ sqlAnd
    $ flip map (HashMap.toList $ unRelMapping colMapping)
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
  forall pgKind m.
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  S.Qual ->
  S.Qual ->
  AnnRedactionExp ('Postgres pgKind) S.SQLExp ->
  LHSField ('Postgres pgKind) ->
  UserInfo ->
  OpExpG ('Postgres pgKind) S.SQLExp ->
  m S.BoolExp
mkFieldCompExp rootReference currTableReference lhsRedactionExp lhsField userInfo operationExpression = do
  sqlExp <- qLhsField
  pure $ mkCompExp sqlExp operationExpression
  where
    qLhsField = case lhsField of
      LColumn fieldName ->
        withRedactionExp currTableReference lhsRedactionExp userInfo
          -- "qual"."column" =
          $ S.SEQIdentifier
          $ S.QIdentifier currTableReference
          $ Identifier
          $ getFieldNameTxt fieldName
      LComputedField function sessionArgPresence ->
        withRedactionExp currTableReference lhsRedactionExp userInfo
          -- "function_schema"."function_name"("qual".*) =
          $ S.SEFunction
          $ mkComputedFieldFunctionExp currTableReference function sessionArgPresence Nothing

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

withRedactionExp ::
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  S.Qual ->
  AnnRedactionExp ('Postgres pgKind) S.SQLExp ->
  UserInfo ->
  S.SQLExp ->
  m S.SQLExp
withRedactionExp tableQual redactionExp userInfo sqlExpression =
  -- Check out [SQL generation for inherited role]
  case redactionExp of
    NoRedaction -> pure sqlExpression
    RedactIfFalse gBoolExp -> do
      boolExp <- S.simplifyBoolExp <$> toSQLBoolExp userInfo tableQual gBoolExp
      pure $ S.SECond boolExp sqlExpression S.SENull
