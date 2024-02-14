-- | Postgres SQL DML
--
-- Provide types and combinators for defining Postgres SQL queries and mutations.
module Hasura.Backends.Postgres.SQL.DML
  ( ColumnAlias (..),
    TableAlias (..),
    BinOp (AndOp, OrOp),
    BoolExp (..),
    TopLevelCTE (CTEDelete, CTEInsert, CTESelect, CTEUpdate, CTEUnsafeRawSQL),
    InnerCTE (..),
    CompareOp (SContainedIn, SContains, SEQ, SGT, SGTE, SHasKey, SHasKeysAll, SHasKeysAny, SILIKE, SIREGEX, SLIKE, SLT, SLTE, SMatchesFulltext, SNE, SNILIKE, SNIREGEX, SNLIKE, SNREGEX, SNSIMILAR, SREGEX, SSIMILAR),
    CountType (CTDistinct, CTSimple, CTStar),
    DistinctExpr (DistinctOn, DistinctSimple),
    Extractor (..),
    FromExp (..),
    FromItem (..),
    FunctionAlias (FunctionAlias),
    FunctionDefinitionListItem (..),
    FunctionArgs (FunctionArgs),
    FunctionExp (FunctionExp),
    GroupByExp (GroupByExp),
    HavingExp (HavingExp),
    JoinCond (..),
    JoinExpr (JoinExpr),
    JoinType (Inner, LeftOuter),
    Lateral (Lateral),
    LimitExp (LimitExp),
    NullsOrder (NullsFirst, NullsLast),
    OffsetExp (OffsetExp),
    OrderByExp (..),
    OrderByItem (OrderByItem, oExpression),
    OrderType (OTAsc, OTDesc),
    QIdentifier (QIdentifier),
    Qual (QualTable, QualVar, QualifiedIdentifier),
    RetExp (RetExp),
    SQLConflict (..),
    SQLConflictTarget (SQLColumn, SQLConstraint),
    SQLDelete (SQLDelete),
    SQLExp (..),
    SQLInsert (SQLInsert, siCols, siConflict, siRet, siTable, siValues),
    SQLOp (SQLOp),
    ColumnOp (..),
    SQLUpdate (..),
    Select (Select, selCTEs, selDistinct, selExtr, selFrom, selLimit, selOffset, selOrderBy, selWhere),
    SelectWith,
    SelectWithG (..),
    SetExp (SetExp),
    SetExpItem (..),
    TupleExp (TupleExp),
    TypeAnn (TypeAnn),
    ValuesExp (ValuesExp),
    WhereFrag (WhereFrag),
    dummySelectList,
    applyJsonBuildArray,
    applyJsonBuildObj,
    applyRowToJson,
    applyUppercase,
    boolTypeAnn,
    buildUpsertSetExp,
    columnDefaultValue,
    countStar,
    handleIfNull,
    incOp,
    int64ToSQLExp,
    intToSQLExp,
    intTypeAnn,
    jsonTypeAnn,
    jsonbConcatOp,
    jsonbDeleteAtPathOp,
    jsonbDeleteOp,
    jsonbPathOp,
    jsonbTypeAnn,
    mkExists,
    mkExtr,
    mkFunctionAlias,
    mkIdenFromExp,
    mkLateralFromItem,
    mkQIdenExp,
    mkQIdentifier,
    mkQIdentifierTable,
    mkQual,
    mkRowExp,
    mkIdentifierSQLExp,
    mkSIdenExp,
    mkSQLOpExp,
    mkSelFromExp,
    mkSelFromItem,
    mkSelect,
    mkSelectWithFromItem,
    mkSimpleFromExp,
    mkTypeAnn,
    mulOp,
    numericTypeAnn,
    returningStar,
    selectStar,
    selectStar',
    simplifyBoolExp,
    textArrTypeAnn,
    textTypeAnn,
    mkColumnAlias,
    mkTableAlias,
    toTableAlias,
    tableAliasToIdentifier,
    toColumnAlias,
    tableIdentifierToColumnAlias,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (pack)
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types
import Hasura.NativeQuery.Metadata
import Hasura.Prelude
import Hasura.SQL.Types
import Text.Builder qualified as TB

-- | An select statement that does not require mutation CTEs.
--
--   See 'SelectWithG' or 'SelectWith' for select statements with mutations as CTEs.
data Select = Select
  { -- | Unlike 'SelectWith', does not allow data-modifying statements (as those are only allowed at
    -- the top level of a query).
    selCTEs :: [(TableAlias, InnerCTE)],
    selDistinct :: Maybe DistinctExpr,
    selExtr :: [Extractor],
    selFrom :: Maybe FromExp,
    selWhere :: Maybe WhereFrag,
    selGroupBy :: Maybe GroupByExp,
    selHaving :: Maybe HavingExp,
    selOrderBy :: Maybe OrderByExp,
    selLimit :: Maybe LimitExp,
    selOffset :: Maybe OffsetExp
  }
  deriving (Show, Eq, Generic, Data)

instance NFData Select

instance Hashable Select

-- | An empty select statement.
mkSelect :: Select
mkSelect =
  Select
    []
    Nothing
    []
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- | A dummy select list to avoid an empty select list, which doesn't work for cockroach db.
--   This is just the value @1@ without an alias.
dummySelectList :: [Extractor]
dummySelectList = [Extractor (SEUnsafe "1") Nothing]

newtype LimitExp
  = LimitExp SQLExp
  deriving (Show, Eq, NFData, Data, Hashable)

instance ToSQL LimitExp where
  toSQL (LimitExp se) =
    "LIMIT" <~> toSQL se

newtype OffsetExp
  = OffsetExp SQLExp
  deriving (Show, Eq, NFData, Data, Hashable)

instance ToSQL OffsetExp where
  toSQL (OffsetExp se) =
    "OFFSET" <~> toSQL se

newtype OrderByExp
  = OrderByExp (NonEmpty OrderByItem)
  deriving (Show, Eq, NFData, Data, Hashable)

data OrderByItem = OrderByItem
  { oExpression :: SQLExp,
    oOrdering :: Maybe OrderType,
    oNullsOrder :: Maybe NullsOrder
  }
  deriving (Show, Eq, Generic, Data)

instance NFData OrderByItem

instance Hashable OrderByItem

instance ToSQL OrderByItem where
  toSQL (OrderByItem expr ordering nullsOrder) =
    toSQL expr <~> toSQL ordering <~> toSQL nullsOrder

-- | Order by ascending or descending
data OrderType = OTAsc | OTDesc
  deriving (Show, Eq, Generic, Data)

instance NFData OrderType

instance Hashable OrderType

instance ToSQL OrderType where
  toSQL OTAsc = "ASC"
  toSQL OTDesc = "DESC"

instance J.FromJSON OrderType where
  parseJSON = J.genericParseJSON $ J.defaultOptions {J.constructorTagModifier = J.snakeCase . drop 2}

instance J.ToJSON OrderType where
  toJSON = J.genericToJSON $ J.defaultOptions {J.constructorTagModifier = J.snakeCase . drop 2}

data NullsOrder
  = NullsFirst
  | NullsLast
  deriving (Show, Eq, Generic, Data)

instance NFData NullsOrder

instance Hashable NullsOrder

instance ToSQL NullsOrder where
  toSQL NullsFirst = "NULLS FIRST"
  toSQL NullsLast = "NULLS LAST"

instance J.FromJSON NullsOrder where
  -- Todo: write a proper parser https://github.com/hasura/graphql-engine-mono/issues/5484
  parseJSON = J.genericParseJSON $ J.defaultOptions {J.constructorTagModifier = J.snakeCase . drop 5}

instance J.ToJSON NullsOrder where
  -- Todo: write a proper parser https://github.com/hasura/graphql-engine-mono/issues/5484
  toJSON = J.genericToJSON $ J.defaultOptions {J.constructorTagModifier = J.snakeCase . drop 5}

instance ToSQL OrderByExp where
  toSQL (OrderByExp l) =
    "ORDER BY" <~> (", " <+> toList l)

newtype GroupByExp
  = GroupByExp [SQLExp]
  deriving (Show, Eq, NFData, Data, Hashable)

instance ToSQL GroupByExp where
  toSQL (GroupByExp idens) =
    "GROUP BY" <~> (", " <+> idens)

newtype FromExp
  = FromExp [FromItem]
  deriving (Show, Eq, NFData, Data, Hashable)

instance ToSQL FromExp where
  toSQL (FromExp items) =
    "FROM" <~> (", " <+> items)

mkIdenFromExp :: TableIdentifier -> FromExp
mkIdenFromExp ident =
  FromExp [FIIdentifier ident]

mkSimpleFromExp :: QualifiedTable -> FromExp
mkSimpleFromExp qt =
  FromExp [FISimple qt Nothing]

mkSelFromExp :: Bool -> Select -> TableName -> FromItem
mkSelFromExp isLateral sel tn =
  FISelect (Lateral isLateral) sel alias
  where
    alias = toTableAlias $ toIdentifier tn

mkRowExp :: [Extractor] -> SQLExp
mkRowExp extrs =
  let innerSel = mkSelect {selExtr = extrs}

      innerSelName = TableName "e"

      -- SELECT r FROM (SELECT col1, col2, .. ) AS r
      outerSel =
        mkSelect
          { selExtr = [Extractor (SERowIdentifier $ toIdentifier innerSelName) Nothing],
            selFrom =
              Just
                $ FromExp
                  [mkSelFromExp False innerSel innerSelName]
          }
   in SESelect outerSel

newtype HavingExp
  = HavingExp BoolExp
  deriving (Show, Eq, NFData, Data, Hashable)

instance ToSQL HavingExp where
  toSQL (HavingExp be) =
    "HAVING" <~> toSQL be

newtype WhereFrag = WhereFrag {getWFBoolExp :: BoolExp}
  deriving (Show, Eq, NFData, Data, Hashable)

instance ToSQL WhereFrag where
  toSQL (WhereFrag be) =
    "WHERE" <~> parenB (toSQL be)

instance ToSQL Select where
  toSQL sel = case selCTEs sel of
    [] ->
      "SELECT"
        <~> toSQL (selDistinct sel)
        <~> (", " <+> selExtr sel)
        <~> toSQL (selFrom sel)
        <~> toSQL (selWhere sel)
        <~> toSQL (selGroupBy sel)
        <~> toSQL (selHaving sel)
        <~> toSQL (selOrderBy sel)
        <~> toSQL (selLimit sel)
        <~> toSQL (selOffset sel)
    -- reuse SelectWith if there are any CTEs, since the generated SQL is the same
    ctes -> toSQL $ SelectWith (map (toTopLevelCTE <$>) ctes) sel {selCTEs = []}

mkSIdenExp :: (IsIdentifier a) => a -> SQLExp
mkSIdenExp = SEIdentifier . toIdentifier

mkQIdenExp :: (IsIdentifier b) => TableIdentifier -> b -> SQLExp
mkQIdenExp q t = SEQIdentifier $ mkQIdentifier q t

data Qual
  = QualifiedIdentifier TableIdentifier (Maybe TypeAnn)
  | QualTable QualifiedTable
  | QualVar Text
  deriving (Show, Eq, Generic, Data)

instance NFData Qual

instance Hashable Qual

mkQual :: QualifiedTable -> Qual
mkQual = QualTable

instance ToSQL Qual where
  toSQL (QualifiedIdentifier i Nothing) = toSQL i
  toSQL (QualifiedIdentifier i (Just ty)) = parenB (toSQL i <> toSQL ty)
  toSQL (QualTable qt) = toSQL qt
  toSQL (QualVar v) = TB.text v

mkQIdentifier :: (IsIdentifier b) => TableIdentifier -> b -> QIdentifier
mkQIdentifier q t = QIdentifier (QualifiedIdentifier q Nothing) (toIdentifier t)

mkQIdentifierTable :: (IsIdentifier a) => QualifiedTable -> a -> QIdentifier
mkQIdentifierTable q = QIdentifier (mkQual q) . toIdentifier

mkIdentifierSQLExp :: forall a. (IsIdentifier a) => Qual -> a -> SQLExp
mkIdentifierSQLExp q = SEQIdentifier . QIdentifier q . toIdentifier

data QIdentifier
  = QIdentifier Qual Identifier
  deriving (Show, Eq, Generic, Data)

instance NFData QIdentifier

instance Hashable QIdentifier

instance ToSQL QIdentifier where
  toSQL (QIdentifier qual iden) =
    mconcat [toSQL qual, TB.char '.', toSQL iden]

data ColumnOp = ColumnOp
  { _colOp :: SQLOp,
    _colExp :: SQLExp
  }
  deriving (Show, Eq, Generic, Data)

instance NFData ColumnOp

instance Hashable ColumnOp

newtype SQLOp = SQLOp {sqlOpTxt :: Text}
  deriving (Show, Eq, NFData, Data, Hashable)

incOp :: SQLOp
incOp = SQLOp "+"

mulOp :: SQLOp
mulOp = SQLOp "*"

jsonbPathOp :: SQLOp
jsonbPathOp = SQLOp "#>"

jsonbConcatOp :: SQLOp
jsonbConcatOp = SQLOp "||"

jsonbDeleteOp :: SQLOp
jsonbDeleteOp = SQLOp "-"

jsonbDeleteAtPathOp :: SQLOp
jsonbDeleteAtPathOp = SQLOp "#-"

newtype TypeAnn = TypeAnn {unTypeAnn :: Text}
  deriving (Show, Eq, NFData, Data, Hashable)

instance ToSQL TypeAnn where
  toSQL (TypeAnn ty) = "::" <> TB.text ty

mkTypeAnn :: CollectableType PGScalarType -> TypeAnn
mkTypeAnn = TypeAnn . toSQLTxt

intTypeAnn :: TypeAnn
intTypeAnn = mkTypeAnn $ CollectableTypeScalar PGInteger

numericTypeAnn :: TypeAnn
numericTypeAnn = mkTypeAnn $ CollectableTypeScalar PGNumeric

textTypeAnn :: TypeAnn
textTypeAnn = mkTypeAnn $ CollectableTypeScalar PGText

textArrTypeAnn :: TypeAnn
textArrTypeAnn = mkTypeAnn $ CollectableTypeArray PGText

jsonTypeAnn :: TypeAnn
jsonTypeAnn = mkTypeAnn $ CollectableTypeScalar PGJSON

jsonbTypeAnn :: TypeAnn
jsonbTypeAnn = mkTypeAnn $ CollectableTypeScalar PGJSONB

boolTypeAnn :: TypeAnn
boolTypeAnn = mkTypeAnn $ CollectableTypeScalar PGBoolean

data CountType columnType
  = CTStar
  | CTSimple [columnType]
  | CTDistinct [columnType]
  deriving (Show, Eq, Generic, Data, Functor, Foldable, Traversable)

instance (NFData columnType) => NFData (CountType columnType)

instance (Hashable columnType) => Hashable (CountType columnType)

instance ToSQL (CountType QIdentifier) where
  toSQL CTStar = "*"
  toSQL (CTSimple cols) =
    parenB $ ", " <+> cols
  toSQL (CTDistinct cols) =
    "DISTINCT" <~> parenB (", " <+> cols)

newtype TupleExp
  = TupleExp [SQLExp]
  deriving (Show, Eq, NFData, Data, Hashable)

instance ToSQL TupleExp where
  toSQL (TupleExp exps) =
    parenB $ ", " <+> exps

data SQLExp
  = SEPrep Int
  | SENull
  | SELit Text
  | SEUnsafe Text
  | SESelect Select
  | -- | all fields (@*@) or all fields from relation (@iden.*@)
    SEStar (Maybe Qual)
  | -- | A column name
    SEIdentifier Identifier
  | -- | SEIdentifier and SERowIdentifier are distinguished for easier rewrite rules
    SERowIdentifier Identifier
  | -- | A qualified column name
    SEQIdentifier QIdentifier
  | -- | this is used to apply a sql function to an expression. The 'Text' is the function name
    SEFnApp Text [SQLExp] (Maybe OrderByExp)
  | SEOpApp SQLOp [SQLExp]
  | SETyAnn SQLExp TypeAnn
  | SECond BoolExp SQLExp SQLExp
  | SEBool BoolExp
  | SEExcluded Identifier
  | SEArray [SQLExp]
  | SEArrayIndex SQLExp SQLExp
  | SETuple TupleExp
  | SECount (CountType QIdentifier)
  | SENamedArg Identifier SQLExp
  | SEFunction FunctionExp
  deriving (Show, Eq, Generic, Data)

instance NFData SQLExp

instance Hashable SQLExp

instance J.ToJSON SQLExp where
  toJSON = J.toJSON . toSQLTxt

-- | Represents an alias assignment for a column
newtype ColumnAlias = ColumnAlias {getColumnAlias :: Identifier}
  deriving (Show, Eq, NFData, Data, Hashable)

instance IsString ColumnAlias where
  fromString = mkColumnAlias . pack

instance Semigroup ColumnAlias where
  (ColumnAlias ca1) <> (ColumnAlias ca2) = ColumnAlias (ca1 <> ca2)

mkColumnAlias :: Text -> ColumnAlias
mkColumnAlias = ColumnAlias . Identifier

instance IsIdentifier ColumnAlias where
  toIdentifier (ColumnAlias identifier) = identifier

tableIdentifierToColumnAlias :: TableIdentifier -> ColumnAlias
tableIdentifierToColumnAlias = mkColumnAlias . unTableIdentifier

toColumnAlias :: (IsIdentifier a) => a -> ColumnAlias
toColumnAlias = ColumnAlias . toIdentifier

-- | Convert a column alias assignment to SQL _with_ @AS@ prefix
columnAliasToSqlWithAs :: ColumnAlias -> TB.Builder
columnAliasToSqlWithAs (ColumnAlias alias) = "AS" <~> toSQL alias

-- | Convert a column alias assignment to SQL _without_ @AS@ prefix
columnAliasToSqlWithoutAs :: ColumnAlias -> TB.Builder
columnAliasToSqlWithoutAs alias = toSQL (toIdentifier alias)

-- | Represents an alias assignment for a table, relation or row
newtype TableAlias = TableAlias {getTableAlias :: Identifier}
  deriving (Show, Eq, NFData, Data, Generic, Hashable)

instance IsString TableAlias where
  fromString = mkTableAlias . pack

instance Semigroup TableAlias where
  (TableAlias ta1) <> (TableAlias ta2) = TableAlias (ta1 <> ta2)

-- | Create a table alias.
mkTableAlias :: Text -> TableAlias
mkTableAlias = TableAlias . Identifier

-- | Create a table identifier from a table alias.
tableAliasToIdentifier :: TableAlias -> TableIdentifier
tableAliasToIdentifier = TableIdentifier . getIdenTxt . getTableAlias

instance IsIdentifier TableAlias where
  toIdentifier (TableAlias identifier) = identifier

-- TODO: Remove when we remove 'Identifier'. We should only be able to create
-- identifiers from aliases, not aliases from identifiers. Aliases represent
-- definition sites and identifiers usage sites.
toTableAlias :: (IsIdentifier a) => a -> TableAlias
toTableAlias = TableAlias . toIdentifier

-- | Convert a table alias assignment to SQL _with_ @AS@ prefix
tableAliasToSqlWithAs :: TableAlias -> TB.Builder
tableAliasToSqlWithAs alias = "AS" <~> toSQL (toIdentifier alias)

-- | Convert a table alias assignment to SQL _without_ @AS@ prefix
tableAliasToSqlWithoutAs :: TableAlias -> TB.Builder
tableAliasToSqlWithoutAs alias = toSQL (toIdentifier alias)

countStar :: SQLExp
countStar = SECount CTStar

instance ToSQL SQLExp where
  toSQL (SEPrep argNumber) =
    TB.char '$' <> fromString (show argNumber)
  toSQL SENull =
    TB.text "NULL"
  toSQL (SELit tv) =
    TB.text $ pgFmtLit tv
  toSQL (SEUnsafe t) =
    TB.text t
  toSQL (SESelect se) =
    parenB $ toSQL se
  toSQL (SEStar Nothing) =
    TB.char '*'
  toSQL (SEStar (Just qual)) =
    mconcat [toSQL qual, TB.char '.', TB.char '*']
  toSQL (SEIdentifier iden) =
    toSQL iden
  toSQL (SERowIdentifier iden) =
    toSQL iden
  toSQL (SEQIdentifier qIdentifier) =
    toSQL qIdentifier
  -- https://www.postgresql.org/docs/10/static/sql-expressions.html#SYNTAX-AGGREGATES
  toSQL (SEFnApp name args mObe) =
    TB.text name <> parenB ((", " <+> args) <~> toSQL mObe)
  toSQL (SEOpApp op args) =
    parenB (sqlOpTxt op <+> args)
  toSQL (SETyAnn e ty) =
    parenB (toSQL e) <> toSQL ty
  toSQL (SECond cond te fe) =
    "CASE WHEN"
      <~> toSQL cond
      <~> "THEN"
      <~> toSQL te
      <~> "ELSE"
      <~> toSQL fe
      <~> "END"
  toSQL (SEBool be) = toSQL be
  toSQL (SEExcluded i) =
    "EXCLUDED."
      <> toSQL i
  toSQL (SEArray exps) =
    "ARRAY"
      <> TB.char '['
      <> (", " <+> exps)
      <> TB.char ']'
  toSQL (SEArrayIndex arrayExp indexExp) =
    parenB (toSQL arrayExp)
      <> TB.char '['
      <> toSQL indexExp
      <> TB.char ']'
  toSQL (SETuple tup) = toSQL tup
  toSQL (SECount ty) = "COUNT" <> parenB (toSQL ty)
  -- https://www.postgresql.org/docs/current/sql-syntax-calling-funcs.html
  toSQL (SENamedArg arg val) = toSQL arg <~> "=>" <~> toSQL val
  toSQL (SEFunction funcExp) = toSQL funcExp

intToSQLExp :: Int -> SQLExp
intToSQLExp = SEUnsafe . tshow

int64ToSQLExp :: Int64 -> SQLExp
int64ToSQLExp = SEUnsafe . tshow

-- | Extractor can be used to apply Postgres alias to a column
data Extractor = Extractor SQLExp (Maybe ColumnAlias)
  deriving (Show, Eq, Generic, Data)

instance NFData Extractor

instance Hashable Extractor

mkSQLOpExp ::
  SQLOp ->
  SQLExp -> -- lhs
  SQLExp -> -- rhs
  SQLExp -- result
mkSQLOpExp op lhs rhs = SEOpApp op [lhs, rhs]

columnDefaultValue :: SQLExp
columnDefaultValue = SEUnsafe "DEFAULT"

handleIfNull :: SQLExp -> SQLExp -> SQLExp
handleIfNull l e = SEFnApp "coalesce" [e, l] Nothing

applyJsonBuildObj :: [SQLExp] -> SQLExp
applyJsonBuildObj args =
  SEFnApp "json_build_object" args Nothing

applyJsonBuildArray :: [SQLExp] -> SQLExp
applyJsonBuildArray args =
  SEFnApp "json_build_array" args Nothing

applyRowToJson :: [Extractor] -> SQLExp
applyRowToJson extrs =
  SEFnApp "row_to_json" [mkRowExp extrs] Nothing

applyUppercase :: SQLExp -> SQLExp
applyUppercase arg =
  SEFnApp "upper" [arg] Nothing

mkExtr :: (IsIdentifier a) => a -> Extractor
mkExtr t = Extractor (mkSIdenExp t) Nothing

instance ToSQL Extractor where
  toSQL (Extractor ce mal) =
    toSQL ce <~> maybe "" columnAliasToSqlWithAs mal

data DistinctExpr
  = DistinctSimple
  | DistinctOn [SQLExp]
  deriving (Show, Eq, Generic, Data)

instance NFData DistinctExpr

instance Hashable DistinctExpr

instance ToSQL DistinctExpr where
  toSQL DistinctSimple = "DISTINCT"
  toSQL (DistinctOn exprs) =
    "DISTINCT ON" <~> parenB ("," <+> exprs)

data FunctionArgs = FunctionArgs
  { fasPostional :: [SQLExp],
    fasNamed :: (HashMap.HashMap Text SQLExp)
  }
  deriving (Show, Eq, Generic, Data)

instance NFData FunctionArgs

instance Hashable FunctionArgs

instance ToSQL FunctionArgs where
  toSQL (FunctionArgs positionalArgs namedArgsMap) =
    let namedArgs = flip map (HashMap.toList namedArgsMap)
          $ \(argName, argVal) -> SENamedArg (Identifier argName) argVal
     in parenB $ ", " <+> (positionalArgs <> namedArgs)

data FunctionDefinitionListItem = FunctionDefinitionListItem
  { _dliColumn :: ColumnAlias,
    _dliType :: PGScalarType
  }
  deriving (Show, Eq, Data, Generic)

instance NFData FunctionDefinitionListItem

instance Hashable FunctionDefinitionListItem

instance ToSQL FunctionDefinitionListItem where
  toSQL (FunctionDefinitionListItem column columnType) =
    columnAliasToSqlWithoutAs column <~> toSQL columnType

-- | We can alias the result of a function call that returns a @SETOF RECORD@
--   by naming the result relation, and the columns and their types. For example:
--
-- > SELECT * FROM
-- > function_returns_record(arg1, arg2 ...) AS relation_name(column_1 column_1_type, column_2 column_2_type, ...)
--
--   Note: a function that returns a table (instead of a record) cannot name the types
--         as seen in the above example.
data FunctionAlias = FunctionAlias
  { _faIdentifier :: TableAlias, -- TODO: Rename to _faAlias
    _faDefinitionList :: Maybe [FunctionDefinitionListItem]
  }
  deriving (Show, Eq, Data, Generic)

instance NFData FunctionAlias

instance Hashable FunctionAlias

functionNameToTableAlias :: QualifiedFunction -> TableAlias
functionNameToTableAlias = mkTableAlias . qualifiedObjectToText

-- | Construct a function alias which represents the "relation signature" for the function invocation,
--   Using the function name as the relation name, and the columns as the relation schema.
mkFunctionAlias :: QualifiedObject FunctionName -> Maybe [(ColumnAlias, PGScalarType)] -> FunctionAlias
mkFunctionAlias alias listM =
  FunctionAlias (functionNameToTableAlias alias)
    $ fmap (map (uncurry FunctionDefinitionListItem)) listM

instance ToSQL FunctionAlias where
  toSQL (FunctionAlias tableAlias (Just definitionList)) =
    tableAliasToSqlWithAs tableAlias <> parenB (", " <+> definitionList)
  toSQL (FunctionAlias tableAlias Nothing) =
    tableAliasToSqlWithAs tableAlias

-- | A function call
data FunctionExp = FunctionExp
  { feName :: QualifiedFunction,
    feArgs :: FunctionArgs,
    feAlias :: Maybe FunctionAlias
  }
  deriving (Show, Eq, Generic, Data)

instance NFData FunctionExp

instance Hashable FunctionExp

instance ToSQL FunctionExp where
  toSQL (FunctionExp qf args alsM) =
    toSQL qf <> toSQL args <~> toSQL alsM

-- | See @from_item@ in <https://www.postgresql.org/docs/current/sql-select.html>
data FromItem
  = -- | A simple table
    FISimple QualifiedTable (Maybe TableAlias)
  | -- | An identifier (from CTEs)
    FIIdentifier TableIdentifier
  | -- | A function call (that should return a relation (@SETOF@) and not a scalar)
    FIFunc FunctionExp
  | -- | @unnest@ converts (an) array(s) to a relation.
    --
    --   We have:
    --   * The unnest function arguments
    --   * The relation alias
    --   * A list of column aliases
    --
    --   See @unnest@ in <https://www.postgresql.org/docs/current/functions-array.html>.
    FIUnnest [SQLExp] TableAlias [ColumnAlias]
  | FISelect Lateral Select TableAlias
  | FISelectWith Lateral (SelectWithG Select) TableAlias
  | FIValues ValuesExp TableAlias (Maybe [ColumnAlias])
  | FIJoin JoinExpr
  deriving (Show, Eq, Generic, Data)

instance NFData FromItem

instance Hashable FromItem

mkSelFromItem :: Select -> TableAlias -> FromItem
mkSelFromItem = FISelect (Lateral False)

mkSelectWithFromItem :: SelectWithG Select -> TableAlias -> FromItem
mkSelectWithFromItem = FISelectWith (Lateral False)

mkLateralFromItem :: Select -> TableAlias -> FromItem
mkLateralFromItem = FISelect (Lateral True)

instance ToSQL FromItem where
  toSQL (FISimple qualifiedTable tableAlias) =
    toSQL qualifiedTable <~> maybe "" tableAliasToSqlWithAs tableAlias
  toSQL (FIIdentifier iden) =
    toSQL iden
  toSQL (FIFunc funcExp) = toSQL funcExp
  -- unnest(expressions) alias(columns)
  toSQL (FIUnnest args tableAlias cols) =
    "UNNEST"
      <> parenB (", " <+> args)
      <~> tableAliasToSqlWithAs tableAlias
      <> parenB (", " <+> map columnAliasToSqlWithoutAs cols)
  toSQL (FISelect isLateral select alias) =
    toSQL isLateral <~> parenB (toSQL select) <~> tableAliasToSqlWithAs alias
  toSQL (FISelectWith isLateral selectWith alias) =
    toSQL isLateral <~> parenB (toSQL selectWith) <~> tableAliasToSqlWithAs alias
  toSQL (FIValues valsExp alias columnAliases) =
    parenB (toSQL valsExp)
      <~> tableAliasToSqlWithAs alias
      <~> case columnAliases of
        Nothing -> ""
        Just cols -> parenB (", " <+> map columnAliasToSqlWithoutAs cols)
  toSQL (FIJoin je) =
    toSQL je

newtype Lateral = Lateral Bool
  deriving (Show, Eq, Data, NFData, Hashable)

instance ToSQL Lateral where
  toSQL (Lateral True) = "LATERAL"
  toSQL (Lateral False) = mempty

data JoinExpr = JoinExpr
  { tjeLeft :: FromItem,
    tjeType :: JoinType,
    tjeRight :: FromItem,
    tjeJC :: JoinCond
  }
  deriving (Show, Eq, Generic, Data)

instance NFData JoinExpr

instance Hashable JoinExpr

instance ToSQL JoinExpr where
  toSQL je =
    toSQL (tjeLeft je)
      <~> toSQL (tjeType je)
      <~> toSQL (tjeRight je)
      <~> toSQL (tjeJC je)

data JoinType
  = Inner
  | LeftOuter
  | RightOuter
  | FullOuter
  deriving (Eq, Show, Generic, Data)

instance NFData JoinType

instance Hashable JoinType

instance ToSQL JoinType where
  toSQL Inner = "INNER JOIN"
  toSQL LeftOuter = "LEFT OUTER JOIN"
  toSQL RightOuter = "RIGHT OUTER JOIN"
  toSQL FullOuter = "FULL OUTER JOIN"

data JoinCond
  = JoinOn BoolExp
  | JoinUsing [Identifier]
  deriving (Show, Eq, Generic, Data)

instance NFData JoinCond

instance Hashable JoinCond

instance ToSQL JoinCond where
  toSQL (JoinOn be) =
    "ON" <~> parenB (toSQL be)
  toSQL (JoinUsing cols) =
    "USING" <~> parenB ("," <+> cols)

data BoolExp
  = BELit Bool
  | BEBin BinOp BoolExp BoolExp
  | BENot BoolExp
  | BECompare CompareOp SQLExp SQLExp
  | -- this is because l = (ANY (e)) is not valid
    -- i.e, (ANY(e)) is not same as ANY(e)
    BECompareAny CompareOp SQLExp SQLExp
  | BENull SQLExp
  | BENotNull SQLExp
  | BEExists Select
  | BEIN SQLExp [SQLExp]
  | BEExp SQLExp
  deriving (Show, Eq, Generic, Data)

instance NFData BoolExp

instance Hashable BoolExp

-- removes extraneous 'AND true's
simplifyBoolExp :: BoolExp -> BoolExp
simplifyBoolExp be = case be of
  BEBin AndOp e1 e2 ->
    let e1s = simplifyBoolExp e1
        e2s = simplifyBoolExp e2
     in if
          | e1s == BELit True -> e2s
          | e2s == BELit True -> e1s
          | otherwise -> BEBin AndOp e1s e2s
  BEBin OrOp e1 e2 ->
    let e1s = simplifyBoolExp e1
        e2s = simplifyBoolExp e2
     in if
          | e1s == BELit False -> e2s
          | e2s == BELit False -> e1s
          | otherwise -> BEBin OrOp e1s e2s
  e -> e

mkExists :: FromItem -> BoolExp -> BoolExp
mkExists fromItem whereFrag =
  BEExists
    mkSelect
      { selExtr = dummySelectList,
        selFrom = Just $ FromExp $ pure fromItem,
        selWhere = Just $ WhereFrag whereFrag
      }

instance ToSQL BoolExp where
  toSQL (BELit True) = TB.text "'true'"
  toSQL (BELit False) = TB.text "'false'"
  toSQL (BEBin bo bel ber) =
    parenB (toSQL bel) <~> toSQL bo <~> parenB (toSQL ber)
  toSQL (BENot be) =
    "NOT" <~> parenB (toSQL be)
  toSQL (BECompare co vl vr) =
    parenB (toSQL vl) <~> toSQL co <~> parenB (toSQL vr)
  toSQL (BECompareAny co vl vr) =
    parenB (toSQL vl) <~> toSQL co <~> "ANY" <> parenB (toSQL vr)
  toSQL (BENull v) =
    parenB (toSQL v) <~> "IS NULL"
  toSQL (BENotNull v) =
    parenB (toSQL v) <~> "IS NOT NULL"
  toSQL (BEExists sel) =
    "EXISTS " <~> parenB (toSQL sel)
  -- special case to handle lhs IN (exp1, exp2)
  toSQL (BEIN vl exps) =
    parenB (toSQL vl) <~> toSQL SIN <~> parenB (", " <+> exps)
  -- Any SQL expression which evaluates to bool value
  toSQL (BEExp e) = parenB $ toSQL e

data BinOp = AndOp | OrOp
  deriving (Show, Eq, Generic, Data)

instance NFData BinOp

instance Hashable BinOp

instance ToSQL BinOp where
  toSQL AndOp = "AND"
  toSQL OrOp = "OR"

data CompareOp
  = SEQ
  | SGT
  | SLT
  | SIN
  | SNE
  | SGTE
  | SLTE
  | SNIN
  | SLIKE
  | SNLIKE
  | SILIKE
  | SNILIKE
  | SSIMILAR
  | SNSIMILAR
  | SREGEX
  | SIREGEX
  | SNREGEX
  | SNIREGEX
  | SContains
  | SContainedIn
  | SHasKey
  | SHasKeysAny
  | SHasKeysAll
  | SMatchesFulltext
  deriving (Eq, Generic, Data)

instance NFData CompareOp

instance Hashable CompareOp

instance Show CompareOp where
  show = \case
    SEQ -> "="
    SGT -> ">"
    SLT -> "<"
    SIN -> "IN"
    SNE -> "<>"
    SGTE -> ">="
    SLTE -> "<="
    SNIN -> "NOT IN"
    SLIKE -> "LIKE"
    SNLIKE -> "NOT LIKE"
    SILIKE -> "ILIKE"
    SNILIKE -> "NOT ILIKE"
    SSIMILAR -> "SIMILAR TO"
    SNSIMILAR -> "NOT SIMILAR TO"
    SREGEX -> "~"
    SIREGEX -> "~*"
    SNREGEX -> "!~"
    SNIREGEX -> "!~*"
    SContains -> "@>"
    SContainedIn -> "<@"
    SHasKey -> "?"
    SHasKeysAny -> "?|"
    SHasKeysAll -> "?&"
    SMatchesFulltext -> "@"

instance ToSQL CompareOp where
  toSQL = fromString . show

data SQLDelete = SQLDelete
  { delTable :: QualifiedTable,
    delUsing :: Maybe UsingExp,
    delWhere :: Maybe WhereFrag,
    delRet :: Maybe RetExp
  }
  deriving (Show, Eq)

data SQLUpdate = SQLUpdate
  { upTable :: QualifiedTable,
    upSet :: SetExp,
    upFrom :: Maybe FromExp,
    upWhere :: Maybe WhereFrag,
    upRet :: Maybe RetExp
  }
  deriving (Show, Eq)

newtype SetExp = SetExp [SetExpItem]
  deriving (Show, Eq)

newtype SetExpItem = SetExpItem (PGCol, SQLExp)
  deriving (Show, Eq)

buildUpsertSetExp ::
  [PGCol] ->
  HashMap.HashMap PGCol SQLExp ->
  SetExp
buildUpsertSetExp cols preSet =
  SetExp $ map SetExpItem $ HashMap.toList setExps
  where
    setExps = HashMap.union preSet
      $ HashMap.fromList
      $ flip map cols
      $ \col ->
        (col, SEExcluded $ toIdentifier col)

newtype UsingExp = UsingExp [TableName]
  deriving (Show, Eq)

instance ToSQL UsingExp where
  toSQL (UsingExp tables) =
    "USING" <~> "," <+> tables

newtype RetExp = RetExp [Extractor]
  deriving (Show, Eq)

selectStar :: Extractor
selectStar = Extractor (SEStar Nothing) Nothing

selectStar' :: Qual -> Extractor
selectStar' q = Extractor (SEStar (Just q)) Nothing

returningStar :: RetExp
returningStar = RetExp [selectStar]

instance ToSQL RetExp where
  toSQL (RetExp []) =
    mempty
  toSQL (RetExp exps) =
    "RETURNING" <~> (", " <+> exps)

instance ToSQL SQLDelete where
  toSQL sd =
    "DELETE FROM"
      <~> toSQL (delTable sd)
      <~> toSQL (delUsing sd)
      <~> toSQL (delWhere sd)
      <~> toSQL (delRet sd)

instance ToSQL SQLUpdate where
  toSQL a =
    "UPDATE"
      <~> toSQL (upTable a)
      <~> toSQL (upSet a)
      <~> toSQL (upFrom a)
      <~> toSQL (upWhere a)
      <~> toSQL (upRet a)

instance ToSQL SetExp where
  toSQL (SetExp cvs) =
    "SET" <~> ("," <+> cvs)

instance ToSQL SetExpItem where
  toSQL (SetExpItem (col, val)) =
    toSQL col <~> "=" <~> toSQL val

data SQLConflictTarget
  = SQLColumn [PGCol]
  | SQLConstraint ConstraintName
  deriving (Show, Eq)

instance ToSQL SQLConflictTarget where
  toSQL (SQLColumn cols) =
    "("
      <~> ("," <+> cols)
      <~> ")"
  toSQL (SQLConstraint cons) = "ON CONSTRAINT" <~> toSQL cons

data SQLConflict
  = DoNothing (Maybe SQLConflictTarget)
  | Update SQLConflictTarget SetExp (Maybe WhereFrag)
  deriving (Show, Eq)

instance ToSQL SQLConflict where
  toSQL (DoNothing Nothing) = "ON CONFLICT DO NOTHING"
  toSQL (DoNothing (Just ct)) =
    "ON CONFLICT"
      <~> toSQL ct
      <~> "DO NOTHING"
  toSQL (Update ct set whr) =
    "ON CONFLICT"
      <~> toSQL ct
      <~> "DO UPDATE"
      <~> toSQL set
      <~> toSQL whr

newtype ValuesExp = ValuesExp {getValuesExp :: [TupleExp]}
  deriving (Show, Eq, Data, NFData, Hashable)

instance ToSQL ValuesExp where
  toSQL (ValuesExp tuples) =
    "VALUES" <~> (", " <+> tuples)

data SQLInsert = SQLInsert
  { siTable :: QualifiedTable,
    siCols :: [PGCol],
    siValues :: ValuesExp,
    siConflict :: (Maybe SQLConflict),
    siRet :: (Maybe RetExp)
  }
  deriving (Show, Eq)

instance ToSQL SQLInsert where
  toSQL si =
    "INSERT INTO"
      <~> toSQL (siTable si)
      <~> ( if null (siCols si)
              then
                "VALUES"
                  <~> ", "
                  <+> map (const ("(DEFAULT)" :: TB.Builder)) (getValuesExp (siValues si))
              else "(" <~> (", " <+> siCols si) <~> ")" <~> toSQL (siValues si)
          )
      <~> maybe "" toSQL (siConflict si)
      <~> toSQL (siRet si)

-- | Top-level Common Table Expression statement.
--
--   A top level CTE can be a query or a mutation statement.
--
--   Postgres supports mutations only in top-level CTEs.
--   See <https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-MODIFYING>
data TopLevelCTE
  = CTESelect Select
  | CTEInsert SQLInsert
  | CTEUpdate SQLUpdate
  | CTEDelete SQLDelete
  | CTEUnsafeRawSQL (InterpolatedQuery SQLExp)
  deriving (Show, Eq)

instance ToSQL TopLevelCTE where
  toSQL = \case
    CTESelect q -> toSQL q
    CTEInsert q -> toSQL q
    CTEUpdate q -> toSQL q
    CTEDelete q -> toSQL q
    CTEUnsafeRawSQL (InterpolatedQuery parts) ->
      foldMap
        ( \case
            IIText t -> TB.text t
            IIVariable v -> toSQL v
        )
        parts

-- | Represents a common table expresion that can be used in nested selects.
data InnerCTE
  = ICTESelect Select
  | ICTEUnsafeRawSQL (InterpolatedQuery SQLExp)
  deriving (Show, Eq, Generic, Data)

instance NFData InnerCTE

instance Hashable InnerCTE

toTopLevelCTE :: InnerCTE -> TopLevelCTE
toTopLevelCTE = \case
  ICTESelect select -> CTESelect select
  ICTEUnsafeRawSQL query -> CTEUnsafeRawSQL query

-- | A @SELECT@ statement with Common Table Expressions.
--   <https://www.postgresql.org/docs/current/queries-with.html>
--
--   These CTEs are determined by the @statement@ parameter.
--   Currently they are either 'TopLevelCTE', which allow for a query or mutation statement,
--   or 'Select', which only allow for querying results.
--
--   The distinction is required because Postgres only supports mutations in CTEs
--   at the top level.
--   See <https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-MODIFYING>
data SelectWithG statement = SelectWith
  { swCTEs :: [(TableAlias, statement)],
    swSelect :: Select
  }
  deriving (Show, Eq, Generic, Data)

instance (NFData v) => NFData (SelectWithG v)

instance (Hashable v) => Hashable (SelectWithG v)

instance (ToSQL v) => ToSQL (SelectWithG v) where
  toSQL (SelectWith [] sel) = toSQL sel
  toSQL (SelectWith ctes sel) =
    "WITH " <> (", " <+> map f ctes) <~> toSQL sel
    where
      f (al, q) = tableAliasToSqlWithoutAs al <~> "AS" <~> parenB (toSQL q)

-- | A top-level select with CTEs.
type SelectWith = SelectWithG TopLevelCTE

-- local helpers

infixr 6 <+>

(<+>) :: (ToSQL a) => Text -> [a] -> TB.Builder
(<+>) _ [] = mempty
(<+>) kat (x : xs) =
  toSQL x <> mconcat [TB.text kat <> toSQL x' | x' <- xs]
{-# INLINE (<+>) #-}
