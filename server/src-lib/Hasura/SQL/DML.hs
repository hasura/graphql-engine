module Hasura.SQL.DML where


import           Hasura.Prelude

import qualified Data.Aeson                 as J
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text.Extended         as T
import qualified Text.Builder               as TB

import           Data.String                (fromString)
import           Language.Haskell.TH.Syntax (Lift)

import           Hasura.Incremental         (Cacheable)
import           Hasura.SQL.Text
import           Hasura.SQL.Types



-- Builder

data SQLBuilder = SQLBuilder
  { sqlbLiteral    :: Text -> TB.Builder
  , sqlbOrderBy    :: OrderByItem -> TB.Builder
  , sqlbIdentifier :: Identifier -> TB.Builder
  }

class ToSQL a where
  toSQL :: SQLBuilder -> a -> TB.Builder

instance ToSQL a => ToSQL (Maybe a) where
  toSQL b = maybe mempty (toSQL b)

toSQLTxt :: ToSQL a => SQLBuilder -> a -> T.Text
toSQLTxt b a = TB.run $ toSQL b a

-- this uses a hardcoded equivalent of the postgres SQL builder
-- it is temporary and should not be used
unsafeToSQLTxt :: ToSQL a => a -> T.Text
unsafeToSQLTxt = toSQLTxt builder
  where
    builder = SQLBuilder pgLiteral pgOrderBy pgIdentifier
    pgOrderBy (OrderByItem e ot no) = toSQL builder e <-> toSQL builder ot <-> toSQL builder no
    pgLiteral    = TB.text . pgFmtLit
    pgIdentifier = TB.text . pgFmtIden . toTxt

infixr 6 <->
(<->) :: TB.Builder -> TB.Builder -> TB.Builder
(<->) l r = l <> TB.char ' ' <> r
{-# INLINE (<->) #-}

paren :: TB.Builder -> TB.Builder
paren t = TB.char '(' <> t <> TB.char ')'
{-# INLINE paren #-}

commaSeparated :: [TB.Builder] -> TB.Builder
commaSeparated = TB.intercalate ", "

-- types instances

instance ToSQL Identifier where
  toSQL = sqlbIdentifier

instance ToSQL TableName where
  toSQL b = toSQL b . toIdentifier

instance ToSQL FunctionName where
  toSQL b = toSQL b . toIdentifier

instance ToSQL SchemaName where
  toSQL b = toSQL b . toIdentifier

instance ToSQL ConstraintName where
  toSQL b = toSQL b . toIdentifier

instance ToSQL PGScalarType where
  toSQL _ = TB.text . toTxt

instance ToSQL PGCol where
  toSQL b = toSQL b . toIdentifier

instance ToSQL a => ToSQL (QualifiedObject a) where
  toSQL b (QualifiedObject sn o) = toSQL b sn <> "." <> toSQL b o

instance ToSQL a => ToSQL (PGType a) where
  toSQL b = \case
    PGTypeScalar ty -> toSQL b ty
    -- typename array is an sql standard way of declaring types
    PGTypeArray ty -> toSQL b ty <> " array"



-- DML

data Select
  = Select
    { selCTEs     :: ![(Alias, Select)]
    -- ^ Unlike 'SelectWith', does not allow data-modifying statements (as those are only allowed at
    -- the top level of a query).
    , selDistinct :: !(Maybe DistinctExpr)
    , selExtr     :: ![Extractor]
    , selFrom     :: !(Maybe FromExp)
    , selWhere    :: !(Maybe WhereFrag)
    , selGroupBy  :: !(Maybe GroupByExp)
    , selHaving   :: !(Maybe HavingExp)
    , selOrderBy  :: !(Maybe OrderByExp)
    , selLimit    :: !(Maybe LimitExp)
    , selOffset   :: !(Maybe OffsetExp)
    } deriving (Show, Eq, Generic, Data)
instance NFData Select
instance Cacheable Select
instance Hashable Select

instance ToSQL Select where
  toSQL b sel = case selCTEs sel of
    [] -> "SELECT"
      <-> toSQL b (selDistinct sel)
      <-> commaSeparated (toSQL b <$> selExtr sel)
      <-> toSQL b (selFrom sel)
      <-> toSQL b (selWhere sel)
      <-> toSQL b (selGroupBy sel)
      <-> toSQL b (selHaving sel)
      <-> toSQL b (selOrderBy sel)
      <-> toSQL b (selLimit sel)
      <-> toSQL b (selOffset sel)
    -- reuse SelectWith if there are any CTEs, since the generated SQL is the same
    ctes -> toSQL b $ SelectWith (map (CTESelect <$>) ctes) sel { selCTEs = [] }

mkSelect :: Select
mkSelect = Select [] Nothing [] Nothing
           Nothing Nothing Nothing
           Nothing Nothing Nothing


newtype LimitExp
  = LimitExp SQLExp
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL LimitExp where
  toSQL b (LimitExp se) =
    "LIMIT" <-> toSQL b se


newtype OffsetExp
  = OffsetExp SQLExp
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL OffsetExp where
  toSQL b (OffsetExp se) =
    "OFFSET" <-> toSQL b se


newtype GroupByExp
  = GroupByExp [SQLExp]
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL GroupByExp where
  toSQL b (GroupByExp idens) =
    "GROUP BY" <-> commaSeparated (toSQL b <$> idens)


newtype OrderByExp
  = OrderByExp (NonEmpty OrderByItem)
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL OrderByExp where
  toSQL b (OrderByExp l) =
    "ORDER BY" <-> commaSeparated (toSQL b <$> toList l)


data DistinctExpr
  = DistinctSimple
  | DistinctOn ![SQLExp]
  deriving (Show, Eq, Generic, Data)
instance NFData DistinctExpr
instance Cacheable DistinctExpr
instance Hashable DistinctExpr

instance ToSQL DistinctExpr where
  toSQL _ DistinctSimple    = "DISTINCT"
  toSQL b (DistinctOn exps) = "DISTINCT ON" <-> paren (commaSeparated $ toSQL b <$> exps)


data OrderByItem
  = OrderByItem
    { oColumn :: !SQLExp
    , oType   :: !(Maybe OrderType)
    , oNulls  :: !(Maybe NullsOrder)
    } deriving (Show, Eq, Generic, Data)
instance NFData OrderByItem
instance Cacheable OrderByItem
instance Hashable OrderByItem

instance ToSQL OrderByItem where
  toSQL b (OrderByItem e ot no) =
    toSQL b e <-> toSQL b ot <-> toSQL b no


data OrderType = OTAsc | OTDesc
  deriving (Show, Eq, Lift, Generic, Data)
instance NFData OrderType
instance Cacheable OrderType
instance Hashable OrderType

instance ToSQL OrderType where
  toSQL _ OTAsc  = "ASC"
  toSQL _ OTDesc = "DESC"


data NullsOrder
  = NFirst
  | NLast
  deriving (Show, Eq, Lift, Generic, Data)
instance NFData NullsOrder
instance Cacheable NullsOrder
instance Hashable NullsOrder

instance ToSQL NullsOrder where
  toSQL _ NFirst = "NULLS FIRST"
  toSQL _ NLast  = "NULLS LAST"


newtype FromExp
  = FromExp [FromItem]
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL FromExp where
  toSQL b (FromExp items) =
    "FROM" <-> commaSeparated (toSQL b <$> items)

mkIdentifierFromExp :: (IsIdentifier a) => a -> FromExp
mkIdentifierFromExp a =
  FromExp [FIIden $ toIdentifier a]

mkSimpleFromExp :: QualifiedTable -> FromExp
mkSimpleFromExp qt =
  FromExp [FISimple qt Nothing]

mkSelFromExp :: Bool -> Select -> TableName -> FromItem
mkSelFromExp isLateral sel tn =
  FISelect (Lateral isLateral) sel alias
  where
    alias = Alias $ toIdentifier tn

mkFuncFromItem :: QualifiedFunction -> FunctionArgs -> FromItem
mkFuncFromItem qf args = FIFunc $ FunctionExp qf args Nothing


newtype HavingExp
  = HavingExp BoolExp
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL HavingExp where
  toSQL b (HavingExp be) =
    "HAVING" <-> toSQL b be


newtype WhereFrag
  = WhereFrag { getWFBoolExp :: BoolExp }
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL WhereFrag where
  toSQL b (WhereFrag be) =
    "WHERE" <-> paren (toSQL b be)


data Qual
  = QualIden !Identifier !(Maybe TypeAnn)
  | QualTable !QualifiedTable
  | QualVar !T.Text
  deriving (Show, Eq, Generic, Data)
instance NFData Qual
instance Cacheable Qual
instance Hashable Qual

instance ToSQL Qual where
  toSQL b (QualIden i tyM) = toSQL b i <> toSQL b tyM
  toSQL b (QualTable qt)   = toSQL b qt
  toSQL _ (QualVar v)      = TB.text v

mkQual :: QualifiedTable -> Qual
mkQual = QualTable


data QIden
  = QIden !Qual !Identifier
  deriving (Show, Eq, Generic, Data)
instance NFData QIden
instance Cacheable QIden
instance Hashable QIden

instance ToSQL QIden where
  toSQL b (QIden qual iden) =
    mconcat [toSQL b qual, TB.char '.', toSQL b iden]

mkQIden :: (IsIdentifier a, IsIdentifier b) => a -> b -> QIden
mkQIden q t = QIden (QualIden (toIdentifier q) Nothing) (toIdentifier t)


newtype SQLOp
  = SQLOp {sqlOpTxt :: T.Text}
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

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


-- FIXME: preserve type information before generating to SQL
newtype TypeAnn
  = TypeAnn { unTypeAnn :: T.Text }
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL TypeAnn where
  toSQL _ (TypeAnn ty) = "::" <> TB.text ty

mkTypeAnn :: PGType PGScalarType -> TypeAnn
mkTypeAnn = TypeAnn . unsafeToSQLTxt

intTypeAnn :: TypeAnn
intTypeAnn = mkTypeAnn $ PGTypeScalar PGInteger

numericTypeAnn :: TypeAnn
numericTypeAnn = mkTypeAnn $ PGTypeScalar PGNumeric

textTypeAnn :: TypeAnn
textTypeAnn = mkTypeAnn $ PGTypeScalar PGText

textArrTypeAnn :: TypeAnn
textArrTypeAnn = mkTypeAnn $ PGTypeArray PGText

jsonTypeAnn :: TypeAnn
jsonTypeAnn = mkTypeAnn $ PGTypeScalar PGJSON

jsonbTypeAnn :: TypeAnn
jsonbTypeAnn = mkTypeAnn $ PGTypeScalar PGJSONB

boolTypeAnn :: TypeAnn
boolTypeAnn = mkTypeAnn $ PGTypeScalar PGBoolean


data CountType
  = CTStar
  | CTSimple ![PGCol]
  | CTDistinct ![PGCol]
  deriving (Show, Eq, Generic, Data)
instance NFData CountType
instance Cacheable CountType
instance Hashable CountType

instance ToSQL CountType where
  toSQL _ CTStar            = "*"
  toSQL b (CTSimple cols)   = paren $ commaSeparated (toSQL b <$> cols)
  toSQL b (CTDistinct cols) = "DISTINCT" <-> paren (commaSeparated $ toSQL b <$> cols)


newtype TupleExp
  = TupleExp [SQLExp]
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL TupleExp where
  toSQL b (TupleExp exps) = paren $ commaSeparated $ toSQL b <$> exps


data SQLExp
  = SEPrep !Int
  | SENull
  | SELit !T.Text
  | SEUnsafe !T.Text
  | SESelect !Select
  | SEStar !(Maybe Qual)
  -- ^ all fields (@*@) or all fields from relation (@iden.*@)
  | SEIden !Identifier
  -- iden and row identifier are distinguished for easier rewrite rules
  | SERowIden !Identifier
  | SEQIden !QIden
  | SEFnApp !T.Text ![SQLExp] !(Maybe OrderByExp)
  | SEOpApp !SQLOp ![SQLExp]
  | SETyAnn !SQLExp !TypeAnn
  | SECond !BoolExp !SQLExp !SQLExp
  | SEBool !BoolExp
  | SEExcluded !Identifier
  | SEArray ![SQLExp]
  | SEArrayIndex !SQLExp !SQLExp
  | SETuple !TupleExp
  | SECount !CountType
  | SENamedArg !Identifier !SQLExp
  | SEFunction !FunctionExp
  deriving (Show, Eq, Generic, Data)
instance NFData SQLExp
instance Cacheable SQLExp
instance Hashable SQLExp

instance J.ToJSON SQLExp where
  toJSON = J.toJSON . unsafeToSQLTxt

instance ToSQL SQLExp where
  toSQL _ (SEPrep argNumber) =
    TB.char '$' <> fromString (show argNumber)
  toSQL _ SENull =
    TB.text "NULL"
  toSQL b (SELit tv) =
    sqlbLiteral b tv
  toSQL _ (SEUnsafe t) =
    TB.text t
  toSQL b (SESelect se) =
    paren $ toSQL b se
  toSQL _ (SEStar Nothing) =
    TB.char '*'
  toSQL b (SEStar (Just qual)) =
    mconcat [paren (toSQL b qual), TB.char '.', TB.char '*']
  toSQL b (SEIden iden) =
    toSQL b iden
  toSQL b (SERowIden iden) =
    toSQL b iden
  toSQL b (SEQIden qIden) =
    toSQL b qIden
  -- https://www.postgresql.org/docs/10/static/sql-expressions.html#SYNTAX-AGGREGATES
  toSQL b (SEFnApp name args mObe) =
    TB.text name <> paren (commaSeparated (toSQL b <$> args) <-> toSQL b mObe)
  toSQL b (SEOpApp op args) =
     paren $ TB.intercalate (TB.text $ sqlOpTxt op) $ toSQL b <$> args
  toSQL b (SETyAnn e ty) =
     paren (toSQL b e) <> toSQL b ty
  toSQL b (SECond cond te fe) =
    "CASE WHEN" <-> toSQL b cond <->
    "THEN" <-> toSQL b te <->
    "ELSE" <-> toSQL b fe <->
    "END"
  toSQL b (SEBool be) = toSQL b be
  toSQL b (SEExcluded i) = "EXCLUDED." <> toSQL b i
  toSQL b (SEArray exps) = "ARRAY" <> TB.char '[' <> (commaSeparated $ toSQL b <$> exps) <> TB.char ']'
  toSQL b (SEArrayIndex arrayExp indexExp) =
    paren (toSQL b arrayExp)
    <> TB.char '[' <> toSQL b indexExp <> TB.char ']'
  toSQL b (SETuple tup) = toSQL b tup
  toSQL b (SECount ty) = "COUNT" <> paren (toSQL b ty)
  -- https://www.postgresql.org/docs/current/sql-syntax-calling-funcs.html
  toSQL b (SENamedArg arg val) = toSQL b arg <-> "=>" <-> toSQL b val
  toSQL b (SEFunction funcExp) = toSQL b funcExp

withTyAnn :: PGScalarType -> SQLExp -> SQLExp
withTyAnn colTy v = SETyAnn v . mkTypeAnn $ PGTypeScalar colTy

mkSIdenExp :: (IsIdentifier a) => a -> SQLExp
mkSIdenExp = SEIden . toIdentifier

mkQIdenExp :: (IsIdentifier a, IsIdentifier b) => a -> b -> SQLExp
mkQIdenExp q t = SEQIden $ mkQIden q t

countStar :: SQLExp
countStar = SECount CTStar

intToSQLExp :: Int -> SQLExp
intToSQLExp =
  SEUnsafe . T.pack . show

mkSQLOpExp
  :: SQLOp
  -> SQLExp -- lhs
  -> SQLExp -- rhs
  -> SQLExp -- result
mkSQLOpExp op lhs rhs = SEOpApp op [lhs, rhs]


newtype Alias
  = Alias { getAlias :: Identifier }
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance IsIdentifier Alias where
  toIdentifier = getAlias
  {-# INLINE toIdentifier #-}

instance ToSQL Alias where
  toSQL b (Alias iden) = "AS" <-> toSQL b iden

toAlias :: (IsIdentifier a) => a -> Alias
toAlias = Alias . toIdentifier


data Extractor = Extractor !SQLExp !(Maybe Alias)
  deriving (Show, Eq, Generic, Data)
instance NFData Extractor
instance Cacheable Extractor
instance Hashable Extractor

instance ToSQL Extractor where
  toSQL b (Extractor ce mal) =
    toSQL b ce <-> toSQL b mal

mkRowExp :: [Extractor] -> SQLExp
mkRowExp extrs = let
  innerSel = mkSelect { selExtr = extrs }
  innerSelName = TableName "e"

  -- SELECT r FROM (SELECT col1, col2, .. ) AS r
  outerSel = mkSelect
             { selExtr = [Extractor (SERowIden $ toIdentifier innerSelName) Nothing]
             , selFrom = Just $ FromExp
                         [mkSelFromExp False innerSel innerSelName]
             }
  in
    SESelect outerSel


buildInsVal :: PGCol -> Int -> (PGCol, SQLExp)
buildInsVal colName argNumber =
  (colName, SEPrep argNumber)

toColTupExp :: [PGCol] -> SQLExp
toColTupExp =
  SETuple . TupleExp . map (SEIden . Identifier . getPGColTxt)

mkColDefValMap :: [PGCol] -> HM.HashMap PGCol SQLExp
mkColDefValMap cols =
  HM.fromList $ zip cols (repeat $ SEUnsafe "DEFAULT")

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

getExtrAlias :: Extractor -> Maybe Alias
getExtrAlias (Extractor _ ma) = ma

mkAliasedExtr :: (IsIdentifier a, IsIdentifier b) => a -> Maybe b -> Extractor
mkAliasedExtr t = mkAliasedExtrFromExp (mkSIdenExp t)

mkAliasedExtrFromExp :: (IsIdentifier a) => SQLExp -> Maybe a -> Extractor
mkAliasedExtrFromExp sqlExp ma = Extractor sqlExp (aliasF <$> ma)
  where
    aliasF = Alias . toIdentifier

mkExtr :: (IsIdentifier a) => a -> Extractor
mkExtr t = Extractor (mkSIdenExp t) Nothing


data FunctionArgs
  = FunctionArgs
  { fasPostional :: ![SQLExp]
  , fasNamed     :: !(HM.HashMap Text SQLExp)
  } deriving (Show, Eq, Generic, Data)
instance NFData FunctionArgs
instance Cacheable FunctionArgs
instance Hashable FunctionArgs

instance ToSQL FunctionArgs where
  toSQL b (FunctionArgs positionalArgs namedArgsMap) =
    let namedArgs = flip map (HM.toList namedArgsMap) $
                    \(argName, argVal) -> SENamedArg (Identifier argName) argVal
    in paren $ commaSeparated $ toSQL b <$> (positionalArgs <> namedArgs)


data DefinitionListItem
  = DefinitionListItem
  { _dliColumn :: !PGCol
  , _dliType   :: !PGScalarType
  } deriving (Show, Eq, Data, Generic)
instance NFData DefinitionListItem
instance Cacheable DefinitionListItem
instance Hashable DefinitionListItem

instance ToSQL DefinitionListItem where
  toSQL b (DefinitionListItem column columnType) =
    toSQL b column <-> toSQL b columnType


data FunctionAlias
  = FunctionAlias
  { _faIden           :: !Alias
  , _faDefinitionList :: !(Maybe [DefinitionListItem])
  } deriving (Show, Eq, Data, Generic)
instance NFData FunctionAlias
instance Cacheable FunctionAlias
instance Hashable FunctionAlias

instance ToSQL FunctionAlias where
  toSQL b (FunctionAlias iden (Just definitionList)) =
    toSQL b iden <> paren (commaSeparated $ toSQL b <$> definitionList)
  toSQL b (FunctionAlias iden Nothing) =
    toSQL b iden

mkSimpleFunctionAlias :: Identifier -> FunctionAlias
mkSimpleFunctionAlias identifier =
  FunctionAlias (toAlias identifier) Nothing

mkFunctionAlias :: Identifier -> Maybe [(PGCol, PGScalarType)] -> FunctionAlias
mkFunctionAlias identifier listM =
  FunctionAlias (toAlias identifier) $
  fmap (map (uncurry DefinitionListItem)) listM


data FunctionExp
  = FunctionExp
  { feName  :: !QualifiedFunction
  , feArgs  :: !FunctionArgs
  , feAlias :: !(Maybe FunctionAlias)
  } deriving (Show, Eq, Generic, Data)
instance NFData FunctionExp
instance Cacheable FunctionExp
instance Hashable FunctionExp

instance ToSQL FunctionExp where
  toSQL b (FunctionExp qf args alsM) =
    toSQL b qf <> toSQL b args <-> toSQL b alsM


data FromItem
  = FISimple !QualifiedTable !(Maybe Alias)
  | FIIden !Identifier
  | FIFunc !FunctionExp
  | FIUnnest ![SQLExp] !Alias ![SQLExp]
  | FISelect !Lateral !Select !Alias
  | FISelectWith !Lateral !(SelectWithG Select) !Alias
  | FIValues !ValuesExp !Alias !(Maybe [PGCol])
  | FIJoin !JoinExpr
  deriving (Show, Eq, Generic, Data)
instance NFData FromItem
instance Cacheable FromItem
instance Hashable FromItem

instance ToSQL FromItem where
  toSQL b (FISimple qt mal) =
    toSQL b qt <-> toSQL b mal
  toSQL b (FIIden iden) =
    toSQL b iden
  toSQL b (FIFunc funcExp) = toSQL b funcExp
  -- unnest(expressions) alias(columns)
  toSQL b (FIUnnest args als cols) =
    "UNNEST" <> paren (commaSeparated (toSQL b <$> args)) <-> toSQL b als <> paren (commaSeparated $ toSQL b <$> cols)
  toSQL b (FISelect mla sel al) =
    toSQL b mla <-> paren (toSQL b sel) <-> toSQL b al
  toSQL b (FISelectWith mla selWith al) =
    toSQL b mla <-> paren (toSQL b selWith) <-> toSQL b al
  toSQL b (FIValues valsExp al mCols) =
    paren (toSQL b valsExp) <-> toSQL b al
    <-> toSQL b (toColTupExp <$> mCols)
  toSQL b (FIJoin je) =
    toSQL b je

mkSelFromItem :: Select -> Alias -> FromItem
mkSelFromItem = FISelect (Lateral False)

mkSelectWithFromItem :: SelectWithG Select -> Alias -> FromItem
mkSelectWithFromItem = FISelectWith (Lateral False)

mkLateralFromItem :: Select -> Alias -> FromItem
mkLateralFromItem = FISelect (Lateral True)


newtype Lateral = Lateral Bool
  deriving (Show, Eq, Data, NFData, Cacheable, Hashable)

instance ToSQL Lateral where
  toSQL _ (Lateral True)  = "LATERAL"
  toSQL _ (Lateral False) = mempty


data JoinExpr
  = JoinExpr
  { tjeLeft  :: !FromItem
  , tjeType  :: !JoinType
  , tjeRight :: !FromItem
  , tjeJC    :: !JoinCond
  } deriving (Show, Eq, Generic, Data)
instance NFData JoinExpr
instance Cacheable JoinExpr
instance Hashable JoinExpr

instance ToSQL JoinExpr where
  toSQL b je =
    toSQL b (tjeLeft je)
    <-> toSQL b (tjeType je)
    <-> toSQL b (tjeRight je)
    <-> toSQL b (tjeJC je)


data JoinType
  = Inner
  | LeftOuter
  | RightOuter
  | FullOuter
  deriving (Eq, Show, Generic, Data)
instance NFData JoinType
instance Cacheable JoinType
instance Hashable JoinType

instance ToSQL JoinType where
  toSQL _ Inner      = "INNER JOIN"
  toSQL _ LeftOuter  = "LEFT OUTER JOIN"
  toSQL _ RightOuter = "RIGHT OUTER JOIN"
  toSQL _ FullOuter  = "FULL OUTER JOIN"


data JoinCond
  = JoinOn !BoolExp
  | JoinUsing ![PGCol]
  deriving (Show, Eq, Generic, Data)
instance NFData JoinCond
instance Cacheable JoinCond
instance Hashable JoinCond

instance ToSQL JoinCond where
  toSQL b (JoinOn be) =
    "ON" <-> paren (toSQL b be)
  toSQL b (JoinUsing cols) =
    "USING" <-> paren (commaSeparated $ toSQL b <$> cols)


data BoolExp
  = BELit !Bool
  | BEBin !BinOp !BoolExp !BoolExp
  | BENot !BoolExp
  | BECompare !CompareOp !SQLExp !SQLExp
  -- this is because l = (ANY (e)) is not valid
  -- i.e, (ANY(e)) is not same as ANY(e)
  | BECompareAny !CompareOp !SQLExp !SQLExp
  | BENull !SQLExp
  | BENotNull !SQLExp
  | BEExists !Select
  | BEIN !SQLExp ![SQLExp]
  | BEExp !SQLExp
  deriving (Show, Eq, Generic, Data)
instance NFData BoolExp
instance Cacheable BoolExp
instance Hashable BoolExp

instance ToSQL BoolExp where
  toSQL _ (BELit True)  = "TRUE"
  toSQL _ (BELit False) = "FALSE"
  toSQL b (BEBin bo bel ber) =
    paren (toSQL b bel) <-> toSQL b bo <-> paren (toSQL b ber)
  toSQL b (BENot be) =
    "NOT" <-> paren (toSQL b be)
  toSQL b (BECompare co vl vr) =
    paren (toSQL b vl) <-> toSQL b co <-> paren (toSQL b vr)
  toSQL b (BECompareAny co vl vr) =
    paren (toSQL b vl) <-> toSQL b co <-> "ANY" <> paren (toSQL b vr)
  toSQL b (BENull v) =
    paren (toSQL b v) <-> "IS NULL"
  toSQL b (BENotNull v) =
    paren (toSQL b v) <-> "IS NOT NULL"
  toSQL b (BEExists sel) =
    "EXISTS " <-> paren (toSQL b sel)
  -- special case to handle lhs IN (exp1, exp2)
  toSQL b (BEIN vl exps) =
    paren (toSQL b vl) <-> toSQL b SIN <-> paren (commaSeparated $ toSQL b <$> exps)
  -- Any SQL expression which evaluates to bool value
  toSQL b (BEExp e) = paren $ toSQL b e

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
  e                          -> e

mkExists :: FromItem -> BoolExp -> BoolExp
mkExists fromItem whereFrag =
  BEExists mkSelect
  { selExtr  = [Extractor (SEUnsafe "1") Nothing]
  , selFrom  = Just $ FromExp $ pure fromItem
  , selWhere = Just $ WhereFrag whereFrag
  }


data BinOp = AndOp | OrOp
  deriving (Show, Eq, Generic, Data)
instance NFData BinOp
instance Cacheable BinOp
instance Hashable BinOp

instance ToSQL BinOp where
  toSQL _ AndOp = "AND"
  toSQL _ OrOp  = "OR"


data CompareOp
  = SEQ
  | SGT
  | SLT
  | SIN
  | SNE
  | SLIKE
  | SNLIKE
  | SILIKE
  | SNILIKE
  | SSIMILAR
  | SNSIMILAR
  | SGTE
  | SLTE
  | SNIN
  | SContains
  | SContainedIn
  | SHasKey
  | SHasKeysAny
  | SHasKeysAll
  deriving (Eq, Generic, Data)
instance NFData CompareOp
instance Cacheable CompareOp
instance Hashable CompareOp

instance Show CompareOp where
  show = \case
    SEQ          -> "="
    SGT          -> ">"
    SLT          -> "<"
    SIN          -> "IN"
    SNE          -> "<>"
    SGTE         -> ">="
    SLTE         -> "<="
    SNIN         -> "NOT IN"
    SLIKE        -> "LIKE"
    SNLIKE       -> "NOT LIKE"
    SILIKE       -> "ILIKE"
    SNILIKE      -> "NOT ILIKE"
    SSIMILAR     -> "SIMILAR TO"
    SNSIMILAR    -> "NOT SIMILAR TO"
    SContains    -> "@>"
    SContainedIn -> "<@"
    SHasKey      -> "?"
    SHasKeysAny  -> "?|"
    SHasKeysAll  -> "?&"

instance ToSQL CompareOp where
  toSQL _ = fromString . show


data SQLDelete
  = SQLDelete
    { delTable :: !QualifiedTable
    , delUsing :: !(Maybe UsingExp)
    , delWhere :: !(Maybe WhereFrag)
    , delRet   :: !(Maybe RetExp)
    } deriving (Show, Eq)

instance ToSQL SQLDelete where
  toSQL b sd = "DELETE FROM"
             <-> toSQL b (delTable sd)
             <-> toSQL b (delUsing sd)
             <-> toSQL b (delWhere sd)
             <-> toSQL b (delRet sd)


data SQLUpdate
  = SQLUpdate
    { upTable :: !QualifiedTable
    , upSet   :: !SetExp
    , upFrom  :: !(Maybe FromExp)
    , upWhere :: !(Maybe WhereFrag)
    , upRet   :: !(Maybe RetExp)
    } deriving (Show, Eq)

instance ToSQL SQLUpdate where
  toSQL b a = "UPDATE"
            <-> toSQL b (upTable a)
            <-> toSQL b (upSet a)
            <-> toSQL b (upFrom a)
            <-> toSQL b (upWhere a)
            <-> toSQL b (upRet a)


newtype SetExp = SetExp [SetExpItem]
  deriving (Show, Eq)

instance ToSQL SetExp where
  toSQL b (SetExp cvs) =
    "SET" <-> (commaSeparated $ toSQL b <$> cvs)


newtype SetExpItem = SetExpItem (PGCol, SQLExp)
  deriving (Show, Eq)

instance ToSQL SetExpItem where
  toSQL b (SetExpItem (col, val)) =
    toSQL b col <-> "=" <-> toSQL b val

buildSEI :: PGCol -> Int -> SetExpItem
buildSEI colName argNumber =
  SetExpItem (colName, SEPrep argNumber)

buildUpsertSetExp
  :: [PGCol]
  -> HM.HashMap PGCol SQLExp
  -> SetExp
buildUpsertSetExp cols preSet =
  SetExp $ map SetExpItem $ HM.toList setExps
  where
    setExps = HM.union preSet $ HM.fromList $
      flip map cols $ \col ->
        (col, SEExcluded $ toIdentifier col)


newtype UsingExp = UsingExp [TableName]
                  deriving (Show, Eq)

instance ToSQL UsingExp where
  toSQL b (UsingExp tables)
    = "USING" <-> commaSeparated (toSQL b <$> tables)


newtype RetExp = RetExp [Extractor]
                  deriving (Show, Eq)

instance ToSQL RetExp where
  toSQL _ (RetExp [])
    = mempty
  toSQL b (RetExp exps)
    = "RETURNING" <-> (commaSeparated $ toSQL b <$> exps)


selectStar :: Extractor
selectStar = Extractor (SEStar Nothing) Nothing

selectStar' :: Qual -> Extractor
selectStar' q = Extractor (SEStar (Just q)) Nothing

returningStar :: RetExp
returningStar = RetExp [selectStar]


data SQLConflictTarget
  = SQLColumn ![PGCol]
  | SQLConstraint !ConstraintName
  deriving (Show, Eq)

instance ToSQL SQLConflictTarget where
  toSQL b (SQLColumn cols)      = "("
                                <-> (commaSeparated $ toSQL b <$> cols)
                                <-> ")"

  toSQL b (SQLConstraint cons) = "ON CONSTRAINT" <-> toSQL b cons


data SQLConflict
  = DoNothing !(Maybe SQLConflictTarget)
  | Update !SQLConflictTarget !SetExp !(Maybe WhereFrag)
  deriving (Show, Eq)

instance ToSQL SQLConflict where
  toSQL _ (DoNothing Nothing)   = "ON CONFLICT DO NOTHING"
  toSQL b (DoNothing (Just ct)) = "ON CONFLICT"
                                <-> toSQL b ct
                                <-> "DO NOTHING"
  toSQL b (Update ct set whr)   = "ON CONFLICT"
                                <-> toSQL b ct <-> "DO UPDATE"
                                <-> toSQL b set <-> toSQL b whr


newtype ValuesExp
  = ValuesExp [TupleExp]
  deriving (Show, Eq, Data, NFData, Cacheable, Hashable)

instance ToSQL ValuesExp where
  toSQL b (ValuesExp tuples) =
    "VALUES" <-> (commaSeparated $ toSQL b <$> tuples)


data SQLInsert = SQLInsert
    { siTable    :: !QualifiedTable
    , siCols     :: ![PGCol]
    , siValues   :: !ValuesExp
    , siConflict :: !(Maybe SQLConflict)
    , siRet      :: !(Maybe RetExp)
    } deriving (Show, Eq)

instance ToSQL SQLInsert where
  toSQL b si =
    "INSERT INTO"
    <-> toSQL b (siTable si)
    <-> "("
    <-> (commaSeparated $ toSQL b <$> siCols si)
    <-> ")"
    <-> toSQL b (siValues si)
    <-> maybe "" (toSQL b) (siConflict si)
    <-> toSQL b (siRet si)


data CTE
  = CTESelect !Select
  | CTEInsert !SQLInsert
  | CTEUpdate !SQLUpdate
  | CTEDelete !SQLDelete
  deriving (Show, Eq)

instance ToSQL CTE where
  toSQL b = \case
    CTESelect q -> toSQL b q
    CTEInsert q -> toSQL b q
    CTEUpdate q -> toSQL b q
    CTEDelete q -> toSQL b q


data SelectWithG v
  = SelectWith
  { swCTEs   :: ![(Alias, v)]
  , swSelect :: !Select
  } deriving (Show, Eq, Generic, Data)

instance (NFData v) => NFData (SelectWithG v)
instance (Cacheable v) => Cacheable (SelectWithG v)
instance (Hashable v) => Hashable (SelectWithG v)

instance (ToSQL v) => ToSQL (SelectWithG v) where
  toSQL b (SelectWith ctes sel) =
    "WITH " <> (commaSeparated $ map f ctes) <-> toSQL b sel
    where
      f (Alias al, q) = toSQL b al <-> "AS" <-> paren (toSQL b q)

type SelectWith = SelectWithG CTE
