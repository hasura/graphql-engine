module Hasura.SQL.DML where

import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.SQL.Types

import           Data.String                (fromString)
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Aeson                 as J
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text.Extended         as T
import qualified Text.Builder               as TB

infixr 6 <->
(<->) :: TB.Builder -> TB.Builder -> TB.Builder
(<->) l r = l <> TB.char ' ' <> r
{-# INLINE (<->) #-}

paren :: TB.Builder -> TB.Builder
paren t = TB.char '(' <> t <> TB.char ')'
{-# INLINE paren #-}

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

mkSelect :: Select
mkSelect = Select [] Nothing [] Nothing
           Nothing Nothing Nothing
           Nothing Nothing Nothing

newtype LimitExp
  = LimitExp SQLExp
  deriving (Show, Eq, NFData, Data, Cacheable)

instance ToSQL LimitExp where
  toSQL (LimitExp se) =
    "LIMIT" <-> toSQL se

newtype OffsetExp
  = OffsetExp SQLExp
  deriving (Show, Eq, NFData, Data, Cacheable)

instance ToSQL OffsetExp where
  toSQL (OffsetExp se) =
    "OFFSET" <-> toSQL se

newtype OrderByExp
  = OrderByExp [OrderByItem]
  deriving (Show, Eq, NFData, Data, Cacheable)

data OrderByItem
  = OrderByItem
    { oColumn :: !SQLExp
    , oType   :: !(Maybe OrderType)
    , oNulls  :: !(Maybe NullsOrder)
    } deriving (Show, Eq, Generic, Data)
instance NFData OrderByItem
instance Cacheable OrderByItem

instance ToSQL OrderByItem where
  toSQL (OrderByItem e ot no) =
    toSQL e <-> toSQL ot <-> toSQL no

data OrderType = OTAsc | OTDesc
  deriving (Show, Eq, Lift, Generic, Data)
instance NFData OrderType
instance Cacheable OrderType

instance ToSQL OrderType where
  toSQL OTAsc  = "ASC"
  toSQL OTDesc = "DESC"

data NullsOrder
  = NFirst
  | NLast
  deriving (Show, Eq, Lift, Generic, Data)
instance NFData NullsOrder
instance Cacheable NullsOrder

instance ToSQL NullsOrder where
  toSQL NFirst = "NULLS FIRST"
  toSQL NLast  = "NULLS LAST"

instance ToSQL OrderByExp where
  toSQL (OrderByExp l) =
    "ORDER BY" <-> (", " <+> l)

newtype GroupByExp
  = GroupByExp [SQLExp]
  deriving (Show, Eq, NFData, Data, Cacheable)

instance ToSQL GroupByExp where
  toSQL (GroupByExp idens) =
    "GROUP BY" <-> (", " <+> idens)

newtype FromExp
  = FromExp [FromItem]
  deriving (Show, Eq, NFData, Data, Cacheable)

instance ToSQL FromExp where
  toSQL (FromExp items) =
    "FROM" <-> (", " <+> items)

mkIdenFromExp :: (IsIden a) => a -> FromExp
mkIdenFromExp a =
  FromExp [FIIden $ toIden a]

mkSimpleFromExp :: QualifiedTable -> FromExp
mkSimpleFromExp qt =
  FromExp [FISimple qt Nothing]

mkSelFromExp :: Bool -> Select -> TableName -> FromItem
mkSelFromExp isLateral sel tn =
  FISelect (Lateral isLateral) sel alias
  where
    alias = Alias $ toIden tn

mkFuncFromItem :: QualifiedFunction -> FunctionArgs -> FromItem
mkFuncFromItem qf args = FIFunc $ FunctionExp qf args Nothing

mkRowExp :: [Extractor] -> SQLExp
mkRowExp extrs = let
  innerSel = mkSelect { selExtr = extrs }

  innerSelName = TableName "e"

  -- SELECT r FROM (SELECT col1, col2, .. ) AS r
  outerSel = mkSelect
             { selExtr = [Extractor (SERowIden $ toIden innerSelName) Nothing]
             , selFrom = Just $ FromExp
                         [mkSelFromExp False innerSel innerSelName]
             }
  in
    SESelect outerSel

newtype HavingExp
  = HavingExp BoolExp
  deriving (Show, Eq, NFData, Data, Cacheable)

instance ToSQL HavingExp where
  toSQL (HavingExp be) =
    "HAVING" <-> toSQL be

newtype WhereFrag
  = WhereFrag { getWFBoolExp :: BoolExp }
  deriving (Show, Eq, NFData, Data, Cacheable)

instance ToSQL WhereFrag where
  toSQL (WhereFrag be) =
    "WHERE" <-> paren (toSQL be)

instance ToSQL Select where
  toSQL sel = case selCTEs sel of
    [] -> "SELECT"
      <-> toSQL (selDistinct sel)
      <-> (", " <+> selExtr sel)
      <-> toSQL (selFrom sel)
      <-> toSQL (selWhere sel)
      <-> toSQL (selGroupBy sel)
      <-> toSQL (selHaving sel)
      <-> toSQL (selOrderBy sel)
      <-> toSQL (selLimit sel)
      <-> toSQL (selOffset sel)
    -- reuse SelectWith if there are any CTEs, since the generated SQL is the same
    ctes -> toSQL $ SelectWith (map (CTESelect <$>) ctes) sel { selCTEs = [] }


mkSIdenExp :: (IsIden a) => a -> SQLExp
mkSIdenExp = SEIden . toIden

mkQIdenExp :: (IsIden a, IsIden b) => a -> b -> SQLExp
mkQIdenExp q t = SEQIden $ mkQIden q t

data Qual
  = QualIden !Iden !(Maybe TypeAnn)
  | QualTable !QualifiedTable
  | QualVar !T.Text
  deriving (Show, Eq, Generic, Data)
instance NFData Qual
instance Cacheable Qual

mkQual :: QualifiedTable -> Qual
mkQual = QualTable

instance ToSQL Qual where
  toSQL (QualIden i tyM) = toSQL i <> toSQL tyM
  toSQL (QualTable qt)   = toSQL qt
  toSQL (QualVar v)      = TB.text v

mkQIden :: (IsIden a, IsIden b) => a -> b -> QIden
mkQIden q t = QIden (QualIden (toIden q) Nothing) (toIden t)

data QIden
  = QIden !Qual !Iden
  deriving (Show, Eq, Generic, Data)
instance NFData QIden
instance Cacheable QIden

instance ToSQL QIden where
  toSQL (QIden qual iden) =
    mconcat [toSQL qual, TB.char '.', toSQL iden]

newtype SQLOp
  = SQLOp {sqlOpTxt :: T.Text}
  deriving (Show, Eq, NFData, Data, Cacheable)

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

newtype TypeAnn
  = TypeAnn { unTypeAnn :: T.Text }
  deriving (Show, Eq, NFData, Data, Cacheable)

instance ToSQL TypeAnn where
  toSQL (TypeAnn ty) = "::" <> TB.text ty

mkTypeAnn :: PGType PGScalarType -> TypeAnn
mkTypeAnn = TypeAnn . toSQLTxt

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

data CountType
  = CTStar
  | CTSimple ![PGCol]
  | CTDistinct ![PGCol]
  deriving (Show, Eq, Generic, Data)
instance NFData CountType
instance Cacheable CountType

instance ToSQL CountType where
  toSQL CTStar            = "*"
  toSQL (CTSimple cols)   =
    paren $ ", " <+> cols
  toSQL (CTDistinct cols) =
    "DISTINCT" <-> paren (", " <+> cols)

newtype TupleExp
  = TupleExp [SQLExp]
  deriving (Show, Eq, NFData, Data, Cacheable)

instance ToSQL TupleExp where
  toSQL (TupleExp exps) =
    paren $ ", " <+> exps

data SQLExp
  = SEPrep !Int
  | SENull
  | SELit !T.Text
  | SEUnsafe !T.Text
  | SESelect !Select
  | SEStar !(Maybe Qual)
  -- ^ all fields (@*@) or all fields from relation (@iden.*@)
  | SEIden !Iden
  -- iden and row identifier are distinguished for easier rewrite rules
  | SERowIden !Iden
  | SEQIden !QIden
  | SEFnApp !T.Text ![SQLExp] !(Maybe OrderByExp)
  | SEOpApp !SQLOp ![SQLExp]
  | SETyAnn !SQLExp !TypeAnn
  | SECond !BoolExp !SQLExp !SQLExp
  | SEBool !BoolExp
  | SEExcluded !Iden
  | SEArray ![SQLExp]
  | SETuple !TupleExp
  | SECount !CountType
  | SENamedArg !Iden !SQLExp
  | SEFunction !FunctionExp
  deriving (Show, Eq, Generic, Data)
instance NFData SQLExp
instance Cacheable SQLExp

withTyAnn :: PGScalarType -> SQLExp -> SQLExp
withTyAnn colTy v = SETyAnn v . mkTypeAnn $ PGTypeScalar colTy

instance J.ToJSON SQLExp where
  toJSON = J.toJSON . toSQLTxt

newtype Alias
  = Alias { getAlias :: Iden }
  deriving (Show, Eq, NFData, Hashable, Data, Cacheable)

instance IsIden Alias where
  toIden (Alias iden) = iden

instance ToSQL Alias where
  toSQL (Alias iden) = "AS" <-> toSQL iden

toAlias :: (IsIden a) => a -> Alias
toAlias = Alias . toIden

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
    paren $ toSQL se
  toSQL (SEStar Nothing) =
    TB.char '*'
  toSQL (SEStar (Just qual)) =
    mconcat [paren (toSQL qual), TB.char '.', TB.char '*']
  toSQL (SEIden iden) =
    toSQL iden
  toSQL (SERowIden iden) =
    toSQL iden
  toSQL (SEQIden qIden) =
    toSQL qIden
  -- https://www.postgresql.org/docs/10/static/sql-expressions.html#SYNTAX-AGGREGATES
  toSQL (SEFnApp name args mObe) =
    TB.text name <> paren ((", " <+> args)  <-> toSQL mObe)
  toSQL (SEOpApp op args) =
     paren (sqlOpTxt op <+> args)
  toSQL (SETyAnn e ty) =
     paren (toSQL e) <> toSQL ty
  toSQL (SECond cond te fe) =
    "CASE WHEN" <-> toSQL cond <->
    "THEN" <-> toSQL te <->
    "ELSE" <-> toSQL fe <->
    "END"
  toSQL (SEBool be) = toSQL be
  toSQL (SEExcluded i) = "EXCLUDED."
                         <> toSQL i
  toSQL (SEArray exps) = "ARRAY" <> TB.char '['
                         <> (", " <+> exps) <> TB.char ']'
  toSQL (SETuple tup) = toSQL tup
  toSQL (SECount ty) = "COUNT" <> paren (toSQL ty)
  -- https://www.postgresql.org/docs/current/sql-syntax-calling-funcs.html
  toSQL (SENamedArg arg val) = toSQL arg <-> "=>" <-> toSQL val
  toSQL (SEFunction funcExp) = toSQL funcExp

intToSQLExp :: Int -> SQLExp
intToSQLExp =
  SEUnsafe . T.pack . show

data Extractor = Extractor !SQLExp !(Maybe Alias)
  deriving (Show, Eq, Generic, Data)
instance NFData Extractor
instance Cacheable Extractor

mkSQLOpExp
  :: SQLOp
  -> SQLExp -- lhs
  -> SQLExp -- rhs
  -> SQLExp -- result
mkSQLOpExp op lhs rhs = SEOpApp op [lhs, rhs]

mkColDefValMap :: [PGCol] -> HM.HashMap PGCol SQLExp
mkColDefValMap cols =
  HM.fromList $ zip cols (repeat $ SEUnsafe "DEFAULT")

handleIfNull :: SQLExp -> SQLExp -> SQLExp
handleIfNull l e = SEFnApp "coalesce" [e, l] Nothing

applyJsonBuildObj :: [SQLExp] -> SQLExp
applyJsonBuildObj args =
  SEFnApp "json_build_object" args Nothing

applyRowToJson :: [Extractor] -> SQLExp
applyRowToJson extrs =
  SEFnApp "row_to_json" [mkRowExp extrs] Nothing

getExtrAlias :: Extractor -> Maybe Alias
getExtrAlias (Extractor _ ma) = ma

mkAliasedExtr :: (IsIden a, IsIden b) => a -> Maybe b -> Extractor
mkAliasedExtr t = mkAliasedExtrFromExp (mkSIdenExp t)

mkAliasedExtrFromExp :: (IsIden a) => SQLExp -> Maybe a -> Extractor
mkAliasedExtrFromExp sqlExp ma = Extractor sqlExp (aliasF <$> ma)
  where
    aliasF = Alias . toIden

mkExtr :: (IsIden a) => a -> Extractor
mkExtr t = Extractor (mkSIdenExp t) Nothing

instance ToSQL Extractor where
  toSQL (Extractor ce mal) =
    toSQL ce <-> toSQL mal

data DistinctExpr
  = DistinctSimple
  | DistinctOn ![SQLExp]
  deriving (Show, Eq, Generic, Data)
instance NFData DistinctExpr
instance Cacheable DistinctExpr

instance ToSQL DistinctExpr where
  toSQL DistinctSimple    = "DISTINCT"
  toSQL (DistinctOn exps) =
    "DISTINCT ON" <-> paren ("," <+> exps)

data FunctionArgs
  = FunctionArgs
  { fasPostional :: ![SQLExp]
  , fasNamed     :: !(HM.HashMap Text SQLExp)
  } deriving (Show, Eq, Generic, Data)
instance NFData FunctionArgs
instance Cacheable FunctionArgs

instance ToSQL FunctionArgs where
  toSQL (FunctionArgs positionalArgs namedArgsMap) =
    let namedArgs = flip map (HM.toList namedArgsMap) $
                    \(argName, argVal) -> SENamedArg (Iden argName) argVal
    in paren $ ", " <+> (positionalArgs <> namedArgs)

data DefinitionListItem
  = DefinitionListItem
  { _dliColumn :: !PGCol
  , _dliType   :: !PGScalarType
  } deriving (Show, Eq, Data, Generic)
instance NFData DefinitionListItem
instance Cacheable DefinitionListItem

instance ToSQL DefinitionListItem where
  toSQL (DefinitionListItem column columnType) =
    toSQL column <-> toSQL columnType

data FunctionAlias
  = FunctionAlias
  { _faIden           :: !Alias
  , _faDefinitionList :: !(Maybe [DefinitionListItem])
  } deriving (Show, Eq, Data, Generic)
instance NFData FunctionAlias
instance Cacheable FunctionAlias

mkSimpleFunctionAlias :: Iden -> FunctionAlias
mkSimpleFunctionAlias identifier =
  FunctionAlias (toAlias identifier) Nothing

mkFunctionAlias :: Iden -> Maybe [(PGCol, PGScalarType)] -> FunctionAlias
mkFunctionAlias identifier listM =
  FunctionAlias (toAlias identifier) $
  fmap (map (uncurry DefinitionListItem)) listM

instance ToSQL FunctionAlias where
  toSQL (FunctionAlias iden (Just definitionList)) =
    toSQL iden <> paren ( ", " <+> definitionList)
  toSQL (FunctionAlias iden Nothing) =
    toSQL iden

data FunctionExp
  = FunctionExp
  { feName  :: !QualifiedFunction
  , feArgs  :: !FunctionArgs
  , feAlias :: !(Maybe FunctionAlias)
  } deriving (Show, Eq, Generic, Data)
instance NFData FunctionExp
instance Cacheable FunctionExp

instance ToSQL FunctionExp where
  toSQL (FunctionExp qf args alsM) =
    toSQL qf <> toSQL args <-> toSQL alsM

data FromItem
  = FISimple !QualifiedTable !(Maybe Alias)
  | FIIden !Iden
  | FIFunc !FunctionExp
  | FIUnnest ![SQLExp] !Alias ![SQLExp]
  | FISelect !Lateral !Select !Alias
  | FIValues !ValuesExp !Alias !(Maybe [PGCol])
  | FIJoin !JoinExpr
  deriving (Show, Eq, Generic, Data)
instance NFData FromItem
instance Cacheable FromItem

mkSelFromItem :: Select -> Alias -> FromItem
mkSelFromItem = FISelect (Lateral False)

mkLateralFromItem :: Select -> Alias -> FromItem
mkLateralFromItem = FISelect (Lateral True)

toColTupExp :: [PGCol] -> SQLExp
toColTupExp =
  SETuple . TupleExp . map (SEIden . Iden . getPGColTxt)

instance ToSQL FromItem where
  toSQL (FISimple qt mal) =
    toSQL qt <-> toSQL mal
  toSQL (FIIden iden) =
    toSQL iden
  toSQL (FIFunc funcExp) = toSQL funcExp
  -- unnest(expressions) alias(columns)
  toSQL (FIUnnest args als cols) =
    "UNNEST" <> paren (", " <+> args) <-> toSQL als <> paren (", " <+> cols)
  toSQL (FISelect mla sel al) =
    toSQL mla <-> paren (toSQL sel) <-> toSQL al
  toSQL (FIValues valsExp al mCols) =
    paren (toSQL valsExp) <-> toSQL al
    <-> toSQL (toColTupExp <$> mCols)
  toSQL (FIJoin je) =
    toSQL je

newtype Lateral = Lateral Bool
  deriving (Show, Eq, Data, NFData, Cacheable)

instance ToSQL Lateral where
  toSQL (Lateral True)  = "LATERAL"
  toSQL (Lateral False) = mempty

data JoinExpr
  = JoinExpr
  { tjeLeft  :: !FromItem
  , tjeType  :: !JoinType
  , tjeRight :: !FromItem
  , tjeJC    :: !JoinCond
  } deriving (Show, Eq, Generic, Data)
instance NFData JoinExpr
instance Cacheable JoinExpr

instance ToSQL JoinExpr where
  toSQL je =
    toSQL (tjeLeft je)
    <-> toSQL (tjeType je)
    <-> toSQL (tjeRight je)
    <-> toSQL (tjeJC je)

data JoinType
  = Inner
  | LeftOuter
  | RightOuter
  | FullOuter
  deriving (Eq, Show, Generic, Data)
instance NFData JoinType
instance Cacheable JoinType

instance ToSQL JoinType where
  toSQL Inner      = "INNER JOIN"
  toSQL LeftOuter  = "LEFT OUTER JOIN"
  toSQL RightOuter = "RIGHT OUTER JOIN"
  toSQL FullOuter  = "FULL OUTER JOIN"

data JoinCond
  = JoinOn !BoolExp
  | JoinUsing ![PGCol]
  deriving (Show, Eq, Generic, Data)
instance NFData JoinCond
instance Cacheable JoinCond

instance ToSQL JoinCond where
  toSQL (JoinOn be) =
    "ON" <-> paren (toSQL be)
  toSQL (JoinUsing cols) =
    "USING" <-> paren (","  <+> cols)

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

instance ToSQL BoolExp where
  toSQL (BELit True)  = TB.text $ T.squote "true"
  toSQL (BELit False) = TB.text $ T.squote "false"
  toSQL (BEBin bo bel ber) =
    paren (toSQL bel) <-> toSQL bo <-> paren (toSQL ber)
  toSQL (BENot be) =
    "NOT" <-> paren (toSQL be)
  toSQL (BECompare co vl vr) =
    paren (toSQL vl) <-> toSQL co <-> paren (toSQL vr)
  toSQL (BECompareAny co vl vr) =
    paren (toSQL vl) <-> toSQL co <-> "ANY" <> paren (toSQL vr)
  toSQL (BENull v) =
    paren (toSQL v) <-> "IS NULL"
  toSQL (BENotNull v) =
    paren (toSQL v) <-> "IS NOT NULL"
  toSQL (BEExists sel) =
    "EXISTS " <-> paren (toSQL sel)
  -- special case to handle lhs IN (exp1, exp2)
  toSQL (BEIN vl exps) =
    paren (toSQL vl) <-> toSQL SIN <-> paren (", " <+> exps)
  -- Any SQL expression which evaluates to bool value
  toSQL (BEExp e) = paren $ toSQL e

data BinOp = AndOp | OrOp
  deriving (Show, Eq, Generic, Data)
instance NFData BinOp
instance Cacheable BinOp

instance ToSQL BinOp where
  toSQL AndOp = "AND"
  toSQL OrOp  = "OR"

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
  toSQL = fromString . show

buildInsVal :: PGCol -> Int -> (PGCol, SQLExp)
buildInsVal colName argNumber =
  (colName, SEPrep argNumber)

data SQLDelete
  = SQLDelete
    { delTable :: !QualifiedTable
    , delUsing :: !(Maybe UsingExp)
    , delWhere :: !(Maybe WhereFrag)
    , delRet   :: !(Maybe RetExp)
    } deriving (Show, Eq)

data SQLUpdate
  = SQLUpdate
    { upTable :: !QualifiedTable
    , upSet   :: !SetExp
    , upFrom  :: !(Maybe FromExp)
    , upWhere :: !(Maybe WhereFrag)
    , upRet   :: !(Maybe RetExp)
    } deriving (Show, Eq)

newtype SetExp = SetExp [SetExpItem]
               deriving (Show, Eq)

newtype SetExpItem = SetExpItem (PGCol, SQLExp)
                   deriving (Show, Eq)

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
        (col, SEExcluded $ toIden col)


newtype UsingExp = UsingExp [TableName]
                  deriving (Show, Eq)

instance ToSQL UsingExp where
  toSQL (UsingExp tables)
    = "USING" <-> "," <+> tables

newtype RetExp = RetExp [Extractor]
                  deriving (Show, Eq)

selectStar :: Extractor
selectStar = Extractor (SEStar Nothing) Nothing

selectStar' :: Qual -> Extractor
selectStar' q = Extractor (SEStar (Just q)) Nothing

returningStar :: RetExp
returningStar = RetExp [selectStar]

instance ToSQL RetExp where
  toSQL (RetExp [])
    = mempty
  toSQL (RetExp exps)
    = "RETURNING" <-> (", " <+> exps)

instance ToSQL SQLDelete where
  toSQL sd = "DELETE FROM"
             <-> toSQL (delTable sd)
             <-> toSQL (delUsing sd)
             <-> toSQL (delWhere sd)
             <-> toSQL (delRet sd)

instance ToSQL SQLUpdate where
  toSQL a = "UPDATE"
            <-> toSQL (upTable a)
            <-> toSQL (upSet a)
            <-> toSQL (upFrom a)
            <-> toSQL (upWhere a)
            <-> toSQL (upRet a)

instance ToSQL SetExp where
  toSQL (SetExp cvs) =
    "SET" <-> ("," <+> cvs)

instance ToSQL SetExpItem where
  toSQL (SetExpItem (col, val)) =
    toSQL col <-> "=" <-> toSQL val


data SQLConflictTarget
  = SQLColumn ![PGCol]
  | SQLConstraint !ConstraintName
  deriving (Show, Eq)

instance ToSQL SQLConflictTarget where
  toSQL (SQLColumn cols)      = "("
                                <-> ("," <+> cols)
                                <-> ")"

  toSQL (SQLConstraint cons) = "ON CONSTRAINT" <-> toSQL cons

data SQLConflict
  = DoNothing !(Maybe SQLConflictTarget)
  | Update !SQLConflictTarget !SetExp !(Maybe WhereFrag)
  deriving (Show, Eq)

instance ToSQL SQLConflict where
  toSQL (DoNothing Nothing)   = "ON CONFLICT DO NOTHING"
  toSQL (DoNothing (Just ct)) = "ON CONFLICT"
                                <-> toSQL ct
                                <-> "DO NOTHING"
  toSQL (Update ct set whr)   = "ON CONFLICT"
                                <-> toSQL ct <-> "DO UPDATE"
                                <-> toSQL set <-> toSQL whr

newtype ValuesExp
  = ValuesExp [TupleExp]
  deriving (Show, Eq, Data, NFData, Cacheable)

instance ToSQL ValuesExp where
  toSQL (ValuesExp tuples) =
    "VALUES" <-> (", " <+> tuples)

data SQLInsert = SQLInsert
    { siTable    :: !QualifiedTable
    , siCols     :: ![PGCol]
    , siValues   :: !ValuesExp
    , siConflict :: !(Maybe SQLConflict)
    , siRet      :: !(Maybe RetExp)
    } deriving (Show, Eq)

instance ToSQL SQLInsert where
  toSQL si =
    "INSERT INTO"
    <-> toSQL (siTable si)
    <-> "("
    <-> (", " <+> siCols si)
    <-> ")"
    <-> toSQL (siValues si)
    <-> maybe "" toSQL (siConflict si)
    <-> toSQL (siRet si)

data CTE
  = CTESelect !Select
  | CTEInsert !SQLInsert
  | CTEUpdate !SQLUpdate
  | CTEDelete !SQLDelete
  deriving (Show, Eq)

instance ToSQL CTE where
  toSQL = \case
    CTESelect q -> toSQL q
    CTEInsert q -> toSQL q
    CTEUpdate q -> toSQL q
    CTEDelete q -> toSQL q

data SelectWith
  = SelectWith
  { swCTEs   :: [(Alias, CTE)]
  , swSelect :: !Select
  } deriving (Show, Eq)

instance ToSQL SelectWith where
  toSQL (SelectWith ctes sel) =
    "WITH " <> (", " <+> map f ctes) <-> toSQL sel
    where
      f (Alias al, q) = toSQL al <-> "AS" <-> paren (toSQL q)
