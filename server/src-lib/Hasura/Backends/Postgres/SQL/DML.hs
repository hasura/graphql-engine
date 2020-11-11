module Hasura.Backends.Postgres.SQL.DML where

import           Hasura.Prelude

import qualified Data.Aeson                         as J
import qualified Data.Aeson.Casing                  as J
import qualified Data.HashMap.Strict                as HM
import qualified Data.Text                          as T
import qualified Text.Builder                       as TB

import           Data.String                        (fromString)
import           Data.Text.Extended
import           Language.Haskell.TH.Syntax         (Lift)

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Incremental                 (Cacheable)
import           Hasura.SQL.Types


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

mkSelect :: Select
mkSelect = Select [] Nothing [] Nothing
           Nothing Nothing Nothing
           Nothing Nothing Nothing

newtype LimitExp
  = LimitExp SQLExp
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL LimitExp where
  toSQL (LimitExp se) =
    "LIMIT" <~> toSQL se

newtype OffsetExp
  = OffsetExp SQLExp
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL OffsetExp where
  toSQL (OffsetExp se) =
    "OFFSET" <~> toSQL se

newtype OrderByExp
  = OrderByExp (NonEmpty OrderByItem)
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

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
  toSQL (OrderByItem e ot no) =
    toSQL e <~> toSQL ot <~> toSQL no

data OrderType = OTAsc | OTDesc
  deriving (Show, Eq, Lift, Generic, Data)
instance NFData OrderType
instance Cacheable OrderType
instance Hashable OrderType

instance ToSQL OrderType where
  toSQL OTAsc  = "ASC"
  toSQL OTDesc = "DESC"

instance J.FromJSON OrderType where
  parseJSON = J.genericParseJSON $ J.defaultOptions{J.constructorTagModifier = J.snakeCase . drop 2}

instance J.ToJSON OrderType where
  toJSON = J.genericToJSON $ J.defaultOptions{J.constructorTagModifier = J.snakeCase . drop 2}

data NullsOrder
  = NFirst
  | NLast
  deriving (Show, Eq, Lift, Generic, Data)
instance NFData NullsOrder
instance Cacheable NullsOrder
instance Hashable NullsOrder

instance ToSQL NullsOrder where
  toSQL NFirst = "NULLS FIRST"
  toSQL NLast  = "NULLS LAST"

instance J.FromJSON NullsOrder where
  parseJSON = J.genericParseJSON $ J.defaultOptions{J.constructorTagModifier = J.snakeCase . drop 1}

instance J.ToJSON NullsOrder where
  toJSON = J.genericToJSON $ J.defaultOptions{J.constructorTagModifier = J.snakeCase . drop 1}

instance ToSQL OrderByExp where
  toSQL (OrderByExp l) =
    "ORDER BY" <~> (", " <+> toList l)

newtype GroupByExp
  = GroupByExp [SQLExp]
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL GroupByExp where
  toSQL (GroupByExp idens) =
    "GROUP BY" <~> (", " <+> idens)

newtype FromExp
  = FromExp [FromItem]
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL FromExp where
  toSQL (FromExp items) =
    "FROM" <~> (", " <+> items)

mkIdenFromExp :: (IsIdentifier a) => a -> FromExp
mkIdenFromExp a =
  FromExp [FIIdentifier $ toIdentifier a]

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

mkRowExp :: [Extractor] -> SQLExp
mkRowExp extrs = let
  innerSel = mkSelect { selExtr = extrs }

  innerSelName = TableName "e"

  -- SELECT r FROM (SELECT col1, col2, .. ) AS r
  outerSel = mkSelect
             { selExtr = [Extractor (SERowIdentifier $ toIdentifier innerSelName) Nothing]
             , selFrom = Just $ FromExp
                         [mkSelFromExp False innerSel innerSelName]
             }
  in
    SESelect outerSel

newtype HavingExp
  = HavingExp BoolExp
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL HavingExp where
  toSQL (HavingExp be) =
    "HAVING" <~> toSQL be

newtype WhereFrag
  = WhereFrag { getWFBoolExp :: BoolExp }
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL WhereFrag where
  toSQL (WhereFrag be) =
    "WHERE" <~> parenB (toSQL be)

instance ToSQL Select where
  toSQL sel = case selCTEs sel of
    [] -> "SELECT"
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
    ctes -> toSQL $ SelectWith (map (CTESelect <$>) ctes) sel { selCTEs = [] }


mkSIdenExp :: (IsIdentifier a) => a -> SQLExp
mkSIdenExp = SEIdentifier . toIdentifier

mkQIdenExp :: (IsIdentifier a, IsIdentifier b) => a -> b -> SQLExp
mkQIdenExp q t = SEQIdentifier $ mkQIdentifier q t

data Qual
  = QualifiedIdentifier !Identifier !(Maybe TypeAnn)
  | QualTable !QualifiedTable
  | QualVar !Text
  deriving (Show, Eq, Generic, Data)
instance NFData Qual
instance Cacheable Qual
instance Hashable Qual

mkQual :: QualifiedTable -> Qual
mkQual = QualTable

instance ToSQL Qual where
  toSQL (QualifiedIdentifier i tyM) = toSQL i <> toSQL tyM
  toSQL (QualTable qt)              = toSQL qt
  toSQL (QualVar v)                 = TB.text v

mkQIdentifier :: (IsIdentifier a, IsIdentifier b) => a -> b -> QIdentifier
mkQIdentifier q t = QIdentifier (QualifiedIdentifier (toIdentifier q) Nothing) (toIdentifier t)

mkQIdentifierTable :: (IsIdentifier a) => QualifiedTable -> a -> QIdentifier
mkQIdentifierTable q = QIdentifier (mkQual q) . toIdentifier

data QIdentifier
  = QIdentifier !Qual !Identifier
  deriving (Show, Eq, Generic, Data)
instance NFData QIdentifier
instance Cacheable QIdentifier
instance Hashable QIdentifier

instance ToSQL QIdentifier where
  toSQL (QIdentifier qual iden) =
    mconcat [toSQL qual, TB.char '.', toSQL iden]

newtype SQLOp
  = SQLOp {sqlOpTxt :: Text}
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

newtype TypeAnn
  = TypeAnn { unTypeAnn :: Text }
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

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
  toSQL CTStar            = "*"
  toSQL (CTSimple cols)   =
    parenB $ ", " <+> cols
  toSQL (CTDistinct cols) =
    "DISTINCT" <~> parenB (", " <+> cols)

newtype TupleExp
  = TupleExp [SQLExp]
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance ToSQL TupleExp where
  toSQL (TupleExp exps) =
    parenB $ ", " <+> exps

data SQLExp
  = SEPrep !Int
  | SENull
  | SELit !Text
  | SEUnsafe !Text
  | SESelect !Select
  | SEStar !(Maybe Qual)
  -- ^ all fields (@*@) or all fields from relation (@iden.*@)
  | SEIdentifier !Identifier
  -- iden and row identifier are distinguished for easier rewrite rules
  | SERowIdentifier !Identifier
  | SEQIdentifier !QIdentifier
  | SEFnApp !Text ![SQLExp] !(Maybe OrderByExp)
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

withTyAnn :: PGScalarType -> SQLExp -> SQLExp
withTyAnn colTy v = SETyAnn v . mkTypeAnn $ PGTypeScalar colTy

instance J.ToJSON SQLExp where
  toJSON = J.toJSON . toSQLTxt

newtype Alias
  = Alias { getAlias :: Identifier }
  deriving (Show, Eq, NFData, Data, Cacheable, Hashable)

instance IsIdentifier Alias where
  toIdentifier (Alias iden) = iden

instance ToSQL Alias where
  toSQL (Alias iden) = "AS" <~> toSQL iden

toAlias :: (IsIdentifier a) => a -> Alias
toAlias = Alias . toIdentifier

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
    mconcat [parenB (toSQL qual), TB.char '.', TB.char '*']
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
    "CASE WHEN" <~> toSQL cond <~>
    "THEN" <~> toSQL te <~>
    "ELSE" <~> toSQL fe <~>
    "END"
  toSQL (SEBool be) = toSQL be
  toSQL (SEExcluded i) = "EXCLUDED."
                         <> toSQL i
  toSQL (SEArray exps) = "ARRAY" <> TB.char '['
                         <> (", " <+> exps) <> TB.char ']'
  toSQL (SEArrayIndex arrayExp indexExp) =
    parenB (toSQL arrayExp)
    <> TB.char '[' <> toSQL indexExp <> TB.char ']'
  toSQL (SETuple tup) = toSQL tup
  toSQL (SECount ty) = "COUNT" <> parenB (toSQL ty)
  -- https://www.postgresql.org/docs/current/sql-syntax-calling-funcs.html
  toSQL (SENamedArg arg val) = toSQL arg <~> "=>" <~> toSQL val
  toSQL (SEFunction funcExp) = toSQL funcExp

intToSQLExp :: Int -> SQLExp
intToSQLExp =
  SEUnsafe . T.pack . show

data Extractor = Extractor !SQLExp !(Maybe Alias)
  deriving (Show, Eq, Generic, Data)
instance NFData Extractor
instance Cacheable Extractor
instance Hashable Extractor

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

instance ToSQL Extractor where
  toSQL (Extractor ce mal) =
    toSQL ce <~> toSQL mal

data DistinctExpr
  = DistinctSimple
  | DistinctOn ![SQLExp]
  deriving (Show, Eq, Generic, Data)
instance NFData DistinctExpr
instance Cacheable DistinctExpr
instance Hashable DistinctExpr

instance ToSQL DistinctExpr where
  toSQL DistinctSimple    = "DISTINCT"
  toSQL (DistinctOn exps) =
    "DISTINCT ON" <~> parenB ("," <+> exps)

data FunctionArgs
  = FunctionArgs
  { fasPostional :: ![SQLExp]
  , fasNamed     :: !(HM.HashMap Text SQLExp)
  } deriving (Show, Eq, Generic, Data)
instance NFData FunctionArgs
instance Cacheable FunctionArgs
instance Hashable FunctionArgs

instance ToSQL FunctionArgs where
  toSQL (FunctionArgs positionalArgs namedArgsMap) =
    let namedArgs = flip map (HM.toList namedArgsMap) $
                    \(argName, argVal) -> SENamedArg (Identifier argName) argVal
    in parenB $ ", " <+> (positionalArgs <> namedArgs)

data DefinitionListItem
  = DefinitionListItem
  { _dliColumn :: !PGCol
  , _dliType   :: !PGScalarType
  } deriving (Show, Eq, Data, Generic)
instance NFData DefinitionListItem
instance Cacheable DefinitionListItem
instance Hashable DefinitionListItem

instance ToSQL DefinitionListItem where
  toSQL (DefinitionListItem column columnType) =
    toSQL column <~> toSQL columnType

data FunctionAlias
  = FunctionAlias
  { _faIdentifier     :: !Alias
  , _faDefinitionList :: !(Maybe [DefinitionListItem])
  } deriving (Show, Eq, Data, Generic)
instance NFData FunctionAlias
instance Cacheable FunctionAlias
instance Hashable FunctionAlias

mkSimpleFunctionAlias :: Identifier -> FunctionAlias
mkSimpleFunctionAlias identifier =
  FunctionAlias (toAlias identifier) Nothing

mkFunctionAlias :: Identifier -> Maybe [(PGCol, PGScalarType)] -> FunctionAlias
mkFunctionAlias identifier listM =
  FunctionAlias (toAlias identifier) $
  fmap (map (uncurry DefinitionListItem)) listM

instance ToSQL FunctionAlias where
  toSQL (FunctionAlias iden (Just definitionList)) =
    toSQL iden <> parenB ( ", " <+> definitionList)
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
instance Hashable FunctionExp

instance ToSQL FunctionExp where
  toSQL (FunctionExp qf args alsM) =
    toSQL qf <> toSQL args <~> toSQL alsM

data FromItem
  = FISimple !QualifiedTable !(Maybe Alias)
  | FIIdentifier !Identifier
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

mkSelFromItem :: Select -> Alias -> FromItem
mkSelFromItem = FISelect (Lateral False)

mkSelectWithFromItem :: SelectWithG Select -> Alias -> FromItem
mkSelectWithFromItem = FISelectWith (Lateral False)

mkLateralFromItem :: Select -> Alias -> FromItem
mkLateralFromItem = FISelect (Lateral True)

toColTupExp :: [PGCol] -> SQLExp
toColTupExp =
  SETuple . TupleExp . map (SEIdentifier . Identifier . getPGColTxt)

instance ToSQL FromItem where
  toSQL (FISimple qt mal) =
    toSQL qt <~> toSQL mal
  toSQL (FIIdentifier iden) =
    toSQL iden
  toSQL (FIFunc funcExp) = toSQL funcExp
  -- unnest(expressions) alias(columns)
  toSQL (FIUnnest args als cols) =
    "UNNEST" <> parenB (", " <+> args) <~> toSQL als <> parenB (", " <+> cols)
  toSQL (FISelect mla sel al) =
    toSQL mla <~> parenB (toSQL sel) <~> toSQL al
  toSQL (FISelectWith mla selWith al) =
    toSQL mla <~> parenB (toSQL selWith) <~> toSQL al
  toSQL (FIValues valsExp al mCols) =
    parenB (toSQL valsExp) <~> toSQL al
    <~> toSQL (toColTupExp <$> mCols)
  toSQL (FIJoin je) =
    toSQL je

newtype Lateral = Lateral Bool
  deriving (Show, Eq, Data, NFData, Cacheable, Hashable)

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
instance Cacheable JoinType
instance Hashable JoinType

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
instance Hashable JoinCond

instance ToSQL JoinCond where
  toSQL (JoinOn be) =
    "ON" <~> parenB (toSQL be)
  toSQL (JoinUsing cols) =
    "USING" <~> parenB (","  <+> cols)

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

-- removes extraneous 'AND true's
simplifyBoolExp :: BoolExp -> BoolExp
simplifyBoolExp be = case be of
  BEBin AndOp e1 e2 ->
    let e1s = simplifyBoolExp e1
        e2s = simplifyBoolExp e2
    in if
      | e1s == BELit True -> e2s
      | e2s == BELit True -> e1s
      | otherwise         -> BEBin AndOp e1s e2s
  BEBin OrOp e1 e2 ->
    let e1s = simplifyBoolExp e1
        e2s = simplifyBoolExp e2
    in if
      | e1s == BELit False -> e2s
      | e2s == BELit False -> e1s
      | otherwise          -> BEBin OrOp e1s e2s
  e                          -> e

mkExists :: FromItem -> BoolExp -> BoolExp
mkExists fromItem whereFrag =
  BEExists mkSelect
  { selExtr  = [Extractor (SEUnsafe "1") Nothing]
  , selFrom  = Just $ FromExp $ pure fromItem
  , selWhere = Just $ WhereFrag whereFrag
  }

instance ToSQL BoolExp where
  toSQL (BELit True)  = TB.text "'true'"
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
instance Cacheable BinOp
instance Hashable BinOp

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
        (col, SEExcluded $ toIdentifier col)


newtype UsingExp = UsingExp [TableName]
                  deriving (Show, Eq)

instance ToSQL UsingExp where
  toSQL (UsingExp tables)
    = "USING" <~> "," <+> tables

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
    = "RETURNING" <~> (", " <+> exps)

instance ToSQL SQLDelete where
  toSQL sd = "DELETE FROM"
             <~> toSQL (delTable sd)
             <~> toSQL (delUsing sd)
             <~> toSQL (delWhere sd)
             <~> toSQL (delRet sd)

instance ToSQL SQLUpdate where
  toSQL a = "UPDATE"
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
  = SQLColumn ![PGCol]
  | SQLConstraint !ConstraintName
  deriving (Show, Eq)

instance ToSQL SQLConflictTarget where
  toSQL (SQLColumn cols)      = "("
                                <~> ("," <+> cols)
                                <~> ")"

  toSQL (SQLConstraint cons) = "ON CONSTRAINT" <~> toSQL cons

data SQLConflict
  = DoNothing !(Maybe SQLConflictTarget)
  | Update !SQLConflictTarget !SetExp !(Maybe WhereFrag)
  deriving (Show, Eq)

instance ToSQL SQLConflict where
  toSQL (DoNothing Nothing)   = "ON CONFLICT DO NOTHING"
  toSQL (DoNothing (Just ct)) = "ON CONFLICT"
                                <~> toSQL ct
                                <~> "DO NOTHING"
  toSQL (Update ct set whr)   = "ON CONFLICT"
                                <~> toSQL ct <~> "DO UPDATE"
                                <~> toSQL set <~> toSQL whr

newtype ValuesExp
  = ValuesExp [TupleExp]
  deriving (Show, Eq, Data, NFData, Cacheable, Hashable)

instance ToSQL ValuesExp where
  toSQL (ValuesExp tuples) =
    "VALUES" <~> (", " <+> tuples)

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
    <~> toSQL (siTable si)
    <~> "("
    <~> (", " <+> siCols si)
    <~> ")"
    <~> toSQL (siValues si)
    <~> maybe "" toSQL (siConflict si)
    <~> toSQL (siRet si)

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

data SelectWithG v
  = SelectWith
  { swCTEs   :: ![(Alias, v)]
  , swSelect :: !Select
  } deriving (Show, Eq, Generic, Data)

instance (NFData v) => NFData (SelectWithG v)
instance (Cacheable v) => Cacheable (SelectWithG v)
instance (Hashable v) => Hashable (SelectWithG v)

instance (ToSQL v) => ToSQL (SelectWithG v) where
  toSQL (SelectWith ctes sel) =
    "WITH " <> (", " <+> map f ctes) <~> toSQL sel
    where
      f (Alias al, q) = toSQL al <~> "AS" <~> parenB (toSQL q)

type SelectWith = SelectWithG CTE
