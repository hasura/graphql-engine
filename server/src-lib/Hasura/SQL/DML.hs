{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hasura.SQL.DML where

import           Hasura.Prelude
import           Hasura.SQL.Types

import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.ByteString.Builder    as BB
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Extended         as T

infixr 6 <->
(<->) :: BB.Builder -> BB.Builder -> BB.Builder
(<->) l r = l <> BB.char7 ' ' <> r
{-# INLINE (<->) #-}

paren :: BB.Builder -> BB.Builder
paren t = BB.char7 '(' <> t <> BB.char7 ')'
{-# INLINE paren #-}

data Select
  = Select
    { selDistinct :: !(Maybe DistinctExpr)
    , selExtr     :: ![Extractor]
    , selFrom     :: !(Maybe FromExp)
    , selWhere    :: !(Maybe WhereFrag)
    , selGroupBy  :: !(Maybe GroupByExp)
    , selHaving   :: !(Maybe HavingExp)
    , selOrderBy  :: !(Maybe OrderByExp)
    , selLimit    :: !(Maybe LimitExp)
    , selOffset   :: !(Maybe OffsetExp)
    } deriving (Show, Eq)

mkSelect :: Select
mkSelect = Select Nothing [] Nothing
           Nothing Nothing Nothing
           Nothing Nothing Nothing

newtype LimitExp
  = LimitExp SQLExp
  deriving (Show, Eq)

instance ToSQL LimitExp where
  toSQL (LimitExp se) =
    BB.string7 "LIMIT" <-> toSQL se

newtype OffsetExp
  = OffsetExp SQLExp
  deriving (Show, Eq)

instance ToSQL OffsetExp where
  toSQL (OffsetExp se) =
    BB.string7 "OFFSET" <-> toSQL se

newtype OrderByExp
  = OrderByExp [OrderByItem]
  deriving (Show, Eq)

data OrderByItem
  = OrderByItem
    { oColumn :: !SQLExp
    , oType   :: !(Maybe OrderType)
    , oNulls  :: !(Maybe NullsOrder)
    } deriving (Show, Eq)

instance ToSQL OrderByItem where
  toSQL (OrderByItem e ot no) =
    toSQL e <-> toSQL ot <-> toSQL no

data OrderType = OTAsc
               | OTDesc
               deriving (Show, Eq, Lift)

instance ToSQL OrderType where
  toSQL OTAsc  = BB.string7 "ASC"
  toSQL OTDesc = BB.string7 "DESC"

data NullsOrder
  = NFirst
  | NLast
  deriving (Show, Eq, Lift)

instance ToSQL NullsOrder where
  toSQL NFirst = BB.string7 "NULLS FIRST"
  toSQL NLast  = BB.string7 "NULLS LAST"

instance ToSQL OrderByExp where
  toSQL (OrderByExp l) =
    BB.string7 "ORDER BY" <-> (", " <+> l)

newtype GroupByExp
  = GroupByExp [SQLExp]
  deriving (Show, Eq)

instance ToSQL GroupByExp where
  toSQL (GroupByExp idens) =
    BB.string7 "GROUP BY" <-> (", " <+> idens)

newtype FromExp
  = FromExp [FromItem]
  deriving (Show, Eq)

instance ToSQL FromExp where
  toSQL (FromExp items) =
    BB.string7 "FROM" <-> (", " <+> items)

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
  deriving (Show, Eq)

instance ToSQL HavingExp where
  toSQL (HavingExp be) =
    BB.string7 "HAVING" <-> toSQL be

newtype WhereFrag
  = WhereFrag { getWFBoolExp :: BoolExp }
  deriving (Show, Eq)

instance ToSQL WhereFrag where
  toSQL (WhereFrag be) =
    BB.string7 "WHERE" <-> paren (toSQL be)

instance ToSQL Select where
  toSQL sel =
    BB.string7 "SELECT"
    <-> toSQL (selDistinct sel)
    <-> (", " <+> selExtr sel)
    <-> toSQL (selFrom sel)
    <-> toSQL (selWhere sel)
    <-> toSQL (selGroupBy sel)
    <-> toSQL (selHaving sel)
    <-> toSQL (selOrderBy sel)
    <-> toSQL (selLimit sel)
    <-> toSQL (selOffset sel)

mkSIdenExp :: (IsIden a) => a -> SQLExp
mkSIdenExp = SEIden . toIden

mkQIdenExp :: (IsIden a, IsIden b) => a -> b -> SQLExp
mkQIdenExp q t = SEQIden $ mkQIden q t

data Qual
  = QualIden !Iden
  | QualTable !QualifiedTable
  | QualVar !T.Text
  deriving (Show, Eq)

mkQual :: QualifiedTable -> Qual
mkQual = QualTable

instance ToSQL Qual where
  toSQL (QualIden i) = toSQL i
  toSQL (QualTable qt) = toSQL qt
  toSQL (QualVar v)  =
    TE.encodeUtf8Builder v

mkQIden :: (IsIden a, IsIden b) => a -> b -> QIden
mkQIden q t = QIden (QualIden (toIden q)) (toIden t)

data QIden
  = QIden !Qual !Iden
  deriving (Show, Eq)

instance ToSQL QIden where
  toSQL (QIden qual iden) =
    mconcat [toSQL qual, BB.char7 '.', toSQL iden]

newtype SQLOp
  = SQLOp {sqlOpTxt :: T.Text}
  deriving (Show, Eq)

incOp :: SQLOp
incOp = SQLOp "+"

mulOp :: SQLOp
mulOp = SQLOp "*"

jsonbConcatOp :: SQLOp
jsonbConcatOp = SQLOp "||"

jsonbDeleteOp :: SQLOp
jsonbDeleteOp = SQLOp "-"

jsonbDeleteAtPathOp :: SQLOp
jsonbDeleteAtPathOp = SQLOp "#-"

newtype AnnType
  = AnnType {unAnnType :: T.Text}
  deriving (Show, Eq)

intType :: AnnType
intType = AnnType "int"

textType :: AnnType
textType = AnnType "text"

textArrType :: AnnType
textArrType = AnnType "text[]"

jsonType :: AnnType
jsonType = AnnType "json"

jsonbType :: AnnType
jsonbType = AnnType "jsonb"

data SQLExp
  = SEPrep !Int
  | SELit !T.Text
  | SEUnsafe !T.Text
  | SESelect !Select
  | SEStar
  | SEIden !Iden
  -- iden and row identifier are distinguished for easier rewrite rules
  | SERowIden !Iden
  | SEQIden !QIden
  | SEFnApp !T.Text ![SQLExp] !(Maybe OrderByExp)
  | SEOpApp !SQLOp ![SQLExp]
  | SETyAnn !SQLExp !AnnType
  | SECond !BoolExp !SQLExp !SQLExp
  | SEBool !BoolExp
  | SEExcluded !T.Text
  | SEArray ![SQLExp]
  deriving (Show, Eq)

newtype Alias
  = Alias { getAlias :: Iden }
  deriving (Show, Eq, Hashable)

instance IsIden Alias where
  toIden (Alias iden) = iden

instance ToSQL Alias where
  toSQL (Alias iden) = "AS" <-> toSQL iden

toAlias :: (IsIden a) => a -> Alias
toAlias = Alias . toIden

instance ToSQL SQLExp where
  toSQL (SEPrep argNumber) =
    BB.char7 '$' <> BB.intDec argNumber
  toSQL (SELit tv) =
    TE.encodeUtf8Builder $ pgFmtLit tv
  toSQL (SEUnsafe t) =
    TE.encodeUtf8Builder t
  toSQL (SESelect se) =
    paren $ toSQL se
  toSQL SEStar =
    BB.char7 '*'
  toSQL (SEIden iden) =
    toSQL iden
  toSQL (SERowIden iden) =
    toSQL iden
  toSQL (SEQIden qIden) =
    toSQL qIden
  -- https://www.postgresql.org/docs/10/static/sql-expressions.html#SYNTAX-AGGREGATES
  toSQL (SEFnApp name args mObe) =
    TE.encodeUtf8Builder name <> paren ((", " <+> args)  <-> toSQL mObe)
  toSQL (SEOpApp op args) =
     paren (sqlOpTxt op <+> args)
  toSQL (SETyAnn e ty) =
     paren (toSQL e) <> BB.string7 "::"
     <> TE.encodeUtf8Builder (unAnnType ty)
  toSQL (SECond cond te fe) =
    BB.string7 "CASE WHEN" <-> toSQL cond <->
    BB.string7 "THEN" <-> toSQL te <->
    BB.string7 "ELSE" <-> toSQL fe <->
    BB.string7 "END"
  toSQL (SEBool be) = toSQL be
  toSQL (SEExcluded t) = BB.string7 "EXCLUDED."
                         <> toSQL (PGCol t)
  toSQL (SEArray exps) = BB.string7 "ARRAY" <> BB.char7 '['
                         <> (", " <+> exps) <> BB.char7 ']'

intToSQLExp :: Int -> SQLExp
intToSQLExp =
  SEUnsafe . T.pack . show

data Extractor = Extractor !SQLExp !(Maybe Alias)
               deriving (Show, Eq)

mkSQLOpExp
  :: SQLOp
  -> SQLExp -- lhs
  -> SQLExp -- rhs
  -> SQLExp -- result
mkSQLOpExp op lhs rhs = SEOpApp op [lhs, rhs]

handleIfNull :: SQLExp -> SQLExp -> SQLExp
handleIfNull l e = SEFnApp "coalesce" [e, l] Nothing

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
  deriving (Show, Eq)

instance ToSQL DistinctExpr where
  toSQL DistinctSimple    = BB.string7 "DISTINCT"
  toSQL (DistinctOn exps) =
    BB.string7 "DISTINCT ON" <-> paren ("," <+> exps)

data FromItem
  = FISimple !QualifiedTable !(Maybe Alias)
  | FIIden !Iden
  | FISelect !Lateral !Select !Alias
  | FIJoin !JoinExpr
  deriving (Show, Eq)

mkSelFromItem :: Select -> Alias -> FromItem
mkSelFromItem = FISelect (Lateral False)

mkLateralFromItem :: Select -> Alias -> FromItem
mkLateralFromItem = FISelect (Lateral True)

instance ToSQL FromItem where
  toSQL (FISimple qt mal) =
    toSQL qt <-> toSQL mal
  toSQL (FIIden iden) =
    toSQL iden
  toSQL (FISelect mla sel al) =
    toSQL mla <-> paren (toSQL sel) <-> toSQL al
  toSQL (FIJoin je) =
    toSQL je

newtype Lateral = Lateral Bool
             deriving (Show, Eq)

instance ToSQL Lateral where
  toSQL (Lateral True)  = BB.string7 "LATERAL"
  toSQL (Lateral False) = mempty

data JoinExpr
  = JoinExpr
  { tjeLeft  :: !FromItem
  , tjeType  :: !JoinType
  , tjeRight :: !FromItem
  , tjeJC    :: !JoinCond
  } deriving (Show, Eq)

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
  deriving (Eq, Show)

instance ToSQL JoinType where
  toSQL Inner      = BB.string7 "INNER JOIN"
  toSQL LeftOuter  = BB.string7 "LEFT OUTER JOIN"
  toSQL RightOuter = BB.string7 "RIGHT OUTER JOIN"
  toSQL FullOuter  = BB.string7 "FULL OUTER JOIN"

data JoinCond
  = JoinOn !BoolExp
  | JoinUsing ![PGCol]
  deriving (Show, Eq)

instance ToSQL JoinCond where
  toSQL (JoinOn be) =
    BB.string7 "ON" <-> paren (toSQL be)
  toSQL (JoinUsing cols) =
    BB.string7 "USING" <-> paren (","  <+> cols)

data BoolExp
  = BELit !Bool
  | BEBin !BinOp !BoolExp !BoolExp
  | BENot !BoolExp
  | BECompare !CompareOp !SQLExp !SQLExp
  | BENull !SQLExp
  | BENotNull !SQLExp
  | BEExists !Select
  deriving (Show, Eq)

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

mkExists :: QualifiedTable -> BoolExp -> BoolExp
mkExists qt whereFrag =
  BEExists mkSelect {
    selExtr  = [Extractor (SEUnsafe "1") Nothing],
    selFrom  = Just $ mkSimpleFromExp qt,
    selWhere = Just $ WhereFrag whereFrag
  }

instance ToSQL BoolExp where
  toSQL (BELit True)  = TE.encodeUtf8Builder $ T.squote "true"
  toSQL (BELit False) = TE.encodeUtf8Builder $ T.squote "false"
  toSQL (BEBin bo bel ber) =
    paren (toSQL bel) <-> toSQL bo <-> paren (toSQL ber)
  toSQL (BENot be) =
    BB.string7 "NOT" <-> paren (toSQL be)
  toSQL (BECompare co vl vr) =
    paren (toSQL vl) <-> toSQL co <-> paren (toSQL vr)
  toSQL (BENull v) =
    paren (toSQL v) <-> BB.string7 "IS NULL"
  toSQL (BENotNull v) =
    paren (toSQL v) <-> BB.string7 "IS NOT NULL"
  toSQL (BEExists sel) =
    BB.string7 "EXISTS " <-> paren (toSQL sel)

data BinOp = AndOp
           | OrOp
           deriving (Show, Eq)

instance ToSQL BinOp where
  toSQL AndOp = BB.string7 "AND"
  toSQL OrOp  = BB.string7 "OR"

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
  deriving (Eq)

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
  toSQL = BB.string7 . show

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

buildSEWithExcluded :: [PGCol] -> SetExp
buildSEWithExcluded cols = SetExp $ flip map cols $
  \col -> SetExpItem (col, SEExcluded $ getPGColTxt col)

newtype UsingExp = UsingExp [TableName]
                  deriving (Show, Eq)

instance ToSQL UsingExp where
  toSQL (UsingExp tables)
    = BB.string7 "USING" <-> "," <+> tables

newtype RetExp = RetExp [Extractor]
                  deriving (Show, Eq)

selectStar :: Extractor
selectStar = Extractor SEStar Nothing

returningStar :: RetExp
returningStar = RetExp [selectStar]

instance ToSQL RetExp where
  toSQL (RetExp [])
    = mempty
  toSQL (RetExp exps)
    = BB.string7 "RETURNING" <-> (", " <+> exps)

instance ToSQL SQLDelete where
  toSQL sd = BB.string7 "DELETE FROM"
             <-> toSQL (delTable sd)
             <-> toSQL (delUsing sd)
             <-> toSQL (delWhere sd)
             <-> toSQL (delRet sd)

instance ToSQL SQLUpdate where
  toSQL a = BB.string7 "UPDATE"
            <-> toSQL (upTable a)
            <-> toSQL (upSet a)
            <-> toSQL (upFrom a)
            <-> toSQL (upWhere a)
            <-> toSQL (upRet a)

instance ToSQL SetExp where
  toSQL (SetExp cvs) =
    BB.string7 "SET" <-> ("," <+> cvs)

instance ToSQL SetExpItem where
  toSQL (SetExpItem (col, val)) =
    toSQL col <-> "=" <-> toSQL val


data SQLConflictTarget
  = SQLColumn ![PGCol]
  | SQLConstraint !ConstraintName
  deriving (Show, Eq)

instance ToSQL SQLConflictTarget where
  toSQL (SQLColumn cols)      = BB.string7 "("
                                <-> ("," <+> cols)
                                <-> BB.string7 ")"

  toSQL (SQLConstraint cons) = BB.string7 "ON CONSTRAINT" <-> toSQL cons

data SQLConflict
  = DoNothing !(Maybe SQLConflictTarget)
  | Update !SQLConflictTarget !SetExp
  deriving (Show, Eq)

instance ToSQL SQLConflict where
  toSQL (DoNothing Nothing)   = BB.string7 "ON CONFLICT DO NOTHING"
  toSQL (DoNothing (Just ct)) = BB.string7 "ON CONFLICT"
                                <-> toSQL ct
                                <-> BB.string7 "DO NOTHING"
  toSQL (Update ct ex)        = BB.string7 "ON CONFLICT"
                                <-> toSQL ct <-> "DO UPDATE"
                                <-> toSQL ex

data SQLInsert = SQLInsert
    { siTable    :: !QualifiedTable
    , siCols     :: ![PGCol]
    , siTuples   :: ![[SQLExp]]
    , siConflict :: !(Maybe SQLConflict)
    , siRet      :: !(Maybe RetExp)
    } deriving (Show, Eq)

instance ToSQL SQLInsert where
  toSQL si =
    let insTuples   = flip map (siTuples si) $ \tupVals ->
          BB.string7 "(" <-> (", " <+> tupVals) <-> BB.string7 ")"
        insConflict = maybe (BB.string7 "") toSQL
    in "INSERT INTO"
       <-> toSQL (siTable si)
       <-> BB.string7 "("
       <-> (", " <+> siCols si)
       <-> BB.string7 ") VALUES"
       <-> (", " <+> insTuples)
       <-> insConflict (siConflict si)
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
