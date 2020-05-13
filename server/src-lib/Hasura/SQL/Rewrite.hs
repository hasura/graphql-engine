module Hasura.SQL.Rewrite
  ( prefixNumToAliases
  , prefixNumToAliasesSelectWith
  ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
import           Hasura.Prelude
import qualified Hasura.SQL.DML      as S
import           Hasura.SQL.Types    (Iden (..))

{- Note [Postgres identifier length limitations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Postgres truncates identifiers to a maximum of 63 characters by default (see
https://www.postgresql.org/docs/12/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS).
-}

-- Prefix an Int to all aliases to preserve the uniqueness of identifiers.
-- See Note [Postgres identifier length limitations].
prefixNumToAliases :: S.Select -> S.Select
prefixNumToAliases s =
  uSelect s `evalState` UniqSt 0 Map.empty

prefixNumToAliasesSelectWith
  :: S.SelectWithG S.Select -> S.SelectWithG S.Select
prefixNumToAliasesSelectWith s =
  uSelectWith s `evalState` UniqSt 0 Map.empty

type Rewrite a = State a

data UniqSt
  = UniqSt
  { _uqVar   :: !Int
  , _uqIdens :: !(Map.HashMap Iden Int)
  } deriving (Show, Eq)

type Uniq = Rewrite UniqSt

withNumPfx :: Iden -> Int -> Iden
withNumPfx iden i =
  Iden pfx <> iden
  where
    pfx = T.pack $ "_" <> show i <> "_"

addAlias :: S.Alias -> Uniq S.Alias
addAlias (S.Alias iden) = do
  UniqSt var idens <- get
  put $ UniqSt (var + 1) $ Map.insert iden var idens
  return $ S.Alias $ withNumPfx iden var

getIden :: Iden -> Uniq Iden
getIden iden = do
  UniqSt _ idens <- get
  let varNumM = Map.lookup iden idens
  return $ maybe iden (withNumPfx iden) varNumM

restoringIdens :: Uniq a -> Uniq a
restoringIdens action = do
  UniqSt _ idens <- get
  res <- action
  -- restore the idens to before the action
  modify' $ \s -> s { _uqIdens = idens }
  return res

uSelectWith :: S.SelectWithG S.Select -> Uniq (S.SelectWithG S.Select)
uSelectWith (S.SelectWith ctes baseSelect) =
  S.SelectWith
    <$> forM ctes (\(als, sel) -> (als,) <$> restoringIdens (uSelect sel))
    <*> uSelect baseSelect

uSelect :: S.Select -> Uniq S.Select
uSelect sel = do
  -- this has to be the first thing to process
  newFromM  <- mapM uFromExp fromM
  newCTEs <- for ctes $ \(alias, cte) -> (,) <$> addAlias alias <*> uSelect cte

  newWhereM <- forM whereM $
               \(S.WhereFrag be) -> S.WhereFrag <$> uBoolExp be
  newGrpM   <- forM grpM $
               \(S.GroupByExp l) -> S.GroupByExp <$> mapM uSqlExp l
  newHavnM  <- forM havnM $
               \(S.HavingExp be) -> S.HavingExp <$> uBoolExp be
  newOrdM   <- mapM uOrderBy ordByM
  newDistM  <- mapM uDistinct distM
  newExtrs  <- mapM uExtractor extrs
  return $ S.Select newCTEs newDistM newExtrs newFromM newWhereM newGrpM
    newHavnM newOrdM limitM offM
  where
    S.Select ctes distM extrs fromM whereM grpM havnM ordByM limitM offM = sel
    uDistinct = \case
      S.DistinctSimple -> return S.DistinctSimple
      S.DistinctOn l   -> S.DistinctOn <$> mapM uSqlExp l
    uExtractor (S.Extractor e alM) =
      S.Extractor <$> uSqlExp e <*> return alM

uFromExp :: S.FromExp -> Uniq S.FromExp
uFromExp (S.FromExp fromItems) =
  S.FromExp <$> mapM uFromItem fromItems

uFunctionArgs :: S.FunctionArgs -> Uniq S.FunctionArgs
uFunctionArgs (S.FunctionArgs positional named) =
  S.FunctionArgs <$> mapM uSqlExp positional <*> mapM uSqlExp named

uFunctionAlias :: S.FunctionAlias -> Uniq S.FunctionAlias
uFunctionAlias (S.FunctionAlias alias definitionList) =
  S.FunctionAlias <$> addAlias alias <*> pure definitionList

uFunctionExp :: S.FunctionExp -> Uniq S.FunctionExp
uFunctionExp (S.FunctionExp qf args alM) =
  S.FunctionExp qf <$> uFunctionArgs args <*> mapM uFunctionAlias alM

uFromItem :: S.FromItem -> Uniq S.FromItem
uFromItem fromItem = case fromItem of
  S.FISimple t alM ->
    S.FISimple t <$> mapM addAlias alM
  S.FIIden iden ->
    S.FIIden <$> return iden
  S.FIFunc funcExp ->
    S.FIFunc <$> uFunctionExp funcExp
  S.FIUnnest args als cols ->
    S.FIUnnest <$> mapM uSqlExp args <*> addAlias als <*> mapM uSqlExp cols
  S.FISelect isLateral sel al -> do
    -- we are kind of ignoring if we have to reset
    -- idens to empty based on correlation
    -- unless isLateral $ modify' $ \s -> s { _uqIdens = Map.empty}
    newSel <- restoringIdens $ uSelect sel
    newAls <- addAlias al
    return $ S.FISelect isLateral newSel newAls
  S.FISelectWith isLateral selectWith al -> do
    newSelectWith <- uSelectWith selectWith
    newAls <- addAlias al
    return $ S.FISelectWith isLateral newSelectWith newAls
  S.FIValues (S.ValuesExp tups) als mCols -> do
    newValExp <- fmap S.ValuesExp $
                 forM tups $ \(S.TupleExp ts) ->
                               S.TupleExp <$> mapM uSqlExp ts
    return $ S.FIValues newValExp als mCols
  S.FIJoin joinExp ->
    S.FIJoin <$> uJoinExp joinExp

uJoinExp :: S.JoinExpr -> Uniq S.JoinExpr
uJoinExp (S.JoinExpr left ty right joinCond) = do
  leftN  <- uFromItem left
  rightN <- uFromItem right
  S.JoinExpr leftN ty rightN <$> uJoinCond joinCond

uJoinCond :: S.JoinCond -> Uniq S.JoinCond
uJoinCond joinCond = case joinCond of
  S.JoinOn be      -> S.JoinOn <$> uBoolExp be
  S.JoinUsing cols -> return $ S.JoinUsing cols

uBoolExp :: S.BoolExp -> Uniq S.BoolExp
uBoolExp = restoringIdens . \case
  S.BELit b -> return $ S.BELit b
  S.BEBin op left right ->
    S.BEBin <$> return op <*> uBoolExp left <*> uBoolExp right
  S.BENot b -> S.BENot <$> uBoolExp b
  S.BECompare op left right ->
    S.BECompare <$> return op <*> uSqlExp left <*> uSqlExp right
  S.BECompareAny op left right ->
    S.BECompareAny <$> return op <*> uSqlExp left <*> uSqlExp right
  S.BENull e -> S.BENull <$> uSqlExp e
  S.BENotNull e -> S.BENotNull <$> uSqlExp e
  S.BEExists sel -> S.BEExists <$> uSelect sel
  S.BEIN left exps -> S.BEIN <$> uSqlExp left <*> mapM uSqlExp exps
  S.BEExp e -> S.BEExp <$> uSqlExp e

uOrderBy :: S.OrderByExp -> Uniq S.OrderByExp
uOrderBy (S.OrderByExp ordByItems) =
  S.OrderByExp <$> mapM uOrderByItem ordByItems
  where
    uOrderByItem (S.OrderByItem e ordTyM nullsOrdM) =
      S.OrderByItem
      <$> uSqlExp e
      <*> return ordTyM
      <*> return nullsOrdM

uSqlExp :: S.SQLExp -> Uniq S.SQLExp
uSqlExp = restoringIdens . \case
  S.SEPrep i                    -> return $ S.SEPrep i
  S.SENull                      -> return S.SENull
  S.SELit t                     -> return $ S.SELit t
  S.SEUnsafe t                  -> return $ S.SEUnsafe t
  S.SESelect s                  -> S.SESelect <$> uSelect s
  S.SEStar qual                 -> S.SEStar <$> traverse uQual qual
  -- this is for row expressions
  -- todo: check if this is always okay
  S.SEIden iden                 -> return $ S.SEIden iden
  S.SERowIden iden              -> S.SERowIden <$> getIden iden
  S.SEQIden (S.QIden qual iden) -> do
    newQual <- uQual qual
    return $ S.SEQIden $ S.QIden newQual iden
  S.SEFnApp fn args ordByM      ->
    S.SEFnApp
    <$> return fn
    <*> mapM uSqlExp args
    <*> mapM uOrderBy ordByM
  S.SEOpApp op args             ->
    S.SEOpApp op
    <$> mapM uSqlExp args
  S.SETyAnn e ty                ->
    S.SETyAnn
    <$> uSqlExp e
    <*> return ty
  S.SECond be onTrue onFalse    ->
    S.SECond
    <$> uBoolExp be
    <*> uSqlExp onTrue
    <*> uSqlExp onFalse
  S.SEBool be                   ->
    S.SEBool <$> uBoolExp be
  S.SEExcluded t                ->
    S.SEExcluded <$> return t
  S.SEArray l                   ->
    S.SEArray <$> mapM uSqlExp l
  S.SEArrayIndex arrayExp indexExp ->
    S.SEArrayIndex <$> uSqlExp arrayExp <*> uSqlExp indexExp
  S.SETuple (S.TupleExp l)      ->
    S.SETuple . S.TupleExp <$> mapM uSqlExp l
  S.SECount cty                 -> return $ S.SECount cty
  S.SENamedArg arg val          -> S.SENamedArg arg <$> uSqlExp val
  S.SEFunction funcExp          -> S.SEFunction <$> uFunctionExp funcExp
  where
    uQual = \case
      S.QualIden iden ty -> S.QualIden <$> getIden iden <*> pure ty
      S.QualTable t   -> return $ S.QualTable t
      S.QualVar t     -> return $ S.QualVar t
