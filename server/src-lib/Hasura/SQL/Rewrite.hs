module Hasura.SQL.Rewrite
  ( prefixNumToAliases
  ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
import           Hasura.Prelude
import qualified Hasura.SQL.DML      as S
import           Hasura.SQL.Types    (Iden (..))


-- add an int as a prefix to all aliases.
-- This is needed in cases identifiers exceed 63 chars
-- as postgres only considers first 63 chars of
-- an identifier
prefixNumToAliases :: S.Select -> S.Select
prefixNumToAliases s =
  uSelect s `evalState` UniqSt 0 Map.empty

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

uSelect :: S.Select -> Uniq S.Select
uSelect sel = do
  -- this has to be the first thing to process
  newFromM  <- mapM uFromExp fromM

  newWhereM <- forM whereM $
               \(S.WhereFrag be) -> S.WhereFrag <$> uBoolExp be
  newGrpM   <- forM grpM $
               \(S.GroupByExp l) -> S.GroupByExp <$> mapM uSqlExp l
  newHavnM  <- forM havnM $
               \(S.HavingExp be) -> S.HavingExp <$> uBoolExp be
  newOrdM   <- mapM uOrderBy ordByM
  newDistM  <- mapM uDistinct distM
  newExtrs  <- mapM uExtractor extrs
  return $ S.Select newDistM newExtrs newFromM newWhereM newGrpM
    newHavnM newOrdM limitM offM
  where
    S.Select distM extrs fromM whereM grpM havnM ordByM limitM offM = sel
    uDistinct = \case
      S.DistinctSimple -> return S.DistinctSimple
      S.DistinctOn l   -> S.DistinctOn <$> mapM uSqlExp l
    uExtractor (S.Extractor e alM) =
      S.Extractor <$> uSqlExp e <*> return alM

uFromExp :: S.FromExp -> Uniq S.FromExp
uFromExp (S.FromExp fromItems) =
  S.FromExp <$> mapM uFromItem fromItems

uFromItem :: S.FromItem -> Uniq S.FromItem
uFromItem fromItem = case fromItem of
  S.FISimple t alM ->
    S.FISimple t <$> mapM addAlias alM
  S.FIIden iden ->
    S.FIIden <$> return iden
  S.FIFunc f args alM ->
    S.FIFunc f args <$> mapM addAlias alM
  S.FIUnnest args als cols ->
    S.FIUnnest <$> mapM uSqlExp args <*> addAlias als <*> mapM uSqlExp cols
  S.FISelect isLateral sel al -> do
    -- we are kind of ignoring if we have to reset
    -- idens to empty based on correlation
    -- unless isLateral $ modify' $ \s -> s { _uqIdens = Map.empty}
    newSel <- restoringIdens $ uSelect sel
    newAls <- addAlias al
    return $ S.FISelect isLateral newSel newAls
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
  S.SELit t                     -> return $ S.SELit t
  S.SEUnsafe t                  -> return $ S.SEUnsafe t
  S.SESelect s                  -> S.SESelect <$> uSelect s
  S.SEStar                      -> return S.SEStar
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
  S.SETuple (S.TupleExp l)     ->
    S.SEArray <$> mapM uSqlExp l
  S.SECount cty                 -> return $ S.SECount cty
  where
    uQual = \case
      S.QualIden iden -> S.QualIden <$> getIden iden
      S.QualTable t   -> return $ S.QualTable t
      S.QualVar t     -> return $ S.QualVar t
