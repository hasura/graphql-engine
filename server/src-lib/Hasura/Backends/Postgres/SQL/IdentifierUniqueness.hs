-- | Postgres SQL IdentifierUniqueness
--
-- Bypass the Postgres limitation of truncating identifiers to 63 characters long
-- by prepending unique numbers.
--
-- See Postgres docs:
-- <https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS>
--
-- Also see Note [Postgres identifier length limitations]
module Hasura.Backends.Postgres.SQL.IdentifierUniqueness
  ( -- * Exported API
    prefixNumToAliases,
    prefixNumToAliasesSelectWith,
  )
where

import Data.HashMap.Strict qualified as Map
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types (Identifier (..))
import Hasura.Prelude

{- Note [Postgres identifier length limitations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Postgres truncates identifiers to a maximum of 63 characters by default (see
https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS).
-}

------------------------------------------------

-- * API

-- | Prefix an Int to all aliases to preserve the uniqueness of identifiers.
-- See Note [Postgres identifier length limitations] above.
prefixNumToAliases :: S.Select -> S.Select
prefixNumToAliases = runUniq . uSelect

prefixNumToAliasesSelectWith :: S.SelectWithG S.Select -> S.SelectWithG S.Select
prefixNumToAliasesSelectWith = runUniq . uSelectWith

------------------------------------------------

-- * Data types

runUniq :: Uniq a -> a
runUniq = flip evalState emptyUniqState

emptyUniqState :: UniqState
emptyUniqState = UniqState 0 Map.empty

data UniqState = UniqState
  { -- | Used to generate fresh prefix ids
    _uqFreshId :: Int,
    -- | Mapping from an identifier to their prefix id
    _uqIdentifierMap :: Map.HashMap Identifier Int
  }
  deriving (Show, Eq)

type Uniq = State UniqState

------------------------------------------------

-- * Utilities

-- | attach a prefix id to an identifier
mkPrefixedName :: Identifier -> Int -> Identifier
mkPrefixedName identifier id' =
  Identifier ("_" <> tshow id' <> "_") <> identifier

-- | Return a prefixed alias by:
--   1. Generating a fresh prefix id for the alias' identifier
--   2. Adding the identifier + id to the identifier mapping
addAlias :: S.TableAlias -> Uniq S.TableAlias
addAlias (S.TableAlias identifier) = do
  UniqState freshId identifierMap <- get
  put $ UniqState (freshId + 1) $ Map.insert identifier freshId identifierMap
  pure $ S.TableAlias $ mkPrefixedName identifier freshId

-- | Search for the identifier in the identifier mapping and return
--   a prefixed identifier if found, or the original identifier
--   if not found in the identifier map.
getIdentifier :: Identifier -> Uniq Identifier
getIdentifier identifier = do
  UniqState _ identifierMap <- get
  let idM = Map.lookup identifier identifierMap
  pure $ maybe identifier (mkPrefixedName identifier) idM

-- | Run an action that might change the identifier mapping
--   and discard the changes made to the identifier map.
restoringIdentifierMap :: Uniq a -> Uniq a
restoringIdentifierMap action = do
  UniqState _ identifierMap <- get
  res <- action
  -- restore the identifier map to before the action
  modify' $ \s -> s {_uqIdentifierMap = identifierMap}
  pure res

------------------------------------------------

-- * Algorithm

-- | We run the algorithm on each CTE separately and discard the identifier mapping,
--   then we run the algorithm on the main select and return that result
--   (with the identifier mapping generated from that).
uSelectWith :: S.SelectWithG S.Select -> Uniq (S.SelectWithG S.Select)
uSelectWith (S.SelectWith ctes baseSelect) =
  S.SelectWith
    <$> forM ctes (\(als, sel) -> (als,) <$> restoringIdentifierMap (uSelect sel))
    <*> uSelect baseSelect

-- | We go in order of each component in the select, starting with
--   the from and CTE clauses (as those introduce new names).
--   We return a transformed 'Select' (with the identifier map in the 'Uniq' state).
--
--   We transform the table names but not column names.
uSelect :: S.Select -> Uniq S.Select
uSelect (S.Select ctes distM extrs fromM whereM grpM havnM ordByM limitM offM) = do
  -- Potentially introduces a new alias so it should go before the rest.
  newFromM <- mapM uFromExp fromM

  -- Potentially introduces a new alias in subsequent CTEs and the main select,
  -- so it should go first.
  newCTEs <- for ctes $ \(alias, cte) -> (,) <$> addAlias alias <*> uSelect cte

  newWhereM <- forM whereM $
    \(S.WhereFrag be) -> S.WhereFrag <$> uBoolExp be
  newGrpM <- forM grpM $
    \(S.GroupByExp l) -> S.GroupByExp <$> mapM uSqlExp l
  newHavnM <- forM havnM $
    \(S.HavingExp be) -> S.HavingExp <$> uBoolExp be
  newOrdM <- mapM uOrderBy ordByM
  newDistM <- mapM uDistinct distM
  newExtrs <- mapM uExtractor extrs
  pure $
    S.Select
      newCTEs
      newDistM
      newExtrs
      newFromM
      newWhereM
      newGrpM
      newHavnM
      newOrdM
      limitM
      offM
  where
    uDistinct = \case
      S.DistinctSimple -> pure S.DistinctSimple
      S.DistinctOn exprs -> S.DistinctOn <$> mapM uSqlExp exprs
    uExtractor (S.Extractor expr alias) =
      S.Extractor <$> uSqlExp expr <*> pure alias

-- | Transform every @from_item@.
--   Potentially introduces a new alias.
uFromExp :: S.FromExp -> Uniq S.FromExp
uFromExp (S.FromExp fromItems) =
  S.FromExp <$> mapM uFromItem fromItems

-- | Transform a single @from_item@.
--   Potentially introduces a new alias.
uFromItem :: S.FromItem -> Uniq S.FromItem
uFromItem fromItem = case fromItem of
  -- _Note_: Potentially introduces a new alias
  S.FISimple qualifiedTable maybeAlias ->
    S.FISimple qualifiedTable <$> mapM addAlias maybeAlias
  S.FIIdentifier identifier ->
    pure $ S.FIIdentifier identifier
  S.FIFunc funcExp ->
    S.FIFunc <$> uFunctionExp funcExp
  -- W transform the arguments and result table alias
  -- Note: Potentially introduces a new alias
  S.FIUnnest args als cols ->
    S.FIUnnest <$> mapM uSqlExp args <*> addAlias als <*> pure cols
  -- Note: Potentially introduces a new alias
  S.FISelect isLateral sel al -> do
    -- we are kind of ignoring if we have to reset
    -- identifiers to empty based on correlation.
    -- If this select is not part of a lateral join, then it shouldn't
    -- have access to tables exposed previously.
    -- > unless isLateral $ modify' $ \s -> s { _uqIdentifiers = Map.empty}
    newSel <- restoringIdentifierMap $ uSelect sel
    newAls <- addAlias al
    pure $ S.FISelect isLateral newSel newAls
  -- _Note_: Potentially introduces a new alias
  S.FISelectWith isLateral selectWith al -> do
    newSelectWith <- uSelectWith selectWith
    newAls <- addAlias al
    pure $ S.FISelectWith isLateral newSelectWith newAls
  S.FIValues (S.ValuesExp tups) als mCols -> do
    newValExp <- fmap S.ValuesExp $
      forM tups $ \(S.TupleExp ts) ->
        S.TupleExp <$> mapM uSqlExp ts
    pure $ S.FIValues newValExp als mCols
  -- _Note_: Potentially introduces a new alias
  S.FIJoin joinExp ->
    S.FIJoin <$> uJoinExp joinExp

-- | Transform a function call expression.
uFunctionExp :: S.FunctionExp -> Uniq S.FunctionExp
uFunctionExp (S.FunctionExp functionName args maybeAlias) =
  S.FunctionExp functionName
    <$> uFunctionArgs args
    <*> mapM uFunctionAlias maybeAlias

-- | Transform function call arguments.
uFunctionArgs :: S.FunctionArgs -> Uniq S.FunctionArgs
uFunctionArgs (S.FunctionArgs positional named) =
  S.FunctionArgs <$> mapM uSqlExp positional <*> mapM uSqlExp named

-- | Transform a function call alias.
uFunctionAlias :: S.FunctionAlias -> Uniq S.FunctionAlias
uFunctionAlias (S.FunctionAlias alias definitionList) =
  S.FunctionAlias <$> addAlias alias <*> pure definitionList

-- | Transform join expressions.
--   Potentially introduces a new alias.
uJoinExp :: S.JoinExpr -> Uniq S.JoinExpr
uJoinExp (S.JoinExpr left joinType right joinCond) = do
  leftN <- uFromItem left
  rightN <- uFromItem right
  joinCondN <- uJoinCond joinCond
  pure $ S.JoinExpr leftN joinType rightN joinCondN

-- | Transform Join condition. `ON` join condition might contain references
--   to table names and aliases.
uJoinCond :: S.JoinCond -> Uniq S.JoinCond
uJoinCond joinCond = case joinCond of
  S.JoinOn be -> S.JoinOn <$> uBoolExp be
  S.JoinUsing cols -> pure $ S.JoinUsing cols

-- | Transform boolean expression.
--
--   The boolean expression structure does not contain a table name currently,
--   So we look for 'SQLExp's and transform those, as those may contain table
--   names and aliases.
--
--   We discard new aliases that might be generated here because we don't
--   use them outside of the boolean expression.
uBoolExp :: S.BoolExp -> Uniq S.BoolExp
uBoolExp =
  restoringIdentifierMap . \case
    S.BELit b -> pure $ S.BELit b
    S.BEBin op left right ->
      S.BEBin op <$> uBoolExp left <*> uBoolExp right
    S.BENot b -> S.BENot <$> uBoolExp b
    S.BECompare op left right ->
      S.BECompare op <$> uSqlExp left <*> uSqlExp right
    S.BECompareAny op left right ->
      S.BECompareAny op <$> uSqlExp left <*> uSqlExp right
    S.BENull e -> S.BENull <$> uSqlExp e
    S.BENotNull e -> S.BENotNull <$> uSqlExp e
    S.BEExists sel -> S.BEExists <$> uSelect sel
    S.BEIN left exps -> S.BEIN <$> uSqlExp left <*> mapM uSqlExp exps
    S.BEExp e -> S.BEExp <$> uSqlExp e

-- | Transform a SQL expression.
--   We look for table names and aliases and rename them if needed.
--   SQL expressions do not introduce new table aliases, so we discard
--   the new aliases that might be generated here.
uSqlExp :: S.SQLExp -> Uniq S.SQLExp
uSqlExp =
  restoringIdentifierMap . \case
    S.SEPrep i -> pure $ S.SEPrep i
    S.SENull -> pure S.SENull
    S.SELit t -> pure $ S.SELit t
    S.SEUnsafe t -> pure $ S.SEUnsafe t
    S.SESelect s -> S.SESelect <$> uSelect s
    S.SEStar qual -> S.SEStar <$> traverse uQual qual
    S.SEIdentifier identifier -> pure $ S.SEIdentifier identifier
    -- this is for row expressions
    -- todo: check if this is always okay
    S.SERowIdentifier identifier -> S.SERowIdentifier <$> getIdentifier identifier
    -- we rename the table alias if needed
    S.SEQIdentifier (S.QIdentifier qualifier identifier) -> do
      newQualifier <- uQual qualifier
      pure $ S.SEQIdentifier $ S.QIdentifier newQualifier identifier
    S.SEFnApp fn args orderBy ->
      S.SEFnApp fn
        <$> mapM uSqlExp args
        <*> mapM uOrderBy orderBy
    S.SEOpApp op args ->
      S.SEOpApp op <$> mapM uSqlExp args
    S.SETyAnn e ty ->
      S.SETyAnn
        <$> uSqlExp e
        <*> pure ty
    S.SECond be onTrue onFalse ->
      S.SECond
        <$> uBoolExp be
        <*> uSqlExp onTrue
        <*> uSqlExp onFalse
    S.SEBool be ->
      S.SEBool <$> uBoolExp be
    S.SEExcluded t ->
      pure $ S.SEExcluded t
    S.SEArray l ->
      S.SEArray <$> mapM uSqlExp l
    S.SEArrayIndex arrayExp indexExp ->
      S.SEArrayIndex <$> uSqlExp arrayExp <*> uSqlExp indexExp
    S.SETuple (S.TupleExp l) ->
      S.SETuple . S.TupleExp <$> mapM uSqlExp l
    S.SECount cty -> pure $ S.SECount cty
    S.SENamedArg arg val -> S.SENamedArg arg <$> uSqlExp val
    S.SEFunction funcExp -> S.SEFunction <$> uFunctionExp funcExp
  where
    -- rename the table alias if needed
    uQual = \case
      S.QualifiedIdentifier identifier typeAnnotation ->
        S.QualifiedIdentifier <$> getIdentifier identifier <*> pure typeAnnotation
      -- I'm not sure why this isn't changed
      S.QualTable t -> pure $ S.QualTable t
      S.QualVar t -> pure $ S.QualVar t

-- | Transform order by clauses.
--   Since order by does not introduce new aliases we can discard the new names
--   that might be added, this is already done by `uSqlExp` though.
uOrderBy :: S.OrderByExp -> Uniq S.OrderByExp
uOrderBy (S.OrderByExp ordByItems) =
  S.OrderByExp <$> mapM uOrderByItem ordByItems
  where
    uOrderByItem (S.OrderByItem expr ordering nullsOrder) = do
      exprN <- uSqlExp expr
      pure $ S.OrderByItem exprN ordering nullsOrder
