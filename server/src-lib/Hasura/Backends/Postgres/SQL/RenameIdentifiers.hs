-- | Postgres SQL Rename Identifiers
--
-- 1. Prefix table names with underscores to avoid issues where column names and tables conflict.
--    This can happen because we give columns and tables the name @root@ for some reason,
--    and that can trip up @row_to_json@.
--    See <https://github.com/PostgREST/postgrest/issues/993#issuecomment-340377813>.
--    An alternative solution would be to not create a @TableAlias@ with the name @root@,
--    but that seemed a bit complicated for me to do at the time.
--
-- 2. Bypass the Postgres limitation of truncating identifiers to 63 characters long
--    by prepending they identifier's md5 hash when they are longer than 63 characters.
--
--   We do both operations in the same traversal for performance reasons, but a simpler
--   implementation of (1) would be @transformBi prefixHash@ from the uniplate or the
--   generic-plate package.
--
-- See Postgres docs:
-- <https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS>
module Hasura.Backends.Postgres.SQL.RenameIdentifiers
  ( -- * Exported API
    renameIdentifiers,
    renameIdentifiersSelectWith,
    renameIdentifiersSelectWithTopLevelCTE,
  )
where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString.Base16 qualified as Base16
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types (Identifier (..), TableIdentifier (..), identifierToTableIdentifier, tableIdentifierToIdentifier)
import Hasura.Prelude

{- Note [Postgres identifier length limitations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Postgres truncates identifiers to a maximum of 63 characters by default (see
https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS).
-}

------------------------------------------------

-- * API

-- | Prefix table names with undescores and rename long identifiers.
renameIdentifiers :: S.Select -> S.Select
renameIdentifiers = renameTablesAndLongIdentifiers

-- | prefix table names with undescores and rename long identifiers.
renameIdentifiersSelectWith :: S.SelectWithG S.Select -> S.SelectWithG S.Select
renameIdentifiersSelectWith = renameTablesAndLongIdentifiersWith

-- | prefix table names with undescores and rename long identifiers.
renameIdentifiersSelectWithTopLevelCTE :: S.SelectWithG S.TopLevelCTE -> S.SelectWithG S.TopLevelCTE
renameIdentifiersSelectWithTopLevelCTE = renameTablesAndLongIdentifiersWithCTEs

------------------------------------------------

-- * Prefix long identifiers

-- $prefix_md5_implementation
--
-- We are traversing the query transform 'Identifier's in a query that are
-- longer than 63 characters by prefixing them with their md5 hash.
--
-- This works because:
--
-- 1. Database table references and column references also use `Identifier`, but they
--    cannot be more than 63 characters long, so we will never transform those cases.
-- 2. The md5 hash is a 32 characters long deterministic representation of the identifier,
--    so even when truncated by postgres, it will always be enough to identify the identifiers.
-- 3. It is possible in theory for to identifiers to produce the same hash, but extremely
--    unlikely and I don't think we'll ever run into such a case.
--
-- /Note/ that we could in theory replace the identifier with a hash,
-- but we prefix the hash instead for our benefit as developers - if we need
-- to read the query at some point we can look at the rest of the identifier
-- ignoring the hash for a readable representation.

---------------------------------------------------

-- | Prefix md5 hash if identifier length is over 63 characters.
--   We assume (rightly) that identifiers with names longer than 63 characters are not
--   database table columns, and are made by us using aliases, so we should be free to
--   rename them.
prefixHash :: Text -> Text
prefixHash name =
  if T.length name > 63
    then
      let hash = T.decodeUtf8 . Base16.encode . MD5.hash . T.encodeUtf8 $ name
       in "md5_" <> hash <> "_" <> name
    else name

identifierPrefixHash :: Identifier -> Identifier
identifierPrefixHash (Identifier name) = Identifier $ prefixHash name

------------------------------------------------

-- * Prefix table names with underscore

-- \$prefix_md5_implementation

-- | Prefix table names with underscores to avoid issues where column names and tables conflict.
--   This can happen because we give columns and tables the name @root@ for some reason,
--   and that can trip up @row_to_json@.
--   See <https://github.com/PostgREST/postgrest/issues/993#issuecomment-340377813>.
--   An alternative solution would be to not create a @TableAlias@ with the name @root@,
--   but that seemed a bit complicated for me to do at the time.

-- ** API

renameTablesAndLongIdentifiers :: S.Select -> S.Select
renameTablesAndLongIdentifiers = runMyState . uSelect

renameTablesAndLongIdentifiersWith :: S.SelectWithG S.Select -> S.SelectWithG S.Select
renameTablesAndLongIdentifiersWith = runMyState . uSelectWith

renameTablesAndLongIdentifiersWithCTEs :: S.SelectWithG S.TopLevelCTE -> S.SelectWithG S.TopLevelCTE
renameTablesAndLongIdentifiersWithCTEs = runMyState . uSelectWithCTEs

------------------------------------------------

-- ** Data types

runMyState :: MyState a -> a
runMyState = flip evalState noTables

noTables :: TableNames
noTables = TableNames mempty

-- | The tables in scope
newtype TableNames = TableNames
  { _tables :: Set.HashSet TableIdentifier
  }
  deriving (Show, Eq)

type MyState = State TableNames

------------------------------------------------

-- ** Utilities

-- | attach a prefix to an identifier
mkPrefixedTableName :: Text -> Text
mkPrefixedTableName identifier = prefixHash $ "_" <> identifier

-- | Add the alias to the set and return a prefixed alias.
addAliasAndPrefixHash :: S.TableAlias -> MyState S.TableAlias
addAliasAndPrefixHash tableAlias@(S.TableAlias identifier) = do
  tables <- _tables <$> get
  put $ TableNames $ Set.insert (S.tableAliasToIdentifier tableAlias) tables
  pure $ S.TableAlias $ Identifier $ mkPrefixedTableName (getIdenTxt identifier)

-- | Search for the identifier in the table names set and return
--   a prefixed identifier if found, or the original identifier
--   if not found in the set.
getTableNameAndPrefixHash :: Identifier -> MyState Identifier
getTableNameAndPrefixHash identifier =
  tableIdentifierToIdentifier <$> getTableIdentifierAndPrefixHash (identifierToTableIdentifier identifier)

-- | Search for the table identifier in the table names set and return
--   a prefixed table identifier if found, or the original table identifier
--   if not found in the set.
getTableIdentifierAndPrefixHash :: TableIdentifier -> MyState TableIdentifier
getTableIdentifierAndPrefixHash identifier = do
  tables <- _tables <$> get
  pure
    $ if Set.member identifier tables
      then TableIdentifier $ mkPrefixedTableName (unTableIdentifier identifier)
      else identifier

-- | Run an action that might change the tables names set
--   and discard the changes made to the set.
restoringTables :: MyState a -> MyState a
restoringTables action = do
  tables <- _tables <$> get
  res <- action
  -- restore the tables to before the action
  modify' $ \s -> s {_tables = tables}
  pure res

------------------------------------------------

-- ** Algorithm

-- | We apply the renaming algorithm to @SELECT@ parts and ignore the others, as we don't
--   generally generate long identifiers on mutations.
uSelectWithCTEs :: S.SelectWithG S.TopLevelCTE -> MyState (S.SelectWithG S.TopLevelCTE)
uSelectWithCTEs (S.SelectWith ctes baseSelect) =
  S.SelectWith
    <$> forM
      ctes
      ( \(alias, topLevelCTE) ->
          (,)
            <$> addAliasAndPrefixHash alias
            <*> ( case topLevelCTE of
                    S.CTESelect select -> S.CTESelect <$> restoringTables (uSelect select)
                    other -> pure other
                )
      )
    <*> uSelect baseSelect

-- | We run the algorithm on each CTE separately and discard the table names set,
--   then we run the algorithm on the main select and return that result
--   (with the table names found in scope).
uSelectWith :: S.SelectWithG S.Select -> MyState (S.SelectWithG S.Select)
uSelectWith (S.SelectWith ctes baseSelect) =
  S.SelectWith
    <$> forM
      ctes
      ( \(alias, sel) ->
          (,)
            <$> addAliasAndPrefixHash alias
            <*> restoringTables (uSelect sel)
      )
    <*> uSelect baseSelect

-- | We go in order of each component in the select, starting with
--   the from and CTE clauses (as those introduce new table names to scope).
--   We return a transformed 'Select' (with the table names).
uSelect :: S.Select -> MyState S.Select
uSelect (S.Select ctes distinctM extrs fromM whereM groupByM havingM orderByM limitM offsetM) = do
  -- Potentially introduces a new alias in subsequent CTEs and the main select,
  -- so it should go first.
  newCTEs <- for ctes $ \(alias, cte) ->
    (,)
      <$> addAliasAndPrefixHash alias
      <*> case cte of
        S.ICTESelect select -> S.ICTESelect <$> uSelect select
        S.ICTEUnsafeRawSQL q -> S.ICTEUnsafeRawSQL <$> traverse uSqlExp q

  -- Potentially introduces a new alias so it should go before the rest.
  newFromM <- mapM uFromExp fromM

  newWhereM <- forM whereM
    $ \(S.WhereFrag be) -> S.WhereFrag <$> uBoolExp be
  newGroupByM <- forM groupByM
    $ \(S.GroupByExp l) -> S.GroupByExp <$> mapM uSqlExp l
  newHavingM <- forM havingM
    $ \(S.HavingExp be) -> S.HavingExp <$> uBoolExp be
  newOrderByM <- mapM uOrderBy orderByM
  newDistinctM <- mapM uDistinct distinctM
  newExtrs <- mapM uExtractor extrs
  newLimitM <- mapM uLimit limitM
  newOffsetM <- mapM uOffset offsetM
  pure
    $ S.Select
      newCTEs
      newDistinctM
      newExtrs
      newFromM
      newWhereM
      newGroupByM
      newHavingM
      newOrderByM
      newLimitM
      newOffsetM
  where
    uDistinct = \case
      S.DistinctSimple -> pure S.DistinctSimple
      S.DistinctOn exprs -> S.DistinctOn <$> mapM uSqlExp exprs
    uExtractor (S.Extractor expr alias) =
      S.Extractor <$> uSqlExp expr <*> pure (fmap prefixHashColumnAlias alias)
    uLimit (S.LimitExp expr) = S.LimitExp <$> uSqlExp expr
    uOffset (S.OffsetExp expr) = S.OffsetExp <$> uSqlExp expr

-- | Transform every @from_item@.
--   Potentially introduces a new alias.
uFromExp :: S.FromExp -> MyState S.FromExp
uFromExp (S.FromExp fromItems) =
  S.FromExp <$> mapM uFromItem fromItems

-- | Transform a single @from_item@.
--   Potentially introduces a new alias.
uFromItem :: S.FromItem -> MyState S.FromItem
uFromItem fromItem = case fromItem of
  -- _Note_: Potentially introduces a new alias
  -- qualifiedTable represents a database table so we don't need to prefix it with a hash.
  S.FISimple qualifiedTable maybeAlias ->
    S.FISimple qualifiedTable <$> mapM addAliasAndPrefixHash maybeAlias
  S.FIIdentifier identifier ->
    S.FIIdentifier <$> getTableIdentifierAndPrefixHash identifier
  S.FIFunc funcExp ->
    S.FIFunc <$> uFunctionExp funcExp
  -- We transform the arguments and result table alias
  -- Note: Potentially introduces a new alias
  S.FIUnnest args tableAlias columnAliases ->
    S.FIUnnest
      <$> mapM uSqlExp args
      <*> addAliasAndPrefixHash tableAlias
      <*> pure (map prefixHashColumnAlias columnAliases)
  -- Note: Potentially introduces a new alias
  S.FISelect isLateral select alias -> do
    -- we are kind of ignoring if we have to reset
    -- identifiers to empty based on correlation.
    -- If this select is not part of a lateral join, then it shouldn't
    -- have access to tables exposed previously.
    -- > unless isLateral $ modify' $ \s -> s { _uqIdentifiers = Map.empty}
    newSel <- restoringTables $ uSelect select
    newAls <- addAliasAndPrefixHash alias
    pure $ S.FISelect isLateral newSel newAls
  -- _Note_: Potentially introduces a new alias
  S.FISelectWith isLateral selectWith alias -> do
    newSelectWith <- uSelectWith selectWith
    newAls <- addAliasAndPrefixHash alias
    pure $ S.FISelectWith isLateral newSelectWith newAls
  S.FIValues (S.ValuesExp tups) alias mCols -> do
    newValExp <- fmap S.ValuesExp
      $ forM tups
      $ \(S.TupleExp ts) ->
        S.TupleExp <$> mapM uSqlExp ts
    pure $ S.FIValues newValExp (prefixHashTableAlias alias) (fmap (map prefixHashColumnAlias) mCols)
  -- _Note_: Potentially introduces a new alias
  S.FIJoin joinExp ->
    S.FIJoin <$> uJoinExp joinExp

-- | Transform a function call expression.
uFunctionExp :: S.FunctionExp -> MyState S.FunctionExp
uFunctionExp (S.FunctionExp functionName args maybeAlias) =
  S.FunctionExp functionName
    <$> uFunctionArgs args
    <*> mapM uFunctionAlias maybeAlias

-- | Transform function call arguments.
uFunctionArgs :: S.FunctionArgs -> MyState S.FunctionArgs
uFunctionArgs (S.FunctionArgs positional named) =
  S.FunctionArgs <$> mapM uSqlExp positional <*> mapM uSqlExp named

-- | Transform a function call alias.
uFunctionAlias :: S.FunctionAlias -> MyState S.FunctionAlias
uFunctionAlias (S.FunctionAlias alias definitionList) =
  S.FunctionAlias
    <$> addAliasAndPrefixHash alias
    <*> pure (fmap (map uDefinitionList) definitionList)
  where
    uDefinitionList (S.FunctionDefinitionListItem columnAlias typ) =
      S.FunctionDefinitionListItem (prefixHashColumnAlias columnAlias) typ

-- | Transform join expressions.
--   Potentially introduces a new alias.
uJoinExp :: S.JoinExpr -> MyState S.JoinExpr
uJoinExp (S.JoinExpr left joinType right joinCond) = do
  leftN <- uFromItem left
  rightN <- uFromItem right
  joinCondN <- uJoinCond joinCond
  pure $ S.JoinExpr leftN joinType rightN joinCondN

-- | Transform Join condition. `ON` join condition might contain references
--   to table names and aliases.
uJoinCond :: S.JoinCond -> MyState S.JoinCond
uJoinCond joinCond = case joinCond of
  S.JoinOn be -> S.JoinOn <$> uBoolExp be
  S.JoinUsing cols -> pure $ S.JoinUsing $ map identifierPrefixHash cols

-- | Transform boolean expression.
--
--   The boolean expression structure does not contain a table name currently,
--   So we look for 'SQLExp's and transform those, as those may contain table
--   names and aliases.
--
--   We discard table names that might be introduced here because we don't
--   use them outside of the boolean expression.
uBoolExp :: S.BoolExp -> MyState S.BoolExp
uBoolExp =
  restoringTables . \case
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
uSqlExp :: S.SQLExp -> MyState S.SQLExp
uSqlExp =
  restoringTables . \case
    S.SEPrep i -> pure $ S.SEPrep i
    S.SENull -> pure S.SENull
    S.SELit t -> pure $ S.SELit t
    S.SEUnsafe t -> pure $ S.SEUnsafe t
    S.SESelect s -> S.SESelect <$> uSelect s
    S.SEStar qual -> S.SEStar <$> traverse uQual qual
    S.SEIdentifier identifier -> pure $ S.SEIdentifier $ identifierPrefixHash identifier
    -- this is for row expressions
    S.SERowIdentifier identifier -> S.SERowIdentifier <$> getTableNameAndPrefixHash identifier
    -- we rename the table alias if needed
    S.SEQIdentifier qIdentifier -> do
      S.SEQIdentifier <$> uQIdentifier qIdentifier
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
    S.SECount cty -> S.SECount <$> traverse uQIdentifier cty
    S.SENamedArg arg val -> S.SENamedArg arg <$> uSqlExp val
    S.SEFunction funcExp -> S.SEFunction <$> uFunctionExp funcExp
  where
    uQIdentifier (S.QIdentifier qualifier identifier) = do
      newQualifier <- uQual qualifier
      pure $ S.QIdentifier newQualifier $ Identifier $ prefixHash $ getIdenTxt identifier

    -- rename the table alias if needed
    uQual = \case
      S.QualifiedIdentifier identifier typeAnnotation ->
        S.QualifiedIdentifier <$> getTableIdentifierAndPrefixHash identifier <*> pure typeAnnotation
      -- refers to a database table
      S.QualTable t -> pure $ S.QualTable t
      S.QualVar t -> pure $ S.QualVar t

-- | Transform order by clauses.
--   Since order by does not introduce new aliases we can discard the new names
--   that might be added, this is already done by `uSqlExp` though.
uOrderBy :: S.OrderByExp -> MyState S.OrderByExp
uOrderBy (S.OrderByExp ordByItems) =
  S.OrderByExp <$> mapM uOrderByItem ordByItems
  where
    uOrderByItem (S.OrderByItem expr ordering nullsOrder) = do
      exprN <- uSqlExp expr
      pure $ S.OrderByItem exprN ordering nullsOrder

-- | Prefix a table alias with a hash if needed.
prefixHashTableAlias :: S.TableAlias -> S.TableAlias
prefixHashTableAlias (S.TableAlias identifier) = S.TableAlias (Identifier $ prefixHash $ getIdenTxt identifier)

-- | Prefix a column alias with a hash if needed.
prefixHashColumnAlias :: S.ColumnAlias -> S.ColumnAlias
prefixHashColumnAlias (S.ColumnAlias identifier) = S.ColumnAlias (Identifier $ prefixHash $ getIdenTxt identifier)
