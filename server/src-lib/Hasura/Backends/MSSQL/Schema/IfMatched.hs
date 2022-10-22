{-# LANGUAGE ApplicativeDo #-}

-- | This module contains the building blocks for parsing @if_matched@ clauses,
-- which in the MSSQL backend are used to implement upsert functionality.
-- These are used by 'Hasura.Backends.MSSQL.Instances.Schema.backendInsertParser' to
-- construct a mssql-specific schema parser for insert (and upsert) mutations.
module Hasura.Backends.MSSQL.Schema.IfMatched
  ( ifMatchedFieldParser,
  )
where

import Data.Has
import Data.Text.Extended
import Hasura.Backends.MSSQL.Types.Insert
import Hasura.Backends.MSSQL.Types.Internal (ScalarType (..))
import Hasura.GraphQL.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
    UnpreparedValue (..),
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

-- | Field-parser for:
--
-- > if_matched: tablename_if_matched
-- >
-- > input tablename_if_matched {
-- >   match_columns: [tablename_select_column!]
-- >   update_columns: [tablename_update_columns!]
-- >   where: tablename_bool_exp
-- > }
--
-- Note that the types ordinarily produced by this parser are only created if
-- the active role has /both/ select and update permissions to the table
-- @tablename@ defined /and/ these grant non-empty column permissions.
ifMatchedFieldParser ::
  forall r m n.
  MonadBuildSchema 'MSSQL r m n =>
  SourceName ->
  TableInfo 'MSSQL ->
  Maybe (SelPermInfo 'MSSQL) ->
  Maybe (UpdPermInfo 'MSSQL) ->
  m (InputFieldsParser n (Maybe (IfMatched (UnpreparedValue 'MSSQL))))
ifMatchedFieldParser sourceName tableInfo selectPerms updatePerms = do
  maybeObject <- ifMatchedObjectParser sourceName tableInfo selectPerms updatePerms
  return $ withJust maybeObject $ P.fieldOptional $$(G.litName "if_matched") (Just "upsert condition")

-- | Parse a @tablename_if_matched@ object.
ifMatchedObjectParser ::
  forall r m n.
  (MonadBuildSchema 'MSSQL r m n) =>
  SourceName ->
  TableInfo 'MSSQL ->
  Maybe (SelPermInfo 'MSSQL) ->
  Maybe (UpdPermInfo 'MSSQL) ->
  m (Maybe (Parser 'Input n (IfMatched (UnpreparedValue 'MSSQL))))
ifMatchedObjectParser sourceName tableInfo maybeSelectPerms maybeUpdatePerms = runMaybeT do
  -- Short-circuit if we don't have sufficient permissions.
  selectPerms <- hoistMaybe maybeSelectPerms
  updatePerms <- hoistMaybe maybeUpdatePerms
  matchColumnsEnum <- MaybeT $ tableInsertMatchColumnsEnum sourceName tableInfo selectPerms
  updateColumnsEnum <- MaybeT $ tableUpdateColumnsEnum tableInfo updatePerms

  -- The style of the above code gives me some cognitive dissonance: We could
  -- push the @hoistMaybe@ checks further away to callers, but not the enum
  -- parsers, and as a result we end up with @MaybeT@ is a sort of local maximum
  -- of legible expression.
  -- See https://github.com/hasura/graphql-engine-mono/issues/3125.

  tableGQLName <- getTableGQLName tableInfo
  objectName <- P.mkTypename $ tableGQLName <> $$(G.litName "_if_matched")
  let _imColumnPresets = partialSQLExpToUnpreparedValue <$> upiSet updatePerms
      updateFilter = fmap partialSQLExpToUnpreparedValue <$> upiFilter updatePerms
      objectDesc = G.Description $ "upsert condition type for table " <>> tableInfoName tableInfo
      matchColumnsName = $$(G.litName "match_columns")
      updateColumnsName = $$(G.litName "update_columns")
      whereName = $$(G.litName "where")

  whereExpParser <- lift $ boolExp sourceName tableInfo (Just selectPerms)
  pure $
    P.object objectName (Just objectDesc) $ do
      _imConditions <-
        (\whereExp -> BoolAnd $ updateFilter : maybeToList whereExp)
          <$> P.fieldOptional whereName Nothing whereExpParser
      _imMatchColumns <-
        P.fieldWithDefault matchColumnsName Nothing (G.VList []) (P.list matchColumnsEnum)
      _imUpdateColumns <-
        P.fieldWithDefault updateColumnsName Nothing (G.VList []) (P.list updateColumnsEnum)
      pure $ IfMatched {..}

-- | Table insert_match_columns enum
--
-- Parser for an enum type that matches the columns that can be used
-- for insert match_columns for a given table.
-- Maps to the insert_match_columns object.
--
-- Return Nothing if there's no column the current user has "select"
-- permissions for.
tableInsertMatchColumnsEnum ::
  forall m n r.
  (MonadSchema n m, MonadRole r m, MonadTableInfo r m, Has P.MkTypename r) =>
  SourceName ->
  TableInfo 'MSSQL ->
  SelPermInfo 'MSSQL ->
  m (Maybe (Parser 'Both n (Column 'MSSQL)))
tableInsertMatchColumnsEnum sourceName tableInfo selectPermissions = do
  tableGQLName <- getTableGQLName @'MSSQL tableInfo
  columns <- tableSelectColumns sourceName tableInfo selectPermissions
  enumName <- P.mkTypename $ tableGQLName <> $$(G.litName "_insert_match_column")
  let description =
        Just $
          G.Description $
            "select match_columns of table " <>> tableInfoName tableInfo
  pure $
    P.enum enumName description
      <$> nonEmpty
        [ ( define $ pgiName column,
            pgiColumn column
          )
          | column <- columns,
            isMatchColumnValid column
        ]
  where
    define name =
      P.Definition name (Just $ G.Description "column name") P.EnumValueInfo

-- | Check whether a column can be used for match_columns.
isMatchColumnValid :: ColumnInfo 'MSSQL -> Bool
isMatchColumnValid = \case
  -- Unfortunately MSSQL does not support comparison for TEXT types.
  ColumnInfo {pgiType = ColumnScalar TextType} -> False
  _ -> True
