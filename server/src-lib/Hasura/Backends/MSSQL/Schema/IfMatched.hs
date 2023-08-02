{-# LANGUAGE ApplicativeDo #-}

-- | MSSQL Schema IfMatched
--
-- This module contains the building blocks for parsing @if_matched@ clauses
-- (represented as 'IfMatched'), which in the MSSQL backend are used to
-- implement upsert functionality.
--
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
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache
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
  ( MonadBuildSchema 'MSSQL r m n,
    AggregationPredicatesSchema 'MSSQL
  ) =>
  TableInfo 'MSSQL ->
  SchemaT r m (InputFieldsParser n (Maybe (IfMatched (UnpreparedValue 'MSSQL))))
ifMatchedFieldParser tableInfo = do
  maybeObject <- ifMatchedObjectParser tableInfo
  pure case maybeObject of
    Nothing -> pure Nothing
    Just object -> P.fieldOptional Name._if_matched (Just "upsert condition") object

-- | Parse a @tablename_if_matched@ object.
ifMatchedObjectParser ::
  forall r m n.
  ( MonadBuildSchema 'MSSQL r m n,
    AggregationPredicatesSchema 'MSSQL
  ) =>
  TableInfo 'MSSQL ->
  SchemaT r m (Maybe (Parser 'Input n (IfMatched (UnpreparedValue 'MSSQL))))
ifMatchedObjectParser tableInfo = runMaybeT do
  -- Short-circuit if we don't have sufficient permissions.
  sourceInfo :: SourceInfo 'MSSQL <- asks getter
  roleName <- retrieve scRole
  let customization = _siCustomization sourceInfo
      mkTypename = runMkTypename $ _rscTypeNames customization
  updatePerms <- hoistMaybe $ _permUpd $ getRolePermInfo roleName tableInfo
  matchColumnsEnum <- MaybeT $ tableInsertMatchColumnsEnum tableInfo
  lift do
    updateColumnsEnum <- updateColumnsPlaceholderParser tableInfo
    tableGQLName <- getTableGQLName tableInfo
    let objectName = mkTypename $ tableGQLName <> Name.__if_matched
        _imColumnPresets = partialSQLExpToUnpreparedValue <$> upiSet updatePerms
        updateFilter = fmap partialSQLExpToUnpreparedValue <$> upiFilter updatePerms
        objectDesc = G.Description $ "upsert condition type for table " <>> tableInfoName tableInfo
        matchColumnsName = Name._match_columns
        updateColumnsName = Name._update_columns
        whereName = Name._where
    whereExpParser <- tableBoolExp tableInfo
    pure
      $ P.object objectName (Just objectDesc) do
        _imConditions <-
          (\whereExp -> BoolAnd $ updateFilter : maybeToList whereExp)
            <$> P.fieldOptional whereName Nothing whereExpParser
        _imMatchColumns <-
          P.fieldWithDefault matchColumnsName Nothing (G.VList []) (P.list matchColumnsEnum)
        _imUpdateColumns <-
          P.fieldWithDefault updateColumnsName Nothing (G.VList []) (P.list updateColumnsEnum) `P.bindFields` \cs ->
            -- this can only happen if the placeholder was used
            sequenceA cs `onNothing` parseError "erroneous column name"

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
  forall r m n.
  (MonadBuildSourceSchema 'MSSQL r m n) =>
  TableInfo 'MSSQL ->
  SchemaT r m (Maybe (Parser 'Both n (Column 'MSSQL)))
tableInsertMatchColumnsEnum tableInfo = do
  sourceInfo :: SourceInfo 'MSSQL <- asks getter
  let customization = _siCustomization sourceInfo
      mkTypename = runMkTypename $ _rscTypeNames customization
  tableGQLName <- getTableGQLName @'MSSQL tableInfo
  columns <- tableSelectColumns tableInfo
  let enumName = mkTypename $ tableGQLName <> Name.__insert_match_column
      description =
        Just
          $ G.Description
          $ "select match_columns of table "
          <>> tableInfoName tableInfo
  pure
    $ P.enum enumName description
    <$> nonEmpty
      [ ( define $ ciName column,
          ciColumn column
        )
        | -- TODO(redactionExp): Does the redaction expression need to be considered here?
          (SCIScalarColumn column, _redactionExp) <- columns,
          isMatchColumnValid column
      ]
  where
    define name =
      P.Definition name (Just $ G.Description "column name") Nothing [] P.EnumValueInfo

-- | Check whether a column can be used for match_columns.
isMatchColumnValid :: ColumnInfo 'MSSQL -> Bool
isMatchColumnValid = \case
  -- Unfortunately MSSQL does not support comparison for TEXT types.
  ColumnInfo {ciType = ColumnScalar TextType} -> False
  _ -> True
