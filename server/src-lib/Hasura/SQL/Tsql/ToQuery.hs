{-# LANGUAGE OverloadedStrings #-}

-- | Convert the simple T-SQL AST to an SQL query, ready to be passed
-- to the odbc package's query/exec functions.

module Hasura.SQL.Tsql.ToQuery where

import           Data.Foldable
import           Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.ODBC.SQLServer
import           Hasura.SQL.Tsql.Types
import           Prelude

fromExpression :: Expression -> Query
fromExpression =
  \case
    ValueExpression value -> toSql value
    AndExpression xs ->
      mconcat
        (intersperse
           " AND "
           (toList
              (fmap
                 (\x -> "(" <> fromExpression x <> ")")
                 (fromMaybe (pure trueExpression) (NE.nonEmpty xs)))))
    OrExpression xs ->
      mconcat
        (intersperse
           " OR "
           (toList
              (fmap
                 (\x -> "(" <> fromExpression x <> ")")
                 (fromMaybe (pure falseExpression) (NE.nonEmpty xs)))))
    NotExpression expression -> "NOT " <> (fromExpression expression)
    SelectExpression select -> fromSelect select
    IsNullExpression expression ->
      "(" <> fromExpression expression <> ") IS NULL"
    ColumnExpression fieldName -> fromFieldName fieldName

fromFieldName :: FieldName -> Query
fromFieldName (FieldName text) = fromString (T.unpack text)

fromSelect :: Select -> Query
fromSelect Select {..} =
  mconcat
    (intersperse
       "\n"
       [ "SELECT"
       , fromCommented (fmap fromTop selectTop)
       , mconcat
           (intersperse ", " (map fromProjection (toList selectProjections)))
       , "FROM"
       , fromFrom selectFrom
       , mconcat
           (map
              (\Join {..} ->
                 " OUTER APPLY (" <> fromSelect joinSelect <> ") AS " <>
                 fromFieldName joinFieldName)
              selectJoins)
       , fromWhere selectWhere
       ])

fromProjection :: Projection -> Query
fromProjection =
  \case
    ExpressionProjection aliasedExpression ->
      fromAliased (fmap fromExpression aliasedExpression)
    FieldNameProjection aliasedFieldName ->
      fromAliased (fmap fromFieldName aliasedFieldName)
    AggregateProjection aliasedAggregate ->
      fromAliased (fmap fromAggregate aliasedAggregate)

fromAggregate :: Aggregate -> Query
fromAggregate =
  \case
    CountAggregate countable -> "COUNT(" <> fromCountable countable <> ")"
    OpAggregate text fieldName ->
      fromString (T.unpack text) <> "(" <> fromFieldName fieldName <> ")"
    TextAggregate text -> fromExpression (ValueExpression (TextValue text))

fromCountable :: Countable -> Query
fromCountable =
  \case
    StarCountable -> "*"
    NonNullFieldCountable fields ->
      mconcat (intersperse ", " (map fromFieldName (toList fields)))
    DistinctCountable fields ->
      "DISTINCT " <>
      mconcat (intersperse ", " (map fromFieldName (toList fields)))

fromTop :: Top -> Query
fromTop =
  \case
    NoTop -> ""
    Top i -> "TOP " <> toSql i

fromWhere :: Where -> Query
fromWhere =
  \case
    NoWhere -> ""
    ExpressionWhere expression ->
      case collapse expression of
        ValueExpression (BoolValue True) -> ""
        collapsedExpression -> "WHERE " <> fromExpression collapsedExpression
        -- Later move this to a rewrite module.
      where collapse (AndExpression [x]) = collapse x
            collapse (AndExpression []) = trueExpression
            collapse (OrExpression [x]) = collapse x
            collapse x = x

fromFrom :: From -> Query
fromFrom =
  \case
    FromQualifiedTable aliasedQualifiedTableName ->
      fromAliased
        (fmap (fromQualified . fmap fromTableName) aliasedQualifiedTableName)

fromTableName :: TableName -> Query
fromTableName TableName {tableNameText} = fromNameText tableNameText

fromAliased :: Aliased Query -> Query
fromAliased Aliased {..} =
  aliasedThing <>
  maybe mempty ((" AS " <>) . fromAlias) aliasedAlias

fromQualified :: Qualified Query -> Query
fromQualified Qualified {..} =
  maybe mempty ((<> ".") . fromSchemaName) qualifiedSchemaName <> qualifiedThing

fromSchemaName :: SchemaName -> Query
fromSchemaName SchemaName {schemaNameParts} =
  mconcat (intersperse "." (map fromNameText schemaNameParts))

fromAlias :: Alias -> Query
fromAlias (Alias text) = fromNameText text

fromNameText :: Text -> Query
fromNameText t = "[" <> fromString (T.unpack t) <> "]"

fromCommented :: Commented Query -> Query
fromCommented Commented {..} =
  commentedThing <>
  maybe
    mempty
    (\comment -> " /* " <> fromComment comment <> " */ ")
    commentedComment

fromComment :: Comment -> Query
fromComment =
  \case
    DueToPermission -> "Due to permission"

trueExpression :: Expression
trueExpression = ValueExpression (BoolValue True)

falseExpression :: Expression
falseExpression = ValueExpression (BoolValue False)
