{-# LANGUAGE OverloadedStrings #-}

-- | Convert the simple T-SQL AST to an SQL query, ready to be passed
-- to the odbc package's query/exec functions.

module Hasura.SQL.Tsql.ToQuery where

import           Data.Foldable
import           Data.List (intersperse)
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

fromSelect :: Select -> Query
fromSelect Select {..} =
  mconcat
    (intersperse
       "\n"
       [ "SELECT"
       , fromCommented (fmap fromTop selectTop)
       , mconcat (intersperse ", " (map fromProjection (toList selectProjections)))
       , "FROM"
       , fromFrom selectFrom
       ])

fromProjection :: Projection -> Query
fromProjection =
  \case
    ExpressionProjection aliasedExpression ->
      fromAliased (fmap fromExpression aliasedExpression)

fromTop :: Top -> Query
fromTop =
  \case
    NoTop -> ""
    Top i -> "TOP " <> toSql i

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
