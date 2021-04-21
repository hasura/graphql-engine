{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Convert the simple T-SQL AST to an SQL query, ready to be passed
-- to the odbc package's query/exec functions.

module Hasura.Backends.MSSQL.ToQuery
  ( fromSelect
  , withExplain
  , fromReselect
  , toSQL
  , toQueryFlat
  , toQueryPretty
  , fromDelete
  , Printer(..)
  ) where

import           Hasura.Prelude              hiding (GT, LT)

import qualified Data.Text                   as T

import           Data.List                   (intersperse)
import           Data.String
import           Database.ODBC.SQLServer

import           Hasura.Backends.MSSQL.Types
import           Hasura.SQL.Types            (ToSQL (..))


--------------------------------------------------------------------------------
-- Types

data Printer
  = SeqPrinter [Printer]
  | SepByPrinter Printer [Printer]
  | NewlinePrinter
  | QueryPrinter Query
  | IndentPrinter Int Printer
  deriving (Show, Eq)

instance IsString Printer where
  fromString = QueryPrinter . fromString

(<+>) :: Printer -> Printer -> Printer
(<+>) x y = SeqPrinter [x,y]

(<+>?) :: Printer -> Maybe Printer -> Printer
(<+>?) x Nothing  = x
(<+>?) x (Just y) = SeqPrinter [x,y]


--------------------------------------------------------------------------------
-- Instances

instance ToSQL Expression where
  toSQL = fromString . show . toQueryFlat . fromExpression


--------------------------------------------------------------------------------
-- Printer generators

fromExpression :: Expression -> Printer
fromExpression =
  \case
    JsonQueryExpression e -> "JSON_QUERY(" <+> fromExpression e <+> ")"
    JsonValueExpression e path ->
      "JSON_VALUE(" <+> fromExpression e <+> ", " <+> fromPath path <+> ")"
    ValueExpression value -> QueryPrinter (toSql value)
    AndExpression xs ->
      SepByPrinter
        (NewlinePrinter <+> "AND ")
        (toList
           (fmap
              (\x -> "(" <+> fromExpression x <+> ")")
              (fromMaybe (pure trueExpression) (nonEmpty xs))))
    OrExpression xs ->
      SepByPrinter
        (NewlinePrinter <+> " OR ")
        (toList
           (fmap
              (\x -> "(" <+> fromExpression x <+> ")")
              (fromMaybe (pure falseExpression) (nonEmpty xs))))
    NotExpression expression -> "NOT " <+> (fromExpression expression)
    ExistsExpression select -> "EXISTS (" <+> fromSelect select <+> ")"
    IsNullExpression expression ->
      "(" <+> fromExpression expression <+> ") IS NULL"
    IsNotNullExpression expression ->
      "(" <+> fromExpression expression <+> ") IS NOT NULL"
    ColumnExpression fieldName -> fromFieldName fieldName
    EqualExpression x y ->
      "(" <+> fromExpression x <+> ") = (" <+> fromExpression y <+> ")"
    NotEqualExpression x y ->
      "(" <+> fromExpression x <+> ") != (" <+> fromExpression y <+> ")"
    ToStringExpression e -> "CONCAT(" <+> fromExpression e <+> ", '')"
    SelectExpression s -> "(" <+> IndentPrinter 1 (fromSelect s) <+> ")"
    MethodExpression field method args ->
      fromExpression field <+> "." <+>
      fromString (T.unpack method) <+>
      "(" <+> SeqPrinter (map fromExpression args) <+> ")"
    OpExpression op x y ->
      "(" <+>
      fromExpression x <+>
      ") " <+> fromOp op <+> " (" <+> fromExpression y <+> ")"
    ListExpression xs -> SepByPrinter ", " $ fromExpression <$> xs
    STOpExpression op e str ->
      "(" <+> fromExpression e <+> ")." <+>
      fromString (show op) <+>
      "(" <+> fromExpression str <+> ") = 1"

fromOp :: Op -> Printer
fromOp =
  \case
    LT    -> "<"
    GT    -> ">"
    GTE   -> ">="
    LTE   -> "<="
    IN    -> "IN"
    NIN   -> "NOT IN"
    LIKE  -> "LIKE"
    NLIKE -> "NOT LIKE"

fromPath :: JsonPath -> Printer
fromPath = \case
  RootPath      -> "$"
  IndexPath r i -> fromPath r <+> "[" <+> fromString (show i) <+> "]"
  FieldPath r f -> fromPath r <+> ".\"" <+> fromString (T.unpack f) <+> "\""

fromFieldName :: FieldName -> Printer
fromFieldName (FieldName {..}) =
  fromNameText fieldNameEntity <+> "." <+> fromNameText fieldName

fromDelete :: Delete -> Printer
fromDelete Delete {deleteTable, deleteWhere} =
  SepByPrinter
    NewlinePrinter
    [ "DELETE " <+> fromNameText (aliasedAlias deleteTable)
    , "FROM " <+> fromAliased (fmap fromTableName deleteTable)
    , fromWhere deleteWhere
    ]

fromSelect :: Select -> Printer
fromSelect Select {..} = case selectFor of
  NoFor     -> result
  JsonFor _ -> SeqPrinter ["SELECT ISNULL((", result, "), '[]')"]
  where
    result =
      SepByPrinter
      NewlinePrinter
      [ "SELECT " <+>
        IndentPrinter
          7
          (SepByPrinter
             ("," <+> NewlinePrinter)
             (map fromProjection (toList selectProjections)))
      , "FROM " <+> IndentPrinter 5 (fromFrom selectFrom)
      , SepByPrinter
          NewlinePrinter
          (map
             (\Join {..} ->
                SeqPrinter
                  [ "OUTER APPLY ("
                  , IndentPrinter 13 (fromJoinSource joinSource)
                  , ") "
                  , NewlinePrinter
                  , "AS "
                  , fromJoinAlias joinJoinAlias
                  ])
             selectJoins)
      , fromWhere selectWhere
      , fromOrderBys selectTop selectOffset selectOrderBy
      , fromFor selectFor
      ]

withExplain :: Printer -> Printer
withExplain p =
  SepByPrinter
    NewlinePrinter
      [ "SET SHOWPLAN_TEXT ON"
      , p
      , "SET SHOWPLAN_TEXT OFF"
      ]

fromJoinSource :: JoinSource -> Printer
fromJoinSource =
  \case
    JoinSelect select     -> fromSelect select
    JoinReselect reselect -> fromReselect reselect

fromReselect :: Reselect -> Printer
fromReselect Reselect {..} =
  SepByPrinter
    NewlinePrinter
    [ "SELECT " <+>
      IndentPrinter
        7
        (SepByPrinter
           ("," <+> NewlinePrinter)
           (map fromProjection (toList reselectProjections)))
    , fromFor reselectFor
    , fromWhere reselectWhere
    ]

fromOrderBys ::
     Top -> Maybe Expression -> Maybe (NonEmpty OrderBy) -> Printer
fromOrderBys NoTop Nothing Nothing = "" -- An ORDER BY is wasteful if not needed.
fromOrderBys top moffset morderBys =
  SeqPrinter
    [ "ORDER BY "
    , IndentPrinter
        9
        (SepByPrinter
           NewlinePrinter
           [ case morderBys of
               Nothing -> "1"
               Just orderBys ->
                 SepByPrinter
                   ("," <+> NewlinePrinter)
                   (concatMap fromOrderBy (toList orderBys))
           , case (top, moffset) of
               (NoTop, Nothing) -> ""
               (NoTop, Just offset) ->
                 "OFFSET " <+> fromExpression offset <+> " ROWS"
               (Top n, Nothing) ->
                 "OFFSET 0 ROWS FETCH NEXT " <+>
                 QueryPrinter (toSql n) <+> " ROWS ONLY"
               (Top n, Just offset) ->
                 "OFFSET " <+>
                 fromExpression offset <+>
                 " ROWS FETCH NEXT " <+> QueryPrinter (toSql n) <+> " ROWS ONLY"
           ])
    ]


fromOrderBy :: OrderBy -> [Printer]
fromOrderBy OrderBy {..} =
  [ fromNullsOrder orderByFieldName orderByNullsOrder
  , fromFieldName orderByFieldName <+> " " <+> fromOrder orderByOrder
  ]

fromOrder :: Order -> Printer
fromOrder =
  \case
    AscOrder  -> "ASC"
    DescOrder -> "DESC"

fromNullsOrder :: FieldName -> NullsOrder -> Printer
fromNullsOrder fieldName =
  \case
    NullsAnyOrder -> ""
    NullsFirst    -> "IIF(" <+> fromFieldName fieldName <+> " IS NULL, 0, 1)"
    NullsLast     -> "IIF(" <+> fromFieldName fieldName <+> " IS NULL, 1, 0)"

fromJoinAlias :: JoinAlias -> Printer
fromJoinAlias JoinAlias {..} =
  fromNameText joinAliasEntity <+>?
  fmap (\name -> "(" <+> fromNameText name <+> ")") joinAliasField

fromFor :: For -> Printer
fromFor =
  \case
    NoFor -> ""
    JsonFor ForJson {jsonCardinality, jsonRoot = root} ->
      "FOR JSON PATH" <+>
      case jsonCardinality of
        JsonArray -> ""
        JsonSingleton ->
          ", WITHOUT_ARRAY_WRAPPER" <+>
          case root of
            NoRoot    -> ""
            Root text -> "ROOT(" <+> QueryPrinter (toSql text) <+> ")"

fromProjection :: Projection -> Printer
fromProjection =
  \case
    ExpressionProjection aliasedExpression ->
      fromAliased (fmap fromExpression aliasedExpression)
    FieldNameProjection aliasedFieldName ->
      fromAliased (fmap fromFieldName aliasedFieldName)
    AggregateProjection aliasedAggregate ->
      fromAliased (fmap fromAggregate aliasedAggregate)
    StarProjection -> "*"

fromAggregate :: Aggregate -> Printer
fromAggregate =
  \case
    CountAggregate countable -> "COUNT(" <+> fromCountable countable <+> ")"
    OpAggregate text args ->
      QueryPrinter (rawUnescapedText text) <+>
      "(" <+> SepByPrinter ", " (map fromExpression (toList args)) <+> ")"
    TextAggregate text -> fromExpression (ValueExpression (TextValue text))

fromCountable :: Countable FieldName -> Printer
fromCountable =
  \case
    StarCountable -> "*"
    NonNullFieldCountable fields ->
      SepByPrinter ", " (map fromFieldName (toList fields))
    DistinctCountable fields ->
      "DISTINCT " <+>
      SepByPrinter ", " (map fromFieldName (toList fields))

fromWhere :: Where -> Printer
fromWhere =
  \case
    Where expressions ->
      case (filter ((/= trueExpression) . collapse)) expressions of
        [] -> ""
        collapsedExpressions ->
          "WHERE " <+>
          IndentPrinter 6 (fromExpression (AndExpression collapsedExpressions))
      where collapse (AndExpression [x]) = collapse x
            collapse (AndExpression [])  = trueExpression
            collapse (OrExpression [x])  = collapse x
            collapse x                   = x

fromFrom :: From -> Printer
fromFrom =
  \case
    FromQualifiedTable aliasedQualifiedTableName ->
      fromAliased (fmap fromTableName aliasedQualifiedTableName)
    FromOpenJson openJson -> fromAliased (fmap fromOpenJson openJson)

fromOpenJson :: OpenJson -> Printer
fromOpenJson OpenJson {openJsonExpression, openJsonWith} =
  SepByPrinter
    NewlinePrinter
    [ "OPENJSON(" <+>
      IndentPrinter 9 (fromExpression openJsonExpression) <+> ")"
    , "WITH (" <+>
      IndentPrinter
        5
        (SepByPrinter
           ("," <+> NewlinePrinter)
           (toList (fmap fromJsonFieldSpec openJsonWith))) <+>
      ")"
    ]

fromJsonFieldSpec :: JsonFieldSpec -> Printer
fromJsonFieldSpec =
  \case
    IntField name mPath    -> fromNameText name <+> " INT" <+> quote mPath
    StringField name mPath -> fromNameText name <+> " NVARCHAR(MAX)" <+> quote mPath
    UuidField name mPath   -> fromNameText name <+> " UNIQUEIDENTIFIER" <+> quote mPath
    JsonField name mPath   -> fromJsonFieldSpec (StringField name mPath) <+> " AS JSON"
    where
      quote mPath = maybe "" ((\p -> " '" <+> p <+> "'"). fromPath) mPath

fromTableName :: TableName -> Printer
fromTableName TableName {tableName, tableSchema} =
  fromNameText tableSchema <+> "." <+> fromNameText tableName

fromAliased :: Aliased Printer -> Printer
fromAliased Aliased {..} =
  aliasedThing <+>
  ((" AS " <+>) . fromNameText) aliasedAlias

fromNameText :: Text -> Printer
fromNameText t = QueryPrinter (rawUnescapedText ("[" <> t <> "]"))

trueExpression :: Expression
trueExpression = ValueExpression (BoolValue True)

falseExpression :: Expression
falseExpression = ValueExpression (BoolValue False)

--------------------------------------------------------------------------------
-- Basic printing API

toQueryFlat :: Printer -> Query
toQueryFlat = go 0
  where
    go level =
      \case
        QueryPrinter q -> q
        SeqPrinter xs -> mconcat (filter notEmpty (map (go level) xs))
        SepByPrinter x xs ->
          mconcat
            (intersperse (go level x) (filter notEmpty (map (go level) xs)))
        NewlinePrinter -> " "
        IndentPrinter n p -> go (level + n) p
    notEmpty = (/= mempty) . renderQuery

toQueryPretty :: Printer -> Query
toQueryPretty = go 0
  where
    go level =
      \case
        QueryPrinter q -> q
        SeqPrinter xs -> mconcat (filter notEmpty (map (go level) xs))
        SepByPrinter x xs ->
          mconcat
            (intersperse (go level x) (filter notEmpty (map (go level) xs)))
        NewlinePrinter -> "\n" <> indentation level
        IndentPrinter n p -> go (level + n) p
    indentation n = rawUnescapedText (T.replicate n " ")
    notEmpty = (/= mempty) . renderQuery
