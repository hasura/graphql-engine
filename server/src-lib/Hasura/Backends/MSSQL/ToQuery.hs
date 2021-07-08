{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Convert the simple T-SQL AST to an SQL query, ready to be passed
-- to the odbc package's query/exec functions.

module Hasura.Backends.MSSQL.ToQuery
  ( fromSelect
  , fromReselect
  , toSQL
  , toQueryFlat
  , toQueryPretty
  , fromDelete
  , Printer(..)
  ) where

import           Hasura.Prelude              hiding (GT, LT)

import qualified Data.Text                   as T
import qualified Data.Text.Extended          as T
import qualified Data.Text.Lazy              as L
import qualified Data.Text.Lazy.Builder      as L

import           Data.Aeson                  (ToJSON (..))
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

-- This is a debug instance, only here because it avoids a circular
-- dependency between this module and Types/Instances.
instance ToJSON Expression where
  toJSON = toJSON . T.toTxt . toQueryFlat . fromExpression

--------------------------------------------------------------------------------
-- Printer generators

fromExpression :: Expression -> Printer
fromExpression =
  \case
    CastExpression e t ->
      "CAST(" <+> fromExpression e <+>
      " AS " <+> fromString (T.unpack t) <+> ")"
    JsonQueryExpression e -> "JSON_QUERY(" <+> fromExpression e <+> ")"
    JsonValueExpression e path ->
      "JSON_VALUE(" <+> fromExpression e <+> fromPath path <+> ")"
    ValueExpression value -> QueryPrinter (toSql value)
    AndExpression xs ->
      case xs of
        [] -> truePrinter
        _ ->
          SepByPrinter
            (NewlinePrinter <+> "AND ")
            (fmap (\x -> "(" <+> fromExpression x <+> ")") (toList xs))
    OrExpression xs ->
      case xs of
        [] -> falsePrinter
        _ ->
          SepByPrinter
            (NewlinePrinter <+> "OR ")
            (fmap (\x -> "(" <+> fromExpression x <+> ")") (toList xs))
    NotExpression expression -> "NOT " <+> (fromExpression expression)
    ExistsExpression sel     -> "EXISTS (" <+> fromSelect sel <+> ")"
    IsNullExpression expression ->
      "(" <+> fromExpression expression <+> ") IS NULL"
    IsNotNullExpression expression ->
      "(" <+> fromExpression expression <+> ") IS NOT NULL"
    ColumnExpression fieldName -> fromFieldName fieldName
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
    ConditionalProjection expression fieldName ->
      "(CASE WHEN(" <+>
      fromExpression expression <+>
      ") THEN " <+> fromFieldName fieldName <+> " ELSE NULL END)"

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
    EQ'   -> "="
    NEQ'  -> "!="

fromPath :: JsonPath -> Printer
fromPath path =
  ", " <+> string path
  where
    string = fromExpression .
             ValueExpression . TextValue . L.toStrict . L.toLazyText . go
    go =
      \case
        RootPath      -> "$"
        IndexPath r i -> go r <> "[" <> L.fromString (show i) <> "]"
        FieldPath r f -> go r <> ".\"" <> L.fromText f <> "\""

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
fromSelect Select {..} = wrapFor selectFor result
  where
    result =
      SepByPrinter
      NewlinePrinter $
      [ "SELECT " <+>
        IndentPrinter
          7
          (SepByPrinter
             ("," <+> NewlinePrinter)
             (map fromProjection (toList selectProjections)))
      ] <>
      [ "FROM " <+> IndentPrinter 5 (fromFrom f) | Just f <- [selectFrom] ] <>
      [ SepByPrinter
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

fromJoinSource :: JoinSource -> Printer
fromJoinSource =
  \case
    JoinSelect sel        -> fromSelect sel
    JoinReselect reselect -> fromReselect reselect

fromReselect :: Reselect -> Printer
fromReselect Reselect {..} = wrapFor reselectFor result
  where
    result =
      SepByPrinter
        NewlinePrinter
        [ "SELECT " <+>
          IndentPrinter
            7
            (SepByPrinter
               ("," <+> NewlinePrinter)
               (map fromProjection (toList reselectProjections)))
        , fromWhere reselectWhere
        , fromFor reselectFor
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
               -- If you ORDER BY 1, a text field will signal an
               -- error. What we want instead is to just order by
               -- nothing, but also satisfy the syntactic
               -- requirements. Thus ORDER BY (SELECT NULL).
               --
               -- This won't create consistent orderings, but that's
               -- why you should specify an order_by in your GraphQL
               -- query anyway, to define the ordering.
               Nothing -> "(SELECT NULL) /* ORDER BY is required for OFFSET */"
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
                 QueryPrinter (toSql (IntValue n)) <+> " ROWS ONLY"
               (Top n, Just offset) ->
                 "OFFSET " <+>
                 fromExpression offset <+>
                 " ROWS FETCH NEXT " <+> QueryPrinter (toSql (IntValue n)) <+> " ROWS ONLY"
           ])
    ]


fromOrderBy :: OrderBy -> [Printer]
fromOrderBy OrderBy {..} =
  [ fromNullsOrder orderByFieldName orderByNullsOrder
    -- Above: This doesn't do anything when using text, ntext or image
    -- types. See below on CAST commentary.
  , wrapNullHandling (fromFieldName orderByFieldName) <+>
    " " <+> fromOrder orderByOrder
  ]
  where
    wrapNullHandling inner =
      case orderByType of
        Just TextType  -> castTextish inner
        Just WtextType -> castTextish inner
        -- Above: For some types, we have to do null handling manually
        -- ourselves:
        _              -> inner
    -- Direct quote from SQL Server error response:
    --
    -- > The text, ntext, and image data types cannot be compared or
    -- > sorted, except when using IS NULL or LIKE operator.
    --
    -- So we cast it to a varchar, maximum length.
    castTextish inner = "CAST(" <+> inner <+> " AS VARCHAR(MAX))"

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
    JsonFor ForJson {jsonCardinality} ->
      "FOR JSON PATH, INCLUDE_NULL_VALUES" <+>
      case jsonCardinality of
        JsonArray     -> ""
        JsonSingleton -> ", WITHOUT_ARRAY_WRAPPER"

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
    Where expressions
      | Just whereExp <- collapseWhere (AndExpression expressions) ->
        "WHERE " <+> IndentPrinter 6 (fromExpression whereExp)
      | otherwise -> ""

-- Drop useless examples like this from the output:
--
-- WHERE (((1<>1))
--       AND ((1=1)))
--       AND ((1=1))
--
-- And
--
-- WHERE ((1<>1))
--
-- They're redundant, but make the output less readable.
collapseWhere :: Expression -> Maybe Expression
collapseWhere = go
  where
    go =
      \case
        ValueExpression (BoolValue True) -> Nothing
        AndExpression xs ->
          case mapMaybe go xs of
            [] -> Nothing
            ys -> pure (AndExpression ys)
        e -> pure e

fromFrom :: From -> Printer
fromFrom =
  \case
    FromQualifiedTable aliasedQualifiedTableName ->
      fromAliased (fmap fromTableName aliasedQualifiedTableName)
    FromOpenJson openJson -> fromAliased (fmap fromOpenJson openJson)
    FromSelect select -> fromAliased (fmap (parens . fromSelect) select)

fromOpenJson :: OpenJson -> Printer
fromOpenJson OpenJson {openJsonExpression, openJsonWith} =
  SepByPrinter
    NewlinePrinter
    [ "OPENJSON(" <+>
      IndentPrinter 9 (fromExpression openJsonExpression) <+> ")"
    , case openJsonWith of
        Nothing -> ""
        Just openJsonWith' -> "WITH (" <+>
          IndentPrinter
            5
            (SepByPrinter
               ("," <+> NewlinePrinter)
               (fmap fromJsonFieldSpec $ toList openJsonWith')) <+> ")"
    ]

fromJsonFieldSpec :: JsonFieldSpec -> Printer
fromJsonFieldSpec =
  \case
    IntField name mPath    -> fromNameText name <+> " INT" <+> quote mPath
    StringField name mPath -> fromNameText name <+> " NVARCHAR(MAX)" <+> quote mPath
    UuidField name mPath   -> fromNameText name <+> " UNIQUEIDENTIFIER" <+> quote mPath
    JsonField name mPath   -> fromJsonFieldSpec (StringField name mPath) <+> " AS JSON"
    where
      quote mPath = maybe "" ((\p -> " '" <+> p <+> "'"). go) mPath
      go = \case
        RootPath      -> "$"
        IndexPath r i -> go r <+> "[" <+> fromString (show i) <+> "]"
        FieldPath r f -> go r <+> ".\"" <+> fromString (T.unpack f) <+> "\""

fromTableName :: TableName -> Printer
fromTableName TableName {tableName, tableSchema} =
  fromNameText tableSchema <+> "." <+> fromNameText tableName

fromAliased :: Aliased Printer -> Printer
fromAliased Aliased {..} =
  aliasedThing <+>
  ((" AS " <+>) . fromNameText) aliasedAlias

fromNameText :: Text -> Printer
fromNameText t = QueryPrinter (rawUnescapedText ("[" <> t <> "]"))

truePrinter :: Printer
truePrinter = "(1=1)"

falsePrinter :: Printer
falsePrinter = "(1<>1)"

parens :: Printer -> Printer
parens p = "(" <+> IndentPrinter 1 p <+> ")"

-- | Wrap a select with things needed when using FOR JSON.
wrapFor :: For -> Printer -> Printer
wrapFor for' inner = nullToArray
  where
    nullToArray =
      case for' of
        NoFor     -> rooted
        JsonFor _ -> SeqPrinter ["SELECT ISNULL((", rooted, "), '[]')"]
    rooted =
      case for' of
        JsonFor ForJson {jsonRoot, jsonCardinality = JsonSingleton} ->
          case jsonRoot of
            NoRoot -> inner
            -- This is gross, but unfortunately ROOT and
            -- WITHOUT_ARRAY_WRAPPER are not allowed to be used at the
            -- same time (reason not specified). Therefore we just
            -- concatenate the necessary JSON string literals around
            -- the JSON.
            Root text ->
              SeqPrinter
                [ fromString ("SELECT CONCAT('{" <> show text <> ":', (")
                , inner
                , "), '}')"
                ]
        _ -> inner

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
