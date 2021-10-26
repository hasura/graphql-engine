{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Convert the simple AST to an SQL query, ready to be passed
-- to the mysql package's query/exec functions.
module Hasura.Backends.MySQL.ToQuery
  ( Printer,
    toQueryPretty,
    fromSelect,
    toQueryFlat,
    Query (..),
    renderBuilderPretty,
    runBuilderPretty,
  )
where

import Data.ByteString (ByteString)
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.List (intersperse)
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Builder qualified as LT
import Data.Tuple (swap)
import Hasura.Backends.MySQL.Types
import Hasura.Prelude hiding (GT, LT)

newtype Query = Query {unQuery :: ByteString} deriving (Show, Eq, Monoid, Semigroup)

data Printer
  = SeqPrinter [Printer]
  | SepByPrinter Printer [Printer]
  | NewlinePrinter
  | QueryPrinter Query
  | IndentPrinter Int Printer
  deriving (Show, Eq)

instance IsString Printer where
  fromString = QueryPrinter . Query . fromString

(<+>) :: Printer -> Printer -> Printer
(<+>) x y = SeqPrinter [x, y]

-- Printer generators

fromExpression :: Expression -> Printer
fromExpression =
  \case
    ValueExpression value -> QueryPrinter (fromScalarType value)
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
    NotExpression expression -> "NOT " <+> fromExpression expression
    ExistsExpression sel -> "EXISTS (" <+> fromSelect sel <+> ")"
    InExpression x xs ->
      fromExpression x <+> " IN " <+> SeqPrinter (fmap fromExpression xs)
    ColumnExpression fieldName -> fromFieldName fieldName
    MethodExpression field method args ->
      fromExpression field <+> "."
        <+> fromString (T.unpack method)
        <+> "("
        <+> SeqPrinter (map fromExpression args)
        <+> ")"
    OpExpression op x y ->
      "("
        <+> fromExpression x
        <+> ") "
        <+> fromOp op
        <+> " ("
        <+> fromExpression y
        <+> ")"

fromScalarType :: ScalarValue -> Query
fromScalarType = \case
  BigValue v -> Query $ fromString $ show v
  BinaryValue v -> Query $ fromString $ show v
  BitValue v -> Query $ fromString $ show v
  BlobValue v -> Query $ fromString $ show v
  CharValue v -> Query $ fromString $ show v
  DatetimeValue v -> Query $ fromString $ show v
  DateValue v -> Query $ fromString $ show v
  DecimalValue v -> Query $ fromString $ show v
  DoubleValue v -> Query $ fromString $ show v
  EnumValue v -> Query $ fromString $ show v
  FloatValue v -> Query $ fromString $ show v
  GeometrycollectionValue v -> Query $ fromString $ show v
  GeometryValue v -> Query $ fromString $ show v
  IntValue v -> Query $ fromString $ show v
  JsonValue v -> Query $ fromString $ show v
  LinestringValue v -> Query $ fromString $ show v
  MediumValue v -> Query $ fromString $ show v
  MultilinestringValue v -> Query $ fromString $ show v
  MultipointValue v -> Query $ fromString $ show v
  MultipolygonValue v -> Query $ fromString $ show v
  NullValue -> Query $ fromString "NULL"
  NumericValue v -> Query $ fromString $ show v
  PointValue v -> Query $ fromString $ show v
  PolygonValue v -> Query $ fromString $ show v
  SmallValue v -> Query $ fromString $ show v
  TextValue v -> Query $ fromString $ show v
  TimestampValue v -> Query $ fromString $ show v
  TimeValue v -> Query $ fromString $ show v
  TinyValue v -> Query $ fromString $ show v
  VarbinaryValue v -> Query $ fromString $ show v
  VarcharValue v -> Query $ fromString $ show v
  YearValue v -> Query $ fromString $ show v
  other -> error $ "fromscalartype: not implemented " <> show other

fromOp :: Op -> Printer
fromOp =
  \case
    LT -> "<"
    GT -> ">"
    GTE -> ">="
    LTE -> "<="
    IN -> "IN"
    NIN -> "NOT IN"
    LIKE -> "LIKE"
    NLIKE -> "NOT LIKE"
    EQ' -> "="
    NEQ' -> "!="

fromFieldName :: FieldName -> Printer
fromFieldName (FieldName {..}) =
  fromNameText fNameEntity <+> "." <+> fromNameText fName

fromSelect :: Select -> Printer
fromSelect Select {..} =
  SepByPrinter
    NewlinePrinter
    $ [ "SELECT "
          <+> IndentPrinter
            7
            ( SepByPrinter
                ("," <+> NewlinePrinter)
                (map fromProjection (toList selectProjections))
            ),
        "FROM " <+> IndentPrinter 5 (fromFrom selectFrom),
        fromWhere selectWhere,
        fromOrderBys selectOrderBy,
        fromOffsetAndLimit selectSqlTop selectSqlOffset
      ]

-- https://dev.mysql.com/doc/refman/5.7/en/select.html
fromOffsetAndLimit :: Top -> Maybe Int -> Printer
fromOffsetAndLimit NoTop Nothing = ""
fromOffsetAndLimit NoTop (Just offset) =
  SeqPrinter
    [ "LIMIT " <+> fromString (show (maxBound :: Int)),
      IndentPrinter 9 (SepByPrinter NewlinePrinter [" OFFSET " <+> fromString (show offset)])
    ]
fromOffsetAndLimit (Top val) Nothing = SeqPrinter ["LIMIT " <+> fromString (show val)]
fromOffsetAndLimit (Top val) (Just offset) =
  SeqPrinter
    [ "LIMIT " <+> fromString (show val),
      IndentPrinter 9 (SepByPrinter NewlinePrinter [" OFFSET " <+> fromString (show offset)])
    ]

fromOrderBys ::
  Maybe (NonEmpty OrderBy) -> Printer
fromOrderBys Nothing = ""
fromOrderBys morderBys =
  SeqPrinter
    [ "ORDER BY ",
      IndentPrinter
        9
        ( SepByPrinter
            NewlinePrinter
            [ case morderBys of
                Nothing -> ""
                Just orderBys ->
                  SepByPrinter
                    ("," <+> NewlinePrinter)
                    (concatMap fromOrderBy (toList orderBys))
            ]
        )
    ]

fromOrderBy :: OrderBy -> [Printer]
fromOrderBy OrderBy {..} =
  [ fromNullsOrder orderByFieldName orderByNullsOrder,
    -- Above: This doesn't do anything when using text, ntext or image
    -- types. See below on CAST commentary.
    wrapNullHandling (fromFieldName orderByFieldName)
      <+> " "
      <+> fromOrder orderByOrder
  ]
  where
    wrapNullHandling inner = inner

fromOrder :: Order -> Printer
fromOrder =
  \case
    Asc -> "ASC"
    Desc -> "DESC"

-- Source <https://gregrs-uk.github.io/2011-02-02/mysql-order-by-with-nulls-first-or-last/>
fromNullsOrder :: FieldName -> NullsOrder -> Printer
fromNullsOrder fieldName =
  \case
    NullsAnyOrder -> ""
    -- ISNULL(NULL)=1, ISNULL(_) = 0 -- therefore we need DESC to put
    -- nulls first.
    NullsFirst -> "ISNULL(" <+> fromFieldName fieldName <+> ") DESC"
    NullsLast -> "ISNULL(" <+> fromFieldName fieldName <+> ") ASC"

fromProjection :: Projection -> Printer
fromProjection =
  \case
    ExpressionProjection aliasedExpression ->
      fromAliased (fmap fromExpression aliasedExpression)
    FieldNameProjection aliasedFieldName ->
      fromAliased (fmap fromFieldName aliasedFieldName)
    AggregateProjection aliasedAggregate ->
      fromAliased (fmap fromAggregate aliasedAggregate)
    AggregateProjections aliasedAggregates ->
      fromAliased
        ( fmap
            ( \aggs ->
                "STRUCT("
                  <+> IndentPrinter
                    7
                    ( SepByPrinter
                        ", "
                        (fmap (fromAliased . fmap fromAggregate) (toList aggs))
                    )
                  <+> ")"
            )
            aliasedAggregates
        )
    StarProjection -> "*"
    EntityProjection aliasedEntity ->
      fromAliased
        ( fmap
            ( \(fields :: [(FieldName, FieldOrigin)]) ->
                -- Example:
                --   STRUCT(
                --     IFNULL(
                --       `aa_articles1`.`aggregate`,
                --       STRUCT(0 as count, struct(null as id) as sum)
                --     ) as aggregate
                --   ) AS `articles_aggregate`
                --
                -- The (AS `articles_aggregate`) part at the end is rendered by 'fromAliased' evaluating
                -- at the root of this branch, and not by anything below
                "STRUCT("
                  <+> ( SepByPrinter
                          ", "
                          ( fields
                              <&> \(fieldName@FieldName {..}, fieldOrigin :: FieldOrigin) ->
                                "IFNULL(" <+> fromFieldName fieldName <+> ", " <+> fromFieldOrigin fieldOrigin
                                  <+> ") AS "
                                  <+> fromNameText fName
                          )
                      )
                  <+> ")"
            )
            aliasedEntity
        )
    ArrayEntityProjection entityAlias aliasedEntity ->
      fromAliased
        ( fmap
            ( \aggs ->
                "ARRAY(SELECT AS STRUCT "
                  <+> IndentPrinter
                    7
                    (SepByPrinter ", " (fmap fromFieldNameNaked (toList aggs)))
                  <+> " FROM "
                  <+> fromNameText (entityAliasText entityAlias)
                  <+> ".agg)"
            )
            aliasedEntity
        )
      where
        fromFieldNameNaked :: FieldName -> Printer
        fromFieldNameNaked (FieldName {..}) =
          fromNameText fName

fromAggregate :: Aggregate -> Printer
fromAggregate =
  \case
    CountAggregate countable -> "COUNT(" <+> fromCountable countable <+> ")"
    OpAggregate text args ->
      QueryPrinter (Query $ fromString $ show text)
        <+> "("
        <+> SepByPrinter ", " (map fromExpression (toList args))
        <+> ")"
    TextAggregate text -> fromExpression (ValueExpression (TextValue text))

fromCountable :: Countable FieldName -> Printer
fromCountable =
  \case
    StarCountable -> "*"
    NonNullFieldCountable fields ->
      SepByPrinter ", " (map fromFieldName (toList fields))
    DistinctCountable fields ->
      "DISTINCT "
        <+> SepByPrinter ", " (map fromFieldName (toList fields))

fromWhere :: Where -> Printer
fromWhere =
  \case
    Where [] -> ""
    Where expressions ->
      "WHERE "
        <+> IndentPrinter 6 (fromExpression (AndExpression expressions))

fromFrom :: From -> Printer
fromFrom =
  \case
    FromQualifiedTable aliasedQualifiedTableName ->
      fromAliased (fmap fromTableName aliasedQualifiedTableName)
    FromSelect select -> fromAliased (fmap (parens . fromSelect) select)

parens :: Printer -> Printer
parens x = "(" <+> IndentPrinter 1 x <+> ")"

fromTableName :: TableName -> Printer
fromTableName TableName {name, schema} =
  maybe "" ((<+> ".") . fromNameText) schema <+> fromNameText name

fromAliased :: Aliased Printer -> Printer
fromAliased Aliased {..} =
  aliasedThing
    <+> ((" AS " <+>) . fromNameText) aliasedAlias

fromNameText :: Text -> Printer
fromNameText t = QueryPrinter (Query . fromString . T.unpack $ t)

truePrinter :: Printer
truePrinter = "TRUE"

falsePrinter :: Printer
falsePrinter = "FALSE"

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
        NewlinePrinter -> Query " "
        IndentPrinter n p -> go (level + n) p
    notEmpty = (/= mempty)

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
        NewlinePrinter -> Query $ fromString $ show $ "\n" <> indentation level
        IndentPrinter n p -> go (level + n) p
    indentation n = T.replicate n " "
    notEmpty = (/= mempty)

-- | Produces a query with holes, and a mapping for each
renderBuilderPretty :: Printer -> (LT.Builder, InsOrdHashMap Int ScalarValue)
renderBuilderPretty =
  second (OMap.fromList . map swap . OMap.toList) . flip runState mempty
    . runBuilderPretty

runBuilderPretty :: Printer -> State (InsOrdHashMap ScalarValue Int) LT.Builder
runBuilderPretty = go 0
  where
    go level =
      \case
        SeqPrinter xs -> fmap (mconcat . filter notEmpty) (mapM (go level) xs)
        SepByPrinter x xs -> do
          i <- go level x
          fmap (mconcat . intersperse i . filter notEmpty) (mapM (go level) xs)
        NewlinePrinter -> pure ("\n" <> indentation level)
        IndentPrinter n p -> go (level + n) p
        QueryPrinter Query {unQuery = q} -> pure . LT.fromText . T.decodeUtf8 $ q
    indentation n = LT.fromText (T.replicate n " ")
    notEmpty = (/= mempty)

fromFieldOrigin :: FieldOrigin -> Printer
fromFieldOrigin = \case
  NoOrigin -> "NULL"
  AggregateOrigin aliasedAggregates ->
    "STRUCT("
      <+>
      -- Example: "0 AS count, STRUCT(NULL AS id) AS sum"
      SepByPrinter ", " (fromAliased . fmap fromNullAggregate <$> aliasedAggregates)
      <+> ")"

fromNullAggregate :: Aggregate -> Printer
fromNullAggregate = \case
  CountAggregate _ -> "0"
  OpAggregate _text _exp -> "NULL"
  TextAggregate _text -> "NULL"
