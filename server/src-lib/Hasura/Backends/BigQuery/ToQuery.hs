-- | Convert the simple BigQuery AST to an SQL query, ready to be passed
-- to the odbc package's query/exec functions.

module Hasura.Backends.BigQuery.ToQuery
  ( fromSelect
  , fromReselect
  , fromExpression
  , toBuilderFlat
  , toBuilderPretty
  , toTextFlat
  , toTextPretty
  , Printer(..)
  , renderBuilderFlat
  , renderBuilderPretty
  , paramName
  ) where

import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Foldable
import           Data.HashMap.Strict.InsOrd     (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd     as OMap
import           Data.List                      (intersperse)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe
import           Data.String
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as LT
import           Data.Text.Lazy.Builder         (Builder)
import qualified Data.Text.Lazy.Builder         as LT
import           Data.Tuple
import qualified Data.Vector                    as V
import           Hasura.Backends.BigQuery.Types
import           Prelude

--------------------------------------------------------------------------------
-- Types

data Printer
  = SeqPrinter [Printer]
  | SepByPrinter Printer [Printer]
  | NewlinePrinter
  | UnsafeTextPrinter Text
  | IndentPrinter Int Printer
  | ValuePrinter Value
  deriving (Show, Eq)

instance IsString Printer where
  fromString = UnsafeTextPrinter . fromString

(<+>) :: Printer -> Printer -> Printer
(<+>) x y = SeqPrinter [x,y]

--------------------------------------------------------------------------------
-- Printer generators

fromExpression :: Expression -> Printer
fromExpression =
  \case
    CastExpression e scalarType ->
      "CAST(" <+> fromExpression e <+> " AS " <+> fromScalarType scalarType <+> ")"
    InExpression e value ->
      "(" <+> fromExpression e <+> ") IN UNNEST(" <+> fromValue value <+> ")"
    JsonQueryExpression e -> "JSON_QUERY(" <+> fromExpression e <+> ")"
    JsonValueExpression e path ->
      "JSON_VALUE(" <+> fromExpression e <+> fromPath path <+> ")"
    ValueExpression value -> fromValue value
    AndExpression xs ->
      SepByPrinter
        (NewlinePrinter <+> "AND ")
        (toList
           (fmap
              (\x -> "(" <+> fromExpression x <+> ")")
              (fromMaybe (pure trueExpression) (NE.nonEmpty xs))))
    OrExpression xs ->
      SepByPrinter
        (NewlinePrinter <+> " OR ")
        (toList
           (fmap
              (\x -> "(" <+> fromExpression x <+> ")")
              (fromMaybe (pure falseExpression) (NE.nonEmpty xs))))
    NotExpression expression -> "NOT " <+> (fromExpression expression)
    ExistsExpression select ->
      "EXISTS (" <+> IndentPrinter 9 (fromSelect select) <+> ")"
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
    OpExpression op x y ->
      "(" <+>
      fromExpression x <+>
      ") " <+> fromOp op <+> " (" <+> fromExpression y <+> ")"
    ConditionalProjection expression fieldName ->
      "(CASE WHEN(" <+> fromExpression expression <+>
      ") THEN " <+> fromFieldName fieldName <+>
      " ELSE NULL END)"

fromScalarType :: ScalarType -> Printer
fromScalarType =
  \case
    StringScalarType     -> "STRING"
    BytesScalarType      -> "BYTES"
    IntegerScalarType    -> "INT64"
    FloatScalarType      -> "FLOAT64"
    BoolScalarType       -> "BOOL"
    TimestampScalarType  -> "TIMESTAMP"
    DateScalarType       -> "DATE"
    TimeScalarType       -> "TIME"
    DatetimeScalarType   -> "DATETIME"
    GeographyScalarType  -> "GEOGRAPHY"
    StructScalarType     -> "STRUCT"
    DecimalScalarType    -> "DECIMAL"
    BigDecimalScalarType -> "BIGDECIMAL"

fromOp :: Op -> Printer
fromOp =
  \case
    LessOp        -> "<"
    MoreOp        -> ">"
    MoreOrEqualOp -> ">="
    LessOrEqualOp -> "<="

fromPath :: JsonPath -> Printer
fromPath path =
  ", " <+> string path
  where
    string = fromExpression .
             ValueExpression . StringValue . LT.toStrict . LT.toLazyText . go
    go =
      \case
        RootPath      -> "$"
        IndexPath r i -> go r <> "[" <> LT.fromString (show i) <> "]"
        FieldPath r f -> go r <> "." <> LT.fromText f

fromFieldName :: FieldName -> Printer
fromFieldName (FieldName {..}) =
  fromNameText fieldNameEntity <+> "." <+> fromNameText fieldName

fromSelect :: Select -> Printer
fromSelect Select {..} = finalExpression
  where
    finalExpression = inner
    projections =
      SepByPrinter
        ("," <+> NewlinePrinter)
        (map fromProjection (toList selectProjections))
    inner =
      SepByPrinter
        NewlinePrinter
        [ "SELECT " <+> IndentPrinter 7 projections
        , "FROM " <+> IndentPrinter 5 (fromFrom selectFrom)
        , SepByPrinter
            NewlinePrinter
            (map
               (\Join {..} ->
                  SeqPrinter
                    [ "LEFT OUTER JOIN " <+>
                      IndentPrinter 16 (fromJoinSource joinSource)
                    , NewlinePrinter
                    , "AS " <+> fromJoinAlias joinAlias
                    , NewlinePrinter
                    , "ON (" <+>
                      IndentPrinter
                        4
                        (SepByPrinter (", " <+> NewlinePrinter) (map fromOn joinOn)) <+>
                      ")"
                    ])
               selectJoins)
        , fromWhere selectWhere
        , fromOrderBys selectTop selectOffset selectOrderBy
        , case selectGroupBy of
            []         -> ""
            fieldNames -> "GROUP BY " <+> SepByPrinter ", " (map fromFieldName fieldNames)
        ]

fromOn :: (FieldName, FieldName) -> Printer
fromOn (x,y) = fromFieldName x <+> " = " <+> fromFieldName y

fromJoinSource :: JoinSource -> Printer
fromJoinSource =
  \case
    JoinSelect select -> "(" <+> fromSelect select <+> ")"
    -- We're not using existingJoins at the moment, which was used to
    -- avoid re-joining on the same table twice.
    -- JoinReselect reselect -> "(" <+> fromReselect reselect <+> ")"

fromReselect :: Reselect -> Printer
fromReselect Reselect {..} =
  SepByPrinter
    NewlinePrinter
    [ "SELECT " <+>
      IndentPrinter 7 projections
    , fromWhere reselectWhere
    ]
  where
    projections =
      SepByPrinter
        ("," <+> NewlinePrinter)
        (map fromProjection (toList reselectProjections))

fromOrderBys ::
     Top -> Maybe Expression -> Maybe (NonEmpty OrderBy) -> Printer
fromOrderBys NoTop Nothing Nothing = "" -- An ORDER BY is wasteful if not needed.
fromOrderBys top moffset morderBys =
  SepByPrinter
    NewlinePrinter
    [ case morderBys of
        Nothing -> ""
        Just orderBys ->
          SeqPrinter
            [ "ORDER BY "
            , SepByPrinter
                ("," <+> NewlinePrinter)
                (map fromOrderBy (toList orderBys))
            ]
    , case (top, moffset) of
        (NoTop, Nothing) -> ""
        (NoTop, Just offset) -> "OFFSET " <+> fromExpression offset
        (Top n, Nothing) -> "LIMIT " <+> fromValue (IntegerValue (intToInt64 n))
        (Top n, Just offset) ->
          "OFFSET " <+>
          fromExpression offset <+>
          " LIMIT " <+> fromValue (IntegerValue (intToInt64 n))
    ]

fromOrderBy :: OrderBy -> Printer
fromOrderBy OrderBy {..} =
  "(" <+>
  fromFieldName orderByFieldName <+>
  ") " <+>
  fromOrder orderByOrder <+>
  fromNullsOrder orderByNullsOrder

fromOrder :: Order -> Printer
fromOrder =
  \case
    AscOrder  -> "ASC"
    DescOrder -> "DESC"

fromNullsOrder :: NullsOrder -> Printer
fromNullsOrder =
  \case
    NullsAnyOrder -> ""
    NullsFirst    -> " NULLS FIRST"
    NullsLast     -> " NULLS LAST"

fromJoinAlias :: EntityAlias -> Printer
fromJoinAlias EntityAlias {entityAliasText} =
  fromNameText entityAliasText

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
    ArrayAggProjection aliasedAgg -> fromAliased (fmap fromArrayAgg aliasedAgg)
    EntityProjection aliasedEntity ->  fromAliased (fmap fromJoinAlias aliasedEntity)

fromArrayAgg :: ArrayAgg -> Printer
fromArrayAgg ArrayAgg {..} =
  SeqPrinter
    [ "ARRAY_AGG("
    , SepByPrinter
        " "
        [ "STRUCT(" <+> projections <+> ")"
        , fromOrderBys
            arrayAggTop
            arrayAggOffset
            (fmap
               (fmap
                  (\orderBy ->
                     orderBy
                       { orderByNullsOrder = NullsAnyOrder
                       -- Because BigQuery reports:
                       -- > NULLS FIRST not supported with descending sort order in aggregate functions
                       -- And the same error with 'ascending'.
                       }))
               arrayAggOrderBy)
        ]
    , ")"
    ]
  where
    projections =
      SepByPrinter
        ("," <+> NewlinePrinter)
        (map fromProjection (toList arrayAggProjections))

fromAggregate :: Aggregate -> Printer
fromAggregate =
  \case
    CountAggregate countable -> "COUNT(" <+> fromCountable countable <+> ")"
    OpAggregate text arg ->
      UnsafeTextPrinter text <+> "(" <+> fromExpression arg <+> ")"
    OpAggregates text args ->
      "STRUCT(" <+>
      SepByPrinter
        ", "
        (map
           (\(alias, arg) ->
              UnsafeTextPrinter text <+>
              "(" <+> fromExpression arg <+> ") AS " <+> fromNameText alias)
           (toList args)) <+>
      ")"
    TextAggregate text -> fromExpression (ValueExpression (StringValue text))

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

fromTableName :: TableName -> Printer
fromTableName TableName {tableName, tableNameSchema} =
  fromNameText tableNameSchema <+> "." <+> fromNameText tableName

fromAliased :: Aliased Printer -> Printer
fromAliased Aliased {..} =
  aliasedThing <+>
  ((" AS " <+>) . fromNameText) aliasedAlias

fromNameText :: Text -> Printer
fromNameText t = UnsafeTextPrinter ("`" <> t <> "`")

trueExpression :: Expression
trueExpression = ValueExpression (BoolValue True)

falseExpression :: Expression
falseExpression = ValueExpression (BoolValue False)

fromValue :: Value -> Printer
fromValue = ValuePrinter

--------------------------------------------------------------------------------
-- Quick and easy query printer

toBuilderFlat :: Printer -> Builder
toBuilderFlat = flip evalState mempty . runBuilderFlat

toBuilderPretty :: Printer -> Builder
toBuilderPretty = flip evalState mempty . runBuilderPretty

toTextPretty :: Printer -> Text
toTextPretty = LT.toStrict . LT.toLazyText . toBuilderPretty

toTextFlat :: Printer -> Text
toTextFlat = LT.toStrict . LT.toLazyText . toBuilderFlat

--------------------------------------------------------------------------------
-- Printer ready for consumption

-- | Produces a query with holes, and a mapping for each
renderBuilderFlat :: Printer -> (Builder, InsOrdHashMap Int Value)
renderBuilderFlat =
  second (OMap.fromList . map swap . OMap.toList) . flip runState mempty .
  runBuilderFlat

-- | Produces a query with holes, and a mapping for each
renderBuilderPretty :: Printer -> (Builder, InsOrdHashMap Int Value)
renderBuilderPretty =
  second (OMap.fromList . map swap . OMap.toList) . flip runState mempty .
  runBuilderPretty

--------------------------------------------------------------------------------
-- Real printer engines

paramName :: Int -> Builder
paramName next = "param" <> fromString (show next)

runBuilderFlat :: Printer -> State (InsOrdHashMap Value Int) Builder
runBuilderFlat = go 0
  where
    go level =
      \case
        UnsafeTextPrinter q -> pure (LT.fromText q)
        SeqPrinter xs -> fmap (mconcat . filter notEmpty) (mapM (go level) xs)
        SepByPrinter x xs -> do
          i <- go level x
          fmap (mconcat . intersperse i . filter notEmpty) (mapM (go level) xs)
        NewlinePrinter -> pure " "
        IndentPrinter n p -> go (level + n) p
        ValuePrinter (ArrayValue x) | V.null x -> pure "[]"
        ValuePrinter v -> do
          themap <- get
          next <- case OMap.lookup v themap of
            Just next -> pure next
            Nothing -> do next <- gets OMap.size
                          modify (OMap.insert v next)
                          pure next
          pure ("@" <> paramName next)
    notEmpty = (/= mempty)

runBuilderPretty :: Printer -> State (InsOrdHashMap Value Int)  Builder
runBuilderPretty = go 0
  where
    go level =
      \case
        UnsafeTextPrinter q -> pure (LT.fromText q)
        SeqPrinter xs -> fmap (mconcat . filter notEmpty) (mapM (go level) xs)
        SepByPrinter x xs -> do
          i <- go level x
          fmap (mconcat . intersperse i . filter notEmpty) (mapM (go level) xs)
        NewlinePrinter -> pure ("\n" <> indentation level)
        IndentPrinter n p -> go (level + n) p
        ValuePrinter (ArrayValue x)
          | V.null x -> pure "[]"
        ValuePrinter v -> do
          themap <- get
          next <-
            case OMap.lookup v themap of
              Just next -> pure next
              Nothing -> do
                next <- gets OMap.size
                modify (OMap.insert v next)
                pure next
          pure ("@" <> paramName next)
    indentation n = LT.fromText (T.replicate n " ")
    notEmpty = (/= mempty)
