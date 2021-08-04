{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Convert the simple AST to an SQL query, ready to be passed
-- to the mysql package's query/exec functions.

module Hasura.Backends.MySQL.ToQuery
  ( Printer
  , toQueryPretty
  , fromSelect
  , toQueryFlat
  , Query(..)
  )
where

import           Data.ByteString             (ByteString)
import           Data.List                   (intersperse)
import           Data.String
import qualified Data.Text                   as T
import           Hasura.Backends.MySQL.Types
import           Hasura.Prelude              hiding (GT, LT)

newtype Query = Query { unQuery :: ByteString } deriving (Show, Eq, Monoid, Semigroup)

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
(<+>) x y = SeqPrinter [x,y]

(<+>?) :: Printer -> Maybe Printer -> Printer
(<+>?) x Nothing  = x
(<+>?) x (Just y) = SeqPrinter [x,y]


-- Printer generators

fromExpression :: Expression -> Printer
fromExpression =
  \case
    ValueExpression value -> QueryPrinter (fromScalarType value)
    AndExpression xs ->
      case xs of
        [] -> ""
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
    ExistsExpression sel     -> "EXISTS (" <+> fromSelect sel <+> ")"
    ColumnExpression fieldName -> fromFieldName fieldName
    MethodExpression field method args ->
      fromExpression field <+> "." <+>
      fromString (T.unpack method) <+>
      "(" <+> SeqPrinter (map fromExpression args) <+> ")"
    OpExpression op x y ->
      "(" <+>
      fromExpression x <+>
      ") " <+> fromOp op <+> " (" <+> fromExpression y <+> ")"

fromScalarType :: ScalarValue -> Query
fromScalarType (IntValue v) = Query $ fromString (show v)
fromScalarType other        = error $ "fromscalartype: not implemented " <> show other

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

fromFieldName :: FieldName -> Printer
fromFieldName (FieldName {..}) =
  fromNameText fNameEntity <+> "." <+> fromNameText fName

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

-- https://dev.mysql.com/doc/refman/5.7/en/select.html
fromOffsetAndLimit :: Top -> Maybe Expression -> Printer
fromOffsetAndLimit NoTop Nothing = ""
fromOffsetAndLimit NoTop (Just offset) =
  SeqPrinter
    [ "LIMIT " <+> fromString (show (maxBound :: Int)),
      IndentPrinter 9 (SepByPrinter NewlinePrinter [" OFFSET " <+> fromExpression offset])
    ]
fromOffsetAndLimit (Top val) Nothing = SeqPrinter ["LIMIT " <+> fromString (show val)]
fromOffsetAndLimit (Top val) (Just offset) =
  SeqPrinter
    [ "LIMIT " <+> fromString (show val),
      IndentPrinter 9 (SepByPrinter NewlinePrinter [" OFFSET " <+> fromExpression offset])
    ]

fromOrderBys ::
     Top -> Maybe Expression -> Maybe (NonEmpty OrderBy) -> Printer
fromOrderBys top offset Nothing = fromOffsetAndLimit top offset
fromOrderBys _ moffset morderBys =
  SeqPrinter
    [ "ORDER BY "
    , IndentPrinter
        9
        (SepByPrinter
           NewlinePrinter
           [ case morderBys of
               Nothing -> ""
               Just orderBys ->
                 SepByPrinter
                   ("," <+> NewlinePrinter)
                   (concatMap fromOrderBy (toList orderBys))
           , case moffset of
               Nothing -> ""
               (Just offset) ->
                 "OFFSET " <+> fromExpression offset
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
    wrapNullHandling inner = inner

fromOrder :: Order -> Printer
fromOrder =
  \case
    Asc  -> "ASC"
    Desc -> "DESC"

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
      "FOR JSON PATH" <+>
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
      QueryPrinter (Query $ fromString $ show text) <+>
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
    Where [] -> ""
    Where expressions ->
      "WHERE " <+>
      IndentPrinter 6 (fromExpression (AndExpression expressions))

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
      quote mPath = maybe "" ((\p -> " '" <+> p <+> "'"). go) mPath
      go = \case
        RootPath      -> "$"
        IndexPath r i -> go r <+> "[" <+> fromString (show i) <+> "]"
        FieldPath r f -> go r <+> ".\"" <+> fromString (T.unpack f) <+> "\""

fromTableName :: TableName -> Printer
fromTableName TableName {name, schema} =
  fromNameText schema <+> "." <+> fromNameText name

fromAliased :: Aliased Printer -> Printer
fromAliased Aliased {..} =
  aliasedThing <+>
  ((" AS " <+>) . fromNameText) aliasedAlias

fromNameText :: Text -> Printer
fromNameText t = QueryPrinter (Query . fromString . T.unpack $ t)

falsePrinter :: Printer
falsePrinter = "(1<>1)"

-- | Wrap a select with things needed when using FOR JSON.
wrapFor :: For -> Printer -> Printer
wrapFor for' inner = nullToArray
  where
    nullToArray =
      case for' of
        NoFor     -> rooted
        JsonFor _ -> rooted
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
