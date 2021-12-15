{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Convert the simple T-SQL AST to an SQL query, ready to be passed
-- to the odbc package's query/exec functions.
module Hasura.Backends.MSSQL.ToQuery
  ( fromSelect,
    fromReselect,
    toQueryFlat,
    toQueryPretty,
    fromInsert,
    fromSetIdentityInsert,
    fromDelete,
    fromUpdate,
    fromSelectIntoTempTable,
    dropTempTableQuery,
    Printer (..),
  )
where

import Data.Aeson (ToJSON (..))
import Data.HashMap.Strict qualified as HM
import Data.List (intersperse)
import Data.String
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Builder qualified as L
import Database.ODBC.SQLServer
import Hasura.Backends.MSSQL.Types
import Hasura.Prelude hiding (GT, LT)

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
(<+>) x y = SeqPrinter [x, y]

(<+>?) :: Printer -> Maybe Printer -> Printer
(<+>?) x Nothing = x
(<+>?) x (Just y) = SeqPrinter [x, y]

(?<+>) :: Maybe Printer -> Printer -> Printer
(?<+>) Nothing x = x
(?<+>) (Just x) y = SeqPrinter [x, y]

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
      "CAST(" <+> fromExpression e
        <+> " AS "
        <+> fromString (T.unpack t)
        <+> ")"
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
    NotExpression expression -> "NOT " <+> fromExpression expression
    ExistsExpression sel -> "EXISTS (" <+> fromSelect sel <+> ")"
    IsNullExpression expression ->
      "(" <+> fromExpression expression <+> ") IS NULL"
    IsNotNullExpression expression ->
      "(" <+> fromExpression expression <+> ") IS NOT NULL"
    ColumnExpression fieldName -> fromFieldName fieldName
    ToStringExpression e -> "CONCAT(" <+> fromExpression e <+> ", '')"
    SelectExpression s -> "(" <+> IndentPrinter 1 (fromSelect s) <+> ")"
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
    FunctionExpression function args ->
      fromString (T.unpack function)
        <+> "("
        <+> SepByPrinter ", " (map fromExpression args)
        <+> ")"
    ListExpression xs -> SepByPrinter ", " $ fromExpression <$> xs
    STOpExpression op e str ->
      "(" <+> fromExpression e <+> ")."
        <+> fromString (show op)
        <+> "("
        <+> fromExpression str
        <+> ") = 1"
    ConditionalExpression condition trueExpression falseExpression ->
      "(CASE WHEN("
        <+> fromExpression condition
        <+> ") THEN "
        <+> fromExpression trueExpression
        <+> " ELSE "
        <+> fromExpression falseExpression
        <+> " END)"
    DefaultExpression -> "DEFAULT"

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

fromPath :: JsonPath -> Printer
fromPath path =
  ", " <+> string path
  where
    string =
      fromExpression
        . ValueExpression
        . TextValue
        . L.toStrict
        . L.toLazyText
        . go
    go =
      \case
        RootPath -> "$"
        IndexPath r i -> go r <> "[" <> L.fromString (show i) <> "]"
        FieldPath r f -> go r <> ".\"" <> L.fromText f <> "\""

fromFieldName :: FieldName -> Printer
fromFieldName (FieldName {..}) =
  fromNameText fieldNameEntity <+> "." <+> fromNameText fieldName

fromInserted :: Inserted -> Printer
fromInserted Inserted = "INSERTED"

fromDeleted :: Deleted -> Printer
fromDeleted Deleted = "DELETED"

fromOutputColumn :: Printer -> OutputColumn -> Printer
fromOutputColumn prefix (OutputColumn columnName) =
  prefix <+> "." <+> fromNameText (columnNameText columnName)

fromOutput :: (t -> Printer) -> Output t -> Printer
fromOutput typePrinter (Output ty outputColumns) =
  "OUTPUT " <+> SepByPrinter ", " (map (fromOutputColumn (typePrinter ty)) outputColumns)

fromInsertOutput :: InsertOutput -> Printer
fromInsertOutput = fromOutput fromInserted

fromDeleteOutput :: DeleteOutput -> Printer
fromDeleteOutput = fromOutput fromDeleted

fromUpdateOutput :: UpdateOutput -> Printer
fromUpdateOutput = fromOutput fromInserted

fromValues :: Values -> Printer
fromValues (Values values) =
  "( " <+> SepByPrinter ", " (map fromExpression values) <+> " )"

fromInsert :: Insert -> Printer
fromInsert Insert {..} =
  SepByPrinter
    NewlinePrinter
    [ "INSERT INTO " <+> fromTableName insertTable,
      "(" <+> SepByPrinter ", " (map (fromNameText . columnNameText) insertColumns) <+> ")",
      fromInsertOutput insertOutput,
      "VALUES " <+> SepByPrinter ", " (map fromValues insertValues)
    ]

fromSetValue :: SetValue -> Printer
fromSetValue = \case
  SetON -> "ON"
  SetOFF -> "OFF"

fromSetIdentityInsert :: SetIdentityInsert -> Printer
fromSetIdentityInsert SetIdentityInsert {..} =
  SepByPrinter
    " "
    [ "SET IDENTITY_INSERT",
      fromTableName setTable,
      fromSetValue setValue
    ]

-- | Generate a delete statement
--
-- > Delete
-- >   (Aliased (TableName "table" "schema") "alias")
-- >   [ColumnName "id", ColumnName "name"]
-- >   (Where [OpExpression EQ' (ValueExpression (IntValue 1)) (ValueExpression (IntValue 1))])
--
-- Becomes:
--
-- > DELETE [alias] OUTPUT DELETED.[id], DELETED.[name] INTO #deleted([id], [name]) FROM [schema].[table] AS [alias] WHERE ((1) = (1))
fromDelete :: Delete -> Printer
fromDelete Delete {deleteTable, deleteOutput, deleteTempTable, deleteWhere} =
  SepByPrinter
    NewlinePrinter
    [ "DELETE " <+> fromNameText (aliasedAlias deleteTable),
      fromDeleteOutput deleteOutput,
      "INTO " <+> fromTempTable deleteTempTable,
      "FROM " <+> fromAliased (fmap fromTableName deleteTable),
      fromWhere deleteWhere
    ]

-- | Generate an update statement
--
-- > Update
-- >    (Aliased (TableName "table" "schema") "alias")
-- >    (fromList [(ColumnName "name", ValueExpression (TextValue "updated_name"))])
-- >    (Output Inserted)
-- >    (TempTable (TempTableName "updated") [ColumnName "id", ColumnName "name"])
-- >    (Where [OpExpression EQ' (ColumnName "id") (ValueExpression (IntValue 1))])
--
-- Becomes:
--
-- > UPDATE [alias] SET [alias].[name] = 'updated_name' OUTPUT INSERTED.[id], INSERTED.[name] INTO
-- > #updated([id], [name]) FROM [schema].[table] AS [alias] WHERE (id = 1)
fromUpdate :: Update -> Printer
fromUpdate Update {..} =
  SepByPrinter
    NewlinePrinter
    [ "UPDATE " <+> fromNameText (aliasedAlias updateTable),
      fromUpdateSet updateSet,
      fromUpdateOutput updateOutput,
      "INTO " <+> fromTempTable updateTempTable,
      "FROM " <+> fromAliased (fmap fromTableName updateTable),
      fromWhere updateWhere
    ]
  where
    fromUpdateSet :: UpdateSet -> Printer
    fromUpdateSet setColumns =
      let updateColumnValue (column, updateOp) =
            fromColumnName column <+> fromUpdateOperator (fromExpression <$> updateOp)
       in "SET " <+> SepByPrinter ", " (map updateColumnValue (HM.toList setColumns))

    fromUpdateOperator :: UpdateOperator Printer -> Printer
    fromUpdateOperator = \case
      UpdateSet p -> " = " <+> p
      UpdateInc p -> " += " <+> p

-- | Converts `SelectIntoTempTable`.
--
--  > SelectIntoTempTable (TempTableName "deleted")  [UnifiedColumn "id" IntegerType, UnifiedColumn "name" TextType] (TableName "table" "schema")
--
--  Becomes:
--
--  > SELECT [id], [name] INTO #deleted([id], [name]) FROM [schema].[table] WHERE (1<>1) UNION ALL SELECT [id], [name] FROM [schema].[table];
--
--  We add the `UNION ALL` part to avoid copying identity constraints, and we cast columns with types such as `timestamp`
--  which are non-insertable to a different type.
fromSelectIntoTempTable :: SelectIntoTempTable -> Printer
fromSelectIntoTempTable SelectIntoTempTable {sittTempTableName, sittColumns, sittFromTableName} =
  SepByPrinter
    NewlinePrinter
    [ "SELECT "
        <+> columns,
      "INTO " <+> fromTempTableName sittTempTableName,
      "FROM " <+> fromTableName sittFromTableName,
      "WHERE " <+> falsePrinter,
      "UNION ALL SELECT " <+> columns,
      "FROM " <+> fromTableName sittFromTableName,
      "WHERE " <+> falsePrinter
    ]
  where
    -- column names separated by commas
    columns =
      SepByPrinter
        ("," <+> NewlinePrinter)
        (map columnNameFromUnifiedColumn sittColumns)

    -- column name with potential modifications of types
    columnNameFromUnifiedColumn (UnifiedColumn columnName columnType) =
      case columnType of
        -- The "timestamp" is type synonym for "rowversion" and it is just an incrementing number and does not preserve a date or a time.
        -- So, the "timestamp" type is neither insertable nor explicitly updatable. Its values are unique binary numbers within a database.
        -- We're using "binary" type instead so that we can copy a timestamp row value safely into the temporary table.
        -- See https://docs.microsoft.com/en-us/sql/t-sql/data-types/rowversion-transact-sql for more details.
        TimestampType -> "CAST(" <+> fromNameText columnName <+> " AS binary(8)) AS " <+> fromNameText columnName
        _ -> fromNameText columnName

-- | @TempTableName "deleted"@ becomes @\#deleted@
fromTempTableName :: TempTableName -> Printer
fromTempTableName (TempTableName v) = QueryPrinter (fromString . T.unpack $ "#" <> v)

fromTempTable :: TempTable -> Printer
fromTempTable (TempTable table columns) =
  fromTempTableName table <+> parens (SepByPrinter ", " (map fromColumnName columns))

-- | @TempTableName "temp_table" is converted to "DROP TABLE #temp_table"
dropTempTableQuery :: TempTableName -> Printer
dropTempTableQuery tempTableName =
  QueryPrinter "DROP TABLE " <+> fromTempTableName tempTableName

fromSelect :: Select -> Printer
fromSelect Select {..} = fmap fromWith selectWith ?<+> wrapFor selectFor result
  where
    result =
      SepByPrinter
        NewlinePrinter
        $ [ "SELECT "
              <+> IndentPrinter
                7
                ( SepByPrinter
                    ("," <+> NewlinePrinter)
                    (map fromProjection (toList selectProjections))
                )
          ]
          <> ["FROM " <+> IndentPrinter 5 (fromFrom f) | Just f <- [selectFrom]]
          <> [ SepByPrinter
                 NewlinePrinter
                 ( map
                     ( \Join {..} ->
                         SeqPrinter
                           [ "OUTER APPLY (",
                             IndentPrinter 13 (fromJoinSource joinSource),
                             ") ",
                             NewlinePrinter,
                             "AS ",
                             fromJoinAlias joinJoinAlias
                           ]
                     )
                     selectJoins
                 ),
               fromWhere selectWhere,
               fromOrderBys selectTop selectOffset selectOrderBy,
               fromFor selectFor
             ]

fromWith :: With -> Printer
fromWith (With withSelects) =
  "WITH " <+> SepByPrinter ", " (map fromAliasedSelect (toList withSelects)) <+> NewlinePrinter
  where
    fromAliasedSelect Aliased {..} =
      fromNameText aliasedAlias <+> " AS " <+> "( " <+> fromSelect aliasedThing <+> " )"

fromJoinSource :: JoinSource -> Printer
fromJoinSource =
  \case
    JoinSelect sel -> fromSelect sel
    JoinReselect reselect -> fromReselect reselect

fromReselect :: Reselect -> Printer
fromReselect Reselect {..} = wrapFor reselectFor result
  where
    result =
      SepByPrinter
        NewlinePrinter
        [ "SELECT "
            <+> IndentPrinter
              7
              ( SepByPrinter
                  ("," <+> NewlinePrinter)
                  (map fromProjection (toList reselectProjections))
              ),
          fromWhere reselectWhere,
          fromFor reselectFor
        ]

fromOrderBys ::
  Top -> Maybe Expression -> Maybe (NonEmpty OrderBy) -> Printer
fromOrderBys NoTop Nothing Nothing = "" -- An ORDER BY is wasteful if not needed.
fromOrderBys top moffset morderBys =
  SeqPrinter
    [ "ORDER BY ",
      IndentPrinter
        9
        ( SepByPrinter
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
                    (concatMap fromOrderBy (toList orderBys)),
              case (top, moffset) of
                (NoTop, Nothing) -> ""
                (NoTop, Just offset) ->
                  "OFFSET " <+> fromExpression offset <+> " ROWS"
                (Top n, Nothing) ->
                  "OFFSET 0 ROWS FETCH NEXT "
                    <+> QueryPrinter (toSql (IntValue n))
                    <+> " ROWS ONLY"
                (Top n, Just offset) ->
                  "OFFSET "
                    <+> fromExpression offset
                    <+> " ROWS FETCH NEXT "
                    <+> QueryPrinter (toSql (IntValue n))
                    <+> " ROWS ONLY"
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
    wrapNullHandling inner =
      case orderByType of
        Just TextType -> castTextish inner
        Just WtextType -> castTextish inner
        -- Above: For some types, we have to do null handling manually
        -- ourselves:
        _ -> inner
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
    AscOrder -> "ASC"
    DescOrder -> "DESC"

fromNullsOrder :: FieldName -> NullsOrder -> Printer
fromNullsOrder fieldName =
  \case
    NullsAnyOrder -> ""
    NullsFirst -> "IIF(" <+> fromFieldName fieldName <+> " IS NULL, 0, 1)"
    NullsLast -> "IIF(" <+> fromFieldName fieldName <+> " IS NULL, 1, 0)"

fromJoinAlias :: JoinAlias -> Printer
fromJoinAlias JoinAlias {..} =
  fromNameText joinAliasEntity
    <+>? fmap (\name -> "(" <+> fromNameText name <+> ")") joinAliasField

fromFor :: For -> Printer
fromFor =
  \case
    NoFor -> ""
    JsonFor ForJson {jsonCardinality} ->
      "FOR JSON PATH, INCLUDE_NULL_VALUES"
        <+> case jsonCardinality of
          JsonArray -> ""
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
    OpAggregate op args ->
      QueryPrinter (rawUnescapedText op)
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
    FromIdentifier identifier -> fromNameText identifier
    FromTempTable aliasedTempTable -> fromAliased (fmap fromTempTableName aliasedTempTable)

fromOpenJson :: OpenJson -> Printer
fromOpenJson OpenJson {openJsonExpression, openJsonWith} =
  SepByPrinter
    NewlinePrinter
    [ "OPENJSON("
        <+> IndentPrinter 9 (fromExpression openJsonExpression)
        <+> ")",
      case openJsonWith of
        Nothing -> ""
        Just openJsonWith' ->
          "WITH ("
            <+> IndentPrinter
              5
              ( SepByPrinter
                  ("," <+> NewlinePrinter)
                  (fromJsonFieldSpec <$> toList openJsonWith')
              )
            <+> ")"
    ]

fromJsonFieldSpec :: JsonFieldSpec -> Printer
fromJsonFieldSpec =
  \case
    IntField name mPath -> fromNameText name <+> " INT" <+> quote mPath
    StringField name mPath -> fromNameText name <+> " NVARCHAR(MAX)" <+> quote mPath
    UuidField name mPath -> fromNameText name <+> " UNIQUEIDENTIFIER" <+> quote mPath
    JsonField name mPath -> fromJsonFieldSpec (StringField name mPath) <+> " AS JSON"
  where
    quote mPath = maybe "" ((\p -> " '" <+> p <+> "'") . go) mPath
    go = \case
      RootPath -> "$"
      IndexPath r i -> go r <+> "[" <+> fromString (show i) <+> "]"
      FieldPath r f -> go r <+> ".\"" <+> fromString (T.unpack f) <+> "\""

fromTableName :: TableName -> Printer
fromTableName TableName {tableName, tableSchema} =
  fromNameText tableSchema <+> "." <+> fromNameText tableName

fromAliased :: Aliased Printer -> Printer
fromAliased Aliased {..} =
  aliasedThing
    <+> ((" AS " <+>) . fromNameText) aliasedAlias

fromColumnName :: ColumnName -> Printer
fromColumnName (ColumnName colname) = fromNameText colname

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
wrapFor for' inner = coalesceNull
  where
    coalesceNull =
      case for' of
        NoFor -> rooted
        JsonFor forJson ->
          SeqPrinter
            [ "SELECT ISNULL((",
              rooted,
              "), '",
              emptyarrayOrNull forJson,
              "')"
            ]
    emptyarrayOrNull ForJson {..} =
      case jsonCardinality of
        JsonSingleton -> "null"
        JsonArray -> "[]"
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
                [ fromString ("SELECT CONCAT('{" <> show text <> ":', ("),
                  inner,
                  "), '}')"
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
