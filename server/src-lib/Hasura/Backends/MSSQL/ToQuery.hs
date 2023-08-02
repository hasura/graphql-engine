{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL ToQuery
--
-- Convert the simple T-SQL AST to an SQL query, ready to be passed to the odbc
-- package's query/exec functions.
--
-- We define a custom prettyprinter with the type 'Printer'.
--
-- If you'd like to trace and see what a 'Printer' looks like as SQL, you can use something like:
-- > ltraceM "sql" (ODBC.renderQuery (toQueryPretty myPrinter))
module Hasura.Backends.MSSQL.ToQuery
  ( fromSelect,
    fromReselect,
    toQueryFlat,
    toQueryPretty,
    fromInsert,
    fromMerge,
    fromTempTableDDL,
    fromSetIdentityInsert,
    fromDelete,
    fromUpdate,
    fromSelectIntoTempTable,
    fromInsertValuesIntoTempTable,
    dropTempTableQuery,
    fromRawUnescapedText,
    fromTableName,
    (<+>),
    Printer (..),
  )
where

import Data.Aeson (ToJSON (..))
import Data.HashMap.Strict qualified as HashMap
import Data.List (intersperse)
import Data.String
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Builder qualified as L
import Database.ODBC.SQLServer
import Hasura.Backends.MSSQL.Types
import Hasura.NativeQuery.Metadata (InterpolatedItem (..), InterpolatedQuery (..))
import Hasura.Prelude hiding (GT, LT)

--------------------------------------------------------------------------------

-- * Types

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

-- * Instances

-- This is a debug instance, only here because it avoids a circular
-- dependency between this module and Types/Instances.
instance ToJSON Expression where
  toJSON = toJSON . T.toTxt . toQueryFlat . fromExpression

--------------------------------------------------------------------------------

-- * Printer generators

fromExpression :: Expression -> Printer
fromExpression =
  \case
    CastExpression e t dataLength ->
      "CAST("
        <+> fromExpression e
        <+> " AS "
        <+> fromString (T.unpack $ scalarTypeDBName dataLength t)
        <+> ")"
    JsonQueryExpression e -> "JSON_QUERY(" <+> fromExpression e <+> ")"
    JsonValueExpression e path ->
      "JSON_VALUE(" <+> fromExpression e <+> fromPath path <+> ")"
    ValueExpression value -> QueryPrinter $ toSql value
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
    OpExpression op x y ->
      "("
        <+> fromExpression x
        <+> ") "
        <+> fromOp op
        <+> " ("
        <+> fromExpression y
        <+> ")"
    MethodApplicationExpression ex methodAppExp -> fromMethodApplicationExpression ex methodAppExp
    FunctionApplicationExpression funAppExp -> fromFunctionApplicationExpression funAppExp
    ListExpression xs -> SepByPrinter ", " $ fromExpression <$> xs
    STOpExpression op e str ->
      "("
        <+> fromExpression e
        <+> ")."
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

fromMethodApplicationExpression :: Expression -> MethodApplicationExpression -> Printer
fromMethodApplicationExpression ex methodAppExp =
  case methodAppExp of
    MethExpSTAsText -> fromApp "STAsText" []
  where
    fromApp :: Text -> [Expression] -> Printer
    fromApp method args =
      fromExpression ex
        <+> "."
        <+> fromString (T.unpack method)
        <+> "("
        <+> SeqPrinter (map fromExpression args)
        <+> ")"

fromFunctionApplicationExpression :: FunctionApplicationExpression -> Printer
fromFunctionApplicationExpression funAppExp = case funAppExp of
  (FunExpISNULL x y) -> fromApp "ISNULL" [x, y]
  where
    fromApp :: Text -> [Expression] -> Printer
    fromApp function args =
      fromString (T.unpack function)
        <+> "("
        <+> SepByPrinter ", " (map fromExpression args)
        <+> ")"

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

fromValuesList :: [Values] -> Printer
fromValuesList valuesList =
  "VALUES " <+> SepByPrinter ", " (map fromValues valuesList)

fromInsert :: Insert -> Printer
fromInsert Insert {..} =
  SepByPrinter
    NewlinePrinter
    $ ["INSERT INTO " <+> fromTableName insertTable]
    <> [ "(" <+> SepByPrinter ", " (map (fromNameText . columnNameText) insertColumns) <+> ")"
         | not (null insertColumns)
       ]
    <> [ fromInsertOutput insertOutput,
         "INTO " <+> fromTempTable insertTempTable,
         if null insertColumns
           then "VALUES " <+> SepByPrinter ", " (map (const "(DEFAULT)") insertValues)
           else fromValuesList insertValues
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
      tableName,
      fromSetValue setValue
    ]
  where
    tableName =
      case setTable of
        RegularTableName name -> fromTableName name
        TemporaryTableName name -> fromTempTableName name

-- | Generate a statement to insert values into temporary table.
fromInsertValuesIntoTempTable :: InsertValuesIntoTempTable -> Printer
fromInsertValuesIntoTempTable InsertValuesIntoTempTable {..} =
  SepByPrinter
    NewlinePrinter
    [ "INSERT INTO " <+> fromTempTableName ivittTempTableName,
      "(" <+> SepByPrinter ", " (map (fromNameText . columnNameText) ivittColumns) <+> ")",
      fromValuesList ivittValues
    ]

-- | Alias for the source table in a MERGE statement. Used when pretty printing MERGE statments.
mergeSourceAlias :: Text
mergeSourceAlias = "source"

-- | Alias for the target table in a MERGE statement. Used when pretty printing MERGE statments.
mergeTargetAlias :: Text
mergeTargetAlias = "target"

-- | USING section of a MERGE statement. Used in 'fromMerge'.
fromMergeUsing :: MergeUsing -> Printer
fromMergeUsing MergeUsing {..} =
  "USING (" <+> fromSelect selectSubQuery <+> ") AS " <+> fromNameText mergeSourceAlias
  where
    selectSubQuery :: Select
    selectSubQuery =
      let alias = "merge_temptable"
          columnNameToProjection ColumnName {columnNameText} =
            -- merge_temptable.column_name AS column_name
            FieldNameProjection
              $ Aliased
                { aliasedThing = FieldName columnNameText alias,
                  aliasedAlias = columnNameText
                }
       in emptySelect
            { selectProjections = map columnNameToProjection mergeUsingColumns,
              selectFrom = Just (FromTempTable $ Aliased mergeUsingTempTable alias) -- FROM temp_table AS merge_temptable
            }

-- | ON section of a MERGE statement. Used in 'fromMerge'.
fromMergeOn :: MergeOn -> Printer
fromMergeOn MergeOn {..} =
  "ON (" <+> onExpression <+> ")"
  where
    onExpression
      | null mergeOnColumns =
          falsePrinter
      | otherwise =
          (fromExpression . AndExpression) (map matchColumn mergeOnColumns)

    matchColumn :: ColumnName -> Expression
    matchColumn ColumnName {..} =
      let sourceColumn = ColumnExpression $ FieldName columnNameText mergeSourceAlias
          targetColumn = ColumnExpression $ FieldName columnNameText mergeTargetAlias
       in OpExpression EQ' sourceColumn targetColumn

-- | WHEN MATCHED section of a MERGE statement. Used in 'fromMerge'.
fromMergeWhenMatched :: MergeWhenMatched -> Printer
fromMergeWhenMatched (MergeWhenMatched updateColumns updateCondition updatePreset) =
  if null updates
    then ""
    else
      "WHEN MATCHED AND "
        <+> fromExpression updateCondition
        <+> " THEN UPDATE "
        <+> fromUpdateSet updates
  where
    updates = updateSet <> HashMap.map UpdateSet updatePreset

    updateSet :: UpdateSet
    updateSet =
      HashMap.fromList
        $ map
          ( \cn@ColumnName {..} ->
              ( cn,
                UpdateSet $ ColumnExpression $ FieldName columnNameText mergeSourceAlias
              )
          )
          updateColumns

-- | WHEN NOT MATCHED section of a MERGE statement. Used in 'fromMerge'.
fromMergeWhenNotMatched :: MergeWhenNotMatched -> Printer
fromMergeWhenNotMatched (MergeWhenNotMatched insertColumns) =
  SepByPrinter
    NewlinePrinter
    [ "WHEN NOT MATCHED THEN INSERT (" <+> SepByPrinter ", " (map fromColumnName insertColumns) <+> ")",
      fromValuesList [Values columnsFromSource]
    ]
  where
    columnsFromSource =
      insertColumns <&> \ColumnName {..} -> ColumnExpression $ FieldName columnNameText mergeSourceAlias

-- | Generate a MERGE SQL statement
fromMerge :: Merge -> Printer
fromMerge Merge {..} =
  SepByPrinter
    NewlinePrinter
    [ "MERGE " <+> fromAliased (fmap fromTableName mergeTableAsTarget),
      fromMergeUsing mergeUsing,
      fromMergeOn mergeOn,
      fromMergeWhenMatched mergeWhenMatched,
      fromMergeWhenNotMatched mergeWhenNotMatched,
      fromInsertOutput mergeInsertOutput,
      "INTO " <+> fromTempTable mergeOutputTempTable,
      ";" -- Always, a Merge statement should end with a ";"
    ]
  where
    mergeTableAsTarget :: Aliased TableName
    mergeTableAsTarget = Aliased mergeTargetTable mergeTargetAlias

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

fromUpdateSet :: UpdateSet -> Printer
fromUpdateSet setColumns =
  let updateColumnValue (column, updateOp) =
        fromColumnName column <+> fromUpdateOperator (fromExpression <$> updateOp)
   in "SET " <+> SepByPrinter ", " (map updateColumnValue (HashMap.toList setColumns))
  where
    fromUpdateOperator :: UpdateOperator Printer -> Printer
    fromUpdateOperator = \case
      UpdateSet p -> " = " <+> p
      UpdateInc p -> " += " <+> p

fromTempTableDDL :: TempTableDDL -> Printer
fromTempTableDDL = \case
  TempTableCreate tempTableName tempColumns ->
    "CREATE TABLE "
      <+> fromTempTableName tempTableName
      <+> " ( "
      <+> columns
      <+> " ) "
    where
      columns =
        SepByPrinter
          ("," <+> NewlinePrinter)
          (map columnNameAndType tempColumns)
      columnNameAndType (UnifiedColumn name ty) =
        fromColumnName name
          <+> " "
          <+> fromString (T.unpack (scalarTypeDBName DataLengthMax ty))
          <+> " null"
  TempTableInsert tempTableName declares interpolatedQuery ->
    SepByPrinter
      NewlinePrinter
      ( map fromDeclare declares
          <> [ "INSERT INTO "
                 <+> fromTempTableName tempTableName
                 <+> " "
                 <+> renderInterpolatedQuery interpolatedQuery
             ]
      )
  TempTableDrop tempTableName ->
    "DROP TABLE "
      <+> fromTempTableName tempTableName

fromDeclare :: Declare -> Printer
fromDeclare (Declare dName dType dValue) =
  SepByPrinter
    NewlinePrinter
    [ "DECLARE @" <+> fromRawUnescapedText dName <+> " " <+> fromRawUnescapedText (scalarTypeDBName DataLengthMax dType) <+> ";",
      "SET @" <+> fromRawUnescapedText dName <+> " = " <+> fromExpression dValue <+> ";"
    ]

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
fromSelectIntoTempTable SelectIntoTempTable {sittTempTableName, sittColumns, sittFromTableName, sittConstraints} =
  SepByPrinter
    NewlinePrinter
    $ [ "SELECT "
          <+> columns,
        "INTO " <+> fromTempTableName sittTempTableName,
        "FROM " <+> fromTableName sittFromTableName,
        "WHERE " <+> falsePrinter
      ]
    <> case sittConstraints of
      RemoveConstraints ->
        [ "UNION ALL SELECT " <+> columns,
          "FROM " <+> fromTableName sittFromTableName,
          "WHERE " <+> falsePrinter
        ]
      KeepConstraints ->
        []
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
        TimestampType -> "CAST(" <+> fromColumnName columnName <+> " AS binary(8)) AS " <+> fromColumnName columnName
        _ -> fromColumnName columnName

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
fromSelect Select {..} = fmap fromWith selectWith ?<+> result
  where
    allWheres = selectWhere <> mconcat (joinWhere <$> selectJoins)
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
             fromWhere allWheres,
             fromOrderBys selectTop selectOffset selectOrderBy,
             fromFor selectFor
           ]

fromWith :: With -> Printer
fromWith (With withSelects) =
  "WITH " <+> SepByPrinter ", " (map fromAliasedSelect (toList withSelects)) <+> NewlinePrinter
  where
    fromAliasedSelect (Aliased {..}) =
      fromNameText aliasedAlias
        <+> " AS "
        <+> "( "
        <+> ( case aliasedThing of
                CTESelect select ->
                  fromSelect select
                CTEUnsafeRawSQL nativeQuery ->
                  renderInterpolatedQuery nativeQuery
            )
        <+> " )"

renderInterpolatedQuery :: InterpolatedQuery Expression -> Printer
renderInterpolatedQuery = foldr (<+>) "" . renderedParts
  where
    renderedParts :: InterpolatedQuery Expression -> [Printer]
    renderedParts (InterpolatedQuery parts) =
      ( \case
          IIText t -> fromRawUnescapedText t
          IIVariable v -> fromExpression v
      )
        <$> parts

fromJoinSource :: JoinSource -> Printer
fromJoinSource =
  \case
    JoinSelect sel -> fromSelect sel
    JoinReselect reselect -> fromReselect reselect

fromReselect :: Reselect -> Printer
fromReselect Reselect {..} = result
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
  [ fromNullsOrder orderByExpression orderByNullsOrder,
    -- Above: This doesn't do anything when using text, ntext or image
    -- types. See below on CAST commentary.
    wrapNullHandling (fromExpression orderByExpression)
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

fromNullsOrder :: Expression -> NullsOrder -> Printer
fromNullsOrder ex =
  \case
    NullsAnyOrder -> ""
    NullsFirst -> "IIF(" <+> fromExpression ex <+> " IS NULL, 0, 1)"
    NullsLast -> "IIF(" <+> fromExpression ex <+> " IS NULL, 1, 0)"

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

fromCountable :: Countable Expression -> Printer
fromCountable =
  \case
    StarCountable -> "*"
    NonNullFieldCountable field -> fromExpression field
    DistinctCountable field -> "DISTINCT " <+> fromExpression field

fromWhere :: Where -> Printer
fromWhere =
  \case
    Where expressions
      | Just whereExp <- collapseWhere (AndExpression expressions) ->
          "WHERE " <+> IndentPrinter 6 (fromExpression whereExp)
      | otherwise -> ""

-- | Drop useless examples like this from the output:
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
    StringField name mPath -> fromNameText name <+> " NVARCHAR(MAX)" <+> quote mPath
    JsonField name mPath -> fromJsonFieldSpec (StringField name mPath) <+> " AS JSON"
    ScalarField fieldType fieldLength name mPath ->
      fromNameText name
        <+> " "
        <+> fromString (T.unpack $ scalarTypeDBName fieldLength fieldType)
        <+> quote mPath
  where
    quote mPath = maybe "" ((\p -> " '" <+> p <+> "'") . go) mPath
    go = \case
      RootPath -> "$"
      IndexPath r i -> go r <+> "[" <+> fromString (show i) <+> "]"
      FieldPath r f -> go r <+> ".\"" <+> fromString (T.unpack f) <+> "\""

fromTableName :: TableName -> Printer
fromTableName (TableName tableName (SchemaName tableSchema)) =
  fromNameText tableSchema <+> "." <+> fromNameText tableName

fromAliased :: Aliased Printer -> Printer
fromAliased Aliased {..} =
  aliasedThing
    <+> ((" AS " <+>) . fromNameText) aliasedAlias

fromColumnName :: ColumnName -> Printer
fromColumnName (ColumnName colname) = quoteIdentifier colname

fromNameText :: Text -> Printer
fromNameText = quoteIdentifier

fromRawUnescapedText :: Text -> Printer
fromRawUnescapedText t = QueryPrinter (rawUnescapedText t)

-- | In Sql Server identifiers can be quoted using square brackets or double
-- quotes, "Delimited Identifiers" in T-SQL parlance, which gives full freedom
-- in what can syntactically constitute a name of a thing.
--
-- The delimiting characters may themselves appear in a delimited identifier,
-- in which case they are quoted by duplication of the terminal delimiter. This
-- is the only character escaping that happens within a delimited identifier.
--
-- (TODO: That fact does not seem to be documented anywhere I could find, but
-- seems to be folklore. I verified it myself at any rate)
--
-- Reference: https://learn.microsoft.com/en-us/sql/relational-databases/databases/database-identifiers?view=sql-server-ver16
quoteIdentifier :: Text -> Printer
quoteIdentifier ident = QueryPrinter (rawUnescapedText ("[" <> duplicateBrackets ident <> "]"))
  where
    duplicateBrackets :: Text -> Text
    duplicateBrackets = T.replace "]" "]]"

truePrinter :: Printer
truePrinter = "(1=1)"

falsePrinter :: Printer
falsePrinter = "(1<>1)"

parens :: Printer -> Printer
parens p = "(" <+> IndentPrinter 1 p <+> ")"

--------------------------------------------------------------------------------

-- * Basic printing API

-- | Pretty-prints a 'Printer' as one line, converting 'NewlinePrinter' to space.
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

-- | Pretty-prints a 'Printer' as multiple lines as defined by the printer.
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
