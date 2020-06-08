{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Hasura.GraphQL.Schema.Select
  ( selectTable
  , selectTableByPk
  , selectTableAggregate
  , tableSelectionSet
  ) where


import           Hasura.Prelude

import           Data.Either                           (partitionEithers)
import           Data.Foldable                         (toList)
import           Data.Maybe                            (fromJust)
import           Data.Parser.JSONPath
import           Data.Traversable                      (mapAccumL)

import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashSet                          as Set
import qualified Data.Sequence                         as Seq
import qualified Data.Text                             as T
import qualified Language.GraphQL.Draft.Syntax         as G

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.RQL.DML.Select                 as RQL
import qualified Hasura.SQL.DML                        as SQL

import           Hasura.GraphQL.Parser                 (FieldParser, InputFieldsParser, Kind (..),
                                                        Parser, UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column          (qualifiedObjectToName)
import           Hasura.GraphQL.Parser.Internal.Parser as P
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value


type SelectExp       = RQL.AnnSimpleSelG UnpreparedValue
type AggSelectExp    = RQL.AnnAggSelG UnpreparedValue
type TableArgs       = RQL.TableArgsG UnpreparedValue
type TablePerms      = RQL.TablePermG UnpreparedValue
type AnnotatedFields = RQL.AnnFldsG UnpreparedValue
type AnnotatedField  = RQL.AnnFldG UnpreparedValue



-- 1. top level selection functions
-- write a blurb?

-- | Simple table selection.
--
-- The field for the table accepts table selection arguments, and
-- expects a selection of fields
--
-- > table_name(limit: 10) {
-- >   col1: col1_type
-- >   col2: col2_type
-- > }: [table!]
--
-- Returns Nothing if there is nothing that can be selected
-- with current permissions.
selectTable
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (Maybe (FieldParser n SelectExp))
selectTable table fieldName description selectPermissions stringifyNum = runMaybeT do
  tableArgsParser    <- lift $ tableArgs table selectPermissions
  selectionSetParser <- lift $ tableSelectionSet table selectPermissions stringifyNum
  pure $ P.subselection fieldName description tableArgsParser selectionSetParser
    <&> \(args, fields) -> RQL.AnnSelG
      { RQL._asnFields   = fields
      , RQL._asnFrom     = RQL.FromTable table
      , RQL._asnPerm     = tablePermissionsInfo selectPermissions
      , RQL._asnArgs     = args
      , RQL._asnStrfyNum = stringifyNum
      }

-- | Table selection by primary key.
--
-- > table_name(id: 42) {
-- >   col1: col1_type
-- >   col2: col2_type
-- > }: [table!]
--
-- Returns Nothing if there's nothing that can be selected with
-- current permissions or if there are primary keys the user
-- doesn't have select permissions for.
selectTableByPk
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (Maybe (FieldParser n SelectExp))
selectTableByPk table fieldName description selectPermissions stringifyNum = runMaybeT do
  primaryKeys <- MaybeT $ fmap _pkColumns . _tciPrimaryKey . _tiCoreInfo <$> askTableInfo table
  guard $ all (\c -> pgiColumn c `Set.member` spiCols selectPermissions) primaryKeys
  argsParser <- lift $ sequenceA <$> for primaryKeys \columnInfo -> do
    field <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
    pure $ BoolFld . AVCol columnInfo . pure . AEQ True . mkParameter <$>
      P.field (pgiName columnInfo) (pgiDescription columnInfo) field
  selectionSetParser <- lift $ tableSelectionSet table selectPermissions stringifyNum
  pure $ P.subselection fieldName description argsParser selectionSetParser
    <&> \(boolExpr, fields) ->
      let defaultPerms = tablePermissionsInfo selectPermissions
          whereExpr    = Just $ BoolAnd $ toList boolExpr
      in RQL.AnnSelG
           { RQL._asnFields   = fields
           , RQL._asnFrom     = RQL.FromTable table
           , RQL._asnPerm     = defaultPerms { RQL._tpLimit = Nothing }
             -- TODO: check whether this is necessary:        ^^^^^^^
             -- This is how it was in legacy code.
           , RQL._asnArgs     = RQL.noTableArgs { RQL._taWhere = whereExpr }
           , RQL._asnStrfyNum = stringifyNum
           }

-- | Table aggregation selection
--
-- Parser for an aggregation selection of a table.
-- > table_aggregate(limit: 10) {
-- >   aggregate: table_aggregate_fields
-- >   nodes: [table!]!
-- > } :: table_aggregate
--
-- Returns Nothing if there's nothing that can be selected with
-- current permissions.
selectTableAggregate
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (Maybe (FieldParser n AggSelectExp))
selectTableAggregate table fieldName description selectPermissions stringifyNum = runMaybeT do
  guard $ spiAllowAgg selectPermissions
  tableArgsParser <- lift $ tableArgs table selectPermissions
  aggregateParser <- lift $ tableAggregationFields table selectPermissions
  selectionName   <- lift $ qualifiedObjectToName table <&> (<> $$(G.litName "_aggregate"))
  nodesParser     <- lift $ tableSelectionSet table selectPermissions stringifyNum
  let aggregationParser = parsedSelectionsToFields RQL.TAFExp <$>
        P.selectionSet selectionName Nothing
        [ RQL.TAFNodes <$> P.subselection_ $$(G.litName "nodes") Nothing nodesParser
        , RQL.TAFAgg <$> P.subselection_ $$(G.litName "aggregate") Nothing aggregateParser
        ]
  pure $ P.subselection fieldName description tableArgsParser aggregationParser
    <&> \(args, fields) -> RQL.AnnSelG
      { RQL._asnFields   = fields
      , RQL._asnFrom     = RQL.FromTable table
      , RQL._asnPerm     = tablePermissionsInfo selectPermissions
      , RQL._asnArgs     = args
      , RQL._asnStrfyNum = stringifyNum
      }

{- Note [Selectability of tables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The GraphQL specification requires that if the type of a selected field is an
interface, union, or object, then its subselection set must not be empty
(Section 5.3.3).  Since we model database tables by GraphQL objects, this means
that a table can be selected as a GraphQL field only if it has fields that we
can select, such as a column.  It is perfectly fine not to allow any selections
of any columns of the table in the database.  In that case, the table would not
be selectable as a field in GraphQL.

However, this is not the end of the story.  In addition to scalar fields, we
support relationships between tables, so that we may have another table B as a
selected field of this table A.  Then the selectability of A depends on the
selectability of B: if we permit selection a column of B, then, as a
consequence, we permit selection of the relationship from A to B, and hence we
permit selection of A, as there would now be valid GraphQL syntax that selects
A.  In turn, the selectability of B can depend on the selectability of a further
table C, through a relationship from B to C.

Now consider the case of a table A, whose columns themselves are not selectable,
but which has a relationship with itself.  Is A selectable?  In fact, if A has
no further relationships with other tables, or any computed fields, A is not
selectable.  But as soon as any leaf field in the transitive closure of tables
related to A becomes selectable, A itself becomes selectable.

In summary, figuring out the selectability of a table is a mess.  In order to
avoid doing graph theory, for now, we simply pretend that GraphQL did not have
the restriction of only allowing selections of fields of type objects when its
subselection is non-empty.  In practice, this white lie is somewhat unlikely to
cause errors on the client side, for the following reasons:

- Introspection of the GraphQL schema is normally provided to aid development of
  valid GraphQL schemas, and so any errors in the exposed schema can be caught
  at development time: when a developer is building a GraphQL query using schema
  introspection, they will eventually find out that the selection they aim to do
  is not valid GraphQL.

- We only support tables that have at least one column (since we require primary
  keys), so that the admin role can select every table anyway.

However, at the time of writing this note I am not convinced that all we are
doing is exposing non-existent schema: we should also make sure not to parse
invalid GraphQL.  So we probably eventually need to figure out the graph theory
underlying this and fix this, or altenatively convince ourselves that we have
set up our parsers in such a way that selections of fields of type objects do
require non-empty subselections.
-}

-- | Fields of a table
--
-- TODO: write a better blurb
tableSelectionSet
  :: (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> Bool
  -> m (Parser 'Output n AnnotatedFields)
tableSelectionSet table selectPermissions stringifyNum = memoizeOn 'tableSelectionSet table do
  tableInfo <- _tiCoreInfo <$> askTableInfo table
  tableName <- qualifiedObjectToName table
  let tableFields = Map.elems  $ _tciFieldInfoMap tableInfo
  fieldParsers <- fmap concat $ for tableFields \fieldInfo ->
    fieldSelection fieldInfo selectPermissions stringifyNum

  -- We don't check *here* that the subselection set is non-empty,
  -- even though the GraphQL specification requires that it is (see
  -- Note [Selectability of tables]). However, the GraphQL parser
  -- enforces that a selection set, if present, is non-empty; and our
  -- parser later verifies that a selection set is present if
  -- required, meaning that not having this check here does not allow
  -- for the construction of invalid queries.

  let description  = G.Description . getPGDescription <$> _tciDescription tableInfo
  pure $ P.selectionSet tableName description fieldParsers
    <&> parsedSelectionsToFields RQL.FExp


-- 2. local parsers
-- Parsers that are used but not exported: sub-components
-- TODO: write better blurb

-- | Arguments for a table selection
--
-- > distinct_on: [table_select_column!]
-- > limit: Int
-- > offset: Int
-- > order_by: [table_order_by!]
-- > where: table_bool_exp
tableArgs
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> m (InputFieldsParser n TableArgs)
tableArgs table selectPermissions = do
  boolExpParser <- boolExp table (Just selectPermissions)
  orderByParser <- orderByExp table selectPermissions
  columnsEnum   <- tableSelectColumnsEnum table selectPermissions
  pure $ do
    whereF   <- P.fieldOptional whereName   whereDesc   boolExpParser
    orderBy  <- P.fieldOptional orderByName orderByDesc orderByParser
    limit    <- P.fieldOptional limitName   limitDesc   positiveInt
    offset   <- P.fieldOptional offsetName  offsetDesc  positiveInt
    distinct <- maybe (pure Nothing) (P.fieldOptional distinctOnName  distinctOnDesc . P.list) columnsEnum

    -- TODO: offset should be a bigint
    --
    -- Previous versions of the code used to also accept a string for the offset
    -- despite the schema explicitly declaring it as an int; the suspected goal
    -- of this was to allow for bigint offsets, but there's no surviving commit
    -- message or documentation that states it explicity. A visible artefact of
    -- this is the fact that in the TableArgs we store a SQL expression for the
    -- offet while the limit is stored as a normal int.
    --
    -- While it would be possible to write a custom parser that advertises
    -- itself as an int, but also accepts strings, to replicate the old
    -- behaviour, a much better approach would be to use custom scalar types.

    pure $ RQL.TableArgs
      { RQL._taWhere    = whereF
      , RQL._taOrderBy  = nonEmpty =<< orderBy
      , RQL._taLimit    = fromIntegral <$> limit
      , RQL._taOffset   = txtEncoder . PGValInteger <$> offset
      , RQL._taDistCols = nonEmpty =<< distinct
      }
  where
    -- TH splices mess up ApplicativeDo
    -- see (FIXME: link to bug here)
    whereName      = $$(G.litName "where")
    orderByName    = $$(G.litName "order_by")
    limitName      = $$(G.litName "limit")
    offsetName     = $$(G.litName "offset")
    distinctOnName = $$(G.litName "distinct_in")
    whereDesc      = Just $ G.Description "filter the rows returned"
    orderByDesc    = Just $ G.Description "sort the rows by one or more columns"
    limitDesc      = Just $ G.Description "limit the number of rows returned"
    offsetDesc     = Just $ G.Description "skip the first n rows. Use only with order_by"
    distinctOnDesc = Just $ G.Description "distinct select on columns"

    -- TODO:
    -- this should either be moved to Common, or to Parser itself; even better,
    -- we could think of exposing a "PositiveInt" custom scalar type in the schema.
    positiveInt = P.int `P.bind` \value -> do
      when (value < 0) $ parseError "expecting a positive integer"
      pure value

-- | Aggregation fields
--
-- TODO: are the names and description correct?
-- > type table_aggregate_fields{
-- >   count: Int
-- >   sum: table_sum_fields
-- >   avg: table_avg_fields
-- >   stddev: table_stddev_fields
-- >   stddev_pop: table_stddev_pop_fields
-- >   variance: table_variance_fields
-- >   var_pop: table_var_pop_fields
-- >   max: table_max_fields
-- >   min: table_min_fields
-- > }
tableAggregationFields
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> m (Parser 'Output n RQL.AggFlds)
tableAggregationFields table selectPermissions = do
  tableName  <- qualifiedObjectToName table
  allColumns <- tableSelectColumns table selectPermissions
  let numColumns   = onlyNumCols allColumns
      compColumns  = onlyComparableCols allColumns
      selectName   = tableName <> $$(G.litName "_aggregate_fields")
      description  = G.Description $ "aggregate fields of \"" <> G.unName tableName <> "\""
      typenameRepr = (FieldName "__typename", RQL.AFExp $ G.unName selectName)
  numFields  <- mkFields numColumns
  compFields <- mkFields compColumns
  -- TODO: this can be heavily simplified
  aggFields <- fmap concat $ sequenceA $ catMaybes
    [ -- count
      Just $ do
        columnsEnum <- tableSelectColumnsEnum table selectPermissions
        let columnsName  = $$(G.litName "columns")
            distinctName = $$(G.litName "distinct")
            args = do
              distinct <- P.fieldOptional distinctName Nothing P.boolean
              columns  <- maybe (pure Nothing) (P.fieldOptional columnsName Nothing . P.list) columnsEnum
              pure $ case columns of
                       Nothing   -> SQL.CTStar
                       Just cols -> if fromMaybe False distinct
                                    then SQL.CTDistinct cols
                                    else SQL.CTSimple   cols
        pure [RQL.AFCount <$> P.selection $$(G.litName "count") Nothing args P.int]
    , -- operators on numeric columns
      if null numColumns then Nothing else Just $ pure $
      numericAggOperators & map \operator ->
        parseOperator operator tableName numFields
    , -- operators on comparable columns
      if null compColumns then Nothing else Just $ pure $
      comparisonAggOperators & map \operator ->
        parseOperator operator tableName compFields
    ]
  pure $ P.selectionSet selectName (Just description) aggFields
    <&> parsedSelectionsToFields RQL.AFExp
  where
    mkFields :: [PGColumnInfo] -> m [FieldParser n RQL.PGColFld]
    mkFields columns = for columns \columnInfo -> do
      field <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
      pure $ P.selection_ (pgiName columnInfo) (pgiDescription columnInfo) field
        $> RQL.PCFCol (pgiColumn columnInfo)

    parseOperator
      :: G.Name
      -> G.Name
      -> [FieldParser n RQL.PGColFld]
      -> FieldParser n RQL.AggFld
    parseOperator operator tableName columns =
      let opText  = G.unName operator
          setName = tableName <> $$(G.litName "_") <> operator <> $$(G.litName "_fields")
          setDesc = Just $ G.Description $ "aggregate " <> opText <> " on columns"
          subselectionParser = P.selectionSet setName setDesc columns
            <&> parsedSelectionsToFields RQL.PCFExp
      in P.subselection_ operator Nothing subselectionParser
         <&> (RQL.AFOp . RQL.AggOp opText)


-- | An individual field of a table
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection
  :: (MonadSchema n m, MonadError QErr m)
  => FieldInfo
  -> SelPermInfo
  -> Bool
  -> m [FieldParser n AnnotatedField]
fieldSelection fieldInfo selectPermissions stringifyNum = do
  case fieldInfo of
    FIColumn columnInfo -> maybeToList <$> runMaybeT do
      guard $ Set.member (pgiColumn columnInfo) (spiCols selectPermissions)
      let fieldName = pgiName columnInfo
          pathArg = jsonPathArg $ pgiType columnInfo
      field <- lift $ P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
      pure $ P.selection fieldName (pgiDescription columnInfo) pathArg field
        <&> RQL.mkAnnColField columnInfo

    FIRelationship relationshipInfo -> concat . maybeToList <$> runMaybeT do
      -- TODO: move this to a separate function?
      let otherTable = riRTable  relationshipInfo
          colMapping = riMapping relationshipInfo
          relName    = riName    relationshipInfo
          nullable   = riIsNullable relationshipInfo
          desc       = Just $ G.Description $ case riType relationshipInfo of
            ObjRel -> "An object relationship"
            ArrRel -> "An array relationship"
      remotePerms      <- MaybeT $ tableSelectPermissions otherTable
      relFieldName     <- lift $ textToName $ relNameToTxt relName
      otherTableParser <- MaybeT $ (if nullable then id else fmap (fmap P.nonNullableField)) $
        selectTable otherTable relFieldName desc remotePerms stringifyNum
      let field = otherTableParser <&> \selectExp ->
            let annotatedRelationship = RQL.AnnRelG relName colMapping selectExp
            in case riType relationshipInfo of
              ObjRel -> RQL.FObj annotatedRelationship
              ArrRel -> RQL.FArr $ RQL.ASSimple annotatedRelationship
      case riType relationshipInfo of
        ObjRel -> pure [field]
        ArrRel -> do
          let relAggFieldName = relFieldName <> $$(G.litName "_aggregate")
              relAggDesc      = Just $ G.Description "An aggregate relationship"
          remoteAggField <- lift $ selectTableAggregate otherTable relAggFieldName relAggDesc remotePerms stringifyNum
          pure $ catMaybes [ Just field
                           , fmap (RQL.FArr . RQL.ASAgg . RQL.AnnRelG relName colMapping) <$> remoteAggField
                           ]

    FIComputedField computedFieldInfo ->
      maybeToList <$> computedField computedFieldInfo selectPermissions stringifyNum



{- WIP notes on computed field

if the underlying function has no args         => no "args" argument
if the underlying function has at least one    => "args" object argument

if the function's return type is a JSON scalar => "path" string argument for JSON colop
if it's a set of table field                   => table selection arguments (where, limit, offset, order_by, distinct_on)

the code has a branch to deal with the lack of "args" object and treat everything as positional, but GraphiQL doesn't seem to allow it? Is it for some other cases where we parse function arguments?

-}


-- | Parses the "args" argument of a computed field.
--   All arguments to the underlying function are parsed as an "args"
--   object. Named arguments are expected in a field with the same
--   name, while positional arguments are expected in an field named
--   "arg_$n".
--   Note that collisions are possible, but ignored for now.
--   (FIXME: link to an issue?)
computedFieldFunctionArgs
  :: (MonadSchema n m, MonadError QErr m)
  => ComputedFieldFunction
  -> m (InputFieldsParser n (RQL.FunctionArgsExpTableRow UnpreparedValue))
computedFieldFunctionArgs ComputedFieldFunction{..}
  | Seq.null _cffInputArgs = pure $ pure RQL.emptyFunctionArgsExp
  | otherwise = do
      argsParser <- sequenceA $ snd $ mapAccumL createField (1 :: Int) $ toList _cffInputArgs
      let argsDesc = G.Description $ "input parameters for function " <>> _cffName
          argsObj  = P.object objName Nothing $ sequenceA argsParser
          objName  = fromJust $ G.mkName $ getFunctionTxt (qName $ _cffName) <> "_args"
      pure $ P.field $$(G.litName "args") (Just argsDesc) argsObj <&> \args ->
        let (positional, Map.fromList -> named) = partitionEithers $ catMaybes args
            tableRowArg = RQL.AETableRow Nothing
        in case _cffTableArgument of
          FTAFirst -> RQL.FunctionArgsExp (tableRowArg:positional) named
          FTANamed argName index ->
            RQL.insertFunctionArg argName index tableRowArg $
              RQL.FunctionArgsExp positional named
  where
    -- This uses Either solely to use partitionEithers.
    -- Arbitrarily, Left is for positional fields, and Right for named fields.
    createField positionalIndex arg = case faName arg of
      Nothing   -> ( positionalIndex + 1
                   , fmap (fmap Left) <$> createField' arg ("arg_" <> T.pack (show positionalIndex))
                   )
      Just name -> ( positionalIndex
                   , let nameText = getFuncArgNameTxt name
                     in fmap (fmap $ Right . (nameText, )) <$> createField' arg nameText
                   )
    createField' arg name = do
      columnParser <- P.column (PGColumnScalar $ _qptName $ faType arg) (G.Nullability False)
      let fieldName  = fromJust $ G.mkName name
          -- ^ FIXME: there probably is a better way than using fromJust
          -- can we safely assume all field names are GraphQL-compatible? If not, why?
          argParser = if unHasDefault $ faHasDefault arg
                      then P.fieldOptional  fieldName Nothing columnParser
                      else Just <$> P.field fieldName Nothing columnParser
      pure $ fmap (RQL.AEInput . mkParameter) <$> argParser

-- FIXME: move to common?
jsonPathArg :: MonadParse n => PGColumnType -> InputFieldsParser n (Maybe RQL.ColOp)
jsonPathArg columnType
  | isScalarColumnWhere isJSONType columnType =
      P.fieldOptional fieldName description P.string `P.bindFields` traverse toColExp
  | otherwise = pure Nothing
  where
    fieldName = $$(G.litName "path")
    description = Just "JSON select path"
    toColExp textValue = case parseJSONPath textValue of
      Left err     -> parseError $ T.pack $ "parse json path error: " ++ err
      Right jPaths -> return $ RQL.ColOp SQL.jsonbPathOp $ SQL.SEArray $ map elToColExp jPaths
    elToColExp (Key k)   = SQL.SELit k
    elToColExp (Index i) = SQL.SELit $ T.pack (show i)

computedField
  :: (MonadSchema n m, MonadError QErr m)
  => ComputedFieldInfo
  -> SelPermInfo
  -> Bool
  -> m (Maybe (FieldParser n AnnotatedField))
computedField ComputedFieldInfo{..} selectPermissions stringifyNum = runMaybeT do
  fieldName <- lift $ textToName $ computedFieldNameToText $ _cfiName
  functionArgsParser <- lift $ computedFieldFunctionArgs _cfiFunction
  case _cfiReturnType of
    CFRScalar scalarReturnType -> do
      guard $ _cfiName `Set.member` spiScalarComputedFields selectPermissions
      let fieldArgsParser = do
            args  <- functionArgsParser
            colOp <- jsonPathArg $ PGColumnScalar scalarReturnType
            pure $ RQL.FComputedField $ RQL.CFSScalar $ RQL.ComputedFieldScalarSel
              { RQL._cfssFunction  = _cffName _cfiFunction
              , RQL._cfssType      = scalarReturnType
              , RQL._cfssColumnOp  = colOp
              , RQL._cfssArguments = args
              }
      dummyParser <- lift $ P.column (PGColumnScalar scalarReturnType) (G.Nullability False)
      pure $ P.selection fieldName fieldDescription fieldArgsParser dummyParser
    CFRSetofTable tableName -> do
      remotePerms        <- MaybeT $ tableSelectPermissions tableName
      selectArgsParser   <- lift   $ tableArgs tableName remotePerms
      selectionSetParser <- lift   $ tableSelectionSet tableName remotePerms stringifyNum
      let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
      pure $ P.subselection fieldName Nothing fieldArgsParser selectionSetParser <&>
        \((functionArgs, args), fields) ->
          RQL.FComputedField $ RQL.CFSTable RQL.JASMultipleRows $ RQL.AnnSelG
            { RQL._asnFields   = fields
            , RQL._asnFrom     = RQL.FromFunction (_cffName _cfiFunction) functionArgs Nothing
            , RQL._asnPerm     = tablePermissionsInfo remotePerms
            , RQL._asnArgs     = args
            , RQL._asnStrfyNum = stringifyNum
            }
  where
    defaultDescription = "A computed field, executes function " <>> _cffName _cfiFunction
    fieldDescription = Just $ G.Description $ case _cffDescription _cfiFunction of
      Nothing                   -> defaultDescription
      Just (PGDescription desc) -> T.unlines [desc, "", "", defaultDescription]
      -- WIP note: the original code contained one "\n" (^ here) instead
      -- I kept that behaviour but made it explicit
      -- I feel it's an error and should be only one ""?



-- 3. local helpers
-- TODO: move to common?

tablePermissionsInfo :: SelPermInfo -> TablePerms
tablePermissionsInfo selectPermissions = RQL.TablePerm
  { RQL._tpFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ spiFilter selectPermissions
  , RQL._tpLimit  = spiLimit selectPermissions
  }
