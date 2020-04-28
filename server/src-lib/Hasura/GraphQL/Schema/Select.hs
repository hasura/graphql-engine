{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Hasura.GraphQL.Schema.Select
  ( queryExp
  , selectTable
  , selectTableAggregate
  ) where

import           Hasura.Prelude

import           Data.Either                   (partitionEithers)
import           Data.Foldable                 (toList)
import           Data.Maybe                    (fromJust)
import           Data.Parser.JSONPath
import           Data.Traversable              (mapAccumL)

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Select         as RQL
import qualified Hasura.SQL.DML                as SQL

import           Hasura.GraphQL.Parser         (FieldsParser, Kind (..), Parser,
                                                UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column  (qualifiedObjectToName)
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common  (partialSQLExpToUnpreparedValue, textToName)
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


-- TODO: move to Schema.hs
queryExp
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m (Parser 'Output n (HashMap G.Name SelectExp))
queryExp allTables stringifyNum = do
  selectExpParsers <- for (toList allTables) $ \tableName -> do
    selectPerms <- tableSelectPermissions tableName
    for selectPerms $ \perms -> do
      displayName <- qualifiedObjectToName tableName
      let desc = G.Description $ "fetch data from the table: \"" <> getTableTxt (qName tableName) <> "\""
      selectTable tableName displayName (Just desc) perms stringifyNum
    -- TODO: add aggregation tables and primary key queries
  let queryFieldsParser = fmap (Map.fromList . catMaybes) $ sequenceA $ catMaybes selectExpParsers
  pure $ P.selectionSet $$(G.litName "Query") Nothing queryFieldsParser



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
selectTable
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (G.Name, SelectExp)))
selectTable table fieldName description selectPermissions stringifyNum = do
  tableArgsParser    <- tableArgs table selectPermissions
  selectionSetParser <- tableFields table selectPermissions stringifyNum
  pure $ P.selection fieldName description tableArgsParser selectionSetParser `mapField`
    \(aliasName, args, fields) ->
      ( aliasName
      , RQL.AnnSelG
        { RQL._asnFields   = fields
        , RQL._asnFrom     = RQL.FromTable table
        , RQL._asnPerm     = tablePermissions selectPermissions
        , RQL._asnArgs     = args
        , RQL._asnStrfyNum = stringifyNum
        }
      )


-- | Table selection by primary key.
--
-- > table_name(id: 42) {
-- >   col1: col1_type
-- >   col2: col2_type
-- > }: [table!]
selectTableByPk
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (Maybe (FieldsParser 'Output n (Maybe (G.Name, SelectExp))))
selectTableByPk table fieldName description selectPermissions stringifyNum = do
  tablePrimaryKeys <- _tciPrimaryKey . _tiCoreInfo <$> askTableInfo table
  join <$> for tablePrimaryKeys \(_pkColumns -> columns) -> do
    if any (\c -> not $ pgiColumn c `Set.member` spiCols selectPermissions) columns
    then pure Nothing
    else do
      argsParser <- sequenceA <$> for columns \columnInfo -> do
        field <- P.column (pgiType columnInfo) (G.Nullability False)
        pure $ BoolFld . AVCol columnInfo . pure . AEQ True . mkParameter <$>
          P.field (pgiName columnInfo) (pgiDescription columnInfo) field
      selectionSetParser <- tableFields table selectPermissions stringifyNum
      pure $ Just $ P.selection fieldName description argsParser selectionSetParser
        `mapField` \(aliasName, boolExpr, fields) ->
        let defaultPerms = tablePermissions selectPermissions
            whereExpr    = BoolAnd $ toList boolExpr
        in ( aliasName
           , RQL.AnnSelG
             { RQL._asnFields   = fields
             , RQL._asnFrom     = RQL.FromTable table
             , RQL._asnPerm     = defaultPerms { RQL._tpLimit = Nothing }
             , RQL._asnArgs     = RQL.noTableArgs { RQL._taWhere = Just whereExpr }
             , RQL._asnStrfyNum = stringifyNum
             }
           )


-- | Table aggregation selection
--
-- Parser for an aggregation selection of a table.
-- > table_aggregate(limit: 10) {
-- >   aggregate: table_aggregate_fields
-- >   nodes: [table!]!
-- > } :: table_aggregate
--
-- FIXME: check for aggregate permissions
-- FIXME: change output to maybe
selectTableAggregate
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (G.Name, AggSelectExp)))
selectTableAggregate table fieldName description selectPermissions stringifyNum = do
  tableArgsParser <- tableArgs table selectPermissions
  nodesParser     <- tableFields table selectPermissions stringifyNum
  aggregateParser <- tableAggregationFields table selectPermissions
  selectionName   <- qualifiedObjectToName table <&> (<> $$(G.litName "_aggregate"))
  let aggregationParser = fmap catMaybes $ P.selectionSet selectionName Nothing $ sequenceA
        [ P.selection_ $$(G.litName "nodes") Nothing nodesParser
          `mapField` fmap RQL.TAFNodes
        , P.selection_ $$(G.litName "aggregate") Nothing aggregateParser
          `mapField` fmap RQL.TAFAgg
        -- TODO: handle __typename here
        ]
  pure $ P.selection fieldName description tableArgsParser aggregationParser `mapField`
    \(aliasName, args, fields) ->
      ( aliasName
      , RQL.AnnSelG
        { RQL._asnFields   = map (first nameToAlias) fields
        , RQL._asnFrom     = RQL.FromTable table
        , RQL._asnPerm     = tablePermissions selectPermissions
        , RQL._asnArgs     = args
        , RQL._asnStrfyNum = stringifyNum
        }
      )



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
  -> m (FieldsParser 'Input n TableArgs)
tableArgs table selectPermissions = do
  boolExpParser <- boolExp table selectPermissions
  orderByParser <- orderByExp table selectPermissions
  columnsEnum   <- tableColumnsEnum table selectPermissions
  pure $ do
    whereF   <- P.fieldOptional whereName   whereDesc   boolExpParser
    orderBy  <- P.fieldOptional orderByName orderByDesc orderByParser
    limit    <- P.fieldOptional limitName   limitDesc   P.int
    offset   <- P.fieldOptional offsetName  offsetDesc  P.int
    distinct <- maybe (pure Nothing) (P.fieldOptional offsetName  offsetDesc . P.list) columnsEnum
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


-- | Fields of a table
--
-- TODO: write a better blurb
tableFields
  :: (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> Bool
  -> m (Parser 'Output n AnnotatedFields)
tableFields table selectPermissions stringifyNum =
  memoizeOn 'tableFields table $ do
    tableInfo <- _tiCoreInfo <$> askTableInfo table
    tableName <- qualifiedObjectToName table
    fields    <- fmap catMaybes
      $ traverse (\fieldInfo -> fieldSelection fieldInfo selectPermissions stringifyNum)
      =<< tableSelectFields table selectPermissions
    -- TODO: handle __typename here
    pure $ P.selectionSet tableName (_tciDescription tableInfo) $ catMaybes <$> sequenceA fields


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
    let numColumns  = onlyNumCols allColumns
    let compColumns = onlyComparableCols allColumns
    numFields  <- mkFields numColumns
    compFields <- mkFields compColumns
    aggFields  <- fmap (fmap catMaybes . sequenceA . concat) $ sequenceA $ catMaybes
      [ -- count
        Just $ do
          columnsEnum <- tableColumnsEnum table selectPermissions
          let columnsName  = $$(G.litName "columns")
              distinctName = $$(G.litName "distinct")
              args = do
                distinct <- P.fieldOptional columnsName  Nothing P.boolean
                columns  <- maybe (pure Nothing) (P.fieldOptional distinctName Nothing . P.list) columnsEnum
                pure $ case columns of
                         Nothing   -> SQL.CTStar
                         Just cols -> if fromMaybe False distinct
                                      then SQL.CTDistinct cols
                                      else SQL.CTSimple   cols
          pure $ pure $ P.selection $$(G.litName "count") Nothing args P.int
            `mapField` (nameToAlias *** RQL.AFCount)
      , -- operators on numeric columns
        if null numColumns then Nothing else Just $
        for [ "sum", "avg", "stddev", "stddev_samp", "stddev_pop"
            , "variance", "var_samp", "var_pop"
            ] \operator -> do
          opName <- textToName operator
          pure $ parseOperator operator opName tableName numFields
      , -- operators on comparable columns
        if null compColumns then Nothing else Just $
        for ["max", "min"] \operator -> do
          opName <- textToName operator
          pure $ parseOperator operator opName tableName compFields
        -- TODO: handle __typename here
      ]
    let selectName  = tableName <> $$(G.litName "_aggregate_fields")
        description = G.Description $ "aggregate fields of \"" <> G.unName tableName <> "\""
    pure $ P.selectionSet selectName (Just description) aggFields
  where
    mkFields :: [PGColumnInfo] -> m (FieldsParser 'Output n RQL.ColFlds)
    mkFields = fmap (fmap catMaybes . sequenceA) . traverse \columnInfo -> do
      field <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
      pure $ P.selection_ (pgiName columnInfo) (pgiDescription columnInfo) field
        `mapField` \name -> (nameToAlias name, RQL.PCFCol $ pgiColumn columnInfo)

    parseOperator
      :: Text
      -> G.Name
      -> G.Name
      -> FieldsParser 'Output n RQL.ColFlds
      -> FieldsParser 'Output n (Maybe (FieldName, RQL.AggFld))
    parseOperator operator opName tableName columns =
      let setName = tableName <> $$(G.litName "_") <> opName <> $$(G.litName "_fields")
          setDesc = Just $ G.Description $ "aggregate " <> operator <> " on columns"
      in P.selection_ opName Nothing (P.selectionSet setName setDesc columns)
           `mapField` (nameToAlias *** RQL.AFOp . RQL.AggOp operator)


-- | An individual field of a table
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection
  :: (MonadSchema n m, MonadError QErr m)
  => FieldInfo
  -> SelPermInfo
  -> Bool
  -> m (Maybe (FieldsParser 'Output n (Maybe (FieldName, AnnotatedField))))
fieldSelection fieldInfo selectPermissions stringifyNum =
  for (fieldInfoGraphQLName fieldInfo) \fieldName ->
    aliasToFieldName <$> case fieldInfo of
      FIColumn columnInfo -> do
        let annotated = RQL.mkAnnColField columnInfo Nothing -- FIXME: support ColOp
        field <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
        pure $ if Set.member (pgiColumn columnInfo) $ spiCols selectPermissions
               then fmap (, annotated) <$> P.selection_ fieldName (pgiDescription columnInfo) field
               else pure Nothing
      FIRelationship relationshipInfo -> do
        let otherTable = riRTable  relationshipInfo
            colMapping = riMapping relationshipInfo
            relName    = riName    relationshipInfo
        tableSelectPermissions otherTable >>= \case
          Nothing    -> pure $ pure Nothing
          Just perms -> do
            -- FIXME: support aggregation
            let desc = Just $ G.Description $ case riType relationshipInfo of
                  ObjRel -> "An object relationship"
                  ArrRel -> "An array relationship"
            relFieldName <- textToName $ relNameToTxt relName
            otherTableParser <- selectTable otherTable relFieldName desc perms stringifyNum
            pure $ otherTableParser <&> fmap \(psName, selectExp) ->
              ( psName
              , let annotatedRelationship = RQL.AnnRelG relName colMapping selectExp
                in case riType relationshipInfo of
                  ObjRel -> RQL.FObj annotatedRelationship
                  ArrRel -> RQL.FArr $ RQL.ASSimple annotatedRelationship
              )
      FIComputedField computedFieldInfo ->
        computedField computedFieldInfo selectPermissions stringifyNum
  where
    aliasToFieldName = fmap $ fmap $ first nameToAlias



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
  -> m (FieldsParser 'Input n (RQL.FunctionArgsExpTableRow UnpreparedValue))
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
jsonPathArg :: MonadParse n => PGScalarType -> FieldsParser 'Input n (Maybe RQL.ColOp)
jsonPathArg functionReturnType
  | isJSONType functionReturnType =
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
  -> m (FieldsParser 'Output n (Maybe (G.Name, AnnotatedField)))
computedField ComputedFieldInfo{..} selectPermissions stringifyNum = do
  fieldName <- textToName $ computedFieldNameToText $ _cfiName
  functionArgsParser <- computedFieldFunctionArgs _cfiFunction
  case _cfiReturnType of
    CFRScalar scalarReturnType ->
      if Set.member _cfiName $ spiScalarComputedFields selectPermissions
      then do
        let fieldArgsParser = do
              args  <- functionArgsParser
              colOp <- jsonPathArg scalarReturnType
              pure $ RQL.FComputedField $ RQL.CFSScalar $ RQL.ComputedFieldScalarSel
                { RQL._cfssFunction  = _cffName _cfiFunction
                , RQL._cfssType      = scalarReturnType
                , RQL._cfssColumnOp  = colOp
                , RQL._cfssArguments = args
                }
        dummyParser <- P.column (PGColumnScalar scalarReturnType) (G.Nullability False)
        pure $ P.selection fieldName fieldDescription fieldArgsParser dummyParser
      else pure $ pure Nothing
    CFRSetofTable tableName -> tableSelectPermissions tableName >>= \case
      Nothing    -> pure $ pure Nothing
      Just perms -> do
        -- WIP note: this is very similar to selectFromTable
        -- I wonder if it would make sense to merge them
        -- I'm erring on the side of no for now, but to be reconsidered
        -- when we clean the code after reaching feature parity
        selectArgsParser   <- tableArgs tableName perms
        selectionSetParser <- tableFields tableName perms stringifyNum
        let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
        pure $ P.selection fieldName Nothing fieldArgsParser selectionSetParser
          <&> fmap \(aliasName, (functionArgs, args), fields) ->
            ( aliasName
            , RQL.FComputedField $ RQL.CFSTable RQL.JASMultipleRows $ RQL.AnnSelG
              { RQL._asnFields   = fields
              , RQL._asnFrom     = RQL.FromFunction (_cffName _cfiFunction) functionArgs Nothing
              , RQL._asnPerm     = tablePermissions perms
              , RQL._asnArgs     = args
              , RQL._asnStrfyNum = stringifyNum
              }
            )
  where
    defaultDescription = "A computed field, executes function " <>> _cffName _cfiFunction
    fieldDescription = Just $ G.Description $ case _cffDescription _cfiFunction of
      Nothing                   -> defaultDescription
      Just (G.Description desc) -> T.unlines [desc, "", "", defaultDescription]
      -- WIP note: the original code contained one "\n" (^ here) instead
      -- I kept that behaviour but made it explicit
      -- I feel it's an error and should be only one ""?



-- 3. local helpers
-- TODO: move to common?

tablePermissions :: SelPermInfo -> TablePerms
tablePermissions selectPermissions = RQL.TablePerm
  { RQL._tpFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ spiFilter selectPermissions
  , RQL._tpLimit  = spiLimit selectPermissions
  }

mapField
  :: Functor m
  => FieldsParser k m (Maybe a) -> (a -> b) -> FieldsParser k m (Maybe b)
mapField fp f = fmap (fmap f) fp

nameToAlias :: G.Name -> FieldName
nameToAlias = FieldName . G.unName
