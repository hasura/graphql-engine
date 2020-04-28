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
import qualified Hasura.SQL.DML                as S

import           Hasura.GraphQL.Parser         (FieldsParser, Kind (..), Parser,
                                                UnpreparedValue (..))
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


-- TODO: move to schema
queryExp
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => HashSet QualifiedTable
  -> Bool
  -> m (Parser 'Output n (HashMap G.Name SelectExp))
queryExp allTables stringifyNum = do
  selectExpParsers <- for (toList allTables) $ \tableName -> do
    displayName <- qualifiedObjectToName tableName
    selectPerms <- tableSelectPermissions tableName
    let desc = G.Description $ "fetch data from the table: \"" <> getTableTxt (qName tableName) <> "\""
    for selectPerms $ \perms ->
      selectTable tableName displayName (Just desc) perms stringifyNum
    -- TODO: add aggregation tables and primary key queries
  let queryFieldsParser = fmap (Map.fromList . catMaybes) $ sequenceA $ catMaybes selectExpParsers
  pure $ P.selectionSet $$(G.litName "Query") Nothing queryFieldsParser



-- 1. top level selection functions
-- write a blurb?

-- | Simple table selection.
--
-- Parser for an output field for a given table, such as
-- > field_name(limit: 10) {
-- >   col1: col1_type
-- >   col2: col2_type
-- > }: [table!]
--
-- Note that the name of the selection field does not necessarily
-- match the name of the table, which is why it is given as an
-- argument.
selectTable
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (G.Name, SelectExp)))
selectTable table fieldName description selectPermissions stringifyNum = do
  tableArgsParser    <- tableArgs table
  selectionSetParser <- tableFields table selectPermissions stringifyNum
  pure $ P.selection fieldName description tableArgsParser selectionSetParser `mapField`
    \(aliasName, tableArgs, tableFields) ->
      ( aliasName
      , RQL.AnnSelG
        { RQL._asnFields   = tableFields
        , RQL._asnFrom     = RQL.FromTable table
        , RQL._asnPerm     = tablePermissions selectPermissions
        , RQL._asnArgs     = tableArgs
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
selectTableAggregate
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (G.Name, AggSelectExp)))
selectTableAggregate table fieldName description selectPermissions stringifyNum = do
  tableArgsParser   <- tableArgs table
  tableFieldsParser <- selectTable table nodesName Nothing selectPermissions stringifyNum
  selectionName     <- qualifiedObjectToName table <&> (<> $$(G.litName "_aggregate"))
  let aggregationParser = P.selectionSet selectionName Nothing $ sequenceA
        [ fmap (fmap RQL.TAFNodes) <$> tableFieldsParser
        , tableAggregationFields undefined undefined
        -- TODO: handle __typename here
        ]
  pure $ P.selection fieldName description tableArgsParser aggregationParser <&> fmap
    \(aliasName, tableArgs, aggregationFields) ->
      ( aliasName
      , RQL.AnnSelG
        { RQL._asnFields   = aggregationFields
        , RQL._asnFrom     = RQL.FromTable table
        , RQL._asnPerm     = tablePermissions selectPermissions
        , RQL._asnArgs     = tableArgs
        , RQL._asnStrfyNum = stringifyNum
        }
      )
  where
    nodesName     = $$(G.litName "nodes")
    aggregateName = $$(G.litName "aggregate")



-- 2. local parsers
-- Parsers that are used but not exported: sub-components
-- todo write better blurb

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
  boolExpParser <- boolExp table
  orderByParser <- orderByExp table
  columnsEnum   <- tableColumnsEnum table selectPermissions
  pure $ do
    whereF   <- P.fieldOptional whereName   whereDesc   boolExpParser
    orderBy  <- P.fieldOptional orderByName orderByDesc orderByParser
    limit    <- P.fieldOptional limitName   limitDesc   P.int
    offset   <- P.fieldOptional offsetName  offsetDesc  P.int
    distinct <- P.fieldOptional offsetName  offsetDesc  columnsEnum
    pure $ RQL.TableArgs
      { RQL._taWhere    = whereF
      , RQL._taOrderBy  = nonEmpty =<< orderBy
      , RQL._taLimit    = fromIntegral <$> limit
      , RQL._taOffset   = txtEncoder . PGValInteger <$> offset
      , RQL._taDistCols = distinct
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
-- TODO: can this be merged with selectTable?
tableFields
  :: (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> Bool
  -> m (Parser 'Output n AnnotatedFields)
tableFields table selectPermissions stringifyNum =
  memoizeOn 'tableSelectionSet tableName $ do
    tableName <- qualifiedObjectToName table
    fields    <- fmap catMaybes
      $ traverse (\fieldInfo -> fieldSelection fieldInfo selectPermissions stringifyNum)
      =<< tableSelectFields table selectPermissions
    -- TODO: handle __typename here
    pure $ P.selectionSet tableName (_tciDescription tableInfo) $ catMaybes <$> sequenceA fields

-- | Table columns enum
--
-- Parser for an enum type that matches the columns of the given
-- table. Used as a parameter for "distinct", among others. Maps to
-- the table_select_columns object.
-- TODO: move this to Table?
tableColumnsEnum
  :: (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> m (Parser 'Both n PGCol)
tableColumnsEnum table selectPermissions = do
  tableName <- qualifiedObjectToName table
  columns   <- tableSelectColumns table selectPermissions
  let enumName    = tableName <> $$(G.litName "_select_columns")
      description = Just $ G.Description $ "select columns of table \"" <> tableName <> "\""
  pure $ P.enum enumName description $ NE.fromList
    [ ( P.mkDefinition (pgiName column) (G.Description "column name") P.EnumValueInfo
      , pgiColumn column
      )
    | column <- columns
    ]


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
  :: (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> m (Parser 'Output n _)
tableAggregationFields table fieldName description selectPermissions =
  memoizeOn 'tableSelectionSet table $ do
    tableName  <- qualifiedObjectToName table
    allColumns <- tableSelectColumns table selectPermissions
    numFields  <- fmap sequenceA $ traverse mkField $ RQL.onlyNumCols  columns
    compFields <- fmap sequenceA $ traverse mkField $ RQL.onlyCompCols columns
    fields <- fmap (sequenceA . concat) $ sequenceA $ catMaybes
      [ Just $ do
          columnsEnum <- tableColumnsEnum table selectPermissions
          let columnsName  = $$(G.litName "columns")
              distinctName = $$(G.litName "distinct")
              args = do
                distinct <- P.fieldOptional columnsName  Nothing P.boolean
                columns  <- P.fieldOptional distinctName Nothing columnsEnum
                case columns of
                  Nothing   -> pure SQL.CTStar
                  Just cols -> if fromMaybe False distinct
                               then SQL.CTDistinct cols
                               else SQL.CTSimple cols
                pure $ RS.AFCount <$> countField
      , if null numFields then Nothing else Just $
        for [ "sum", "avg", "stddev", "stddev_samp", "stddev_pop"
            , "variance", "var_samp", "var_pop"
            ] \operator -> do
          operatorName <- textToName operator
          let setName = tableName <> "_" <> operatorName <> $$(G.litName "_fields")
          pure $ P.selection_ operatorName Nothing $ selectionSet setName numFields
            `mapField` \(name, selection) -> RQL.AFOp $ RQL.AggOp name selection
      , if null compFields then Nothing else Just $
        for ["max", "min"] \operator -> do
          operatorName <- textToName operator
          let setName = tableName <> "_" <> operatorName <> $$(G.litName "_fields")
          pure $ P.selection_ operatorName Nothing $ selectionSet setName compFields
            `mapField` \(name, selection) -> RQL.AFOp $ RQL.AggOp name selection
      -- TODO: handle __typename here
      ]
    let selectName = tableName <> $$(G.litName "_aggregate_fields")
    pure $ P.selection fieldName description $ selectionSet selectName description fields
  where
    mkField columnInfo = do
      let annotated = RQL.mkAnnColField columnInfo Nothing -- FIXME: support ColOp?
      field <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
      pure $ fmap (, annotated) <$> P.selection_ fieldName (pgiDescription columnInfo) field


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
    aliasToFieldName = fmap $ fmap $ first $ FieldName . G.unName



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
      pure $ fmap (RQL.AEInput . P.mkParameter) <$> argParser

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
      Right jPaths -> return $ RQL.ColOp S.jsonbPathOp $ S.SEArray $ map elToColExp jPaths
    elToColExp (Key k)   = S.SELit k
    elToColExp (Index i) = S.SELit $ T.pack (show i)

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
        selectArgsParser   <- tableArgs tableName
        selectionSetParser <- tableSelectionSet tableName perms stringifyNum
        let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
        pure $ P.selection fieldName Nothing fieldArgsParser selectionSetParser
          <&> fmap \(aliasName, (functionArgs, tableArgs), tableFields) ->
            ( aliasName
            , RQL.FComputedField $ RQL.CFSTable RQL.JASMultipleRows $ RQL.AnnSelG
              { RQL._asnFields   = tableFields
              , RQL._asnFrom     = RQL.FromFunction (_cffName _cfiFunction) functionArgs Nothing
              , RQL._asnPerm     = tablePermissions perms
              , RQL._asnArgs     = tableArgs
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
