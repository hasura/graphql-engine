{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Hasura.GraphQL.Schema.Select
  ( selectTable
  , selectTableByPk
  , selectTableAggregate
  , selectFunction
  , selectFunctionAggregate
  , tableSelectionSet
  , nodeField
  ) where


import           Hasura.Prelude

import           Control.Lens
import           Data.Has
import           Data.Maybe                            (fromJust)
import           Data.Parser.JSONPath
import           Data.Traversable                      (mapAccumL)

import qualified Data.Aeson                            as J
import qualified Data.Aeson.Extended                   as J
import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashSet                          as Set
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Sequence                         as Seq
import qualified Data.Sequence.NonEmpty                as NESeq
import qualified Data.Text                             as T
import qualified Language.GraphQL.Draft.Syntax         as G

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P
import qualified Hasura.RQL.DML.Select                 as RQL
import qualified Hasura.RQL.Types.BoolExp              as RQL
import qualified Hasura.SQL.DML                        as SQL

import           Hasura.GraphQL.Parser                 (FieldParser, InputFieldsParser, Kind (..),
                                                        Parser, UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column          (qualifiedObjectToName)
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Schema.Remote
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
selectTable
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> m (FieldParser n SelectExp)
selectTable table fieldName description selectPermissions = do
  stringifyNum <- asks $ qcStringifyNum . getter
  tableArgsParser    <- tableArgs table selectPermissions
  selectionSetParser <- tableSelectionSet table selectPermissions Nothing
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> m (Maybe (FieldParser n SelectExp))
selectTableByPk table fieldName description selectPermissions = runMaybeT do
  stringifyNum <- asks $ qcStringifyNum . getter
  primaryKeys <- MaybeT $ fmap _pkColumns . _tciPrimaryKey . _tiCoreInfo <$> askTableInfo table
  guard $ all (\c -> pgiColumn c `Set.member` spiCols selectPermissions) primaryKeys
  argsParser <- lift $ sequenceA <$> for primaryKeys \columnInfo -> do
    field <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
    pure $ BoolFld . AVCol columnInfo . pure . AEQ True . mkParameter <$>
      P.field (pgiName columnInfo) (pgiDescription columnInfo) field
  selectionSetParser <- lift $ tableSelectionSet table selectPermissions Nothing
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the table
  -> m (Maybe (FieldParser n AggSelectExp))
selectTableAggregate table fieldName description selectPermissions = runMaybeT do
  stringifyNum <- asks $ qcStringifyNum . getter
  guard $ spiAllowAgg selectPermissions
  tableArgsParser <- lift $ tableArgs table selectPermissions
  aggregateParser <- lift $ tableAggregationFields table selectPermissions
  selectionName   <- lift $ qualifiedObjectToName table <&> (<> $$(G.litName "_aggregate"))
  nodesParser     <- lift $ tableSelectionSet table selectPermissions Nothing
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
  :: ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => QualifiedTable
  -> SelPermInfo
  -> Maybe (Parser 'Output n b)
  -- ^ An interface that this object claims to implement (used for Relay)
  -> m (Parser 'Output n AnnotatedFields)
tableSelectionSet table selectPermissions interfaceM = memoizeOn 'tableSelectionSet table do
  tableInfo <- _tiCoreInfo <$> askTableInfo table
  tableName <- qualifiedObjectToName table
  let tableFields = Map.elems  $ _tciFieldInfoMap tableInfo
  fieldParsers <- fmap concat $ for tableFields \fieldInfo ->
    fieldSelection fieldInfo selectPermissions

  -- We don't check *here* that the subselection set is non-empty,
  -- even though the GraphQL specification requires that it is (see
  -- Note [Selectability of tables]). However, the GraphQL parser
  -- enforces that a selection set, if present, is non-empty; and our
  -- parser later verifies that a selection set is present if
  -- required, meaning that not having this check here does not allow
  -- for the construction of invalid queries.

  let description  = G.Description . getPGDescription <$> _tciDescription tableInfo
  pure $ P.selectionSetObject tableName description fieldParsers (toList interfaceM)
    <&> parsedSelectionsToFields RQL.FExp


-- | User-defined function (AKA custom function)
selectFunction
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => FunctionInfo         -- ^ SQL function info
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the target table
  -> m (FieldParser n SelectExp)
selectFunction function fieldName description selectPermissions = do
  stringifyNum <- asks $ qcStringifyNum . getter
  let table = fiReturnType function
  tableArgsParser    <- tableArgs table selectPermissions
  functionArgsParser <- customSQLFunctionArgs function
  selectionSetParser <- tableSelectionSet table selectPermissions Nothing
  let argsParser = liftA2 (,) functionArgsParser tableArgsParser
  pure $ P.subselection fieldName description argsParser selectionSetParser
    <&> \((funcArgs, tableArgs'), fields) -> RQL.AnnSelG
      { RQL._asnFields   = fields
      , RQL._asnFrom     = RQL.FromFunction (fiName function) funcArgs Nothing
      , RQL._asnPerm     = tablePermissionsInfo selectPermissions
      , RQL._asnArgs     = tableArgs'
      , RQL._asnStrfyNum = stringifyNum
      }

selectFunctionAggregate
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => FunctionInfo         -- ^ SQL function info
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo          -- ^ select permissions of the target table
  -> m (Maybe (FieldParser n AggSelectExp))
selectFunctionAggregate function fieldName description selectPermissions = runMaybeT do
  let table = fiReturnType function
  stringifyNum <- asks $ qcStringifyNum . getter
  guard $ spiAllowAgg selectPermissions
  tableArgsParser    <- lift $ tableArgs table selectPermissions
  functionArgsParser <- lift $ customSQLFunctionArgs function
  aggregateParser    <- lift $ tableAggregationFields table selectPermissions
  selectionName      <- lift $ qualifiedObjectToName table <&> (<> $$(G.litName "_aggregate"))
  nodesParser        <- lift $ tableSelectionSet table selectPermissions Nothing
  let argsParser = liftA2 (,) functionArgsParser tableArgsParser
      aggregationParser = parsedSelectionsToFields RQL.TAFExp <$>
        P.selectionSet selectionName Nothing
        [ RQL.TAFNodes <$> P.subselection_ $$(G.litName "nodes") Nothing nodesParser
        , RQL.TAFAgg <$> P.subselection_ $$(G.litName "aggregate") Nothing aggregateParser
        ]
  pure $ P.subselection fieldName description argsParser aggregationParser
    <&> \((funcArgs, tableArgs'), fields) -> RQL.AnnSelG
      { RQL._asnFields   = fields
      , RQL._asnFrom     = RQL.FromFunction (fiName function) funcArgs Nothing
      , RQL._asnPerm     = tablePermissionsInfo selectPermissions
      , RQL._asnArgs     = tableArgs'
      , RQL._asnStrfyNum = stringifyNum
      }


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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> SelPermInfo
  -> m (InputFieldsParser n TableArgs)
tableArgs table selectPermissions = do
  boolExpParser <- boolExp table (Just selectPermissions)
  orderByParser <- orderByExp table selectPermissions
  columnsEnum   <- tableSelectColumnsEnum table selectPermissions
  pure $ do
    whereF   <- fmap join $ P.fieldOptional whereName   whereDesc   $ P.nullable boolExpParser
    orderBy  <- fmap join $ P.fieldOptional orderByName orderByDesc $ P.nullable $ P.list orderByParser
    limit    <- fmap join $ P.fieldOptional limitName   limitDesc   $ P.nullable positiveInt
    offset   <- fmap join $ P.fieldOptional offsetName  offsetDesc  $ P.nullable positiveInt
    distinct <- fmap (join.join) $ for columnsEnum \columnNameParser ->
      P.fieldOptional distinctOnName distinctOnDesc $ P.nullable $ P.list columnNameParser

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

    -- TODO: distinct_on must be validated ungainst order_by
    -- the check at Resolve/Select.hs:152 must be ported here

    pure $ RQL.TableArgs
      { RQL._taWhere    = whereF
      , RQL._taOrderBy  = nonEmpty . concat =<< orderBy
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
    distinctOnName = $$(G.litName "distinct_on")
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
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

lookupRemoteField'
  :: (MonadSchema n m, MonadTableInfo r m)
  => [P.Definition P.FieldInfo]
  -> FieldCall
  -> m P.FieldInfo
lookupRemoteField' fieldInfos (FieldCall fcName _) =
  case find ((== fcName) . P.dName) fieldInfos of
    Nothing -> throw400 RemoteSchemaError $ "field with name " <> fcName <<> " not found"
    Just (P.Definition _ _ _ fieldInfo) -> pure fieldInfo

lookupRemoteField
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => [P.Definition P.FieldInfo]
  -> NonEmpty FieldCall
  -> m P.FieldInfo
lookupRemoteField fieldInfos (fieldCall :| rest) =
  case NE.nonEmpty rest of
    Nothing -> lookupRemoteField' fieldInfos fieldCall
    Just rest' -> do
      (P.FieldInfo _ type') <- lookupRemoteField' fieldInfos fieldCall
      (P.Definition _ _ _ (P.ObjectInfo objFieldInfos _))
       <- onNothing (P.getObjectInfo $ type') $
          throw400 RemoteSchemaError $ "field " <> fcName fieldCall <<> " is expected to be an object"
      lookupRemoteField objFieldInfos rest'

-- | An individual field of a table
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => FieldInfo
  -> SelPermInfo
  -> m [FieldParser n AnnotatedField]
fieldSelection fieldInfo selectPermissions = do
  case fieldInfo of
    FIColumn columnInfo -> maybeToList <$> runMaybeT do
      let columnName = pgiColumn columnInfo
      guard $ Set.member columnName (spiCols selectPermissions)
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
      otherTableParser <- lift $ selectTable otherTable relFieldName desc remotePerms
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
          remoteAggField <- lift $ selectTableAggregate otherTable relAggFieldName relAggDesc remotePerms
          pure $ catMaybes [ Just field
                           , fmap (RQL.FArr . RQL.ASAgg . RQL.AnnRelG relName colMapping) <$> remoteAggField
                           ]

    FIComputedField computedFieldInfo ->
      maybeToList <$> computedField computedFieldInfo selectPermissions

    FIRemoteRelationship (remoteFieldInfo :: RemoteFieldInfo)  -> do
      remoteSchemasFieldDefns <- asks $ qcRemoteFields . getter
      let remoteSchemaName = _rfiRemoteSchemaName remoteFieldInfo
      fieldDefns <-
        case Map.lookup remoteSchemaName remoteSchemasFieldDefns of
          Nothing ->
            throw500 $ "unexpected: remote schema "
            <> remoteSchemaName
            <<> " not found"
          Just fieldDefns -> pure fieldDefns

      fieldName <- textToName $ remoteRelationshipNameToText $ _rfiName remoteFieldInfo
      remoteFieldsArgumentsParser <-
        fmap sequenceA $ for (Map.toList $ _rfiParamMap remoteFieldInfo) $
        \(name, inpValDefn) ->
          inputValueDefinitionParser (_rfiSchemaDoc remoteFieldInfo) inpValDefn
          -- FIXME: It shouldn't be Nothing below, construct the 'Maybe [(VariableDefinition, A.Value)]'
          --  and use that!
          <&> fmap (\gVal -> RQL.RemoteFieldArgument name gVal Nothing)

      -- This selection set parser, should be of the remote node's selection set parser, which comes
      -- from the fieldCall
      nestedFieldInfo <- lookupRemoteField fieldDefns $ unRemoteFields $ _rfiRemoteFields remoteFieldInfo
      case nestedFieldInfo of
        P.FieldInfo{ P.fType = fieldType } -> do
          let fieldInfo' = P.FieldInfo
                { P.fArguments = P.ifDefinitions remoteFieldsArgumentsParser
                , P.fType = fieldType }
          pure $ pure $ P.unsafeRawField (P.mkDefinition fieldName Nothing fieldInfo')
            `P.bindField` \G.Field{ G._fArguments = args, G._fSelectionSet = selSet } -> do
              remoteArgs <- P.ifParser remoteFieldsArgumentsParser args
              pure $ RQL.FRemote $ RQL.RemoteSelect
                { _rselArgs          = remoteArgs
                , _rselSelection     = selSet
                , _rselHasuraColumns = _rfiHasuraFields remoteFieldInfo
                , _rselFieldCall     = unRemoteFields $ _rfiRemoteFields remoteFieldInfo
                , _rselRemoteSchema  = _rfiRemoteSchema remoteFieldInfo
                }

-- | Parses the arguments to the underlying sql function of a computed field or
--   a custom function. All arguments to the underlying sql function are parsed
--   as an "args" object. Named arguments are expected in a field with the same
--   name, while positional arguments are expected in an field named "arg_$n".
--   Note that collisions are possible, but ignored for now, if a named argument
--   is also named "arg_$n". (FIXME: link to an issue?)
--
--   Depending on the arguments the SQL function requires, there are three possible outcomes:
--   1. if the function requires no argument, or if its only argument is not
--      user-provided (the session argument in the case of custom functions, the
--      table row argument in the case of computed fields), the args object will
--      NOT be part of the schema;
--   2. if all non-session arguments have a default value, then the args object will be in
--      the schema as an optional field;
--   3. otherwise, if there is at least one field that doesn't have a default value, the
--      args object will be a required field.
functionArgs
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  => QualifiedFunction
  -> Seq.Seq FunctionInputArgument
  -> m (InputFieldsParser n (RQL.FunctionArgsExpTableRow UnpreparedValue))
functionArgs functionName (toList -> inputArgs) = do
  -- First, we iterate through the original sql arguments in order, to find the
  -- corresponding graphql names. At the same time, we create the input field
  -- parsers, in three groups: session argument, optional arguments, and
  -- mandatory arguments.
  let (names, session, optional, mandatory) = mconcat $ snd $ mapAccumL splitArguments 1 inputArgs
      defaultArguments = RQL.FunctionArgsExp (snd <$> session) Map.empty

  if | length session > 1 -> do
         -- We somehow found more than one session argument; this should never
         -- happen and is an error on our side.
         error "there shouldn't be more than one session argument" -- FIXME: make it a 500
     | null optional && null mandatory -> do
         -- There are no user-provided arguments to the function: there will be
         -- no args field.
         pure $ pure defaultArguments
     | otherwise -> do
         -- There are user-provided arguments: we need to parse an args object.
         argumentParsers <- sequenceA $ optional <> mandatory
         let objectName   = fromJust $ G.mkName $ getFunctionTxt (qName functionName) <> "_args"
             fieldName    = $$(G.litName "args")
             fieldDesc    = G.Description $ "input parameters for function " <>> functionName
             objectParser = P.object objectName Nothing (sequenceA argumentParsers) `P.bind` \arguments -> do
               -- After successfully parsing, we create a dictionary of the parsed fields
               -- and we re-iterate through the original list of sql arguments, now with
               -- the knowledge of their graphql name.
               let foundArguments = Map.fromList $ catMaybes arguments <> session
                   argsWithNames  = zip names inputArgs

               -- All elements (in the orignal sql order) that are found in the result map
               -- are treated as positional arguments, whether they were originally named or
               -- not.
               (positional, left) <- spanMaybeM (\(name, _) -> pure $ Map.lookup name foundArguments) argsWithNames

               -- If there are arguments left, it means we found one that was not passed
               -- positionally. As a result, any remaining argument will have to be passed
               -- by name. We fail with a parse error if we encounter a positional sql
               -- argument (that does not have a name in the sql function), as:
               --   * only the last positional arguments can be omitted;
               --   * it has no name we can use.
               named <- Map.fromList . catMaybes <$> traverse (namedArgument foundArguments) left
               pure $ RQL.FunctionArgsExp positional named

         -- If all fields are optional, the object itself is optional.
         pure $ if null mandatory
                then P.fieldOptional fieldName (Just fieldDesc) objectParser <&> fromMaybe defaultArguments
                else P.field fieldName (Just fieldDesc) objectParser

  where
    splitArguments
      :: Int
      -> FunctionInputArgument
      -> (Int, ( [Text] -- graphql names, in order
               , [(Text, RQL.ArgumentExp UnpreparedValue)] -- session argument
               , [m (InputFieldsParser n (Maybe (Text, RQL.ArgumentExp UnpreparedValue)))] -- optional argument
               , [m (InputFieldsParser n (Maybe (Text, RQL.ArgumentExp UnpreparedValue)))] -- mandatory argument
               )
         )
    splitArguments positionalIndex (IASessionVariables name) =
      let argName = getFuncArgNameTxt name
      in (positionalIndex, ([argName], [(argName, RQL.AEInput P.UVSession)], [], []))
    splitArguments positionalIndex (IAUserProvided arg) =
      let (argName, newIndex) = case faName arg of
                                  Nothing   -> ("arg_" <> T.pack (show positionalIndex), positionalIndex + 1)
                                  Just name -> (getFuncArgNameTxt name, positionalIndex)
      in if unHasDefault $ faHasDefault arg
         then (newIndex, ([argName], [], [parseArgument arg argName], []))
         else (newIndex, ([argName], [], [], [parseArgument arg argName]))

    parseArgument :: FunctionArg -> Text -> m (InputFieldsParser n (Maybe (Text, RQL.ArgumentExp UnpreparedValue)))
    parseArgument arg name = do
      columnParser <- P.column (PGColumnScalar $ _qptName $ faType arg) (G.Nullability True)
      fieldName    <- textToName name
      let argParser = if unHasDefault $ faHasDefault arg
                      then P.fieldOptional  fieldName Nothing columnParser
                      else Just <$> P.field fieldName Nothing columnParser
      pure $ argParser `mapField` ((name,) . RQL.AEInput . mkParameter)

    namedArgument dictionary (name, inputArgument) = for (Map.lookup name dictionary) \parsedValue ->
      case inputArgument of
        IASessionVariables _ -> pure (name, parsedValue)
        IAUserProvided arg   -> case faName arg of
          Just _  -> pure (name, parsedValue)
          Nothing -> parseError "Only last set of positional arguments can be omitted"


customSQLFunctionArgs
  :: (MonadSchema n m, MonadTableInfo r m)
  => FunctionInfo
  -> m (InputFieldsParser n (RQL.FunctionArgsExpTableRow UnpreparedValue))
customSQLFunctionArgs FunctionInfo{..} = functionArgs fiName fiInputArgs

computedFieldFunctionArgs
  :: (MonadSchema n m, MonadTableInfo r m)
  => ComputedFieldFunction
  -> m (InputFieldsParser n (RQL.FunctionArgsExpTableRow UnpreparedValue))
computedFieldFunctionArgs ComputedFieldFunction{..} =
  functionArgs _cffName (IAUserProvided <$> _cffInputArgs) <&> fmap addTableRowArgument
  where
    addTableRowArgument args@(RQL.FunctionArgsExp positional named) = case _cffTableArgument of
      FTAFirst               -> RQL.FunctionArgsExp (tableRowArgument : positional) named
      FTANamed argName index -> RQL.insertFunctionArg argName index tableRowArgument args
    tableRowArgument = RQL.AETableRow Nothing


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
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => ComputedFieldInfo
  -> SelPermInfo
  -> m (Maybe (FieldParser n AnnotatedField))
computedField ComputedFieldInfo{..} selectPermissions = runMaybeT do
  stringifyNum <- asks $ qcStringifyNum . getter
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
      selectionSetParser <- lift   $ tableSelectionSet tableName remotePerms Nothing
      let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
      pure $ P.subselection fieldName Nothing fieldArgsParser selectionSetParser <&>
        \((functionArgs', args), fields) ->
          RQL.FComputedField $ RQL.CFSTable RQL.JASMultipleRows $ RQL.AnnSelG
            { RQL._asnFields   = fields
            , RQL._asnFrom     = RQL.FromFunction (_cffName _cfiFunction) functionArgs' Nothing
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

{- Note [Relay Node Id]
~~~~~~~~~~~~~~~~~~~~~~~

The 'Node' interface in Relay schema has exactly one field which returns
a non-null 'ID' value. Each table object type in Relay schema should implement
'Node' interface to provide global object identification.
See https://relay.dev/graphql/objectidentification.htm for more details.

To identify each row in a table, we need to encode the table information
(schema and name) and primary key column values in the 'Node' id.

Node id data:
-------------
We are using JSON format for encoding and decoding the node id. The JSON
schema looks like following

'[<version-integer>, "<table-schema>", "<table-name>", "column-1", "column-2", ... "column-n"]'

It is represented in the type @'NodeId'. The 'version-integer' represents the JSON
schema version to enable any backward compatibility if it is broken in upcoming versions.

The stringified JSON is Base64 encoded and sent to client. Also the same
base64 encoded JSON string is accepted for 'node' field resolver's 'id' input.
-}

data NodeIdVersion
  = NIVersion1
  deriving (Show, Eq)

nodeIdVersionInt :: NodeIdVersion -> Int
nodeIdVersionInt NIVersion1 = 1

currentNodeIdVersion :: NodeIdVersion
currentNodeIdVersion = NIVersion1

instance J.FromJSON NodeIdVersion where
  parseJSON v = do
    versionInt :: Int <- J.parseJSON v
    case versionInt of
      1 -> pure NIVersion1
      _ -> fail $ "expecting version 1 for node id, but got " <> show versionInt

data V1NodeId
  = V1NodeId
  { _nidTable   :: !QualifiedTable
  , _nidColumns :: !(NESeq.NESeq J.Value)
  } deriving (Show, Eq)

-- | The Relay 'Node' inteface's 'id' field value.
-- See Note [Relay Node id].
data NodeId
  = NodeIdV1 !V1NodeId
  deriving (Show, Eq)

instance J.FromJSON NodeId where
  parseJSON v = do
    valueList <- J.parseJSON v
    case valueList of
      []              -> fail "unexpected GUID format, found empty list"
      J.Number 1:rest -> NodeIdV1 <$> parseNodeIdV1 rest
      J.Number n:_    -> fail $ "unsupported GUID version: " <> show n
      _               -> fail "unexpected GUID format, needs to start with a version number"
    where
      parseNodeIdV1 (schemaValue:(nameValue:(firstColumn:remainingColumns))) =
        V1NodeId
        <$> (QualifiedObject <$> J.parseJSON schemaValue <*> J.parseJSON nameValue)
        <*> pure (firstColumn NESeq.:<|| Seq.fromList remainingColumns)
      parseNodeIdV1 _ = fail "GUID version 1: expecting schema name, table name and at least one column value"

parseNodeId
  :: MonadParse n
  => Text
  -> n NodeId
parseNodeId =
  either (throwInvalidNodeId . T.pack) pure . J.eitherDecode . base64Decode

throwInvalidNodeId :: MonadParse m => Text -> m a
throwInvalidNodeId t = parseError $ "the node id is invalid: " <> t

-- | The 'node' root field of a Relay request.
node
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => HashSet QualifiedTable
  -> m (P.Parser 'Output n (HashMap QualifiedTable (SelPermInfo, NESeq PGColumnInfo, AnnotatedFields)))
node allTables = memoizeOn 'node allTables do
  myself <- node allTables
  let idDescription = G.Description "A globally unique identifier"
      idField = P.selection_ $$(G.litName "id") (Just idDescription) P.identifier
      nodeInterfaceDescription = G.Description "An object with globally unique ID"
  tables :: HashMap QualifiedTable (Parser 'Output n (SelPermInfo, NESeq PGColumnInfo, AnnotatedFields)) <-
    Map.mapMaybe id <$> flip Map.traverseWithKey (Set.toMap allTables) \table _ -> runMaybeT do
      tablePkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns) <$> askTableInfo table
      selectPermissions <- MaybeT $ tableSelectPermissions table
      annotatedFieldsParser <- lift $ tableSelectionSet table selectPermissions $ Just myself
      pure $ (selectPermissions, tablePkeyColumns,) <$> annotatedFieldsParser
  pure $ P.selectionSetInterface $$(G.litName "Node")
         (Just nodeInterfaceDescription) [idField] tables []

nodeField
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => HashSet QualifiedTable
  -> m (P.FieldParser n SelectExp)
nodeField allTables = do
  let idDescription = G.Description "A globally unique id"
      idArgument = P.field $$(G.litName "id") (Just idDescription) P.identifier
  stringifyNum <- asks $ qcStringifyNum . getter
  nodeObject <- node allTables
  return $ P.subselection $$(G.litName "node") Nothing idArgument nodeObject `P.bindField`
    \(ident, parseds) -> do
      NodeIdV1 (V1NodeId table columnValues) <- parseNodeId ident
      (perms, pkeyColumns, fields) <-
        onNothing (Map.lookup table parseds) $ throwInvalidNodeId $ "the table " <>> ident
      whereExp <- buildNodeIdBoolExp columnValues pkeyColumns
      return $ RQL.AnnSelG
        { RQL._asnFields   = fields
        , RQL._asnFrom     = RQL.FromTable table
        , RQL._asnPerm     = tablePermissionsInfo perms
        , RQL._asnArgs     = RQL.TableArgs
          { RQL._taWhere    = Just whereExp
          , RQL._taOrderBy  = Nothing
          , RQL._taLimit    = Nothing
          , RQL._taOffset   = Nothing
          , RQL._taDistCols = Nothing
          }
        , RQL._asnStrfyNum = stringifyNum
        }
  where
    buildNodeIdBoolExp
      :: NESeq.NESeq J.Value
      -> NESeq.NESeq PGColumnInfo
      -> n (RQL.AnnBoolExp UnpreparedValue)
    buildNodeIdBoolExp columnValues pkeyColumns = do
        let firstPkColumn NESeq.:<|| remainingPkColumns = pkeyColumns
            firstColumnValue NESeq.:<|| remainingColumns = columnValues
            (nonAlignedPkColumns, nonAlignedColumnValues, alignedTuples) =
              partitionThese $ toList $ align remainingPkColumns remainingColumns

        when (not $ null nonAlignedPkColumns) $ throwInvalidNodeId $
          "primary key columns " <> dquoteList (map pgiColumn nonAlignedPkColumns) <> " are missing"

        when (not $ null nonAlignedColumnValues) $ throwInvalidNodeId $
          "unexpected column values " <> J.encodeToStrictText nonAlignedColumnValues

        let allTuples = (firstPkColumn, firstColumnValue):alignedTuples

        either (throwInvalidNodeId . qeError) pure $ runExcept $
          fmap RQL.BoolAnd $ for allTuples $ \(columnInfo, columnValue) -> do
            let modifyErrFn t = "value of column " <> pgiColumn columnInfo
                                <<> " in node id: " <> t
                pgColumnType = pgiType columnInfo
            pgValue <- modifyErr modifyErrFn $ parsePGScalarValue pgColumnType columnValue
            let unpreparedValue = flip UVParameter Nothing $ P.PGColumnValue pgColumnType pgValue
            pure $ RQL.BoolFld $ RQL.AVCol columnInfo [RQL.AEQ True unpreparedValue]
