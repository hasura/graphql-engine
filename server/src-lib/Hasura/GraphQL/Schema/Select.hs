{-# LANGUAGE ViewPatterns #-}

-- | Generate table selection schema both for ordinary Hasura-type and
-- relay-type queries.  All schema with "relay" or "connection" in the name is
-- used exclusively by relay.
module Hasura.GraphQL.Schema.Select
  ( selectTable
  , selectTableByPk
  , selectTableAggregate
  , selectTableConnection
  , selectFunction
  , selectFunctionAggregate
  , selectFunctionConnection
  , computedFieldPG
  , remoteRelationshipFieldPG
  , tableArgs
  , tableSelectionSet
  , tableSelectionList
  , nodePG
  , nodeField
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                 as J
import qualified Data.Aeson.Extended                        as J
import qualified Data.Aeson.Internal                        as J
import qualified Data.ByteString.Lazy                       as BL
import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashSet                               as Set
import qualified Data.List.NonEmpty                         as NE
import qualified Data.Sequence                              as Seq
import qualified Data.Sequence.NonEmpty                     as NESeq
import qualified Data.Text                                  as T
import qualified Language.GraphQL.Draft.Syntax              as G

import           Control.Lens                               hiding (index)
import           Data.Has
import           Data.Int                                   (Int32)
import           Data.Parser.JSONPath
import           Data.Text.Extended
import           Data.Traversable                           (mapAccumL)

import qualified Hasura.Backends.Postgres.SQL.Types         as PG
import qualified Hasura.GraphQL.Execute.Types               as ET
import qualified Hasura.GraphQL.Parser                      as P
import qualified Hasura.GraphQL.Parser.Internal.Parser      as P
import qualified Hasura.RQL.IR.BoolExp                      as IR
import qualified Hasura.RQL.IR.OrderBy                      as IR
import qualified Hasura.RQL.IR.Select                       as IR
import qualified Hasura.SQL.AnyBackend                      as AB

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser                      (FieldParser, InputFieldsParser,
                                                             Kind (..), Parser,
                                                             UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Schema.Remote
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.DDL.RemoteRelationship.Validate
import           Hasura.RQL.Types
import           Hasura.Server.Utils                        (executeJSONPath)
import           Hasura.Session


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
-- > }: [table!]!
selectTable
  :: forall b m n r
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => TableName b          -- ^ full name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo b        -- ^ select permissions of the table
  -> m (FieldParser n (SelectExp b))
selectTable table fieldName description selectPermissions = memoizeOn 'selectTable (table, fieldName) do
  stringifyNum       <- asks $ qcStringifyNum . getter
  tableArgsParser    <- tableArgs table selectPermissions
  selectionSetParser <- tableSelectionList table selectPermissions
  pure $ P.subselection fieldName description tableArgsParser selectionSetParser
    <&> \(args, fields) -> IR.AnnSelectG
      { IR._asnFields   = fields
      , IR._asnFrom     = IR.FromTable table
      , IR._asnPerm     = tablePermissionsInfo selectPermissions
      , IR._asnArgs     = args
      , IR._asnStrfyNum = stringifyNum
      }

-- | Simple table connection selection.
--
-- The field for the table accepts table connection selection argument, and
-- expects a selection of connection fields
--
-- > table_name_connection(first: 1) {
-- >   pageInfo: {
-- >     hasNextPage: Boolean!
-- >     endCursor: String!
-- >   }
-- >   edges: {
-- >     cursor: String!
-- >     node: {
-- >       id: ID!
-- >       col1: col1_type
-- >       col2: col2_type
-- >     }
-- >   }
-- > }: table_nameConnection!
selectTableConnection
  :: forall m n r b
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => TableName b          -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> PrimaryKeyColumns b  -- ^ primary key columns
  -> SelPermInfo b        -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (ConnectionSelectExp b)))
selectTableConnection table fieldName description pkeyColumns selectPermissions = do
  xRelay <- asks $ backendRelay @b . getter
  for xRelay \xRelayInfo -> memoizeOn 'selectTableConnection (table, fieldName) do
    stringifyNum       <- asks $ qcStringifyNum . getter
    selectArgsParser   <- tableConnectionArgs pkeyColumns table selectPermissions
    selectionSetParser <- P.nonNullableParser <$> tableConnectionSelectionSet table selectPermissions
    pure $ P.subselection fieldName description selectArgsParser selectionSetParser
      <&> \((args, split, slice), fields) -> IR.ConnectionSelect
        { IR._csXRelay = xRelayInfo
        , IR._csPrimaryKeyColumns = pkeyColumns
        , IR._csSplit = split
        , IR._csSlice = slice
        , IR._csSelect = IR.AnnSelectG
          { IR._asnFields   = fields
          , IR._asnFrom     = IR.FromTable table
          , IR._asnPerm     = tablePermissionsInfo selectPermissions
          , IR._asnArgs     = args
          , IR._asnStrfyNum = stringifyNum
          }
        }

-- | Table selection by primary key.
--
-- > table_name(id: 42) {
-- >   col1: col1_type
-- >   col2: col2_type
-- > }: table
--
-- Returns Nothing if there's nothing that can be selected with
-- current permissions or if there are primary keys the user
-- doesn't have select permissions for.
selectTableByPk
  :: forall m n r b
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => TableName b          -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo b        -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (SelectExp b)))
selectTableByPk table fieldName description selectPermissions = runMaybeT do
  primaryKeys <- MaybeT $ fmap _pkColumns . _tciPrimaryKey . _tiCoreInfo <$> askTableInfo table
  guard $ all (\c -> pgiColumn c `Map.member` spiCols selectPermissions) primaryKeys
  lift $ memoizeOn 'selectTableByPk (table, fieldName) do
    stringifyNum <- asks $ qcStringifyNum . getter
    argsParser <- sequenceA <$> for primaryKeys \columnInfo -> do
      field <- columnParser (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
      pure $ BoolFld . AVCol columnInfo . pure . AEQ True . mkParameter <$>
        P.field (pgiName columnInfo) (pgiDescription columnInfo) field
    selectionSetParser <- tableSelectionSet table selectPermissions
    pure $ P.subselection fieldName description argsParser selectionSetParser
      <&> \(boolExpr, fields) ->
        let defaultPerms = tablePermissionsInfo selectPermissions
            -- Do not account permission limit since the result is just a nullable object
            permissions  = defaultPerms { IR._tpLimit = Nothing }
            whereExpr    = Just $ BoolAnd $ toList boolExpr
        in IR.AnnSelectG
             { IR._asnFields   = fields
             , IR._asnFrom     = IR.FromTable table
             , IR._asnPerm     = permissions
             , IR._asnArgs     = IR.noSelectArgs { IR._saWhere = whereExpr }
             , IR._asnStrfyNum = stringifyNum
             }

-- | Table aggregation selection
--
-- Parser for an aggregation selection of a table.
-- > table_aggregate(limit: 10) {
-- >   aggregate: table_aggregate_fields
-- >   nodes: [table!]!
-- > } :: table_aggregate!
--
-- Returns Nothing if there's nothing that can be selected with
-- current permissions.
selectTableAggregate
  :: forall m n r b
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => TableName b          -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo b        -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (AggSelectExp b)))
selectTableAggregate table fieldName description selectPermissions = runMaybeT $ do
  guard $ spiAllowAgg selectPermissions
  xNodesAgg <- MaybeT $ asks $ backendNodesAgg @b . getter
  lift $ memoizeOn 'selectTableAggregate (table, fieldName) do
    stringifyNum    <- asks $ qcStringifyNum . getter
    tableGQLName    <- getTableGQLName @b table
    tableArgsParser <- tableArgs table selectPermissions
    aggregateParser <- tableAggregationFields table selectPermissions
    nodesParser     <- tableSelectionList table selectPermissions
    let selectionName = tableGQLName <> $$(G.litName "_aggregate")
        aggregationParser = P.nonNullableParser $
          parsedSelectionsToFields IR.TAFExp <$>
          P.selectionSet selectionName (Just $ G.Description $ "aggregated selection of " <>> table)
          [ IR.TAFNodes xNodesAgg <$> P.subselection_ $$(G.litName "nodes") Nothing nodesParser
          , IR.TAFAgg <$> P.subselection_ $$(G.litName "aggregate") Nothing aggregateParser
          ]
    pure $ P.subselection fieldName description tableArgsParser aggregationParser
      <&> \(args, fields) -> IR.AnnSelectG
        { IR._asnFields   = fields
        , IR._asnFrom     = IR.FromTable table
        , IR._asnPerm     = tablePermissionsInfo selectPermissions
        , IR._asnArgs     = args
        , IR._asnStrfyNum = stringifyNum
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
  is not valid GraphQL.  Put differently: exposing a given field through
  introspection is not the same as claiming that there is a valid GraphQL query
  that selects that field.

- We only support tables that have at least one column (since we require primary
  keys), so that the admin role can select every table anyway.
-}

-- | Fields of a table
--
-- > type table{
-- >   # table columns
-- >   column_1: column1_type
-- >   .
-- >   column_n: columnn_type
-- >
-- >   # table relationships
-- >   object_relationship: remote_table
-- >   array_relationship: [remote_table!]!
-- >
-- >   # computed fields
-- >   computed_field: field_type
-- >
-- >   # remote relationships
-- >   remote_field: field_type
-- > }
tableSelectionSet
  :: forall m n r b
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => TableName b
  -> SelPermInfo b
  -> m (Parser 'Output n (AnnotatedFields b))
tableSelectionSet table selectPermissions = memoizeOn 'tableSelectionSet table do
  tableInfo    <- _tiCoreInfo <$> askTableInfo table
  tableGQLName <- getTableGQLName @b table
  let tableFields      = Map.elems  $ _tciFieldInfoMap tableInfo
      tablePkeyColumns = _pkColumns <$> _tciPrimaryKey tableInfo
      description      = Just $ mkDescriptionWith (_tciDescription tableInfo) $
                         "columns and relationships of " <>> table
  fieldParsers <- concat <$> for tableFields \fieldInfo ->
    fieldSelection table tablePkeyColumns fieldInfo selectPermissions

  -- We don't check *here* that the subselection set is non-empty,
  -- even though the GraphQL specification requires that it is (see
  -- Note [Selectability of tables]). However, the GraphQL parser
  -- enforces that a selection set, if present, is non-empty; and our
  -- parser later verifies that a selection set is present if
  -- required, meaning that not having this check here does not allow
  -- for the construction of invalid queries.

  queryType <- asks $ qcQueryType . getter
  xRelay    <- asks $ backendRelay @b . getter
  case (queryType, tablePkeyColumns, xRelay) of
    -- A relay table
    (ET.QueryRelay, Just pkeyColumns, Just xRelayInfo) -> do
      let nodeIdFieldParser =
            P.selection_ $$(G.litName "id") Nothing P.identifier $> IR.AFNodeId xRelayInfo table pkeyColumns
          allFieldParsers = fieldParsers <> [nodeIdFieldParser]
      nodeInterface <- node @b
      pure $ P.selectionSetObject tableGQLName description allFieldParsers [nodeInterface]
            <&> parsedSelectionsToFields IR.AFExpression
    _ ->
      pure $ P.selectionSetObject tableGQLName description fieldParsers []
            <&> parsedSelectionsToFields IR.AFExpression


-- | List of table fields object.
-- Just a @'nonNullableObjectList' wrapper over @'tableSelectionSet'.
-- > table_name: [table!]!
tableSelectionList
  :: ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => TableName b
  -> SelPermInfo b
  -> m (Parser 'Output n (AnnotatedFields b))
tableSelectionList table selectPermissions =
  nonNullableObjectList <$> tableSelectionSet table selectPermissions

-- | Converts an output type parser from object_type to [object_type!]!
nonNullableObjectList :: Parser 'Output m a -> Parser 'Output m a
nonNullableObjectList =
  P.nonNullableParser . P.multiple . P.nonNullableParser

-- | Connection fields of a table
--
-- > type tableConnection{
-- >   pageInfo: PageInfo!
-- >   edges: [tableEdge!]!
-- > }
--
-- > type PageInfo{
-- >   startCursor: String!
-- >   endCursor: String!
-- >   hasNextPage: Boolean!
-- >   hasPreviousPage: Boolean!
-- > }
--
-- > type tableEdge{
-- >   cursor: String!
-- >   node: table!
-- > }
tableConnectionSelectionSet
  :: forall m n r b
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => TableName b
  -> SelPermInfo b
  -> m (Parser 'Output n (IR.ConnectionFields b (UnpreparedValue b)))
tableConnectionSelectionSet table selectPermissions = memoizeOn 'tableConnectionSelectionSet table do
  edgesParser  <- tableEdgesSelectionSet
  tableGQLName <- getTableGQLName @b table
  let connectionTypeName = tableGQLName <> $$(G.litName "Connection")
      pageInfo = P.subselection_ $$(G.litName "pageInfo") Nothing
                 pageInfoSelectionSet <&> IR.ConnectionPageInfo
      edges = P.subselection_ $$(G.litName "edges") Nothing
              edgesParser <&> IR.ConnectionEdges
      connectionDescription = G.Description $ "A Relay connection object on " <>> table
  pure $ P.nonNullableParser $
         P.selectionSet connectionTypeName (Just connectionDescription) [pageInfo, edges]
         <&> parsedSelectionsToFields IR.ConnectionTypename
  where
    pageInfoSelectionSet :: Parser 'Output n IR.PageInfoFields
    pageInfoSelectionSet =
      let startCursorField     = P.selection_ $$(G.litName "startCursor") Nothing
                                 P.string $> IR.PageInfoStartCursor
          endCursorField       = P.selection_ $$(G.litName "endCursor") Nothing
                                 P.string $> IR.PageInfoEndCursor
          hasNextPageField     = P.selection_ $$(G.litName "hasNextPage") Nothing
                                 P.boolean $> IR.PageInfoHasNextPage
          hasPreviousPageField = P.selection_ $$(G.litName "hasPreviousPage") Nothing
                                 P.boolean $> IR.PageInfoHasPreviousPage
          allFields            =
            [ startCursorField, endCursorField
            , hasNextPageField, hasPreviousPageField
            ]
      in P.nonNullableParser $ P.selectionSet $$(G.litName "PageInfo") Nothing allFields
         <&> parsedSelectionsToFields IR.PageInfoTypename

    tableEdgesSelectionSet
      :: m (Parser 'Output n (IR.EdgeFields b (UnpreparedValue b)))
    tableEdgesSelectionSet = do
      tableGQLName        <- getTableGQLName @b table
      edgeNodeParser      <- P.nonNullableParser <$> tableSelectionSet table selectPermissions
      let edgesType = tableGQLName <> $$(G.litName "Edge")
          cursor    = P.selection_ $$(G.litName "cursor") Nothing
                      P.string $> IR.EdgeCursor
          edgeNode  = P.subselection_ $$(G.litName "node") Nothing
                      edgeNodeParser <&> IR.EdgeNode
      pure $ nonNullableObjectList $ P.selectionSet edgesType Nothing [cursor, edgeNode]
             <&> parsedSelectionsToFields IR.EdgeTypename

-- | User-defined function (AKA custom function)
selectFunction
  :: forall b m n r
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => FunctionInfo b       -- ^ SQL function info
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo b        -- ^ select permissions of the target table
  -> m (FieldParser n (SelectExp b))
selectFunction function fieldName description selectPermissions = do
  stringifyNum <- asks $ qcStringifyNum . getter
  let table = _fiReturnType function
  tableArgsParser    <- tableArgs table selectPermissions
  functionArgsParser <- customSQLFunctionArgs function
  selectionSetParser <- tableSelectionList table selectPermissions
  let argsParser = liftA2 (,) functionArgsParser tableArgsParser
  pure $ P.subselection fieldName description argsParser selectionSetParser
    <&> \((funcArgs, tableArgs'), fields) -> IR.AnnSelectG
      { IR._asnFields   = fields
      , IR._asnFrom     = IR.FromFunction (_fiName function) funcArgs Nothing
      , IR._asnPerm     = tablePermissionsInfo selectPermissions
      , IR._asnArgs     = tableArgs'
      , IR._asnStrfyNum = stringifyNum
      }

selectFunctionAggregate
  :: forall b m n r
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => FunctionInfo b       -- ^ SQL function info
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo b        -- ^ select permissions of the target table
  -> m (Maybe (FieldParser n (AggSelectExp b)))
selectFunctionAggregate function fieldName description selectPermissions = runMaybeT do
  let table = _fiReturnType function
  guard $ spiAllowAgg selectPermissions
  stringifyNum       <- asks $ qcStringifyNum . getter
  xNodesAgg          <- MaybeT $ asks $ backendNodesAgg @b . getter
  tableGQLName       <- getTableGQLName @b table
  tableArgsParser    <- lift $ tableArgs table selectPermissions
  functionArgsParser <- lift $ customSQLFunctionArgs function
  aggregateParser    <- lift $ tableAggregationFields table selectPermissions
  selectionName      <- lift $ pure tableGQLName <&> (<> $$(G.litName "_aggregate"))
  nodesParser        <- lift $ tableSelectionList table selectPermissions
  let argsParser = liftA2 (,) functionArgsParser tableArgsParser
      aggregationParser = fmap (parsedSelectionsToFields IR.TAFExp) $
        P.nonNullableParser $
        P.selectionSet selectionName Nothing
        [ IR.TAFNodes xNodesAgg <$> P.subselection_ $$(G.litName "nodes") Nothing nodesParser
        , IR.TAFAgg <$> P.subselection_ $$(G.litName "aggregate") Nothing aggregateParser
        ]
  pure $ P.subselection fieldName description argsParser aggregationParser
    <&> \((funcArgs, tableArgs'), fields) -> IR.AnnSelectG
      { IR._asnFields   = fields
      , IR._asnFrom     = IR.FromFunction (_fiName function) funcArgs Nothing
      , IR._asnPerm     = tablePermissionsInfo selectPermissions
      , IR._asnArgs     = tableArgs'
      , IR._asnStrfyNum = stringifyNum
      }

selectFunctionConnection
  :: ( BackendSchema 'Postgres
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => FunctionInfo 'Postgres       -- ^ SQL function info
  -> G.Name                       -- ^ field display name
  -> Maybe G.Description          -- ^ field description, if any
  -> PrimaryKeyColumns 'Postgres  -- ^ primary key columns of the target table
  -> SelPermInfo 'Postgres        -- ^ select permissions of the target table
  -> m (Maybe (FieldParser n (ConnectionSelectExp 'Postgres)))
selectFunctionConnection function fieldName description pkeyColumns selectPermissions = do
  xRelay <- asks $ backendRelay @'Postgres . getter
  for xRelay \xRelayInfo -> do
    stringifyNum <- asks $ qcStringifyNum . getter
    let table = _fiReturnType function
    tableConnectionArgsParser <- tableConnectionArgs pkeyColumns table selectPermissions
    functionArgsParser <- customSQLFunctionArgs function
    selectionSetParser <- tableConnectionSelectionSet table selectPermissions
    let argsParser = liftA2 (,) functionArgsParser tableConnectionArgsParser
    pure $ P.subselection fieldName description argsParser selectionSetParser
      <&> \((funcArgs, (args, split, slice)), fields) -> IR.ConnectionSelect
        { IR._csXRelay = xRelayInfo
        , IR._csPrimaryKeyColumns = pkeyColumns
        , IR._csSplit  = split
        , IR._csSlice  = slice
        , IR._csSelect = IR.AnnSelectG
          { IR._asnFields   = fields
          , IR._asnFrom     = IR.FromFunction (_fiName function) funcArgs Nothing
          , IR._asnPerm     = tablePermissionsInfo selectPermissions
          , IR._asnArgs     = args
          , IR._asnStrfyNum = stringifyNum
          }
        }



-- 2. local parsers
-- Parsers that are used but not exported: sub-components

-- | Argument to filter rows returned from table selection
-- > where: table_bool_exp
tableWhere
  :: forall b r m n. MonadBuildSchema b r m n
  => TableName b
  -> SelPermInfo b
  -> m (InputFieldsParser n (Maybe (IR.AnnBoolExp b (UnpreparedValue b))))
tableWhere table selectPermissions = do
  boolExpParser <- boolExp table (Just selectPermissions)
  pure $ fmap join $
    P.fieldOptional whereName whereDesc $ P.nullable boolExpParser
  where
    whereName      = $$(G.litName "where")
    whereDesc      = Just $ G.Description "filter the rows returned"

-- | Argument to sort rows returned from table selection
-- > order_by: [table_order_by!]
tableOrderBy
  :: forall m n r b. (BackendSchema b, MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => TableName b
  -> SelPermInfo b
  -> m (InputFieldsParser n (Maybe (NonEmpty (IR.AnnOrderByItemG b (UnpreparedValue b)))))
tableOrderBy table selectPermissions = do
  orderByParser <- orderByExp table selectPermissions
  pure $ do
    maybeOrderByExps <- fmap join $
      P.fieldOptional orderByName orderByDesc $ P.nullable $ P.list orderByParser
    pure $ maybeOrderByExps >>= NE.nonEmpty . concat
  where
    orderByName = $$(G.litName "order_by")
    orderByDesc = Just $ G.Description "sort the rows by one or more columns"

-- | Arguments for a table selection
--
-- > distinct_on: [table_select_column!]
-- > limit: Int
-- > offset: Int
-- > order_by: [table_order_by!]
-- > where: table_bool_exp
tableArgs
  :: forall b r m n. MonadBuildSchema b r m n
  => TableName b
  -> SelPermInfo b
  -> m (InputFieldsParser n (SelectArgs b))
tableArgs table selectPermissions = do
  whereParser    <- tableWhere table selectPermissions
  orderByParser  <- tableOrderBy table selectPermissions
  distinctParser <- tableDistinctOn table selectPermissions
  let selectArgs = do
        whereF   <- whereParser
        orderBy  <- orderByParser
        limit    <- fmap join $ P.fieldOptional limitName   limitDesc   $ P.nullable positiveInt
        offset   <- fmap join $ P.fieldOptional offsetName  offsetDesc  $ P.nullable (offsetParser @b)
        distinct <- distinctParser
        pure $ IR.SelectArgs
          { IR._saWhere    = whereF
          , IR._saOrderBy  = orderBy
          , IR._saLimit    = fromIntegral <$> limit
          , IR._saOffset   = offset
          , IR._saDistinct = distinct
          }
  pure $ selectArgs `P.bindFields`
   \args -> do
      traverse_ (validateDistinctOn $ IR._saOrderBy args) $ snd <$> IR._saDistinct args
      pure args
  where
    -- TH splices mess up ApplicativeDo
    -- see (FIXME: link to bug here)
    limitName  = $$(G.litName "limit")
    offsetName = $$(G.litName "offset")
    limitDesc  = Just $ G.Description "limit the number of rows returned"
    offsetDesc = Just $ G.Description "skip the first n rows. Use only with order_by"

    validateDistinctOn Nothing _ = return ()
    validateDistinctOn (Just orderByCols) distinctOnCols = do
      let colsLen = length distinctOnCols
          initOrderBys = take colsLen $ NE.toList orderByCols
          initOrdByCols = flip mapMaybe initOrderBys $ \ob ->
            case IR.obiColumn ob of
              IR.AOCColumn pgCol -> Just $ pgiColumn pgCol
              _                  -> Nothing
          isValid = (colsLen == length initOrdByCols)
                    && all (`elem` initOrdByCols) (toList distinctOnCols)
      unless isValid $ parseError
        "\"distinct_on\" columns must match initial \"order_by\" columns"

-- TODO:
-- this should either be moved to Common, or to Parser itself; even better,
-- we could think of exposing a "PositiveInt" custom scalar type in the schema
positiveInt :: MonadParse n => Parser 'Both n Int32
positiveInt = P.int `P.bind` \value -> do
  when (value < 0) $ parseErrorWith NotSupported "unexpected negative value"
  pure value

-- | Arguments for a table connection selection
--
-- > distinct_on: [table_select_column!]
-- > order_by: [table_order_by!]
-- > where: table_bool_exp
-- > first: Int
-- > last: Int
-- > before: String
-- > after: String
tableConnectionArgs
  :: forall b r m n. MonadBuildSchema b r m n
  => PrimaryKeyColumns b
  -> TableName b
  -> SelPermInfo b
  -> m ( InputFieldsParser n
         ( SelectArgs b
         , Maybe (NonEmpty (IR.ConnectionSplit b (UnpreparedValue b)))
         , Maybe IR.ConnectionSlice
         )
       )
tableConnectionArgs pkeyColumns table selectPermissions = do
  whereParser <- tableWhere table selectPermissions
  orderByParser <- fmap (fmap appendPrimaryKeyOrderBy) <$> tableOrderBy table selectPermissions
  distinctParser <- tableDistinctOn table selectPermissions
  let maybeFirst = fmap join $ P.fieldOptional $$(G.litName "first")
                   Nothing $ P.nullable positiveInt
      maybeLast = fmap join $ P.fieldOptional $$(G.litName "last")
                  Nothing $ P.nullable positiveInt
      maybeAfter = fmap join $ P.fieldOptional $$(G.litName "after")
                 Nothing $ P.nullable base64Text
      maybeBefore = fmap join $ P.fieldOptional $$(G.litName "before")
                  Nothing $ P.nullable base64Text
      firstAndLast = (,) <$> maybeFirst <*> maybeLast
      afterBeforeAndOrderBy = (,,) <$> maybeAfter <*> maybeBefore <*> orderByParser

  pure $ do
    whereF <- whereParser
    orderBy <- orderByParser
    distinct <- distinctParser
    split <- afterBeforeAndOrderBy `P.bindFields` \(after, before, orderBy') -> do
      rawSplit <- case (after, before) of
        (Nothing, Nothing) -> pure Nothing
        (Just _, Just _)   -> parseError "\"after\" and \"before\" are not allowed at once"
        (Just v, Nothing)  -> pure $ Just (IR.CSKAfter, v)
        (Nothing, Just v)  -> pure $ Just (IR.CSKBefore, v)
      for rawSplit (uncurry (parseConnectionSplit orderBy'))

    slice <- firstAndLast `P.bindFields` \case
      (Nothing, Nothing) -> pure Nothing
      (Just _, Just _)   -> parseError "\"first\" and \"last\" are not allowed at once"
      (Just v, Nothing)  -> pure $ Just $ IR.SliceFirst $ fromIntegral v
      (Nothing, Just v)  -> pure $ Just $ IR.SliceLast $ fromIntegral v

    pure ( IR.SelectArgs whereF orderBy Nothing Nothing distinct
         , split
         , slice
         )
  where
    base64Text = base64Decode <$> P.string

    appendPrimaryKeyOrderBy :: NonEmpty (IR.AnnOrderByItemG b v) -> NonEmpty (IR.AnnOrderByItemG b v)
    appendPrimaryKeyOrderBy orderBys@(h NE.:| t) =
      let orderByColumnNames =
            orderBys ^.. traverse . to IR.obiColumn . IR._AOCColumn . to pgiColumn
          pkeyOrderBys = flip mapMaybe (toList pkeyColumns) $ \pgColumnInfo ->
                         if pgiColumn pgColumnInfo `elem` orderByColumnNames then Nothing
                         else Just $ IR.OrderByItemG Nothing (IR.AOCColumn pgColumnInfo) Nothing
      in h NE.:| (t <> pkeyOrderBys)

    parseConnectionSplit
      :: Maybe (NonEmpty (IR.AnnOrderByItemG b (UnpreparedValue b)))
      -> IR.ConnectionSplitKind
      -> BL.ByteString
      -> n (NonEmpty (IR.ConnectionSplit b (UnpreparedValue b)))
    parseConnectionSplit maybeOrderBys splitKind cursorSplit = do
      cursorValue <- J.eitherDecode cursorSplit `onLeft` const throwInvalidCursor
      case maybeOrderBys of
        Nothing -> forM (NESeq.toNonEmpty pkeyColumns) $
          \pgColumnInfo -> do
            let columnJsonPath = [J.Key $ toTxt $ pgiColumn pgColumnInfo]
                columnType = pgiType pgColumnInfo
            pgColumnValue <- iResultToMaybe (executeJSONPath columnJsonPath cursorValue)
              `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parseScalarValueColumnType columnType pgColumnValue
            let unresolvedValue = UVParameter Nothing $ ColumnValue columnType pgValue
            pure $ IR.ConnectionSplit splitKind unresolvedValue $
                   IR.OrderByItemG Nothing (IR.AOCColumn pgColumnInfo) Nothing
        Just orderBys ->
          forM orderBys $ \orderBy -> do
            let IR.OrderByItemG orderType annObCol nullsOrder = orderBy
                columnType = getOrderByColumnType annObCol
            orderByItemValue <- iResultToMaybe (executeJSONPath (getPathFromOrderBy annObCol) cursorValue)
              `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parseScalarValueColumnType columnType orderByItemValue
            let unresolvedValue = UVParameter Nothing $ ColumnValue columnType pgValue
            pure $ IR.ConnectionSplit splitKind unresolvedValue $
                   IR.OrderByItemG orderType (() <$ annObCol) nullsOrder
      where
        throwInvalidCursor = parseError "the \"after\" or \"before\" cursor is invalid"
        liftQErr = either (parseError . qeError) pure . runExcept

        getPathFromOrderBy = \case
          IR.AOCColumn pgColInfo ->
            let pathElement = J.Key $ toTxt $ pgiColumn pgColInfo
            in [pathElement]
          IR.AOCObjectRelation relInfo _ obCol ->
            let pathElement = J.Key $ relNameToTxt $ riName relInfo
            in pathElement : getPathFromOrderBy obCol
          IR.AOCArrayAggregation relInfo _ aggOb ->
            let fieldName = J.Key $ relNameToTxt (riName relInfo) <> "_aggregate"
            in fieldName : case aggOb of
                 IR.AAOCount    -> [J.Key "count"]
                 IR.AAOOp t col -> [J.Key t, J.Key $ toTxt $ pgiColumn col]

        getOrderByColumnType = \case
          IR.AOCColumn pgColInfo -> pgiType pgColInfo
          IR.AOCObjectRelation _ _ obCol -> getOrderByColumnType obCol
          IR.AOCArrayAggregation _ _ aggOb ->
            case aggOb of
              IR.AAOCount        -> ColumnScalar (aggregateOrderByCountType @b)
              IR.AAOOp _ colInfo -> pgiType colInfo

-- | Aggregation fields
--
-- > type table_aggregate_fields{
-- >   count(distinct: Boolean, columns: [table_select_column!]): Int!
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
  :: forall m n r b. (BackendSchema b, MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => TableName b
  -> SelPermInfo b
  -> m (Parser 'Output n (IR.AggregateFields b))
tableAggregationFields table selectPermissions = memoizeOn 'tableAggregationFields table do
  tableGQLName  <- getTableGQLName @b table
  allColumns <- tableSelectColumns table selectPermissions
  let numericColumns   = onlyNumCols allColumns
      comparableColumns  = onlyComparableCols allColumns
      selectName   = tableGQLName <> $$(G.litName "_aggregate_fields")
      description  = G.Description $ "aggregate fields of " <>> table
  count     <- countField
  numericAndComparable <- fmap concat $ sequenceA $ catMaybes
    [ -- operators on numeric columns
      if null numericColumns then Nothing else Just $
        for numericAggOperators $ \operator -> do
          numFields <- mkNumericAggFields operator numericColumns
          pure $ parseAggOperator operator tableGQLName numFields
    , -- operators on comparable columns
      if null comparableColumns then Nothing else Just $ do
        comparableFields <- traverse mkColumnAggField comparableColumns
        pure $ comparisonAggOperators & map \operator ->
               parseAggOperator operator tableGQLName comparableFields
    ]
  let aggregateFields = count : numericAndComparable
  pure $ P.selectionSet selectName (Just description) aggregateFields
        <&> parsedSelectionsToFields IR.AFExp
  where
    mkNumericAggFields :: G.Name -> [ColumnInfo b] -> m [FieldParser n (IR.ColFld b)]
    mkNumericAggFields name
      | name == $$(G.litName "sum") = traverse mkColumnAggField
      | otherwise                   = traverse \columnInfo ->
          pure $ P.selection_
            (pgiName columnInfo)
            (pgiDescription columnInfo)
            (P.nullable P.float)
            $> IR.CFCol (pgiColumn columnInfo) (pgiType columnInfo)

    mkColumnAggField :: ColumnInfo b -> m (FieldParser n (IR.ColFld b))
    mkColumnAggField columnInfo = do
      field <- columnParser (pgiType columnInfo) (G.Nullability True)
      pure $ P.selection_
        (pgiName columnInfo)
        (pgiDescription columnInfo) field
        $> IR.CFCol (pgiColumn columnInfo) (pgiType columnInfo)

    countField :: m (FieldParser n (IR.AggregateField b))
    countField = do
      columnsEnum <- tableSelectColumnsEnum table selectPermissions
      let columnsName  = $$(G.litName "columns")
          distinctName = $$(G.litName "distinct")
          args = do
            distinct <- P.fieldOptional distinctName Nothing P.boolean
            columns  <- maybe (pure Nothing) (P.fieldOptional columnsName Nothing . P.list) columnsEnum
            pure $ mkCountType @b distinct columns
      pure $ IR.AFCount <$> P.selection $$(G.litName "count") Nothing args P.int

    parseAggOperator
      :: G.Name
      -> G.Name
      -> [FieldParser n (IR.ColFld b)]
      -> FieldParser n (IR.AggregateField b)
    parseAggOperator operator tableGQLName columns =
      let opText  = G.unName operator
          setName = tableGQLName <> $$(G.litName "_") <> operator <> $$(G.litName "_fields")
          setDesc = Just $ G.Description $ "aggregate " <> opText <> " on columns"
          subselectionParser = P.selectionSet setName setDesc columns
            <&> parsedSelectionsToFields IR.CFExp
      in P.subselection_ operator Nothing subselectionParser
         <&> (IR.AFOp . IR.AggregateOp opText)

lookupRemoteField'
  :: (MonadSchema n m, MonadError QErr m)
  => [P.Definition P.FieldInfo]
  -> FieldCall
  -> m P.FieldInfo
lookupRemoteField' fieldInfos (FieldCall fcName _) =
  case find ((== fcName) . P.dName) fieldInfos of
    Nothing -> throw400 RemoteSchemaError $ "field with name " <> fcName <<> " not found"
    Just (P.Definition _ _ _ fieldInfo) -> pure fieldInfo

lookupRemoteField
  :: (MonadSchema n m, MonadError QErr m)
  => [P.Definition P.FieldInfo]
  -> NonEmpty FieldCall
  -> m P.FieldInfo
lookupRemoteField fieldInfos (fieldCall :| rest) =
  case NE.nonEmpty rest of
    Nothing -> lookupRemoteField' fieldInfos fieldCall
    Just rest' -> do
      (P.FieldInfo _ type') <- lookupRemoteField' fieldInfos fieldCall
      (P.Definition _ _ _ (P.ObjectInfo objFieldInfos _))
       <- onNothing (P.getObjectInfo type') $
          throw400 RemoteSchemaError $ "field " <> fcName fieldCall <<> " is expected to be an object"
      lookupRemoteField objFieldInfos rest'

-- | An individual field of a table
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection
  :: forall b m n r
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => TableName b
  -> Maybe (PrimaryKeyColumns b)
  -> FieldInfo b
  -> SelPermInfo b
  -> m [FieldParser n (AnnotatedField b)]
fieldSelection table maybePkeyColumns fieldInfo selectPermissions = do
  case fieldInfo of
    FIColumn columnInfo -> maybeToList <$> runMaybeT do
      queryType <- asks $ qcQueryType . getter
      let columnName = pgiColumn columnInfo
          fieldName = pgiName columnInfo
      if | fieldName == $$(G.litName "id") && queryType == ET.QueryRelay -> do
             xRelayInfo  <- MaybeT $ asks $ backendRelay @b . getter
             pkeyColumns <- hoistMaybe maybePkeyColumns
             pure $ P.selection_ fieldName Nothing P.identifier
                    $> IR.AFNodeId xRelayInfo table pkeyColumns
         | otherwise -> do
             guard $ columnName `Map.member` (spiCols selectPermissions)
             let caseBoolExp = join $ Map.lookup columnName (spiCols selectPermissions)
             let caseBoolExpUnpreparedValue =
                   fmapAnnColumnCaseBoolExp partialSQLExpToUnpreparedValue <$> caseBoolExp
             let pathArg = jsonPathArg $ pgiType columnInfo
                 -- In an inherited role, when a column is part of all the select
                 -- permissions which make up the inherited role then the nullability
                 -- of the field is determined by the nullability of the DB column
                 -- otherwise it is marked as nullable explicitly, ignoring the column's
                 -- nullability. We do this because
                 -- in multiple roles we execute an SQL query like:
                 --
                 --  select
                 --    (case when (P1 or P2) then addr else null end) as addr,
                 --    (case when P2 then phone else null end) as phone
                 -- from employee
                 -- where (P1 or P2)
                 --
                 -- In the above example, P(n) is a predicate configured for a role
                 --
                 -- NOTE: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/FGALanguageICDE07.pdf
                 -- The above is the paper which talks about the idea of cell-level
                 -- authorization and multiple roles. The paper says that we should only
                 -- allow the case analysis only on nullable columns.
                 nullability = pgiIsNullable columnInfo || isJust caseBoolExp
             field <- lift $ columnParser (pgiType columnInfo) (G.Nullability nullability)
             pure $ P.selection fieldName (pgiDescription columnInfo) pathArg field
               <&> IR.mkAnnColumnField columnInfo caseBoolExpUnpreparedValue

    FIRelationship relationshipInfo ->
      concat . maybeToList <$> relationshipField relationshipInfo

    FIComputedField computedFieldInfo ->
      maybeToList <$> computedField computedFieldInfo selectPermissions

    FIRemoteRelationship remoteFieldInfo  ->
      concat . maybeToList <$> remoteRelationshipField remoteFieldInfo

-- | Field parsers for a table relationship
relationshipField
  :: forall b m n r
   . ( BackendSchema b
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension b) r
     )
  => RelInfo b
  -> m (Maybe [FieldParser n (AnnotatedField b)])
relationshipField relationshipInfo = runMaybeT do
  let otherTable = riRTable     relationshipInfo
      colMapping = riMapping    relationshipInfo
      relName    = riName       relationshipInfo
      nullable   = riIsNullable relationshipInfo
  remotePerms  <- MaybeT $ tableSelectPermissions otherTable
  relFieldName <- lift $ textToName $ relNameToTxt relName
  case riType relationshipInfo of
    ObjRel -> do
      let desc = Just $ G.Description "An object relationship"
      selectionSetParser <- lift $ tableSelectionSet otherTable remotePerms
      pure $ pure $ (if nullable then id else P.nonNullableField) $
        P.subselection_ relFieldName desc selectionSetParser
             <&> \fields -> IR.AFObjectRelation $ IR.AnnRelationSelectG relName colMapping $
                    IR.AnnObjectSelectG fields otherTable $
                    IR._tpFilter $ tablePermissionsInfo remotePerms
    ArrRel -> do
      let arrayRelDesc = Just $ G.Description "An array relationship"
      otherTableParser <- lift $ selectTable otherTable relFieldName arrayRelDesc remotePerms
      let arrayRelField = otherTableParser <&> \selectExp -> IR.AFArrayRelation $
            IR.ASSimple $ IR.AnnRelationSelectG relName colMapping selectExp
          relAggFieldName = relFieldName <> $$(G.litName "_aggregate")
          relAggDesc      = Just $ G.Description "An aggregate relationship"
      remoteAggField <- lift $ selectTableAggregate otherTable relAggFieldName relAggDesc remotePerms
      remoteConnectionField <- runMaybeT $ do
        -- Parse array connection field only for relay schema
        queryType <- asks $ qcQueryType . getter
        guard $ queryType == ET.QueryRelay
        xRelayInfo  <- MaybeT $ asks $ backendRelay @b . getter
        pkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns)
                       <$> askTableInfo otherTable
        let relConnectionName = relFieldName <> $$(G.litName "_connection")
            relConnectionDesc = Just $ G.Description "An array relationship connection"
        MaybeT $ lift $ selectTableConnection otherTable relConnectionName relConnectionDesc pkeyColumns remotePerms
      pure $ catMaybes [ Just arrayRelField
                       , fmap (IR.AFArrayRelation . IR.ASAggregate . IR.AnnRelationSelectG relName colMapping) <$> remoteAggField
                       , fmap (IR.AFArrayRelation . IR.ASConnection . IR.AnnRelationSelectG relName colMapping) <$> remoteConnectionField
                       ]

-- | Computed field parser
computedFieldPG
  :: forall m n r
   . ( BackendSchema 'Postgres
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => ComputedFieldInfo 'Postgres
  -> SelPermInfo 'Postgres
  -> m (Maybe (FieldParser n (AnnotatedField 'Postgres)))
computedFieldPG ComputedFieldInfo{..} selectPermissions = runMaybeT do
  stringifyNum <- asks $ qcStringifyNum . getter
  fieldName <- lift $ textToName $ computedFieldNameToText _cfiName
  functionArgsParser <- lift $ computedFieldFunctionArgs _cfiFunction
  case _cfiReturnType of
    CFRScalar scalarReturnType -> do
      caseBoolExpMaybe <-
        onNothing
          (Map.lookup _cfiName (spiScalarComputedFields selectPermissions))
          (hoistMaybe Nothing)
      let caseBoolExpUnpreparedValue =
            fmapAnnColumnCaseBoolExp partialSQLExpToUnpreparedValue <$> caseBoolExpMaybe
          fieldArgsParser = do
            args  <- functionArgsParser
            colOp <- jsonPathArg $ ColumnScalar scalarReturnType
            pure $ IR.AFComputedField _cfiXComputedFieldInfo
                      (IR.CFSScalar (IR.ComputedFieldScalarSelect
                       { IR._cfssFunction  = _cffName _cfiFunction
                       , IR._cfssType      = scalarReturnType
                       , IR._cfssColumnOp  = colOp
                       , IR._cfssArguments = args
                       })
                      caseBoolExpUnpreparedValue)
      dummyParser <- lift $ columnParser @'Postgres (ColumnScalar scalarReturnType) (G.Nullability True)
      pure $ P.selection fieldName (Just fieldDescription) fieldArgsParser dummyParser
    CFRSetofTable tableName -> do
      remotePerms        <- MaybeT $ tableSelectPermissions tableName
      selectArgsParser   <- lift   $ tableArgs tableName remotePerms
      selectionSetParser <- lift   $ P.multiple . P.nonNullableParser <$> tableSelectionSet tableName remotePerms
      let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
      pure $ P.subselection fieldName (Just fieldDescription) fieldArgsParser selectionSetParser <&>
        \((functionArgs', args), fields) ->
          IR.AFComputedField _cfiXComputedFieldInfo $ IR.CFSTable JASMultipleRows $ IR.AnnSelectG
            { IR._asnFields   = fields
            , IR._asnFrom     = IR.FromFunction (_cffName _cfiFunction) functionArgs' Nothing
            , IR._asnPerm     = tablePermissionsInfo remotePerms
            , IR._asnArgs     = args
            , IR._asnStrfyNum = stringifyNum
            }
  where
    fieldDescription =
      let defaultDescription = "A computed field, executes function " <>> _cffName _cfiFunction
      in mkDescriptionWith (_cffDescription _cfiFunction) defaultDescription

    computedFieldFunctionArgs
      :: ComputedFieldFunction 'Postgres -> m (InputFieldsParser n (IR.FunctionArgsExpTableRow 'Postgres (UnpreparedValue 'Postgres)))
    computedFieldFunctionArgs ComputedFieldFunction{..} =
      functionArgs _cffName (IAUserProvided <$> _cffInputArgs) <&> fmap addTableAndSessionArgument
      where
        tableRowArgument = IR.AETableRow Nothing

        addTableAndSessionArgument args@(IR.FunctionArgsExp positional named) =
          let withTable = case _cffTableArgument of
                FTAFirst               -> IR.FunctionArgsExp (tableRowArgument : positional) named
                FTANamed argName index -> IR.insertFunctionArg argName index tableRowArgument args
              sessionArgVal = IR.AESession UVSession
          in
            case _cffSessionArgument of
              Nothing -> withTable
              Just (FunctionSessionArgument argName index) ->
                IR.insertFunctionArg argName index sessionArgVal withTable

-- | Remote relationship field parsers
remoteRelationshipFieldPG
  :: ( MonadSchema n m
     , MonadRole r m
     , MonadError QErr m
     , Has QueryContext r
     )
  => RemoteFieldInfo 'Postgres
  -> m (Maybe [FieldParser n (AnnotatedField 'Postgres)])
remoteRelationshipFieldPG remoteFieldInfo = runMaybeT do
  queryType                   <- asks $ qcQueryType . getter
  remoteRelationshipQueryCtx  <- asks $ qcRemoteRelationshipContext . getter
  -- https://github.com/hasura/graphql-engine/issues/5144
  -- The above issue is easily fixable by removing the following guard and 'MaybeT' monad transformation
  guard $ queryType == ET.QueryHasura
  let RemoteFieldInfo () name _params hasuraFields remoteFields
        remoteSchemaInfo remoteSchemaInputValueDefns remoteSchemaName (table, source) = remoteFieldInfo
  (roleIntrospectionResult, parsedIntrospection) <-
    -- The remote relationship field should not be accessible
    -- if the remote schema is not accessible to the said role
    hoistMaybe $ Map.lookup remoteSchemaName remoteRelationshipQueryCtx
  let fieldDefns = map P.fDefinition (piQuery parsedIntrospection)
  role <- askRoleName
  let hasuraFieldNames = Set.map (FieldName . PG.getPGColTxt . pgiColumn)  hasuraFields
      remoteRelationship = RemoteRelationship name source table hasuraFieldNames remoteSchemaName remoteFields
  (newInpValDefns, remoteFieldParamMap) <-
    if | role == adminRoleName ->
         -- we don't validate the remote relationship when the role is admin
         -- because it's already been validated, when the remote relationship
         -- was created
         pure (remoteSchemaInputValueDefns, _rfiParamMap remoteFieldInfo)
       | otherwise -> do
           roleRemoteField <-
             afold @(Either _) $
             validateRemoteRelationship remoteRelationship (remoteSchemaInfo, roleIntrospectionResult) $
             Set.toList hasuraFields
           pure $ (_rfiInputValueDefinitions roleRemoteField, _rfiParamMap roleRemoteField)
  let RemoteSchemaIntrospection typeDefns = irDoc roleIntrospectionResult
      -- add the new input value definitions created by the remote relationship
      -- to the existing schema introspection of the role
      remoteRelationshipIntrospection = RemoteSchemaIntrospection $ typeDefns <> newInpValDefns
  fieldName <- textToName $ remoteRelationshipNameToText $ _rfiName remoteFieldInfo
  -- This selection set parser, should be of the remote node's selection set parser, which comes
  -- from the fieldCall
  nestedFieldInfo <- lift $ lookupRemoteField fieldDefns $ unRemoteFields $ _rfiRemoteFields remoteFieldInfo
  case nestedFieldInfo of
    P.FieldInfo{ P.fType = fieldType } -> do
      let typeName = P.getName fieldType
      fieldTypeDefinition <- onNothing (lookupType (irDoc roleIntrospectionResult) typeName)
                             -- the below case will never happen because we get the type name
                             -- from the schema document itself i.e. if a field exists for the
                             -- given role, then it's return type also must exist
                             $ throw500 $ "unexpected: " <> typeName <<> " not found "
      -- These are the arguments that are given by the user while executing a query
      let remoteFieldUserArguments = map snd $ Map.toList remoteFieldParamMap
      remoteFld <-
        lift $ remoteField remoteRelationshipIntrospection fieldName Nothing remoteFieldUserArguments fieldTypeDefinition
      pure $ pure $ remoteFld
        `P.bindField` \G.Field{ G._fArguments = args, G._fSelectionSet = selSet } -> do
          let remoteArgs =
                Map.toList args <&> \(argName, argVal) -> IR.RemoteFieldArgument argName $ P.GraphQLValue $ argVal
          pure $ IR.AFRemote () $ IR.RemoteSelect
            { _rselArgs          = remoteArgs
            , _rselSelection     = selSet
            , _rselHasuraColumns = _rfiHasuraFields remoteFieldInfo
            , _rselFieldCall     = unRemoteFields $ _rfiRemoteFields remoteFieldInfo
            , _rselRemoteSchema  = _rfiRemoteSchema remoteFieldInfo
            }

-- | The custom SQL functions' input "args" field parser
-- > function_name(args: function_args)
customSQLFunctionArgs
  :: (BackendSchema b, MonadSchema n m, MonadTableInfo r m)
  => FunctionInfo b
  -> m (InputFieldsParser n (IR.FunctionArgsExpTableRow b (UnpreparedValue b)))
customSQLFunctionArgs FunctionInfo{..} = functionArgs _fiName _fiInputArgs

-- | Parses the arguments to the underlying sql function of a computed field or
--   a custom function. All arguments to the underlying sql function are parsed
--   as an "args" object. Named arguments are expected in a field with the same
--   name, while positional arguments are expected in an field named "arg_$n".
--   Note that collisions are possible, but ignored for now, if a named argument
--   is also named "arg_$n". (FIXME: link to an issue?)
--
--   If the function requires no argument, or if its only argument is not
--   user-provided (the session argument in the case of custom functions, the
--   table row argument in the case of computed fields), the args object will
--   be omitted.
functionArgs
  :: forall b m n r
  . ( BackendSchema b
    , MonadSchema n m
    , MonadTableInfo r m
    )
  => FunctionName b
  -> Seq.Seq (FunctionInputArgument b)
  -> m (InputFieldsParser n (IR.FunctionArgsExpTableRow b (UnpreparedValue b)))
functionArgs functionName (toList -> inputArgs) = do
  -- First, we iterate through the original sql arguments in order, to find the
  -- corresponding graphql names. At the same time, we create the input field
  -- parsers, in three groups: session argument, optional arguments, and
  -- mandatory arguments. Optional arguments have a default value, mandatory
  -- arguments don't.
  let (names, session, optional, mandatory) = mconcat $ snd $ mapAccumL splitArguments 1 inputArgs
      defaultArguments = IR.FunctionArgsExp (snd <$> session) Map.empty

  if | length session > 1 ->
         -- We somehow found more than one session argument; this should never
         -- happen and is an error on our side.
         throw500 "there shouldn't be more than one session argument"
     | null optional && null mandatory ->
         -- There are no user-provided arguments to the function: there will be
         -- no args field.
         pure $ pure defaultArguments
     | otherwise -> do
         -- There are user-provided arguments: we need to parse an args object.
         argumentParsers <- sequenceA $ optional <> mandatory
         objectName <- fmap (<> $$(G.litName "_args")) $ functionGraphQLName @b functionName `onLeft` throwError
         let fieldName    = $$(G.litName "args")
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
               -- We also fail if we find a mandatory argument that was not
               -- provided by the user.
               named <- Map.fromList . catMaybes <$> traverse (namedArgument foundArguments) left
               pure $ IR.FunctionArgsExp positional named

         pure $ P.field fieldName (Just fieldDesc) objectParser

  where
    sessionPlaceholder :: IR.ArgumentExp b (UnpreparedValue b)
    sessionPlaceholder = IR.AEInput P.UVSession

    splitArguments
      :: Int
      -> FunctionInputArgument b
      -> (Int, ( [Text] -- graphql names, in order
               , [(Text, IR.ArgumentExp b (UnpreparedValue b))] -- session argument
               , [m (InputFieldsParser n (Maybe (Text, IR.ArgumentExp b (UnpreparedValue b))))] -- optional argument
               , [m (InputFieldsParser n (Maybe (Text, IR.ArgumentExp b (UnpreparedValue b))))] -- mandatory argument
               )
         )
    splitArguments positionalIndex (IASessionVariables name) =
      let argName = getFuncArgNameTxt name
      in (positionalIndex, ([argName], [(argName, sessionPlaceholder)], [], []))
    splitArguments positionalIndex (IAUserProvided arg) =
      let (argName, newIndex) = case faName arg of
                                  Nothing   -> ("arg_" <> tshow positionalIndex, positionalIndex + 1)
                                  Just name -> (getFuncArgNameTxt name, positionalIndex)
      in if unHasDefault $ faHasDefault arg
         then (newIndex, ([argName], [], [parseArgument arg argName], []))
         else (newIndex, ([argName], [], [], [parseArgument arg argName]))

    parseArgument :: FunctionArg b -> Text -> m (InputFieldsParser n (Maybe (Text, IR.ArgumentExp b (UnpreparedValue b))))
    parseArgument arg name = do
      typedParser  <- columnParser (ColumnScalar $ functionArgScalarType @b $ faType arg) (G.Nullability True)
      fieldName    <- textToName name

      -- While some arguments are "mandatory" (i.e. they don't have a default
      -- value), we don't enforce the distinction at the GraphQL type system
      -- level, because all postgres function arguments are nullable, and
      -- GraphQL conflates nullability and optionality (see Note [Optional
      -- fields and nullability]). Making the field "mandatory" in the GraphQL
      -- sense would mean giving a default value of `null`, implicitly passing
      -- `null` to the postgres function if the user were to omit the
      -- argument. For backwards compatibility reasons, and also to avoid
      -- surprises, we prefer to reject the query if a mandatory argument is
      -- missing rather than filling the blanks for the user.
      let argParser = P.fieldOptional fieldName Nothing typedParser
      pure $ argParser `mapField` ((name,) . IR.AEInput . mkParameter)

    namedArgument
      :: HashMap Text (IR.ArgumentExp b (UnpreparedValue b))
      -> (Text, FunctionInputArgument b)
      -> n (Maybe (Text, IR.ArgumentExp b (UnpreparedValue b)))
    namedArgument dictionary (name, inputArgument) = case inputArgument of
      IASessionVariables _ -> pure $ Just (name, sessionPlaceholder)
      IAUserProvided arg   -> case Map.lookup name dictionary of
        Just parsedValue -> case faName arg of
          Just _  -> pure $ Just (name, parsedValue)
          Nothing -> parseErrorWith NotSupported "Only last set of positional arguments can be omitted"
        Nothing -> whenMaybe (not $ unHasDefault $ faHasDefault arg) $
          parseErrorWith NotSupported "Non default arguments cannot be omitted"

tablePermissionsInfo :: Backend b => SelPermInfo b -> TablePerms b
tablePermissionsInfo selectPermissions = IR.TablePerm
  { IR._tpFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ spiFilter selectPermissions
  , IR._tpLimit  = spiLimit selectPermissions
  }

------------------------ Node interface from Relay ---------------------------

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

data V1NodeId
  = V1NodeId
  { _nidTable   :: !(TableName 'Postgres)
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
        <$> (PG.QualifiedObject <$> J.parseJSON schemaValue <*> J.parseJSON nameValue)
        <*> pure (firstColumn NESeq.:<|| Seq.fromList remainingColumns)
      parseNodeIdV1 _ = fail "GUID version 1: expecting schema name, table name and at least one column value"

throwInvalidNodeId :: MonadParse n => Text -> n a
throwInvalidNodeId t = parseError $ "the node id is invalid: " <> t

-- | The 'node' root field of a Relay request.
nodePG
  :: forall m n r
   . ( BackendSchema 'Postgres
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => m (P.Parser 'Output n
         ( HashMap (TableName 'Postgres)
           ( SourceName
           , SourceConfig 'Postgres
           , SelPermInfo 'Postgres
           , PrimaryKeyColumns 'Postgres
           , AnnotatedFields 'Postgres
           )
         )
       )
nodePG = memoizeOn 'nodePG () do
  let idDescription = G.Description "A globally unique identifier"
      idField = P.selection_ $$(G.litName "id") (Just idDescription) P.identifier
      nodeInterfaceDescription = G.Description "An object with globally unique ID"
  sources :: SourceCache <- asks getter
  let allTables = Map.fromList $ do
        -- FIXME? When source name is used in type generation?
        (source, sourceInfo) <- Map.toList sources
        tableName            <- maybe [] (Map.keys . takeValidTables) $ unsafeSourceTables @'Postgres sourceInfo
        sourceConfig         <- maybeToList $ unsafeSourceConfiguration @'Postgres sourceInfo
        pure (tableName, (source, sourceConfig))
  tables <-
    Map.mapMaybe id <$> flip Map.traverseWithKey allTables \table (source, sourceConfig) -> runMaybeT do
      tablePkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns) <$> askTableInfo table
      selectPermissions <- MaybeT $ tableSelectPermissions table
      annotatedFieldsParser <- lift $ tableSelectionSet table selectPermissions
      pure $ (source, sourceConfig, selectPermissions, tablePkeyColumns,) <$> annotatedFieldsParser
  pure $ P.selectionSetInterface $$(G.litName "Node")
         (Just nodeInterfaceDescription) [idField] tables

nodeField
  :: forall m n r
   . ( BackendSchema 'Postgres
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => m (P.FieldParser n (QueryRootField UnpreparedValue))
nodeField = do
  let idDescription = G.Description "A globally unique id"
      idArgument = P.field $$(G.litName "id") (Just idDescription) P.identifier
  stringifyNum <- asks $ qcStringifyNum . getter
  nodeObject <- node
  return $ P.subselection $$(G.litName "node") Nothing idArgument nodeObject `P.bindField`
    \(ident, parseds) -> do
      NodeIdV1 (V1NodeId table columnValues) <- parseNodeId ident
      (source, sourceConfig, perms, pkeyColumns, fields) <-
        onNothing (Map.lookup table parseds) $
        withArgsPath $  throwInvalidNodeId $ "the table " <>> ident
      whereExp <- buildNodeIdBoolExp columnValues pkeyColumns
      return
        $ RFDB source
        $ AB.mkAnyBackend
        $ SourceConfigWith sourceConfig
        $ QDBR
        $ QDBSingleRow
        $ IR.AnnSelectG
          { IR._asnFields   = fields
          , IR._asnFrom     = IR.FromTable table
          , IR._asnPerm     = tablePermissionsInfo perms
          , IR._asnArgs     = IR.SelectArgs
            { IR._saWhere    = Just whereExp
            , IR._saOrderBy  = Nothing
            , IR._saLimit    = Nothing
            , IR._saOffset   = Nothing
            , IR._saDistinct = Nothing
            }
          , IR._asnStrfyNum = stringifyNum
          }
  where
    parseNodeId :: Text -> n NodeId
    parseNodeId =
      either (withArgsPath . throwInvalidNodeId . T.pack) pure . J.eitherDecode . base64Decode
    withArgsPath = withPath (++ [Key "args", Key "id"])

    buildNodeIdBoolExp
      :: NESeq.NESeq J.Value
      -> NESeq.NESeq (ColumnInfo 'Postgres)
      -> n (IR.AnnBoolExp 'Postgres (UnpreparedValue 'Postgres))
    buildNodeIdBoolExp columnValues pkeyColumns = do
        let firstPkColumn NESeq.:<|| remainingPkColumns = pkeyColumns
            firstColumnValue NESeq.:<|| remainingColumns = columnValues
            (nonAlignedPkColumns, nonAlignedColumnValues, alignedTuples) =
              partitionThese $ toList $ align remainingPkColumns remainingColumns

        unless (null nonAlignedPkColumns) $ throwInvalidNodeId $
          "primary key columns " <> dquoteList (map pgiColumn nonAlignedPkColumns) <> " are missing"

        unless (null nonAlignedColumnValues) $ throwInvalidNodeId $
          "unexpected column values " <> J.encodeToStrictText nonAlignedColumnValues

        let allTuples = (firstPkColumn, firstColumnValue):alignedTuples

        flip onLeft (parseErrorWith ParseFailed . qeError) $ runExcept $
          fmap IR.BoolAnd $ for allTuples $ \(columnInfo, columnValue) -> do
            let modifyErrFn t = "value of column " <> pgiColumn columnInfo
                                <<> " in node id: " <> t
                pgColumnType = pgiType columnInfo
            pgValue <- modifyErr modifyErrFn $ parseScalarValueColumnType pgColumnType columnValue
            let unpreparedValue = UVParameter Nothing $ ColumnValue pgColumnType pgValue
            pure $ IR.BoolFld $ IR.AVCol columnInfo [IR.AEQ True unpreparedValue]
