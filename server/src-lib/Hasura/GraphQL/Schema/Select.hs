{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
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
  , tableSelectionSet
  , tableSelectionList
  , nodeField
  ) where


import           Hasura.Prelude

import           Control.Lens                          hiding (index)
import           Data.Has
import           Data.Int                              (Int32)
import           Data.Parser.JSONPath
import           Data.Traversable                      (mapAccumL)

import qualified Data.Aeson                            as J
import qualified Data.Aeson.Extended                   as J
import qualified Data.Aeson.Internal                   as J
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashSet                          as Set
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Sequence                         as Seq
import qualified Data.Sequence.NonEmpty                as NESeq
import qualified Data.Text                             as T
import qualified Language.GraphQL.Draft.Syntax         as G

import qualified Hasura.Backends.Postgres.SQL.DML      as SQL
import qualified Hasura.GraphQL.Execute.Types          as ET
import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P
import qualified Hasura.RQL.IR.BoolExp                 as RQL
import qualified Hasura.RQL.IR.Select                  as RQL

import           Data.Text.Extended
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.GraphQL.Parser                 (FieldParser, InputFieldsParser, Kind (..),
                                                        Parser, UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Schema          (toGraphQLType)
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Schema.Remote
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.Server.Utils                   (executeJSONPath)

type SelectExp           b = RQL.AnnSimpleSelG b UnpreparedValue
type AggSelectExp        b = RQL.AnnAggregateSelectG b UnpreparedValue
type ConnectionSelectExp b = RQL.ConnectionSelect b UnpreparedValue
type SelectArgs          b = RQL.SelectArgsG b UnpreparedValue
type TablePerms          b = RQL.TablePermG b UnpreparedValue
type AnnotatedFields     b = RQL.AnnFieldsG b UnpreparedValue
type AnnotatedField      b = RQL.AnnFieldG b UnpreparedValue



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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo 'Postgres          -- ^ select permissions of the table
  -> m (FieldParser n (SelectExp 'Postgres))
selectTable table fieldName description selectPermissions = do
  stringifyNum <- asks $ qcStringifyNum . getter
  tableArgsParser    <- tableArgs table selectPermissions
  selectionSetParser <- tableSelectionList table selectPermissions
  pure $ P.subselection fieldName description tableArgsParser selectionSetParser
    <&> \(args, fields) -> RQL.AnnSelectG
      { RQL._asnFields   = fields
      , RQL._asnFrom     = RQL.FromTable table
      , RQL._asnPerm     = tablePermissionsInfo selectPermissions
      , RQL._asnArgs     = args
      , RQL._asnStrfyNum = stringifyNum
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable         -- ^ qualified name of the table
  -> G.Name                 -- ^ field display name
  -> Maybe G.Description    -- ^ field description, if any
  -> PrimaryKeyColumns 'Postgres      -- ^ primary key columns
  -> SelPermInfo 'Postgres            -- ^ select permissions of the table
  -> m (FieldParser n (ConnectionSelectExp 'Postgres))
selectTableConnection table fieldName description pkeyColumns selectPermissions = do
  stringifyNum       <- asks $ qcStringifyNum . getter
  selectArgsParser   <- tableConnectionArgs pkeyColumns table selectPermissions
  selectionSetParser <- P.nonNullableParser <$> tableConnectionSelectionSet table selectPermissions
  pure $ P.subselection fieldName description selectArgsParser selectionSetParser
    <&> \((args, split, slice), fields) -> RQL.ConnectionSelect
      { RQL._csPrimaryKeyColumns = pkeyColumns
      , RQL._csSplit = split
      , RQL._csSlice = slice
      , RQL._csSelect = RQL.AnnSelectG
        { RQL._asnFields   = fields
        , RQL._asnFrom     = RQL.FromTable table
        , RQL._asnPerm     = tablePermissionsInfo selectPermissions
        , RQL._asnArgs     = args
        , RQL._asnStrfyNum = stringifyNum
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo 'Postgres          -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (SelectExp 'Postgres)))
selectTableByPk table fieldName description selectPermissions = runMaybeT do
  stringifyNum <- asks $ qcStringifyNum . getter
  primaryKeys <- MaybeT $ fmap _pkColumns . _tciPrimaryKey . _tiCoreInfo <$> askTableInfo table
  guard $ all (\c -> pgiColumn c `Set.member` spiCols selectPermissions) primaryKeys
  argsParser <- lift $ sequenceA <$> for primaryKeys \columnInfo -> do
    field <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
    pure $ BoolFld . AVCol columnInfo . pure . AEQ True . mkParameter <$>
      P.field (pgiName columnInfo) (pgiDescription columnInfo) field
  selectionSetParser <- lift $ tableSelectionSet table selectPermissions
  pure $ P.subselection fieldName description argsParser selectionSetParser
    <&> \(boolExpr, fields) ->
      let defaultPerms = tablePermissionsInfo selectPermissions
          -- Do not account permission limit since the result is just a nullable object
          permissions  = defaultPerms { RQL._tpLimit = Nothing }
          whereExpr    = Just $ BoolAnd $ toList boolExpr
      in RQL.AnnSelectG
           { RQL._asnFields   = fields
           , RQL._asnFrom     = RQL.FromTable table
           , RQL._asnPerm     = permissions
           , RQL._asnArgs     = RQL.noSelectArgs { RQL._saWhere = whereExpr }
           , RQL._asnStrfyNum = stringifyNum
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo 'Postgres          -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (AggSelectExp 'Postgres)))
selectTableAggregate table fieldName description selectPermissions = runMaybeT do
  guard $ spiAllowAgg selectPermissions
  stringifyNum    <- asks $ qcStringifyNum . getter
  tableGQLName    <- lift $ getTableGQLName table
  tableArgsParser <- lift $ tableArgs table selectPermissions
  aggregateParser <- lift $ tableAggregationFields table selectPermissions
  nodesParser     <- lift $ tableSelectionList table selectPermissions
  let selectionName = tableGQLName <> $$(G.litName "_aggregate")
      aggregationParser = P.nonNullableParser $
        parsedSelectionsToFields RQL.TAFExp <$>
        P.selectionSet selectionName (Just $ G.Description $ "aggregated selection of " <>> table)
        [ RQL.TAFNodes <$> P.subselection_ $$(G.litName "nodes") Nothing nodesParser
        , RQL.TAFAgg <$> P.subselection_ $$(G.litName "aggregate") Nothing aggregateParser
        ]
  pure $ P.subselection fieldName description tableArgsParser aggregationParser
    <&> \(args, fields) -> RQL.AnnSelectG
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
  :: ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => QualifiedTable
  -> SelPermInfo 'Postgres
  -> m (Parser 'Output n (AnnotatedFields 'Postgres))
tableSelectionSet table selectPermissions = memoizeOn 'tableSelectionSet table do
  tableInfo <- _tiCoreInfo <$> askTableInfo table
  tableGQLName <- getTableGQLName table
  let tableFields = Map.elems  $ _tciFieldInfoMap tableInfo
      tablePkeyColumns = _pkColumns <$> _tciPrimaryKey tableInfo
      description  = Just $ mkDescriptionWith (_tciDescription tableInfo) $
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
  case (queryType, tablePkeyColumns) of
    -- A relay table
    (ET.QueryRelay, Just pkeyColumns) -> do
      let nodeIdFieldParser =
            P.selection_ $$(G.litName "id") Nothing P.identifier $> RQL.AFNodeId table pkeyColumns
          allFieldParsers = fieldParsers <> [nodeIdFieldParser]
      nodeInterface <- node
      pure $ P.selectionSetObject tableGQLName description allFieldParsers [nodeInterface]
            <&> parsedSelectionsToFields RQL.AFExpression
    _                                 ->
      pure $ P.selectionSetObject tableGQLName description fieldParsers []
            <&> parsedSelectionsToFields RQL.AFExpression

-- | List of table fields object.
-- Just a @'nonNullableObjectList' wrapper over @'tableSelectionSet'.
-- > table_name: [table!]!
tableSelectionList
  :: ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => QualifiedTable
  -> SelPermInfo 'Postgres
  -> m (Parser 'Output n (AnnotatedFields 'Postgres))
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
  :: forall m n r. ( MonadSchema n m
                   , MonadTableInfo r m
                   , MonadRole r m
                   , Has QueryContext r
                   )
  => QualifiedTable
  -> SelPermInfo 'Postgres
  -> m (Parser 'Output n (RQL.ConnectionFields 'Postgres UnpreparedValue))
tableConnectionSelectionSet table selectPermissions = do
  edgesParser  <- tableEdgesSelectionSet
  tableGQLName <- getTableGQLName table
  let connectionTypeName = tableGQLName <> $$(G.litName "Connection")
      pageInfo = P.subselection_ $$(G.litName "pageInfo") Nothing
                 pageInfoSelectionSet <&> RQL.ConnectionPageInfo
      edges = P.subselection_ $$(G.litName "edges") Nothing
              edgesParser <&> RQL.ConnectionEdges
      connectionDescription = G.Description $ "A Relay connection object on " <>> table
  pure $ P.nonNullableParser $
         P.selectionSet connectionTypeName (Just connectionDescription) [pageInfo, edges]
         <&> parsedSelectionsToFields RQL.ConnectionTypename
  where
    pageInfoSelectionSet :: Parser 'Output n RQL.PageInfoFields
    pageInfoSelectionSet =
      let startCursorField     = P.selection_ $$(G.litName "startCursor") Nothing
                                 P.string $> RQL.PageInfoStartCursor
          endCursorField       = P.selection_ $$(G.litName "endCursor") Nothing
                                 P.string $> RQL.PageInfoEndCursor
          hasNextPageField     = P.selection_ $$(G.litName "hasNextPage") Nothing
                                 P.boolean $> RQL.PageInfoHasNextPage
          hasPreviousPageField = P.selection_ $$(G.litName "hasPreviousPage") Nothing
                                 P.boolean $> RQL.PageInfoHasPreviousPage
          allFields            =
            [ startCursorField, endCursorField
            , hasNextPageField, hasPreviousPageField
            ]
      in P.nonNullableParser $ P.selectionSet $$(G.litName "PageInfo") Nothing allFields
         <&> parsedSelectionsToFields RQL.PageInfoTypename


    tableEdgesSelectionSet
      :: m (Parser 'Output n (RQL.EdgeFields 'Postgres UnpreparedValue))
    tableEdgesSelectionSet = do
      tableGQLName        <- getTableGQLName table
      edgeNodeParser      <- P.nonNullableParser <$> tableSelectionSet table selectPermissions
      let edgesType = tableGQLName <> $$(G.litName "Edge")
          cursor    = P.selection_ $$(G.litName "cursor") Nothing
                      P.string $> RQL.EdgeCursor
          edgeNode  = P.subselection_ $$(G.litName "node") Nothing
                      edgeNodeParser <&> RQL.EdgeNode
      pure $ nonNullableObjectList $ P.selectionSet edgesType Nothing [cursor, edgeNode]
             <&> parsedSelectionsToFields RQL.EdgeTypename

-- | User-defined function (AKA custom function)
selectFunction
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => FunctionInfo         -- ^ SQL function info
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> SelPermInfo 'Postgres          -- ^ select permissions of the target table
  -> m (FieldParser n (SelectExp 'Postgres))
selectFunction function fieldName description selectPermissions = do
  stringifyNum <- asks $ qcStringifyNum . getter
  let table = fiReturnType function
  tableArgsParser    <- tableArgs table selectPermissions
  functionArgsParser <- customSQLFunctionArgs function
  selectionSetParser <- tableSelectionList table selectPermissions
  let argsParser = liftA2 (,) functionArgsParser tableArgsParser
  pure $ P.subselection fieldName description argsParser selectionSetParser
    <&> \((funcArgs, tableArgs'), fields) -> RQL.AnnSelectG
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
  -> SelPermInfo 'Postgres          -- ^ select permissions of the target table
  -> m (Maybe (FieldParser n (AggSelectExp 'Postgres)))
selectFunctionAggregate function fieldName description selectPermissions = runMaybeT do
  let table = fiReturnType function
  stringifyNum <- asks $ qcStringifyNum . getter
  guard $ spiAllowAgg selectPermissions
  tableGQLName <- getTableGQLName table
  tableArgsParser    <- lift $ tableArgs table selectPermissions
  functionArgsParser <- lift $ customSQLFunctionArgs function
  aggregateParser    <- lift $ tableAggregationFields table selectPermissions
  selectionName      <- lift $ pure tableGQLName <&> (<> $$(G.litName "_aggregate"))
  nodesParser        <- lift $ tableSelectionList table selectPermissions
  let argsParser = liftA2 (,) functionArgsParser tableArgsParser
      aggregationParser = fmap (parsedSelectionsToFields RQL.TAFExp) $
        P.nonNullableParser $
        P.selectionSet selectionName Nothing
        [ RQL.TAFNodes <$> P.subselection_ $$(G.litName "nodes") Nothing nodesParser
        , RQL.TAFAgg <$> P.subselection_ $$(G.litName "aggregate") Nothing aggregateParser
        ]
  pure $ P.subselection fieldName description argsParser aggregationParser
    <&> \((funcArgs, tableArgs'), fields) -> RQL.AnnSelectG
      { RQL._asnFields   = fields
      , RQL._asnFrom     = RQL.FromFunction (fiName function) funcArgs Nothing
      , RQL._asnPerm     = tablePermissionsInfo selectPermissions
      , RQL._asnArgs     = tableArgs'
      , RQL._asnStrfyNum = stringifyNum
      }

selectFunctionConnection
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => FunctionInfo           -- ^ SQL function info
  -> G.Name                 -- ^ field display name
  -> Maybe G.Description    -- ^ field description, if any
  -> PrimaryKeyColumns 'Postgres      -- ^ primary key columns of the target table
  -> SelPermInfo 'Postgres            -- ^ select permissions of the target table
  -> m (FieldParser n (ConnectionSelectExp 'Postgres))
selectFunctionConnection function fieldName description pkeyColumns selectPermissions = do
  stringifyNum <- asks $ qcStringifyNum . getter
  let table = fiReturnType function
  tableConnectionArgsParser <- tableConnectionArgs pkeyColumns table selectPermissions
  functionArgsParser <- customSQLFunctionArgs function
  selectionSetParser <- tableConnectionSelectionSet table selectPermissions
  let argsParser = liftA2 (,) functionArgsParser tableConnectionArgsParser
  pure $ P.subselection fieldName description argsParser selectionSetParser
    <&> \((funcArgs, (args, split, slice)), fields) -> RQL.ConnectionSelect
      { RQL._csPrimaryKeyColumns = pkeyColumns
      , RQL._csSplit = split
      , RQL._csSlice = slice
      , RQL._csSelect = RQL.AnnSelectG
        { RQL._asnFields   = fields
        , RQL._asnFrom     = RQL.FromFunction (fiName function) funcArgs Nothing
        , RQL._asnPerm     = tablePermissionsInfo selectPermissions
        , RQL._asnArgs     = args
        , RQL._asnStrfyNum = stringifyNum
        }
      }



-- 2. local parsers
-- Parsers that are used but not exported: sub-components

-- | Argument to filter rows returned from table selection
-- > where: table_bool_exp
tableWhere
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> SelPermInfo 'Postgres
  -> m (InputFieldsParser n (Maybe (RQL.AnnBoolExp 'Postgres UnpreparedValue)))
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> SelPermInfo 'Postgres
  -> m (InputFieldsParser n (Maybe (NonEmpty (RQL.AnnOrderByItemG 'Postgres UnpreparedValue))))
tableOrderBy table selectPermissions = do
  orderByParser <- orderByExp table selectPermissions
  pure $ do
    maybeOrderByExps <- fmap join $
      P.fieldOptional orderByName orderByDesc $ P.nullable $ P.list orderByParser
    pure $ maybeOrderByExps >>= NE.nonEmpty . concat
  where
    orderByName = $$(G.litName "order_by")
    orderByDesc = Just $ G.Description "sort the rows by one or more columns"

-- | Argument to distinct select on columns returned from table selection
-- > distinct_on: [table_select_column!]
tableDistinctOn
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> SelPermInfo 'Postgres
  -> m (InputFieldsParser n (Maybe (NonEmpty PGCol)))
tableDistinctOn table selectPermissions = do
  columnsEnum   <- tableSelectColumnsEnum table selectPermissions
  pure $ do
    maybeDistinctOnColumns <- join.join <$> for columnsEnum
      (P.fieldOptional distinctOnName distinctOnDesc . P.nullable . P.list)
    pure $ maybeDistinctOnColumns >>= NE.nonEmpty
  where
    distinctOnName = $$(G.litName "distinct_on")
    distinctOnDesc = Just $ G.Description "distinct select on columns"

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
  -> SelPermInfo 'Postgres
  -> m (InputFieldsParser n (SelectArgs 'Postgres))
tableArgs table selectPermissions = do
  whereParser   <- tableWhere table selectPermissions
  orderByParser <- tableOrderBy table selectPermissions
  distinctParser <- tableDistinctOn table selectPermissions
  let selectArgs = do
        whereF   <- whereParser
        orderBy  <- orderByParser
        limit    <- fmap join $ P.fieldOptional limitName   limitDesc   $ P.nullable positiveInt
        offset   <- fmap join $ P.fieldOptional offsetName  offsetDesc  $ P.nullable fakeBigInt
        distinct <- distinctParser
        pure $ RQL.SelectArgs
          { RQL._saWhere    = whereF
          , RQL._saOrderBy  = orderBy
          , RQL._saLimit    = fromIntegral <$> limit
          , RQL._saOffset   = txtEncoder <$> offset
          , RQL._saDistinct = distinct
          }
  pure $ selectArgs `P.bindFields`
   \args -> do
      traverse_ (validateDistinctOn $ RQL._saOrderBy args) $ RQL._saDistinct args
      pure args
  where
    -- TODO: THIS IS A TEMPORARY FIX
    -- while offset is exposed in the schema as a GraphQL Int, which
    -- is a bounded Int32, previous versions of the code used to also
    -- silently accept a string as an input for the offset as a way to
    -- support int64 values (postgres bigint)
    -- a much better way of supporting this would be to expose the
    -- offset in the code as a postgres bigint, but for now, to avoid
    -- a breaking change, we are defining a custom parser that also
    -- accepts a string
    fakeBigInt :: Parser 'Both n PGScalarValue
    fakeBigInt = P.Parser
      { pType = fakeBigIntSchemaType
      , pParser = P.peelVariable (Just $ toGraphQLType fakeBigIntSchemaType) >=> \case
          P.GraphQLValue (G.VInt    i) -> PGValBigInt <$> convertWith scientificToInteger (fromInteger i)
          P.JSONValue    (J.Number  n) -> PGValBigInt <$> convertWith scientificToInteger n
          P.GraphQLValue (G.VString s) -> pure $ PGValUnknown s
          P.JSONValue    (J.String  s) -> pure $ PGValUnknown s
          v ->  P.typeMismatch $$(G.litName "Int") "a 32-bit integer, or a 64-bit integer represented as a string" v
      }
    fakeBigIntSchemaType = P.NonNullable $ P.TNamed $ P.mkDefinition $$(G.litName "Int") Nothing P.TIScalar
    convertWith f = either (parseErrorWith ParseFailed . qeError) pure . runAesonParser f

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
            case obiColumn ob of
              RQL.AOCColumn pgCol -> Just $ pgiColumn pgCol
              _                   -> Nothing
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => PrimaryKeyColumns 'Postgres
  -> QualifiedTable
  -> SelPermInfo 'Postgres
  -> m ( InputFieldsParser n
         ( SelectArgs 'Postgres
         , Maybe (NonEmpty (RQL.ConnectionSplit 'Postgres UnpreparedValue))
         , Maybe RQL.ConnectionSlice
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
        (Just v, Nothing)  -> pure $ Just (RQL.CSKAfter, v)
        (Nothing, Just v)  -> pure $ Just (RQL.CSKBefore, v)
      for rawSplit (uncurry (parseConnectionSplit orderBy'))

    slice <- firstAndLast `P.bindFields` \case
      (Nothing, Nothing) -> pure Nothing
      (Just _, Just _)   -> parseError "\"first\" and \"last\" are not allowed at once"
      (Just v, Nothing)  -> pure $ Just $ RQL.SliceFirst $ fromIntegral v
      (Nothing, Just v)  -> pure $ Just $ RQL.SliceLast $ fromIntegral v

    pure ( RQL.SelectArgs whereF orderBy Nothing Nothing distinct
         , split
         , slice
         )
  where
    base64Text = base64Decode <$> P.string

    appendPrimaryKeyOrderBy :: NonEmpty (RQL.AnnOrderByItemG 'Postgres v) -> NonEmpty (RQL.AnnOrderByItemG 'Postgres v)
    appendPrimaryKeyOrderBy orderBys@(h NE.:| t) =
      let orderByColumnNames =
            orderBys ^.. traverse . to obiColumn . RQL._AOCColumn . to pgiColumn
          pkeyOrderBys = flip mapMaybe (toList pkeyColumns) $ \pgColumnInfo ->
                         if pgiColumn pgColumnInfo `elem` orderByColumnNames then Nothing
                         else Just $ OrderByItemG Nothing (RQL.AOCColumn pgColumnInfo) Nothing
      in h NE.:| (t <> pkeyOrderBys)

    parseConnectionSplit
      :: Maybe (NonEmpty (RQL.AnnOrderByItemG 'Postgres UnpreparedValue))
      -> RQL.ConnectionSplitKind
      -> BL.ByteString
      -> n (NonEmpty (RQL.ConnectionSplit 'Postgres UnpreparedValue))
    parseConnectionSplit maybeOrderBys splitKind cursorSplit = do
      cursorValue <- J.eitherDecode cursorSplit `onLeft` const throwInvalidCursor
      case maybeOrderBys of
        Nothing -> forM (NESeq.toNonEmpty pkeyColumns) $
          \pgColumnInfo -> do
            let columnJsonPath = [J.Key $ getPGColTxt $ pgiColumn pgColumnInfo]
                columnType = pgiType pgColumnInfo
            pgColumnValue <- iResultToMaybe (executeJSONPath columnJsonPath cursorValue)
              `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parsePGScalarValue columnType pgColumnValue
            let unresolvedValue = flip UVParameter Nothing $ P.PGColumnValue columnType pgValue
            pure $ RQL.ConnectionSplit splitKind unresolvedValue $
                   OrderByItemG Nothing (RQL.AOCColumn pgColumnInfo) Nothing
        Just orderBys ->
          forM orderBys $ \orderBy -> do
            let OrderByItemG orderType annObCol nullsOrder = orderBy
                columnType = getOrderByColumnType annObCol
            orderByItemValue <- iResultToMaybe (executeJSONPath (getPathFromOrderBy annObCol) cursorValue)
              `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parsePGScalarValue columnType orderByItemValue
            let unresolvedValue = flip UVParameter Nothing $ P.PGColumnValue columnType pgValue
            pure $ RQL.ConnectionSplit splitKind unresolvedValue $
                   OrderByItemG orderType (() <$ annObCol) nullsOrder
      where
        throwInvalidCursor = parseError "the \"after\" or \"before\" cursor is invalid"
        liftQErr = either (parseError . qeError) pure . runExcept

        getPathFromOrderBy = \case
          RQL.AOCColumn pgColInfo ->
            let pathElement = J.Key $ getPGColTxt $ pgiColumn pgColInfo
            in [pathElement]
          RQL.AOCObjectRelation relInfo _ obCol ->
            let pathElement = J.Key $ relNameToTxt $ riName relInfo
            in pathElement : getPathFromOrderBy obCol
          RQL.AOCArrayAggregation relInfo _ aggOb ->
            let fieldName = J.Key $ relNameToTxt (riName relInfo) <> "_aggregate"
            in fieldName : case aggOb of
                 RQL.AAOCount    -> [J.Key "count"]
                 RQL.AAOOp t col -> [J.Key t, J.Key $ getPGColTxt $ pgiColumn col]

        getOrderByColumnType = \case
          RQL.AOCColumn pgColInfo -> pgiType pgColInfo
          RQL.AOCObjectRelation _ _ obCol -> getOrderByColumnType obCol
          RQL.AOCArrayAggregation _ _ aggOb ->
            case aggOb of
              RQL.AAOCount        -> PGColumnScalar PGInteger
              RQL.AAOOp _ colInfo -> pgiType colInfo

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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> SelPermInfo 'Postgres
  -> m (Parser 'Output n (RQL.AggregateFields 'Postgres))
tableAggregationFields table selectPermissions = do
  tableGQLName  <- getTableGQLName table
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
        <&> parsedSelectionsToFields RQL.AFExp
  where
    mkNumericAggFields :: G.Name -> [ColumnInfo 'Postgres] -> m [FieldParser n (RQL.ColFld 'Postgres)]
    mkNumericAggFields name
      | name == $$(G.litName "sum") = traverse mkColumnAggField
      | otherwise                   = traverse \columnInfo ->
          pure $ P.selection_ (pgiName columnInfo) (pgiDescription columnInfo)
                 (P.nullable P.float) $> RQL.CFCol (pgiColumn columnInfo)

    mkColumnAggField :: ColumnInfo 'Postgres -> m (FieldParser n (RQL.ColFld 'Postgres))
    mkColumnAggField columnInfo = do
      field <- P.column (pgiType columnInfo) (G.Nullability True)
      pure $ P.selection_ (pgiName columnInfo) (pgiDescription columnInfo) field
        $> RQL.CFCol (pgiColumn columnInfo)

    countField :: m (FieldParser n (RQL.AggregateField 'Postgres))
    countField = do
      columnsEnum <- tableSelectColumnsEnum table selectPermissions
      let columnsName  = $$(G.litName "columns")
          distinctName = $$(G.litName "distinct")
          args = do
            distinct <- P.fieldOptional distinctName Nothing P.boolean
            columns  <- maybe (pure Nothing) (P.fieldOptional columnsName Nothing . P.list) columnsEnum
            pure $ case columns of
                     Nothing   -> SQL.CTStar
                     Just cols -> if Just True == distinct
                                  then SQL.CTDistinct cols
                                  else SQL.CTSimple   cols
      pure $ RQL.AFCount <$> P.selection $$(G.litName "count") Nothing args P.int

    parseAggOperator
      :: G.Name
      -> G.Name
      -> [FieldParser n (RQL.ColFld 'Postgres)]
      -> FieldParser n (RQL.AggregateField 'Postgres)
    parseAggOperator operator tableGQLName columns =
      let opText  = G.unName operator
          setName = tableGQLName <> $$(G.litName "_") <> operator <> $$(G.litName "_fields")
          setDesc = Just $ G.Description $ "aggregate " <> opText <> " on columns"
          subselectionParser = P.selectionSet setName setDesc columns
            <&> parsedSelectionsToFields RQL.CFExp
      in P.subselection_ operator Nothing subselectionParser
         <&> (RQL.AFOp . RQL.AggregateOp opText)

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
       <- onNothing (P.getObjectInfo type') $
          throw400 RemoteSchemaError $ "field " <> fcName fieldCall <<> " is expected to be an object"
      lookupRemoteField objFieldInfos rest'

-- | An individual field of a table
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable
  -> Maybe (PrimaryKeyColumns 'Postgres)
  -> FieldInfo 'Postgres
  -> SelPermInfo 'Postgres
  -> m [FieldParser n (AnnotatedField 'Postgres)]
fieldSelection table maybePkeyColumns fieldInfo selectPermissions =
  case fieldInfo of
    FIColumn columnInfo -> maybeToList <$> runMaybeT do
      queryType <- asks $ qcQueryType . getter
      let columnName = pgiColumn columnInfo
          fieldName = pgiName columnInfo
      if | fieldName == $$(G.litName "id") && queryType == ET.QueryRelay -> do
             pkeyColumns <- MaybeT $ pure maybePkeyColumns
             pure $ P.selection_ fieldName Nothing P.identifier
                    $> RQL.AFNodeId table pkeyColumns
         | otherwise -> do
             guard $ Set.member columnName (spiCols selectPermissions)
             let pathArg = jsonPathArg $ pgiType columnInfo
             field <- lift $ P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
             pure $ P.selection fieldName (pgiDescription columnInfo) pathArg field
               <&> RQL.mkAnnColumnField columnInfo

    FIRelationship relationshipInfo ->
      concat . maybeToList <$> relationshipField relationshipInfo

    FIComputedField computedFieldInfo ->
      maybeToList <$> computedField computedFieldInfo selectPermissions

    FIRemoteRelationship remoteFieldInfo  ->
      concat . maybeToList <$> remoteRelationshipField remoteFieldInfo

-- | Field parsers for a table relationship
relationshipField
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => RelInfo -> m (Maybe [FieldParser n (AnnotatedField 'Postgres)])
relationshipField relationshipInfo = runMaybeT do
  let otherTable = riRTable  relationshipInfo
      colMapping = riMapping relationshipInfo
      relName    = riName    relationshipInfo
      nullable   = riIsNullable relationshipInfo
  remotePerms      <- MaybeT $ tableSelectPermissions otherTable
  relFieldName     <- lift $ textToName $ relNameToTxt relName
  case riType relationshipInfo of
    ObjRel -> do
      let desc = Just $ G.Description "An object relationship"
      selectionSetParser <- lift $ tableSelectionSet otherTable remotePerms
      pure $ pure $ (if nullable then id else P.nonNullableField) $
        P.subselection_ relFieldName desc selectionSetParser
             <&> \fields -> RQL.AFObjectRelation $ RQL.AnnRelationSelectG relName colMapping $
                    RQL.AnnObjectSelectG fields otherTable $
                    RQL._tpFilter $ tablePermissionsInfo remotePerms
    ArrRel -> do
      let arrayRelDesc = Just $ G.Description "An array relationship"
      otherTableParser <- lift $ selectTable otherTable relFieldName arrayRelDesc remotePerms
      let arrayRelField = otherTableParser <&> \selectExp -> RQL.AFArrayRelation $
            RQL.ASSimple $ RQL.AnnRelationSelectG relName colMapping selectExp
          relAggFieldName = relFieldName <> $$(G.litName "_aggregate")
          relAggDesc      = Just $ G.Description "An aggregate relationship"
      remoteAggField <- lift $ selectTableAggregate otherTable relAggFieldName relAggDesc remotePerms
      remoteConnectionField <- runMaybeT $ do
        -- Parse array connection field only for relay schema
        queryType <- asks $ qcQueryType . getter
        guard $ queryType == ET.QueryRelay
        pkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns)
                       <$> askTableInfo otherTable
        let relConnectionName = relFieldName <> $$(G.litName "_connection")
            relConnectionDesc = Just $ G.Description "An array relationship connection"
        lift $ lift $ selectTableConnection otherTable relConnectionName
                      relConnectionDesc pkeyColumns remotePerms
      pure $ catMaybes [ Just arrayRelField
                       , fmap (RQL.AFArrayRelation . RQL.ASAggregate . RQL.AnnRelationSelectG relName colMapping) <$> remoteAggField
                       , fmap (RQL.AFArrayRelation . RQL.ASConnection . RQL.AnnRelationSelectG relName colMapping) <$> remoteConnectionField
                       ]

-- | Computed field parser
computedField
  :: forall m n r
  . (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => ComputedFieldInfo 'Postgres
  -> SelPermInfo 'Postgres
  -> m (Maybe (FieldParser n (AnnotatedField 'Postgres)))
computedField ComputedFieldInfo{..} selectPermissions = runMaybeT do
  stringifyNum <- asks $ qcStringifyNum . getter
  fieldName <- lift $ textToName $ computedFieldNameToText _cfiName
  functionArgsParser <- lift $ computedFieldFunctionArgs _cfiFunction
  case _cfiReturnType of
    CFRScalar scalarReturnType -> do
      guard $ _cfiName `Set.member` spiScalarComputedFields selectPermissions
      let fieldArgsParser = do
            args  <- functionArgsParser
            colOp <- jsonPathArg $ PGColumnScalar scalarReturnType
            pure $ RQL.AFComputedField $ RQL.CFSScalar $ RQL.ComputedFieldScalarSelect
              { RQL._cfssFunction  = _cffName _cfiFunction
              , RQL._cfssType      = scalarReturnType
              , RQL._cfssColumnOp  = colOp
              , RQL._cfssArguments = args
              }
      dummyParser <- lift $ P.column (PGColumnScalar scalarReturnType) (G.Nullability True)
      pure $ P.selection fieldName (Just fieldDescription) fieldArgsParser dummyParser
    CFRSetofTable tableName -> do
      remotePerms        <- MaybeT $ tableSelectPermissions tableName
      selectArgsParser   <- lift   $ tableArgs tableName remotePerms
      selectionSetParser <- lift   $ P.multiple . P.nonNullableParser <$> tableSelectionSet tableName remotePerms
      let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
      pure $ P.subselection fieldName (Just fieldDescription) fieldArgsParser selectionSetParser <&>
        \((functionArgs', args), fields) ->
          RQL.AFComputedField $ RQL.CFSTable RQL.JASMultipleRows $ RQL.AnnSelectG
            { RQL._asnFields   = fields
            , RQL._asnFrom     = RQL.FromFunction (_cffName _cfiFunction) functionArgs' Nothing
            , RQL._asnPerm     = tablePermissionsInfo remotePerms
            , RQL._asnArgs     = args
            , RQL._asnStrfyNum = stringifyNum
            }
  where
    fieldDescription =
      let defaultDescription = "A computed field, executes function " <>> _cffName _cfiFunction
      in mkDescriptionWith (_cffDescription _cfiFunction) defaultDescription

    computedFieldFunctionArgs
      :: ComputedFieldFunction -> m (InputFieldsParser n (RQL.FunctionArgsExpTableRow UnpreparedValue))
    computedFieldFunctionArgs ComputedFieldFunction{..} =
      functionArgs _cffName (IAUserProvided <$> _cffInputArgs) <&> fmap addTableAndSessionArgument
      where
        tableRowArgument = RQL.AETableRow Nothing

        addTableAndSessionArgument args@(RQL.FunctionArgsExp positional named) =
          let withTable = case _cffTableArgument of
                FTAFirst               -> RQL.FunctionArgsExp (tableRowArgument : positional) named
                FTANamed argName index -> RQL.insertFunctionArg argName index tableRowArgument args
              sessionArgVal = RQL.AESession UVSession
          in
            case _cffSessionArgument of
              Nothing -> withTable
              Just (FunctionSessionArgument argName index) ->
                RQL.insertFunctionArg argName index sessionArgVal withTable

-- | Remote relationship field parsers
remoteRelationshipField
  :: (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => RemoteFieldInfo 'Postgres -> m (Maybe [FieldParser n (AnnotatedField 'Postgres)])
remoteRelationshipField remoteFieldInfo = runMaybeT do
  queryType <- asks $ qcQueryType . getter
  -- https://github.com/hasura/graphql-engine/issues/5144
  -- The above issue is easily fixable by removing the following guard and 'MaybeT' monad transformation
  guard $ queryType == ET.QueryHasura
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
    sequenceA <$> for (Map.toList $ _rfiParamMap remoteFieldInfo) \(name, inpValDefn) -> do
      parser <- lift $ inputValueDefinitionParser (_rfiSchemaIntrospect remoteFieldInfo) inpValDefn
      pure $ parser `mapField` RQL.RemoteFieldArgument name

  -- This selection set parser, should be of the remote node's selection set parser, which comes
  -- from the fieldCall
  nestedFieldInfo <- lift $ lookupRemoteField fieldDefns $ unRemoteFields $ _rfiRemoteFields remoteFieldInfo
  let remoteFieldsArgumentsParser' = fmap catMaybes remoteFieldsArgumentsParser
  case nestedFieldInfo of
    P.FieldInfo{ P.fType = fieldType } -> do
      let fieldInfo' = P.FieldInfo
            { P.fArguments = P.ifDefinitions remoteFieldsArgumentsParser'
            , P.fType = fieldType }
      pure $ pure $ P.unsafeRawField (P.mkDefinition fieldName Nothing fieldInfo')
        `P.bindField` \G.Field{ G._fArguments = args, G._fSelectionSet = selSet } -> do
          remoteArgs <- P.ifParser remoteFieldsArgumentsParser' $ P.GraphQLValue <$> args
          pure $ RQL.AFRemote $ RQL.RemoteSelect
            { _rselArgs          = remoteArgs
            , _rselSelection     = selSet
            , _rselHasuraColumns = _rfiHasuraFields remoteFieldInfo
            , _rselFieldCall     = unRemoteFields $ _rfiRemoteFields remoteFieldInfo
            , _rselRemoteSchema  = _rfiRemoteSchema remoteFieldInfo
            }

-- | The custom SQL functions' input "args" field parser
-- > function_name(args: function_args)
customSQLFunctionArgs
  :: (MonadSchema n m, MonadTableInfo r m)
  => FunctionInfo
  -> m (InputFieldsParser n (RQL.FunctionArgsExpTableRow UnpreparedValue))
customSQLFunctionArgs FunctionInfo{..} = functionArgs fiName fiInputArgs

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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  => QualifiedFunction
  -> Seq.Seq FunctionInputArgument
  -> m (InputFieldsParser n (RQL.FunctionArgsExpTableRow UnpreparedValue))
functionArgs functionName (toList -> inputArgs) = do
  -- First, we iterate through the original sql arguments in order, to find the
  -- corresponding graphql names. At the same time, we create the input field
  -- parsers, in three groups: session argument, optional arguments, and
  -- mandatory arguments. Optional arguments have a default value, mandatory
  -- arguments don't.
  let (names, session, optional, mandatory) = mconcat $ snd $ mapAccumL splitArguments 1 inputArgs
      defaultArguments = RQL.FunctionArgsExp (snd <$> session) Map.empty

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
         objectName <- qualifiedObjectToName functionName <&> (<> $$(G.litName "_args"))
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
               pure $ RQL.FunctionArgsExp positional named

         pure $ P.field fieldName (Just fieldDesc) objectParser

  where
    sessionPlaceholder :: RQL.ArgumentExp UnpreparedValue
    sessionPlaceholder = RQL.AEInput P.UVSession

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
      in (positionalIndex, ([argName], [(argName, sessionPlaceholder)], [], []))
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
      let argParser = P.fieldOptional fieldName Nothing columnParser
      pure $ argParser `mapField` ((name,) . RQL.AEInput . mkParameter)

    namedArgument
      :: HashMap Text (RQL.ArgumentExp UnpreparedValue)
      -> (Text, InputArgument FunctionArg)
      -> n (Maybe (Text, RQL.ArgumentExp UnpreparedValue))
    namedArgument dictionary (name, inputArgument) = case inputArgument of
      IASessionVariables _ -> pure $ Just (name, sessionPlaceholder)
      IAUserProvided arg   -> case Map.lookup name dictionary of
        Just parsedValue -> case faName arg of
          Just _  -> pure $ Just (name, parsedValue)
          Nothing -> parseErrorWith NotSupported "Only last set of positional arguments can be omitted"
        Nothing -> whenMaybe (not $ unHasDefault $ faHasDefault arg) $
          parseErrorWith NotSupported "Non default arguments cannot be omitted"


-- | The "path" argument for json column fields
jsonPathArg :: MonadParse n => PGColumnType -> InputFieldsParser n (Maybe (RQL.ColumnOp 'Postgres))
jsonPathArg columnType
  | isScalarColumnWhere isJSONType columnType =
      P.fieldOptional fieldName description P.string `P.bindFields` fmap join . traverse toColExp
  | otherwise = pure Nothing
  where
    fieldName = $$(G.litName "path")
    description = Just "JSON select path"
    toColExp textValue = case parseJSONPath textValue of
      Left err     -> parseError $ T.pack $ "parse json path error: " ++ err
      Right []     -> pure Nothing
      Right jPaths -> pure $ Just $ RQL.ColumnOp SQL.jsonbPathOp $ SQL.SEArray $ map elToColExp jPaths
    elToColExp (Key k)   = SQL.SELit k
    elToColExp (Index i) = SQL.SELit $ T.pack (show i)

tablePermissionsInfo :: SelPermInfo 'Postgres -> TablePerms 'Postgres
tablePermissionsInfo selectPermissions = RQL.TablePerm
  { RQL._tpFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ spiFilter selectPermissions
  , RQL._tpLimit  = spiLimit selectPermissions
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

throwInvalidNodeId :: MonadParse n => Text -> n a
throwInvalidNodeId t = parseError $ "the node id is invalid: " <> t

-- | The 'node' root field of a Relay request.
node
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => m (P.Parser 'Output n (HashMap QualifiedTable (SelPermInfo 'Postgres, PrimaryKeyColumns 'Postgres, AnnotatedFields 'Postgres)))
node = memoizeOn 'node () do
  let idDescription = G.Description "A globally unique identifier"
      idField = P.selection_ $$(G.litName "id") (Just idDescription) P.identifier
      nodeInterfaceDescription = G.Description "An object with globally unique ID"
  allTables :: TableCache <- asks getter
  tables :: HashMap QualifiedTable (Parser 'Output n (SelPermInfo 'Postgres, NESeq (ColumnInfo 'Postgres), AnnotatedFields 'Postgres)) <-
    Map.mapMaybe id <$> flip Map.traverseWithKey allTables \table _ -> runMaybeT do
      tablePkeyColumns <- MaybeT $ (^? tiCoreInfo.tciPrimaryKey._Just.pkColumns) <$> askTableInfo table
      selectPermissions <- MaybeT $ tableSelectPermissions table
      annotatedFieldsParser <- lift $ tableSelectionSet table selectPermissions
      pure $ (selectPermissions, tablePkeyColumns,) <$> annotatedFieldsParser
  pure $ P.selectionSetInterface $$(G.litName "Node")
         (Just nodeInterfaceDescription) [idField] tables

nodeField
  :: forall m n r
   . ( MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     )
  => m (P.FieldParser n (SelectExp 'Postgres))
nodeField = do
  let idDescription = G.Description "A globally unique id"
      idArgument = P.field $$(G.litName "id") (Just idDescription) P.identifier
  stringifyNum <- asks $ qcStringifyNum . getter
  nodeObject <- node
  return $ P.subselection $$(G.litName "node") Nothing idArgument nodeObject `P.bindField`
    \(ident, parseds) -> do
      NodeIdV1 (V1NodeId table columnValues) <- parseNodeId ident
      (perms, pkeyColumns, fields) <-
        onNothing (Map.lookup table parseds) $
        withArgsPath $  throwInvalidNodeId $ "the table " <>> ident
      whereExp <- buildNodeIdBoolExp columnValues pkeyColumns
      return $ RQL.AnnSelectG
        { RQL._asnFields   = fields
        , RQL._asnFrom     = RQL.FromTable table
        , RQL._asnPerm     = tablePermissionsInfo perms
        , RQL._asnArgs     = RQL.SelectArgs
          { RQL._saWhere    = Just whereExp
          , RQL._saOrderBy  = Nothing
          , RQL._saLimit    = Nothing
          , RQL._saOffset   = Nothing
          , RQL._saDistinct = Nothing
          }
        , RQL._asnStrfyNum = stringifyNum
        }
  where
    parseNodeId :: Text -> n NodeId
    parseNodeId =
      either (withArgsPath . throwInvalidNodeId . T.pack) pure . J.eitherDecode . base64Decode
    withArgsPath = withPath (++ [Key "args", Key "id"])

    buildNodeIdBoolExp
      :: NESeq.NESeq J.Value
      -> NESeq.NESeq (ColumnInfo 'Postgres)
      -> n (RQL.AnnBoolExp 'Postgres UnpreparedValue)
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
          fmap RQL.BoolAnd $ for allTuples $ \(columnInfo, columnValue) -> do
            let modifyErrFn t = "value of column " <> pgiColumn columnInfo
                                <<> " in node id: " <> t
                pgColumnType = pgiType columnInfo
            pgValue <- modifyErr modifyErrFn $ parsePGScalarValue pgColumnType columnValue
            let unpreparedValue = flip UVParameter Nothing $ P.PGColumnValue pgColumnType pgValue
            pure $ RQL.BoolFld $ RQL.AVCol columnInfo [RQL.AEQ True unpreparedValue]
