{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate table selection schema both for ordinary Hasura-type and
-- relay-type queries.  All schema with "relay" or "connection" in the name is
-- used exclusively by relay.
module Hasura.GraphQL.Schema.Select
  ( selectTable,
    selectTableByPk,
    selectTableAggregate,
    selectTableConnection,
    selectFunction,
    selectFunctionAggregate,
    selectFunctionConnection,
    computedFieldPG,
    remoteRelationshipField,
    defaultTableArgs,
    tableWhereArg,
    tableOrderByArg,
    tableDistinctArg,
    tableLimitArg,
    tableOffsetArg,
    tablePermissionsInfo,
    tableSelectionSet,
    tableSelectionList,
    nodePG,
    nodeField,
  )
where

import Control.Lens hiding (index)
import Data.Aeson qualified as J
import Data.Aeson.Extended qualified as J
import Data.Aeson.Internal qualified as J
import Data.Align (align)
import Data.ByteString.Lazy qualified as BL
import Data.Has
import Data.HashMap.Strict.Extended qualified as Map
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Data.Parser.JSONPath
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Text.Extended
import Data.These (partitionThese)
import Data.Traversable (mapAccumL)
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Types qualified as ET
import Hasura.GraphQL.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    Parser,
    UnpreparedValue (..),
    mkParameter,
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
  ( MonadParse (parseErrorWith, withPath),
    MonadSchema (..),
    MonadTableInfo,
    askTableInfo,
    parseError,
  )
import Hasura.GraphQL.Parser.Internal.Parser qualified as P
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.OrderBy
import {-# SOURCE #-} Hasura.GraphQL.Schema.RemoteRelationship
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Utils (executeJSONPath)
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- Top-level functions.
--
-- Those functions implement parsers for top-level components of the schema,
-- such as querying a table or a function. They are typically used to implement
-- root fields.

-- | Simple table selection.
--
-- The field for the table accepts table selection arguments, and
-- expects a selection of fields
--
-- > table_name(limit: 10) {
-- >   col1: col1_type
-- >   col2: col2_type
-- > }: [table!]!
selectTable ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | select permissions of the table
  SelPermInfo b ->
  m (FieldParser n (SelectExp b))
selectTable sourceName tableInfo fieldName description selectPermissions = memoizeOn 'selectTable (sourceName, tableName, fieldName) do
  stringifyNum <- asks $ qcStringifyNum . getter
  tableArgsParser <- tableArguments sourceName tableInfo selectPermissions
  selectionSetParser <- tableSelectionList sourceName tableInfo selectPermissions
  pure $
    P.subselection fieldName description tableArgsParser selectionSetParser
      <&> \(args, fields) ->
        IR.AnnSelectG
          { IR._asnFields = fields,
            IR._asnFrom = IR.FromTable tableName,
            IR._asnPerm = tablePermissionsInfo selectPermissions,
            IR._asnArgs = args,
            IR._asnStrfyNum = stringifyNum
          }
  where
    tableName = tableInfoName tableInfo

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
selectTableConnection ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | primary key columns
  PrimaryKeyColumns b ->
  -- | select permissions of the table
  SelPermInfo b ->
  m (Maybe (FieldParser n (ConnectionSelectExp b)))
selectTableConnection sourceName tableInfo fieldName description pkeyColumns selectPermissions =
  for (relayExtension @b) \xRelayInfo -> memoizeOn 'selectTableConnection (sourceName, tableName, fieldName) do
    stringifyNum <- asks $ qcStringifyNum . getter
    selectArgsParser <- tableConnectionArgs pkeyColumns sourceName tableInfo selectPermissions
    selectionSetParser <- P.nonNullableParser <$> tableConnectionSelectionSet sourceName tableInfo selectPermissions
    pure $
      P.subselection fieldName description selectArgsParser selectionSetParser
        <&> \((args, split, slice), fields) ->
          IR.ConnectionSelect
            { IR._csXRelay = xRelayInfo,
              IR._csPrimaryKeyColumns = pkeyColumns,
              IR._csSplit = split,
              IR._csSlice = slice,
              IR._csSelect =
                IR.AnnSelectG
                  { IR._asnFields = fields,
                    IR._asnFrom = IR.FromTable tableName,
                    IR._asnPerm = tablePermissionsInfo selectPermissions,
                    IR._asnArgs = args,
                    IR._asnStrfyNum = stringifyNum
                  }
            }
  where
    tableName = tableInfoName tableInfo

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
selectTableByPk ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | select permissions of the table
  SelPermInfo b ->
  m (Maybe (FieldParser n (SelectExp b)))
selectTableByPk sourceName tableInfo fieldName description selectPermissions = runMaybeT do
  primaryKeys <- hoistMaybe $ fmap _pkColumns . _tciPrimaryKey . _tiCoreInfo $ tableInfo
  guard $ all (\c -> ciColumn c `Map.member` spiCols selectPermissions) primaryKeys
  lift $ memoizeOn 'selectTableByPk (sourceName, tableName, fieldName) do
    stringifyNum <- asks $ qcStringifyNum . getter
    argsParser <-
      sequenceA <$> for primaryKeys \columnInfo -> do
        field <- columnParser (ciType columnInfo) (G.Nullability $ ciIsNullable columnInfo)
        pure $
          BoolFld . AVColumn columnInfo . pure . AEQ True . mkParameter
            <$> P.field (ciName columnInfo) (ciDescription columnInfo) field
    selectionSetParser <- tableSelectionSet sourceName tableInfo selectPermissions
    pure $
      P.subselection fieldName description argsParser selectionSetParser
        <&> \(boolExpr, fields) ->
          let defaultPerms = tablePermissionsInfo selectPermissions
              -- Do not account permission limit since the result is just a nullable object
              permissions = defaultPerms {IR._tpLimit = Nothing}
              whereExpr = Just $ BoolAnd $ toList boolExpr
           in IR.AnnSelectG
                { IR._asnFields = fields,
                  IR._asnFrom = IR.FromTable tableName,
                  IR._asnPerm = permissions,
                  IR._asnArgs = IR.noSelectArgs {IR._saWhere = whereExpr},
                  IR._asnStrfyNum = stringifyNum
                }
  where
    tableName = tableInfoName tableInfo

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
selectTableAggregate ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | select permissions of the table
  SelPermInfo b ->
  m (Maybe (FieldParser n (AggSelectExp b)))
selectTableAggregate sourceName tableInfo fieldName description selectPermissions = runMaybeT $ do
  guard $ spiAllowAgg selectPermissions
  xNodesAgg <- hoistMaybe $ nodesAggExtension @b
  lift $ memoizeOn 'selectTableAggregate (sourceName, tableName, fieldName) do
    stringifyNum <- asks $ qcStringifyNum . getter
    tableGQLName <- getTableGQLName tableInfo
    tableArgsParser <- tableArguments sourceName tableInfo selectPermissions
    aggregateParser <- tableAggregationFields sourceName tableInfo selectPermissions
    nodesParser <- tableSelectionList sourceName tableInfo selectPermissions
    selectionName <- P.mkTypename $ tableGQLName <> $$(G.litName "_aggregate")
    let aggregationParser =
          P.nonNullableParser $
            parsedSelectionsToFields IR.TAFExp
              <$> P.selectionSet
                selectionName
                (Just $ G.Description $ "aggregated selection of " <>> tableName)
                [ IR.TAFNodes xNodesAgg <$> P.subselection_ $$(G.litName "nodes") Nothing nodesParser,
                  IR.TAFAgg <$> P.subselection_ $$(G.litName "aggregate") Nothing aggregateParser
                ]
    pure $
      P.subselection fieldName description tableArgsParser aggregationParser
        <&> \(args, fields) ->
          IR.AnnSelectG
            { IR._asnFields = fields,
              IR._asnFrom = IR.FromTable tableName,
              IR._asnPerm = tablePermissionsInfo selectPermissions,
              IR._asnArgs = args,
              IR._asnStrfyNum = stringifyNum
            }
  where
    tableName = tableInfoName tableInfo

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
tableSelectionSet ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (Parser 'Output n (AnnotatedFields b))
tableSelectionSet sourceName tableInfo selectPermissions = memoizeOn 'tableSelectionSet (sourceName, tableName) do
  tableGQLName <- getTableGQLName tableInfo
  objectTypename <- P.mkTypename tableGQLName
  let xRelay = relayExtension @b
      tableFields = Map.elems $ _tciFieldInfoMap tableCoreInfo
      tablePkeyColumns = _pkColumns <$> _tciPrimaryKey tableCoreInfo
      description =
        Just $
          mkDescriptionWith (_tciDescription tableCoreInfo) $
            "columns and relationships of " <>> tableName
  fieldParsers <-
    concat <$> for tableFields \fieldInfo ->
      fieldSelection sourceName tableName tablePkeyColumns fieldInfo selectPermissions

  -- We don't check *here* that the subselection set is non-empty,
  -- even though the GraphQL specification requires that it is (see
  -- Note [Selectability of tables]). However, the GraphQL parser
  -- enforces that a selection set, if present, is non-empty; and our
  -- parser later verifies that a selection set is present if
  -- required, meaning that not having this check here does not allow
  -- for the construction of invalid queries.

  queryType <- asks $ qcQueryType . getter
  case (queryType, tablePkeyColumns, xRelay) of
    -- A relay table
    (ET.QueryRelay, Just pkeyColumns, Just xRelayInfo) -> do
      let nodeIdFieldParser =
            P.selection_ $$(G.litName "id") Nothing P.identifier $> IR.AFNodeId xRelayInfo tableName pkeyColumns
          allFieldParsers = fieldParsers <> [nodeIdFieldParser]
      nodeInterface <- node @b
      pure $
        P.selectionSetObject objectTypename description allFieldParsers [nodeInterface]
          <&> parsedSelectionsToFields IR.AFExpression
    _ ->
      pure $
        P.selectionSetObject objectTypename description fieldParsers []
          <&> parsedSelectionsToFields IR.AFExpression
  where
    tableName = tableInfoName tableInfo
    tableCoreInfo = _tiCoreInfo tableInfo

-- | List of table fields object.
-- Just a @'nonNullableObjectList' wrapper over @'tableSelectionSet'.
-- > table_name: [table!]!
tableSelectionList ::
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (Parser 'Output n (AnnotatedFields b))
tableSelectionList sourceName tableInfo selectPermissions =
  nonNullableObjectList <$> tableSelectionSet sourceName tableInfo selectPermissions

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
tableConnectionSelectionSet ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (Parser 'Output n (ConnectionFields b))
tableConnectionSelectionSet sourceName tableInfo selectPermissions = memoizeOn 'tableConnectionSelectionSet (sourceName, tableName) do
  tableGQLName <- getTableGQLName tableInfo
  edgesParser <- tableEdgesSelectionSet tableGQLName
  connectionTypeName <- P.mkTypename $ tableGQLName <> $$(G.litName "Connection")
  let pageInfo =
        P.subselection_
          $$(G.litName "pageInfo")
          Nothing
          pageInfoSelectionSet
          <&> IR.ConnectionPageInfo
      edges =
        P.subselection_
          $$(G.litName "edges")
          Nothing
          edgesParser
          <&> IR.ConnectionEdges
      connectionDescription = G.Description $ "A Relay connection object on " <>> tableName
  pure $
    P.nonNullableParser $
      P.selectionSet connectionTypeName (Just connectionDescription) [pageInfo, edges]
        <&> parsedSelectionsToFields IR.ConnectionTypename
  where
    tableName = tableInfoName tableInfo

    pageInfoSelectionSet :: Parser 'Output n IR.PageInfoFields
    pageInfoSelectionSet =
      let startCursorField =
            P.selection_
              $$(G.litName "startCursor")
              Nothing
              P.string
              $> IR.PageInfoStartCursor
          endCursorField =
            P.selection_
              $$(G.litName "endCursor")
              Nothing
              P.string
              $> IR.PageInfoEndCursor
          hasNextPageField =
            P.selection_
              $$(G.litName "hasNextPage")
              Nothing
              P.boolean
              $> IR.PageInfoHasNextPage
          hasPreviousPageField =
            P.selection_
              $$(G.litName "hasPreviousPage")
              Nothing
              P.boolean
              $> IR.PageInfoHasPreviousPage
          allFields =
            [ startCursorField,
              endCursorField,
              hasNextPageField,
              hasPreviousPageField
            ]
       in P.nonNullableParser $
            P.selectionSet $$(G.litName "PageInfo") Nothing allFields
              <&> parsedSelectionsToFields IR.PageInfoTypename

    tableEdgesSelectionSet ::
      G.Name -> m (Parser 'Output n (EdgeFields b))
    tableEdgesSelectionSet tableGQLName = do
      edgeNodeParser <- P.nonNullableParser <$> tableSelectionSet sourceName tableInfo selectPermissions
      edgesType <- P.mkTypename $ tableGQLName <> $$(G.litName "Edge")
      let cursor =
            P.selection_
              $$(G.litName "cursor")
              Nothing
              P.string
              $> IR.EdgeCursor
          edgeNode =
            P.subselection_
              $$(G.litName "node")
              Nothing
              edgeNodeParser
              <&> IR.EdgeNode
      pure $
        nonNullableObjectList $
          P.selectionSet edgesType Nothing [cursor, edgeNode]
            <&> parsedSelectionsToFields IR.EdgeTypename

-- | User-defined function (AKA custom function)
selectFunction ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  -- | source name
  SourceName ->
  -- | SQL function info
  FunctionInfo b ->
  -- | field description, if any
  Maybe G.Description ->
  -- | select permissions of the target table
  SelPermInfo b ->
  m (FieldParser n (SelectExp b))
selectFunction sourceName fi@FunctionInfo {..} description selectPermissions = do
  stringifyNum <- asks $ qcStringifyNum . getter
  tableInfo <- askTableInfo sourceName _fiReturnType
  tableArgsParser <- tableArguments sourceName tableInfo selectPermissions
  selectionSetParser <- returnFunctionParser sourceName tableInfo selectPermissions
  functionArgsParser <- customSQLFunctionArgs fi _fiGQLName _fiGQLArgsName
  let argsParser = liftA2 (,) functionArgsParser tableArgsParser
  functionFieldName <- mkRootFieldName _fiGQLName
  pure $
    P.subselection functionFieldName description argsParser selectionSetParser
      <&> \((funcArgs, tableArgs'), fields) ->
        IR.AnnSelectG
          { IR._asnFields = fields,
            IR._asnFrom = IR.FromFunction _fiSQLName funcArgs Nothing,
            IR._asnPerm = tablePermissionsInfo selectPermissions,
            IR._asnArgs = tableArgs',
            IR._asnStrfyNum = stringifyNum
          }
  where
    returnFunctionParser =
      case _fiJsonAggSelect of
        JASSingleObject -> tableSelectionSet
        JASMultipleRows -> tableSelectionList

selectFunctionAggregate ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  -- | source name
  SourceName ->
  -- | SQL function info
  FunctionInfo b ->
  -- | field description, if any
  Maybe G.Description ->
  -- | select permissions of the target table
  SelPermInfo b ->
  m (Maybe (FieldParser n (AggSelectExp b)))
selectFunctionAggregate sourceName fi@FunctionInfo {..} description selectPermissions = runMaybeT do
  guard $ spiAllowAgg selectPermissions
  xNodesAgg <- hoistMaybe $ nodesAggExtension @b
  tableInfo <- askTableInfo sourceName _fiReturnType
  stringifyNum <- asks $ qcStringifyNum . getter
  tableGQLName <- getTableGQLName tableInfo
  tableArgsParser <- lift $ tableArguments sourceName tableInfo selectPermissions
  functionArgsParser <- lift $ customSQLFunctionArgs fi _fiGQLAggregateName _fiGQLArgsName
  aggregateParser <- lift $ tableAggregationFields sourceName tableInfo selectPermissions
  selectionName <- P.mkTypename =<< lift (pure tableGQLName <&> (<> $$(G.litName "_aggregate")))
  nodesParser <- lift $ tableSelectionList sourceName tableInfo selectPermissions
  aggregateFieldName <- mkRootFieldName _fiGQLAggregateName
  let argsParser = liftA2 (,) functionArgsParser tableArgsParser
      aggregationParser =
        fmap (parsedSelectionsToFields IR.TAFExp) $
          P.nonNullableParser $
            P.selectionSet
              selectionName
              Nothing
              [ IR.TAFNodes xNodesAgg <$> P.subselection_ $$(G.litName "nodes") Nothing nodesParser,
                IR.TAFAgg <$> P.subselection_ $$(G.litName "aggregate") Nothing aggregateParser
              ]
  pure $
    P.subselection aggregateFieldName description argsParser aggregationParser
      <&> \((funcArgs, tableArgs'), fields) ->
        IR.AnnSelectG
          { IR._asnFields = fields,
            IR._asnFrom = IR.FromFunction _fiSQLName funcArgs Nothing,
            IR._asnPerm = tablePermissionsInfo selectPermissions,
            IR._asnArgs = tableArgs',
            IR._asnStrfyNum = stringifyNum
          }

selectFunctionConnection ::
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  -- | source name
  SourceName ->
  -- | SQL function info
  FunctionInfo ('Postgres pgKind) ->
  -- | field description, if any
  Maybe G.Description ->
  -- | primary key columns of the target table
  PrimaryKeyColumns ('Postgres pgKind) ->
  -- | select permissions of the target table
  SelPermInfo ('Postgres pgKind) ->
  m (Maybe (FieldParser n (ConnectionSelectExp ('Postgres pgKind))))
selectFunctionConnection sourceName fi@FunctionInfo {..} description pkeyColumns selectPermissions = do
  fieldName <- mkRootFieldName $ _fiGQLName <> $$(G.litName "_connection")
  for (relayExtension @('Postgres pgKind)) \xRelayInfo -> do
    stringifyNum <- asks $ qcStringifyNum . getter
    tableInfo <- askTableInfo sourceName _fiReturnType
    tableConnectionArgsParser <- tableConnectionArgs pkeyColumns sourceName tableInfo selectPermissions
    functionArgsParser <- customSQLFunctionArgs fi _fiGQLName _fiGQLArgsName
    selectionSetParser <- tableConnectionSelectionSet sourceName tableInfo selectPermissions
    let argsParser = liftA2 (,) functionArgsParser tableConnectionArgsParser
    pure $
      P.subselection fieldName description argsParser selectionSetParser
        <&> \((funcArgs, (args, split, slice)), fields) ->
          IR.ConnectionSelect
            { IR._csXRelay = xRelayInfo,
              IR._csPrimaryKeyColumns = pkeyColumns,
              IR._csSplit = split,
              IR._csSlice = slice,
              IR._csSelect =
                IR.AnnSelectG
                  { IR._asnFields = fields,
                    IR._asnFrom = IR.FromFunction _fiSQLName funcArgs Nothing,
                    IR._asnPerm = tablePermissionsInfo selectPermissions,
                    IR._asnArgs = args,
                    IR._asnStrfyNum = stringifyNum
                  }
            }

--------------------------------------------------------------------------------
-- Components
--
-- Those parsers are sub-components of those top-level parsers.

-- | Arguments for a table selection. Default implementation for BackendSchema.
--
-- > distinct_on: [table_select_column!]
-- > limit: Int
-- > offset: Int
-- > order_by: [table_order_by!]
-- > where: table_bool_exp
defaultTableArgs ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (InputFieldsParser n (SelectArgs b))
defaultTableArgs sourceName tableInfo selectPermissions = do
  whereParser <- tableWhereArg sourceName tableInfo selectPermissions
  orderByParser <- tableOrderByArg sourceName tableInfo selectPermissions
  distinctParser <- tableDistinctArg sourceName tableInfo selectPermissions
  let result = do
        whereArg <- whereParser
        orderByArg <- orderByParser
        limitArg <- tableLimitArg
        offsetArg <- tableOffsetArg
        distinctArg <- distinctParser
        pure $
          IR.SelectArgs
            { IR._saWhere = whereArg,
              IR._saOrderBy = orderByArg,
              IR._saLimit = limitArg,
              IR._saOffset = offsetArg,
              IR._saDistinct = distinctArg
            }
  pure $
    result `P.bindFields` \args -> do
      sequence_ do
        orderBy <- IR._saOrderBy args
        distinct <- IR._saDistinct args
        Just $ validateArgs orderBy distinct
      pure args
  where
    validateArgs orderByCols distinctCols = do
      let colsLen = length distinctCols
          initOrderBys = take colsLen $ NE.toList orderByCols
          initOrdByCols = flip mapMaybe initOrderBys $ \ob ->
            case IR.obiColumn ob of
              IR.AOCColumn columnInfo -> Just $ ciColumn columnInfo
              _ -> Nothing
          isValid =
            (colsLen == length initOrdByCols)
              && all (`elem` initOrdByCols) (toList distinctCols)
      unless isValid $
        parseError
          "\"distinct_on\" columns must match initial \"order_by\" columns"

-- | Argument to filter rows returned from table selection
-- > where: table_bool_exp
tableWhereArg ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (InputFieldsParser n (Maybe (IR.AnnBoolExp b (UnpreparedValue b))))
tableWhereArg sourceName tableInfo selectPermissions = do
  boolExpParser <- boolExp sourceName tableInfo (Just selectPermissions)
  pure $
    fmap join $
      P.fieldOptional whereName whereDesc $
        P.nullable boolExpParser
  where
    whereName = $$(G.litName "where")
    whereDesc = Just $ G.Description "filter the rows returned"

-- | Argument to sort rows returned from table selection
-- > order_by: [table_order_by!]
tableOrderByArg ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (InputFieldsParser n (Maybe (NonEmpty (IR.AnnotatedOrderByItemG b (UnpreparedValue b)))))
tableOrderByArg sourceName tableInfo selectPermissions = do
  orderByParser <- orderByExp sourceName tableInfo selectPermissions
  pure $ do
    maybeOrderByExps <-
      fmap join $
        P.fieldOptional orderByName orderByDesc $ P.nullable $ P.list orderByParser
    pure $ maybeOrderByExps >>= NE.nonEmpty . concat
  where
    orderByName = $$(G.litName "order_by")
    orderByDesc = Just $ G.Description "sort the rows by one or more columns"

-- | Argument to distinct select on columns returned from table selection
-- > distinct_on: [table_select_column!]
tableDistinctArg ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (InputFieldsParser n (Maybe (NonEmpty (Column b))))
tableDistinctArg sourceName tableInfo selectPermissions = do
  columnsEnum <- tableSelectColumnsEnum sourceName tableInfo selectPermissions
  pure do
    maybeDistinctOnColumns <-
      join . join
        <$> for
          columnsEnum
          (P.fieldOptional distinctOnName distinctOnDesc . P.nullable . P.list)
    pure $ maybeDistinctOnColumns >>= NE.nonEmpty
  where
    distinctOnName = $$(G.litName "distinct_on")
    distinctOnDesc = Just $ G.Description "distinct select on columns"

-- | Argument to limit rows returned from table selection
-- > limit: NonNegativeInt
tableLimitArg ::
  forall n.
  MonadParse n =>
  InputFieldsParser n (Maybe Int)
tableLimitArg =
  fmap (fmap fromIntegral . join) $
    P.fieldOptional limitName limitDesc $
      P.nullable P.nonNegativeInt
  where
    limitName = $$(G.litName "limit")
    limitDesc = Just $ G.Description "limit the number of rows returned"

-- | Argument to skip some rows, in conjunction with order_by
-- > offset: BigInt
tableOffsetArg ::
  forall n.
  MonadParse n =>
  InputFieldsParser n (Maybe Int64)
tableOffsetArg =
  fmap join $
    P.fieldOptional offsetName offsetDesc $
      P.nullable P.bigInt
  where
    offsetName = $$(G.litName "offset")
    offsetDesc = Just $ G.Description "skip the first n rows. Use only with order_by"

-- | Arguments for a table connection selection
--
-- > distinct_on: [table_select_column!]
-- > order_by: [table_order_by!]
-- > where: table_bool_exp
-- > first: Int
-- > last: Int
-- > before: String
-- > after: String
tableConnectionArgs ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  PrimaryKeyColumns b ->
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m
    ( InputFieldsParser
        n
        ( SelectArgs b,
          Maybe (NonEmpty (IR.ConnectionSplit b (UnpreparedValue b))),
          Maybe IR.ConnectionSlice
        )
    )
tableConnectionArgs pkeyColumns sourceName tableInfo selectPermissions = do
  whereParser <- tableWhereArg sourceName tableInfo selectPermissions
  orderByParser <- fmap (fmap appendPrimaryKeyOrderBy) <$> tableOrderByArg sourceName tableInfo selectPermissions
  distinctParser <- tableDistinctArg sourceName tableInfo selectPermissions
  let maybeFirst = fmap join $ P.fieldOptional $$(G.litName "first") Nothing $ P.nullable P.nonNegativeInt
      maybeLast = fmap join $ P.fieldOptional $$(G.litName "last") Nothing $ P.nullable P.nonNegativeInt
      maybeAfter = fmap join $ P.fieldOptional $$(G.litName "after") Nothing $ P.nullable base64Text
      maybeBefore = fmap join $ P.fieldOptional $$(G.litName "before") Nothing $ P.nullable base64Text
      firstAndLast = (,) <$> maybeFirst <*> maybeLast
      afterBeforeAndOrderBy = (,,) <$> maybeAfter <*> maybeBefore <*> orderByParser

  pure $ do
    whereF <- whereParser
    orderBy <- orderByParser
    distinct <- distinctParser
    split <-
      afterBeforeAndOrderBy `P.bindFields` \(after, before, orderBy') -> do
        rawSplit <- case (after, before) of
          (Nothing, Nothing) -> pure Nothing
          (Just _, Just _) -> parseError "\"after\" and \"before\" are not allowed at once"
          (Just v, Nothing) -> pure $ Just (IR.CSKAfter, v)
          (Nothing, Just v) -> pure $ Just (IR.CSKBefore, v)
        for rawSplit (uncurry (parseConnectionSplit orderBy'))

    slice <-
      firstAndLast `P.bindFields` \case
        (Nothing, Nothing) -> pure Nothing
        (Just _, Just _) -> parseError "\"first\" and \"last\" are not allowed at once"
        (Just v, Nothing) -> pure $ Just $ IR.SliceFirst $ fromIntegral v
        (Nothing, Just v) -> pure $ Just $ IR.SliceLast $ fromIntegral v

    pure
      ( IR.SelectArgs whereF orderBy Nothing Nothing distinct,
        split,
        slice
      )
  where
    base64Text = base64Decode <$> P.string

    appendPrimaryKeyOrderBy :: NonEmpty (IR.AnnotatedOrderByItemG b v) -> NonEmpty (IR.AnnotatedOrderByItemG b v)
    appendPrimaryKeyOrderBy orderBys@(h NE.:| t) =
      let orderByColumnNames =
            orderBys ^.. traverse . to IR.obiColumn . IR._AOCColumn . to ciColumn
          pkeyOrderBys = flip mapMaybe (toList pkeyColumns) $ \columnInfo ->
            if ciColumn columnInfo `elem` orderByColumnNames
              then Nothing
              else Just $ IR.OrderByItemG Nothing (IR.AOCColumn columnInfo) Nothing
       in h NE.:| (t <> pkeyOrderBys)

    parseConnectionSplit ::
      Maybe (NonEmpty (IR.AnnotatedOrderByItemG b (UnpreparedValue b))) ->
      IR.ConnectionSplitKind ->
      BL.ByteString ->
      n (NonEmpty (IR.ConnectionSplit b (UnpreparedValue b)))
    parseConnectionSplit maybeOrderBys splitKind cursorSplit = do
      cursorValue <- J.eitherDecode cursorSplit `onLeft` const throwInvalidCursor
      case maybeOrderBys of
        Nothing -> forM (NESeq.toNonEmpty pkeyColumns) $
          \columnInfo -> do
            let columnJsonPath = [J.Key $ toTxt $ ciColumn columnInfo]
                columnType = ciType columnInfo
            columnValue <-
              iResultToMaybe (executeJSONPath columnJsonPath cursorValue)
                `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parseScalarValueColumnType columnType columnValue
            let unresolvedValue = UVParameter Nothing $ ColumnValue columnType pgValue
            pure $
              IR.ConnectionSplit splitKind unresolvedValue $
                IR.OrderByItemG Nothing (IR.AOCColumn columnInfo) Nothing
        Just orderBys ->
          forM orderBys $ \orderBy -> do
            let IR.OrderByItemG orderType annObCol nullsOrder = orderBy
                columnType = getOrderByColumnType annObCol
            orderByItemValue <-
              iResultToMaybe (executeJSONPath (getPathFromOrderBy annObCol) cursorValue)
                `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parseScalarValueColumnType columnType orderByItemValue
            let unresolvedValue = UVParameter Nothing $ ColumnValue columnType pgValue
            pure $
              IR.ConnectionSplit splitKind unresolvedValue $
                IR.OrderByItemG orderType annObCol nullsOrder
      where
        throwInvalidCursor = parseError "the \"after\" or \"before\" cursor is invalid"
        liftQErr = either (parseError . qeError) pure . runExcept

        mkAggregateOrderByPath = \case
          IR.AAOCount -> [J.Key "count"]
          IR.AAOOp t col -> [J.Key t, J.Key $ toTxt $ ciColumn col]

        getPathFromOrderBy = \case
          IR.AOCColumn columnInfo ->
            let pathElement = J.Key $ toTxt $ ciColumn columnInfo
             in [pathElement]
          IR.AOCObjectRelation relInfo _ obCol ->
            let pathElement = J.Key $ relNameToTxt $ riName relInfo
             in pathElement : getPathFromOrderBy obCol
          IR.AOCArrayAggregation relInfo _ aggOb ->
            let fieldName = J.Key $ relNameToTxt (riName relInfo) <> "_aggregate"
             in fieldName : mkAggregateOrderByPath aggOb
          IR.AOCComputedField cfob ->
            let fieldNameText = computedFieldNameToText $ IR._cfobName cfob
             in case IR._cfobOrderByElement cfob of
                  IR.CFOBEScalar _ -> [J.Key fieldNameText]
                  IR.CFOBETableAggregation _ _ aggOb ->
                    J.Key (fieldNameText <> "_aggregate") : mkAggregateOrderByPath aggOb

        getOrderByColumnType = \case
          IR.AOCColumn columnInfo -> ciType columnInfo
          IR.AOCObjectRelation _ _ obCol -> getOrderByColumnType obCol
          IR.AOCArrayAggregation _ _ aggOb -> aggregateOrderByColumnType aggOb
          IR.AOCComputedField cfob ->
            case IR._cfobOrderByElement cfob of
              IR.CFOBEScalar scalarType -> ColumnScalar scalarType
              IR.CFOBETableAggregation _ _ aggOb -> aggregateOrderByColumnType aggOb
          where
            aggregateOrderByColumnType = \case
              IR.AAOCount -> ColumnScalar (aggregateOrderByCountType @b)
              IR.AAOOp _ colInfo -> ciType colInfo

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
tableAggregationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  SelPermInfo b ->
  m (Parser 'Output n (IR.AggregateFields b))
tableAggregationFields sourceName tableInfo selectPermissions = memoizeOn 'tableAggregationFields (sourceName, tableInfoName tableInfo) do
  tableGQLName <- getTableGQLName tableInfo
  allColumns <- tableSelectColumns sourceName tableInfo selectPermissions
  let numericColumns = onlyNumCols allColumns
      comparableColumns = onlyComparableCols allColumns
      description = G.Description $ "aggregate fields of " <>> tableInfoName tableInfo
  selectName <- P.mkTypename $ tableGQLName <> $$(G.litName "_aggregate_fields")
  count <- countField
  mkTypename <- asks getter
  numericAndComparable <-
    fmap concat $
      sequenceA $
        catMaybes
          [ -- operators on numeric columns
            if null numericColumns
              then Nothing
              else Just $
                for numericAggOperators $ \operator -> do
                  numFields <- mkNumericAggFields operator numericColumns
                  pure $ parseAggOperator mkTypename operator tableGQLName numFields,
            -- operators on comparable columns
            if null comparableColumns
              then Nothing
              else Just $ do
                comparableFields <- traverse mkColumnAggField comparableColumns
                pure $
                  comparisonAggOperators & map \operator ->
                    parseAggOperator mkTypename operator tableGQLName comparableFields
          ]
  let aggregateFields = count : numericAndComparable
  pure $
    P.selectionSet selectName (Just description) aggregateFields
      <&> parsedSelectionsToFields IR.AFExp
  where
    mkNumericAggFields :: G.Name -> [ColumnInfo b] -> m [FieldParser n (IR.ColFld b)]
    mkNumericAggFields name
      | name == $$(G.litName "sum") = traverse mkColumnAggField
      | otherwise = traverse \columnInfo ->
        pure $
          P.selection_
            (ciName columnInfo)
            (ciDescription columnInfo)
            (P.nullable P.float)
            $> IR.CFCol (ciColumn columnInfo) (ciType columnInfo)

    mkColumnAggField :: ColumnInfo b -> m (FieldParser n (IR.ColFld b))
    mkColumnAggField columnInfo = do
      field <- columnParser (ciType columnInfo) (G.Nullability True)
      pure $
        P.selection_
          (ciName columnInfo)
          (ciDescription columnInfo)
          field
          $> IR.CFCol (ciColumn columnInfo) (ciType columnInfo)

    countField :: m (FieldParser n (IR.AggregateField b))
    countField = do
      columnsEnum <- tableSelectColumnsEnum sourceName tableInfo selectPermissions
      let distinctName = $$(G.litName "distinct")
          args = do
            distinct <- P.fieldOptional distinctName Nothing P.boolean
            mkCountType <- countTypeInput @b columnsEnum
            pure $
              mkCountType $
                maybe
                  IR.SelectCountNonDistinct -- If "distinct" is "null" or absent, we default to @'SelectCountNonDistinct'
                  (bool IR.SelectCountNonDistinct IR.SelectCountDistinct)
                  distinct

      pure $ IR.AFCount <$> P.selection $$(G.litName "count") Nothing args P.int

    parseAggOperator ::
      P.MkTypename ->
      G.Name ->
      G.Name ->
      [FieldParser n (IR.ColFld b)] ->
      FieldParser n (IR.AggregateField b)
    parseAggOperator mkTypename operator tableGQLName columns =
      let opText = G.unName operator
          setName = P.runMkTypename mkTypename $ tableGQLName <> $$(G.litName "_") <> operator <> $$(G.litName "_fields")
          setDesc = Just $ G.Description $ "aggregate " <> opText <> " on columns"
          subselectionParser =
            P.selectionSet setName setDesc columns
              <&> parsedSelectionsToFields IR.CFExp
       in P.subselection_ operator Nothing subselectionParser
            <&> IR.AFOp . IR.AggregateOp opText

-- | An individual field of a table
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableName b ->
  Maybe (PrimaryKeyColumns b) ->
  FieldInfo b ->
  SelPermInfo b ->
  m [FieldParser n (AnnotatedField b)]
fieldSelection sourceName table maybePkeyColumns fieldInfo selectPermissions = do
  case fieldInfo of
    FIColumn columnInfo ->
      maybeToList <$> runMaybeT do
        queryType <- asks $ qcQueryType . getter
        let columnName = ciColumn columnInfo
            fieldName = ciName columnInfo
        if fieldName == $$(G.litName "id") && queryType == ET.QueryRelay
          then do
            xRelayInfo <- hoistMaybe $ relayExtension @b
            pkeyColumns <- hoistMaybe maybePkeyColumns
            pure $
              P.selection_ fieldName Nothing P.identifier
                $> IR.AFNodeId xRelayInfo table pkeyColumns
          else do
            guard $ columnName `Map.member` spiCols selectPermissions
            let caseBoolExp = join $ Map.lookup columnName (spiCols selectPermissions)
            let caseBoolExpUnpreparedValue =
                  (fmap . fmap) partialSQLExpToUnpreparedValue <$> caseBoolExp
            let pathArg = jsonPathArg $ ciType columnInfo
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
                nullability = ciIsNullable columnInfo || isJust caseBoolExp
            field <- lift $ columnParser (ciType columnInfo) (G.Nullability nullability)
            pure $
              P.selection fieldName (ciDescription columnInfo) pathArg field
                <&> IR.mkAnnColumnField (ciColumn columnInfo) (ciType columnInfo) caseBoolExpUnpreparedValue
    FIRelationship relationshipInfo ->
      concat . maybeToList <$> relationshipField sourceName table relationshipInfo
    FIComputedField computedFieldInfo ->
      maybeToList <$> computedField sourceName computedFieldInfo table selectPermissions
    FIRemoteRelationship remoteFieldInfo -> do
      relationshipFields <- fromMaybe [] <$> remoteRelationshipField remoteFieldInfo
      let lhsFields = _rfiLHS remoteFieldInfo
      pure $ map (fmap (IR.AFRemote . IR.RemoteRelationshipSelect lhsFields)) relationshipFields

{- Note [Permission filter deduplication]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. `T` and `U` are tables.

1. `r` is a relationship on `T` to table `U` with the join condition, `T.c =
   U.d` where `c` and `d` are columns on tables `T` and `U` respectively.

1. `s` is a relationship on `U` to table `T` with the join condition, `U.d =
   T.c`.

1. `p(T)` and `p(U)` denote the permission filters on table `T` and `U`
   respectively for some role `R`.

Consider the SQL that we generate for this query:

```
query {
  T {
    c
    r {
      d
    }
  }
}
```

It would be along these lines:

```sql
SELECT
  *
FROM
  (
    SELECT * FROM T WHERE p(T)
  ) AS T
  LEFT OUTER JOIN LATERAL
  (
    SELECT * FROM U WHERE T.c = U.d AND p(U)
  ) AS U
  ON TRUE
```

The expression `T.c = U.d` is the join condition for relationship `r`. Note
that we use lateral joins, so the join condition is not expressed using `ON`
but on the where clause of `U`.

Now, let's say `p(U)` is of the form `{ s : p(T) }`.

```sql
SELECT
  *
FROM
  (
    SELECT * FROM T WHERE p(T)
  ) AS T
  LEFT OUTER JOIN LATERAL
  (
    SELECT * FROM U WHERE T.c = U.d
    AND EXISTS (
      SELECT 1 FROM T WHERE U.d = T.c AND p(T)
    )
  ) AS U
  ON TRUE
```

`p(U)`, i.e, `{ s : p(T) }` got expanded to

```sql
EXISTS (
  SELECT 1 FROM T WHERE U.d = T.c AND p(T)
)
```

Now, assuming, in the `WHERE` clause for `U`, that `T.c = U.d` holds, then the
`EXISTS` clause must evaluate to true. The `EXISTS` clause must evaluate to true
because the row from `T` we are joining against is exactly such a row satisfying
`p(T)`. In other words, the row obtained from `T` (as the left-hand side of the
join) satisfies `p(T)`.
-}

-- | Field parsers for a table relationship
relationshipField ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableName b ->
  RelInfo b ->
  m (Maybe [FieldParser n (AnnotatedField b)])
relationshipField sourceName table ri = runMaybeT do
  optimizePermissionFilters <- asks $ qcOptimizePermissionFilters . getter
  otherTableInfo <- lift $ askTableInfo sourceName $ riRTable ri
  remotePerms <- MaybeT $ tableSelectPermissions otherTableInfo
  relFieldName <- lift $ textToName $ relNameToTxt $ riName ri
  -- START black magic to deduplicate permission checks
  thisTablePerm <-
    IR._tpFilter . tablePermissionsInfo
      <$> MaybeT (tableSelectPermissions =<< askTableInfo @b sourceName table)
  let deduplicatePermissions :: AnnBoolExp b (UnpreparedValue b) -> AnnBoolExp b (UnpreparedValue b)
      deduplicatePermissions x =
        case (optimizePermissionFilters, x) of
          (True, BoolAnd [BoolFld (AVRelationship remoteRI remoteTablePerm)]) ->
            -- Here we try to figure out if the "forwards" joining condition
            -- from `table` to the related table `riRTable ri` is equal to the
            -- "backwards" joining condition from the related table back to
            -- `table`.  If it is, then we can optimize the row-level permission
            -- filters by dropping them here.
            if riRTable remoteRI == table
              && riMapping remoteRI `Map.isInverseOf` riMapping ri
              && thisTablePerm == remoteTablePerm
              then BoolAnd []
              else x
          _ -> x
      deduplicatePermissions' :: SelectExp b -> SelectExp b
      deduplicatePermissions' expr =
        let newFilter = deduplicatePermissions (IR._tpFilter (IR._asnPerm expr))
         in expr {IR._asnPerm = (IR._asnPerm expr) {IR._tpFilter = newFilter}}
  -- END black magic to deduplicate permission checks

  case riType ri of
    ObjRel -> do
      let desc = Just $ G.Description "An object relationship"
      selectionSetParser <- lift $ tableSelectionSet sourceName otherTableInfo remotePerms
      -- We need to set the correct nullability of our GraphQL field.  Manual
      -- relationships are always nullable, and so are "reverse" object
      -- relationships, i.e. Hasura relationships that are generated from a
      -- referenced table to a referencing table.  For automatic forward object
      -- relationships, i.e. the generated relationship from table1 to table2,
      -- where table1 has a foreign key constraint, we have to do some work.
      --
      -- Specifically, we would like to mark the relationship generated from a
      -- foreign key constraint from table1 to table2 to be non-nullable if
      -- Postgres enforces that for each row in table1, a corresponding row in
      -- table2 exists.  From the Postgres manual:
      --
      -- "Normally, a referencing row need not satisfy the foreign key
      -- constraint if any of its referencing columns are null.  If MATCH FULL
      -- is added to the foreign key declaration, a referencing row escapes
      -- satisfying the constraint only if all its referencing columns are null
      -- (so a mix of null and non-null values is guaranteed to fail a MATCH
      -- FULL constraint)."
      --
      -- https://www.postgresql.org/docs/9.5/ddl-constraints.html#DDL-CONSTRAINTS-FK
      --
      -- Since we don't store MATCH FULL in the RQL representation of the
      -- database, the closest we can get is to only set the field to be
      -- non-nullable if _all_ of the columns that reference the foreign table
      -- are non-nullable.  Strictly speaking, we could do slightly better by
      -- setting the field to be non-nullable also if the foreign key has MATCH
      -- FULL set, and _any_ of the columns is non-nullable.  But we skip doing
      -- this since, as of writing this, the old code used to make the wrong
      -- decision about nullability of joint foreign keys entirely, so this is
      -- probably not a very widely used mode of use.  The impact of this
      -- suboptimality is merely that in introspection some fields might get
      -- marked nullable which are in fact known to always be non-null.
      nullable <- case (riIsManual ri, riInsertOrder ri) of
        -- Automatically generated forward relationship
        (False, BeforeParent) -> do
          tableInfo <- askTableInfo @b sourceName table
          let columns = Map.keys $ riMapping ri
              fieldInfoMap = _tciFieldInfoMap $ _tiCoreInfo tableInfo
              findColumn col = Map.lookup (fromCol @b col) fieldInfoMap ^? _Just . _FIColumn
          -- Fetch information about the referencing columns of the foreign key
          -- constraint
          colInfo <-
            traverse findColumn columns
              `onNothing` throw500 "could not find column info in schema cache"
          pure $ boolToNullable $ any ciIsNullable colInfo
        -- Manual or reverse relationships are always nullable
        _ -> pure Nullable
      pure $
        pure $
          case nullable of { Nullable -> id; NotNullable -> P.nonNullableField } $
            P.subselection_ relFieldName desc selectionSetParser
              <&> \fields ->
                IR.AFObjectRelation $
                  IR.AnnRelationSelectG (riName ri) (riMapping ri) $
                    IR.AnnObjectSelectG fields (riRTable ri) $
                      deduplicatePermissions $
                        IR._tpFilter $ tablePermissionsInfo remotePerms
    ArrRel -> do
      let arrayRelDesc = Just $ G.Description "An array relationship"
      otherTableParser <- lift $ selectTable sourceName otherTableInfo relFieldName arrayRelDesc remotePerms
      let arrayRelField =
            otherTableParser <&> \selectExp ->
              IR.AFArrayRelation $
                IR.ASSimple $
                  IR.AnnRelationSelectG (riName ri) (riMapping ri) $
                    deduplicatePermissions' selectExp
          relAggFieldName = relFieldName <> $$(G.litName "_aggregate")
          relAggDesc = Just $ G.Description "An aggregate relationship"
      remoteAggField <- lift $ selectTableAggregate sourceName otherTableInfo relAggFieldName relAggDesc remotePerms
      remoteConnectionField <- runMaybeT $ do
        -- Parse array connection field only for relay schema
        queryType <- asks $ qcQueryType . getter
        guard $ queryType == ET.QueryRelay
        xRelayInfo <- hoistMaybe $ relayExtension @b
        pkeyColumns <-
          MaybeT $
            (^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns)
              <$> pure otherTableInfo
        let relConnectionName = relFieldName <> $$(G.litName "_connection")
            relConnectionDesc = Just $ G.Description "An array relationship connection"
        MaybeT $ lift $ selectTableConnection sourceName otherTableInfo relConnectionName relConnectionDesc pkeyColumns remotePerms
      pure $
        catMaybes
          [ Just arrayRelField,
            fmap (IR.AFArrayRelation . IR.ASAggregate . IR.AnnRelationSelectG (riName ri) (riMapping ri)) <$> remoteAggField,
            fmap (IR.AFArrayRelation . IR.ASConnection . IR.AnnRelationSelectG (riName ri) (riMapping ri)) <$> remoteConnectionField
          ]

-- | Computed field parser
computedFieldPG ::
  forall pgKind r m n.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceName ->
  ComputedFieldInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  SelPermInfo ('Postgres pgKind) ->
  m (Maybe (FieldParser n (AnnotatedField ('Postgres pgKind))))
computedFieldPG sourceName ComputedFieldInfo {..} parentTable selectPermissions = runMaybeT do
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
            (fmap . fmap) partialSQLExpToUnpreparedValue <$> caseBoolExpMaybe
          fieldArgsParser = do
            args <- functionArgsParser
            colOp <- jsonPathArg $ ColumnScalar scalarReturnType
            pure $
              IR.AFComputedField
                _cfiXComputedFieldInfo
                _cfiName
                ( IR.CFSScalar
                    ( IR.ComputedFieldScalarSelect
                        { IR._cfssFunction = _cffName _cfiFunction,
                          IR._cfssType = scalarReturnType,
                          IR._cfssColumnOp = colOp,
                          IR._cfssArguments = args
                        }
                    )
                    caseBoolExpUnpreparedValue
                )
      dummyParser <- lift $ columnParser @('Postgres pgKind) (ColumnScalar scalarReturnType) (G.Nullability True)
      pure $ P.selection fieldName (Just fieldDescription) fieldArgsParser dummyParser
    CFRSetofTable tableName -> do
      tableInfo <- lift $ askTableInfo sourceName tableName
      remotePerms <- MaybeT $ tableSelectPermissions tableInfo
      selectArgsParser <- lift $ tableArguments sourceName tableInfo remotePerms
      selectionSetParser <- lift $ P.multiple . P.nonNullableParser <$> tableSelectionSet sourceName tableInfo remotePerms
      let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
      pure $
        P.subselection fieldName (Just fieldDescription) fieldArgsParser selectionSetParser
          <&> \((functionArgs', args), fields) ->
            IR.AFComputedField _cfiXComputedFieldInfo _cfiName $
              IR.CFSTable JASMultipleRows $
                IR.AnnSelectG
                  { IR._asnFields = fields,
                    IR._asnFrom = IR.FromFunction (_cffName _cfiFunction) functionArgs' Nothing,
                    IR._asnPerm = tablePermissionsInfo remotePerms,
                    IR._asnArgs = args,
                    IR._asnStrfyNum = stringifyNum
                  }
  where
    fieldDescription =
      let defaultDescription = "A computed field, executes function " <>> _cffName _cfiFunction
       in mkDescriptionWith (_cffDescription _cfiFunction) defaultDescription

    computedFieldFunctionArgs ::
      ComputedFieldFunction ('Postgres pgKind) ->
      m (InputFieldsParser n (IR.FunctionArgsExpTableRow (UnpreparedValue ('Postgres pgKind))))
    computedFieldFunctionArgs ComputedFieldFunction {..} =
      functionArgs (FTAComputedField _cfiName sourceName parentTable) (IAUserProvided <$> _cffInputArgs) <&> fmap addTableAndSessionArgument
      where
        addTableAndSessionArgument args@(IR.FunctionArgsExp positional named) =
          let withTable = case _cffTableArgument of
                FTAFirst -> IR.FunctionArgsExp (IR.AETableRow : positional) named
                FTANamed argName index -> IR.insertFunctionArg argName index IR.AETableRow args
              sessionArgVal = IR.AESession UVSession
           in case _cffSessionArgument of
                Nothing -> withTable
                Just (FunctionSessionArgument argName index) ->
                  IR.insertFunctionArg argName index sessionArgVal withTable

-- | The custom SQL functions' input "args" field parser
-- > function_name(args: function_args)
customSQLFunctionArgs ::
  (BackendSchema b, MonadSchema n m, MonadTableInfo r m, Has P.MkTypename r) =>
  FunctionInfo b ->
  G.Name ->
  G.Name ->
  m (InputFieldsParser n (IR.FunctionArgsExpTableRow (UnpreparedValue b)))
customSQLFunctionArgs FunctionInfo {..} functionName functionArgsName =
  functionArgs
    ( FTACustomFunction $
        CustomFunctionNames
          { cfnFunctionName = functionName,
            cfnArgsName = functionArgsName
          }
    )
    _fiInputArgs

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
functionArgs ::
  forall b m n r.
  ( BackendSchema b,
    MonadSchema n m,
    MonadTableInfo r m,
    Has P.MkTypename r
  ) =>
  FunctionTrackedAs b ->
  Seq.Seq (FunctionInputArgument b) ->
  m (InputFieldsParser n (IR.FunctionArgsExpTableRow (UnpreparedValue b)))
functionArgs functionTrackedAs (toList -> inputArgs) = do
  -- First, we iterate through the original sql arguments in order, to find the
  -- corresponding graphql names. At the same time, we create the input field
  -- parsers, in three groups: session argument, optional arguments, and
  -- mandatory arguments. Optional arguments have a default value, mandatory
  -- arguments don't.
  let (names, session, optional, mandatory) = mconcat $ snd $ mapAccumL splitArguments 1 inputArgs
      defaultArguments = IR.FunctionArgsExp (snd <$> session) Map.empty

  if
      | length session > 1 ->
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
        objectName <-
          P.mkTypename
            =<< case functionTrackedAs of
              FTAComputedField computedFieldName sourceName tableName -> do
                tableInfo <- askTableInfo sourceName tableName
                computedFieldGQLName <- textToName $ computedFieldNameToText computedFieldName
                tableGQLName <- getTableGQLName @b tableInfo
                pure $ computedFieldGQLName <> $$(G.litName "_") <> tableGQLName <> $$(G.litName "_args")
              FTACustomFunction (CustomFunctionNames {cfnArgsName}) ->
                pure cfnArgsName
        let fieldName = $$(G.litName "args")
            fieldDesc =
              case functionTrackedAs of
                FTAComputedField computedFieldName _sourceName tableName ->
                  G.Description $
                    "input parameters for computed field "
                      <> computedFieldName <<> " defined on table " <>> tableName
                FTACustomFunction (CustomFunctionNames {cfnFunctionName}) ->
                  G.Description $ "input parameters for function " <>> cfnFunctionName
            objectParser =
              P.object objectName Nothing (sequenceA argumentParsers) `P.bind` \arguments -> do
                -- After successfully parsing, we create a dictionary of the parsed fields
                -- and we re-iterate through the original list of sql arguments, now with
                -- the knowledge of their graphql name.
                let foundArguments = Map.fromList $ catMaybes arguments <> session
                    argsWithNames = zip names inputArgs

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
    sessionPlaceholder :: IR.ArgumentExp (UnpreparedValue b)
    sessionPlaceholder = IR.AEInput P.UVSession

    splitArguments ::
      Int ->
      FunctionInputArgument b ->
      ( Int,
        ( [Text], -- graphql names, in order
          [(Text, IR.ArgumentExp (UnpreparedValue b))], -- session argument
          [m (InputFieldsParser n (Maybe (Text, IR.ArgumentExp (UnpreparedValue b))))], -- optional argument
          [m (InputFieldsParser n (Maybe (Text, IR.ArgumentExp (UnpreparedValue b))))] -- mandatory argument
        )
      )
    splitArguments positionalIndex (IASessionVariables name) =
      let argName = getFuncArgNameTxt name
       in (positionalIndex, ([argName], [(argName, sessionPlaceholder)], [], []))
    splitArguments positionalIndex (IAUserProvided arg) =
      let (argName, newIndex) = case faName arg of
            Nothing -> ("arg_" <> tshow positionalIndex, positionalIndex + 1)
            Just name -> (getFuncArgNameTxt name, positionalIndex)
       in if unHasDefault $ faHasDefault arg
            then (newIndex, ([argName], [], [parseArgument arg argName], []))
            else (newIndex, ([argName], [], [], [parseArgument arg argName]))

    parseArgument :: FunctionArg b -> Text -> m (InputFieldsParser n (Maybe (Text, IR.ArgumentExp (UnpreparedValue b))))
    parseArgument arg name = do
      typedParser <- columnParser (ColumnScalar $ functionArgScalarType @b $ faType arg) (G.Nullability True)
      fieldName <- textToName name

      -- Since all postgres function arguments are nullable, we define the
      -- GraphQL fields in nullable types. As explained in Note [When are fields
      -- optional?], this implies that they can be omitted. For backwards
      -- compatibility reasons, and also to avoid surprises, we prefer to reject
      -- the query if a mandatory argument is missing rather than filling the
      -- blanks for the user.
      --
      -- As explained in Note [The value of omitted fields], we can still reject
      -- queries when such nullable fields are omitted, and accept them when an
      -- explicit value of `null` is used, as long as we don't set a default
      -- value, not even `null`.
      let argParser = P.fieldOptional fieldName Nothing typedParser
      pure $ argParser `mapField` ((name,) . IR.AEInput . mkParameter)

    namedArgument ::
      HashMap Text (IR.ArgumentExp (UnpreparedValue b)) ->
      (Text, FunctionInputArgument b) ->
      n (Maybe (Text, IR.ArgumentExp (UnpreparedValue b)))
    namedArgument dictionary (name, inputArgument) = case inputArgument of
      IASessionVariables _ -> pure $ Just (name, sessionPlaceholder)
      IAUserProvided arg -> case Map.lookup name dictionary of
        Just parsedValue -> case faName arg of
          Just _ -> pure $ Just (name, parsedValue)
          Nothing -> parseErrorWith NotSupported "Only last set of positional arguments can be omitted"
        Nothing ->
          whenMaybe (not $ unHasDefault $ faHasDefault arg) $
            parseErrorWith NotSupported "Non default arguments cannot be omitted"

tablePermissionsInfo :: Backend b => SelPermInfo b -> TablePerms b
tablePermissionsInfo selectPermissions =
  IR.TablePerm
    { IR._tpFilter = fmap partialSQLExpToUnpreparedValue <$> spiFilter selectPermissions,
      IR._tpLimit = spiLimit selectPermissions
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

data V1NodeId = V1NodeId
  { _nidTable :: !(TableName ('Postgres 'Vanilla)),
    _nidColumns :: !(NESeq.NESeq J.Value)
  }
  deriving (Show, Eq)

-- | The Relay 'Node' inteface's 'id' field value.
-- See Note [Relay Node id].
data NodeId
  = NodeIdV1 !V1NodeId
  deriving (Show, Eq)

instance J.FromJSON NodeId where
  parseJSON v = do
    valueList <- J.parseJSON v
    case valueList of
      [] -> fail "unexpected GUID format, found empty list"
      J.Number 1 : rest -> NodeIdV1 <$> parseNodeIdV1 rest
      J.Number n : _ -> fail $ "unsupported GUID version: " <> show n
      _ -> fail "unexpected GUID format, needs to start with a version number"
    where
      parseNodeIdV1 (schemaValue : (nameValue : (firstColumn : remainingColumns))) =
        V1NodeId
          <$> (PG.QualifiedObject <$> J.parseJSON schemaValue <*> J.parseJSON nameValue)
          <*> pure (firstColumn NESeq.:<|| Seq.fromList remainingColumns)
      parseNodeIdV1 _ = fail "GUID version 1: expecting schema name, table name and at least one column value"

throwInvalidNodeId :: MonadParse n => Text -> n a
throwInvalidNodeId t = parseError $ "the node id is invalid: " <> t

-- | The 'node' root field of a Relay request.
nodePG ::
  forall m n r.
  MonadBuildSchema ('Postgres 'Vanilla) r m n =>
  m
    ( P.Parser
        'Output
        n
        ( HashMap
            (TableName ('Postgres 'Vanilla))
            ( SourceName,
              SourceConfig ('Postgres 'Vanilla),
              SelPermInfo ('Postgres 'Vanilla),
              PrimaryKeyColumns ('Postgres 'Vanilla),
              AnnotatedFields ('Postgres 'Vanilla)
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
        tableName <- maybe [] (Map.keys . takeValidTables) $ unsafeSourceTables @('Postgres 'Vanilla) sourceInfo
        sourceConfig <- maybeToList $ unsafeSourceConfiguration @('Postgres 'Vanilla) sourceInfo
        pure (tableName, (source, sourceConfig, AB.runBackend sourceInfo _siCustomization))
  tables <-
    Map.mapMaybe id <$> flip Map.traverseWithKey allTables \table (source, sourceConfig, sourceCustomization) -> runMaybeT do
      tableInfo <- lift $ askTableInfo source table
      tablePkeyColumns <- hoistMaybe $ tableInfo ^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns
      selectPermissions <- MaybeT $ tableSelectPermissions tableInfo
      annotatedFieldsParser <-
        lift $
          P.withTypenameCustomization (mkCustomizedTypename $ _scTypeNames sourceCustomization) $
            tableSelectionSet source tableInfo selectPermissions
      pure $ (source,sourceConfig,selectPermissions,tablePkeyColumns,) <$> annotatedFieldsParser
  pure $
    P.selectionSetInterface
      $$(G.litName "Node")
      (Just nodeInterfaceDescription)
      [idField]
      tables

nodeField ::
  forall m n r.
  MonadBuildSchema ('Postgres 'Vanilla) r m n =>
  m (P.FieldParser n (IR.QueryRootField UnpreparedValue))
nodeField = do
  let idDescription = G.Description "A globally unique id"
      idArgument = P.field $$(G.litName "id") (Just idDescription) P.identifier
  stringifyNum <- asks $ qcStringifyNum . getter
  nodeObject <- node
  return $
    P.subselection $$(G.litName "node") Nothing idArgument nodeObject
      `P.bindField` \(ident, parseds) -> do
        NodeIdV1 (V1NodeId table columnValues) <- parseNodeId ident
        (source, sourceConfig, perms, pkeyColumns, fields) <-
          onNothing (Map.lookup table parseds) $
            withArgsPath $ throwInvalidNodeId $ "the table " <>> ident
        whereExp <- buildNodeIdBoolExp columnValues pkeyColumns
        return $
          IR.RFDB source $
            AB.mkAnyBackend $
              IR.SourceConfigWith sourceConfig Nothing $
                IR.QDBR $
                  IR.QDBSingleRow $
                    IR.AnnSelectG
                      { IR._asnFields = fields,
                        IR._asnFrom = IR.FromTable table,
                        IR._asnPerm = tablePermissionsInfo perms,
                        IR._asnArgs =
                          IR.SelectArgs
                            { IR._saWhere = Just whereExp,
                              IR._saOrderBy = Nothing,
                              IR._saLimit = Nothing,
                              IR._saOffset = Nothing,
                              IR._saDistinct = Nothing
                            },
                        IR._asnStrfyNum = stringifyNum
                      }
  where
    parseNodeId :: Text -> n NodeId
    parseNodeId =
      either (withArgsPath . throwInvalidNodeId . T.pack) pure . J.eitherDecode . base64Decode
    withArgsPath = withPath (++ [Key "args", Key "id"])

    buildNodeIdBoolExp ::
      NESeq.NESeq J.Value ->
      NESeq.NESeq (ColumnInfo ('Postgres 'Vanilla)) ->
      n (IR.AnnBoolExp ('Postgres 'Vanilla) (UnpreparedValue ('Postgres 'Vanilla)))
    buildNodeIdBoolExp columnValues pkeyColumns = do
      let firstPkColumn NESeq.:<|| remainingPkColumns = pkeyColumns
          firstColumnValue NESeq.:<|| remainingColumns = columnValues
          (nonAlignedPkColumns, nonAlignedColumnValues, alignedTuples) =
            partitionThese $ toList $ align remainingPkColumns remainingColumns

      unless (null nonAlignedPkColumns) $
        throwInvalidNodeId $
          "primary key columns " <> dquoteList (map ciColumn nonAlignedPkColumns) <> " are missing"

      unless (null nonAlignedColumnValues) $
        throwInvalidNodeId $
          "unexpected column values " <> J.encodeToStrictText nonAlignedColumnValues

      let allTuples = (firstPkColumn, firstColumnValue) : alignedTuples

      flip onLeft (parseErrorWith ParseFailed . qeError) $
        runExcept $
          fmap IR.BoolAnd $
            for allTuples $ \(columnInfo, columnValue) -> do
              let modifyErrFn t =
                    "value of column " <> ciColumn columnInfo
                      <<> " in node id: " <> t
                  pgColumnType = ciType columnInfo
              pgValue <- modifyErr modifyErrFn $ parseScalarValueColumnType pgColumnType columnValue
              let unpreparedValue = UVParameter Nothing $ ColumnValue pgColumnType pgValue
              pure $ IR.BoolFld $ IR.AVColumn columnInfo [IR.AEQ True unpreparedValue]
