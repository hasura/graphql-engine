{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate table selection schema both for ordinary Hasura-type and
-- relay-type queries.  All schema with "relay" or "connection" in the name is
-- used exclusively by relay.
module Hasura.GraphQL.Schema.Select
  ( selectTableByPk,
    selectTableConnection,
    defaultSelectTable,
    defaultSelectTableAggregate,
    defaultTableArgs,
    defaultTableSelectionSet,
    tableAggregationFields,
    tableConnectionArgs,
    tableConnectionSelectionSet,
    tableWhereArg,
    tableOrderByArg,
    tableDistinctArg,
    tableLimitArg,
    tableOffsetArg,
    tablePermissionsInfo,
    tableSelectionList,
  )
where

import Control.Lens hiding (index)
import Data.Aeson qualified as J
import Data.Aeson.Internal qualified as J
import Data.Aeson.Key qualified as K
import Data.ByteString.Lazy qualified as BL
import Data.Has
import Data.HashMap.Strict.Extended qualified as Map
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Internal.Parser qualified as P
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Options (OptimizePermissionFilters (..))
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Schema.OrderBy
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
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
defaultSelectTable ::
  forall b r m n.
  (MonadBuildSchema b r m n, BackendTableSelectSchema b) =>
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  m (Maybe (FieldParser n (SelectExp b)))
defaultSelectTable sourceInfo tableInfo fieldName description = runMaybeT do
  tCase <- asks getter
  roleName <- retrieve scRole
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  selectionSetParser <- MaybeT $ tableSelectionList sourceInfo tableInfo
  lift $ P.memoizeOn 'defaultSelectTable (_siName sourceInfo, tableName, fieldName) do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableArgsParser <- tableArguments sourceInfo tableInfo
    pure $
      P.setFieldParserOrigin (MOSource (_siName sourceInfo)) $
        P.subselection fieldName description tableArgsParser selectionSetParser
          <&> \(args, fields) ->
            IR.AnnSelectG
              { IR._asnFields = fields,
                IR._asnFrom = IR.FromTable tableName,
                IR._asnPerm = tablePermissionsInfo selectPermissions,
                IR._asnArgs = args,
                IR._asnStrfyNum = stringifyNumbers,
                IR._asnNamingConvention = Just tCase
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
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b
  ) =>
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | primary key columns
  PrimaryKeyColumns b ->
  m (Maybe (FieldParser n (ConnectionSelectExp b)))
selectTableConnection sourceInfo tableInfo fieldName description pkeyColumns = runMaybeT do
  tCase <- asks getter
  roleName <- retrieve scRole
  xRelayInfo <- hoistMaybe $ relayExtension @b
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  selectionSetParser <- fmap P.nonNullableParser <$> MaybeT $ tableConnectionSelectionSet sourceInfo tableInfo
  lift $ P.memoizeOn 'selectTableConnection (_siName sourceInfo, tableName, fieldName) do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    selectArgsParser <- tableConnectionArgs pkeyColumns sourceInfo tableInfo
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
                    IR._asnStrfyNum = stringifyNumbers,
                    IR._asnNamingConvention = Just tCase
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
  (MonadBuildSchema b r m n, BackendTableSelectSchema b) =>
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  m (Maybe (FieldParser n (SelectExp b)))
selectTableByPk sourceInfo tableInfo fieldName description = runMaybeT do
  tCase <- asks getter
  roleName <- retrieve scRole
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  primaryKeys <- hoistMaybe $ fmap _pkColumns . _tciPrimaryKey . _tiCoreInfo $ tableInfo
  selectionSetParser <- MaybeT $ tableSelectionSet sourceInfo tableInfo
  guard $ all (\c -> ciColumn c `Map.member` spiCols selectPermissions) primaryKeys
  lift $ P.memoizeOn 'selectTableByPk (_siName sourceInfo, tableName, fieldName) do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    argsParser <-
      sequenceA <$> for primaryKeys \columnInfo -> do
        field <- columnParser (ciType columnInfo) (G.Nullability $ ciIsNullable columnInfo)
        pure $
          BoolField . AVColumn columnInfo . pure . AEQ True . IR.mkParameter
            <$> P.field (ciName columnInfo) (ciDescription columnInfo) field
    pure $
      P.setFieldParserOrigin (MOSource (_siName sourceInfo)) $
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
                    IR._asnStrfyNum = stringifyNumbers,
                    IR._asnNamingConvention = Just tCase
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
defaultSelectTableAggregate ::
  forall b r m n.
  (MonadBuildSchema b r m n, BackendTableSelectSchema b) =>
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  m (Maybe (FieldParser n (AggSelectExp b)))
defaultSelectTableAggregate sourceInfo tableInfo fieldName description = runMaybeT $ do
  tCase <- asks getter
  roleName <- retrieve scRole
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  guard $ spiAllowAgg selectPermissions
  xNodesAgg <- hoistMaybe $ nodesAggExtension @b
  nodesParser <- MaybeT $ tableSelectionList sourceInfo tableInfo
  lift $ P.memoizeOn 'defaultSelectTableAggregate (_siName sourceInfo, tableName, fieldName) do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableGQLName <- getTableGQLName tableInfo
    tableArgsParser <- tableArguments sourceInfo tableInfo
    aggregateParser <- tableAggregationFields sourceInfo tableInfo
    selectionName <- mkTypename $ tableGQLName <> Name.__aggregate
    let aggregationParser =
          P.nonNullableParser $
            parsedSelectionsToFields IR.TAFExp
              <$> P.selectionSet
                selectionName
                (Just $ G.Description $ "aggregated selection of " <>> tableName)
                [ IR.TAFNodes xNodesAgg <$> P.subselection_ Name._nodes Nothing nodesParser,
                  IR.TAFAgg <$> P.subselection_ Name._aggregate Nothing aggregateParser
                ]
    pure $
      P.setFieldParserOrigin (MOSource (_siName sourceInfo)) $
        P.subselection fieldName description tableArgsParser aggregationParser
          <&> \(args, fields) ->
            IR.AnnSelectG
              { IR._asnFields = fields,
                IR._asnFrom = IR.FromTable tableName,
                IR._asnPerm = tablePermissionsInfo selectPermissions,
                IR._asnArgs = args,
                IR._asnStrfyNum = stringifyNumbers,
                IR._asnNamingConvention = Just tCase
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
defaultTableSelectionSet ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b,
    Eq (AnnBoolExp b (IR.UnpreparedValue b))
  ) =>
  SourceInfo b ->
  TableInfo b ->
  m (Maybe (Parser 'Output n (AnnotatedFields b)))
defaultTableSelectionSet sourceInfo tableInfo = runMaybeT do
  roleName <- retrieve scRole
  _selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  schemaKind <- lift $ retrieve scSchemaKind
  -- If this check fails, it means we're attempting to build a Relay schema, but
  -- the current backend b does't support Relay; rather than returning an
  -- incomplete selection set, we fail early and return 'Nothing'. This check
  -- must happen first, since we can't memoize a @Maybe Parser@.
  guard $ isHasuraSchema schemaKind || isJust (relayExtension @b)
  lift $ P.memoizeOn 'defaultTableSelectionSet (sourceName, tableName) do
    tableGQLName <- getTableGQLName tableInfo
    objectTypename <- mkTypename tableGQLName
    let xRelay = relayExtension @b
        tableFields = Map.elems $ _tciFieldInfoMap tableCoreInfo
        tablePkeyColumns = _pkColumns <$> _tciPrimaryKey tableCoreInfo
        pkFields = concatMap toList tablePkeyColumns
        pkFieldDirective = T.intercalate " " $ map (G.unName . ciName) pkFields
        -- Adding `@key` directives to type for apollo federation. An example
        -- of type with key directive:
        --  type Product @key(fields: "upc sku"){
        --    upc: UPC!
        --    sku: SKU!
        --    name: String
        --  }
        pkDirectives =
          if isApolloFedV1enabled (_tciApolloFederationConfig tableCoreInfo) && (not . null) pkFields
            then [(G.Directive Name._key . Map.singleton Name._fields . G.VString) pkFieldDirective]
            else mempty
        description = G.Description . PG.getPGDescription <$> _tciDescription tableCoreInfo
    fieldParsers <-
      concat
        <$> for
          tableFields
          (fieldSelection sourceInfo tableName tableInfo)

    -- We don't check *here* that the subselection set is non-empty,
    -- even though the GraphQL specification requires that it is (see
    -- Note [Selectability of tables]). However, the GraphQL parser
    -- enforces that a selection set, if present, is non-empty; and our
    -- parser later verifies that a selection set is present if
    -- required, meaning that not having this check here does not allow
    -- for the construction of invalid queries.

    case (schemaKind, tablePkeyColumns, xRelay) of
      -- A relay table
      (RelaySchema nodeBuilder, Just pkeyColumns, Just xRelayInfo) -> do
        let nodeIdFieldParser =
              P.selection_ Name._id Nothing P.identifier $> IR.AFNodeId xRelayInfo sourceName tableName pkeyColumns
            allFieldParsers = fieldParsers <> [nodeIdFieldParser]
        nodeInterface <- runNodeBuilder nodeBuilder
        pure $
          selectionSetObjectWithDirective objectTypename description allFieldParsers [nodeInterface] pkDirectives
            <&> parsedSelectionsToFields IR.AFExpression
      _ ->
        pure $
          selectionSetObjectWithDirective objectTypename description fieldParsers [] pkDirectives
            <&> parsedSelectionsToFields IR.AFExpression
  where
    sourceName = _siName sourceInfo
    tableName = tableInfoName tableInfo
    tableCoreInfo = _tiCoreInfo tableInfo
    selectionSetObjectWithDirective name description parsers implementsInterfaces directives =
      P.setParserDirectives directives $
        P.selectionSetObject name description parsers implementsInterfaces

-- | List of table fields object.
-- Just a @'nonNullableObjectList' wrapper over @'tableSelectionSet'.
-- > table_name: [table!]!
tableSelectionList ::
  (MonadBuildSchemaBase r m n, BackendTableSelectSchema b) =>
  SourceInfo b ->
  TableInfo b ->
  m (Maybe (Parser 'Output n (AnnotatedFields b)))
tableSelectionList sourceInfo tableInfo =
  fmap nonNullableObjectList <$> tableSelectionSet sourceInfo tableInfo

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
  (MonadBuildSchema b r m n, BackendTableSelectSchema b) =>
  SourceInfo b ->
  TableInfo b ->
  m (Maybe (Parser 'Output n (ConnectionFields b)))
tableConnectionSelectionSet sourceInfo tableInfo = runMaybeT do
  roleName <- retrieve scRole
  tableGQLName <- lift $ getTableGQLName tableInfo
  void $ hoistMaybe $ tableSelectPermissions roleName tableInfo
  edgesParser <- MaybeT $ tableEdgesSelectionSet tableGQLName
  lift $ P.memoizeOn 'tableConnectionSelectionSet (_siName sourceInfo, tableName) do
    connectionTypeName <- mkTypename $ tableGQLName <> Name._Connection
    let pageInfo =
          P.subselection_
            Name._pageInfo
            Nothing
            pageInfoSelectionSet
            <&> IR.ConnectionPageInfo
        edges =
          P.subselection_
            Name._edges
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
              Name._startCursor
              Nothing
              P.string
              $> IR.PageInfoStartCursor
          endCursorField =
            P.selection_
              Name._endCursor
              Nothing
              P.string
              $> IR.PageInfoEndCursor
          hasNextPageField =
            P.selection_
              Name._hasNextPage
              Nothing
              P.boolean
              $> IR.PageInfoHasNextPage
          hasPreviousPageField =
            P.selection_
              Name._hasPreviousPage
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
            P.selectionSet Name._PageInfo Nothing allFields
              <&> parsedSelectionsToFields IR.PageInfoTypename

    tableEdgesSelectionSet ::
      G.Name -> m (Maybe (Parser 'Output n (EdgeFields b)))
    tableEdgesSelectionSet tableGQLName = runMaybeT do
      edgeNodeParser <- MaybeT $ fmap P.nonNullableParser <$> tableSelectionSet sourceInfo tableInfo
      edgesType <- lift $ mkTypename $ tableGQLName <> Name._Edge
      let cursor =
            P.selection_
              Name._cursor
              Nothing
              P.string
              $> IR.EdgeCursor
          edgeNode =
            P.subselection_
              Name._node
              Nothing
              edgeNodeParser
              <&> IR.EdgeNode
      pure $
        nonNullableObjectList $
          P.selectionSet edgesType Nothing [cursor, edgeNode]
            <&> parsedSelectionsToFields IR.EdgeTypename

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
  SourceInfo b ->
  TableInfo b ->
  m (InputFieldsParser n (SelectArgs b))
defaultTableArgs sourceInfo tableInfo = do
  whereParser <- tableWhereArg sourceInfo tableInfo
  orderByParser <- tableOrderByArg sourceInfo tableInfo
  distinctParser <- tableDistinctArg sourceInfo tableInfo
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
  SourceInfo b ->
  TableInfo b ->
  m (InputFieldsParser n (Maybe (IR.AnnBoolExp b (IR.UnpreparedValue b))))
tableWhereArg sourceInfo tableInfo = do
  boolExpParser <- boolExp sourceInfo tableInfo
  pure $
    fmap join $
      P.fieldOptional whereName whereDesc $
        P.nullable boolExpParser
  where
    whereName = Name._where
    whereDesc = Just $ G.Description "filter the rows returned"

-- | Argument to sort rows returned from table selection
-- > order_by: [table_order_by!]
tableOrderByArg ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableInfo b ->
  m (InputFieldsParser n (Maybe (NonEmpty (IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)))))
tableOrderByArg sourceInfo tableInfo = do
  tCase <- asks getter
  orderByParser <- orderByExp sourceInfo tableInfo
  let orderByName = applyFieldNameCaseCust tCase Name._order_by
      orderByDesc = Just $ G.Description "sort the rows by one or more columns"
  pure $ do
    maybeOrderByExps <-
      fmap join $
        P.fieldOptional orderByName orderByDesc $ P.nullable $ P.list orderByParser
    pure $ maybeOrderByExps >>= NE.nonEmpty . concat

-- | Argument to distinct select on columns returned from table selection
-- > distinct_on: [table_select_column!]
tableDistinctArg ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableInfo b ->
  m (InputFieldsParser n (Maybe (NonEmpty (Column b))))
tableDistinctArg sourceInfo tableInfo = do
  tCase <- asks getter
  columnsEnum <- tableSelectColumnsEnum sourceInfo tableInfo
  let distinctOnName = applyFieldNameCaseCust tCase Name._distinct_on
      distinctOnDesc = Just $ G.Description "distinct select on columns"
  pure do
    maybeDistinctOnColumns <-
      join . join
        <$> for
          columnsEnum
          (P.fieldOptional distinctOnName distinctOnDesc . P.nullable . P.list)
    pure $ maybeDistinctOnColumns >>= NE.nonEmpty

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
    limitName = Name._limit
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
    offsetName = Name._offset
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
  SourceInfo b ->
  TableInfo b ->
  m
    ( InputFieldsParser
        n
        ( SelectArgs b,
          Maybe (NonEmpty (IR.ConnectionSplit b (IR.UnpreparedValue b))),
          Maybe IR.ConnectionSlice
        )
    )
tableConnectionArgs pkeyColumns sourceInfo tableInfo = do
  whereParser <- tableWhereArg sourceInfo tableInfo
  orderByParser <- fmap (fmap appendPrimaryKeyOrderBy) <$> tableOrderByArg sourceInfo tableInfo
  distinctParser <- tableDistinctArg sourceInfo tableInfo
  let maybeFirst = fmap join $ P.fieldOptional Name._first Nothing $ P.nullable P.nonNegativeInt
      maybeLast = fmap join $ P.fieldOptional Name._last Nothing $ P.nullable P.nonNegativeInt
      maybeAfter = fmap join $ P.fieldOptional Name._after Nothing $ P.nullable base64Text
      maybeBefore = fmap join $ P.fieldOptional Name._before Nothing $ P.nullable base64Text
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
      Maybe (NonEmpty (IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b))) ->
      IR.ConnectionSplitKind ->
      BL.ByteString ->
      n (NonEmpty (IR.ConnectionSplit b (IR.UnpreparedValue b)))
    parseConnectionSplit maybeOrderBys splitKind cursorSplit = do
      cursorValue <- J.eitherDecode cursorSplit `onLeft` const throwInvalidCursor
      case maybeOrderBys of
        Nothing -> forM (nonEmptySeqToNonEmptyList pkeyColumns) $
          \columnInfo -> do
            let columnJsonPath = [J.Key $ K.fromText $ toTxt $ ciColumn columnInfo]
                columnType = ciType columnInfo
            columnValue <-
              iResultToMaybe (executeJSONPath columnJsonPath cursorValue)
                `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parseScalarValueColumnType columnType columnValue
            let unresolvedValue = IR.UVParameter Nothing $ ColumnValue columnType pgValue
            pure $
              IR.ConnectionSplit splitKind unresolvedValue $
                IR.OrderByItemG Nothing (IR.AOCColumn columnInfo) Nothing
        Just orderBys ->
          forM orderBys $ \orderBy -> do
            let IR.OrderByItemG orderType annObCol nullsOrder = orderBy
                columnType = getOrderByColumnType annObCol
            orderByItemValue <-
              iResultToMaybe (executeJSONPath (map (J.Key . K.fromText) (getPathFromOrderBy annObCol)) cursorValue)
                `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parseScalarValueColumnType columnType orderByItemValue
            let unresolvedValue = IR.UVParameter Nothing $ ColumnValue columnType pgValue
            pure $
              IR.ConnectionSplit splitKind unresolvedValue $
                IR.OrderByItemG orderType annObCol nullsOrder
      where
        throwInvalidCursor = parseError "the \"after\" or \"before\" cursor is invalid"
        liftQErr = either (parseError . toErrorMessage . qeError) pure . runExcept

        mkAggregateOrderByPath = \case
          IR.AAOCount -> ["count"]
          IR.AAOOp t col -> [t, toTxt $ ciColumn col]

        getPathFromOrderBy = \case
          IR.AOCColumn columnInfo ->
            let pathElement = toTxt $ ciColumn columnInfo
             in [pathElement]
          IR.AOCObjectRelation relInfo _ obCol ->
            let pathElement = relNameToTxt $ riName relInfo
             in pathElement : getPathFromOrderBy obCol
          IR.AOCArrayAggregation relInfo _ aggOb ->
            let fieldName = relNameToTxt (riName relInfo) <> "_aggregate"
             in fieldName : mkAggregateOrderByPath aggOb
          IR.AOCComputedField cfob ->
            let fieldNameText = computedFieldNameToText $ IR._cfobName cfob
             in case IR._cfobOrderByElement cfob of
                  IR.CFOBEScalar _ -> [fieldNameText]
                  IR.CFOBETableAggregation _ _ aggOb ->
                    (fieldNameText <> "_aggregate") : mkAggregateOrderByPath aggOb

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
  SourceInfo b ->
  TableInfo b ->
  m (Parser 'Output n (IR.AggregateFields b))
tableAggregationFields sourceInfo tableInfo =
  P.memoizeOn 'tableAggregationFields (_siName sourceInfo, tableInfoName tableInfo) do
    tableGQLName <- getTableGQLName tableInfo
    tCase <- asks getter
    allColumns <- tableSelectColumns sourceInfo tableInfo
    let numericColumns = onlyNumCols allColumns
        comparableColumns = onlyComparableCols allColumns
        description = G.Description $ "aggregate fields of " <>> tableInfoName tableInfo
    selectName <- mkTypename $ tableGQLName <> Name.__aggregate_fields
    count <- countField
    makeTypename <- asks getter
    numericAndComparable <-
      fmap concat $
        sequenceA $
          catMaybes
            [ -- operators on numeric columns
              if null numericColumns
                then Nothing
                else Just $
                  for numericAggOperators $ \operator -> do
                    let fieldNameCase = applyFieldNameCaseCust tCase operator
                    numFields <- mkNumericAggFields operator numericColumns
                    pure $ parseAggOperator makeTypename operator fieldNameCase tableGQLName numFields,
              -- operators on comparable columns
              if null comparableColumns
                then Nothing
                else Just $ do
                  comparableFields <- traverse mkColumnAggField comparableColumns
                  pure $
                    comparisonAggOperators & map \operator ->
                      let fieldNameCase = applyFieldNameCaseCust tCase operator
                       in parseAggOperator makeTypename operator fieldNameCase tableGQLName comparableFields
            ]
    let aggregateFields = count : numericAndComparable
    pure $
      P.selectionSet selectName (Just description) aggregateFields
        <&> parsedSelectionsToFields IR.AFExp
  where
    mkNumericAggFields :: G.Name -> [ColumnInfo b] -> m [FieldParser n (IR.ColFld b)]
    mkNumericAggFields name
      | name == Name._sum = traverse mkColumnAggField
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
      columnsEnum <- tableSelectColumnsEnum sourceInfo tableInfo
      let distinctName = Name._distinct
          args = do
            distinct <- P.fieldOptional distinctName Nothing P.boolean
            mkCountType <- countTypeInput @b columnsEnum
            pure $
              mkCountType $
                maybe
                  IR.SelectCountNonDistinct -- If "distinct" is "null" or absent, we default to @'SelectCountNonDistinct'
                  (bool IR.SelectCountNonDistinct IR.SelectCountDistinct)
                  distinct

      pure $ IR.AFCount <$> P.selection Name._count Nothing args P.int

    parseAggOperator ::
      MkTypename ->
      G.Name ->
      G.Name ->
      G.Name ->
      [FieldParser n (IR.ColFld b)] ->
      FieldParser n (IR.AggregateField b)
    parseAggOperator makeTypename operator fieldName tableGQLName columns =
      let opText = G.unName operator
          setName = runMkTypename makeTypename $ tableGQLName <> Name.__ <> operator <> Name.__fields
          setDesc = Just $ G.Description $ "aggregate " <> opText <> " on columns"
          subselectionParser =
            P.selectionSet setName setDesc columns
              <&> parsedSelectionsToFields IR.CFExp
       in P.subselection_ fieldName Nothing subselectionParser
            <&> IR.AFOp . IR.AggregateOp opText

-- | An individual field of a table
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b,
    Eq (AnnBoolExp b (IR.UnpreparedValue b))
  ) =>
  SourceInfo b ->
  TableName b ->
  TableInfo b ->
  FieldInfo b ->
  m [FieldParser n (AnnotatedField b)]
fieldSelection sourceInfo table tableInfo = \case
  FIColumn columnInfo ->
    maybeToList <$> runMaybeT do
      roleName <- retrieve scRole
      schemaKind <- retrieve scSchemaKind
      let fieldName = ciName columnInfo
      -- If the field name is 'id' and we're building a schema for the Relay
      -- API, Node's id field will take precedence; consequently we simply
      -- ignore the original.
      guard $ isHasuraSchema schemaKind || fieldName /= Name._id
      let columnName = ciColumn columnInfo
      selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
      guard $ columnName `Map.member` spiCols selectPermissions
      let caseBoolExp = join $ Map.lookup columnName (spiCols selectPermissions)
          caseBoolExpUnpreparedValue =
            (fmap . fmap) partialSQLExpToUnpreparedValue <$> caseBoolExp
          pathArg = scalarSelectionArgumentsParser $ ciType columnInfo
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
    concat . maybeToList <$> relationshipField sourceInfo table relationshipInfo
  FIComputedField computedFieldInfo ->
    maybeToList <$> computedField sourceInfo computedFieldInfo table tableInfo
  FIRemoteRelationship remoteFieldInfo -> do
    schemaKind <- retrieve scSchemaKind
    case (schemaKind, _rfiRHS remoteFieldInfo) of
      (RelaySchema _, RFISchema _) ->
        -- Remote schemas aren't currently supported in Relay, and we therefore
        -- cannot include remote relationships to them while building a
        -- Relay-specific schema: attempting to do so would raise an error, as
        -- 'remoteRelationshipField' would attempt to look into the
        -- 'SchemaOptions' for information about the targeted schema.
        pure []
      _ -> do
        RemoteRelationshipParserBuilder remoteRelationshipField <- retrieve scRemoteRelationshipParserBuilder
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
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b,
    Eq (AnnBoolExp b (IR.UnpreparedValue b))
  ) =>
  SourceInfo b ->
  TableName b ->
  RelInfo b ->
  m (Maybe [FieldParser n (AnnotatedField b)])
relationshipField sourceInfo table ri = runMaybeT do
  tCase <- asks getter
  roleName <- retrieve scRole
  optimizePermissionFilters <- retrieve Options.soOptimizePermissionFilters
  tableInfo <- lift $ askTableInfo sourceInfo table
  otherTableInfo <- lift $ askTableInfo sourceInfo $ riRTable ri
  tablePerms <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  remotePerms <- hoistMaybe $ tableSelectPermissions roleName otherTableInfo
  relFieldName <- lift $ textToName $ relNameToTxt $ riName ri
  -- START black magic to deduplicate permission checks
  let thisTablePerm = IR._tpFilter $ tablePermissionsInfo tablePerms
      deduplicatePermissions :: AnnBoolExp b (IR.UnpreparedValue b) -> AnnBoolExp b (IR.UnpreparedValue b)
      deduplicatePermissions x =
        case (optimizePermissionFilters, x) of
          (OptimizePermissionFilters, BoolAnd [BoolField (AVRelationship remoteRI remoteTablePerm)]) ->
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
      selectionSetParser <- MaybeT $ tableSelectionSet sourceInfo otherTableInfo
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
      otherTableParser <- MaybeT $ selectTable sourceInfo otherTableInfo relFieldName arrayRelDesc
      let arrayRelField =
            otherTableParser <&> \selectExp ->
              IR.AFArrayRelation $
                IR.ASSimple $
                  IR.AnnRelationSelectG (riName ri) (riMapping ri) $
                    deduplicatePermissions' selectExp
          relAggFieldName = applyFieldNameCaseCust tCase $ relFieldName <> Name.__aggregate
          relAggDesc = Just $ G.Description "An aggregate relationship"
      remoteAggField <- lift $ selectTableAggregate sourceInfo otherTableInfo relAggFieldName relAggDesc
      remoteConnectionField <- runMaybeT $ do
        -- Parse array connection field only for relay schema
        RelaySchema _ <- retrieve scSchemaKind
        _xRelayInfo <- hoistMaybe $ relayExtension @b
        pkeyColumns <-
          MaybeT $
            (^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns)
              <$> pure otherTableInfo
        let relConnectionName = relFieldName <> Name.__connection
            relConnectionDesc = Just $ G.Description "An array relationship connection"
        MaybeT $ lift $ selectTableConnection sourceInfo otherTableInfo relConnectionName relConnectionDesc pkeyColumns
      pure $
        catMaybes
          [ Just arrayRelField,
            fmap (IR.AFArrayRelation . IR.ASAggregate . IR.AnnRelationSelectG (riName ri) (riMapping ri)) <$> remoteAggField,
            fmap (IR.AFArrayRelation . IR.ASConnection . IR.AnnRelationSelectG (riName ri) (riMapping ri)) <$> remoteConnectionField
          ]

tablePermissionsInfo :: Backend b => SelPermInfo b -> TablePerms b
tablePermissionsInfo selectPermissions =
  IR.TablePerm
    { IR._tpFilter = fmap partialSQLExpToUnpreparedValue <$> spiFilter selectPermissions,
      IR._tpLimit = spiLimit selectPermissions
    }
