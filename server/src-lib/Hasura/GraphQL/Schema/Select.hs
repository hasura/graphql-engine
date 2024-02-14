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
    defaultArgsParser,
    tableAggregationFields,
    tableConnectionArgs,
    tableConnectionSelectionSet,
    tableWhereArg,
    tableOrderByArg,
    tableDistinctArg,
    tableLimitArg,
    tableOffsetArg,
    tableSelectionList,
  )
where

import Control.Lens hiding (index)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.Types qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Has
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types qualified as Postgres
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Internal.Parser qualified as IP
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
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
import Hasura.LogicalModel.Cache (LogicalModelCache, LogicalModelInfo (..))
import Hasura.LogicalModel.Types
  ( LogicalModelField (..),
    LogicalModelName (..),
    LogicalModelType (..),
    LogicalModelTypeArray (..),
    LogicalModelTypeReference (..),
    LogicalModelTypeScalar (..),
  )
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Select.Lenses qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Schema.Options (OptimizePermissionFilters (..))
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Utils (executeJSONPath)
import Hasura.Table.Cache
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
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (SelectExp b)))
defaultSelectTable tableInfo fieldName description = runMaybeT do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
  roleName <- retrieve scRole
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  selectionSetParser <- MaybeT $ tableSelectionList tableInfo
  lift $ P.memoizeOn 'defaultSelectTable (sourceName, tableName, fieldName) do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableArgsParser <- tableArguments tableInfo
    pure
      $ P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName))
      $ P.subselection fieldName description tableArgsParser selectionSetParser
      <&> \(args, fields) ->
        IR.AnnSelectG
          { IR._asnFields = fields,
            IR._asnFrom = IR.FromTable tableName,
            IR._asnPerm = tablePermissionsInfo selectPermissions,
            IR._asnArgs = args,
            IR._asnStrfyNum = stringifyNumbers,
            IR._asnNamingConvention = Just tCase
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
selectTableConnection ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b,
    AggregationPredicatesSchema b
  ) =>
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | primary key columns
  PrimaryKeyColumns b ->
  SchemaT r m (Maybe (FieldParser n (ConnectionSelectExp b)))
selectTableConnection tableInfo fieldName description pkeyColumns = runMaybeT do
  sourceInfo :: SourceInfo b <- asks getter
  let tableName = tableInfoName tableInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
  roleName <- retrieve scRole
  xRelayInfo <- hoistMaybe $ relayExtension @b
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  selectionSetParser <- fmap P.nonNullableParser <$> MaybeT $ tableConnectionSelectionSet tableInfo
  lift $ P.memoizeOn 'selectTableConnection (_siName sourceInfo, tableName, fieldName) do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    selectArgsParser <- tableConnectionArgs pkeyColumns tableInfo selectPermissions
    pure
      $ P.subselection fieldName description selectArgsParser selectionSetParser
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
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (SelectExp b)))
selectTableByPk tableInfo fieldName description = runMaybeT do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
  roleName <- retrieve scRole
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  primaryKeys <- hoistMaybe $ fmap _pkColumns . _tciPrimaryKey . _tiCoreInfo $ tableInfo
  selectionSetParser <- MaybeT $ tableSelectionSet tableInfo
  guard $ all (\c -> ciColumn c `HashMap.member` spiCols selectPermissions) primaryKeys
  lift $ P.memoizeOn 'selectTableByPk (sourceName, tableName, fieldName) do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    argsParser <-
      sequenceA <$> for primaryKeys \columnInfo -> do
        let redactionExp = fromMaybe NoRedaction $ getRedactionExprForColumn selectPermissions (ciColumn columnInfo)
        field <- columnParser (ciType columnInfo) (G.Nullability $ ciIsNullable columnInfo)
        pure
          $ BoolField
          . AVColumn columnInfo redactionExp
          . pure
          . AEQ NonNullableComparison
          . IR.mkParameter
          <$> P.field (ciName columnInfo) (ciDescription columnInfo) field
    pure
      $ P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName))
      $ P.subselection fieldName description argsParser selectionSetParser
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

-- | Table aggregation selection
--
-- Parser for an aggregation selection of a table.
-- > table_aggregate(limit: 10) {
-- >   aggregate: table_aggregate_fields
-- >   group_by(...): table_group_by
-- >   nodes: [table!]!
-- > } :: table_aggregate!
--
-- Returns Nothing if there's nothing that can be selected with
-- current permissions.
defaultSelectTableAggregate ::
  forall b r m n.
  (MonadBuildSchema b r m n, BackendTableSelectSchema b) =>
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (AggSelectExp b)))
defaultSelectTableAggregate tableInfo fieldName description = runMaybeT $ do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
  roleName <- retrieve scRole
  selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  guard $ spiAllowAgg selectPermissions
  xNodesAgg <- hoistMaybe $ nodesAggExtension @b
  nodesParser <- MaybeT $ tableSelectionList tableInfo
  lift $ P.memoizeOn 'defaultSelectTableAggregate (sourceName, tableName, fieldName) do
    stringifyNumbers <- retrieve Options.soStringifyNumbers
    tableGQLName <- getTableIdentifierName tableInfo
    tableArgsParser <- tableArguments tableInfo
    aggregateParser <- tableAggregationFields tableInfo
    groupByParser <- groupBy tableInfo
    let aggregateFields =
          [ IR.TAFNodes xNodesAgg <$> P.subselection_ Name._nodes Nothing nodesParser,
            IR.TAFAgg <$> P.subselection_ Name._aggregate Nothing aggregateParser
          ]
            <> (maybeToList groupByParser)
    let selectionName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableAggregateTypeName tableGQLName
        aggregationParser =
          P.nonNullableParser
            $ parsedSelectionsToFields IR.TAFExp
            <$> P.selectionSet
              selectionName
              (Just $ G.Description $ "aggregated selection of " <>> tableName)
              aggregateFields
    pure
      $ P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName))
      $ P.subselection fieldName description tableArgsParser aggregationParser
      <&> \(args, fields) ->
        IR.AnnSelectG
          { IR._asnFields = fields,
            IR._asnFrom = IR.FromTable tableName,
            IR._asnPerm = tablePermissionsInfo selectPermissions,
            IR._asnArgs = args,
            IR._asnStrfyNum = stringifyNumbers,
            IR._asnNamingConvention = Just tCase
          }

-- | Table aggregation group by selection
--
-- Parser for grouping over a table's rows:
-- > group_by(
-- >   keys: [table_group_by_key!]!
-- > ) {
-- >   group_key: table_group_by_key_fields
-- >   aggregate: table_aggregate_fields
-- > }
--
-- Returns 'Nothing' if the feature is disabled, the backend does not support group by
-- or if no columns are selectable on the table
groupBy ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Maybe (FieldParser n (IR.TableAggregateFieldG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
groupBy tableInfo = runMaybeT $ do
  xGroupBy <- guardGroupByFeatureSupported
  groupByInputFieldsParser <- groupByInputFields tableInfo
  namingCase <- retrieve (_rscNamingConvention . _siCustomization @b)
  let groupByFieldName = applyFieldNameCaseCust namingCase Name._group_by

  selectionSetParser <- groupBySelectionSet tableInfo
  P.subselection groupByFieldName (Just "Groups the table by the specified keys") groupByInputFieldsParser selectionSetParser
    <&> (\(keys, fields) -> IR.TAFGroupBy xGroupBy (IR.GroupByG keys fields))
    & pure
  where
    guardGroupByFeatureSupported :: MaybeT (SchemaT r m) (XGroupBy b)
    guardGroupByFeatureSupported = do
      includeGroupByAggregateFields <- retrieve Options.soIncludeGroupByAggregateFields
      case includeGroupByAggregateFields of
        Options.IncludeGroupByAggregateFields -> hoistMaybe (groupByExtension @b)
        Options.ExcludeGroupByAggregateFields -> hoistMaybe Nothing

-- | Parser for the input fields of the group_by field.
--
-- These are:
-- > keys: [table_group_by_key!]!
--
-- Fails in 'MaybeT' if there are no selectable columns on the table
groupByInputFields ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  MaybeT (SchemaT r m) (InputFieldsParser n [IR.GroupKeyField b])
groupByInputFields tableInfo = do
  namingCase <- retrieve (_rscNamingConvention . _siCustomization @b)
  let keyFieldName = applyFieldNameCaseCust namingCase Name._keys
  keysFieldValueParser <- groupByKeyField tableInfo
  pure $ P.field keyFieldName (Just "The keys on which to group by") (P.list keysFieldValueParser)

-- | Parser for the table_group_by_key input type that captures a grouping key
--
-- > {
-- >   column: table_select_column
-- > }
--
-- Fails in 'MaybeT' if there are no selectable columns on the table
groupByKeyField ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  MaybeT (SchemaT r m) (Parser 'Input n (IR.GroupKeyField b))
groupByKeyField tableInfo = do
  customization <- retrieve (_siCustomization @b)
  let namingCase = _rscNamingConvention customization
  let mkTypename = runMkTypename $ _rscTypeNames customization
  tableGQLName <- getTableIdentifierName tableInfo
  let groupByKeyTypeName = mkTypename $ applyTypeNameCaseIdentifier namingCase $ mkGroupByKeyTypeName tableGQLName
  let columnFieldName = applyFieldNameCaseCust namingCase Name._column

  tableColumnsEnumParser <- MaybeT $ tableSelectColumnsEnum tableInfo
  let groupByKeyFields =
        (\(column, _redactionExp) -> IR.GKFColumn column)
          <$> P.field columnFieldName (Just "A column grouping key") tableColumnsEnumParser
  pure $ P.object groupByKeyTypeName (Just groupByKeyDescription) groupByKeyFields
  where
    tableName = tableInfoName tableInfo
    groupByKeyDescription = G.Description $ "Allows the selection of a grouping key from " <>> tableName

-- | Parser for the selection set type of the group_by field, table_group_by
--
-- > {
-- >   group_key: table_group_by_key_fields
-- >   aggregate: table_aggregate_fields
-- > }
--
-- Fails in 'MaybeT' if there are no selectable columns on the table
groupBySelectionSet ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  MaybeT (SchemaT r m) (Parser 'Output n (Fields (IR.GroupByField b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
groupBySelectionSet tableInfo = do
  customization <- retrieve (_siCustomization @b)
  let namingCase = _rscNamingConvention customization
  let mkTypename = runMkTypename $ _rscTypeNames customization
  tableGQLName <- getTableIdentifierName tableInfo
  let groupByTypeName = mkTypename $ applyTypeNameCaseIdentifier namingCase $ mkGroupByTypeName tableGQLName
  let aggregateFieldName = applyFieldNameCaseCust namingCase Name._aggregate
  let groupKeyName = applyFieldNameCaseCust namingCase Name._group_key

  aggregateParser <- lift $ tableAggregationFields tableInfo
  groupByKeyParser <- lift $ groupByKeySelectionSet tableInfo
  P.selectionSet
    groupByTypeName
    (Just groupByDescription)
    [ IR.GBFGroupKey <$> P.subselection_ groupKeyName Nothing groupByKeyParser,
      IR.GBFAggregate <$> P.subselection_ aggregateFieldName Nothing aggregateParser
    ]
    <&> parsedSelectionsToFields IR.GBFExp
    & P.nonNullableParser
    & pure
  where
    tableName = tableInfoName tableInfo
    groupByDescription = G.Description $ "Group by fields of " <>> tableName

-- | Parser for the selection set type of the group_key field, table_group_by_key_fields
--
-- > {
-- >   TableColumnA: ColumnScalarType
-- >   TableColumnB: ColumnScalarType
-- >   ...
-- > }
--
-- This is not the same as 'defaultTableSelectionSet' because all columns must be nullable
-- since they can be null if selected but aren't used in the group key, or if a rollup
-- or cube type of grouping is used, which puts nulls into the group key columns when
-- rolling up.
groupByKeySelectionSet ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Parser 'Output n (Fields (IR.GroupKeyField b)))
groupByKeySelectionSet tableInfo = do
  customization <- retrieve (_siCustomization @b)
  let namingCase = _rscNamingConvention customization
  let mkTypename = runMkTypename $ _rscTypeNames customization
  tableGQLName <- getTableIdentifierName tableInfo
  let groupByKeyFieldsTypeName = mkTypename $ applyTypeNameCaseIdentifier namingCase $ mkGroupByKeyFieldsTypeName tableGQLName

  -- TODO(redactionExp): Probably need to deal with the redaction expression here
  scalarColumns <- mapMaybe (^? _1 . _SCIScalarColumn) <$> tableSelectColumns tableInfo
  columnFieldParsers <-
    for scalarColumns $ \columnInfo -> do
      let columnFieldName = ciName columnInfo
      let column = ciColumn columnInfo
      columnParser' <- columnParser (ciType columnInfo) (G.Nullability True)
      pure $ (IR.GKFColumn @b column) <$ P.selection_ columnFieldName (ciDescription columnInfo) columnParser'

  P.selectionSet
    groupByKeyFieldsTypeName
    (Just groupByKeyFieldsDescription)
    columnFieldParsers
    <&> parsedSelectionsToFields IR.GKFExp
    & P.nonNullableParser
    & pure
  where
    tableName = tableInfoName tableInfo
    groupByKeyFieldsDescription = G.Description $ "Allows the selection of fields from the grouping key of " <>> tableName

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
  ( AggregationPredicatesSchema b,
    BackendTableSelectSchema b,
    BackendNativeQuerySelectSchema b,
    Eq (AnnBoolExp b (IR.UnpreparedValue b)),
    MonadBuildSchema b r m n
  ) =>
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Output n (AnnotatedFields b)))
defaultTableSelectionSet tableInfo = runMaybeT do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      tableCoreInfo = _tiCoreInfo tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      logicalModelCache = _siLogicalModels sourceInfo
  roleName <- retrieve scRole
  _selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  schemaKind <- lift $ retrieve scSchemaKind
  -- If this check fails, it means we're attempting to build a Relay schema, but
  -- the current backend b does't support Relay; rather than returning an
  -- incomplete selection set, we fail early and return 'Nothing'. This check
  -- must happen first, since we can't memoize a @Maybe Parser@.
  guard $ isHasuraSchema schemaKind || isJust (relayExtension @b)
  lift $ P.memoizeOn 'defaultTableSelectionSet (sourceName, tableName) do
    tableGQLName <- getTableIdentifierName tableInfo
    let objectTypename = mkTypename $ applyTypeNameCaseIdentifier tCase tableGQLName
        xRelay = relayExtension @b
        tableFields = HashMap.elems $ _tciFieldInfoMap tableCoreInfo
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
            then [(G.Directive Name._key . HashMap.singleton Name._fields . G.VString) pkFieldDirective]
            else mempty
        description = G.Description . Postgres.getPGDescription <$> _tciDescription tableCoreInfo
    fieldParsers <-
      concat
        <$> for
          tableFields
          (fieldSelection logicalModelCache tableName tableInfo)

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
        context <- asks getter
        options <- asks getter
        -- This `lift` is important! If we don't use it, the underlying node
        -- builder will assume that the current `SchemaT r m` is the monad in
        -- which to run, and will stack another `SchemaT` on top of it when
        -- recursively processing tables.
        nodeInterface <- lift $ runNodeBuilder nodeBuilder context options
        pure
          $ selectionSetObjectWithDirective objectTypename description allFieldParsers [nodeInterface] pkDirectives
          <&> parsedSelectionsToFields IR.AFExpression
      _ ->
        pure
          $ selectionSetObjectWithDirective objectTypename description fieldParsers [] pkDirectives
          <&> parsedSelectionsToFields IR.AFExpression
  where
    selectionSetObjectWithDirective name description parsers implementsInterfaces directives =
      IP.setParserDirectives directives
        $ P.selectionSetObject name description parsers implementsInterfaces

-- | List of table fields object.
-- Just a @'nonNullableObjectList' wrapper over @'tableSelectionSet'.
-- > table_name: [table!]!
tableSelectionList ::
  (MonadBuildSchema b r m n, BackendTableSelectSchema b) =>
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Output n (AnnotatedFields b)))
tableSelectionList tableInfo =
  fmap nonNullableObjectList <$> tableSelectionSet tableInfo

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
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Output n (ConnectionFields b)))
tableConnectionSelectionSet tableInfo = runMaybeT do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
  roleName <- retrieve scRole
  tableIdentifierName <- lift $ getTableIdentifierName tableInfo
  let tableGQLName = applyTypeNameCaseIdentifier tCase tableIdentifierName
  void $ hoistMaybe $ tableSelectPermissions roleName tableInfo
  edgesParser <- MaybeT $ tableEdgesSelectionSet mkTypename tableGQLName
  lift $ P.memoizeOn 'tableConnectionSelectionSet (sourceName, tableName) do
    let connectionTypeName = mkTypename $ tableGQLName <> Name._Connection
        pageInfo =
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
    pure
      $ P.nonNullableParser
      $ P.selectionSet connectionTypeName (Just connectionDescription) [pageInfo, edges]
      <&> parsedSelectionsToFields IR.ConnectionTypename
  where
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
       in P.nonNullableParser
            $ P.selectionSet Name._PageInfo Nothing allFields
            <&> parsedSelectionsToFields IR.PageInfoTypename

    tableEdgesSelectionSet ::
      (G.Name -> G.Name) ->
      G.Name ->
      SchemaT r m (Maybe (Parser 'Output n (EdgeFields b)))
    tableEdgesSelectionSet mkTypename tableGQLName = runMaybeT do
      edgeNodeParser <- MaybeT $ fmap P.nonNullableParser <$> tableSelectionSet tableInfo
      let edgesType = mkTypename $ tableGQLName <> Name._Edge
          cursor =
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
      pure
        $ nonNullableObjectList
        $ P.selectionSet edgesType Nothing [cursor, edgeNode]
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
  (MonadBuildSchema b r m n, AggregationPredicatesSchema b) =>
  TableInfo b ->
  SchemaT r m (InputFieldsParser n (SelectArgs b))
defaultTableArgs tableInfo = do
  whereParser <- tableWhereArg tableInfo
  orderByParser <- tableOrderByArg tableInfo
  distinctParser <- tableDistinctArg tableInfo
  defaultArgsParser whereParser orderByParser distinctParser

-- | Argument to filter rows returned from table selection
-- > where: table_bool_exp
tableWhereArg ::
  forall b r m n.
  ( AggregationPredicatesSchema b,
    MonadBuildSchema b r m n
  ) =>
  TableInfo b ->
  SchemaT r m (InputFieldsParser n (Maybe (IR.AnnBoolExp b (IR.UnpreparedValue b))))
tableWhereArg tableInfo = do
  boolExpParser <- tableBoolExp tableInfo
  pure
    $ fmap join
    $ P.fieldOptional whereName whereDesc
    $ P.nullable boolExpParser
  where
    whereName = Name._where
    whereDesc = Just $ G.Description "filter the rows returned"

-- | Argument to sort rows returned from table selection
-- > order_by: [table_order_by!]
tableOrderByArg ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (InputFieldsParser n (Maybe (NonEmpty (IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)))))
tableOrderByArg tableInfo = do
  tCase <- retrieve $ _rscNamingConvention . _siCustomization @b
  orderByParser <- tableOrderByExp tableInfo
  let orderByName = applyFieldNameCaseCust tCase Name._order_by
      orderByDesc = Just $ G.Description "sort the rows by one or more columns"
  pure $ do
    maybeOrderByExps <-
      fmap join
        $ P.fieldOptional orderByName orderByDesc
        $ P.nullable
        $ P.list orderByParser
    pure $ maybeOrderByExps >>= NE.nonEmpty . concat

-- | Argument to distinct select on columns returned from table selection
-- > distinct_on: [table_select_column!]
tableDistinctArg ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (InputFieldsParser n (Maybe (NonEmpty (IR.AnnDistinctColumn b (IR.UnpreparedValue b)))))
tableDistinctArg tableInfo = do
  tCase <- retrieve $ _rscNamingConvention . _siCustomization @b
  columnsEnum <- fmap (fmap (uncurry IR.AnnDistinctColumn)) <$> tableSelectColumnsEnum tableInfo
  let distinctOnName = applyFieldNameCaseCust tCase Name._distinct_on
      distinctOnDesc = Just $ G.Description "distinct select on columns"
  pure do
    maybeDistinctOnColumns <-
      join
        . join
        <$> for
          columnsEnum
          (P.fieldOptional distinctOnName distinctOnDesc . P.nullable . P.list)
    pure $ maybeDistinctOnColumns >>= NE.nonEmpty

-- | Argument to limit rows returned from table selection
-- > limit: NonNegativeInt
tableLimitArg ::
  forall n.
  (MonadParse n) =>
  InputFieldsParser n (Maybe Int)
tableLimitArg =
  fmap (fmap fromIntegral . join)
    $ P.fieldOptional limitName limitDesc
    $ P.nullable P.nonNegativeInt
  where
    limitName = Name._limit
    limitDesc = Just $ G.Description "limit the number of rows returned"

-- | Argument to skip some rows, in conjunction with order_by
-- > offset: BigInt
tableOffsetArg ::
  forall n.
  (MonadParse n) =>
  InputFieldsParser n (Maybe Int64)
tableOffsetArg =
  fmap join
    $ P.fieldOptional offsetName offsetDesc
    $ P.nullable P.bigInt
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
  (MonadBuildSchema b r m n, AggregationPredicatesSchema b) =>
  PrimaryKeyColumns b ->
  TableInfo b ->
  SelPermInfo b ->
  SchemaT
    r
    m
    ( InputFieldsParser
        n
        ( SelectArgs b,
          Maybe (NonEmpty (IR.ConnectionSplit b (IR.UnpreparedValue b))),
          Maybe IR.ConnectionSlice
        )
    )
tableConnectionArgs pkeyColumns tableInfo selectPermissions = do
  whereParser <- tableWhereArg tableInfo
  orderByParser <- fmap (fmap appendPrimaryKeyOrderBy) <$> tableOrderByArg tableInfo
  distinctParser <- tableDistinctArg tableInfo
  let maybeFirst = fmap join $ P.fieldOptional Name._first Nothing $ P.nullable P.nonNegativeInt
      maybeLast = fmap join $ P.fieldOptional Name._last Nothing $ P.nullable P.nonNegativeInt
      maybeAfter = fmap join $ P.fieldOptional Name._after Nothing $ P.nullable base64Text
      maybeBefore = fmap join $ P.fieldOptional Name._before Nothing $ P.nullable base64Text
      firstAndLast = (,) <$> maybeFirst <*> maybeLast
      afterBeforeAndOrderBy = (,,) <$> maybeAfter <*> maybeBefore <*> orderByParser

  scalarTypeParsingContext <- askScalarTypeParsingContext @b
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
        for rawSplit (uncurry (parseConnectionSplit scalarTypeParsingContext orderBy'))

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

    appendPrimaryKeyOrderBy :: NonEmpty (IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)) -> NonEmpty (IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b))
    appendPrimaryKeyOrderBy orderBys@(h NE.:| t) =
      let orderByColumnNames =
            orderBys ^.. traverse . to IR.obiColumn . IR._AOCColumn . _1 . to ciColumn
          pkeyOrderBys = flip mapMaybe (toList pkeyColumns) $ \columnInfo ->
            if ciColumn columnInfo `elem` orderByColumnNames
              then Nothing
              else
                let redactionExp = fromMaybe NoRedaction $ getRedactionExprForColumn selectPermissions (ciColumn columnInfo)
                 in Just $ IR.OrderByItemG Nothing (IR.AOCColumn columnInfo redactionExp) Nothing
       in h NE.:| (t <> pkeyOrderBys)

    parseConnectionSplit ::
      ScalarTypeParsingContext b ->
      Maybe (NonEmpty (IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b))) ->
      IR.ConnectionSplitKind ->
      BL.ByteString ->
      n (NonEmpty (IR.ConnectionSplit b (IR.UnpreparedValue b)))
    parseConnectionSplit scalarTypeParsingContext maybeOrderBys splitKind cursorSplit = do
      cursorValue <- J.eitherDecode cursorSplit `onLeft` const throwInvalidCursor
      case maybeOrderBys of
        Nothing -> forM (nonEmptySeqToNonEmptyList pkeyColumns)
          $ \columnInfo -> do
            let columnJsonPath = [J.Key $ K.fromText $ toTxt $ ciColumn columnInfo]
                columnType = ciType columnInfo
                redactionExp = fromMaybe NoRedaction $ getRedactionExprForColumn selectPermissions (ciColumn columnInfo)
            columnValue <-
              iResultToMaybe (executeJSONPath columnJsonPath cursorValue)
                `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parseScalarValueColumnTypeWithContext scalarTypeParsingContext columnType columnValue
            let unresolvedValue = IR.UVParameter IR.FreshVar $ ColumnValue columnType pgValue
            pure
              $ IR.ConnectionSplit splitKind unresolvedValue
              $ IR.OrderByItemG Nothing (IR.AOCColumn columnInfo redactionExp) Nothing
        Just orderBys ->
          forM orderBys $ \orderBy -> do
            let IR.OrderByItemG orderType annObCol nullsOrder = orderBy
                columnType = getOrderByColumnType annObCol
            orderByItemValue <-
              iResultToMaybe (executeJSONPath (map (J.Key . K.fromText) (getPathFromOrderBy annObCol)) cursorValue)
                `onNothing` throwInvalidCursor
            pgValue <- liftQErr $ parseScalarValueColumnTypeWithContext scalarTypeParsingContext columnType orderByItemValue
            let unresolvedValue = IR.UVParameter IR.FreshVar $ ColumnValue columnType pgValue
            pure
              $ IR.ConnectionSplit splitKind unresolvedValue
              $ IR.OrderByItemG orderType annObCol nullsOrder
      where
        throwInvalidCursor = parseError "the \"after\" or \"before\" cursor is invalid"
        liftQErr = either (parseError . toErrorMessage . qeError) pure . runExcept

        mkAggregateOrderByPath = \case
          IR.AAOCount -> ["count"]
          IR.AAOOp IR.AggregateOrderByColumn {..} -> [_aobcAggregateFunctionName, toTxt $ ciColumn _aobcColumn]

        getPathFromOrderBy = \case
          IR.AOCColumn columnInfo _redactionExp ->
            let pathElement = toTxt $ ciColumn columnInfo
             in [pathElement]
          IR.AOCNestedObject nestedObjectInfo nestedOrderBy ->
            let pathElement = toTxt $ _noiName nestedObjectInfo
             in pathElement : getPathFromOrderBy nestedOrderBy
          IR.AOCObjectRelation relInfo _ obCol ->
            let pathElement = relNameToTxt $ riName relInfo
             in pathElement : getPathFromOrderBy obCol
          IR.AOCArrayAggregation relInfo _ aggOb ->
            let fieldName = relNameToTxt (riName relInfo) <> "_aggregate"
             in fieldName : mkAggregateOrderByPath aggOb
          IR.AOCComputedField cfob ->
            let fieldNameText = computedFieldNameToText $ IR._cfobName cfob
             in case IR._cfobOrderByElement cfob of
                  IR.CFOBEScalar _ _redactionExp -> [fieldNameText]
                  IR.CFOBETableAggregation _ _ aggOb ->
                    (fieldNameText <> "_aggregate") : mkAggregateOrderByPath aggOb

        getOrderByColumnType = \case
          IR.AOCColumn columnInfo _redactionExp -> ciType columnInfo
          IR.AOCNestedObject _ nestedOrderBy -> getOrderByColumnType nestedOrderBy
          IR.AOCObjectRelation _ _ obCol -> getOrderByColumnType obCol
          IR.AOCArrayAggregation _ _ aggOb -> aggregateOrderByColumnType aggOb
          IR.AOCComputedField cfob ->
            case IR._cfobOrderByElement cfob of
              IR.CFOBEScalar scalarType _redactionExp -> ColumnScalar scalarType
              IR.CFOBETableAggregation _ _ aggOb -> aggregateOrderByColumnType aggOb
          where
            aggregateOrderByColumnType = \case
              IR.AAOCount -> ColumnScalar (aggregateOrderByCountType @b)
              IR.AAOOp IR.AggregateOrderByColumn {..} -> _aobcAggregateFunctionReturnType

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
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Parser 'Output n (IR.AggregateFields b (IR.UnpreparedValue b)))
tableAggregationFields tableInfo = do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = _rscTypeNames customization
  P.memoizeOn 'tableAggregationFields (sourceName, tableName) do
    tableGQLName <- getTableIdentifierName tableInfo
    allScalarColumns <- mapMaybe (\(column, redactionExp) -> column ^? _SCIScalarColumn <&> (,redactionExp)) <$> tableSelectColumns tableInfo
    allComputedFields <-
      if supportsAggregateComputedFields @b -- See 'supportsAggregateComputedFields' for an explanation
        then tableSelectComputedFields tableInfo
        else pure []
    let numericColumns = filter (isNumCol . fst) allScalarColumns
        numericComputedFields = onlyNumComputedFields allComputedFields
        comparableColumns = filter (isComparableCol . fst) allScalarColumns
        comparableComputedFields = onlyComparableComputedFields allComputedFields
        customOperatorsAndColumns =
          HashMap.toList $ HashMap.mapMaybe (getCustomAggOpsColumns allScalarColumns) $ getCustomAggregateOperators @b (_siConfiguration sourceInfo)
        description = G.Description $ "aggregate fields of " <>> tableInfoName tableInfo
        selectName = runMkTypename mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableAggregateFieldTypeName tableGQLName
    count <- countField
    nonCountComputedFieldsMap <-
      fmap (HashMap.unionsWith (++) . concat)
        $ sequenceA
        $ catMaybes
          [ -- operators on numeric computed fields
            if null numericComputedFields
              then Nothing
              else Just
                $ for numericAggOperators
                $ \operator -> do
                  numFields <- mkColumnAggComputedFields tableName numericComputedFields

                  pure $ HashMap.singleton operator numFields,
            -- operators on comparable computed fields
            if null comparableComputedFields
              then Nothing
              else Just
                $ for comparisonAggOperators
                $ \operator -> do
                  comparableFields <- mkColumnAggComputedFields tableName comparableComputedFields

                  pure $ HashMap.singleton operator comparableFields
          ]
    nonCountFieldsMap <-
      fmap (HashMap.unionsWith (++) . concat)
        $ sequenceA
        $ catMaybes
          [ -- operators on numeric columns
            if null numericColumns
              then Nothing
              else Just
                $ for numericAggOperators
                $ \operator -> do
                  numFields <- mkNumericAggFields operator numericColumns
                  pure $ HashMap.singleton operator numFields,
            -- operators on comparable columns
            if null comparableColumns
              then Nothing
              else Just $ do
                comparableFields <- traverse mkColumnAggField comparableColumns
                pure
                  $ comparisonAggOperators
                  & map \operator ->
                    HashMap.singleton operator comparableFields,
            -- -- custom operators
            if null customOperatorsAndColumns
              then Nothing
              else Just
                $ for customOperatorsAndColumns \(operator, columnTypes) -> do
                  customFields <- traverse (uncurry mkNullableScalarTypeAggField) (toList columnTypes)
                  pure $ HashMap.singleton (C.fromCustomName operator) customFields
          ]

    let nonCountFields =
          HashMap.mapWithKey
            ( \operator fields -> parseAggOperator mkTypename operator tCase tableGQLName fields
            )
            (HashMap.unionWith (++) nonCountFieldsMap nonCountComputedFieldsMap)

        aggregateFields :: [FieldParser n (IR.AggregateField b (IR.UnpreparedValue b))]
        aggregateFields = count : HashMap.elems nonCountFields
    pure
      $ P.selectionSet selectName (Just description) aggregateFields
      <&> parsedSelectionsToFields IR.AFExp
  where
    getCustomAggOpsColumns :: [(ColumnInfo b, AnnRedactionExpUnpreparedValue b)] -> HashMap (ScalarType b) (ScalarType b) -> Maybe (NonEmpty ((ColumnInfo b, AnnRedactionExpUnpreparedValue b), ScalarType b))
    getCustomAggOpsColumns columnInfos typeMap =
      columnInfos
        & mapMaybe
          ( \(ci@ColumnInfo {..}, redactionExp) ->
              case ciType of
                ColumnEnumReference _ -> Nothing
                ColumnScalar scalarType ->
                  ((ci, redactionExp),) <$> HashMap.lookup scalarType typeMap
          )
        & nonEmpty

    mkColumnAggComputedFields :: TableName b -> [ComputedFieldInfo b] -> SchemaT r m [FieldParser n (IR.SelectionField b (IR.UnpreparedValue b))]
    mkColumnAggComputedFields tableName computedFieldInfos =
      traverse (mkColumnAggComputedField tableName) computedFieldInfos <&> catMaybes

    mkColumnAggComputedField :: TableName b -> ComputedFieldInfo b -> SchemaT r m (Maybe (FieldParser n (IR.SelectionField b (IR.UnpreparedValue b))))
    mkColumnAggComputedField tableName computedFieldInfo = do
      let annotatedFieldToSelectionField :: AnnotatedField b -> n (IR.SelectionField b (IR.UnpreparedValue b))
          annotatedFieldToSelectionField = \case
            IR.AFComputedField _ computedFieldName (IR.CFSScalar computedFieldScalarSelect) ->
              pure $ IR.SFComputedField computedFieldName computedFieldScalarSelect
            _ -> parseError "Only computed fields that return scalar types are supported"

      computedField computedFieldInfo tableName tableInfo
        >>= \case
          (Just fieldParser) -> (pure . Just) (fieldParser `P.bindField` annotatedFieldToSelectionField)
          Nothing -> pure Nothing

    mkNumericAggFields :: GQLNameIdentifier -> [(ColumnInfo b, AnnRedactionExpUnpreparedValue b)] -> SchemaT r m [FieldParser n (IR.SelectionField b (IR.UnpreparedValue b))]
    mkNumericAggFields name
      | (C.toSnakeG name) == Name._sum = traverse mkColumnAggField
      -- Memoize here for more sharing. Note: we can't do `P.memoizeOn 'mkNumericAggFields...`
      -- due to stage restrictions, so just add a string key:
      | otherwise = traverse \(columnInfo, redactionExp) ->
          P.memoizeOn 'tableAggregationFields ("mkNumericAggFields" :: Text, columnInfo)
            $
            -- CAREFUL!: below must only reference columnInfo else memoization key needs to be adapted
            pure
            $! do
              let !cfcol = IR.SFCol (ciColumn columnInfo) (ciType columnInfo) redactionExp
              P.selection_
                (ciName columnInfo)
                (ciDescription columnInfo)
                (P.nullable P.float)
                $> cfcol

    mkColumnAggField :: (ColumnInfo b, AnnRedactionExpUnpreparedValue b) -> SchemaT r m (FieldParser n (IR.SelectionField b (IR.UnpreparedValue b)))
    mkColumnAggField columnAndRedactionExp@(columnInfo, _redactionExp) =
      mkColumnAggField' columnAndRedactionExp (ciType columnInfo)

    mkColumnAggField' :: (ColumnInfo b, AnnRedactionExpUnpreparedValue b) -> ColumnType b -> SchemaT r m (FieldParser n (IR.SelectionField b (IR.UnpreparedValue b)))
    mkColumnAggField' (columnInfo, redactionExp) resultType = do
      field <- columnParser resultType (G.Nullability True)
      pure
        $ P.selection_
          (ciName columnInfo)
          (ciDescription columnInfo)
          field
        $> IR.SFCol (ciColumn columnInfo) (ciType columnInfo) redactionExp

    mkNullableScalarTypeAggField :: (ColumnInfo b, AnnRedactionExpUnpreparedValue b) -> ScalarType b -> SchemaT r m (FieldParser n (IR.SelectionField b (IR.UnpreparedValue b)))
    mkNullableScalarTypeAggField columnInfo resultType =
      mkColumnAggField' columnInfo (ColumnScalar resultType)

    countField :: SchemaT r m (FieldParser n (IR.AggregateField b (IR.UnpreparedValue b)))
    countField = do
      columnsEnum <- tableSelectColumnsEnum tableInfo
      let distinctName = Name._distinct
          args = do
            distinct <- P.fieldOptional distinctName Nothing P.boolean
            mkCountType <- countTypeInput @b columnsEnum
            pure
              $ mkCountType
              $ maybe
                IR.SelectCountNonDistinct -- If "distinct" is "null" or absent, we default to @'SelectCountNonDistinct'
                (bool IR.SelectCountNonDistinct IR.SelectCountDistinct)
                distinct

      pure $ IR.AFCount <$> P.selection Name._count Nothing args P.int

    parseAggOperator ::
      MkTypename ->
      GQLNameIdentifier ->
      NamingCase ->
      GQLNameIdentifier ->
      [FieldParser n (IR.SelectionField b (IR.UnpreparedValue b))] ->
      FieldParser n (IR.AggregateField b (IR.UnpreparedValue b))
    parseAggOperator makeTypename operator tCase tableGQLName columns =
      let opFieldName = applyFieldNameCaseIdentifier tCase operator
          opText = G.unName opFieldName
          setName = runMkTypename makeTypename $ applyTypeNameCaseIdentifier tCase $ mkTableAggOperatorTypeName tableGQLName operator
          setDesc = Just $ G.Description $ "aggregate " <> opText <> " on columns"
          subselectionParser =
            P.selectionSet setName setDesc columns
              <&> parsedSelectionsToFields IR.SFExp
       in P.subselection_ opFieldName Nothing subselectionParser
            <&> IR.AFOp . IR.AggregateOp opText

-- | shared implementation between tables and logical models
defaultArgsParser ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  InputFieldsParser n (Maybe (AnnBoolExp b (IR.UnpreparedValue b))) ->
  InputFieldsParser n (Maybe (NonEmpty (IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)))) ->
  InputFieldsParser n (Maybe (NonEmpty (IR.AnnDistinctColumn b (IR.UnpreparedValue b)))) ->
  SchemaT r m (InputFieldsParser n (SelectArgs b))
defaultArgsParser whereParser orderByParser distinctParser = do
  let result = do
        whereArg <- whereParser
        orderByArg <- orderByParser
        limitArg <- tableLimitArg
        offsetArg <- tableOffsetArg
        distinctArg <- distinctParser
        pure
          $ IR.SelectArgs
            { IR._saWhere = whereArg,
              IR._saOrderBy = orderByArg,
              IR._saLimit = limitArg,
              IR._saOffset = offsetArg,
              IR._saDistinct = distinctArg
            }
  pure
    $ result
    `P.bindFields` \args -> do
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
              IR.AOCColumn columnInfo _redactionExp -> Just $ ciColumn columnInfo
              _ -> Nothing
          isValid =
            (colsLen == length initOrdByCols)
              && all (`elem` initOrdByCols) (IR._adcColumn <$> toList distinctCols)
      unless isValid
        $ parseError
          "\"distinct_on\" columns must match initial \"order_by\" columns"

-- | An individual field of a table
--
-- > field_name(arg_name: arg_type, ...): field_type
fieldSelection ::
  forall b r m n.
  ( AggregationPredicatesSchema b,
    BackendTableSelectSchema b,
    BackendNativeQuerySelectSchema b,
    Eq (AnnBoolExp b (IR.UnpreparedValue b)),
    MonadBuildSchema b r m n
  ) =>
  LogicalModelCache b ->
  TableName b ->
  TableInfo b ->
  FieldInfo b ->
  SchemaT r m [FieldParser n (AnnotatedField b)]
fieldSelection logicalModelCache table tableInfo = \case
  FIColumn (SCIScalarColumn columnInfo) ->
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
      guard $ columnName `HashMap.member` spiCols selectPermissions
      let redactionExp = fromMaybe NoRedaction $ getRedactionExprForColumn selectPermissions columnName
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
          nullability = ciIsNullable columnInfo || redactionExp /= NoRedaction
      field <- lift $ columnParser (ciType columnInfo) (G.Nullability nullability)
      pure
        $! P.selection fieldName (ciDescription columnInfo) pathArg field
        <&> IR.mkAnnColumnField (ciColumn columnInfo) (ciType columnInfo) redactionExp
  FIColumn (SCIObjectColumn nestedObjectInfo) ->
    pure . fmap IR.AFNestedObject <$> nestedObjectFieldParser nestedObjectInfo
  FIColumn (SCIArrayColumn NestedArrayInfo {..}) ->
    fmap (nestedArrayFieldParser _naiSupportsNestedArrays _naiIsNullable) <$> fieldSelection logicalModelCache table tableInfo (FIColumn _naiColumnInfo)
  FIRelationship relationshipInfo ->
    concat . maybeToList <$> relationshipField table relationshipInfo
  FIComputedField computedFieldInfo ->
    maybeToList <$> computedField computedFieldInfo table tableInfo
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
  where
    nestedObjectFieldParser :: NestedObjectInfo b -> SchemaT r m (FieldParser n (AnnotatedNestedObjectSelect b))
    nestedObjectFieldParser NestedObjectInfo {..} = do
      case HashMap.lookup _noiType logicalModelCache of
        Just objectType -> do
          parser <- nestedObjectParser _noiSupportsNestedObjects logicalModelCache objectType _noiColumn _noiIsNullable
          pure $ P.subselection_ _noiName _noiDescription parser
        _ -> throw500 $ "fieldSelection: object type " <> _noiType <<> " not found"

outputParserModifier :: Bool -> IP.Parser origin 'Output m a -> IP.Parser origin 'Output m a
outputParserModifier True = P.nullableParser
outputParserModifier False = P.nonNullableParser

nestedArrayFieldParser :: forall origin m b r v. (Functor m) => XNestedObjects b -> Bool -> IP.FieldParser origin m (IR.AnnFieldG b r v) -> IP.FieldParser origin m (IR.AnnFieldG b r v)
nestedArrayFieldParser supportsNestedArrays isNullable =
  wrapNullable isNullable . IP.multipleField . fmap (IR.AFNestedArray @b supportsNestedArrays . IR.ANASSimple)

wrapNullable :: Bool -> IP.FieldParser origin m a -> IP.FieldParser origin m a
wrapNullable isNullable = if isNullable then IP.nullableField else IP.nonNullableField

nestedObjectParser ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  XNestedObjects b ->
  LogicalModelCache b ->
  LogicalModelInfo b ->
  Column b ->
  Bool ->
  SchemaT r m (P.Parser 'Output n (AnnotatedNestedObjectSelect b))
nestedObjectParser supportsNestedObjects objectTypes LogicalModelInfo {..} column isNullable = do
  allFieldParsers <- for (toList $ _lmiFields) outputFieldParser
  let LogicalModelName gqlName = _lmiName
  pure
    $ outputParserModifier isNullable
    $ P.selectionSet gqlName (G.Description <$> _lmiDescription) allFieldParsers
    <&> IR.AnnNestedObjectSelectG supportsNestedObjects column . parsedSelectionsToFields IR.AFExpression
  where
    outputFieldParser ::
      LogicalModelField b ->
      SchemaT r m (IP.FieldParser MetadataObjId n (IR.AnnFieldG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)))
    outputFieldParser LogicalModelField {..} =
      P.memoizeOn 'nestedObjectParser (_lmiName, lmfName) do
        name <- textToName $ toTxt lmfName
        let go = \case
              LogicalModelTypeScalar LogicalModelTypeScalarC {..} -> do
                fieldTypeName <- textToName $ toTxt lmtsScalar
                wrapScalar lmtsNullable name lmtsScalar $ customScalarParser lmtsNullable fieldTypeName
              LogicalModelTypeReference LogicalModelTypeReferenceC {..} -> do
                objectType' <- HashMap.lookup lmtrReference objectTypes `onNothing` throw500 ("Custom logical model type " <> lmtrReference <<> " not found")
                parser <- fmap (IR.AFNestedObject @b) <$> nestedObjectParser supportsNestedObjects objectTypes objectType' lmfName lmtrNullable
                pure $ P.subselection_ name (G.Description <$> lmfDescription) parser
              LogicalModelTypeArray LogicalModelTypeArrayC {..} -> do
                nestedArrayFieldParser supportsNestedObjects lmtaNullable <$> go lmtaArray
        go lmfType
      where
        wrapScalar isNullable' name scalarType parser =
          pure
            $ wrapNullable isNullable' (P.selection_ name (G.Description <$> lmfDescription) parser)
            $> IR.mkAnnColumnField lmfName (ColumnScalar scalarType) NoRedaction Nothing
        customScalarParser isNullable' fieldTypeName =
          let nullable = if isNullable' then P.Nullable else P.NonNullable
              schemaType = P.TNamed nullable $ P.Definition fieldTypeName Nothing Nothing [] P.TIScalar
           in P.Parser
                { pType = schemaType,
                  pParser = P.valueToJSON (P.toGraphQLType schemaType)
                }

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
  ( AggregationPredicatesSchema b,
    BackendTableSelectSchema b,
    BackendNativeQuerySelectSchema b,
    Eq (AnnBoolExp b (IR.UnpreparedValue b)),
    MonadBuildSchema b r m n
  ) =>
  TableName b ->
  RelInfo b ->
  SchemaT r m (Maybe [FieldParser n (AnnotatedField b)])
relationshipField table ri@RelInfo {riTarget = RelTargetTable otherTableName} = runMaybeT do
  tCase <- retrieve $ _rscNamingConvention . _siCustomization @b
  roleName <- retrieve scRole
  optimizePermissionFilters <- retrieve Options.soOptimizePermissionFilters
  tableInfo <- lift $ askTableInfo @b table
  otherTableInfo <- lift $ askTableInfo otherTableName
  tablePerms <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  remotePerms <- hoistMaybe $ tableSelectPermissions roleName otherTableInfo
  relFieldName <- lift $ textToName $ relNameToTxt $ riName ri
  -- START black magic to deduplicate permission checks
  let thisTablePerm = IR._tpFilter $ tablePermissionsInfo tablePerms
      deduplicatePermissions :: AnnBoolExp b (IR.UnpreparedValue b) -> AnnBoolExp b (IR.UnpreparedValue b)
      deduplicatePermissions x =
        case (optimizePermissionFilters, x) of
          ( OptimizePermissionFilters,
            BoolAnd
              [ BoolField
                  -- We want to match on relationships that appear in permissions of the _current_ table.
                  -- Therefore we need to inspect `rfFilter`, rather than `rfTargetTablePermissions`,
                  -- which encodes the permissions on the _target_ table, which are always necessarily
                  -- empty, since they are not included in the permission definitions of the _current_ table.
                  ( AVRelationship
                      remoteRI
                      RelationshipFilters
                        { rfTargetTablePermissions = BoolAnd [],
                          rfFilter
                        }
                    )
                ]
            ) ->
              -- Here we try to figure out if the "forwards" joining condition
              -- from `table` to the related table `riRTable ri` is equal to the
              -- "backwards" joining condition from the related table back to
              -- `table`.  If it is, then we can optimize the row-level permission
              -- filters by dropping them here.
              let remoteTableName = case riTarget remoteRI of
                    RelTargetTable tn -> Just tn
                    _ -> Nothing
               in if (remoteTableName == Just table)
                    && (unRelMapping (riMapping remoteRI) `HashMap.isInverseOf` unRelMapping (riMapping ri))
                    && (thisTablePerm == rfFilter)
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
      selectionSetParser <- MaybeT $ tableSelectionSet otherTableInfo
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
          let columns = fmap (getColumnPathColumn @b) $ HashMap.keys $ unRelMapping $ riMapping ri
              fieldInfoMap = _tciFieldInfoMap $ _tiCoreInfo tableInfo
              findColumn col = HashMap.lookup (fromCol @b col) fieldInfoMap ^? _Just . _FIColumn . _SCIScalarColumn
          -- Fetch information about the referencing columns of the foreign key
          -- constraint
          colInfo <-
            traverse findColumn columns
              `onNothing` throw500 "could not find column info in schema cache"
          pure $ boolToNullable $ any ciIsNullable colInfo
        -- Manual or reverse relationships are always nullable
        _ -> pure Nullable
      pure
        $ pure
        $ case nullable of Nullable -> id; NotNullable -> IP.nonNullableField
        $ P.subselection_ relFieldName desc selectionSetParser
        <&> \fields ->
          IR.AFObjectRelation
            $ IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) Nullable
            $ IR.AnnObjectSelectG fields (IR.FromTable otherTableName)
            $ deduplicatePermissions
            $ IR._tpFilter
            $ tablePermissionsInfo remotePerms
    ArrRel -> do
      let arrayRelDesc = Just $ G.Description "An array relationship"
      otherTableParser <- MaybeT $ selectTable otherTableInfo relFieldName arrayRelDesc
      let arrayRelField =
            otherTableParser <&> \selectExp ->
              IR.AFArrayRelation
                $ IR.ASSimple
                $ IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) Nullable
                $ deduplicatePermissions' selectExp
          relAggFieldName = applyFieldNameCaseCust tCase $ relFieldName <> Name.__aggregate
          relAggDesc = Just $ G.Description "An aggregate relationship"
      remoteAggField <- lift $ selectTableAggregate otherTableInfo relAggFieldName relAggDesc
      remoteConnectionField <- runMaybeT $ do
        -- Parse array connection field only for relay schema
        RelaySchema _ <- retrieve scSchemaKind
        _xRelayInfo <- hoistMaybe $ relayExtension @b
        pkeyColumns <-
          MaybeT
            $ (^? tiCoreInfo . tciPrimaryKey . _Just . pkColumns)
            <$> pure otherTableInfo
        let relConnectionName = relFieldName <> Name.__connection
            relConnectionDesc = Just $ G.Description "An array relationship connection"
        MaybeT $ lift $ selectTableConnection otherTableInfo relConnectionName relConnectionDesc pkeyColumns
      pure
        $ catMaybes
          [ Just arrayRelField,
            fmap (IR.AFArrayRelation . IR.ASAggregate . IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) Nullable) <$> remoteAggField,
            fmap (IR.AFArrayRelation . IR.ASConnection . IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) Nullable) <$> remoteConnectionField
          ]
relationshipField _table ri@RelInfo {riTarget = RelTargetNativeQuery nativeQueryName} = runMaybeT do
  relFieldName <- lift $ textToName $ relNameToTxt $ riName ri

  case riType ri of
    ObjRel -> do
      nativeQueryInfo <- askNativeQueryInfo nativeQueryName

      let objectRelDesc = Just $ G.Description "An object relationship"

      nativeQueryParser <-
        MaybeT $ selectNativeQueryObject nativeQueryInfo relFieldName objectRelDesc

      -- this only affects the generated GraphQL type
      let nullability = Nullable

      pure
        $ pure
        $ nativeQueryParser
        <&> \selectExp ->
          IR.AFObjectRelation (IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) nullability selectExp)
    ArrRel -> do
      nativeQueryInfo <- askNativeQueryInfo nativeQueryName

      let objectRelDesc = Just $ G.Description "An array relationship"
          arrayNullability = Nullable
          innerNullability = Nullable

      nativeQueryParser <-
        MaybeT $ selectNativeQuery nativeQueryInfo relFieldName arrayNullability objectRelDesc

      pure
        $ pure
        $ nativeQueryParser
        <&> \selectExp ->
          IR.AFArrayRelation
            $ IR.ASSimple
            $ IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) innerNullability selectExp
