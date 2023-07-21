-- | This module provides building blocks for the GraphQL Schema that the
-- GraphQL Engine presents.
--
-- The functions defined here are used to serve as default implementations for
-- their namesakes in the 'BackendSchema' type class.
--
-- When, for some backend, you want to implement a new feature that manifests
-- itself visibly in the schema (e.g., if you're developing support for update
-- mutations), this module is likely where your efforts should start.
--
-- Using these functions help us present a consistent GraphQL schema across
-- different backends.
--
-- There is a bit of tension however, as sometimes we intentionally do want the
-- GraphQL Schema relating to some backend to be different in some way.
--
-- It could be that a backend only has limited support for some common feature,
-- or, more interestingly, that some backend just does things differently (c.f.
-- MSSQL's @MERGE@ statement with PostgreSQL's @INSERT .. ON CONFLICT@, which
-- are similar enough that we want to use the same overall upsert schema but
-- different enough that we want to use different field names)
--
-- When you want to implement new schema for a backend, there is overall three
-- different ways do deal with this tension:
--
-- 1. You can duplicate existing code and implement the new behavior in the
--    duplicate.
-- 2. You can infuse the new behavior into existing code and switch dynamically
--    at runtime (or via type class instance dispatch, which is the same
--    for our purposes)
-- 3. You can refactor the existing building blocks and compose them differently
--    at use sites to get the desired behavior nuances.
--
-- Of these three, steps 1. and 2. are by far the easiest to execute, while 3.
-- requires some critical thought. However, both 1. and 2. produce legacy code
-- that is difficult to maintain and understand.
--
-- As a guideline, if you find yourself wanting add new behavior to some of
-- these functions it's very likely that you should consider refactoring them
-- instead, thus shifting the responsibility deciding on the correct behavior to
-- use sites.
--
-- It an ongoing effort to adapt and refactor these building blocks such that
-- they have the sizes and shapes that result in the most elegant uses of them
-- that we can manage.
module Hasura.GraphQL.Schema.Build
  ( buildTableDeleteMutationFields,
    buildTableInsertMutationFields,
    buildTableQueryAndSubscriptionFields,
    buildTableStreamingSubscriptionFields,
    buildSingleBatchTableUpdateMutationFields,
    setFieldNameCase,
    buildFieldDescription,
  )
where

import Data.Has (getter)
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.GraphQL.ApolloFederation
import Hasura.GraphQL.Schema.Backend (BackendTableSelectSchema (..), BackendUpdateOperatorsSchema (..), MonadBuildSchema)
import Hasura.GraphQL.Schema.BoolExp (AggregationPredicatesSchema)
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Mutation
import Hasura.GraphQL.Schema.Parser hiding (EnumValueInfo, field)
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.SubscriptionStream (selectStreamTable)
import Hasura.GraphQL.Schema.Table (getTableIdentifierName, tableSelectPermissions)
import Hasura.GraphQL.Schema.Typename
import Hasura.GraphQL.Schema.Update.Batch (updateTable, updateTableByPk)
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Update.Batch
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

-- | buildTableQueryAndSubscriptionFields builds the field parsers of a table.
--   It returns a tuple with array of field parsers that correspond to the field
--   parsers of the query root and the field parsers of the subscription root
buildTableQueryAndSubscriptionFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b,
    BackendTableSelectSchema b
  ) =>
  MkRootFieldName ->
  TableName b ->
  TableInfo b ->
  C.GQLNameIdentifier ->
  SchemaT
    r
    m
    ( [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))],
      [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))],
      Maybe (G.Name, Parser 'Output n (ApolloFederationParserFunction n))
    )
buildTableQueryAndSubscriptionFields mkRootFieldName tableName tableInfo gqlName = do
  sourceInfo :: SourceInfo b <- asks getter
  roleName <- retrieve scRole
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      -- select table
      selectName = runMkRootFieldName mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfSelect mkSelectField gqlName
      -- select table by pk
      selectPKName = runMkRootFieldName mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfSelectByPk mkSelectByPkField gqlName
      -- select table aggregate
      selectAggName = runMkRootFieldName mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfSelectAggregate mkSelectAggregateField gqlName

  selectTableParser <- optionalFieldParser QDBMultipleRows $ selectTable tableInfo selectName selectDesc
  selectTableByPkParser <- optionalFieldParser QDBSingleRow $ selectTableByPk tableInfo selectPKName selectPKDesc
  selectTableAggregateParser <- optionalFieldParser QDBAggregation $ selectTableAggregate tableInfo selectAggName selectAggDesc

  case tableSelectPermissions roleName tableInfo of
    -- No select permission found for the current role, so
    -- no root fields will be accessible to the role
    Nothing -> pure (mempty, mempty, Nothing)
    -- Filter the root fields which have been enabled
    Just SelPermInfo {..} -> do
      selectStreamParser <-
        if (isRootFieldAllowed SRFTSelectStream spiAllowedSubscriptionRootFields)
          then buildTableStreamingSubscriptionFields mkRootFieldName tableName tableInfo gqlName
          else pure mempty

      let (querySelectTableParser, subscriptionSelectTableParser) =
            getQueryAndSubscriptionRootFields
              selectTableParser
              (isRootFieldAllowed QRFTSelect spiAllowedQueryRootFields)
              (isRootFieldAllowed SRFTSelect spiAllowedSubscriptionRootFields)

          (querySelectTableByPkParser, subscriptionSelectTableByPkParser) =
            getQueryAndSubscriptionRootFields
              selectTableByPkParser
              (isRootFieldAllowed QRFTSelectByPk spiAllowedQueryRootFields)
              (isRootFieldAllowed SRFTSelectByPk spiAllowedSubscriptionRootFields)

          (querySelectTableAggParser, subscriptionSelectTableAggParser) =
            getQueryAndSubscriptionRootFields
              selectTableAggregateParser
              (isRootFieldAllowed QRFTSelectAggregate spiAllowedQueryRootFields)
              (isRootFieldAllowed SRFTSelectAggregate spiAllowedSubscriptionRootFields)

          queryRootFields = catMaybes [querySelectTableParser, querySelectTableByPkParser, querySelectTableAggParser]
          subscriptionRootFields =
            selectStreamParser
              <> catMaybes [subscriptionSelectTableParser, subscriptionSelectTableByPkParser, subscriptionSelectTableAggParser]

      -- This parser is for generating apollo federation field _entities
      apolloFedTableParser <- runMaybeT do
        guard $ isApolloFedV1enabled (_tciApolloFederationConfig (_tiCoreInfo tableInfo))
        tableSelSet <- MaybeT $ tableSelectionSet tableInfo
        selectPerm <- hoistMaybe $ tableSelectPermissions roleName tableInfo
        stringifyNumbers <- retrieve Options.soStringifyNumbers
        primaryKeys <- hoistMaybe $ fmap _pkColumns . _tciPrimaryKey . _tiCoreInfo $ tableInfo
        tableGQLName <- getTableIdentifierName tableInfo
        let objectTypename = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableTypeName $ tableGQLName
        pure $ (objectTypename, convertToApolloFedParserFunc sourceInfo tableInfo selectPerm stringifyNumbers (Just tCase) primaryKeys tableSelSet)

      pure (queryRootFields, subscriptionRootFields, apolloFedTableParser)
  where
    selectDesc = buildFieldDescription defaultSelectDesc $ _crfComment _tcrfSelect
    selectPKDesc = buildFieldDescription defaultSelectPKDesc $ _crfComment _tcrfSelectByPk
    selectAggDesc = buildFieldDescription defaultSelectAggDesc $ _crfComment _tcrfSelectAggregate
    defaultSelectDesc = "fetch data from the table: " <>> tableName
    defaultSelectPKDesc = "fetch data from the table: " <> tableName <<> " using primary key columns"
    defaultSelectAggDesc = "fetch aggregated fields from the table: " <>> tableName
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

    -- This function checks if a root field is allowed to be exposed
    -- in the query root and a subscription root and when it is allowed,
    -- the parser will be returned.
    getQueryAndSubscriptionRootFields parser allowedInQuery allowedInSubscription =
      case (allowedInQuery, allowedInSubscription) of
        (True, True) -> (parser, parser)
        (True, False) -> (parser, Nothing)
        (False, True) -> (Nothing, parser)
        (False, False) -> (Nothing, Nothing)

buildTableStreamingSubscriptionFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b,
    BackendTableSelectSchema b
  ) =>
  MkRootFieldName ->
  TableName b ->
  TableInfo b ->
  C.GQLNameIdentifier ->
  SchemaT r m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableStreamingSubscriptionFields mkRootFieldName tableName tableInfo tableIdentifier = do
  -- Check in schema options whether we should include streaming subscription
  -- fields
  include <- retrieve Options.soIncludeStreamFields
  case include of
    Options.Don'tIncludeStreamFields -> pure mempty
    Options.IncludeStreamFields -> do
      sourceInfo :: SourceInfo b <- asks getter
      let customization = _siCustomization sourceInfo
          tCase = _rscNamingConvention customization
          customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
          selectDesc = Just $ G.Description $ "fetch data from the table in a streaming manner: " <>> tableName
          selectStreamName =
            runMkRootFieldName mkRootFieldName
              $ setFieldNameCase tCase tableInfo (_tcrfSelectStream customRootFields) mkSelectStreamField tableIdentifier
      catMaybes
        <$> sequenceA
          [ optionalFieldParser QDBStreamMultipleRows $ selectStreamTable tableInfo selectStreamName selectDesc
          ]

buildTableInsertMutationFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b
  ) =>
  (TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (UnpreparedValue b)))) ->
  MkRootFieldName ->
  Scenario ->
  TableName b ->
  TableInfo b ->
  C.GQLNameIdentifier ->
  SchemaT r m [FieldParser n (AnnotatedInsert b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableInsertMutationFields backendInsertAction mkRootFieldName scenario tableName tableInfo gqlName = do
  sourceInfo :: SourceInfo b <- asks getter
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      -- insert in table
      insertName = runMkRootFieldName mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfInsert mkInsertField gqlName
      -- insert one in table
      insertOneName = runMkRootFieldName mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfInsertOne mkInsertOneField gqlName

  insert <- insertIntoTable backendInsertAction scenario tableInfo insertName insertDesc
  -- Select permissions are required for insertOne: the selection set is the
  -- same as a select on that table, and it therefore can't be populated if the
  -- user doesn't have select permissions.
  insertOne <- insertOneIntoTable backendInsertAction scenario tableInfo insertOneName insertOneDesc
  pure $ catMaybes [insert, insertOne]
  where
    insertDesc = buildFieldDescription defaultInsertDesc $ _crfComment _tcrfInsert
    insertOneDesc = buildFieldDescription defaultInsertOneDesc $ _crfComment _tcrfInsertOne
    defaultInsertDesc = "insert data into the table: " <>> tableName
    defaultInsertOneDesc = "insert a single row into the table: " <>> tableName
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

-- | This function implements the parsers for the basic, single batch, update mutations. It
-- implements the mutation schema in the general shape described in
-- @https://hasura.io/docs/latest/graphql/core/databases/postgres/mutations/update.html@.
-- (ie. update_<table> and update_<table>_by_pk root fields)
--
-- Different backends can have different update types (single batch, multiple batches, etc),
-- and so the parsed UpdateBatch needs to be embedded in the custom UpdateVariant defined
-- by the backend, which is done by passing a function to this function.
buildSingleBatchTableUpdateMutationFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b,
    BackendTableSelectSchema b,
    BackendUpdateOperatorsSchema b
  ) =>
  -- | Embed the UpdateBack in the backend-specific UpdateVariant
  (UpdateBatch b (UpdateOperators b) (UnpreparedValue b) -> UpdateVariant b (UnpreparedValue b)) ->
  Scenario ->
  -- | table info
  TableInfo b ->
  -- | field display name
  C.GQLNameIdentifier ->
  SchemaT r m [FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildSingleBatchTableUpdateMutationFields mkSingleBatchUpdateVariant scenario tableInfo gqlName = do
  update <- updateTable mkSingleBatchUpdateVariant scenario tableInfo gqlName
  updateByPk <- updateTableByPk mkSingleBatchUpdateVariant scenario tableInfo gqlName
  pure $ catMaybes [update, updateByPk]

buildTableDeleteMutationFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b,
    BackendTableSelectSchema b
  ) =>
  MkRootFieldName ->
  Scenario ->
  TableName b ->
  TableInfo b ->
  C.GQLNameIdentifier ->
  SchemaT r m [FieldParser n (AnnDelG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableDeleteMutationFields mkRootFieldName scenario tableName tableInfo gqlName = do
  sourceInfo :: SourceInfo b <- asks getter
  let customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      -- delete from table
      deleteName = runMkRootFieldName mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfDelete mkDeleteField gqlName
      -- delete from table by pk
      deletePKName = runMkRootFieldName mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfDeleteByPk mkDeleteByPkField gqlName

  delete <- deleteFromTable scenario tableInfo deleteName deleteDesc
  -- Primary keys can only be tested in the `where` clause if the user has
  -- select permissions for them, which at the very least requires select
  -- permissions.
  deleteByPk <- deleteFromTableByPk scenario tableInfo deletePKName deletePKDesc
  pure $ catMaybes [delete, deleteByPk]
  where
    deleteDesc = buildFieldDescription defaultDeleteDesc $ _crfComment _tcrfDelete
    deletePKDesc = buildFieldDescription defaultDeletePKDesc $ _crfComment _tcrfDeleteByPk
    defaultDeleteDesc = "delete data from the table: " <>> tableName
    defaultDeletePKDesc = "delete single row from the table: " <>> tableName
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

buildFieldDescription :: Text -> Comment -> Maybe G.Description
buildFieldDescription defaultDescription = \case
  Automatic -> Just $ G.Description defaultDescription
  Explicit comment -> G.Description . toTxt <$> comment
