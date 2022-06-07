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
  ( buildFunctionMutationFieldsPG,
    buildFunctionQueryFieldsPG,
    buildTableDeleteMutationFields,
    buildTableInsertMutationFields,
    buildTableQueryAndSubscriptionFields,
    buildTableStreamingSubscriptionFields,
    buildTableUpdateMutationFields,
  )
where

import Data.Has (getter)
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.GraphQL.Parser hiding (EnumValueInfo, field)
import Hasura.GraphQL.Schema.Backend (MonadBuildSchema)
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Mutation
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.SubscriptionStream (selectStreamTable)
import Hasura.GraphQL.Schema.Table (tableSelectPermissions)
import Hasura.GraphQL.Schema.Update (updateTable, updateTableByPk)
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend
import Hasura.Server.Types (StreamingSubscriptionsCtx (..))
import Language.GraphQL.Draft.Syntax qualified as G

-- | Builds field name with proper case. Please note that this is a pure
--   function as all the validation has already been done while preparing
--   @GQLNameIdentifier@.
setFieldNameCase ::
  NamingCase ->
  TableInfo b ->
  CustomRootField ->
  (C.GQLNameIdentifier -> C.GQLNameIdentifier) ->
  C.GQLNameIdentifier ->
  G.Name
setFieldNameCase tCase tInfo crf getFieldName tableName =
  (applyFieldNameCaseIdentifier tCase fieldIdentifier)
  where
    tccName = fmap (`C.Identifier` []) . _tcCustomName . _tciCustomConfig . _tiCoreInfo $ tInfo
    crfName = fmap (`C.Identifier` []) (_crfName crf)
    fieldIdentifier = fromMaybe (getFieldName (fromMaybe tableName tccName)) crfName

-- | buildTableQueryAndSubscriptionFields builds the field parsers of a table.
--   It returns a tuple with array of field parsers that correspond to the field
--   parsers of the query root and the field parsers of the subscription root
buildTableQueryAndSubscriptionFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableName b ->
  TableInfo b ->
  StreamingSubscriptionsCtx ->
  C.GQLNameIdentifier ->
  m
    ( [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))],
      [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
    )
buildTableQueryAndSubscriptionFields sourceInfo tableName tableInfo streamSubCtx gqlName = do
  tCase <- asks getter
  -- select table
  selectName <- mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfSelect mkSelectField gqlName
  -- select table by pk
  selectPKName <- mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfSelectByPk mkSelectByPkField gqlName
  -- select table aggregate
  selectAggName <- mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfSelectAggregate mkSelectAggregateField gqlName

  selectPermission <- tableSelectPermissions tableInfo

  selectTableParser <- optionalFieldParser QDBMultipleRows $ selectTable sourceInfo tableInfo selectName selectDesc
  selectTableByPkParser <- optionalFieldParser QDBSingleRow $ selectTableByPk sourceInfo tableInfo selectPKName selectPKDesc
  selectTableAggregateParser <- optionalFieldParser QDBAggregation $ selectTableAggregate sourceInfo tableInfo selectAggName selectAggDesc

  case selectPermission of
    -- No select permission found for the current role, so
    -- no root fields will be accessible to the role
    Nothing -> pure (mempty, mempty)
    -- Filter the root fields which have been enabled
    Just SelPermInfo {..} -> do
      selectStreamParser <-
        if (isRootFieldAllowed SRFTSelectStream spiAllowedSubscriptionRootFields && streamSubCtx == StreamingSubscriptionsEnabled)
          then buildTableStreamingSubscriptionFields sourceInfo tableName tableInfo gqlName
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

      pure (queryRootFields, subscriptionRootFields)
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
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  TableName b ->
  TableInfo b ->
  C.GQLNameIdentifier ->
  m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableStreamingSubscriptionFields sourceInfo tableName tableInfo tableIdentifier = do
  tCase <- asks getter
  let customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
      selectDesc = Just $ G.Description $ "fetch data from the table in a streaming manner : " <>> tableName
  selectStreamName <-
    mkRootFieldName $ setFieldNameCase tCase tableInfo (_tcrfSelect customRootFields) mkSelectStreamField tableIdentifier
  catMaybes
    <$> sequenceA
      [ optionalFieldParser QDBStreamMultipleRows $ selectStreamTable sourceInfo tableInfo selectStreamName selectDesc
      ]

buildTableInsertMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  (SourceInfo b -> TableInfo b -> m (InputFieldsParser n (BackendInsert b (UnpreparedValue b)))) ->
  Scenario ->
  SourceInfo b ->
  TableName b ->
  TableInfo b ->
  C.GQLNameIdentifier ->
  m [FieldParser n (AnnotatedInsert b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableInsertMutationFields backendInsertAction scenario sourceInfo tableName tableInfo gqlName = do
  tCase <- asks getter
  -- insert in table
  insertName <- mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfInsert mkInsertField gqlName
  -- insert one in table
  insertOneName <- mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfInsertOne mkInsertOneField gqlName
  insert <- insertIntoTable backendInsertAction scenario sourceInfo tableInfo insertName insertDesc
  -- Select permissions are required for insertOne: the selection set is the
  -- same as a select on that table, and it therefore can't be populated if the
  -- user doesn't have select permissions.
  insertOne <- insertOneIntoTable backendInsertAction scenario sourceInfo tableInfo insertOneName insertOneDesc
  pure $ catMaybes [insert, insertOne]
  where
    insertDesc = buildFieldDescription defaultInsertDesc $ _crfComment _tcrfInsert
    insertOneDesc = buildFieldDescription defaultInsertOneDesc $ _crfComment _tcrfInsertOne
    defaultInsertDesc = "insert data into the table: " <>> tableName
    defaultInsertOneDesc = "insert a single row into the table: " <>> tableName
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

-- | This function is the basic building block for update mutations. It
-- implements the mutation schema in the general shape described in
-- @https://hasura.io/docs/latest/graphql/core/databases/postgres/mutations/update.html@.
--
-- Something that varies between backends is the @update operators@ that they
-- support (i.e. the schema fields @_set@, @_inc@, etc., see
-- <src/Hasura.Backends.Postgres.Instances.Schema.html#updateOperators Hasura.Backends.Postgres.Instances.Schema.updateOperators> for an example
-- implementation). Therefore, this function is parameterised over a monadic
-- action that produces the operators that the backend supports in the context
-- of some table and associated update permissions.
--
-- Apart from this detail, the rest of the arguments are the same as those
-- of @BackendSchema.@'Hasura.GraphQL.Schema.Backend.buildTableUpdateMutationFields'.
--
-- The suggested way to use this is like:
--
-- > instance BackendSchema MyBackend where
-- >   ...
-- >   buildTableUpdateMutationFields = GSB.buildTableUpdateMutationFields myBackendUpdateOperators
-- >   ...
buildTableUpdateMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  -- | an action that builds @BackendUpdate@ with the
  -- backend-specific data needed to perform an update mutation
  ( TableInfo b ->
    m
      (InputFieldsParser n (BackendUpdate b (UnpreparedValue b)))
  ) ->
  Scenario ->
  -- | The source that the table lives in
  SourceInfo b ->
  -- | The name of the table being acted on
  TableName b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  C.GQLNameIdentifier ->
  m [FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableUpdateMutationFields mkBackendUpdate scenario sourceInfo tableName tableInfo gqlName = do
  tCase <- asks getter
  backendUpdate <- mkBackendUpdate tableInfo
  -- update table
  updateName <- mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfUpdate mkUpdateField gqlName
  -- update table by pk
  updatePKName <- mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfUpdateByPk mkUpdateByPkField gqlName
  update <- updateTable backendUpdate scenario sourceInfo tableInfo updateName updateDesc
  -- Primary keys can only be tested in the `where` clause if a primary key
  -- exists on the table and if the user has select permissions on all columns
  -- that make up the key.
  updateByPk <- updateTableByPk backendUpdate scenario sourceInfo tableInfo updatePKName updatePKDesc
  pure $ catMaybes [update, updateByPk]
  where
    updateDesc = buildFieldDescription defaultUpdateDesc $ _crfComment _tcrfUpdate
    updatePKDesc = buildFieldDescription defaultUpdatePKDesc $ _crfComment _tcrfUpdateByPk
    defaultUpdateDesc = "update data of the table: " <>> tableName
    defaultUpdatePKDesc = "update single row of the table: " <>> tableName
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

buildTableDeleteMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  Scenario ->
  SourceInfo b ->
  TableName b ->
  TableInfo b ->
  C.GQLNameIdentifier ->
  m [FieldParser n (AnnDelG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableDeleteMutationFields scenario sourceInfo tableName tableInfo gqlName = do
  tCase <- asks getter
  -- delete from table
  deleteName <- mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfDelete mkDeleteField gqlName
  -- delete from table by pk
  deletePKName <- mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfDeleteByPk mkDeleteByPkField gqlName
  delete <- deleteFromTable scenario sourceInfo tableInfo deleteName deleteDesc
  -- Primary keys can only be tested in the `where` clause if the user has
  -- select permissions for them, which at the very least requires select
  -- permissions.
  deleteByPk <- deleteFromTableByPk scenario sourceInfo tableInfo deletePKName deletePKDesc
  pure $ catMaybes [delete, deleteByPk]
  where
    deleteDesc = buildFieldDescription defaultDeleteDesc $ _crfComment _tcrfDelete
    deletePKDesc = buildFieldDescription defaultDeletePKDesc $ _crfComment _tcrfDeleteByPk
    defaultDeleteDesc = "delete data from the table: " <>> tableName
    defaultDeletePKDesc = "delete single row from the table: " <>> tableName
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

buildFunctionQueryFieldsPG ::
  forall r m n pgKind.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceInfo ('Postgres pgKind) ->
  FunctionName ('Postgres pgKind) ->
  FunctionInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  m [FieldParser n (QueryDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
buildFunctionQueryFieldsPG sourceInfo functionName functionInfo tableName = do
  let -- select function
      funcDesc =
        Just . G.Description $
          flip fromMaybe (_fiComment functionInfo) $ "execute function " <> functionName <<> " which returns " <>> tableName
      -- select function agg

      funcAggDesc = Just $ G.Description $ "execute function " <> functionName <<> " and query aggregates on result of table type " <>> tableName

      queryResultType =
        case _fiJsonAggSelect functionInfo of
          JASMultipleRows -> QDBMultipleRows
          JASSingleObject -> QDBSingleRow

  catMaybes
    <$> sequenceA
      [ optionalFieldParser (queryResultType) $ selectFunction sourceInfo functionInfo funcDesc,
        optionalFieldParser (QDBAggregation) $ selectFunctionAggregate sourceInfo functionInfo funcAggDesc
      ]

buildFunctionMutationFieldsPG ::
  forall r m n pgKind.
  MonadBuildSchema ('Postgres pgKind) r m n =>
  SourceInfo ('Postgres pgKind) ->
  FunctionName ('Postgres pgKind) ->
  FunctionInfo ('Postgres pgKind) ->
  TableName ('Postgres pgKind) ->
  m [FieldParser n (MutationDB ('Postgres pgKind) (RemoteRelationshipField UnpreparedValue) (UnpreparedValue ('Postgres pgKind)))]
buildFunctionMutationFieldsPG sourceInfo functionName functionInfo tableName = do
  let funcDesc = Just $ G.Description $ "execute VOLATILE function " <> functionName <<> " which returns " <>> tableName
      jsonAggSelect = _fiJsonAggSelect functionInfo
  catMaybes
    <$> sequenceA
      [ optionalFieldParser (MDBFunction jsonAggSelect) $ selectFunction sourceInfo functionInfo funcDesc
      -- TODO: do we want aggregate mutation functions?
      ]

buildFieldDescription :: Text -> Comment -> Maybe G.Description
buildFieldDescription defaultDescription = \case
  Automatic -> Just $ G.Description defaultDescription
  Explicit comment -> G.Description . toTxt <$> comment
