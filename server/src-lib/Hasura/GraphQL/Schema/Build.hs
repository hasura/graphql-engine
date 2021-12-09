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
  ( buildFunctionMutationFields,
    buildFunctionQueryFields,
    buildTableDeleteMutationFields,
    buildTableInsertMutationFields,
    buildTableQueryFields,
    buildTableUpdateMutationFields,
  )
where

import Data.Text.Extended
import Hasura.GraphQL.Parser hiding (EnumValueInfo, field)
import Hasura.GraphQL.Schema.Backend (MonadBuildSchema)
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Mutation
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Update (updateTable, updateTableByPk)
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

buildTableQueryFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableName b ->
  TableInfo b ->
  G.Name ->
  SelPermInfo b ->
  (m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))])
buildTableQueryFields sourceName tableName tableInfo gqlName selPerms = do
  let customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
      -- select table
      selectDesc = Just $ G.Description $ "fetch data from the table: " <>> tableName
      -- select table by pk
      selectPKDesc = Just $ G.Description $ "fetch data from the table: " <> tableName <<> " using primary key columns"
      -- select table aggregate
      selectAggDesc = Just $ G.Description $ "fetch aggregated fields from the table: " <>> tableName
  selectName <- mkRootFieldName $ fromMaybe gqlName $ _tcrfSelect customRootFields
  selectPKName <- mkRootFieldName $ fromMaybe (gqlName <> $$(G.litName "_by_pk")) $ _tcrfSelectByPk customRootFields
  selectAggName <- mkRootFieldName $ fromMaybe (gqlName <> $$(G.litName "_aggregate")) $ _tcrfSelectAggregate customRootFields
  catMaybes
    <$> sequenceA
      [ requiredFieldParser QDBMultipleRows $ selectTable sourceName tableInfo selectName selectDesc selPerms,
        optionalFieldParser QDBSingleRow $ selectTableByPk sourceName tableInfo selectPKName selectPKDesc selPerms,
        optionalFieldParser QDBAggregation $ selectTableAggregate sourceName tableInfo selectAggName selectAggDesc selPerms
      ]

buildTableInsertMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  (SourceName -> TableInfo b -> Maybe (SelPermInfo b) -> Maybe (UpdPermInfo b) -> m (InputFieldsParser n (BackendInsert b (UnpreparedValue b)))) ->
  SourceName ->
  TableName b ->
  TableInfo b ->
  G.Name ->
  InsPermInfo b ->
  Maybe (SelPermInfo b) ->
  Maybe (UpdPermInfo b) ->
  m [FieldParser n (AnnInsert b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableInsertMutationFields
  backendInsertAction
  sourceName
  tableName
  tableInfo
  gqlName
  insPerms
  mSelPerms
  mUpdPerms =
    do
      let customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
          -- insert into table
          insertDesc = Just $ G.Description $ "insert data into the table: " <>> tableName
          -- insert one into table
          insertOneDesc = Just $ G.Description $ "insert a single row into the table: " <>> tableName
      insertName <- mkRootFieldName $ fromMaybe ($$(G.litName "insert_") <> gqlName) $ _tcrfInsert customRootFields
      insertOneName <- mkRootFieldName $ fromMaybe ($$(G.litName "insert_") <> gqlName <> $$(G.litName "_one")) $ _tcrfInsertOne customRootFields
      insert <- insertIntoTable backendInsertAction sourceName tableInfo insertName insertDesc insPerms mSelPerms mUpdPerms
      -- Select permissions are required for insertOne: the selection set is the
      -- same as a select on that table, and it therefore can't be populated if the
      -- user doesn't have select permissions.
      insertOne <- for mSelPerms \selPerms ->
        insertOneIntoTable backendInsertAction sourceName tableInfo insertOneName insertOneDesc insPerms selPerms mUpdPerms
      pure $ insert : maybeToList insertOne

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
    UpdPermInfo b ->
    m
      (InputFieldsParser n (BackendUpdate b (UnpreparedValue b)))
  ) ->
  -- | The source that the table lives in
  SourceName ->
  -- | The name of the table being acted on
  TableName b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | update permissions of the table
  UpdPermInfo b ->
  -- | select permissions of the table (if any)
  Maybe (SelPermInfo b) ->
  m [FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableUpdateMutationFields mkBackendUpdate sourceName tableName tableInfo gqlName updPerms mSelPerms = do
  let customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
      -- update table
      updateDesc = Just $ G.Description $ "update data of the table: " <>> tableName
      -- update table by pk
      updatePKDesc = Just $ G.Description $ "update single row of the table: " <>> tableName
  backendUpdate <- mkBackendUpdate tableInfo updPerms
  updateName <- mkRootFieldName $ fromMaybe ($$(G.litName "update_") <> gqlName) $ _tcrfUpdate customRootFields
  updatePKName <- mkRootFieldName $ fromMaybe ($$(G.litName "update_") <> gqlName <> $$(G.litName "_by_pk")) $ _tcrfUpdateByPk customRootFields
  update <- updateTable backendUpdate sourceName tableInfo updateName updateDesc updPerms mSelPerms
  -- Primary keys can only be tested in the `where` clause if a primary key
  -- exists on the table and if the user has select permissions on all columns
  -- that make up the key.
  updateByPk <- fmap join $ for mSelPerms $ updateTableByPk backendUpdate sourceName tableInfo updatePKName updatePKDesc updPerms
  pure $ update : catMaybes [updateByPk]

buildTableDeleteMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableName b ->
  TableInfo b ->
  G.Name ->
  DelPermInfo b ->
  Maybe (SelPermInfo b) ->
  m [FieldParser n (AnnDelG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableDeleteMutationFields sourceName tableName tableInfo gqlName delPerms mSelPerms = do
  let customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
      -- delete from table
      deleteDesc = Just $ G.Description $ "delete data from the table: " <>> tableName
      -- delete from table by pk
      deletePKDesc = Just $ G.Description $ "delete single row from the table: " <>> tableName
  deleteName <- mkRootFieldName $ fromMaybe ($$(G.litName "delete_") <> gqlName) $ _tcrfDelete customRootFields
  deletePKName <- mkRootFieldName $ fromMaybe ($$(G.litName "delete_") <> gqlName <> $$(G.litName "_by_pk")) $ _tcrfDeleteByPk customRootFields
  delete <- deleteFromTable sourceName tableInfo deleteName deleteDesc delPerms mSelPerms
  -- Primary keys can only be tested in the `where` clause if the user has
  -- select permissions for them, which at the very least requires select
  -- permissions.
  deleteByPk <- fmap join $ for mSelPerms $ deleteFromTableByPk sourceName tableInfo deletePKName deletePKDesc delPerms
  pure $ delete : maybeToList deleteByPk

buildFunctionQueryFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  FunctionName b ->
  FunctionInfo b ->
  TableName b ->
  SelPermInfo b ->
  m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildFunctionQueryFields sourceName functionName functionInfo tableName selPerms = do
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
      [ requiredFieldParser (queryResultType) $ selectFunction sourceName functionInfo funcDesc selPerms,
        optionalFieldParser (QDBAggregation) $ selectFunctionAggregate sourceName functionInfo funcAggDesc selPerms
      ]

buildFunctionMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  FunctionName b ->
  FunctionInfo b ->
  TableName b ->
  SelPermInfo b ->
  m [FieldParser n (MutationDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildFunctionMutationFields sourceName functionName functionInfo tableName selPerms = do
  let funcDesc = Just $ G.Description $ "execute VOLATILE function " <> functionName <<> " which returns " <>> tableName

      jsonAggSelect = _fiJsonAggSelect functionInfo
  catMaybes
    <$> sequenceA
      [ requiredFieldParser (MDBFunction jsonAggSelect) $ selectFunction sourceName functionInfo funcDesc selPerms
      -- TODO: do we want aggregate mutation functions?
      ]
