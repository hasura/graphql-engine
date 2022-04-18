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
import Hasura.GraphQL.Parser.Constants qualified as G
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
  (m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))])
buildTableQueryFields sourceName tableName tableInfo gqlName = do
  -- select table
  selectName <- mkRootFieldName . fromMaybe gqlName $ _crfName _tcrfSelect
  -- select table by pk
  selectPKName <- mkRootFieldName . fromMaybe (gqlName <> G.__by_pk) $ _crfName _tcrfSelectByPk
  -- select table aggregate
  selectAggName <- mkRootFieldName . fromMaybe (gqlName <> G.__aggregate) $ _crfName _tcrfSelectAggregate
  catMaybes
    <$> sequenceA
      [ optionalFieldParser QDBMultipleRows $ selectTable sourceName tableInfo selectName selectDesc,
        optionalFieldParser QDBSingleRow $ selectTableByPk sourceName tableInfo selectPKName selectPKDesc,
        optionalFieldParser QDBAggregation $ selectTableAggregate sourceName tableInfo selectAggName selectAggDesc
      ]
  where
    selectDesc = buildFieldDescription defaultSelectDesc $ _crfComment _tcrfSelect
    selectPKDesc = buildFieldDescription defaultSelectPKDesc $ _crfComment _tcrfSelectByPk
    selectAggDesc = buildFieldDescription defaultSelectAggDesc $ _crfComment _tcrfSelectAggregate
    defaultSelectDesc = "fetch data from the table: " <>> tableName
    defaultSelectPKDesc = "fetch data from the table: " <> tableName <<> " using primary key columns"
    defaultSelectAggDesc = "fetch aggregated fields from the table: " <>> tableName
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

buildTableInsertMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  (SourceName -> TableInfo b -> m (InputFieldsParser n (BackendInsert b (UnpreparedValue b)))) ->
  Scenario ->
  SourceName ->
  TableName b ->
  TableInfo b ->
  G.Name ->
  m [FieldParser n (AnnotatedInsert b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableInsertMutationFields backendInsertAction scenario sourceName tableName tableInfo gqlName = do
  insertName <- mkRootFieldName . fromMaybe (G._insert_ <> gqlName) $ _crfName _tcrfInsert
  insertOneName <- mkRootFieldName . fromMaybe (G._insert_ <> gqlName <> G.__one) $ _crfName _tcrfInsertOne
  insert <- insertIntoTable backendInsertAction scenario sourceName tableInfo insertName insertDesc
  -- Select permissions are required for insertOne: the selection set is the
  -- same as a select on that table, and it therefore can't be populated if the
  -- user doesn't have select permissions.
  insertOne <- insertOneIntoTable backendInsertAction scenario sourceName tableInfo insertOneName insertOneDesc
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
  -- | The source that the table lives in
  SourceName ->
  -- | The name of the table being acted on
  TableName b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  m [FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableUpdateMutationFields mkBackendUpdate sourceName tableName tableInfo gqlName = do
  let _viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
  backendUpdate <- mkBackendUpdate tableInfo
  -- update table
  updateName <- mkRootFieldName . fromMaybe (G._update_ <> gqlName) $ _crfName _tcrfUpdate
  -- update table by pk
  updatePKName <- mkRootFieldName . fromMaybe (G._update_ <> gqlName <> G.__by_pk) $ _crfName _tcrfUpdateByPk
  update <- updateTable backendUpdate sourceName tableInfo updateName updateDesc
  -- Primary keys can only be tested in the `where` clause if a primary key
  -- exists on the table and if the user has select permissions on all columns
  -- that make up the key.
  updateByPk <- updateTableByPk backendUpdate sourceName tableInfo updatePKName updatePKDesc
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
  SourceName ->
  TableName b ->
  TableInfo b ->
  G.Name ->
  m [FieldParser n (AnnDelG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildTableDeleteMutationFields sourceName tableName tableInfo gqlName = do
  -- delete from table
  deleteName <- mkRootFieldName . fromMaybe (G._delete_ <> gqlName) $ _crfName _tcrfDelete
  -- delete from table by pk
  deletePKName <- mkRootFieldName . fromMaybe (G._delete_ <> gqlName <> G.__by_pk) $ _crfName _tcrfDeleteByPk
  delete <- deleteFromTable sourceName tableInfo deleteName deleteDesc
  -- Primary keys can only be tested in the `where` clause if the user has
  -- select permissions for them, which at the very least requires select
  -- permissions.
  deleteByPk <- deleteFromTableByPk sourceName tableInfo deletePKName deletePKDesc
  pure $ catMaybes [delete, deleteByPk]
  where
    deleteDesc = buildFieldDescription defaultDeleteDesc $ _crfComment _tcrfDelete
    deletePKDesc = buildFieldDescription defaultDeletePKDesc $ _crfComment _tcrfDeleteByPk
    defaultDeleteDesc = "delete data from the table: " <>> tableName
    defaultDeletePKDesc = "delete single row from the table: " <>> tableName
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

buildFunctionQueryFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  FunctionName b ->
  FunctionInfo b ->
  TableName b ->
  m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildFunctionQueryFields sourceName functionName functionInfo tableName = do
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
      [ optionalFieldParser (queryResultType) $ selectFunction sourceName functionInfo funcDesc,
        optionalFieldParser (QDBAggregation) $ selectFunctionAggregate sourceName functionInfo funcAggDesc
      ]

buildFunctionMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  FunctionName b ->
  FunctionInfo b ->
  TableName b ->
  m [FieldParser n (MutationDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
buildFunctionMutationFields sourceName functionName functionInfo tableName = do
  let funcDesc = Just $ G.Description $ "execute VOLATILE function " <> functionName <<> " which returns " <>> tableName

      jsonAggSelect = _fiJsonAggSelect functionInfo
  catMaybes
    <$> sequenceA
      [ optionalFieldParser (MDBFunction jsonAggSelect) $ selectFunction sourceName functionInfo funcDesc
      -- TODO: do we want aggregate mutation functions?
      ]

buildFieldDescription :: Text -> Comment -> Maybe G.Description
buildFieldDescription defaultDescription = \case
  Automatic -> Just $ G.Description defaultDescription
  Explicit comment -> G.Description . toTxt <$> comment
