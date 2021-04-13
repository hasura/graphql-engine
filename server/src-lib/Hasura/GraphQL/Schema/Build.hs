-- | Top-level schema building function. Those are used to construct a schema for a given resource,
-- such as a table or a function. Those matches what the @BackendSchema@ class expects, and are the
-- default implementation a fully-fledged backend should support.

module Hasura.GraphQL.Schema.Build where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax  as G

import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend          as AB

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser          hiding (EnumValueInfo, field)
import           Hasura.GraphQL.Schema.Backend  (MonadBuildSchema)
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Mutation
import           Hasura.GraphQL.Schema.Select
import           Hasura.RQL.Types


buildTableQueryFields
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> SourceConfig b
  -> TableName b
  -> TableInfo b
  -> G.Name
  -> SelPermInfo b
  -> m [FieldParser n (QueryRootField UnpreparedValue)]
buildTableQueryFields sourceName sourceInfo tableName tableInfo gqlName selPerms = do
  let
    mkRF = RFDB sourceName
             . AB.mkAnyBackend
             . SourceConfigWith sourceInfo
             . QDBR
    customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
    -- select table
    selectName = fromMaybe gqlName $ _tcrfSelect customRootFields
    selectDesc = Just $ G.Description $ "fetch data from the table: " <>> tableName
    -- select table by pk
    selectPKName = fromMaybe (gqlName <> $$(G.litName "_by_pk")) $ _tcrfSelectByPk customRootFields
    selectPKDesc = Just $ G.Description $ "fetch data from the table: " <> tableName <<> " using primary key columns"
    -- select table aggregate
    selectAggName = fromMaybe (gqlName <> $$(G.litName "_aggregate")) $ _tcrfSelectAggregate customRootFields
    selectAggDesc = Just $ G.Description $ "fetch aggregated fields from the table: " <>> tableName
  catMaybes <$> sequenceA
    [ requiredFieldParser (mkRF . QDBMultipleRows) $ selectTable          tableName selectName    selectDesc    selPerms
    , optionalFieldParser (mkRF . QDBSingleRow)    $ selectTableByPk      tableName selectPKName  selectPKDesc  selPerms
    , optionalFieldParser (mkRF . QDBAggregation)  $ selectTableAggregate tableName selectAggName selectAggDesc selPerms
    ]

buildTableInsertMutationFields
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> SourceConfig b
  -> TableName b
  -> TableInfo b
  -> G.Name
  -> InsPermInfo b
  -> Maybe (SelPermInfo b)
  -> Maybe (UpdPermInfo b)
  -> m [FieldParser n (MutationRootField UnpreparedValue)]
buildTableInsertMutationFields sourceName sourceInfo tableName tableInfo gqlName insPerms mSelPerms mUpdPerms = do
  let
    mkRF = RFDB sourceName
             . AB.mkAnyBackend
             . SourceConfigWith sourceInfo
             . MDBR
    customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
    -- insert into table
    insertName = fromMaybe ($$(G.litName "insert_") <> gqlName) $ _tcrfInsert customRootFields
    insertDesc = Just $ G.Description $ "insert data into the table: " <>> tableName
    -- insert one into table
    insertOneName = fromMaybe ($$(G.litName "insert_") <> gqlName <> $$(G.litName "_one")) $ _tcrfInsertOne customRootFields
    insertOneDesc = Just $ G.Description $ "insert a single row into the table: " <>> tableName
  insert <- insertIntoTable tableName insertName insertDesc insPerms mSelPerms mUpdPerms
  -- Select permissions are required for insertOne: the selection set is the
  -- same as a select on that table, and it therefore can't be populated if the
  -- user doesn't have select permissions.
  insertOne <- for mSelPerms \selPerms ->
    insertOneIntoTable tableName insertOneName insertOneDesc insPerms selPerms mUpdPerms
  pure $ fmap (mkRF . MDBInsert) <$> insert : maybeToList insertOne

buildTableUpdateMutationFields
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> SourceConfig b
  -> TableName b
  -> TableInfo b
  -> G.Name
  -> UpdPermInfo b
  -> Maybe (SelPermInfo b)
  -> m [FieldParser n (MutationRootField UnpreparedValue)]
buildTableUpdateMutationFields sourceName sourceInfo tableName tableInfo gqlName updPerms mSelPerms = do
  let
    mkRF = RFDB sourceName
             . AB.mkAnyBackend
             . SourceConfigWith sourceInfo
             . MDBR
    customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
    -- update table
    updateName = fromMaybe ($$(G.litName "update_") <> gqlName) $ _tcrfUpdate customRootFields
    updateDesc = Just $ G.Description $ "update data of the table: " <>> tableName
    -- update table by pk
    updatePKName = fromMaybe ($$(G.litName "update_") <> gqlName <> $$(G.litName "_by_pk")) $ _tcrfUpdateByPk customRootFields
    updatePKDesc = Just $ G.Description $ "update single row of the table: " <>> tableName
  update <- updateTable tableName updateName updateDesc updPerms mSelPerms
  -- Primary keys can only be tested in the `where` clause if the user has
  -- select permissions for them, which at the very least requires select
  -- permissions.
  updateByPk <- fmap join $ for mSelPerms $ updateTableByPk tableName updatePKName updatePKDesc updPerms
  pure $ fmap (mkRF . MDBUpdate) <$> catMaybes [update, updateByPk]

buildTableDeleteMutationFields
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> SourceConfig b
  -> TableName b
  -> TableInfo b
  -> G.Name
  -> DelPermInfo b
  -> Maybe (SelPermInfo b)
  -> m [FieldParser n (MutationRootField UnpreparedValue)]
buildTableDeleteMutationFields sourceName sourceInfo tableName tableInfo gqlName delPerms mSelPerms = do
  let
    mkRF = RFDB sourceName
             . AB.mkAnyBackend
             . SourceConfigWith sourceInfo
             . MDBR
    customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
    -- delete from table
    deleteName = fromMaybe ($$(G.litName "delete_") <> gqlName) $ _tcrfDelete customRootFields
    deleteDesc = Just $ G.Description $ "delete data from the table: " <>> tableName
    -- delete from table by pk
    deletePKName = fromMaybe ($$(G.litName "delete_") <> gqlName <> $$(G.litName "_by_pk")) $ _tcrfDeleteByPk customRootFields
    deletePKDesc = Just $ G.Description $ "delete single row from the table: " <>> tableName
  delete <- deleteFromTable tableName deleteName deleteDesc delPerms mSelPerms
  -- Primary keys can only be tested in the `where` clause if the user has
  -- select permissions for them, which at the very least requires select
  -- permissions.
  deleteByPk <- fmap join $ for mSelPerms $ deleteFromTableByPk tableName deletePKName deletePKDesc delPerms
  pure $ fmap (mkRF . MDBDelete) <$> delete : maybeToList deleteByPk

buildFunctionQueryFields
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> SourceConfig b
  -> FunctionName b
  -> FunctionInfo b
  -> TableName b
  -> SelPermInfo b
  -> m [FieldParser n (QueryRootField UnpreparedValue)]
buildFunctionQueryFields sourceName sourceInfo functionName functionInfo tableName selPerms = do
  funcName <- functionGraphQLName @b functionName `onLeft` throwError
  let
    mkRF = RFDB sourceName
             . AB.mkAnyBackend
             . SourceConfigWith sourceInfo
             . QDBR
    -- select function
    funcDesc = Just $ G.Description $ "execute function " <> functionName <<> " which returns " <>> tableName
    -- select function agg
    funcAggName = funcName <> $$(G.litName "_aggregate")
    funcAggDesc = Just $ G.Description $ "execute function " <> functionName <<> " and query aggregates on result of table type " <>> tableName
    queryResultType =
      case _fiJsonAggSelect functionInfo of
        JASMultipleRows -> QDBMultipleRows
        JASSingleObject -> QDBSingleRow
  catMaybes <$> sequenceA
    [ requiredFieldParser (mkRF . queryResultType) $ selectFunction          functionInfo funcName    funcDesc    selPerms
    , optionalFieldParser (mkRF . QDBAggregation)  $ selectFunctionAggregate functionInfo funcAggName funcAggDesc selPerms
    ]

buildFunctionMutationFields
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> SourceConfig b
  -> FunctionName b
  -> FunctionInfo b
  -> TableName b
  -> SelPermInfo b
  -> m [FieldParser n (MutationRootField UnpreparedValue)]
buildFunctionMutationFields sourceName sourceInfo functionName functionInfo tableName selPerms = do
  funcName <- functionGraphQLName @b functionName `onLeft` throwError
  let
    mkRF = RFDB sourceName
             . AB.mkAnyBackend
             . SourceConfigWith sourceInfo
             . MDBR
    funcDesc = Just $ G.Description $ "execute VOLATILE function " <> functionName <<> " which returns " <>> tableName
    jsonAggSelect = _fiJsonAggSelect functionInfo
  catMaybes <$> sequenceA
    [ requiredFieldParser (mkRF . MDBFunction jsonAggSelect) $ selectFunction functionInfo funcName funcDesc selPerms
      -- TODO: do we want aggregate mutation functions?
    ]
