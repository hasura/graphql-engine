-- | Top-level schema building function. Those are used to construct a schema for a given resource,
-- such as a table or a function. Those matches what the @BackendSchema@ class expects, and are the
-- default implementation a fully-fledged backend should support.
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
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

buildTableQueryFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  SourceConfig b ->
  Maybe QueryTagsConfig ->
  TableName b ->
  TableInfo b ->
  G.Name ->
  SelPermInfo b ->
  m [FieldParser n (QueryRootField UnpreparedValue)]
buildTableQueryFields sourceName sourceInfo queryTagsConfig tableName tableInfo gqlName selPerms = do
  let mkRF =
        RFDB sourceName
          . AB.mkAnyBackend
          . SourceConfigWith sourceInfo queryTagsConfig
          . QDBR
      customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
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
      [ requiredFieldParser (mkRF . QDBMultipleRows) $ selectTable sourceName tableInfo selectName selectDesc selPerms,
        optionalFieldParser (mkRF . QDBSingleRow) $ selectTableByPk sourceName tableInfo selectPKName selectPKDesc selPerms,
        optionalFieldParser (mkRF . QDBAggregation) $ selectTableAggregate sourceName tableInfo selectAggName selectAggDesc selPerms
      ]

buildTableInsertMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  SourceConfig b ->
  Maybe QueryTagsConfig ->
  TableName b ->
  TableInfo b ->
  G.Name ->
  InsPermInfo b ->
  Maybe (SelPermInfo b) ->
  Maybe (UpdPermInfo b) ->
  m [FieldParser n (MutationRootField UnpreparedValue)]
buildTableInsertMutationFields
  sourceName
  sourceInfo
  queryTagsConfig
  tableName
  tableInfo
  gqlName
  insPerms
  mSelPerms
  mUpdPerms =
    do
      let mkRF ::
            FieldParser
              n
              (AnnInsert b (RemoteSelect UnpreparedValue) (UnpreparedValue b)) ->
            FieldParser n (MutationRootField UnpreparedValue)
          mkRF =
            fmap
              ( RFDB sourceName
                  . AB.mkAnyBackend
                  . SourceConfigWith sourceInfo queryTagsConfig
                  . MDBR
                  . MDBInsert
              )
          customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
          -- insert into table
          insertDesc = Just $ G.Description $ "insert data into the table: " <>> tableName
          -- insert one into table
          insertOneDesc = Just $ G.Description $ "insert a single row into the table: " <>> tableName
      insertName <- mkRootFieldName $ fromMaybe ($$(G.litName "insert_") <> gqlName) $ _tcrfInsert customRootFields
      insertOneName <- mkRootFieldName $ fromMaybe ($$(G.litName "insert_") <> gqlName <> $$(G.litName "_one")) $ _tcrfInsertOne customRootFields
      insert <- insertIntoTable sourceName tableInfo insertName insertDesc insPerms mSelPerms mUpdPerms
      -- Select permissions are required for insertOne: the selection set is the
      -- same as a select on that table, and it therefore can't be populated if the
      -- user doesn't have select permissions.
      insertOne <- for mSelPerms \selPerms ->
        insertOneIntoTable sourceName tableInfo insertOneName insertOneDesc insPerms selPerms mUpdPerms
      pure $ map mkRF (insert : maybeToList insertOne)

buildTableUpdateMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  -- | Action that builds the @update operators@ supported.
  ( TableInfo b ->
    UpdPermInfo b ->
    m
      (InputFieldsParser n [(Column b, UpdOpExpG (UnpreparedValue b))])
  ) ->
  SourceName ->
  SourceConfig b ->
  Maybe QueryTagsConfig ->
  TableName b ->
  TableInfo b ->
  G.Name ->
  UpdPermInfo b ->
  Maybe (SelPermInfo b) ->
  m [FieldParser n (MutationRootField UnpreparedValue)]
buildTableUpdateMutationFields updateOperators sourceName sourceInfo queryTagsConfig tableName tableInfo gqlName updPerms mSelPerms = do
  let mkRF =
        RFDB sourceName
          . AB.mkAnyBackend
          . SourceConfigWith sourceInfo queryTagsConfig
          . MDBR
      customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
      -- update table
      updateDesc = Just $ G.Description $ "update data of the table: " <>> tableName
      -- update table by pk
      updatePKDesc = Just $ G.Description $ "update single row of the table: " <>> tableName
  updateName <- mkRootFieldName $ fromMaybe ($$(G.litName "update_") <> gqlName) $ _tcrfUpdate customRootFields
  updatePKName <- mkRootFieldName $ fromMaybe ($$(G.litName "update_") <> gqlName <> $$(G.litName "_by_pk")) $ _tcrfUpdateByPk customRootFields
  update <- updateTable updateOperators sourceName tableInfo updateName updateDesc updPerms mSelPerms
  -- Primary keys can only be tested in the `where` clause if a primary key
  -- exists on the table and if the user has select permissions on all columns
  -- that make up the key.
  updateByPk <- fmap join $ for mSelPerms $ updateTableByPk updateOperators sourceName tableInfo updatePKName updatePKDesc updPerms
  pure $ fmap (mkRF . MDBUpdate) <$> update : catMaybes [updateByPk]

buildTableDeleteMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  SourceConfig b ->
  Maybe QueryTagsConfig ->
  TableName b ->
  TableInfo b ->
  G.Name ->
  DelPermInfo b ->
  Maybe (SelPermInfo b) ->
  m [FieldParser n (MutationRootField UnpreparedValue)]
buildTableDeleteMutationFields sourceName sourceInfo queryTagsConfig tableName tableInfo gqlName delPerms mSelPerms = do
  let mkRF =
        RFDB sourceName
          . AB.mkAnyBackend
          . SourceConfigWith sourceInfo queryTagsConfig
          . MDBR
      customRootFields = _tcCustomRootFields $ _tciCustomConfig $ _tiCoreInfo tableInfo
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
  pure $ fmap (mkRF . MDBDelete) <$> delete : maybeToList deleteByPk

buildFunctionQueryFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  SourceConfig b ->
  Maybe QueryTagsConfig ->
  FunctionName b ->
  FunctionInfo b ->
  TableName b ->
  SelPermInfo b ->
  m [FieldParser n (QueryRootField UnpreparedValue)]
buildFunctionQueryFields sourceName sourceInfo queryTagsConfig functionName functionInfo tableName selPerms = do
  let mkRF =
        RFDB sourceName
          . AB.mkAnyBackend
          . SourceConfigWith sourceInfo queryTagsConfig
          . QDBR

      -- select function
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
      [ requiredFieldParser (mkRF . queryResultType) $ selectFunction sourceName functionInfo funcDesc selPerms,
        optionalFieldParser (mkRF . QDBAggregation) $ selectFunctionAggregate sourceName functionInfo funcAggDesc selPerms
      ]

buildFunctionMutationFields ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  SourceConfig b ->
  Maybe QueryTagsConfig ->
  FunctionName b ->
  FunctionInfo b ->
  TableName b ->
  SelPermInfo b ->
  m [FieldParser n (MutationRootField UnpreparedValue)]
buildFunctionMutationFields sourceName sourceInfo queryTagsConfig functionName functionInfo tableName selPerms = do
  let mkRF =
        RFDB sourceName
          . AB.mkAnyBackend
          . SourceConfigWith sourceInfo queryTagsConfig
          . MDBR

      funcDesc = Just $ G.Description $ "execute VOLATILE function " <> functionName <<> " which returns " <>> tableName

      jsonAggSelect = _fiJsonAggSelect functionInfo
  catMaybes
    <$> sequenceA
      [ requiredFieldParser (mkRF . MDBFunction jsonAggSelect) $ selectFunction sourceName functionInfo funcDesc selPerms
      -- TODO: do we want aggregate mutation functions?
      ]
