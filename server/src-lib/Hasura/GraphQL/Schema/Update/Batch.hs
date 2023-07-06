module Hasura.GraphQL.Schema.Update.Batch
  ( updateTable,
    updateTableMany,
    updateTableByPk,
  )
where

import Data.Has (Has (getter))
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Extended (toTxt, (<>>))
import Hasura.GraphQL.Schema.Backend (BackendTableSelectSchema (..), BackendUpdateOperatorsSchema (..), MonadBuildSchema)
import Hasura.GraphQL.Schema.BoolExp (AggregationPredicatesSchema, tableBoolExp)
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Mutation (mutationSelectionSet, primaryKeysArguments)
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table (tableColumns)
import Hasura.GraphQL.Schema.Typename
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (annBoolExpTrue)
import Hasura.RQL.IR.Returning (MutationOutputG (..))
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Update (AnnotatedUpdateG (..))
import Hasura.RQL.IR.Update.Batch (UpdateBatch (..))
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend (Backend (..))
import Hasura.RQL.Types.Column (ColumnInfo (..))
import Hasura.RQL.Types.Common (Comment (..), ResolvedWebhook)
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax (Description (..), Name (..))

buildAnnotatedUpdateGField ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  Scenario ->
  TableInfo b ->
  -- | field display name
  Name ->
  -- | field description, if any
  Maybe Description ->
  -- | parser of 'MutationOutputG'
  MaybeT (SchemaT r m) (P.Parser 'P.Output n (MutationOutputG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))) ->
  -- | parser of the backend-specific 'UpdateVariant'
  (UpdPermInfo b -> MaybeT (SchemaT r m) (P.InputFieldsParser n (UpdateVariant b (UnpreparedValue b)))) ->
  MaybeT (SchemaT r m) (P.FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)))
buildAnnotatedUpdateGField scenario tableInfo fieldName description parseOutput mkUpdateVariantParser = do
  roleName <- retrieve scRole
  updatePerms <- hoistMaybe $ _permUpd $ getRolePermInfo roleName tableInfo
  guard $ not $ scenario == Frontend && upiBackendOnly updatePerms

  (sourceInfo :: SourceInfo b) <- asks getter
  let sourceName = _siName sourceInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      columns = tableColumns tableInfo
      viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
      tableName = tableInfoName tableInfo
      validateInput = upiValidateInput updatePerms

  guard $ isMutable viIsUpdatable viewInfo
  outputParser <- parseOutput
  updateVariantParser <- mkUpdateVariantParser updatePerms
  pure
    $ P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName))
    $ mkAnnotatedUpdateG tableName columns updatePerms (Just tCase) validateInput
    <$> P.subselection fieldName description updateVariantParser outputParser

-- | Construct a root field, normally called update_tablename, that can be used
-- to update rows in a DB table specified by filters. Only returns a parser if
-- there are columns the user is allowed to update; otherwise returns Nothing.
updateTable ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b,
    BackendTableSelectSchema b,
    BackendUpdateOperatorsSchema b
  ) =>
  (UpdateBatch b (UpdateOperators b) (UnpreparedValue b) -> UpdateVariant b (UnpreparedValue b)) ->
  Scenario ->
  TableInfo b ->
  -- | table field display name
  GQLNameIdentifier ->
  SchemaT r m (Maybe (P.FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
updateTable mkSingleBatchUpdateVariant scenario tableInfo tableGqlName = runMaybeT $ do
  customization <- asks (_siCustomization . getter @(SourceInfo b))
  let (MkRootFieldName mkRootFieldName) = _rscRootFields customization
      tCase = _rscNamingConvention customization
      updateTableFieldName = mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfUpdate mkUpdateField tableGqlName
  let parseOutput = lift $ fmap MOutMultirowFields <$> mutationSelectionSet tableInfo

  buildAnnotatedUpdateGField scenario tableInfo updateTableFieldName updateTableFieldDescription parseOutput $ \updatePerms -> lift $ do
    whereArg <- P.field Name._where (Just whereDesc) <$> tableBoolExp tableInfo
    updateOperators <- parseUpdateOperators tableInfo updatePerms
    pure $ mkSingleBatchUpdateVariant <$> (UpdateBatch <$> updateOperators <*> whereArg)
  where
    tableName = tableInfoName tableInfo
    updateTableFieldDescription = buildFieldDescription defaultUpdateDesc $ _crfComment _tcrfUpdate
    defaultUpdateDesc = "update data of the table: " <>> tableName
    whereDesc = "filter the rows which have to be updated"
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

updateTableMany ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b,
    BackendTableSelectSchema b,
    BackendUpdateOperatorsSchema b
  ) =>
  ([UpdateBatch b (UpdateOperators b) (UnpreparedValue b)] -> UpdateVariant b (UnpreparedValue b)) ->
  Scenario ->
  TableInfo b ->
  -- | table field display name
  GQLNameIdentifier ->
  SchemaT r m (Maybe (P.FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
updateTableMany mkSingleBatchUpdateVariant scenario tableInfo tableGqlName = runMaybeT $ do
  validateShouldIncludeUpdateManyFields

  customization <- asks (_siCustomization . getter @(SourceInfo b))
  let (MkRootFieldName mkRootFieldName) = _rscRootFields customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      tCase = _rscNamingConvention customization
      updatesObjectName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkMultiRowUpdateTypeName tableGqlName
      updateTableManyFieldName = mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfUpdateMany mkUpdateManyField tableGqlName
  let parseOutput = lift $ fmap MOutMultirowFields . P.multiple <$> mutationSelectionSet tableInfo

  buildAnnotatedUpdateGField scenario tableInfo updateTableManyFieldName updateManyFieldDescription parseOutput $ \updatePerms -> lift $ do
    updateOperators <- parseUpdateOperators tableInfo updatePerms
    fmap mkSingleBatchUpdateVariant
      . P.field Name._updates (Just updatesDesc)
      . P.list
      . P.object updatesObjectName Nothing
      <$> do
        whereExp <- P.field Name._where (Just whereDesc) <$> tableBoolExp tableInfo
        pure $ UpdateBatch <$> updateOperators <*> whereExp
  where
    tableName = tableInfoName tableInfo
    updateManyFieldDescription = buildFieldDescription defaultUpdateManyDesc $ _crfComment _tcrfUpdateMany
    defaultUpdateManyDesc = "update multiples rows of table: " <>> tableName
    whereDesc = "filter the rows which have to be updated"
    updatesDesc = "updates to execute, in order"
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

    -- we only include the multiUpdate field if the
    -- experimental feature 'hide_update_many_fields' is off
    validateShouldIncludeUpdateManyFields =
      retrieve Options.soIncludeUpdateManyFields >>= \case
        Options.IncludeUpdateManyFields -> hoistMaybe $ Just ()
        Options.Don'tIncludeUpdateManyFields -> hoistMaybe $ Nothing

-- | Construct a root field, normally called 'update_tablename_by_pk', that can be used
-- to update a single in a DB table, specified by primary key. Only returns a
-- parser if there are columns the user is allowed to update and if the user has
-- select permissions on all primary keys; otherwise returns Nothing.
updateTableByPk ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendTableSelectSchema b,
    BackendUpdateOperatorsSchema b
  ) =>
  (UpdateBatch b (UpdateOperators b) (UnpreparedValue b) -> UpdateVariant b (UnpreparedValue b)) ->
  Scenario ->
  TableInfo b ->
  -- | table field display name
  GQLNameIdentifier ->
  SchemaT r m (Maybe (P.FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
updateTableByPk mkSingleBatchUpdateVariant scenario tableInfo tableGqlName = runMaybeT do
  customization <- asks (_siCustomization . getter @(SourceInfo b))
  let (MkRootFieldName mkRootFieldName) = _rscRootFields customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      tCase = _rscNamingConvention customization
      updateTableFieldName = mkRootFieldName $ setFieldNameCase tCase tableInfo _tcrfUpdateByPk mkUpdateByPkField tableGqlName
      pkObjectName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTablePkColumnsInputTypeName tableGqlName
      pkFieldName = applyFieldNameCaseIdentifier tCase pkColumnsFieldName
  let parseOutput = fmap MOutSinglerowObject <$> MaybeT (tableSelectionSet tableInfo)

  buildAnnotatedUpdateGField scenario tableInfo updateTableFieldName updateByPkFieldDescription parseOutput $ \updatePerms -> do
    pkArgs <- MaybeT $ primaryKeysArguments tableInfo
    lift $ do
      updateOperators <- parseUpdateOperators tableInfo updatePerms
      let pkParser = P.object pkObjectName (Just pkObjectDesc) pkArgs
      let whereParser = P.field pkFieldName Nothing pkParser
      pure $ mkSingleBatchUpdateVariant <$> (UpdateBatch <$> updateOperators <*> whereParser)
  where
    tableName = tableInfoName tableInfo
    updateByPkFieldDescription = buildFieldDescription defaultUpdateByPkDesc $ _crfComment _tcrfUpdateByPk
    defaultUpdateByPkDesc = "update single row of the table: " <>> tableName
    pkObjectDesc = Description $ "primary key columns input for table: " <> toTxt tableName
    TableCustomRootFields {..} = _tcCustomRootFields . _tciCustomConfig $ _tiCoreInfo tableInfo

mkAnnotatedUpdateG ::
  (Backend b) =>
  TableName b ->
  [ColumnInfo b] ->
  UpdPermInfo b ->
  (Maybe NamingCase) ->
  Maybe (ValidateInput ResolvedWebhook) ->
  ( UpdateVariant b (UnpreparedValue b),
    MutationOutputG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)
  ) ->
  AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)
mkAnnotatedUpdateG _auTable _auAllCols updatePerms _auNamingConvention _auValidateInput (_auUpdateVariant, _auOutput) =
  AnnotatedUpdateG {..}
  where
    _auUpdatePermissions = fmap partialSQLExpToUnpreparedValue <$> upiFilter updatePerms
    _auCheck = maybe annBoolExpTrue ((fmap . fmap) partialSQLExpToUnpreparedValue) $ upiCheck updatePerms

buildFieldDescription :: Text -> Comment -> Maybe Description
buildFieldDescription defaultDescription = \case
  Automatic -> Just $ Description defaultDescription
  Explicit comment -> Description . toTxt <$> comment
