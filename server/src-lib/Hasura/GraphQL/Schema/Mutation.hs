{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasura.GraphQL.Schema.Mutation
  ( insertIntoTable,
    insertOneIntoTable,
    deleteFromTable,
    deleteFromTableByPk,
    mkDefaultRelationshipParser,
    mutationSelectionSet,
    primaryKeysArguments,
  )
where

import Control.Lens ((^.))
import Data.Has (getter)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Text.Extended
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Delete qualified as IR
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Returning qualified as IR
import Hasura.RQL.IR.Root qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

-- insert

-- | Construct the parser for a field that can be used to add several rows to a DB table.
--
-- This function is used to create the insert_tablename root field.
-- The field accepts the following arguments:
--   - objects: the list of objects to insert into the table (see 'tableFieldsInput')
--   - parser for backend-specific fields, e.g. upsert fields on_conflict or if_matched
insertIntoTable ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  (BackendTableSelectSchema b) =>
  (TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  Scenario ->
  -- | qualified name of the table
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnotatedInsert b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
insertIntoTable backendInsertAction scenario tableInfo fieldName description = runMaybeT $ do
  sourceInfo :: SourceInfo b <- asks getter
  roleName <- retrieve scRole
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
      permissions = getRolePermInfo roleName tableInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
  guard $ isMutable viIsInsertable viewInfo
  insertPerms <- hoistMaybe $ _permIns permissions
  -- If we're in a frontend scenario, we should not include backend_only inserts
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && ipiBackendOnly insertPerms
  lift do
    let updatePerms = _permUpd permissions
    -- objects [{ ... }]
    objectParser <- tableFieldsInput tableInfo
    backendInsertParser <- backendInsertAction tableInfo
    -- returning clause, affected rows, etc.
    selectionParser <- mutationSelectionSet tableInfo
    let argsParser = do
          backendInsert <- backendInsertParser
          objects <- mkObjectsArg objectParser
          pure $ mkInsertObject objects tableInfo backendInsert insertPerms updatePerms
    pure
      $ P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName))
      $ P.subselection fieldName description argsParser selectionParser
      <&> \(insertObject, output) -> IR.AnnotatedInsert (G.unName fieldName) False insertObject (IR.MOutMultirowFields output) (Just tCase)
  where
    mkObjectsArg objectParser =
      P.field
        Name._objects
        (Just "the rows to be inserted")
        (P.list objectParser)

-- | Variant of 'insertIntoTable' that inserts a single row.
--
-- Instead of expecting a list of rows to insert in a 'objects' argument, this
-- field instead expects a single 'object'. Its selection set is also slightly
-- different: it only allows selecting columns from the row being inserted.
insertOneIntoTable ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  (BackendTableSelectSchema b) =>
  (TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  Scenario ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnotatedInsert b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
insertOneIntoTable backendInsertAction scenario tableInfo fieldName description = runMaybeT do
  sourceInfo :: SourceInfo b <- asks getter
  roleName <- retrieve scRole
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
      permissions = getRolePermInfo roleName tableInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
  guard $ isMutable viIsInsertable viewInfo
  insertPerms <- hoistMaybe $ _permIns permissions
  -- If we're in a frontend scenario, we should not include backend_only inserts
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && ipiBackendOnly insertPerms
  selectionParser <- MaybeT $ tableSelectionSet tableInfo
  lift do
    let updatePerms = _permUpd permissions
    objectParser <- tableFieldsInput tableInfo
    backendInsertParser <- backendInsertAction tableInfo
    let argsParser = do
          backendInsert <- backendInsertParser
          object <- mkObjectArg objectParser
          pure $ mkInsertObject [object] tableInfo backendInsert insertPerms updatePerms
    pure
      $ P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName))
      $ P.subselection fieldName description argsParser selectionParser
      <&> \(insertObject, output) -> IR.AnnotatedInsert (G.unName fieldName) True insertObject (IR.MOutSinglerowObject output) (Just tCase)
  where
    mkObjectArg objectParser =
      P.field
        Name._object
        (Just "the row to be inserted")
        objectParser

-- | Creates the parser for an input object for a row of the given table.
--
-- This function creates an input object type named "tablename_insert_input" in
-- the GraphQL shema, which has a field for each of the columns of that table
-- that the user has insert permissions for.
--
-- > {
-- >  insert_author (
-- >    objects: [
-- >      { # tableFieldsInput output
-- >        name: "John",
-- >        id:12
-- >      }
-- >    ] ...
-- >  ) ...
-- > }
--
-- TODO: When there are no columns to insert, accessible to a role,
-- this function may generate an empty input object. The GraphQL spec
-- mandates that an input object type must define one or more input fields.
-- In this case, when there are no columns that are accessible to a role,
-- we should disallow generating the `insert_<table>` and the `insert_<table>_one`
-- altogether.
tableFieldsInput ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  -- | qualified name of the table
  TableInfo b ->
  SchemaT r m (Parser 'Input n (IR.AnnotatedInsertRow b (IR.UnpreparedValue b)))
tableFieldsInput tableInfo = do
  sourceInfo :: SourceInfo b <- asks getter
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      allFields = _tciFieldInfoMap . _tiCoreInfo $ tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
  P.memoizeOn 'tableFieldsInput (sourceName, tableName) do
    tableGQLName <- getTableIdentifierName tableInfo
    objectFields <- traverse mkFieldParser (HashMap.elems allFields)
    let objectName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableInsertInputTypeName tableGQLName
        objectDesc = G.Description $ "input type for inserting data into table " <>> tableName
    pure $ P.object objectName (Just objectDesc) $ coalesceFields objectFields
  where
    -- For each field, we have a Maybe parser: not all fields will be allowed
    -- (we don't allow insertions in computed fields for instance). Each parser
    -- returns a maybe value, as some of the fields may be omitted. This
    -- function does the necessary transformations to coalesce all of this in
    -- one 'InputFieldsParser'.
    coalesceFields ::
      [Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsertField b (IR.UnpreparedValue b))))] ->
      InputFieldsParser n (IR.AnnotatedInsertRow b (IR.UnpreparedValue b))
    coalesceFields = fmap catMaybes . sequenceA . catMaybes

    mkFieldParser ::
      FieldInfo b ->
      SchemaT r m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsertField b (IR.UnpreparedValue b)))))
    mkFieldParser = \case
      FIComputedField _ -> pure Nothing
      FIRemoteRelationship _ -> pure Nothing
      FIColumn (SCIScalarColumn columnInfo) -> do
        if (_cmIsInsertable $ ciMutability columnInfo)
          then mkColumnParser columnInfo
          else pure Nothing
      FIColumn (SCIObjectColumn _) -> pure Nothing -- TODO(dmoverton)
      FIColumn (SCIArrayColumn _) -> pure Nothing -- TODO(dmoverton)
      FIRelationship relInfo -> mkRelationshipParser relInfo

    mkColumnParser ::
      ColumnInfo b ->
      SchemaT r m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsertField b (IR.UnpreparedValue b)))))
    mkColumnParser columnInfo = runMaybeT $ do
      roleName <- retrieve scRole
      insertPerms <- hoistMaybe $ _permIns $ getRolePermInfo roleName tableInfo
      let columnName = ciName columnInfo
          columnDesc = ciDescription columnInfo
          isAllowed = Set.member (ciColumn columnInfo) (ipiCols insertPerms)
      guard isAllowed
      fieldParser <- lift $ columnParser (ciType columnInfo) (G.Nullability $ ciIsNullable columnInfo)
      pure
        $ P.fieldOptional columnName columnDesc fieldParser
        `mapField` \value ->
          IR.AIColumn (ciColumn columnInfo, IR.mkParameter value)

mkDefaultRelationshipParser ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  (TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  XNestedInserts b ->
  RelInfo b ->
  SchemaT r m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsertField b (IR.UnpreparedValue b)))))
mkDefaultRelationshipParser backendInsertAction xNestedInserts relationshipInfo = runMaybeT do
  otherTableName <- case riTarget relationshipInfo of
    RelTargetNativeQuery _ ->
      -- Native Queries do not support mutations atm
      hoistMaybe Nothing
    RelTargetTable tn -> pure tn
  let relName = riName relationshipInfo
  otherTableInfo <- askTableInfo otherTableName
  relFieldName <- lift $ textToName $ relNameToTxt relName
  case riType relationshipInfo of
    ObjRel -> do
      parser <- MaybeT $ objectRelationshipInput backendInsertAction otherTableInfo
      pure
        $ P.fieldOptional relFieldName Nothing (P.nullable parser)
        <&> \objRelIns -> do
          rel <- join objRelIns
          Just $ IR.AIObjectRelationship xNestedInserts $ IR.RelationInsert rel relationshipInfo
    ArrRel -> do
      parser <- MaybeT $ arrayRelationshipInput backendInsertAction otherTableInfo
      pure
        $ P.fieldOptional relFieldName Nothing (P.nullable parser)
        <&> \arrRelIns -> do
          rel <- join arrRelIns
          guard $ not $ null $ IR._aiInsertObject rel
          Just $ IR.AIArrayRelationship xNestedInserts $ IR.RelationInsert rel relationshipInfo

-- | Construct the parser for an input object that represents an insert through
-- an object relationship.
--
-- When inserting objects into tables, we allow insertions through
-- relationships. This function creates the parser for an object that represents
-- the insertion object across an object relationship; it is co-recursive with
-- 'tableFieldsInput'
objectRelationshipInput ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  (TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Input n (IR.SingleObjectInsert b (IR.UnpreparedValue b))))
objectRelationshipInput backendInsertAction tableInfo = runMaybeT $ do
  sourceInfo :: SourceInfo b <- asks getter
  roleName <- retrieve scRole
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      permissions = getRolePermInfo roleName tableInfo
      updatePerms = _permUpd permissions
  insertPerms <- hoistMaybe $ _permIns permissions
  lift $ P.memoizeOn 'objectRelationshipInput (sourceName, tableName) do
    tableGQLName <- getTableIdentifierName tableInfo
    objectParser <- tableFieldsInput tableInfo
    backendInsertParser <- backendInsertAction tableInfo
    let inputName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableObjRelInsertInputTypeName $ tableGQLName
        objectName = Name._data
        inputDesc = G.Description $ "input type for inserting object relation for remote table " <>> tableName
        inputParser = do
          backendInsert <- backendInsertParser
          object <- P.field objectName Nothing objectParser
          pure $ mkInsertObject (IR.Single object) tableInfo backendInsert insertPerms updatePerms
    pure $ P.object inputName (Just inputDesc) inputParser

-- | Construct the parser for an input object that represents an insert through
-- an array relationship.
--
-- When inserting objects into tables, we allow insertions through
-- relationships. This function creates the parser for an object that represents
-- the insertion object across an array relationship; it is co-recursive with
-- 'tableFieldsInput'.
arrayRelationshipInput ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  (TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Input n (IR.MultiObjectInsert b (IR.UnpreparedValue b))))
arrayRelationshipInput backendInsertAction tableInfo = runMaybeT $ do
  sourceInfo :: SourceInfo b <- asks getter
  roleName <- retrieve scRole
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
      permissions = getRolePermInfo roleName tableInfo
      updatePerms = _permUpd permissions
  insertPerms <- hoistMaybe $ _permIns permissions
  lift $ P.memoizeOn 'arrayRelationshipInput (sourceName, tableName) do
    tableGQLName <- getTableIdentifierName tableInfo
    objectParser <- tableFieldsInput tableInfo
    backendInsertParser <- backendInsertAction tableInfo
    let inputName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableArrRelInsertInputTypeName tableGQLName
        objectsName = Name._data
        inputDesc = G.Description $ "input type for inserting array relation for remote table " <>> tableName
        inputParser = do
          backendInsert <- backendInsertParser
          objects <- P.field objectsName Nothing $ P.list objectParser
          pure $ mkInsertObject objects tableInfo backendInsert insertPerms updatePerms
    pure $ P.object inputName (Just inputDesc) inputParser

-- | Helper function that creates an 'AnnIns' object.
mkInsertObject ::
  forall b f.
  (BackendSchema b) =>
  f (IR.AnnotatedInsertRow b (IR.UnpreparedValue b)) ->
  TableInfo b ->
  BackendInsert b (IR.UnpreparedValue b) ->
  InsPermInfo b ->
  Maybe (UpdPermInfo b) ->
  IR.AnnotatedInsertData b f (IR.UnpreparedValue b)
mkInsertObject objects tableInfo backendInsert insertPerms updatePerms =
  IR.AnnotatedInsertData
    { _aiInsertObject = objects,
      _aiTableName = table,
      _aiCheckCondition = (insertCheck, updateCheck),
      _aiTableColumns = columns,
      _aiPrimaryKey = primaryKey,
      _aiExtraTableMetadata = extraTableMetadata,
      _aiPresetValues = presetValues,
      _aiBackendInsert = backendInsert,
      _aiValidateInput = ipiValidateInput insertPerms
    }
  where
    table = tableInfoName tableInfo
    columns = tableColumns tableInfo
    primaryKey = tableInfo ^. tiCoreInfo . tciPrimaryKey <&> (_pkColumns >>> fmap ciColumn)
    extraTableMetadata = tableInfo ^. tiCoreInfo . tciExtraTableMetadata
    insertCheck = fmap partialSQLExpToUnpreparedValue <$> ipiCheck insertPerms
    updateCheck = (fmap . fmap . fmap) partialSQLExpToUnpreparedValue $ upiCheck =<< updatePerms
    presetValues = partialSQLExpToUnpreparedValue <$> ipiSet insertPerms

-- delete

-- | Construct a root field, normally called delete_tablename, that can be used
-- to delete several rows from a DB table
deleteFromTable ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b,
    BackendTableSelectSchema b
  ) =>
  Scenario ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnDelG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
deleteFromTable scenario tableInfo fieldName description = runMaybeT $ do
  sourceInfo :: SourceInfo b <- asks getter
  roleName <- retrieve scRole
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
  guard $ isMutable viIsInsertable viewInfo
  deletePerms <- hoistMaybe $ _permDel $ getRolePermInfo roleName tableInfo
  -- If we're in a frontend scenario, we should not include backend_only deletes
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && dpiBackendOnly deletePerms
  lift do
    let whereName = Name._where
        whereDesc = "filter the rows which have to be deleted"
    whereArg <- P.field whereName (Just whereDesc) <$> tableBoolExp tableInfo
    selection <- mutationSelectionSet tableInfo
    let columns = tableColumns tableInfo
    pure
      $ P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName))
      $ P.subselection fieldName description whereArg selection
      <&> mkDeleteObject (tableInfoName tableInfo) columns deletePerms (Just tCase) False
      . fmap IR.MOutMultirowFields

-- | Construct a root field, normally called delete_tablename_by_pk, that can be used to delete an
-- individual rows from a DB table, specified by primary key. Select permissions are required, as
-- the user must be allowed to access all the primary keys of the table.
deleteFromTableByPk ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  (BackendTableSelectSchema b) =>
  Scenario ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnDelG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
deleteFromTableByPk scenario tableInfo fieldName description = runMaybeT $ do
  sourceInfo :: SourceInfo b <- asks getter
  roleName <- retrieve scRole
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
      columns = tableColumns tableInfo
  guard $ isMutable viIsInsertable viewInfo
  pkArgs <- MaybeT $ primaryKeysArguments tableInfo
  deletePerms <- hoistMaybe $ _permDel $ getRolePermInfo roleName tableInfo
  -- If we're in a frontend scenario, we should not include backend_only deletes
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && dpiBackendOnly deletePerms
  selection <- MaybeT $ tableSelectionSet tableInfo
  pure
    $ P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName))
    $ P.subselection fieldName description pkArgs selection
    <&> mkDeleteObject (tableInfoName tableInfo) columns deletePerms (Just tCase) True
    . fmap IR.MOutSinglerowObject

mkDeleteObject ::
  (Backend b) =>
  TableName b ->
  [ColumnInfo b] ->
  DelPermInfo b ->
  Maybe NamingCase ->
  Bool ->
  (AnnBoolExp b (IR.UnpreparedValue b), IR.MutationOutputG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)) ->
  IR.AnnDelG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)
mkDeleteObject table columns deletePerms tCase isDeleteByPK (whereExp, mutationOutput) =
  IR.AnnDel
    { IR._adTable = table,
      IR._adWhere = (permissionFilter, whereExp),
      IR._adOutput = mutationOutput,
      IR._adAllCols = columns,
      IR._adNamingConvention = tCase,
      IR._adValidateInput = dpiValidateInput deletePerms,
      IR._adIsDeleteByPk = isDeleteByPK
    }
  where
    permissionFilter = fmap partialSQLExpToUnpreparedValue <$> dpiFilter deletePerms

-- common

-- | All mutations allow returning results, such as what the updated database
-- rows look like. This parser allows a query to specify what data to fetch.
mutationSelectionSet ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  (BackendTableSelectSchema b) =>
  TableInfo b ->
  SchemaT r m (Parser 'Output n (IR.MutFldsG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)))
mutationSelectionSet tableInfo = do
  sourceInfo :: SourceInfo b <- asks getter
  roleName <- retrieve scRole
  let sourceName = _siName sourceInfo
      tableName = tableInfoName tableInfo
      customization = _siCustomization sourceInfo
      tCase = _rscNamingConvention customization
      mkTypename = runMkTypename $ _rscTypeNames customization
  P.memoizeOn 'mutationSelectionSet (sourceName, tableName) do
    tableGQLName <- getTableIdentifierName tableInfo
    returning <- runMaybeT do
      _permissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
      tableSet <- MaybeT $ tableSelectionList tableInfo
      let returningName = Name._returning
          returningDesc = "data from the rows affected by the mutation"
      pure $ IR.MRet <$> P.subselection_ returningName (Just returningDesc) tableSet
    let selectionName = mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableMutationResponseTypeName tableGQLName
        affectedRowsName = applyFieldNameCaseIdentifier tCase affectedRowsFieldName
        affectedRowsDesc = "number of rows affected by the mutation"
        selectionDesc = G.Description $ "response of any mutation on the table " <>> tableName
        selectionFields =
          catMaybes
            [ Just
                $ IR.MCount
                <$ P.selection_ affectedRowsName (Just affectedRowsDesc) P.int,
              returning
            ]
    pure
      $ P.selectionSet selectionName (Just selectionDesc) selectionFields
      <&> parsedSelectionsToFields IR.MExp

-- | How to specify a database row by primary key.
--
-- This will give @Nothing@ when either there are no primary keys defined for
-- the table or when the given permissions do not permit selecting from all the
-- columns that make up the key.
primaryKeysArguments ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  TableInfo b ->
  SchemaT r m (Maybe (InputFieldsParser n (AnnBoolExp b (IR.UnpreparedValue b))))
primaryKeysArguments tableInfo = runMaybeT $ do
  roleName <- retrieve scRole
  selectPerms <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  primaryKeys <- hoistMaybe $ _tciPrimaryKey . _tiCoreInfo $ tableInfo
  let columns = _pkColumns primaryKeys
  guard $ all (\c -> ciColumn c `HashMap.member` spiCols selectPerms) columns
  lift
    $ fmap (BoolAnd . toList)
    . sequenceA
    <$> for columns \columnInfo -> do
      let redactionExp = fromMaybe NoRedaction $ getRedactionExprForColumn selectPerms (ciColumn columnInfo)
      field <- columnParser (ciType columnInfo) (G.Nullability False)
      pure
        $ BoolField
        . AVColumn columnInfo redactionExp
        . pure
        . AEQ NonNullableComparison
        . IR.mkParameter
        <$> P.field (ciName columnInfo) (ciDescription columnInfo) field
