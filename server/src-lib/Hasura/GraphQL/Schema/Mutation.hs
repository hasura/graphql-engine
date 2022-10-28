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

import Data.Has (getter)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text.Extended
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.NamingCase (NamingCase)
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename (mkTypename)
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
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
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
  MonadBuildSchema b r m n =>
  BackendTableSelectSchema b =>
  (SourceInfo b -> TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  Scenario ->
  SourceInfo b ->
  -- | qualified name of the table
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnotatedInsert b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
insertIntoTable backendInsertAction scenario sourceInfo tableInfo fieldName description = runMaybeT $ do
  let viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
  guard $ isMutable viIsInsertable viewInfo
  roleName <- retrieve scRole
  let permissions = getRolePermInfo roleName tableInfo
  insertPerms <- hoistMaybe $ _permIns permissions
  -- If we're in a frontend scenario, we should not include backend_only inserts
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && ipiBackendOnly insertPerms
  tCase <- asks getter
  lift do
    let updatePerms = _permUpd permissions
    -- objects [{ ... }]
    objectParser <- tableFieldsInput sourceInfo tableInfo
    backendInsertParser <- backendInsertAction sourceInfo tableInfo
    -- returning clause, affected rows, etc.
    selectionParser <- mutationSelectionSet sourceInfo tableInfo
    let argsParser = do
          backendInsert <- backendInsertParser
          objects <- mkObjectsArg objectParser
          pure $ mkInsertObject objects tableInfo backendInsert insertPerms updatePerms
    pure $
      P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName)) $
        P.subselection fieldName description argsParser selectionParser
          <&> \(insertObject, output) -> IR.AnnotatedInsert (G.unName fieldName) False insertObject (IR.MOutMultirowFields output) (Just tCase)
  where
    sourceName = _siName sourceInfo
    tableName = tableInfoName tableInfo
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
  MonadBuildSchema b r m n =>
  BackendTableSelectSchema b =>
  (SourceInfo b -> TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  Scenario ->
  -- | source of the table
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnotatedInsert b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
insertOneIntoTable backendInsertAction scenario sourceInfo tableInfo fieldName description = runMaybeT do
  let viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
  guard $ isMutable viIsInsertable viewInfo
  roleName <- retrieve scRole
  let permissions = getRolePermInfo roleName tableInfo
  insertPerms <- hoistMaybe $ _permIns permissions
  -- If we're in a frontend scenario, we should not include backend_only inserts
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && ipiBackendOnly insertPerms
  selectionParser <- MaybeT $ tableSelectionSet sourceInfo tableInfo
  tCase <- asks getter
  lift do
    let updatePerms = _permUpd permissions
    objectParser <- tableFieldsInput sourceInfo tableInfo
    backendInsertParser <- backendInsertAction sourceInfo tableInfo
    let argsParser = do
          backendInsert <- backendInsertParser
          object <- mkObjectArg objectParser
          pure $ mkInsertObject [object] tableInfo backendInsert insertPerms updatePerms
    pure $
      P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName)) $
        P.subselection fieldName description argsParser selectionParser
          <&> \(insertObject, output) -> IR.AnnotatedInsert (G.unName fieldName) True insertObject (IR.MOutSinglerowObject output) (Just tCase)
  where
    sourceName = _siName sourceInfo
    tableName = tableInfoName tableInfo
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
tableFieldsInput ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceInfo b ->
  -- | qualified name of the table
  TableInfo b ->
  SchemaT r m (Parser 'Input n (IR.AnnotatedInsertRow b (IR.UnpreparedValue b)))
tableFieldsInput sourceInfo tableInfo =
  P.memoizeOn 'tableFieldsInput (_siName sourceInfo, tableName) do
    tCase <- asks getter
    tableGQLName <- getTableIdentifierName tableInfo
    objectFields <- traverse mkFieldParser (Map.elems allFields)
    objectName <- mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableInsertInputTypeName tableGQLName
    let objectDesc = G.Description $ "input type for inserting data into table " <>> tableName
    pure $ P.object objectName (Just objectDesc) $ coalesceFields objectFields
  where
    allFields = _tciFieldInfoMap . _tiCoreInfo $ tableInfo
    tableName = tableInfoName tableInfo

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
      FIColumn columnInfo -> do
        if (_cmIsInsertable $ ciMutability columnInfo)
          then mkColumnParser columnInfo
          else pure Nothing
      FIRelationship relInfo -> mkRelationshipParser sourceInfo relInfo

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
      pure $
        P.fieldOptional columnName columnDesc fieldParser `mapField` \value ->
          IR.AIColumn (ciColumn columnInfo, IR.mkParameter value)

mkDefaultRelationshipParser ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  (SourceInfo b -> TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  XNestedInserts b ->
  SourceInfo b ->
  RelInfo b ->
  SchemaT r m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsertField b (IR.UnpreparedValue b)))))
mkDefaultRelationshipParser backendInsertAction xNestedInserts sourceInfo relationshipInfo = runMaybeT do
  let otherTableName = riRTable relationshipInfo
      relName = riName relationshipInfo
  otherTableInfo <- askTableInfo sourceInfo otherTableName
  relFieldName <- lift $ textToName $ relNameToTxt relName
  case riType relationshipInfo of
    ObjRel -> do
      parser <- MaybeT $ objectRelationshipInput backendInsertAction sourceInfo otherTableInfo
      pure $
        P.fieldOptional relFieldName Nothing (P.nullable parser) <&> \objRelIns -> do
          rel <- join objRelIns
          Just $ IR.AIObjectRelationship xNestedInserts $ IR.RelationInsert rel relationshipInfo
    ArrRel -> do
      parser <- MaybeT $ arrayRelationshipInput backendInsertAction sourceInfo otherTableInfo
      pure $
        P.fieldOptional relFieldName Nothing (P.nullable parser) <&> \arrRelIns -> do
          rel <- join arrRelIns
          guard $ not $ null $ IR._aiInsertObject rel
          Just $ IR.AIArrayRelationship xNestedInserts $ IR.RelationInsert rel relationshipInfo

-- | Construct the parser for an input object that represents an insert through
-- an object relationship.
--
-- When inserting objects into tables, we allow insertions through
-- relationships. This function creates the parser for an object that represents
-- the insertion object across an object relationship; it is co-recursive with
-- 'tableFieldsInput'.
objectRelationshipInput ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  (SourceInfo b -> TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  SourceInfo b ->
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Input n (IR.SingleObjectInsert b (IR.UnpreparedValue b))))
objectRelationshipInput backendInsertAction sourceInfo tableInfo = runMaybeT $ do
  tCase <- asks getter
  roleName <- retrieve scRole
  let permissions = getRolePermInfo roleName tableInfo
      updatePerms = _permUpd permissions
  insertPerms <- hoistMaybe $ _permIns permissions
  lift $ P.memoizeOn 'objectRelationshipInput (_siName sourceInfo, tableName) do
    tableGQLName <- getTableIdentifierName tableInfo
    objectParser <- tableFieldsInput sourceInfo tableInfo
    backendInsertParser <- backendInsertAction sourceInfo tableInfo
    inputName <- mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableObjRelInsertInputTypeName $ tableGQLName
    let objectName = Name._data
        inputDesc = G.Description $ "input type for inserting object relation for remote table " <>> tableName
        inputParser = do
          backendInsert <- backendInsertParser
          object <- P.field objectName Nothing objectParser
          pure $ mkInsertObject (IR.Single object) tableInfo backendInsert insertPerms updatePerms
    pure $ P.object inputName (Just inputDesc) inputParser
  where
    tableName = tableInfoName tableInfo

-- | Construct the parser for an input object that represents an insert through
-- an array relationship.
--
-- When inserting objects into tables, we allow insertions through
-- relationships. This function creates the parser for an object that represents
-- the insertion object across an array relationship; it is co-recursive with
-- 'tableFieldsInput'.
arrayRelationshipInput ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  (SourceInfo b -> TableInfo b -> SchemaT r m (InputFieldsParser n (BackendInsert b (IR.UnpreparedValue b)))) ->
  SourceInfo b ->
  TableInfo b ->
  SchemaT r m (Maybe (Parser 'Input n (IR.MultiObjectInsert b (IR.UnpreparedValue b))))
arrayRelationshipInput backendInsertAction sourceInfo tableInfo = runMaybeT $ do
  tCase <- asks getter
  roleName <- retrieve scRole
  let permissions = getRolePermInfo roleName tableInfo
      updatePerms = _permUpd permissions
  insertPerms <- hoistMaybe $ _permIns permissions
  lift $ P.memoizeOn 'arrayRelationshipInput (_siName sourceInfo, tableName) do
    tableGQLName <- getTableIdentifierName tableInfo
    objectParser <- tableFieldsInput sourceInfo tableInfo
    backendInsertParser <- backendInsertAction sourceInfo tableInfo
    inputName <- mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableArrRelInsertInputTypeName tableGQLName
    let objectsName = Name._data
        inputDesc = G.Description $ "input type for inserting array relation for remote table " <>> tableName
        inputParser = do
          backendInsert <- backendInsertParser
          objects <- P.field objectsName Nothing $ P.list objectParser
          pure $ mkInsertObject objects tableInfo backendInsert insertPerms updatePerms
    pure $ P.object inputName (Just inputDesc) inputParser
  where
    tableName = tableInfoName tableInfo

-- | Helper function that creates an 'AnnIns' object.
mkInsertObject ::
  forall b f.
  BackendSchema b =>
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
      _aiPresetValues = presetValues,
      _aiBackendInsert = backendInsert
    }
  where
    table = tableInfoName tableInfo
    columns = tableColumns tableInfo
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
  -- | table source
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnDelG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
deleteFromTable scenario sourceInfo tableInfo fieldName description = runMaybeT $ do
  let viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
  guard $ isMutable viIsInsertable viewInfo
  roleName <- retrieve scRole
  deletePerms <- hoistMaybe $ _permDel $ getRolePermInfo roleName tableInfo
  -- If we're in a frontend scenario, we should not include backend_only deletes
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && dpiBackendOnly deletePerms
  tCase <- asks getter
  lift do
    let whereName = Name._where
        whereDesc = "filter the rows which have to be deleted"
    whereArg <- P.field whereName (Just whereDesc) <$> boolExp sourceInfo tableInfo
    selection <- mutationSelectionSet sourceInfo tableInfo
    let columns = tableColumns tableInfo
    pure $
      P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName)) $
        P.subselection fieldName description whereArg selection
          <&> mkDeleteObject (tableInfoName tableInfo) columns deletePerms (Just tCase) . fmap IR.MOutMultirowFields
  where
    sourceName = _siName sourceInfo
    tableName = tableInfoName tableInfo

-- | Construct a root field, normally called delete_tablename_by_pk, that can be used to delete an
-- individual rows from a DB table, specified by primary key. Select permissions are required, as
-- the user must be allowed to access all the primary keys of the table.
deleteFromTableByPk ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  BackendTableSelectSchema b =>
  Scenario ->
  -- | table source
  SourceInfo b ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnDelG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b))))
deleteFromTableByPk scenario sourceInfo tableInfo fieldName description = runMaybeT $ do
  let viewInfo = _tciViewInfo $ _tiCoreInfo tableInfo
  guard $ isMutable viIsInsertable viewInfo
  pkArgs <- MaybeT $ primaryKeysArguments tableInfo
  roleName <- retrieve scRole
  deletePerms <- hoistMaybe $ _permDel $ getRolePermInfo roleName tableInfo
  -- If we're in a frontend scenario, we should not include backend_only deletes
  -- For more info see Note [Backend only permissions]
  guard $ not $ scenario == Frontend && dpiBackendOnly deletePerms
  selection <- MaybeT $ tableSelectionSet sourceInfo tableInfo
  tCase <- asks getter
  let columns = tableColumns tableInfo
  pure $
    P.setFieldParserOrigin (MOSourceObjId sourceName (AB.mkAnyBackend $ SMOTable @b tableName)) $
      P.subselection fieldName description pkArgs selection
        <&> mkDeleteObject (tableInfoName tableInfo) columns deletePerms (Just tCase) . fmap IR.MOutSinglerowObject
  where
    sourceName = _siName sourceInfo
    tableName = tableInfoName tableInfo

mkDeleteObject ::
  Backend b =>
  TableName b ->
  [ColumnInfo b] ->
  DelPermInfo b ->
  Maybe NamingCase ->
  (AnnBoolExp b (IR.UnpreparedValue b), IR.MutationOutputG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)) ->
  IR.AnnDelG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)
mkDeleteObject table columns deletePerms tCase (whereExp, mutationOutput) =
  IR.AnnDel
    { IR._adTable = table,
      IR._adWhere = (permissionFilter, whereExp),
      IR._adOutput = mutationOutput,
      IR._adAllCols = columns,
      IR._adNamingConvention = tCase
    }
  where
    permissionFilter = fmap partialSQLExpToUnpreparedValue <$> dpiFilter deletePerms

-- common

-- | All mutations allow returning results, such as what the updated database
-- rows look like. This parser allows a query to specify what data to fetch.
mutationSelectionSet ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  BackendTableSelectSchema b =>
  SourceInfo b ->
  TableInfo b ->
  SchemaT r m (Parser 'Output n (IR.MutFldsG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)))
mutationSelectionSet sourceInfo tableInfo =
  P.memoizeOn 'mutationSelectionSet (_siName sourceInfo, tableName) do
    roleName <- retrieve scRole
    tCase <- asks getter
    tableGQLName <- getTableIdentifierName tableInfo
    returning <- runMaybeT do
      _permissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
      tableSet <- MaybeT $ tableSelectionList sourceInfo tableInfo
      let returningName = Name._returning
          returningDesc = "data from the rows affected by the mutation"
      pure $ IR.MRet <$> P.subselection_ returningName (Just returningDesc) tableSet
    selectionName <- mkTypename $ applyTypeNameCaseIdentifier tCase $ mkTableMutationResponseTypeName tableGQLName
    let affectedRowsName = Name._affected_rows
        affectedRowsDesc = "number of rows affected by the mutation"
        selectionDesc = G.Description $ "response of any mutation on the table " <>> tableName

        selectionFields =
          catMaybes
            [ Just $
                IR.MCount
                  <$ P.selection_ affectedRowsName (Just affectedRowsDesc) P.int,
              returning
            ]
    pure $
      P.selectionSet selectionName (Just selectionDesc) selectionFields
        <&> parsedSelectionsToFields IR.MExp
  where
    tableName = tableInfoName tableInfo

-- | How to specify a database row by primary key.
--
-- This will give @Nothing@ when either there are no primary keys defined for
-- the table or when the given permissions do not permit selecting from all the
-- columns that make up the key.
primaryKeysArguments ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  TableInfo b ->
  SchemaT r m (Maybe (InputFieldsParser n (AnnBoolExp b (IR.UnpreparedValue b))))
primaryKeysArguments tableInfo = runMaybeT $ do
  roleName <- retrieve scRole
  selectPerms <- hoistMaybe $ tableSelectPermissions roleName tableInfo
  primaryKeys <- hoistMaybe $ _tciPrimaryKey . _tiCoreInfo $ tableInfo
  let columns = _pkColumns primaryKeys
  guard $ all (\c -> ciColumn c `Map.member` spiCols selectPerms) columns
  lift $
    fmap (BoolAnd . toList) . sequenceA <$> for columns \columnInfo -> do
      field <- columnParser (ciType columnInfo) (G.Nullability False)
      pure $
        BoolField . AVColumn columnInfo . pure . AEQ True . IR.mkParameter
          <$> P.field (ciName columnInfo) (ciDescription columnInfo) field
