{-# LANGUAGE ApplicativeDo #-}

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

import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text.Extended
import Hasura.GraphQL.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    Parser,
    UnpreparedValue (..),
    mkParameter,
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.Prelude
import Hasura.RQL.IR.Delete qualified as IR
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Returning qualified as IR
import Hasura.RQL.IR.Root qualified as IR
import Hasura.RQL.Types
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
  (SourceName -> TableInfo b -> Maybe (SelPermInfo b) -> Maybe (UpdPermInfo b) -> m (InputFieldsParser n (BackendInsert b (UnpreparedValue b)))) ->
  SourceName ->
  -- | qualified name of the table
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | insert permissions of the table
  InsPermInfo b ->
  -- | select permissions of the table (if any)
  Maybe (SelPermInfo b) ->
  -- | update permissions of the table (if any)
  Maybe (UpdPermInfo b) ->
  m (FieldParser n (IR.AnnInsert b (IR.RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)))
insertIntoTable backendInsertAction sourceName tableInfo fieldName description insertPerms selectPerms updatePerms = do
  -- objects [{ ... }]
  objectParser <- tableFieldsInput sourceName tableInfo insertPerms
  backendInsertParser <- backendInsertAction sourceName tableInfo selectPerms updatePerms
  -- returning clause, affected rows, etc.
  selectionParser <- mutationSelectionSet sourceName tableInfo selectPerms
  let argsParser = do
        backendInsert <- backendInsertParser
        objects <- mkObjectsArg objectParser
        pure $ mkInsertObject objects tableInfo backendInsert insertPerms updatePerms
  return $
    P.subselection fieldName description argsParser selectionParser
      <&> \(insertObject, output) -> IR.AnnInsert (G.unName fieldName) False insertObject (IR.MOutMultirowFields output)
  where
    mkObjectsArg objectParser =
      P.field
        $$(G.litName "objects")
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
  (SourceName -> TableInfo b -> Maybe (SelPermInfo b) -> Maybe (UpdPermInfo b) -> m (InputFieldsParser n (BackendInsert b (UnpreparedValue b)))) ->
  -- | source of the table
  SourceName ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | insert permissions of the table
  InsPermInfo b ->
  -- | select permissions of the table
  SelPermInfo b ->
  -- | update permissions of the table (if any)
  Maybe (UpdPermInfo b) ->
  m (FieldParser n (IR.AnnInsert b (IR.RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)))
insertOneIntoTable backendInsertAction sourceName tableInfo fieldName description insertPerms selectPerms updatePerms = do
  selectionParser <- tableSelectionSet sourceName tableInfo selectPerms
  objectParser <- tableFieldsInput sourceName tableInfo insertPerms
  backendInsertParser <- backendInsertAction sourceName tableInfo (Just selectPerms) updatePerms
  let argsParser = do
        backendInsert <- backendInsertParser
        object <- mkObjectArg objectParser
        pure $ mkInsertObject [object] tableInfo backendInsert insertPerms updatePerms
  pure $
    P.subselection fieldName description argsParser selectionParser
      <&> \(insertObject, output) -> IR.AnnInsert (G.unName fieldName) True insertObject (IR.MOutSinglerowObject output)
  where
    mkObjectArg objectParser =
      P.field
        $$(G.litName "object")
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
  SourceName ->
  -- | qualified name of the table
  TableInfo b ->
  -- | insert permissions of the table
  InsPermInfo b ->
  m (Parser 'Input n (IR.AnnotatedInsertRow b (UnpreparedValue b)))
tableFieldsInput sourceName tableInfo insertPerms =
  memoizeOn 'tableFieldsInput (sourceName, tableName) do
    tableGQLName <- getTableGQLName tableInfo
    objectFields <- traverse mkFieldParser (Map.elems allFields)
    objectName <- P.mkTypename $ tableGQLName <> $$(G.litName "_insert_input")
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
      [Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsert b (UnpreparedValue b))))] ->
      InputFieldsParser n (IR.AnnotatedInsertRow b (UnpreparedValue b))
    coalesceFields = fmap catMaybes . sequenceA . catMaybes

    mkFieldParser ::
      FieldInfo b ->
      m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsert b (UnpreparedValue b)))))
    mkFieldParser = \case
      FIComputedField _ -> pure Nothing
      FIRemoteRelationship _ -> pure Nothing
      FIColumn columnInfo -> do
        if (_cmIsInsertable $ ciMutability columnInfo)
          then mkColumnParser columnInfo
          else pure Nothing
      FIRelationship relInfo -> mkRelationshipParser sourceName relInfo

    mkColumnParser ::
      ColumnInfo b ->
      m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsert b (UnpreparedValue b)))))
    mkColumnParser columnInfo = do
      let columnName = ciName columnInfo
          columnDesc = ciDescription columnInfo
          isAllowed = Set.member (ciColumn columnInfo) (ipiCols insertPerms)
      whenMaybe isAllowed do
        fieldParser <- columnParser (ciType columnInfo) (G.Nullability $ ciIsNullable columnInfo)
        pure $
          P.fieldOptional columnName columnDesc fieldParser `mapField` \value ->
            IR.AIColumn (ciColumn columnInfo, mkParameter value)

mkDefaultRelationshipParser ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  (SourceName -> TableInfo b -> Maybe (SelPermInfo b) -> Maybe (UpdPermInfo b) -> m (InputFieldsParser n (BackendInsert b (UnpreparedValue b)))) ->
  XNestedInserts b ->
  SourceName ->
  RelInfo b ->
  m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsert b (UnpreparedValue b)))))
mkDefaultRelationshipParser backendInsertAction xNestedInserts sourceName relationshipInfo = runMaybeT do
  let otherTableName = riRTable relationshipInfo
      relName = riName relationshipInfo
  otherTableInfo <- askTableInfo sourceName otherTableName
  permissions <- MaybeT $ tablePermissions otherTableInfo
  relFieldName <- lift $ textToName $ relNameToTxt relName
  insPerms <- hoistMaybe $ _permIns permissions
  let selPerms = _permSel permissions
      updPerms = _permUpd permissions
  lift $ case riType relationshipInfo of
    ObjRel -> do
      parser <- objectRelationshipInput backendInsertAction sourceName otherTableInfo insPerms selPerms updPerms
      pure $
        P.fieldOptional relFieldName Nothing (P.nullable parser) <&> \objRelIns -> do
          rel <- join objRelIns
          Just $ IR.AIObjectRelationship xNestedInserts $ IR.RelIns rel relationshipInfo
    ArrRel -> do
      parser <- arrayRelationshipInput backendInsertAction sourceName otherTableInfo insPerms selPerms updPerms
      pure $
        P.fieldOptional relFieldName Nothing (P.nullable parser) <&> \arrRelIns -> do
          rel <- join arrRelIns
          guard $ not $ null $ IR._aiInsObj rel
          Just $ IR.AIArrayRelationship xNestedInserts $ IR.RelIns rel relationshipInfo

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
  (SourceName -> TableInfo b -> Maybe (SelPermInfo b) -> Maybe (UpdPermInfo b) -> m (InputFieldsParser n (BackendInsert b (UnpreparedValue b)))) ->
  SourceName ->
  TableInfo b ->
  InsPermInfo b ->
  Maybe (SelPermInfo b) ->
  Maybe (UpdPermInfo b) ->
  m (Parser 'Input n (IR.SingleObjIns b (UnpreparedValue b)))
objectRelationshipInput backendInsertAction sourceName tableInfo insertPerms selectPerms updatePerms =
  memoizeOn 'objectRelationshipInput (sourceName, tableName) do
    tableGQLName <- getTableGQLName tableInfo
    objectParser <- tableFieldsInput sourceName tableInfo insertPerms
    backendInsertParser <- backendInsertAction sourceName tableInfo selectPerms updatePerms
    inputName <- P.mkTypename $ tableGQLName <> $$(G.litName "_obj_rel_insert_input")
    let objectName = $$(G.litName "data")
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
  (SourceName -> TableInfo b -> Maybe (SelPermInfo b) -> Maybe (UpdPermInfo b) -> m (InputFieldsParser n (BackendInsert b (UnpreparedValue b)))) ->
  SourceName ->
  TableInfo b ->
  InsPermInfo b ->
  Maybe (SelPermInfo b) ->
  Maybe (UpdPermInfo b) ->
  m (Parser 'Input n (IR.MultiObjIns b (UnpreparedValue b)))
arrayRelationshipInput backendInsertAction sourceName tableInfo insertPerms selectPerms updatePerms =
  memoizeOn 'arrayRelationshipInput (sourceName, tableName) do
    tableGQLName <- getTableGQLName tableInfo
    objectParser <- tableFieldsInput sourceName tableInfo insertPerms
    backendInsertParser <- backendInsertAction sourceName tableInfo selectPerms updatePerms
    inputName <- P.mkTypename $ tableGQLName <> $$(G.litName "_arr_rel_insert_input")
    let objectsName = $$(G.litName "data")
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
  f (IR.AnnotatedInsertRow b (UnpreparedValue b)) ->
  TableInfo b ->
  BackendInsert b (UnpreparedValue b) ->
  InsPermInfo b ->
  Maybe (UpdPermInfo b) ->
  IR.AnnIns b f (UnpreparedValue b)
mkInsertObject objects tableInfo backendInsert insertPerms updatePerms =
  IR.AnnIns
    { _aiInsObj = objects,
      _aiTableName = table,
      _aiCheckCond = (insertCheck, updateCheck),
      _aiTableCols = columns,
      _aiDefVals = defaultValues,
      _aiBackendInsert = backendInsert
    }
  where
    table = tableInfoName tableInfo
    columns = tableColumns tableInfo
    insertCheck = fmap partialSQLExpToUnpreparedValue <$> ipiCheck insertPerms
    updateCheck = (fmap . fmap . fmap) partialSQLExpToUnpreparedValue $ upiCheck =<< updatePerms
    defaultValues =
      Map.union (partialSQLExpToUnpreparedValue <$> ipiSet insertPerms) $
        Map.fromList
          [ (column, UVLiteral $ columnDefaultValue @b column)
            | ci <- columns,
              _cmIsInsertable (ciMutability ci),
              let column = ciColumn ci
          ]

-- delete

-- | Construct a root field, normally called delete_tablename, that can be used
-- to delete several rows from a DB table
deleteFromTable ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  -- | table source
  SourceName ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | delete permissions of the table
  DelPermInfo b ->
  -- | select permissions of the table (if any)
  Maybe (SelPermInfo b) ->
  m (FieldParser n (IR.AnnDelG b (IR.RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)))
deleteFromTable sourceName tableInfo fieldName description deletePerms selectPerms = do
  let whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be deleted"
  whereArg <- P.field whereName (Just whereDesc) <$> boolExp sourceName tableInfo selectPerms
  selection <- mutationSelectionSet sourceName tableInfo selectPerms
  let columns = tableColumns tableInfo
  pure $
    P.subselection fieldName description whereArg selection
      <&> mkDeleteObject (tableInfoName tableInfo) columns deletePerms . fmap IR.MOutMultirowFields

-- | Construct a root field, normally called delete_tablename_by_pk, that can be used to delete an
-- individual rows from a DB table, specified by primary key. Select permissions are required, as
-- the user must be allowed to access all the primary keys of the table.
deleteFromTableByPk ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  -- | table source
  SourceName ->
  -- | table info
  TableInfo b ->
  -- | field display name
  G.Name ->
  -- | field description, if any
  Maybe G.Description ->
  -- | delete permissions of the table
  DelPermInfo b ->
  -- | select permissions of the table
  SelPermInfo b ->
  m (Maybe (FieldParser n (IR.AnnDelG b (IR.RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
deleteFromTableByPk sourceName tableInfo fieldName description deletePerms selectPerms = runMaybeT $ do
  let columns = tableColumns tableInfo
  pkArgs <- MaybeT $ primaryKeysArguments tableInfo selectPerms
  selection <- lift $ tableSelectionSet sourceName tableInfo selectPerms
  pure $
    P.subselection fieldName description pkArgs selection
      <&> mkDeleteObject (tableInfoName tableInfo) columns deletePerms . fmap IR.MOutSinglerowObject

mkDeleteObject ::
  Backend b =>
  TableName b ->
  [ColumnInfo b] ->
  DelPermInfo b ->
  (AnnBoolExp b (UnpreparedValue b), IR.MutationOutputG b (IR.RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)) ->
  IR.AnnDelG b (IR.RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)
mkDeleteObject table columns deletePerms (whereExp, mutationOutput) =
  IR.AnnDel
    { IR.dqp1Table = table,
      IR.dqp1Where = (permissionFilter, whereExp),
      IR.dqp1Output = mutationOutput,
      IR.dqp1AllCols = columns
    }
  where
    permissionFilter = fmap partialSQLExpToUnpreparedValue <$> dpiFilter deletePerms

-- common

-- | All mutations allow returning results, such as what the updated database
-- rows look like. This parser allows a query to specify what data to fetch.
mutationSelectionSet ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  SourceName ->
  TableInfo b ->
  Maybe (SelPermInfo b) ->
  m (Parser 'Output n (IR.MutFldsG b (IR.RemoteRelationshipField UnpreparedValue) (UnpreparedValue b)))
mutationSelectionSet sourceName tableInfo selectPerms =
  memoizeOn 'mutationSelectionSet (sourceName, tableName) do
    tableGQLName <- getTableGQLName tableInfo
    returning <- runMaybeT do
      permissions <- hoistMaybe selectPerms
      tableSet <- lift $ tableSelectionList sourceName tableInfo permissions
      let returningName = $$(G.litName "returning")
          returningDesc = "data from the rows affected by the mutation"
      pure $ IR.MRet <$> P.subselection_ returningName (Just returningDesc) tableSet
    selectionName <- P.mkTypename $ tableGQLName <> $$(G.litName "_mutation_response")
    let affectedRowsName = $$(G.litName "affected_rows")
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
  SelPermInfo b ->
  m (Maybe (InputFieldsParser n (AnnBoolExp b (UnpreparedValue b))))
primaryKeysArguments tableInfo selectPerms = runMaybeT $ do
  primaryKeys <- hoistMaybe $ _tciPrimaryKey . _tiCoreInfo $ tableInfo
  let columns = _pkColumns primaryKeys
  guard $ all (\c -> ciColumn c `Map.member` spiCols selectPerms) columns
  lift $
    fmap (BoolAnd . toList) . sequenceA <$> for columns \columnInfo -> do
      field <- columnParser (ciType columnInfo) (G.Nullability False)
      pure $
        BoolFld . AVColumn columnInfo . pure . AEQ True . mkParameter
          <$> P.field (ciName columnInfo) (ciDescription columnInfo) field
