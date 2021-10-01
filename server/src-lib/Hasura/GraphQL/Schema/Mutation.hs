{-# LANGUAGE ApplicativeDo #-}

module Hasura.GraphQL.Schema.Mutation
  ( insertIntoTable,
    insertOneIntoTable,
    updateTable,
    updateTableByPk,
    deleteFromTable,
    deleteFromTableByPk,
    mkDefaultRelationshipParser,
    defaultConflictObject,
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
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Update qualified as IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

-- insert

-- | Construct the parser for a field that can be used to add several rows to a DB table.
--
-- This function is used to create the insert_tablename root field.
-- The field accepts the following arguments:
--   - objects: the list of objects to insert into the table (see 'tableFieldsInput')
--   - on_conflict: an object describing how to perform an upsert in case of conflict
insertIntoTable ::
  forall b r m n.
  MonadBuildSchema b r m n =>
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
  m (FieldParser n (IR.AnnInsert b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b)))
insertIntoTable sourceName tableInfo fieldName description insertPerms selectPerms updatePerms = do
  selectionParser <- mutationSelectionSet sourceName tableInfo selectPerms
  objectParser <- tableFieldsInput sourceName tableInfo insertPerms
  conflictParser <- withJust updatePerms $ conflictObject sourceName tableInfo selectPerms
  let argsParser = do
        conflict <- mkConflictArg conflictParser
        objects <- mkObjectsArg objectParser
        pure $ mkInsertObject objects tableInfo conflict insertPerms updatePerms
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
  m (FieldParser n (IR.AnnInsert b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b)))
insertOneIntoTable sourceName tableInfo fieldName description insertPerms selectPerms updatePerms = do
  selectionParser <- tableSelectionSet sourceName tableInfo selectPerms
  objectParser <- tableFieldsInput sourceName tableInfo insertPerms
  conflictParser <- withJust updatePerms $ conflictObject sourceName tableInfo (Just selectPerms)
  let argsParser = do
        conflict <- mkConflictArg conflictParser
        object <- mkObjectArg objectParser
        pure $ mkInsertObject [object] tableInfo conflict insertPerms updatePerms
  pure $
    P.subselection fieldName description argsParser selectionParser
      <&> \(insertObject, output) -> IR.AnnInsert (G.unName fieldName) True insertObject (IR.MOutSinglerowObject output)
  where
    mkObjectArg objectParser =
      P.field
        $$(G.litName "object")
        (Just "the row to be inserted")
        objectParser

-- | Creates a field parser for the "on_conflict" argument of insert fields.
--
-- The parser might not exist, as the current role might not have the
-- appropriate permissions, but insert fields can exist regardless. This
-- function creates a dummy 'InputFieldsParser' that always returns @Nothing@ in
-- such a case.
mkConflictArg ::
  MonadParse n =>
  Maybe (Parser 'Input n (XOnConflict b, IR.ConflictClauseP1 b (UnpreparedValue b))) ->
  InputFieldsParser n (Maybe (XOnConflict b, IR.ConflictClauseP1 b (UnpreparedValue b)))
mkConflictArg conflictParser = withJust conflictParser $ P.fieldOptional conflictName (Just conflictDesc)
  where
    conflictName = $$(G.litName "on_conflict")
    conflictDesc = "on conflict condition"

-- | Creates the parser for an input object for a row of the given table.
--
-- This function creates an input object type named "tablename_insert_input" in
-- the GraphQL shema, which has a field for each of the columns of that table
-- that the user has insert permissions for.
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
    let objectName = tableGQLName <> $$(G.litName "_insert_input")
        objectDesc = G.Description $ "input type for inserting data into table " <>> tableName
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
      FIColumn columnInfo -> mkColumnParser columnInfo
      FIRelationship relInfo -> mkRelationshipParser sourceName relInfo

    mkColumnParser ::
      ColumnInfo b ->
      m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsert b (UnpreparedValue b)))))
    mkColumnParser columnInfo = do
      let columnName = pgiName columnInfo
          columnDesc = pgiDescription columnInfo
          isAllowed = Set.member (pgiColumn columnInfo) (ipiCols insertPerms)
      whenMaybe isAllowed do
        fieldParser <- columnParser (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
        pure $
          P.fieldOptional columnName columnDesc fieldParser `mapField` \value ->
            IR.AIColumn (pgiColumn columnInfo, mkParameter value)

mkDefaultRelationshipParser ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  XNestedInserts b ->
  SourceName ->
  RelInfo b ->
  m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsert b (UnpreparedValue b)))))
mkDefaultRelationshipParser xNestedInserts sourceName relationshipInfo = runMaybeT do
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
      parser <- objectRelationshipInput sourceName otherTableInfo insPerms selPerms updPerms
      pure $
        P.fieldOptional relFieldName Nothing (P.nullable parser) <&> \objRelIns -> do
          rel <- join objRelIns
          Just $ IR.AIObjectRelationship xNestedInserts $ IR.RelIns rel relationshipInfo
    ArrRel -> do
      parser <- arrayRelationshipInput sourceName otherTableInfo insPerms selPerms updPerms
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
  SourceName ->
  TableInfo b ->
  InsPermInfo b ->
  Maybe (SelPermInfo b) ->
  Maybe (UpdPermInfo b) ->
  m (Parser 'Input n (IR.SingleObjIns b (UnpreparedValue b)))
objectRelationshipInput sourceName tableInfo insertPerms selectPerms updatePerms =
  memoizeOn 'objectRelationshipInput (sourceName, tableName) do
    tableGQLName <- getTableGQLName tableInfo
    objectParser <- tableFieldsInput sourceName tableInfo insertPerms
    conflictParser <- withJust updatePerms $ conflictObject sourceName tableInfo selectPerms
    let objectName = $$(G.litName "data")
        inputName = tableGQLName <> $$(G.litName "_obj_rel_insert_input")
        inputDesc = G.Description $ "input type for inserting object relation for remote table " <>> tableName
        inputParser = do
          conflict <- mkConflictArg conflictParser
          object <- P.field objectName Nothing objectParser
          pure $ mkInsertObject (IR.Single object) tableInfo conflict insertPerms updatePerms
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
  SourceName ->
  TableInfo b ->
  InsPermInfo b ->
  Maybe (SelPermInfo b) ->
  Maybe (UpdPermInfo b) ->
  m (Parser 'Input n (IR.MultiObjIns b (UnpreparedValue b)))
arrayRelationshipInput sourceName tableInfo insertPerms selectPerms updatePerms =
  memoizeOn 'arrayRelationshipInput (sourceName, tableName) do
    tableGQLName <- getTableGQLName tableInfo
    objectParser <- tableFieldsInput sourceName tableInfo insertPerms
    conflictParser <- withJust updatePerms $ conflictObject sourceName tableInfo selectPerms
    let objectsName = $$(G.litName "data")
        inputName = tableGQLName <> $$(G.litName "_arr_rel_insert_input")
        inputDesc = G.Description $ "input type for inserting array relation for remote table " <>> tableName
        inputParser = do
          conflict <- mkConflictArg conflictParser
          objects <- P.field objectsName Nothing $ P.list objectParser
          pure $ mkInsertObject objects tableInfo conflict insertPerms updatePerms
    pure $ P.object inputName (Just inputDesc) inputParser
  where
    tableName = tableInfoName tableInfo

-- | Helper function that creates an 'AnnIns' object.
mkInsertObject ::
  forall b f.
  BackendSchema b =>
  f (IR.AnnotatedInsertRow b (UnpreparedValue b)) ->
  TableInfo b ->
  Maybe (XOnConflict b, IR.ConflictClauseP1 b (UnpreparedValue b)) ->
  InsPermInfo b ->
  Maybe (UpdPermInfo b) ->
  IR.AnnIns b f (UnpreparedValue b)
mkInsertObject objects tableInfo conflictClause insertPerms updatePerms =
  IR.AnnIns
    { _aiInsObj = objects,
      _aiTableName = table,
      _aiConflictClause = conflictClause,
      _aiCheckCond = (insertCheck, updateCheck),
      _aiTableCols = columns,
      _aiDefVals = defaultValues,
      _aiExtraInsertData = extraInsertData
    }
  where
    table = tableInfoName tableInfo
    columns = tableColumns tableInfo
    extraInsertData = getExtraInsertData tableInfo
    insertCheck = fmap partialSQLExpToUnpreparedValue <$> ipiCheck insertPerms
    updateCheck = (fmap . fmap . fmap) partialSQLExpToUnpreparedValue $ upiCheck =<< updatePerms
    defaultValues =
      Map.union (partialSQLExpToUnpreparedValue <$> ipiSet insertPerms) $
        Map.fromList [(column, UVLiteral $ columnDefaultValue @b column) | column <- pgiColumn <$> columns]

-- | Creates a parser for the "_on_conflict" object of the given table.
--
-- This object is used to generate the "ON CONFLICT" SQL clause: what should be
-- done if an insert raises a conflict? It may not always exist: it can't be
-- created if there aren't any unique or primary keys constraints. However, if
-- there are no columns for which the current role has update permissions, we
-- must still accept an empty list for `update_columns`; we do this by adding a
-- placeholder value to the enum (see 'tableUpdateColumnsEnum').
defaultConflictObject ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  XOnConflict b ->
  SourceName ->
  TableInfo b ->
  Maybe (SelPermInfo b) ->
  UpdPermInfo b ->
  m (Maybe (Parser 'Input n (XOnConflict b, IR.ConflictClauseP1 b (UnpreparedValue b))))
defaultConflictObject xOnConflict sourceName tableInfo selectPerms updatePerms = runMaybeT $ do
  tableGQLName <- getTableGQLName tableInfo
  columnsEnum <- lift $ tableUpdateColumnsEnum tableInfo updatePerms
  constraints <- hoistMaybe $ tciUniqueOrPrimaryKeyConstraints . _tiCoreInfo $ tableInfo
  constraintParser <- lift $ conflictConstraint constraints sourceName tableInfo
  whereExpParser <- lift $ boolExp sourceName tableInfo selectPerms
  let presetColumns = partialSQLExpToUnpreparedValue <$> upiSet updatePerms
      updateFilter = fmap partialSQLExpToUnpreparedValue <$> upiFilter updatePerms
      objectName = tableGQLName <> $$(G.litName "_on_conflict")
      objectDesc = G.Description $ "on conflict condition type for table " <>> tableInfoName tableInfo
      constraintName = $$(G.litName "constraint")
      columnsName = $$(G.litName "update_columns")
      whereExpName = $$(G.litName "where")
  pure $
    P.object objectName (Just objectDesc) $ do
      constraint <- IR.CTConstraint <$> P.field constraintName Nothing constraintParser
      whereExp <- P.fieldOptional whereExpName Nothing whereExpParser
      columns <-
        P.fieldWithDefault columnsName Nothing (G.VList []) (P.list columnsEnum) `P.bindFields` \cs ->
          -- this can only happen if the placeholder was used
          sequenceA cs `onNothing` parseError "erroneous column name"
      pure $
        (xOnConflict,) $ case columns of
          [] -> IR.CP1DoNothing $ Just constraint
          _ -> IR.CP1Update constraint columns presetColumns $ BoolAnd $ updateFilter : maybeToList whereExp

-- | Constructs a Parser for the name of the constraints on a given table.
--
-- The TableCoreInfo of a given table contains a list of unique or primary key
-- constraints. Given the list of such constraints, this function creates a
-- parser for an enum type that matches it. This function makes no attempt at
-- de-duplicating contraint names, and assumes they are correct.
--
-- This function can fail if a constraint has a name that cannot be translated
-- to a GraphQL name (see hasura/graphql-engine-mono#1748).
conflictConstraint ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  NonEmpty (Constraint b) ->
  SourceName ->
  TableInfo b ->
  m (Parser 'Both n (ConstraintName b))
conflictConstraint constraints sourceName tableInfo =
  memoizeOn 'conflictConstraint (sourceName, tableName) $ do
    tableGQLName <- getTableGQLName tableInfo
    constraintEnumValues <- for constraints \constraint -> do
      name <- textToName $ toTxt $ _cName constraint
      pure
        ( P.mkDefinition name (Just "unique or primary key constraint") P.EnumValueInfo,
          _cName constraint
        )
    let enumName = tableGQLName <> $$(G.litName "_constraint")
        enumDesc = G.Description $ "unique or primary key constraints on table " <>> tableName
    pure $ P.enum enumName (Just enumDesc) constraintEnumValues
  where
    tableName = tableInfoName tableInfo

-- update

-- | Construct a root field, normally called update_tablename, that can be used
-- to update rows in a DB table specified by filters. Only returns a parser if
-- there are columns the user is allowed to update; otherwise returns Nothing.
updateTable ::
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
  -- | update permissions of the table
  UpdPermInfo b ->
  -- | select permissions of the table (if any)
  Maybe (SelPermInfo b) ->
  m (Maybe (FieldParser n (IR.AnnUpdG b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b))))
updateTable sourceName tableInfo fieldName description updatePerms selectPerms = runMaybeT $ do
  let tableName = tableInfoName tableInfo
      columns = tableColumns tableInfo
      whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be updated"
  opArgs <- MaybeT $ updateOperators tableInfo updatePerms
  whereArg <- lift $ P.field whereName (Just whereDesc) <$> boolExp sourceName tableInfo selectPerms
  selection <- lift $ mutationSelectionSet sourceName tableInfo selectPerms
  let argsParser = liftA2 (,) opArgs whereArg
  pure $
    P.subselection fieldName description argsParser selection
      <&> mkUpdateObject tableName columns updatePerms . fmap IR.MOutMultirowFields

-- | Construct a root field, normally called update_tablename, that can be used
-- to update a single in a DB table, specified by primary key. Only returns a
-- parser if there are columns the user is allowed to update and if the user has
-- select permissions on all primary keys; otherwise returns Nothing.
updateTableByPk ::
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
  -- | update permissions of the table
  UpdPermInfo b ->
  -- | select permissions of the table
  SelPermInfo b ->
  m (Maybe (FieldParser n (IR.AnnUpdG b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b))))
updateTableByPk sourceName tableInfo fieldName description updatePerms selectPerms = runMaybeT $ do
  let columns = tableColumns tableInfo
      tableName = tableInfoName tableInfo
  tableGQLName <- getTableGQLName tableInfo
  pkArgs <- MaybeT $ primaryKeysArguments tableInfo selectPerms
  opArgs <- MaybeT $ updateOperators tableInfo updatePerms
  selection <- lift $ tableSelectionSet sourceName tableInfo selectPerms
  let pkFieldName = $$(G.litName "pk_columns")
      pkObjectName = tableGQLName <> $$(G.litName "_pk_columns_input")
      pkObjectDesc = G.Description $ "primary key columns input for table: " <> G.unName tableGQLName
      pkParser = P.object pkObjectName (Just pkObjectDesc) pkArgs
      argsParser = do
        operators <- opArgs
        primaryKeys <- P.field pkFieldName Nothing pkParser
        pure (operators, primaryKeys)
  pure $
    P.subselection fieldName description argsParser selection
      <&> mkUpdateObject tableName columns updatePerms . fmap IR.MOutSinglerowObject

mkUpdateObject ::
  Backend b =>
  TableName b ->
  [ColumnInfo b] ->
  UpdPermInfo b ->
  ( ( [(Column b, IR.UpdOpExpG (UnpreparedValue b))],
      AnnBoolExp b (UnpreparedValue b)
    ),
    IR.MutationOutputG b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b)
  ) ->
  IR.AnnUpdG b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b)
mkUpdateObject table columns updatePerms ((opExps, whereExp), mutationOutput) =
  IR.AnnUpd
    { IR.uqp1Table = table,
      IR.uqp1OpExps = opExps,
      IR.uqp1Where = (permissionFilter, whereExp),
      IR.uqp1Check = checkExp,
      IR.uqp1Output = mutationOutput,
      IR.uqp1AllCols = columns
    }
  where
    permissionFilter = fmap partialSQLExpToUnpreparedValue <$> upiFilter updatePerms
    checkExp = maybe annBoolExpTrue ((fmap . fmap) partialSQLExpToUnpreparedValue) $ upiCheck updatePerms

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
  m (FieldParser n (IR.AnnDelG b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b)))
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
  m (Maybe (FieldParser n (IR.AnnDelG b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b))))
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
  (AnnBoolExp b (UnpreparedValue b), IR.MutationOutputG b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b)) ->
  IR.AnnDelG b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b)
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
  m (Parser 'Output n (IR.MutFldsG b (IR.RemoteSelect UnpreparedValue) (UnpreparedValue b)))
mutationSelectionSet sourceName tableInfo selectPerms =
  memoizeOn 'mutationSelectionSet (sourceName, tableName) do
    tableGQLName <- getTableGQLName tableInfo
    returning <- runMaybeT do
      permissions <- hoistMaybe selectPerms
      tableSet <- lift $ tableSelectionList sourceName tableInfo permissions
      let returningName = $$(G.litName "returning")
          returningDesc = "data from the rows affected by the mutation"
      pure $ IR.MRet <$> P.subselection_ returningName (Just returningDesc) tableSet
    let affectedRowsName = $$(G.litName "affected_rows")
        affectedRowsDesc = "number of rows affected by the mutation"
        selectionName = tableGQLName <> $$(G.litName "_mutation_response")
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
primaryKeysArguments ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  TableInfo b ->
  SelPermInfo b ->
  m (Maybe (InputFieldsParser n (AnnBoolExp b (UnpreparedValue b))))
primaryKeysArguments tableInfo selectPerms = runMaybeT $ do
  primaryKeys <- hoistMaybe $ _tciPrimaryKey . _tiCoreInfo $ tableInfo
  let columns = _pkColumns primaryKeys
  guard $ all (\c -> pgiColumn c `Map.member` spiCols selectPerms) columns
  lift $
    fmap (BoolAnd . toList) . sequenceA <$> for columns \columnInfo -> do
      field <- columnParser (pgiType columnInfo) (G.Nullability False)
      pure $
        BoolFld . AVColumn columnInfo . pure . AEQ True . mkParameter
          <$> P.field (pgiName columnInfo) (pgiDescription columnInfo) field
