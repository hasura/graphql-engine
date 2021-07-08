{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Schema.Mutation
  ( insertIntoTable
  , insertOneIntoTable
  , updateTable
  , updateTableByPk
  , deleteFromTable
  , deleteFromTableByPk
  ) where


import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Text.Extended

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.IR.Delete          as IR
import qualified Hasura.RQL.IR.Insert          as IR
import qualified Hasura.RQL.IR.Returning       as IR
import qualified Hasura.RQL.IR.Update          as IR

import           Hasura.GraphQL.Parser         (FieldParser, InputFieldsParser, Kind (..), Parser,
                                                UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types


-- insert

-- | Construct a root field, normally called insert_tablename, that can be used to add several rows to a DB table
insertIntoTable
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> TableInfo b            -- ^ qualified name of the table
  -> G.Name                 -- ^ field display name
  -> Maybe G.Description    -- ^ field description, if any
  -> InsPermInfo b          -- ^ insert permissions of the table
  -> Maybe (SelPermInfo b)  -- ^ select permissions of the table (if any)
  -> Maybe (UpdPermInfo b)  -- ^ update permissions of the table (if any)
  -> m (FieldParser n (IR.AnnInsert b UnpreparedValue (UnpreparedValue b)))
insertIntoTable sourceName tableInfo fieldName description insertPerms selectPerms updatePerms = do
  let columns = tableColumns tableInfo
  selectionParser <- mutationSelectionSet sourceName tableInfo selectPerms
  objectsParser   <- P.list <$> tableFieldsInput sourceName tableInfo insertPerms
  conflictParser  <- fmap join $ sequenceA $ conflictObject sourceName tableInfo selectPerms <$> updatePerms
  let objectsName  = $$(G.litName "objects")
      objectsDesc  = "the rows to be inserted"
      argsParser   = do
        conflictClause <- mkConflictClause conflictParser
        objects <- P.field objectsName (Just objectsDesc) objectsParser
        pure (conflictClause, objects)
  pure $ P.subselection fieldName description argsParser selectionParser
    <&> \((conflictClause, objects), output) -> IR.AnnInsert (G.unName fieldName) False
      ( mkInsertObject objects (tableInfoName tableInfo) columns conflictClause insertPerms updatePerms )
      ( IR.MOutMultirowFields output )

mkConflictClause :: MonadParse n => Maybe (Parser 'Input n a) -> InputFieldsParser n (Maybe a)
mkConflictClause conflictParser
  = maybe
    (pure Nothing) -- Empty InputFieldsParser (no arguments allowed)
    (P.fieldOptional conflictName (Just conflictDesc))
    conflictParser
  where
    conflictName = $$(G.litName "on_conflict")
    conflictDesc = "on conflict condition"

-- | Variant of 'insertIntoTable' that inserts a single row
insertOneIntoTable
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName             -- ^ source of the table
  -> TableInfo b            -- ^ table info
  -> G.Name                 -- ^ field display name
  -> Maybe G.Description    -- ^ field description, if any
  -> InsPermInfo b          -- ^ insert permissions of the table
  -> SelPermInfo b          -- ^ select permissions of the table
  -> Maybe (UpdPermInfo b)  -- ^ update permissions of the table (if any)
  -> m (FieldParser n (IR.AnnInsert b UnpreparedValue (UnpreparedValue b)))
insertOneIntoTable sourceName tableInfo fieldName description insertPerms selectPerms updatePerms = do
  let columns = tableColumns tableInfo
  selectionParser <- tableSelectionSet sourceName tableInfo selectPerms
  objectParser    <- tableFieldsInput sourceName tableInfo insertPerms
  conflictParser  <- fmap join $ sequenceA $ conflictObject sourceName tableInfo (Just selectPerms) <$> updatePerms
  let objectName    = $$(G.litName "object")
      objectDesc    = "the row to be inserted"
      argsParser    = do
        conflictClause <- mkConflictClause conflictParser
        object <- P.field objectName (Just objectDesc) objectParser
        pure (conflictClause, object)
  pure $ P.subselection fieldName description argsParser selectionParser
    <&> \((conflictClause, object), output) -> IR.AnnInsert (G.unName fieldName) True
       ( mkInsertObject [object] (tableInfoName tableInfo) columns conflictClause insertPerms updatePerms )
       ( IR.MOutSinglerowObject output )

-- | We specify the data of an individual row to insert through this input parser.
tableFieldsInput
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> TableInfo b     -- ^ qualified name of the table
  -> InsPermInfo b   -- ^ insert permissions of the table
  -> m (Parser 'Input n (IR.AnnInsObj b (UnpreparedValue b)))
tableFieldsInput sourceName tableInfo insertPerms = memoizeOn 'tableFieldsInput (sourceName, tableName) do
  tableGQLName <- getTableGQLName tableInfo
  roleName     <- askRoleName
  let allFields = _tciFieldInfoMap . _tiCoreInfo $ tableInfo
  objectFields <- catMaybes <$> for (Map.elems allFields) \case
    FIComputedField _ -> pure Nothing
    FIRemoteRelationship _ -> pure Nothing
    FIColumn columnInfo ->
      whenMaybe (Set.member (pgiColumn columnInfo) (ipiCols insertPerms)) do
        let columnName = pgiName columnInfo
            columnDesc = pgiDescription columnInfo
        fieldParser <- columnParser (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
        pure $ P.fieldOptional columnName columnDesc fieldParser `mapField`
          \(mkParameter -> value) -> IR.AnnInsObj [(pgiColumn columnInfo, value)] [] []
    FIRelationship relationshipInfo -> runMaybeT $ do
      let otherTableName  = riRTable  relationshipInfo
          relName         = riName    relationshipInfo
      otherTableInfo <- askTableInfo sourceName otherTableName
      permissions  <- MaybeT $ tablePermissions otherTableInfo
      relFieldName <- lift $ textToName $ relNameToTxt relName
      insPerms     <- hoistMaybe $ _permIns permissions
      let selPerms = _permSel permissions
          updPerms = _permUpd permissions
      lift $ case riType relationshipInfo of
        ObjRel -> do
          parser <- objectRelationshipInput sourceName otherTableInfo insPerms selPerms updPerms
          pure $ P.fieldOptional relFieldName Nothing parser `mapField`
            \objRelIns -> IR.AnnInsObj [] [IR.RelIns objRelIns relationshipInfo] []
        ArrRel -> do
          parser <- P.nullable <$> arrayRelationshipInput sourceName otherTableInfo insPerms selPerms updPerms
          pure $ P.fieldOptional relFieldName Nothing parser <&> \arrRelIns -> do
            rel <- join arrRelIns
            Just $ IR.AnnInsObj [] [] [IR.RelIns rel relationshipInfo | not $ null $ IR._aiInsObj rel]
  let objectName = tableGQLName <> $$(G.litName "_insert_input")
      objectDesc = G.Description $ "input type for inserting data into table " <>> tableName
  pure $ P.object objectName (Just objectDesc) $ catMaybes <$> sequenceA objectFields
    <&> mconcat
    where
      tableName = tableInfoName tableInfo

-- | Used by 'tableFieldsInput' for object data that is nested through object relationships
objectRelationshipInput
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> TableInfo b
  -> InsPermInfo b
  -> Maybe (SelPermInfo b)
  -> Maybe (UpdPermInfo b)
  -> m (Parser 'Input n (IR.SingleObjIns b (UnpreparedValue b)))
objectRelationshipInput sourceName tableInfo insertPerms selectPerms updatePerms =
  memoizeOn 'objectRelationshipInput (sourceName, tableName) do
  tableGQLName   <- getTableGQLName tableInfo
  let columns = tableColumns tableInfo
  objectParser   <- tableFieldsInput sourceName tableInfo insertPerms
  conflictParser <- fmap join $ sequenceA $ conflictObject sourceName tableInfo selectPerms <$> updatePerms
  let objectName   = $$(G.litName "data")
      inputName    = tableGQLName <> $$(G.litName "_obj_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting object relation for remote table " <>> tableName
      inputParser = do
        conflictClause <- mkConflictClause conflictParser
        object <- P.field objectName Nothing objectParser
        pure $ mkInsertObject (IR.Single object) tableName columns conflictClause insertPerms updatePerms
  pure $ P.object inputName (Just inputDesc) inputParser
  where tableName = tableInfoName tableInfo

-- | Used by 'tableFieldsInput' for object data that is nested through object relationships
arrayRelationshipInput
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> TableInfo b
  -> InsPermInfo b
  -> Maybe (SelPermInfo b)
  -> Maybe (UpdPermInfo b)
  -> m (Parser 'Input n (IR.MultiObjIns b (UnpreparedValue b)))
arrayRelationshipInput sourceName tableInfo insertPerms selectPerms updatePerms =
  memoizeOn 'arrayRelationshipInput (sourceName, tableName) do
  tableGQLName      <- getTableGQLName tableInfo
  let columns = tableColumns tableInfo
  objectParser   <- tableFieldsInput sourceName tableInfo insertPerms
  conflictParser <- fmap join $ sequenceA $ conflictObject sourceName tableInfo selectPerms <$> updatePerms
  let objectsName  = $$(G.litName "data")
      inputName    = tableGQLName <> $$(G.litName "_arr_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting array relation for remote table " <>> tableName
      inputParser = do
        conflictClause <- mkConflictClause conflictParser
        objects <- P.field objectsName Nothing $ P.list objectParser
        pure $ mkInsertObject objects tableName columns conflictClause insertPerms updatePerms
  pure $ P.object inputName (Just inputDesc) inputParser
  where tableName = tableInfoName tableInfo

mkInsertObject
  :: forall b f
   . BackendSchema b
  => f (IR.AnnInsObj b (UnpreparedValue b))
  -> TableName b
  -> [ColumnInfo b]
  -> Maybe (IR.ConflictClauseP1 b (UnpreparedValue b))
  -> InsPermInfo b
  -> Maybe (UpdPermInfo b)
  -> IR.AnnIns b f (UnpreparedValue b)
mkInsertObject objects table columns conflictClause insertPerms updatePerms =
  IR.AnnIns { _aiInsObj         = objects
            , _aiTableName      = table
            , _aiConflictClause = conflictClause
            , _aiCheckCond      = (insertCheck, updateCheck)
            , _aiTableCols      = columns
            , _aiDefVals        = defaultValues
            }
  where insertCheck = (fmap . fmap) partialSQLExpToUnpreparedValue $ ipiCheck insertPerms
        updateCheck = (fmap . fmap) partialSQLExpToUnpreparedValue <$> (upiCheck =<< updatePerms)
        defaultValues = Map.union (partialSQLExpToUnpreparedValue <$> ipiSet insertPerms)
          $ Map.fromList [(column, UVLiteral $ columnDefaultValue @b column) | column <- pgiColumn <$> columns]

-- | Specifies the "ON CONFLICT" SQL clause.
-- This object cannot exist if there aren't any unique or primary keys constraints. However,
-- if there are no columns for which the current role has update permissions, we must still
-- accept an empty list for `update_columns`; we do this by adding a placeholder value to
-- the enum (see 'tableUpdateColumnsEnum').
conflictObject
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> TableInfo b
  -> Maybe (SelPermInfo b)
  -> UpdPermInfo b
  -> m (Maybe (Parser 'Input n (IR.ConflictClauseP1 b (UnpreparedValue b))))
conflictObject sourceName tableInfo selectPerms updatePerms = runMaybeT $ do
  tableGQLName     <- getTableGQLName tableInfo
  columnsEnum      <- lift $ tableUpdateColumnsEnum tableInfo updatePerms
  constraints      <- hoistMaybe $ tciUniqueOrPrimaryKeyConstraints . _tiCoreInfo $ tableInfo
  constraintParser <- lift $ conflictConstraint constraints sourceName tableInfo
  whereExpParser   <- lift $ boolExp sourceName tableInfo selectPerms
  let objectName = tableGQLName <> $$(G.litName "_on_conflict")
      objectDesc = G.Description $ "on conflict condition type for table " <>> tableInfoName tableInfo
      constraintName = $$(G.litName "constraint")
      columnsName    = $$(G.litName "update_columns")
      whereExpName   = $$(G.litName "where")
      fieldsParser = do
        constraint <- IR.CTConstraint <$> P.field constraintName Nothing constraintParser
        whereExp   <- P.fieldOptional whereExpName Nothing whereExpParser
        columns    <- P.fieldWithDefault columnsName Nothing (G.VList []) (P.list columnsEnum) `P.bindFields` \cs ->
          -- this can only happen if the placeholder was used
          sequenceA cs `onNothing` parseError "erroneous column name"
        pure $ case columns of
          [] -> IR.CP1DoNothing $ Just constraint
          _  -> IR.CP1Update constraint columns preSetColumns $
            BoolAnd $ catMaybes [whereExp, Just $ (fmap . fmap) partialSQLExpToUnpreparedValue $ upiFilter updatePerms]
  pure $ P.object objectName (Just objectDesc) fieldsParser
  where preSetColumns = partialSQLExpToUnpreparedValue <$> upiSet updatePerms

conflictConstraint
  :: forall b r m n
   . MonadBuildSchema b r m n
  => NonEmpty (Constraint b)
  -> SourceName
  -> TableInfo b
  -> m (Parser 'Both n (ConstraintName b))
conflictConstraint constraints sourceName tableInfo = memoizeOn 'conflictConstraint (sourceName, tableName) $ do
  tableGQLName <- getTableGQLName tableInfo
  constraintEnumValues <- for constraints \constraint -> do
    name <- textToName $ toTxt $ _cName constraint
    pure ( P.mkDefinition name (Just "unique or primary key constraint") P.EnumValueInfo
         , _cName constraint
         )
  let enumName  = tableGQLName <> $$(G.litName "_constraint")
      enumDesc  = G.Description $ "unique or primary key constraints on table " <>> tableName
  pure $ P.enum enumName (Just enumDesc) constraintEnumValues
  where
    tableName = tableInfoName tableInfo


-- update

-- | Construct a root field, normally called update_tablename, that can be used
-- to update rows in a DB table specified by filters. Only returns a parser if
-- there are columns the user is allowed to update; otherwise returns Nothing.
updateTable
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName               -- ^ table source
  -> TableInfo b              -- ^ table info
  -> G.Name                   -- ^ field display name
  -> Maybe G.Description      -- ^ field description, if any
  -> UpdPermInfo b            -- ^ update permissions of the table
  -> Maybe (SelPermInfo b)    -- ^ select permissions of the table (if any)
  -> m (Maybe (FieldParser n (IR.AnnUpdG b UnpreparedValue (UnpreparedValue b))))
updateTable sourceName tableInfo fieldName description updatePerms selectPerms = runMaybeT $ do
  let whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be updated"
  opArgs    <- MaybeT $ updateOperators tableInfo updatePerms
  let columns = tableColumns tableInfo
  whereArg  <- lift $ P.field whereName (Just whereDesc) <$> boolExp sourceName tableInfo selectPerms
  selection <- lift $ mutationSelectionSet sourceName tableInfo selectPerms
  let argsParser = liftA2 (,) opArgs whereArg
  pure $ P.subselection fieldName description argsParser selection
    <&> mkUpdateObject tableName columns updatePerms . fmap IR.MOutMultirowFields
  where tableName = tableInfoName tableInfo

-- | Construct a root field, normally called update_tablename, that can be used
-- to update a single in a DB table, specified by primary key. Only returns a
-- parser if there are columns the user is allowed to update and if the user has
-- select permissions on all primary keys; otherwise returns Nothing.
updateTableByPk
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName           -- ^ table source
  -> TableInfo b          -- ^ table info
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> UpdPermInfo b        -- ^ update permissions of the table
  -> SelPermInfo b        -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (IR.AnnUpdG b UnpreparedValue (UnpreparedValue b))))
updateTableByPk sourceName tableInfo fieldName description updatePerms selectPerms = runMaybeT $ do
  tableGQLName <- getTableGQLName tableInfo
  let columns = tableColumns tableInfo
  pkArgs       <- MaybeT $ primaryKeysArguments tableInfo selectPerms
  opArgs       <- MaybeT $ updateOperators tableInfo updatePerms
  selection    <- lift $ tableSelectionSet sourceName tableInfo selectPerms
  let pkFieldName  = $$(G.litName "pk_columns")
      pkObjectName = tableGQLName <> $$(G.litName "_pk_columns_input")
      pkObjectDesc = G.Description $ "primary key columns input for table: " <> G.unName tableGQLName
      argsParser   = do
        operators <- opArgs
        primaryKeys <- P.field pkFieldName Nothing $ P.object pkObjectName (Just pkObjectDesc) pkArgs
        pure (operators, primaryKeys)
  pure $ P.subselection fieldName description argsParser selection
    <&> mkUpdateObject tableName columns updatePerms . fmap IR.MOutSinglerowObject
  where tableName = tableInfoName tableInfo

mkUpdateObject
  :: Backend b
  => TableName b
  -> [ColumnInfo b]
  -> UpdPermInfo b
  -> ( ( [(Column b, IR.UpdOpExpG (UnpreparedValue b))]
       , AnnBoolExp b (UnpreparedValue b)
       )
     , IR.MutationOutputG b UnpreparedValue (UnpreparedValue b)
     )
  -> IR.AnnUpdG b UnpreparedValue (UnpreparedValue b)
mkUpdateObject table columns updatePerms ((opExps, whereExp), mutationOutput) =
  IR.AnnUpd { IR.uqp1Table   = table
            , IR.uqp1OpExps  = opExps
            , IR.uqp1Where   = (permissionFilter, whereExp)
            , IR.uqp1Check   = checkExp
            , IR.uqp1Output  = mutationOutput
            , IR.uqp1AllCols = columns
            }
  where
    permissionFilter = (fmap . fmap) partialSQLExpToUnpreparedValue $ upiFilter updatePerms
    checkExp = maybe annBoolExpTrue ((fmap . fmap) partialSQLExpToUnpreparedValue) $ upiCheck updatePerms


-- delete

-- | Construct a root field, normally called delete_tablename, that can be used
-- to delete several rows from a DB table
deleteFromTable
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName             -- ^ table source
  -> TableInfo b            -- ^ table info
  -> G.Name                 -- ^ field display name
  -> Maybe G.Description    -- ^ field description, if any
  -> DelPermInfo b          -- ^ delete permissions of the table
  -> Maybe (SelPermInfo b)  -- ^ select permissions of the table (if any)
  -> m (FieldParser n (IR.AnnDelG b UnpreparedValue (UnpreparedValue b)))
deleteFromTable sourceName tableInfo fieldName description deletePerms selectPerms = do
  let whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be deleted"
  whereArg  <- P.field whereName (Just whereDesc) <$> boolExp sourceName tableInfo selectPerms
  selection <- mutationSelectionSet sourceName tableInfo selectPerms
  let columns = tableColumns tableInfo
  pure $ P.subselection fieldName description whereArg selection
    <&> mkDeleteObject (tableInfoName tableInfo) columns deletePerms . fmap IR.MOutMultirowFields

-- | Construct a root field, normally called delete_tablename_by_pk, that can be used to delete an
-- individual rows from a DB table, specified by primary key. Select permissions are required, as
-- the user must be allowed to access all the primary keys of the table.
deleteFromTableByPk
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName           -- ^ table source
  -> TableInfo b          -- ^ table info
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> DelPermInfo b        -- ^ delete permissions of the table
  -> SelPermInfo b        -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (IR.AnnDelG b UnpreparedValue (UnpreparedValue b))))
deleteFromTableByPk sourceName tableInfo fieldName description deletePerms selectPerms = runMaybeT $ do
  let columns = tableColumns tableInfo
  pkArgs    <- MaybeT $ primaryKeysArguments tableInfo selectPerms
  selection <- lift $ tableSelectionSet sourceName tableInfo selectPerms
  pure $ P.subselection fieldName description pkArgs selection
    <&> mkDeleteObject (tableInfoName tableInfo) columns deletePerms . fmap IR.MOutSinglerowObject

mkDeleteObject
  :: Backend b
  => TableName b
  -> [ColumnInfo b]
  -> DelPermInfo b
  -> (AnnBoolExp b (UnpreparedValue b), IR.MutationOutputG b UnpreparedValue (UnpreparedValue b))
  -> IR.AnnDelG b UnpreparedValue (UnpreparedValue b)
mkDeleteObject table columns deletePerms (whereExp, mutationOutput) =
  IR.AnnDel { IR.dqp1Table   = table
            , IR.dqp1Where   = (permissionFilter, whereExp)
            , IR.dqp1Output  = mutationOutput
            , IR.dqp1AllCols = columns
            }
  where
    permissionFilter = (fmap . fmap) partialSQLExpToUnpreparedValue $ dpiFilter deletePerms


-- common

-- | All mutations allow returning results, such as what the updated database
-- rows look like.  This parser allows a query to specify what data to fetch.
mutationSelectionSet
  :: forall b r m n
   . MonadBuildSchema b r m n
  => SourceName
  -> TableInfo b
  -> Maybe (SelPermInfo b)
  -> m (Parser 'Output n (IR.MutFldsG b UnpreparedValue (UnpreparedValue b)))
mutationSelectionSet sourceName tableInfo selectPerms =
  memoizeOn 'mutationSelectionSet (sourceName, tableName) do
  tableGQLName <- getTableGQLName tableInfo
  returning <- runMaybeT do
    permissions <- hoistMaybe selectPerms
    tableSet    <- lift $ tableSelectionList sourceName tableInfo permissions
    let returningName = $$(G.litName "returning")
        returningDesc = "data from the rows affected by the mutation"
    pure $ IR.MRet <$> P.subselection_ returningName  (Just returningDesc) tableSet
  let affectedRowsName = $$(G.litName "affected_rows")
      affectedRowsDesc = "number of rows affected by the mutation"
      selectionName    = tableGQLName <> $$(G.litName "_mutation_response")
      selectionDesc    = G.Description $ "response of any mutation on the table " <>> tableName

      selectionFields  = catMaybes
        [ Just $ IR.MCount <$
            P.selection_ affectedRowsName (Just affectedRowsDesc) P.int
        , returning
        ]
  pure $ P.selectionSet selectionName (Just selectionDesc) selectionFields
    <&> parsedSelectionsToFields IR.MExp
  where
    tableName = tableInfoName tableInfo

-- | How to specify a database row by primary key.
primaryKeysArguments
  :: forall b r m n
   . MonadBuildSchema b r m n
  => TableInfo b
  -> SelPermInfo b
  -> m (Maybe (InputFieldsParser n (AnnBoolExp b (UnpreparedValue b))))
primaryKeysArguments tableInfo selectPerms = runMaybeT $ do
  primaryKeys <- hoistMaybe $ _tciPrimaryKey . _tiCoreInfo $ tableInfo
  let columns = _pkColumns primaryKeys
  guard $ all (\c -> pgiColumn c `Map.member` spiCols selectPerms) columns
  lift $ fmap (BoolAnd . toList) . sequenceA <$> for columns \columnInfo -> do
    field <- columnParser (pgiType columnInfo) (G.Nullability False)
    pure $ BoolFld . AVColumn columnInfo . pure . AEQ True . mkParameter <$>
      P.field (pgiName columnInfo) (pgiDescription columnInfo) field
