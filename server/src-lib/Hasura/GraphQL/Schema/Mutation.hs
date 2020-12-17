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

import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd.Extended as OMap
import qualified Data.HashSet                        as Set
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Data.Has
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.DML    as PG
import qualified Hasura.GraphQL.Parser               as P
import qualified Hasura.RQL.IR.Delete                as IR
import qualified Hasura.RQL.IR.Insert                as IR
import qualified Hasura.RQL.IR.Returning             as IR
import qualified Hasura.RQL.IR.Update                as IR

import           Hasura.Backends.Postgres.SQL.Types  hiding (TableName)
import           Hasura.GraphQL.Parser               (FieldParser, InputFieldsParser, Kind (..),
                                                      Parser, UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Backend
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types                    hiding (ConstraintName)



-- insert

-- | Construct a root field, normally called insert_tablename, that can be used to add several rows to a DB table
insertIntoTable
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> InsPermInfo 'Postgres          -- ^ insert permissions of the table
  -> Maybe (SelPermInfo 'Postgres)    -- ^ select permissions of the table (if any)
  -> Maybe (UpdPermInfo 'Postgres)    -- ^ update permissions of the table (if any)
  -> m (FieldParser n (IR.AnnInsert 'Postgres (UnpreparedValue 'Postgres)))
insertIntoTable table fieldName description insertPerms selectPerms updatePerms = do
  columns         <- tableColumns table
  selectionParser <- mutationSelectionSet table selectPerms
  objectsParser   <- P.list <$> tableFieldsInput table insertPerms
  conflictParser  <- fmap join $ sequenceA $ conflictObject table selectPerms <$> updatePerms
  let objectsName  = $$(G.litName "objects")
      objectsDesc  = "the rows to be inserted"
      argsParser   = do
        conflictClause <- mkConflictClause conflictParser
        objects <- P.field objectsName (Just objectsDesc) objectsParser
        pure (conflictClause, objects)
  pure $ P.subselection fieldName description argsParser selectionParser
    <&> \((conflictClause, objects), output) -> IR.AnnInsert (G.unName fieldName) False
      ( mkInsertObject objects table columns conflictClause insertPerms updatePerms
      , IR.MOutMultirowFields output
      )

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
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> InsPermInfo 'Postgres          -- ^ insert permissions of the table
  -> SelPermInfo 'Postgres          -- ^ select permissions of the table
  -> Maybe (UpdPermInfo 'Postgres)    -- ^ update permissions of the table (if any)
  -> m (FieldParser n (IR.AnnInsert 'Postgres (UnpreparedValue 'Postgres)))
insertOneIntoTable table fieldName description insertPerms selectPerms updatePerms  = do
  columns         <- tableColumns table
  selectionParser <- tableSelectionSet table selectPerms
  objectParser    <- tableFieldsInput table insertPerms
  conflictParser  <- fmap join $ sequenceA $ conflictObject table (Just selectPerms) <$> updatePerms
  let objectName    = $$(G.litName "object")
      objectDesc    = "the row to be inserted"
      argsParser    = do
        conflictClause <- mkConflictClause conflictParser
        object <- P.field objectName (Just objectDesc) objectParser
        pure (conflictClause, object)
  pure $ P.subselection fieldName description argsParser selectionParser
    <&> \((conflictClause, object), output) -> IR.AnnInsert (G.unName fieldName) True
       ( mkInsertObject [object] table columns conflictClause insertPerms updatePerms
       , IR.MOutSinglerowObject output
       )

-- | We specify the data of an individual row to insert through this input parser.
tableFieldsInput
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m)
  => QualifiedTable -- ^ qualified name of the table
  -> InsPermInfo 'Postgres    -- ^ insert permissions of the table
  -> m (Parser 'Input n (IR.AnnInsObj 'Postgres (UnpreparedValue 'Postgres)))
tableFieldsInput table insertPerms = memoizeOn 'tableFieldsInput table do
  tableGQLName <- getTableGQLName @'Postgres table
  allFields    <- _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
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
      let otherTable = riRTable  relationshipInfo
          relName    = riName    relationshipInfo
      permissions  <- MaybeT $ tablePermissions otherTable
      relFieldName <- lift $ textToName $ relNameToTxt relName
      insPerms     <- MaybeT $ pure $ _permIns permissions
      let selPerms = _permSel permissions
          updPerms = _permUpd permissions
      lift $ case riType relationshipInfo of
        ObjRel -> do
          parser <- objectRelationshipInput otherTable insPerms selPerms updPerms
          pure $ P.fieldOptional relFieldName Nothing parser `mapField`
            \objRelIns -> IR.AnnInsObj [] [IR.RelIns objRelIns relationshipInfo] []
        ArrRel -> do
          parser <- P.nullable <$> arrayRelationshipInput otherTable insPerms selPerms updPerms
          pure $ P.fieldOptional relFieldName Nothing parser <&> \arrRelIns -> do
            rel <- join arrRelIns
            Just $ IR.AnnInsObj [] [] [IR.RelIns rel relationshipInfo | not $ null $ IR._aiInsObj rel]
  let objectName = tableGQLName <> $$(G.litName "_insert_input")
      objectDesc = G.Description $ "input type for inserting data into table " <>> table
  pure $ P.object objectName (Just objectDesc) $ catMaybes <$> sequenceA objectFields
    <&> mconcat

-- | Used by 'tableFieldsInput' for object data that is nested through object relationships
objectRelationshipInput
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m)
  => QualifiedTable
  -> InsPermInfo 'Postgres
  -> Maybe (SelPermInfo 'Postgres)
  -> Maybe (UpdPermInfo 'Postgres)
  -> m (Parser 'Input n (IR.SingleObjIns 'Postgres (UnpreparedValue 'Postgres)))
objectRelationshipInput table insertPerms selectPerms updatePerms =
  memoizeOn 'objectRelationshipInput table do
  tableGQLName   <- getTableGQLName @'Postgres table
  columns        <- tableColumns table
  objectParser   <- tableFieldsInput table insertPerms
  conflictParser <- fmap join $ sequenceA $ conflictObject table selectPerms <$> updatePerms
  let objectName   = $$(G.litName "data")
      inputName    = tableGQLName <> $$(G.litName "_obj_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting object relation for remote table " <>> table
      inputParser = do
        conflictClause <- mkConflictClause conflictParser
        object <- P.field objectName Nothing objectParser
        pure $ mkInsertObject object table columns conflictClause insertPerms updatePerms
  pure $ P.object inputName (Just inputDesc) inputParser

-- | Used by 'tableFieldsInput' for object data that is nested through object relationships
arrayRelationshipInput
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m)
  => QualifiedTable
  -> InsPermInfo 'Postgres
  -> Maybe (SelPermInfo 'Postgres)
  -> Maybe (UpdPermInfo 'Postgres)
  -> m (Parser 'Input n (IR.MultiObjIns 'Postgres (UnpreparedValue 'Postgres)))
arrayRelationshipInput table insertPerms selectPerms updatePerms =
  memoizeOn 'arrayRelationshipInput table do
  tableGQLName      <- getTableGQLName @'Postgres table
  columns        <- tableColumns table
  objectParser   <- tableFieldsInput table insertPerms
  conflictParser <- fmap join $ sequenceA $ conflictObject table selectPerms <$> updatePerms
  let objectsName  = $$(G.litName "data")
      inputName    = tableGQLName <> $$(G.litName "_arr_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting array relation for remote table " <>> table
      inputParser = do
        conflictClause <- mkConflictClause conflictParser
        objects <- P.field objectsName Nothing $ P.list objectParser
        pure $ mkInsertObject objects table columns conflictClause insertPerms updatePerms
  pure $ P.object inputName (Just inputDesc) inputParser

mkInsertObject
  :: a
  -> QualifiedTable
  -> [ColumnInfo 'Postgres]
  -> Maybe (IR.ConflictClauseP1 'Postgres (UnpreparedValue 'Postgres))
  -> InsPermInfo 'Postgres
  -> Maybe (UpdPermInfo 'Postgres)
  -> IR.AnnIns 'Postgres a (UnpreparedValue 'Postgres)
mkInsertObject objects table columns conflictClause insertPerms updatePerms =
  IR.AnnIns { _aiInsObj         = objects
            , _aiTableName      = table
            , _aiConflictClause = conflictClause
            , _aiCheckCond      = (insertCheck, updateCheck)
            , _aiTableCols      = columns
            , _aiDefVals        = defaultValues
            }
  where insertCheck = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ ipiCheck insertPerms
        updateCheck = fmapAnnBoolExp partialSQLExpToUnpreparedValue <$> (upiCheck =<< updatePerms)
        defaultValues = Map.union (partialSQLExpToUnpreparedValue <$> ipiSet insertPerms)
          $ fmap UVLiteral $ PG.mkColDefValMap $ map pgiColumn columns

-- | Specifies the "ON CONFLICT" SQL clause
conflictObject
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m)
  => TableName 'Postgres
  -> Maybe (SelPermInfo 'Postgres)
  -> UpdPermInfo 'Postgres
  -> m (Maybe (Parser 'Input n (IR.ConflictClauseP1 'Postgres (UnpreparedValue 'Postgres))))
conflictObject table selectPerms updatePerms = runMaybeT $ do
  tableGQLName      <- getTableGQLName @'Postgres table
  columnsEnum      <- MaybeT $ tableUpdateColumnsEnum table updatePerms
  constraints      <- MaybeT $ tciUniqueOrPrimaryKeyConstraints . _tiCoreInfo <$> askTableInfo @'Postgres table
  constraintParser <- lift $ conflictConstraint constraints table
  whereExpParser   <- lift $ boolExp table selectPerms
  let objectName = tableGQLName <> $$(G.litName "_on_conflict")
      objectDesc = G.Description $ "on conflict condition type for table " <>> table
      constraintName = $$(G.litName "constraint")
      columnsName    = $$(G.litName "update_columns")
      whereExpName   = $$(G.litName "where")
      fieldsParser = do
        constraint <- IR.CTConstraint <$> P.field constraintName Nothing constraintParser
        columns    <- P.field columnsName Nothing $ P.list columnsEnum
        whereExp   <- P.fieldOptional whereExpName Nothing whereExpParser
        pure $ case columns of
          [] -> IR.CP1DoNothing $ Just constraint
          _  -> IR.CP1Update constraint columns preSetColumns $
            BoolAnd $ catMaybes [whereExp, Just $ fmapAnnBoolExp partialSQLExpToUnpreparedValue $ upiFilter updatePerms]
  pure $ P.object objectName (Just objectDesc) fieldsParser
  where preSetColumns = partialSQLExpToUnpreparedValue <$> upiSet updatePerms

conflictConstraint
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m)
  => NonEmpty Constraint
  -> QualifiedTable
  -> m (Parser 'Both n ConstraintName)
conflictConstraint constraints table = memoizeOn 'conflictConstraint table $ do
  tableGQLName <- getTableGQLName @'Postgres table
  constraintEnumValues <- for constraints \constraint -> do
    name <- textToName $ getConstraintTxt $ _cName constraint
    pure ( P.mkDefinition name (Just "unique or primary key constraint") P.EnumValueInfo
         , _cName constraint
         )
  let enumName  = tableGQLName <> $$(G.litName "_constraint")
      enumDesc  = G.Description $ "unique or primary key constraints on table " <>> table
  pure $ P.enum enumName (Just enumDesc) constraintEnumValues


-- update

-- | Construct a root field, normally called update_tablename, that can be used
-- to update rows in a DB table specified by filters. Only returns a parser if
-- there are columns the user is allowed to update; otherwise returns Nothing.
updateTable
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> UpdPermInfo 'Postgres          -- ^ update permissions of the table
  -> Maybe (SelPermInfo 'Postgres)    -- ^ select permissions of the table (if any)
  -> m (Maybe (FieldParser n (IR.AnnUpdG 'Postgres (UnpreparedValue 'Postgres))))
updateTable table fieldName description updatePerms selectPerms = runMaybeT $ do
  let whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be updated"
  opArgs    <- MaybeT $ updateOperators table updatePerms
  columns   <- lift $ tableColumns table
  whereArg  <- lift $ P.field whereName (Just whereDesc) <$> boolExp table selectPerms
  selection <- lift $ mutationSelectionSet table selectPerms
  let argsParser = liftA2 (,) opArgs whereArg
  pure $ P.subselection fieldName description argsParser selection
    <&> mkUpdateObject table columns updatePerms . fmap IR.MOutMultirowFields

-- | Construct a root field, normally called update_tablename, that can be used
-- to update a single in a DB table, specified by primary key. Only returns a
-- parser if there are columns the user is allowed to update and if the user has
-- select permissions on all primary keys; otherwise returns Nothing.
updateTableByPk
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> UpdPermInfo 'Postgres          -- ^ update permissions of the table
  -> SelPermInfo 'Postgres          -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (IR.AnnUpdG 'Postgres (UnpreparedValue 'Postgres))))
updateTableByPk table fieldName description updatePerms selectPerms = runMaybeT $ do
  tableGQLName <- getTableGQLName @'Postgres table
  columns      <- lift $ tableColumns table
  pkArgs       <- MaybeT $ primaryKeysArguments table selectPerms
  opArgs       <- MaybeT $ updateOperators table updatePerms
  selection    <- lift $ tableSelectionSet table selectPerms
  let pkFieldName  = $$(G.litName "pk_columns")
      pkObjectName = tableGQLName <> $$(G.litName "_pk_columns_input")
      pkObjectDesc = G.Description $ "primary key columns input for table: " <> G.unName tableGQLName
      argsParser   = do
        operators <- opArgs
        primaryKeys <- P.field pkFieldName Nothing $ P.object pkObjectName (Just pkObjectDesc) pkArgs
        pure (operators, primaryKeys)
  pure $ P.subselection fieldName description argsParser selection
    <&> mkUpdateObject table columns updatePerms . fmap IR.MOutSinglerowObject

mkUpdateObject
  :: QualifiedTable
  -> [ColumnInfo 'Postgres]
  -> UpdPermInfo 'Postgres
  -> ( ( [(PGCol, IR.UpdOpExpG (UnpreparedValue 'Postgres))]
       , AnnBoolExp 'Postgres (UnpreparedValue 'Postgres)
       )
     , IR.MutationOutputG 'Postgres (UnpreparedValue 'Postgres)
     )
  -> IR.AnnUpdG 'Postgres (UnpreparedValue 'Postgres)
mkUpdateObject table columns updatePerms ((opExps, whereExp), mutationOutput) =
  IR.AnnUpd { IR.uqp1Table   = table
            , IR.uqp1OpExps  = opExps
            , IR.uqp1Where   = (permissionFilter, whereExp)
            , IR.uqp1Check   = checkExp
            , IR.uqp1Output  = mutationOutput
            , IR.uqp1AllCols = columns
            }
  where
    permissionFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ upiFilter updatePerms
    checkExp = maybe annBoolExpTrue (fmapAnnBoolExp partialSQLExpToUnpreparedValue) $ upiCheck updatePerms

-- | Various update operators
updateOperators
  :: forall m n r. (BackendSchema 'Postgres, MonadSchema n m, MonadTableInfo 'Postgres r m)
  => QualifiedTable -- ^ qualified name of the table
  -> UpdPermInfo 'Postgres    -- ^ update permissions of the table
  -> m (Maybe (InputFieldsParser n [(PGCol, IR.UpdOpExpG (UnpreparedValue 'Postgres))]))
updateOperators table updatePermissions = do
  tableGQLName <- getTableGQLName @'Postgres table
  columns      <- tableUpdateColumns table updatePermissions
  let numericCols = onlyNumCols   columns
      jsonCols    = onlyJSONBCols columns
  parsers <- catMaybes <$> sequenceA
    [ updateOperator tableGQLName $$(G.litName "_set")
        typedParser IR.UpdSet columns
        "sets the columns of the filtered rows to the given values"
        (G.Description $ "input type for updating data in table " <>> table)

    , updateOperator tableGQLName $$(G.litName "_inc")
        typedParser IR.UpdInc numericCols
        "increments the numeric columns with given value of the filtered values"
        (G.Description $"input type for incrementing numeric columns in table " <>> table)

    , let desc = "prepend existing jsonb value of filtered columns with new jsonb value"
      in updateOperator tableGQLName $$(G.litName "_prepend")
         typedParser IR.UpdPrepend jsonCols desc desc

    , let desc = "append existing jsonb value of filtered columns with new jsonb value"
      in updateOperator tableGQLName $$(G.litName "_append")
         typedParser IR.UpdAppend jsonCols desc desc

    , let desc = "delete key/value pair or string element. key/value pairs are matched based on their key value"
      in updateOperator tableGQLName $$(G.litName "_delete_key")
         nullableTextParser IR.UpdDeleteKey jsonCols desc desc

    , let desc = "delete the array element with specified index (negative integers count from the end). "
                 <> "throws an error if top level container is not an array"
      in updateOperator tableGQLName $$(G.litName "_delete_elem")
         nonNullableIntParser IR.UpdDeleteElem jsonCols desc desc

    , let desc = "delete the field or element with specified path (for JSON arrays, negative integers count from the end)"
      in updateOperator tableGQLName $$(G.litName "_delete_at_path")
         (fmap P.list . nonNullableTextParser) IR.UpdDeleteAtPath jsonCols desc desc
    ]
  whenMaybe (not $ null parsers) do
    let allowedOperators = fst <$> parsers
    pure $ fmap catMaybes (sequenceA $ snd <$> parsers)
      `P.bindFields` \opExps -> do
        -- there needs to be at least one operator in the update, even if it is empty
        let presetColumns = Map.toList $ IR.UpdSet . partialSQLExpToUnpreparedValue <$> upiSet updatePermissions
        when (null opExps && null presetColumns) $ parseError $
          "at least any one of " <> commaSeparated allowedOperators <> " is expected"

        -- no column should appear twice
        let flattenedExps = concat opExps
            erroneousExps = OMap.filter ((>1) . length) $ OMap.groupTuples flattenedExps
        unless (OMap.null erroneousExps) $ parseError $
          "column found in multiple operators; " <>
          T.intercalate ". " [ dquote columnName <> " in " <> commaSeparated (IR.updateOperatorText <$> ops)
                             | (columnName, ops) <- OMap.toList erroneousExps
                             ]

        pure $ presetColumns <> flattenedExps
  where
    typedParser columnInfo  = fmap P.mkParameter <$> columnParser (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
    nonNullableTextParser _ = fmap P.mkParameter <$> columnParser (ColumnScalar PGText)    (G.Nullability False)
    nullableTextParser    _ = fmap P.mkParameter <$> columnParser (ColumnScalar PGText)    (G.Nullability True)
    nonNullableIntParser  _ = fmap P.mkParameter <$> columnParser (ColumnScalar PGInteger) (G.Nullability False)

    updateOperator
      :: G.Name
      -> G.Name
      -> (ColumnInfo 'Postgres -> m (Parser 'Both n a))
      -> (a -> IR.UpdOpExpG (UnpreparedValue 'Postgres))
      -> [ColumnInfo 'Postgres]
      -> G.Description
      -> G.Description
      -> m (Maybe (Text, InputFieldsParser n (Maybe [(PGCol, IR.UpdOpExpG (UnpreparedValue 'Postgres))])))
    updateOperator tableGQLName opName mkParser updOpExp columns opDesc objDesc =
      whenMaybe (not $ null columns) do
        fields <- for columns \columnInfo -> do
          let fieldName = pgiName columnInfo
              fieldDesc = pgiDescription columnInfo
          fieldParser <- mkParser columnInfo
          pure $ P.fieldOptional fieldName fieldDesc fieldParser
            `mapField` \value -> (pgiColumn columnInfo, updOpExp value)
        let objName = tableGQLName <> opName <> $$(G.litName "_input")
        pure $ (G.unName opName,)
             $ P.fieldOptional opName (Just opDesc)
             $ P.object objName (Just objDesc)
             $ catMaybes <$> sequenceA fields



-- delete

-- | Construct a root field, normally called delete_tablename, that can be used
-- to delete several rows from a DB table
deleteFromTable
  :: forall b m n r. (BackendSchema b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m, Has QueryContext r)
  => TableName b       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> DelPermInfo b -- ^ delete permissions of the table
  -> Maybe (SelPermInfo b)    -- ^ select permissions of the table (if any)
  -> m (FieldParser n (IR.AnnDelG b (UnpreparedValue b)))
deleteFromTable table fieldName description deletePerms selectPerms = do
  let whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be deleted"
  whereArg  <- P.field whereName (Just whereDesc) <$> boolExp table selectPerms
  selection <- mutationSelectionSet table selectPerms
  columns   <- tableColumns table
  pure $ P.subselection fieldName description whereArg selection
    <&> mkDeleteObject table columns deletePerms . fmap IR.MOutMultirowFields

-- | Construct a root field, normally called delete_tablename, that can be used
-- to delete an individual rows from a DB table, specified by primary key
deleteFromTableByPk
  :: forall b m n r. (BackendSchema b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m, Has QueryContext r)
  => TableName b          -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> DelPermInfo b  -- ^ delete permissions of the table
  -> SelPermInfo b  -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (IR.AnnDelG b (UnpreparedValue b))))
deleteFromTableByPk table fieldName description deletePerms selectPerms = runMaybeT $ do
  columns   <- lift $ tableColumns table
  pkArgs    <- MaybeT $ primaryKeysArguments table selectPerms
  selection <- lift $ tableSelectionSet table selectPerms
  pure $ P.subselection fieldName description pkArgs selection
    <&> mkDeleteObject table columns deletePerms . fmap IR.MOutSinglerowObject

mkDeleteObject
  :: TableName b
  -> [ColumnInfo b]
  -> DelPermInfo b
  -> (AnnBoolExp b (UnpreparedValue b), IR.MutationOutputG b (UnpreparedValue b))
  -> IR.AnnDelG b (UnpreparedValue b)
mkDeleteObject table columns deletePerms (whereExp, mutationOutput) =
  IR.AnnDel { IR.dqp1Table   = table
            , IR.dqp1Where   = (permissionFilter, whereExp)
            , IR.dqp1Output  = mutationOutput
            , IR.dqp1AllCols = columns
            }
  where
    permissionFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ dpiFilter deletePerms



-- common

-- | All mutations allow returning results, such as what the updated database
-- rows look like.  This parser allows a query to specify what data to fetch.
mutationSelectionSet
  :: forall b m n r. (BackendSchema b, MonadSchema n m, MonadTableInfo b r m, MonadRole r m, Has QueryContext r)
  => TableName b
  -> Maybe (SelPermInfo b)
  -> m (Parser 'Output n (IR.MutFldsG b (UnpreparedValue b)))
mutationSelectionSet table selectPerms =
  memoizeOn 'mutationSelectionSet table do
  tableGQLName <- getTableGQLName @b table
  returning <- runMaybeT do
    permissions <- MaybeT $ pure selectPerms
    tableSet    <- lift $ tableSelectionList table permissions
    let returningName = $$(G.litName "returning")
        returningDesc = "data from the rows affected by the mutation"
    pure $ IR.MRet <$> P.subselection_ returningName  (Just returningDesc) tableSet
  let affectedRowsName = $$(G.litName "affected_rows")
      affectedRowsDesc = "number of rows affected by the mutation"
      selectionName    = tableGQLName <> $$(G.litName "_mutation_response")
      selectionDesc    = G.Description $ "response of any mutation on the table " <>> table

      selectionFields  = catMaybes
        [ Just $ IR.MCount <$
            P.selection_ affectedRowsName (Just affectedRowsDesc) P.int
        , returning
        ]
  pure $ P.selectionSet selectionName (Just selectionDesc) selectionFields
    <&> parsedSelectionsToFields IR.MExp

-- | How to specify a database row by primary key.
primaryKeysArguments
  :: forall b m n r. (BackendSchema b, MonadSchema n m, MonadTableInfo b r m)
  => TableName b
  -> SelPermInfo b
  -> m (Maybe (InputFieldsParser n (AnnBoolExp b (UnpreparedValue b))))
primaryKeysArguments table selectPerms = runMaybeT $ do
  primaryKeys <- MaybeT $ _tciPrimaryKey . _tiCoreInfo <$> askTableInfo table
  let columns = _pkColumns primaryKeys
  guard $ all (\c -> pgiColumn c `Set.member` spiCols selectPerms) columns
  lift $ fmap (BoolAnd . toList) . sequenceA <$> for columns \columnInfo -> do
    field <- columnParser (pgiType columnInfo) (G.Nullability False)
    pure $ BoolFld . AVCol columnInfo . pure . AEQ True . mkParameter <$>
      P.field (pgiName columnInfo) (pgiDescription columnInfo) field
