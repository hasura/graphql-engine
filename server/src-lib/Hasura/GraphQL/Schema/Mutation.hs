{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Schema.Mutation
  ( insertIntoTable
  , insertOneIntoTable
  , updateTable
  , updateTableByPk
  , deleteFromTable
  , deleteFromTableByPk
  ) where


import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd.Extended as OMap
import qualified Data.HashSet                        as Set
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import qualified Hasura.GraphQL.Parser               as P
import qualified Hasura.RQL.DML.Delete.Types         as RQL
import qualified Hasura.RQL.DML.Insert.Types         as RQL
import qualified Hasura.RQL.DML.Returning.Types      as RQL
import qualified Hasura.RQL.DML.Update               as RQL
import qualified Hasura.RQL.DML.Update.Types         as RQL
import qualified Hasura.SQL.DML                      as S

import           Hasura.GraphQL.Parser               (FieldParser, InputFieldsParser, Kind (..),
                                                      Parser, UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Insert
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.SQL.Types



-- insert

-- | Construct a root field, normally called insert_tablename, that can be used to add several rows to a DB table
insertIntoTable
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> InsPermInfo          -- ^ insert permissions of the table
  -> Maybe SelPermInfo    -- ^ select permissions of the table (if any)
  -> Maybe UpdPermInfo    -- ^ update permissions of the table (if any)
  -> m (FieldParser n (AnnInsert UnpreparedValue))
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
    <&> \((conflictClause, objects), output) -> AnnInsert (G.unName fieldName) False
       ( mkInsertObject objects table columns conflictClause insertPerms updatePerms
       , RQL.MOutMultirowFields output
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
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> InsPermInfo          -- ^ insert permissions of the table
  -> SelPermInfo          -- ^ select permissions of the table
  -> Maybe UpdPermInfo    -- ^ update permissions of the table (if any)
  -> m (FieldParser n (AnnInsert UnpreparedValue))
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
    <&> \((conflictClause, object), output) -> AnnInsert (G.unName fieldName) True
       ( mkInsertObject [object] table columns conflictClause insertPerms updatePerms
       , RQL.MOutSinglerowObject output
       )

-- | We specify the data of an individual row to insert through this input parser.
tableFieldsInput
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable -- ^ qualified name of the table
  -> InsPermInfo    -- ^ insert permissions of the table
  -> m (Parser 'Input n (AnnInsObj UnpreparedValue))
tableFieldsInput table insertPerms = memoizeOn 'tableFieldsInput table do
  tableName    <- qualifiedObjectToName table
  allFields    <- _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  objectFields <- catMaybes <$> for (Map.elems allFields) \case
    FIComputedField _ -> pure Nothing
    FIRemoteRelationship _ -> pure Nothing
    FIColumn columnInfo ->
      whenMaybe (Set.member (pgiColumn columnInfo) (ipiCols insertPerms)) do
        let columnName = pgiName columnInfo
            columnDesc = pgiDescription columnInfo
        fieldParser <- P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
        pure $ P.fieldOptional columnName columnDesc fieldParser `mapField`
          \(mkParameter -> value) -> AnnInsObj [(pgiColumn columnInfo, value)] [] []
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
            \objRelIns -> AnnInsObj [] [RelIns objRelIns relationshipInfo] []
        ArrRel -> do
          parser <- P.nullable <$> arrayRelationshipInput otherTable insPerms selPerms updPerms
          pure $ P.fieldOptional relFieldName Nothing parser <&> \arrRelIns -> do
            rel <- join arrRelIns
            Just $ AnnInsObj [] [] [RelIns rel relationshipInfo | not $ null $ _aiInsObj rel]
  let objectName = tableName <> $$(G.litName "_insert_input")
      objectDesc = G.Description $ "input type for inserting data into table " <>> table
  pure $ P.object objectName (Just objectDesc) $ catMaybes <$> sequenceA objectFields
    <&> mconcat

-- | Used by 'tableFieldsInput' for object data that is nested through object relationships
objectRelationshipInput
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> InsPermInfo
  -> Maybe SelPermInfo
  -> Maybe UpdPermInfo
  -> m (Parser 'Input n (SingleObjIns UnpreparedValue))
objectRelationshipInput table insertPerms selectPerms updatePerms =
  memoizeOn 'objectRelationshipInput table do
  tableName      <- qualifiedObjectToName table
  columns        <- tableColumns table
  objectParser   <- tableFieldsInput table insertPerms
  conflictParser <- fmap join $ sequenceA $ conflictObject table selectPerms <$> updatePerms
  let objectName   = $$(G.litName "data")
      inputName    = tableName <> $$(G.litName "_obj_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting object relation for remote table " <>> table
      inputParser = do
        conflictClause <- mkConflictClause conflictParser
        object <- P.field objectName Nothing objectParser
        pure $ mkInsertObject object table columns conflictClause insertPerms updatePerms
  pure $ P.object inputName (Just inputDesc) inputParser

-- | Used by 'tableFieldsInput' for object data that is nested through object relationships
arrayRelationshipInput
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> InsPermInfo
  -> Maybe SelPermInfo
  -> Maybe UpdPermInfo
  -> m (Parser 'Input n (MultiObjIns UnpreparedValue))
arrayRelationshipInput table insertPerms selectPerms updatePerms =
  memoizeOn 'arrayRelationshipInput table do
  tableName      <- qualifiedObjectToName table
  columns        <- tableColumns table
  objectParser   <- tableFieldsInput table insertPerms
  conflictParser <- fmap join $ sequenceA $ conflictObject table selectPerms <$> updatePerms
  let objectsName  = $$(G.litName "data")
      inputName    = tableName <> $$(G.litName "_arr_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting array relation for remote table " <>> table
      inputParser = do
        conflictClause <- mkConflictClause conflictParser
        objects <- P.field objectsName Nothing $ P.list objectParser
        pure $ mkInsertObject objects table columns conflictClause insertPerms updatePerms
  pure $ P.object inputName (Just inputDesc) inputParser

mkInsertObject
  :: a
  -> QualifiedTable
  -> [PGColumnInfo]
  -> Maybe (RQL.ConflictClauseP1 UnpreparedValue)
  -> InsPermInfo
  -> Maybe UpdPermInfo
  -> AnnIns a UnpreparedValue
mkInsertObject objects table columns conflictClause insertPerms updatePerms =
  AnnIns { _aiInsObj         = objects
         , _aiTableName      = table
         , _aiConflictClause = conflictClause
         , _aiCheckCond      = (insertCheck, updateCheck)
         , _aiTableCols      = columns
         , _aiDefVals        = defaultValues
         }
  where insertCheck = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ ipiCheck insertPerms
        updateCheck = fmapAnnBoolExp partialSQLExpToUnpreparedValue <$> (upiCheck =<< updatePerms)
        defaultValues = Map.union (partialSQLExpToUnpreparedValue <$> ipiSet insertPerms)
          $ fmap UVLiteral $ S.mkColDefValMap $ map pgiColumn columns

-- | Specifies the "ON CONFLICT" SQL clause
conflictObject
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => QualifiedTable
  -> Maybe SelPermInfo
  -> UpdPermInfo
  -> m (Maybe (Parser 'Input n (RQL.ConflictClauseP1 UnpreparedValue)))
conflictObject table selectPerms updatePerms = runMaybeT $ do
  tableName        <- lift $ qualifiedObjectToName table
  columnsEnum      <- MaybeT $ tableUpdateColumnsEnum table updatePerms
  constraints      <- MaybeT $ tciUniqueOrPrimaryKeyConstraints . _tiCoreInfo <$> askTableInfo table
  constraintParser <- lift $ conflictConstraint constraints table
  whereExpParser   <- lift $ boolExp table selectPerms
  let objectName = tableName <> $$(G.litName "_on_conflict")
      objectDesc = G.Description $ "on conflict condition type for table " <>> table
      constraintName = $$(G.litName "constraint")
      columnsName    = $$(G.litName "update_columns")
      whereExpName   = $$(G.litName "where")
      fieldsParser = do
        constraint <- RQL.CTConstraint <$> P.field constraintName Nothing constraintParser
        columns    <- P.field columnsName Nothing $ P.list columnsEnum
        whereExp   <- P.fieldOptional whereExpName Nothing whereExpParser
        pure $ case columns of
          [] -> RQL.CP1DoNothing $ Just constraint
          _  -> RQL.CP1Update constraint columns preSetColumns $
            BoolAnd $ catMaybes [whereExp, Just $ fmapAnnBoolExp partialSQLExpToUnpreparedValue $ upiFilter updatePerms]
  pure $ P.object objectName (Just objectDesc) fieldsParser
  where preSetColumns = partialSQLExpToUnpreparedValue <$> upiSet updatePerms

conflictConstraint
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  => NonEmpty Constraint
  -> QualifiedTable
  -> m (Parser 'Both n ConstraintName)
conflictConstraint constraints table = memoizeOn 'conflictConstraint table $ do
  tableName <- qualifiedObjectToName table
  constraintEnumValues <- for constraints \constraint -> do
    name <- textToName $ getConstraintTxt $ _cName constraint
    pure ( P.mkDefinition name (Just "unique or primary key constraint") P.EnumValueInfo
         , _cName constraint
         )
  let enumName  = tableName <> $$(G.litName "_constraint")
      enumDesc  = G.Description $ "unique or primary key constraints on table " <>> table
  pure $ P.enum enumName (Just enumDesc) constraintEnumValues



-- update

-- | Construct a root field, normally called update_tablename, that can be used
-- to update rows in a DB table specified by filters. Only returns a parser if
-- there are columns the user is allowed to update; otherwise returns Nothing.
updateTable
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> UpdPermInfo          -- ^ update permissions of the table
  -> Maybe SelPermInfo    -- ^ select permissions of the table (if any)
  -> m (Maybe (FieldParser n (RQL.AnnUpdG UnpreparedValue)))
updateTable table fieldName description updatePerms selectPerms = runMaybeT $ do
  let whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be updated"
  opArgs    <- MaybeT $ updateOperators table updatePerms
  columns   <- lift $ tableColumns table
  whereArg  <- lift $ P.field whereName (Just whereDesc) <$> boolExp table selectPerms
  selection <- lift $ mutationSelectionSet table selectPerms
  let argsParser = liftA2 (,) opArgs whereArg
  pure $ P.subselection fieldName description argsParser selection
    <&> mkUpdateObject table columns updatePerms . fmap RQL.MOutMultirowFields

-- | Construct a root field, normally called update_tablename, that can be used
-- to update a single in a DB table, specified by primary key. Only returns a
-- parser if there are columns the user is allowed to update and if the user has
-- select permissions on all primary keys; otherwise returns Nothing.
updateTableByPk
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> UpdPermInfo          -- ^ update permissions of the table
  -> SelPermInfo          -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (RQL.AnnUpdG UnpreparedValue)))
updateTableByPk table fieldName description updatePerms selectPerms = runMaybeT $ do
  tableName <- qualifiedObjectToName table
  columns   <- lift   $ tableSelectColumns table selectPerms
  pkArgs    <- MaybeT $ primaryKeysArguments table selectPerms
  opArgs    <- MaybeT $ updateOperators table updatePerms
  selection <- lift $ tableSelectionSet table selectPerms
  let pkFieldName  = $$(G.litName "pk_columns")
      pkObjectName = tableName <> $$(G.litName "_pk_columns_input")
      pkObjectDesc = G.Description $ "primary key columns input for table: " <> G.unName tableName
      argsParser   = do
        operators <- opArgs
        primaryKeys <- P.field pkFieldName Nothing $ P.object pkObjectName (Just pkObjectDesc) pkArgs
        pure (operators, primaryKeys)
  pure $ P.subselection fieldName description argsParser selection
    <&> mkUpdateObject table columns updatePerms . fmap RQL.MOutSinglerowObject

mkUpdateObject
  :: QualifiedTable
  -> [PGColumnInfo]
  -> UpdPermInfo
  -> ( ( [(PGCol, RQL.UpdOpExpG UnpreparedValue)]
       , AnnBoolExp UnpreparedValue
       )
     , RQL.MutationOutputG UnpreparedValue
     )
  -> RQL.AnnUpdG UnpreparedValue
mkUpdateObject table columns updatePerms ((opExps, whereExp), mutationOutput) =
  RQL.AnnUpd { RQL.uqp1Table   = table
             , RQL.uqp1OpExps  = opExps
             , RQL.uqp1Where   = (permissionFilter, whereExp)
             , RQL.uqp1Check   = checkExp
             , RQL.uqp1Output  = mutationOutput
             , RQL.uqp1AllCols = columns
             }
  where
    permissionFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ upiFilter updatePerms
    checkExp = maybe annBoolExpTrue (fmapAnnBoolExp partialSQLExpToUnpreparedValue) $ upiCheck updatePerms

-- | Various update operators
updateOperators
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  => QualifiedTable -- ^ qualified name of the table
  -> UpdPermInfo    -- ^ update permissions of the table
  -> m (Maybe (InputFieldsParser n [(PGCol, RQL.UpdOpExpG UnpreparedValue)]))
updateOperators table updatePermissions = do
  tableName <- qualifiedObjectToName table
  columns   <- tableUpdateColumns table updatePermissions
  let numericCols = onlyNumCols   columns
      jsonCols    = onlyJSONBCols columns
  parsers <- catMaybes <$> sequenceA
    [ updateOperator tableName $$(G.litName "_set")
        columnParser RQL.UpdSet columns
        "sets the columns of the filtered rows to the given values"
        (G.Description $ "input type for updating data in table " <>> table)

    , updateOperator tableName $$(G.litName "_inc")
        columnParser RQL.UpdInc numericCols
        "increments the numeric columns with given value of the filtered values"
        (G.Description $"input type for incrementing numeric columns in table " <>> table)

    , let desc = "prepend existing jsonb value of filtered columns with new jsonb value"
      in updateOperator tableName $$(G.litName "_prepend")
         columnParser RQL.UpdPrepend jsonCols desc desc

    , let desc = "append existing jsonb value of filtered columns with new jsonb value"
      in updateOperator tableName $$(G.litName "_append")
         columnParser RQL.UpdAppend jsonCols desc desc

    , let desc = "delete key/value pair or string element. key/value pairs are matched based on their key value"
      in updateOperator tableName $$(G.litName "_delete_key")
         nullableTextParser RQL.UpdDeleteKey jsonCols desc desc

    , let desc = "delete the array element with specified index (negative integers count from the end). "
                 <> "throws an error if top level container is not an array"
      in updateOperator tableName $$(G.litName "_delete_elem")
         nonNullableIntParser RQL.UpdDeleteElem jsonCols desc desc

    , let desc = "delete the field or element with specified path (for JSON arrays, negative integers count from the end)"
      in updateOperator tableName $$(G.litName "_delete_at_path")
         (fmap P.list . nonNullableTextParser) RQL.UpdDeleteAtPath jsonCols desc desc
    ]
  whenMaybe (not $ null parsers) do
    let allowedOperators = fst <$> parsers
    pure $ fmap catMaybes (sequenceA $ snd <$> parsers)
      `P.bindFields` \opExps -> do
        -- there needs to be at least one operator in the update, even if it is empty
        let presetColumns = Map.toList $ RQL.UpdSet . partialSQLExpToUnpreparedValue <$> upiSet updatePermissions
        when (null opExps && null presetColumns) $ parseError $
          "at least any one of " <> T.intercalate ", " allowedOperators <> " is expected"

        -- no column should appear twice
        let flattenedExps = concat opExps
            erroneousExps = OMap.filter ((>1) . length) $ OMap.groupTuples flattenedExps
        unless (OMap.null erroneousExps) $ parseError $
          "column found in multiple operators; " <>
          T.intercalate ". " [ dquote column <> " in " <> T.intercalate ", " (toList $ RQL.updateOperatorText <$> ops)
                             | (column, ops) <- OMap.toList erroneousExps
                             ]

        pure $ presetColumns <> flattenedExps
  where
    columnParser columnInfo = fmap P.mkParameter <$> P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
    nonNullableTextParser _ = fmap P.mkParameter <$> P.column (PGColumnScalar PGText)    (G.Nullability False)
    nullableTextParser    _ = fmap P.mkParameter <$> P.column (PGColumnScalar PGText)    (G.Nullability True)
    nonNullableIntParser  _ = fmap P.mkParameter <$> P.column (PGColumnScalar PGInteger) (G.Nullability False)

    updateOperator
      :: G.Name
      -> G.Name
      -> (PGColumnInfo -> m (Parser 'Both n a))
      -> (a -> RQL.UpdOpExpG UnpreparedValue)
      -> [PGColumnInfo]
      -> G.Description
      -> G.Description
      -> m (Maybe (Text, InputFieldsParser n (Maybe [(PGCol, RQL.UpdOpExpG UnpreparedValue)])))
    updateOperator tableName opName mkParser updOpExp columns opDesc objDesc =
      whenMaybe (not $ null columns) do
        fields <- for columns \columnInfo -> do
          let fieldName = pgiName columnInfo
              fieldDesc = pgiDescription columnInfo
          fieldParser <- mkParser columnInfo
          pure $ P.fieldOptional fieldName fieldDesc fieldParser
            `mapField` \value -> (pgiColumn columnInfo, updOpExp value)
        let objName = tableName <> opName <> $$(G.litName "_input")
        pure $ (G.unName opName,)
             $ P.fieldOptional opName (Just opDesc)
             $ P.object objName (Just objDesc)
             $ catMaybes <$> sequenceA fields



-- delete

-- | Construct a root field, normally called delete_tablename, that can be used
-- to delete several rows from a DB table
deleteFromTable
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> DelPermInfo          -- ^ delete permissions of the table
  -> Maybe SelPermInfo    -- ^ select permissions of the table (if any)
  -> m (FieldParser n (RQL.AnnDelG UnpreparedValue))
deleteFromTable table fieldName description deletePerms selectPerms = do
  let whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be deleted"
  whereArg  <- P.field whereName (Just whereDesc) <$> boolExp table selectPerms
  selection <- mutationSelectionSet table selectPerms
  columns   <- tableColumns table
  pure $ P.subselection fieldName description whereArg selection
    <&> mkDeleteObject table columns deletePerms . fmap RQL.MOutMultirowFields

-- | Construct a root field, normally called delete_tablename, that can be used
-- to delete an individual rows from a DB table, specified by primary key
deleteFromTableByPk
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> DelPermInfo          -- ^ delete permissions of the table
  -> SelPermInfo          -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (RQL.AnnDelG UnpreparedValue)))
deleteFromTableByPk table fieldName description deletePerms selectPerms = runMaybeT $ do
  columns   <- lift   $ tableSelectColumns table selectPerms
  pkArgs    <- MaybeT $ primaryKeysArguments table selectPerms
  selection <- lift $ tableSelectionSet table selectPerms
  pure $ P.subselection fieldName description pkArgs selection
    <&> mkDeleteObject table columns deletePerms . fmap RQL.MOutSinglerowObject

mkDeleteObject
  :: QualifiedTable
  -> [PGColumnInfo]
  -> DelPermInfo
  -> (AnnBoolExp UnpreparedValue, RQL.MutationOutputG UnpreparedValue)
  -> RQL.AnnDelG UnpreparedValue
mkDeleteObject table columns deletePerms (whereExp, mutationOutput) =
  RQL.AnnDel { RQL.dqp1Table   = table
             , RQL.dqp1Where   = (permissionFilter, whereExp)
             , RQL.dqp1Output  = mutationOutput
             , RQL.dqp1AllCols = columns
             }
  where
    permissionFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ dpiFilter deletePerms



-- common

-- | All mutations allow returning results, such as what the updated database
-- rows look like.  This parser allows a query to specify what data to fetch.
mutationSelectionSet
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => QualifiedTable
  -> Maybe SelPermInfo
  -> m (Parser 'Output n (RQL.MutFldsG UnpreparedValue))
mutationSelectionSet table selectPerms =
  memoizeOn 'mutationSelectionSet table do
  tableName <- qualifiedObjectToName table
  returning <- runMaybeT do
    permissions <- MaybeT $ pure selectPerms
    tableSet    <- lift $ tableSelectionList table permissions
    let returningName = $$(G.litName "returning")
        returningDesc = "data from the rows affected by the mutation"
    pure $ RQL.MRet <$> P.subselection_ returningName  (Just returningDesc) tableSet
  let affectedRowsName = $$(G.litName "affected_rows")
      affectedRowsDesc = "number of rows affected by the mutation"
      selectionName    = tableName <> $$(G.litName "_mutation_response")
      selectionDesc    = G.Description $ "response of any mutation on the table " <>> table

      selectionFields  = catMaybes
        [ Just $ RQL.MCount <$
            P.selection_ affectedRowsName (Just affectedRowsDesc) P.int
        , returning
        ]
  pure $ P.selectionSet selectionName (Just selectionDesc) selectionFields
    <&> parsedSelectionsToFields RQL.MExp

-- | How to specify a database row by primary key.
primaryKeysArguments
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  => QualifiedTable
  -> SelPermInfo
  -> m (Maybe (InputFieldsParser n (AnnBoolExp UnpreparedValue)))
primaryKeysArguments table selectPerms = runMaybeT $ do
  primaryKeys <- MaybeT $ _tciPrimaryKey . _tiCoreInfo <$> askTableInfo table
  let columns = _pkColumns primaryKeys
  guard $ all (\c -> pgiColumn c `Set.member` spiCols selectPerms) columns
  lift $ fmap (BoolAnd . toList) . sequenceA <$> for columns \columnInfo -> do
    field <- P.column (pgiType columnInfo) (G.Nullability False)
    pure $ BoolFld . AVCol columnInfo . pure . AEQ True . mkParameter <$>
      P.field (pgiName columnInfo) (pgiDescription columnInfo) field
