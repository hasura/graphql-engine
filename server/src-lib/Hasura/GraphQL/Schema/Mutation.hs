{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Schema.Mutation
  ( insertIntoTable
  , insertOneIntoTable
  , updateTable
  , updateTableByPk
  , deleteFromTable
  , deleteFromTableByPk
  , actionAsync
  , actionSync
  -- FIXME: move somewhere else
  , traverseAnnInsert
  , convertToSQLTransaction
  ) where


import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashMap.Strict.InsOrd            as OMap
import qualified Data.HashSet                          as Set
import qualified Data.List                             as L
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Sequence                         as Seq
import qualified Data.Text                             as T
import qualified Database.PG.Query                     as Q
import qualified Language.GraphQL.Draft.Syntax         as G

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P
import qualified Hasura.RQL.DML.Delete.Types           as RQL
import qualified Hasura.RQL.DML.Insert                 as RQL
import qualified Hasura.RQL.DML.Insert.Types           as RQL
import qualified Hasura.RQL.DML.Internal               as RQL
import qualified Hasura.RQL.DML.Mutation               as RQL
import qualified Hasura.RQL.DML.Returning              as RQL
import qualified Hasura.RQL.DML.Returning.Types        as RQL
import qualified Hasura.RQL.DML.Select.Types           as RQL
import qualified Hasura.RQL.DML.Update                 as RQL
import qualified Hasura.RQL.DML.Update.Types           as RQL
import qualified Hasura.RQL.GBoolExp                   as RQL
import qualified Hasura.SQL.DML                        as S

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Parser                 (FieldParser, InputFieldsParser, Kind (..),
                                                        Parser, UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column          (qualifiedObjectToName)
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Insert
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.GraphQL.Schema.Common
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value



-- insert

insertIntoTable
  :: forall m n. (MonadSchema n m, MonadError QErr m, MonadReader QueryContext m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> InsPermInfo          -- ^ insert permissions of the table
  -> Maybe SelPermInfo    -- ^ select permissions of the table (if any)
  -> Maybe UpdPermInfo    -- ^ update permissions of the table (if any)
  -> m (FieldParser n (AnnMultiInsert UnpreparedValue))
insertIntoTable table fieldName description insertPerms selectPerms updatePerms = do
  columns         <- tableColumns table
  selectionParser <- mutationSelectionSet table selectPerms
  objectsParser   <- P.list <$> tableFieldsInput table insertPerms
  conflictParser  <- fmap join $ sequenceA $ conflictObject table selectPerms <$> updatePerms
  let conflictName = $$(G.litName "on_conflict")
      conflictDesc = "on conflict condition"
      objectsName  = $$(G.litName "objects")
      objectsDesc  = "the rows to be inserted"
      argsParser   = do
        conflictClause <- maybe
          (pure Nothing)
          (P.fieldOptional conflictName (Just conflictDesc))
          conflictParser
        objects <- P.field objectsName (Just objectsDesc) objectsParser
        pure (conflictClause, objects)
  pure $ P.subselection fieldName description argsParser selectionParser
    <&> \((conflictClause, objects), output) ->
       ( mkInsertObject objects table columns conflictClause insertPerms updatePerms
       , RQL.MOutMultirowFields output
       )

insertOneIntoTable
  :: forall m n. (MonadSchema n m, MonadError QErr m, MonadReader QueryContext m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> InsPermInfo          -- ^ insert permissions of the table
  -> SelPermInfo          -- ^ select permissions of the table
  -> Maybe UpdPermInfo    -- ^ update permissions of the table (if any)
  -> m (FieldParser n (AnnMultiInsert UnpreparedValue))
insertOneIntoTable table fieldName description insertPerms selectPerms updatePerms  = do
  columns         <- tableColumns table
  selectionParser <- tableSelectionSet table selectPerms
  objectParser    <- tableFieldsInput table insertPerms
  conflictParser  <- fmap join $ sequenceA $ conflictObject table (Just selectPerms) <$> updatePerms
  let conflictName  = $$(G.litName "on_conflict")
      conflictDesc  = "on conflict condition"
      objectName    = $$(G.litName "object")
      objectDesc    = "the row to be inserted"
      argsParser    = do
        conflictClause <- maybe
          (pure Nothing)
          (P.fieldOptional conflictName (Just conflictDesc))
          conflictParser
        object <- P.field objectName (Just objectDesc) objectParser
        pure (conflictClause, object)
  pure $ P.subselection fieldName description argsParser selectionParser
    <&> \((conflictClause, object), output) ->
       ( mkInsertObject [object] table columns conflictClause insertPerms updatePerms
       , RQL.MOutSinglerowObject output
       )


tableFieldsInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
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
          parser <- arrayRelationshipInput otherTable insPerms selPerms updPerms
          pure $ P.fieldOptional relFieldName Nothing parser `mapField`
            \arrRelIns -> AnnInsObj [] [] [RelIns arrRelIns relationshipInfo]
  let objectName = tableName <> $$(G.litName "_insert_input")
      objectDesc = G.Description $ "input type for inserting data into table \"" <> G.unName tableName <> "\""
  pure $ P.object objectName (Just objectDesc) $ catMaybes <$> sequenceA objectFields
    <&> mconcat

objectRelationshipInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
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
  let conflictName = $$(G.litName "on_conflict")
      objectName   = $$(G.litName "data")
      inputName    = tableName <> $$(G.litName "_obj_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting object relation for remote table \"" <> G.unName tableName <> "\""
      inputParser = do
        conflictClause <- maybe
          (pure Nothing)
          (P.fieldOptional conflictName Nothing)
          conflictParser
        object <- P.field objectName Nothing objectParser
        pure $ mkInsertObject object table columns conflictClause insertPerms updatePerms
  pure $ P.object inputName (Just inputDesc) inputParser <&> undefined

arrayRelationshipInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
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
  let conflictName = $$(G.litName "on_conflict")
      objectsName  = $$(G.litName "data")
      inputName    = tableName <> $$(G.litName "_arr_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting array relation for remote table \"" <> G.unName tableName <> "\""
      inputParser = do
        conflictClause <- maybe
          (pure Nothing)
          (P.fieldOptional conflictName Nothing)
          conflictParser
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
         , _aiDefVals        = fmap partialSQLExpToUnpreparedValue $ ipiSet insertPerms
         }
  where insertCheck = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ ipiCheck insertPerms
        updateCheck = fmapAnnBoolExp partialSQLExpToUnpreparedValue <$> (upiCheck =<< updatePerms)

conflictObject
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> Maybe SelPermInfo
  -> UpdPermInfo
  -> m (Maybe (Parser 'Input n (RQL.ConflictClauseP1 UnpreparedValue)))
conflictObject table selectPerms updatePerms = runMaybeT $ do
  tableName        <- lift $ qualifiedObjectToName table
  columnsEnum      <- MaybeT $ tableUpdateColumnsEnum table updatePerms
  constraintParser <- lift $ conflictConstraint table
  whereExpParser   <- lift $ boolExp table selectPerms
  let objectName = tableName <> $$(G.litName "_on_conflict")
      objectDesc = G.Description $ "on conflict condition type for table \"" <> G.unName tableName <> "\""
      constraintName = $$(G.litName "constraint")
      columnsName    = $$(G.litName "update_columnns")
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
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> m (Parser 'Both n ConstraintName)
conflictConstraint table =
  memoizeOn 'conflictConstraint table do
  tableName <- qualifiedObjectToName table
  let enumName  = tableName <> $$(G.litName "_constraint")
      enumDesc  = G.Description $ "unique or primary key constraints on table " <> G.unName tableName <> "\""
      idKeyName = tableName <> $$(G.litName "_id_key")
      idKeyDesc = "unique or primary key constraint"
      pKeyName  = tableName <> $$(G.litName "_pkey")
      pKeyDesc  = "unique or primary key constraint"
  pure $ P.enum enumName (Just enumDesc) $ NE.fromList
    [ (define idKeyName idKeyDesc, ConstraintName $ G.unName idKeyName)
    , (define pKeyName  pKeyDesc,  ConstraintName $ G.unName pKeyName)
    ]
  where
    define name desc = P.mkDefinition name (Just desc) P.EnumValueInfo



-- update

updateTable
  :: forall m n. (MonadSchema n m, MonadError QErr m, MonadReader QueryContext m)
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

updateTableByPk
  :: forall m n. (MonadSchema n m, MonadError QErr m, MonadReader QueryContext m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> UpdPermInfo          -- ^ update permissions of the table
  -> SelPermInfo          -- ^ select permissions of the table
  -> m (Maybe (FieldParser n (RQL.AnnUpdG UnpreparedValue)))
updateTableByPk table fieldName description updatePerms selectPerms = runMaybeT $ do
  columns   <- lift   $ tableSelectColumns table selectPerms
  pkArgs    <- MaybeT $ primaryKeysArguments table selectPerms
  opArgs    <- MaybeT $ updateOperators table updatePerms
  selection <- lift $ tableSelectionSet table selectPerms
  let argsParser = liftA2 (,) opArgs pkArgs
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
    -- FIXME!!!
    -- OpExps should also include preset columns
    permissionFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ upiFilter updatePerms
    checkExp = maybe annBoolExpTrue (fmapAnnBoolExp partialSQLExpToUnpreparedValue) $ upiCheck updatePerms


updateOperators
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable -- ^ qualified name of the table
  -> UpdPermInfo    -- ^ update permissions of the table
  -> m (Maybe (InputFieldsParser n [(PGCol, RQL.UpdOpExpG UnpreparedValue)]))
updateOperators table updatePermissions = do
  tableName <- qualifiedObjectToName table
  columns   <- tableUpdateColumns table updatePermissions
  let intCols  = onlyIntCols   columns
      jsonCols = onlyJSONBCols columns
  parsers <- catMaybes <$> sequenceA
    [ updateOperator tableName $$(G.litName "_set")
        columnParser RQL.UpdSet columns
        ("sets the columns of the filtered rows to the given values")
        (G.Description $ "input type for updating data in table \"" <> G.unName tableName <> "\"")
    , updateOperator tableName $$(G.litName "_inc")
        intParser RQL.UpdInc intCols
        ("increments the integer columns with given value of the filtered values")
        (G.Description $"input type for incrementing integer columns in table \"" <> G.unName tableName <> "\"")

    -- WIP NOTE
    -- all json operators use the same description for the input type and the field
    -- i kept the same behaviour
    -- the comments also mention "_concat", but it seems it never existed
    -- i am guessing that's what prepend and append are for?
    , updateOperator tableName $$(G.litName "_prepend")
        textParser RQL.UpdPrepend jsonCols
        "prepend existing jsonb value of filtered columns with new jsonb value"
        "prepend existing jsonb value of filtered columns with new jsonb value"
    , updateOperator tableName $$(G.litName "_append")
        textParser RQL.UpdAppend jsonCols
        "append existing jsonb value of filtered columns with new jsonb value"
        "append existing jsonb value of filtered columns with new jsonb value"
    , updateOperator tableName $$(G.litName "_delete_key")
        textParser RQL.UpdDeleteKey jsonCols
        "delete key/value pair or string element. key/value pairs are matched based on their key value"
        "delete key/value pair or string element. key/value pairs are matched based on their key value"
    , updateOperator tableName $$(G.litName "_delete_elem")
        intParser RQL.UpdDeleteElem jsonCols
        "delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array"
        "delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array"
    , updateOperator tableName $$(G.litName "_delete_at_path")
        (fmap P.list . textParser) RQL.UpdDeleteAtPath jsonCols
        "delete the field or element with specified path (for JSON arrays, negative integers count from the end)"
        "delete the field or element with specified path (for JSON arrays, negative integers count from the end)"
    ]
  whenMaybe (not $ null parsers) do
    let allowedOperators = fst <$> parsers
    pure $ fmap (concat . catMaybes) (sequenceA $ snd <$> parsers)
      `P.bindFields` \opExps -> do
        -- there needs to be at least one column in the update
        -- FIXME this is wrong: if there are pre-sets values we shouldn't fail here
        when (null opExps) $ parseError $
          "at lease one of " <> (T.intercalate ", " allowedOperators) <> " is expected"
        -- no column should appear twice
        let groupedExps   = L.groupBy ((==) `on` fst) $ sortOn (getPGColTxt . fst) opExps
            erroneousExps = concat $ filter ((> 1) . length) groupedExps
        when (not $ null erroneousExps) $ parseError $ "column found in multiple operators; "
          <> (T.intercalate ", " $ flip map erroneousExps \(column, opExp) ->
                 getPGColTxt column <> " in " <> RQL.updateOperatorText opExp)
        -- FIXME: validate JSON parsing here?
        pure opExps
  where
    columnParser columnInfo = fmap P.mkParameter <$> P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
    textParser   _          = fmap P.mkParameter <$> P.column (PGColumnScalar PGText)    (G.Nullability False)
    intParser    _          = fmap P.mkParameter <$> P.column (PGColumnScalar PGInteger) (G.Nullability False)

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

deleteFromTable
  :: forall m n. (MonadSchema n m, MonadError QErr m, MonadReader QueryContext m)
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

deleteFromTableByPk
  :: forall m n. (MonadSchema n m, MonadError QErr m, MonadReader QueryContext m)
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

mutationSelectionSet
  :: forall m n. (MonadSchema n m, MonadError QErr m, MonadReader QueryContext m)
  => QualifiedTable
  -> Maybe SelPermInfo
  -> m (Parser 'Output n (RQL.MutFldsG UnpreparedValue))
mutationSelectionSet table selectPerms =
  memoizeOn 'mutationSelectionSet table do
  tableName <- qualifiedObjectToName table
  returning <- runMaybeT do
    permissions <- MaybeT $ pure selectPerms
    tableSet    <- lift $ tableSelectionSet table permissions
    let returningName = $$(G.litName "returning")
        returningDesc = "data from the rows affected by the mutation"
    pure $ RQL.MRet <$> P.subselection_ returningName  (Just returningDesc) tableSet
  let affectedRowsName = $$(G.litName "affected_rows")
      affectedRowsDesc = "number of rows affected by the mutation"
      selectionName    = tableName <> $$(G.litName "_mutation_response")
      selectionDesc    = G.Description $ "response of any mutation on the table \"" <> G.unName tableName <> "\""

      selectionFields  = catMaybes
        [ Just $ RQL.MCount <$
            P.selection_ affectedRowsName (Just affectedRowsDesc) P.int
        , returning
        ]
  pure $ P.selectionSet selectionName (Just selectionDesc) selectionFields
    <&> parsedSelectionsToFields RQL.MExp

primaryKeysArguments
  :: forall m n. (MonadSchema n m, MonadError QErr m)
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

third :: (c -> d) -> (a,b,c) -> (a,b,d)
third f (a,b,c) = (a,b,f c)

-- insert translation

-- FIXME: this should probably be elsewhere, perhaps in Execute?
-- Furthermore, this has been lifted almost verbatim from Resolve
-- and is unlikely to be correct on the first try. For instance:
-- - all calls to "validate" have been removed, since everything they
--   do should be baked directly into the parsers above.
-- - some paths still throw errors; is this something we're okay with
--   or should this operation be total? what should we move to our
--   internal representation, to avoid errors here?
-- - is some of this code dead or unused? are there paths never taken?
--   can it be simplified?

traverseAnnInsert
  :: (Applicative f)
  => (a -> f b)
  -> AnnMultiInsert a
  -> f (AnnMultiInsert b)
traverseAnnInsert f (annIns, mutationOutput) =
  (,) <$> traverseMulti annIns
      <*> RQL.traverseMutationOutput f mutationOutput
  where
    traverseMulti (AnnIns objs table conflictClause (insertCheck, updateCheck) columns defaultValues) =
      AnnIns <$> traverse traverseObject objs
             <*> pure table
             <*> traverse (traverse f) conflictClause
             <*> ( (,) <$> traverseAnnBoolExp f insertCheck
                       <*> traverse (traverseAnnBoolExp f) updateCheck
                 )
             <*> pure columns
             <*> traverse f defaultValues
    traverseSingle (AnnIns obj table conflictClause (insertCheck, updateCheck) columns defaultValues) =
      AnnIns <$> traverseObject obj
             <*> pure table
             <*> traverse (traverse f) conflictClause
             <*> ( (,) <$> traverseAnnBoolExp f insertCheck
                       <*> traverse (traverseAnnBoolExp f) updateCheck
                 )
             <*> pure columns
             <*> traverse f defaultValues
    traverseObject (AnnInsObj columns objRels arrRels) =
      AnnInsObj <$> traverse (traverse f) columns
                <*> traverse (traverseRel traverseSingle) objRels
                <*> traverse (traverseRel traverseMulti)  arrRels
    traverseRel t (RelIns object relInfo) =
      RelIns <$> t object <*> pure relInfo


convertToSQLTransaction
  :: AnnMultiInsert S.SQLExp
  -> Bool
  -> RespTx
convertToSQLTransaction (annIns, mutationOutput) stringifyNum =
  if null $ _aiInsObj annIns
  then pure $ buildEmptyMutResp mutationOutput
  else insertMultipleObjects annIns mutationOutput stringifyNum

insertMultipleObjects
  :: MultiObjIns S.SQLExp
  -> RQL.MutationOutput
  -> Bool
  -> Q.TxE QErr EncJSON
insertMultipleObjects multiObjIns mutationOutput stringifyNum =
  bool withoutRelsInsert withRelsInsert anyRelsToInsert
  where
    AnnIns insObjs table conflictClause checkCondition columnInfos defVals = multiObjIns
    allInsObjRels = concatMap _aioObjRels insObjs
    allInsArrRels = concatMap _aioArrRels insObjs
    anyRelsToInsert = not $ null allInsArrRels && null allInsObjRels

    withoutRelsInsert =
      let columnValues = map (mkSQLRow defVals) $ map _aioColumns insObjs
          columnNames  = Map.keys defVals
          insertQuery  = RQL.InsertQueryP1
            table
            columnNames
            columnValues
            conflictClause
            checkCondition
            mutationOutput
            columnInfos
      in RQL.insertP2 stringifyNum (insertQuery, Seq.empty)
         -- FIXME: is this correct? Can we pass empty PrepArgs here?
         -- if no: they'll need to be injected all the way from outside
         -- I think it's fine since the other branch doesn't rely on prep args?

    withRelsInsert = do
      insertRequests <- for insObjs \obj -> do
        let singleObj = AnnIns obj table conflictClause checkCondition columnInfos defVals
        insertObject singleObj stringifyNum
      let affectedRows = sum $ map fst insertRequests
          columnValues = catMaybes $ map snd insertRequests
      selectExpr <- RQL.mkSelCTEFromColVals table columnInfos columnValues
      let sqlOutput = toSQL $ RQL.mkMutationOutputExp table (Just affectedRows) selectExpr mutationOutput stringifyNum
      runIdentity . Q.getRow <$> Q.rawQE RQL.dmlTxErrorHandler (Q.fromBuilder sqlOutput) [] False

insertObject
  :: SingleObjIns S.SQLExp
  -> Bool
  -> Q.TxE QErr (Int, Maybe (ColumnValues TxtEncodedPGVal))
insertObject singleObjIns stringifyNum = do
  -- insert all object relations and fetch this insert dependent column values
  objInsRes <- forM objectRels $ insertObjRel stringifyNum

  -- prepare final insert columns
  let objRelAffRows = sum $ map fst objInsRes
      objRelDeterminedCols = concatMap snd objInsRes
      finalInsCols = columns <> objRelDeterminedCols

  cte <- mkInsertQ table onConflict finalInsCols defaultValues checkCond

  MutateResp affRows colVals <- RQL.mutateAndFetchCols table allColumns (cte, Seq.empty) stringifyNum
  colValM <- asSingleObject colVals

  arrRelAffRows <- bool (withArrRels colValM) (return 0) $ null arrayRels
  let totAffRows = objRelAffRows + affRows + arrRelAffRows

  return (totAffRows, colValM)
  where
    AnnIns annObj table onConflict checkCond allColumns defaultValues = singleObjIns
    AnnInsObj columns objectRels arrayRels = annObj

    arrRelDepCols = flip getColInfos allColumns $
      concatMap (Map.keys . riMapping . _riRelInfo) arrayRels

    withArrRels colValM = do
      colVal <- onNothing colValM $ throw400 NotSupported cannotInsArrRelErr
      arrDepColsWithVal <- fetchFromColVals colVal arrRelDepCols
      arrInsARows <- forM arrayRels $ insertArrRel arrDepColsWithVal stringifyNum
      return $ sum arrInsARows

    asSingleObject = \case
      [] -> pure Nothing
      [r] -> pure $ Just r
      _ -> throw500 "more than one row returned"

    cannotInsArrRelErr =
      "cannot proceed to insert array relations since insert to table "
      <> table <<> " affects zero rows"

insertObjRel
  :: Bool
  -> ObjRelIns S.SQLExp
  -> Q.TxE QErr (Int, [(PGCol, S.SQLExp)])
insertObjRel stringifyNum objRelIns = do
  (affRows, colValM) <- insertObject singleObjIns stringifyNum
  colVal <- onNothing colValM $ throw400 NotSupported errMsg
  retColsWithVals <- fetchFromColVals colVal rColInfos
  let c = mergeListsWith (Map.toList mapCols) retColsWithVals
        (\(_, rCol) (col, _) -> rCol == col)
        (\(lCol, _) (_, cVal) -> (lCol, cVal))
  return (affRows, c)
  where
    RelIns singleObjIns relInfo = objRelIns
    -- multiObjIns = singleToMulti singleObjIns
    relName = riName relInfo
    table = riRTable relInfo
    mapCols = riMapping relInfo
    allCols = _aiTableCols singleObjIns
    rCols = Map.elems mapCols
    rColInfos = getColInfos rCols allCols
    errMsg = "cannot proceed to insert object relation "
             <> relName <<> " since insert to table "
             <> table <<> " affects zero rows"

insertArrRel
  :: [(PGCol, S.SQLExp)]
  -> Bool
  -> ArrRelIns S.SQLExp
  -> Q.TxE QErr Int
insertArrRel resCols stringifyNum arrRelIns = do
  resBS <- insertMultipleObjects multiObjIns mutOutput stringifyNum
  resObj <- decodeEncJSON resBS
  onNothing (Map.lookup ("affected_rows" :: T.Text) resObj) $
    throw500 "affected_rows not returned in array rel insert"
  where
    RelIns multiObjIns relInfo = arrRelIns
    mutOutput = RQL.MOutMultirowFields [("affected_rows", RQL.MCount)]

mkInsertQ
  :: MonadError QErr m
  => QualifiedTable
  -> Maybe (RQL.ConflictClauseP1 S.SQLExp)
  -> [(PGCol, S.SQLExp)]
  -> Map.HashMap PGCol S.SQLExp
  -> (AnnBoolExpSQL, Maybe AnnBoolExpSQL)
  -> m S.CTE
mkInsertQ table onConflictM insCols defVals (insCheck, updCheck) = do
  let sqlConflict = RQL.toSQLConflict table <$> onConflictM
      sqlExps = mkSQLRow defVals insCols
      valueExp = S.ValuesExp [S.TupleExp sqlExps]
      tableCols = Map.keys defVals
      sqlInsert =
        S.SQLInsert table tableCols valueExp sqlConflict
          . Just
          $ S.RetExp
            [ S.selectStar
            , S.Extractor
                (RQL.insertOrUpdateCheckExpr table onConflictM
                  (RQL.toSQLBoolExp (S.QualTable table) insCheck)
                  (fmap (RQL.toSQLBoolExp (S.QualTable table)) updCheck))
                Nothing
            ]
  pure $ S.CTEInsert sqlInsert

fetchFromColVals
  :: MonadError QErr m
  => ColumnValues TxtEncodedPGVal
  -> [PGColumnInfo]
  -> m [(PGCol, S.SQLExp)]
fetchFromColVals colVal reqCols =
  forM reqCols $ \ci -> do
    let valM = Map.lookup (pgiColumn ci) colVal
    val <- onNothing valM $ throw500 $ "column "
           <> pgiColumn ci <<> " not found in given colVal"
    let pgColVal = case val of
          TENull  -> S.SENull
          TELit t -> S.SELit t
    return (pgiColumn ci, pgColVal)

mkSQLRow :: Map.HashMap PGCol S.SQLExp -> [(PGCol, S.SQLExp)] -> [S.SQLExp]
mkSQLRow defVals withPGCol = map snd $
  flip map (Map.toList defVals) $
    \(col, defVal) -> (col,) $ fromMaybe defVal $ Map.lookup col withPGColMap
  where
    withPGColMap = Map.fromList withPGCol

buildEmptyMutResp :: RQL.MutationOutput -> EncJSON
buildEmptyMutResp = \case
  RQL.MOutMultirowFields mutFlds -> encJFromJValue $ OMap.fromList $ map (second convMutFld) mutFlds
  RQL.MOutSinglerowObject _      -> encJFromJValue $ J.Object mempty
  where
    convMutFld = \case
      RQL.MCount -> J.toJSON (0 :: Int)
      RQL.MExp e -> J.toJSON e
      RQL.MRet _ -> J.toJSON ([] :: [J.Value])

mergeListsWith
  :: [a] -> [b] -> (a -> b -> Bool) -> (a -> b -> c) -> [c]
mergeListsWith _ [] _ _ = []
mergeListsWith [] _ _ _ = []
mergeListsWith (x:xs) l b f = case find (b x) l of
  Nothing -> mergeListsWith xs l b f
  Just y  ->  f x y : mergeListsWith xs l b f

decodeEncJSON :: (J.FromJSON a, QErrM m) => EncJSON -> m a
decodeEncJSON =
  either (throw500 . T.pack) decodeValue .
  J.eitherDecode . encJToLBS

------------------- Actions -----------------------
actionAsync
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => NonObjectTypeMap
  -> ActionInfo
  -> m (Maybe (FieldParser n AnnActionMutationAsync))
actionAsync nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- lift askRoleName
  guard $ roleName == adminRole || roleName `Map.member` permissions
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  let fieldName = unActionName actionName
      description = G.Description <$> comment
      outputTypeParser = P.scalar $$(G.litName "uuid") Nothing P.SRString
  pure $ P.selection fieldName description inputArguments outputTypeParser
         <&> AnnActionMutationAsync actionName
  where
    ActionInfo actionName outputObject definition permissions comment = actionInfo

actionSync
  :: forall m n. (MonadSchema n m, MonadError QErr m, MonadReader QueryContext m)
  => NonObjectTypeMap
  -> ActionInfo
  -> m (Maybe (FieldParser n (AnnActionMutationSync UnpreparedValue)))
actionSync nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- lift askRoleName
  guard $ roleName == adminRole || roleName `Map.member` permissions
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  let scalarOrEnumFields = map scalarOrEnumFieldParser $ toList $ _otdFields outputObject
  relationshipFields <- forM (_otdRelationships outputObject) $ traverse relationshipFieldParser
  let allFieldParsers = scalarOrEnumFields <>
                        maybe [] toList relationshipFields
      selectionSet =
        let outputTypeName = unObjectTypeName $ _otdName outputObject
            outputTypeDescription = _otdDescription outputObject
        in P.selectionSet outputTypeName outputTypeDescription allFieldParsers
           <&> parsedSelectionsToFields RQL.FExp

  stringifyNum <- asks qcStringifyNum
  pure $ P.subselection fieldName description inputArguments selectionSet
         <&> \(argsJson, fields) -> AnnActionMutationSync
               { _aamsName = actionName
               , _aamsFields = fields
               , _aamsPayload = argsJson
               , _aamsOutputType = _adOutputType definition
               , _aamsOutputFields = getActionOutputFields outputObject
               , _aamsDefinitionList = mkDefinitionList outputObject
               , _aamsWebhook = _adHandler definition
               , _aamsHeaders = _adHeaders definition
               , _aamsForwardClientHeaders = _adForwardClientHeaders definition
               , _aamsStrfyNum = stringifyNum
               }
  where
    ActionInfo actionName outputObject definition permissions comment = actionInfo

    scalarOrEnumFieldParser
      :: ObjectFieldDefinition (G.GType, AnnotatedObjectFieldType)
      -> FieldParser n (RQL.AnnFldG UnpreparedValue)
    scalarOrEnumFieldParser (ObjectFieldDefinition name _ description ty) =
      let (gType, objectFieldType) = ty
          fieldName = unObjectFieldName name
          -- FIXME?
          pgColumnInfo = PGColumnInfo (unsafePGCol $ G.unName fieldName)
                         fieldName 0 (PGColumnScalar PGJSON) (G.isNullable gType) Nothing
          fieldParser = case objectFieldType of
            AOFTScalar def -> customScalarParser def
            AOFTEnum def   -> customEnumParser def
      in bool P.nonNullableField id (G.isNullable gType) $
         P.selection_ (unObjectFieldName name) description fieldParser
         $> RQL.mkAnnColField pgColumnInfo Nothing

    relationshipFieldParser
      :: TypeRelationship TableInfo PGColumnInfo
      -> MaybeT m (FieldParser n (RQL.AnnFldG UnpreparedValue))
    relationshipFieldParser typeRelationship = do
      let TypeRelationship relName relType tableInfo fieldMapping = typeRelationship
          tableName = _tciName $ _tiCoreInfo tableInfo
          fieldName = unRelationshipName relName
      roleName <- lift askRoleName
      tablePerms <- MaybeT $ pure $ RQL.getPermInfoMaybe roleName PASelect tableInfo
      tableParser <- lift $ selectTable tableName fieldName Nothing tablePerms
      pure $ tableParser <&> \selectExp ->
        let tableRelName = RelName $ mkNonEmptyTextUnsafe $ G.unName fieldName
            columnMapping = Map.fromList $
              [ (unsafePGCol $ G.unName $ unObjectFieldName k, pgiColumn v)
              | (k, v) <- Map.toList fieldMapping
              ]
            annotatedRelationship = RQL.AnnRelG tableRelName columnMapping selectExp
        in case relType of
             ObjRel -> RQL.FObj annotatedRelationship
             ArrRel -> RQL.FArr $ RQL.ASSimple annotatedRelationship

mkDefinitionList :: AnnotatedObjectType -> [(PGCol, PGScalarType)]
mkDefinitionList annotatedOutputType =
  [ (unsafePGCol $ G.unName k,  PGJSON) -- FIXME? - Use relationship mapping references
  | k <- map (unObjectFieldName . _ofdName) $
         toList $ _otdFields annotatedOutputType
  ]

-- This should into schema/action.hs
actionInputArguments
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => NonObjectTypeMap
  -> [ArgumentDefinition (G.GType, NonObjectCustomType)]
  -> m (InputFieldsParser n J.Value)
actionInputArguments nonObjectTypeMap arguments = do
  argumentParsers <- for arguments $ \argument -> do
    let ArgumentDefinition argumentName (gType, nonObjectType) argumentDescription = argument
        name = unArgumentName argumentName
    (name,) <$> argumentParser name argumentDescription gType nonObjectType
  pure $ fmap (J.toJSON . Map.fromList) $ for argumentParsers $
         \(name, parser) -> (name,) <$> parser
  where
    argumentParser
      :: G.Name
      -> Maybe G.Description
      -> G.GType
      -> NonObjectCustomType
      -> m (InputFieldsParser n J.Value)
    argumentParser name description gType nonObjectType =
      case nonObjectType of
        NOCTScalar def -> pure $ P.field name description $ mkParserModifier gType $ customScalarParser def
        NOCTEnum def -> pure $ P.field name description $ mkParserModifier gType $ customEnumParser def
        NOCTInputObject def -> do
          let InputObjectTypeDefinition typeName objectDescription inputFields = def
              objectName = unInputObjectTypeName typeName
          inputFieldsParsers <- forM (toList inputFields) $ \inputField -> do
            let InputObjectFieldName fieldName = _iofdName inputField
                GraphQLType fieldType = _iofdType inputField
            nonObjectFieldType <-
              onNothing (Map.lookup (G.getBaseType fieldType) nonObjectTypeMap) $
                throw500 "object type for a field found in custom input object type"
            (fieldName,) <$> argumentParser fieldName (_iofdDescription inputField) fieldType nonObjectFieldType

          pure $ P.field name description $ mkParserModifier gType $ P.object objectName objectDescription $
            fmap (J.toJSON . Map.fromList) $ for inputFieldsParsers $ \(n, parser) -> (n,) <$> parser

mkParserModifier
  :: (MonadParse m, 'Input P.<: k)
  => G.GType -> Parser k m J.Value -> Parser k m J.Value
mkParserModifier = \case
  G.TypeNamed nullable _    -> nullableModifier nullable
  G.TypeList nullable gType ->
    nullableModifier nullable . fmap J.toJSON . P.list . mkParserModifier gType
  where
    nullableModifier =
      bool (fmap J.toJSON) (fmap J.toJSON . P.nullable) . G.unNullability

customScalarParser
  :: MonadParse m
  => ScalarTypeDefinition -> Parser 'Both m J.Value
customScalarParser (ScalarTypeDefinition name description) =
  P.scalar name description P.SRAny

customEnumParser
  :: MonadParse m
  => EnumTypeDefinition -> Parser 'Both m J.Value
customEnumParser (EnumTypeDefinition typeName description enumValues) =
  let enumName = unEnumTypeName typeName
      enumValueDefinitions = enumValues <&> \enumValue ->
        let valueName = G.unEnumValue $ _evdValue enumValue
        in (, J.toJSON valueName) $ P.mkDefinition valueName
           (_evdDescription enumValue) P.EnumValueInfo
  in P.enum enumName description enumValueDefinitions
