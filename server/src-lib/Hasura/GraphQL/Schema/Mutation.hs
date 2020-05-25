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

import           Data.Int                      (Int32)

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Delete         as RQL
import qualified Hasura.RQL.DML.Insert         as RQL
import qualified Hasura.RQL.DML.Returning      as RQL
import qualified Hasura.RQL.DML.Update         as RQL
import qualified Hasura.SQL.DML                as SQL

import           Hasura.GraphQL.Parser         (FieldsParser, Kind (..), Parser,
                                                UnpreparedValue (..), mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Parser.Column  (qualifiedObjectToName)
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.GraphQL.Schema.Table
import           Hasura.RQL.Types
import           Hasura.SQL.Types



-- insert


-- WIP NOTE
-- An abstract representation of insert was tricky to pin down.
-- The following structures are taken from Resolve, and slightly
-- modified.
-- What needs to be decided is where those should go and what we
-- should do with them, really.
-- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

data AnnIns a v
  = AnnIns
  { _aiInsObj         :: !a
  , _aiConflictClause :: !(Maybe (RQL.ConflictClauseP1 v))
  , _aiCheckCond      :: !(AnnBoolExp v, AnnBoolExpPartialSQL)
  , _aiTableCols      :: ![PGColumnInfo]
  , _aiDefVals        :: !(PreSetColsPartial)
  } deriving (Show, Eq)

type SingleObjIns v = AnnIns (AnnInsObj v) v
type MultiObjIns  v = AnnIns [AnnInsObj v] v

data RelIns a
  = RelIns
  { _riAnnIns  :: !a
  , _riRelInfo :: !RelInfo
  } deriving (Show, Eq)

type ObjRelIns v = RelIns (SingleObjIns v)
type ArrRelIns v = RelIns (MultiObjIns  v)

data AnnInsObj v
  = AnnInsObj
  { _aioColumns :: ![(PGCol, v)]
  , _aioObjRels :: ![ObjRelIns v]
  , _aioArrRels :: ![ArrRelIns v]
  } deriving (Show, Eq)

type AnnSingleInsert v = (SingleObjIns v, RQL.MutationOutputG v)
type AnnMultiInsert  v = (MultiObjIns  v, RQL.MutationOutputG v)

instance Semigroup (AnnInsObj v) where
  (AnnInsObj col1 obj1 rel1) <> (AnnInsObj col2 obj2 rel2) =
    AnnInsObj (col1 <> col2) (obj1 <> obj2) (rel1 <> rel2)

instance Monoid (AnnInsObj v) where
  mempty = AnnInsObj [] [] []

-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


insertIntoTable
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> InsPermInfo          -- ^ insert permissions of the table
  -> Maybe SelPermInfo    -- ^ select permissions of the table (if any)
  -> Maybe UpdPermInfo    -- ^ update permissions of the table (if any)
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (AnnMultiInsert UnpreparedValue)))
insertIntoTable table fieldName description insertPerms selectPerms updatePerms stringifyNum = do
  columns         <- tableColumns table
  selectionParser <- mutationSelectionSet table selectPerms stringifyNum
  objectsParser   <- P.list <$> tableFieldsInput table insertPerms
  conflictParser  <- fmap join $ sequenceA $ conflictObject table selectPerms <$> updatePerms
  let conflictName = $$(G.litName "on_conflict")
      conflictDesc = "on conflict condition"
      objectsName  = $$(G.litName "objects")
      objectsDesc  = "the rows to be inserted"
      argsParser   = do
        onConflict <- maybe
          (pure Nothing)
          (P.fieldOptional conflictName (Just conflictDesc))
          conflictParser
        objects    <- P.field objectsName (Just objectsDesc) objectsParser
        pure (onConflict, objects)
  pure $ P.selection fieldName description argsParser selectionParser
    `mapField` \(_, (onConflict, objects), output) ->
       ( AnnIns { _aiInsObj         = objects
                , _aiConflictClause = onConflict
                , _aiCheckCond      = (undefined {- FIXME!!! -}, ipiCheck insertPerms)
                , _aiTableCols      = columns
                , _aiDefVals        = ipiSet insertPerms
                }
       , RQL.MOutMultirowFields output
       )

insertOneIntoTable
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> InsPermInfo          -- ^ insert permissions of the table
  -> SelPermInfo          -- ^ select permissions of the table
  -> Maybe UpdPermInfo    -- ^ update permissions of the table (if any)
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (AnnMultiInsert UnpreparedValue)))
insertOneIntoTable table fieldName description insertPerms selectPerms updatePerms stringifyNum = do
  columns         <- tableColumns table
  selectionParser <- tableSelectionSet table selectPerms stringifyNum
  objectsParser   <- tableFieldsInput table insertPerms
  conflictParser  <- fmap join $ sequenceA $ conflictObject table (Just selectPerms) <$> updatePerms
  let conflictName  = $$(G.litName "on_conflict")
      conflictDesc  = "on conflict condition"
      objectsName   = $$(G.litName "objects")
      objectsDesc   = "the rows to be inserted"
      argsParser    = do
        onConflict <- maybe
          (pure Nothing)
          (P.fieldOptional conflictName (Just conflictDesc))
          conflictParser
        objects    <- P.field objectsName (Just objectsDesc) objectsParser
        pure (onConflict, objects)
  pure $ P.selection fieldName description argsParser selectionParser
    `mapField` \(_, (onConflict, object), output) ->
       ( AnnIns { _aiInsObj         = [object]
                , _aiConflictClause = onConflict
                , _aiCheckCond      = (undefined {- FIXME!!! -}, ipiCheck insertPerms)
                , _aiTableCols      = columns
                , _aiDefVals        = ipiSet insertPerms
                }
       , RQL.MOutSinglerowObject output
       )


tableFieldsInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable -- ^ qualified name of the table
  -> InsPermInfo    -- ^ insert permissions of the table
  -> m (Parser 'Input n (AnnInsObj UnpreparedValue))
tableFieldsInput table insertPerms = do -- TODO: memoize this!
  tableName    <- qualifiedObjectToName table
  allFields    <- _tciFieldInfoMap . _tiCoreInfo <$> askTableInfo table
  objectFields <- catMaybes <$> for (Map.elems allFields) \case
    FIComputedField _ -> pure Nothing
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
objectRelationshipInput table insertPerms selectPerms updatePerms = do
  tableName      <- qualifiedObjectToName table
  columns        <- tableColumns table
  objectParser   <- tableFieldsInput table insertPerms
  conflictParser <- fmap join $ sequenceA $ conflictObject table selectPerms <$> updatePerms
  let conflictName = $$(G.litName "on_conflict")
      objectName   = $$(G.litName "data")
      inputName    = tableName <> $$(G.litName "_obj_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting object relation for remote table \"" <> G.unName tableName <> "\""
      inputParser = do
        conflict <- maybe
          (pure Nothing)
          (P.fieldOptional conflictName Nothing)
          conflictParser
        object   <- P.field objectName Nothing objectParser
        pure $ AnnIns
          { _aiInsObj         = object
          , _aiConflictClause = conflict
          , _aiCheckCond      = (undefined {- FIXME!!! -}, ipiCheck insertPerms)
          , _aiTableCols      = columns
          , _aiDefVals        = ipiSet insertPerms
          }
  pure $ P.object inputName (Just inputDesc) inputParser <&> undefined

arrayRelationshipInput
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> InsPermInfo
  -> Maybe SelPermInfo
  -> Maybe UpdPermInfo
  -> m (Parser 'Input n (MultiObjIns UnpreparedValue))
arrayRelationshipInput table insertPerms selectPerms updatePerms = do
  tableName      <- qualifiedObjectToName table
  columns        <- tableColumns table
  objectParser   <- tableFieldsInput table insertPerms
  conflictParser <- fmap join $ sequenceA $ conflictObject table selectPerms <$> updatePerms
  let conflictName = $$(G.litName "on_conflict")
      objectsName  = $$(G.litName "data")
      inputName    = tableName <> $$(G.litName "_arr_rel_insert_input")
      inputDesc    = G.Description $ "input type for inserting array relation for remote table \"" <> G.unName tableName <> "\""
      inputParser = do
        conflict <- maybe
          (pure Nothing)
          (P.fieldOptional conflictName Nothing)
          conflictParser
        objects  <- P.field objectsName Nothing $ P.list objectParser
        pure $ AnnIns
          { _aiInsObj         = objects
          , _aiConflictClause = conflict
          , _aiCheckCond      = (undefined {- FIXME!!! -}, ipiCheck insertPerms)
          , _aiTableCols      = columns
          , _aiDefVals        = ipiSet insertPerms
          }
  pure $ P.object inputName (Just inputDesc) inputParser

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
          _  -> RQL.CP1Update constraint columns (upiSet updatePerms) whereExp
  pure $ P.object objectName (Just objectDesc) fieldsParser

conflictConstraint
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> m (Parser 'Both n ConstraintName)
conflictConstraint table = do
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
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> UpdPermInfo          -- ^ update permissions of the table
  -> Maybe SelPermInfo    -- ^ select permissions of the table (if any)
  -> Bool
  -> m (Maybe (FieldsParser 'Output n (Maybe (RQL.AnnUpdG UnpreparedValue))))
updateTable table fieldName description updatePerms selectPerms stringifyNum = runMaybeT $ do
  let whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be updated"
  opArgs    <- MaybeT $ updateOperators table updatePerms
  columns   <- lift $ tableColumns table
  whereArg  <- lift $ P.field whereName (Just whereDesc) <$> boolExp table selectPerms
  selection <- lift $ mutationSelectionSet table selectPerms stringifyNum
  let argsParser = liftA2 (,) opArgs whereArg
  pure $ P.selection fieldName description argsParser selection
    `mapField` (mkUpdateObject table columns updatePerms . third RQL.MOutMultirowFields)

updateTableByPk
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> UpdPermInfo          -- ^ update permissions of the table
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (Maybe (FieldsParser 'Output n (Maybe (RQL.AnnUpdG UnpreparedValue))))
updateTableByPk table fieldName description updatePerms selectPerms stringifyNum = runMaybeT $ do
  columns   <- lift   $ tableSelectColumns table selectPerms
  pkArgs    <- MaybeT $ primaryKeysArguments table selectPerms
  opArgs    <- MaybeT $ updateOperators table updatePerms
  selection <- lift $ tableSelectionSet table selectPerms stringifyNum
  let argsParser = liftA2 (,) opArgs pkArgs
  pure $ P.selection fieldName description argsParser selection
    `mapField` (mkUpdateObject table columns updatePerms . third RQL.MOutSinglerowObject)

mkUpdateObject
  :: QualifiedTable
  -> [PGColumnInfo]
  -> UpdPermInfo
  -> ( G.Name
     , ( [(PGColumnInfo, RQL.UpdOpExpG UnpreparedValue)]
       , AnnBoolExp UnpreparedValue
       )
     , RQL.MutationOutputG UnpreparedValue
     )
  -> RQL.AnnUpdG UnpreparedValue
mkUpdateObject table columns updatePerms (_, (opExps, whereExp), mutationOutput) =
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


updateOperators
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable -- ^ qualified name of the table
  -> UpdPermInfo    -- ^ update permissions of the table
  -> m (Maybe (FieldsParser 'Input n [(PGColumnInfo, RQL.UpdOpExpG UnpreparedValue)]))
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
    , updateOperator tableName $$(G.litName "_delete_path_at")
        textParser RQL.UpdDeleteAtPath jsonCols
        "delete the field or element with specified path (for JSON arrays, negative integers count from the end)"
        "delete the field or element with specified path (for JSON arrays, negative integers count from the end)"
    ]
  whenMaybe (not $ null parsers) do
    let allowedOperators = fst <$> parsers
    pure $ fmap (concat . catMaybes) (sequenceA $ snd <$> parsers)
      `P.bindFields` \opExps -> do
        -- there needs to be at least one column in the update
        when (null opExps) $ parseError $
          "at lease one of " <> (T.intercalate ", " allowedOperators) <> " is expected"
        -- no column should appear twice
        let groupedExps   = L.groupBy ((==) `on` fst) $ sortOn (pgiName . fst) opExps
            erroneousExps = concat $ filter ((> 1) . length) groupedExps
        when (not $ null erroneousExps) $ parseError $ "column found in multiple operators; "
          <> (T.intercalate ", " $ flip map erroneousExps \(columnInfo, opExp) ->
                 G.unName (pgiName columnInfo) <> " in " <> RQL.updateOperatorText opExp)
        -- FIXME: validate JSON parsing here?
        pure opExps
  where
    columnParser columnInfo = fmap P.mkParameter <$> P.column (pgiType columnInfo) (G.Nullability $ pgiIsNullable columnInfo)
    textParser   _          = fmap P.mkParameter <$> P.column (PGColumnScalar PGText)    (G.Nullability False)
    intParser    _          = fmap P.mkParameter <$> P.column (PGColumnScalar PGInteger) (G.Nullability False)

    updateOperator
      :: G.Name
      -> G.Name
      -> (PGColumnInfo -> m (Parser 'Both n UnpreparedValue))
      -> (UnpreparedValue -> RQL.UpdOpExpG UnpreparedValue)
      -> [PGColumnInfo]
      -> G.Description
      -> G.Description
      -> m (Maybe (Text, FieldsParser 'Input n (Maybe [(PGColumnInfo, RQL.UpdOpExpG UnpreparedValue)])))
    updateOperator tableName opName mkParser updOpExp columns opDesc objDesc =
      whenMaybe (not $ null columns) do
        fields <- for columns \columnInfo -> do
          let fieldName = pgiName columnInfo
              fieldDesc = pgiDescription columnInfo
          fieldParser <- mkParser columnInfo
          pure $ P.fieldOptional fieldName fieldDesc fieldParser
            `mapField` \value -> (columnInfo, updOpExp value)
        let objName = tableName <> opName <> $$(G.litName "_input")
        pure $ (G.unName opName,)
             $ P.fieldOptional opName (Just opDesc)
             $ P.object objName (Just objDesc)
             $ catMaybes <$> sequenceA fields



-- delete

deleteFromTable
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> DelPermInfo          -- ^ delete permissions of the table
  -> Maybe SelPermInfo    -- ^ select permissions of the table (if any)
  -> Bool
  -> m (FieldsParser 'Output n (Maybe (RQL.AnnDelG UnpreparedValue)))
deleteFromTable table fieldName description deletePerms selectPerms stringifyNum = do
  let whereName = $$(G.litName "where")
      whereDesc = "filter the rows which have to be deleted"
  whereArg  <- P.field whereName (Just whereDesc) <$> boolExp table selectPerms
  selection <- mutationSelectionSet table selectPerms stringifyNum
  columns   <- tableColumns table
  pure $ P.selection fieldName description whereArg selection
    `mapField` (mkDeleteObject table columns deletePerms . third RQL.MOutMultirowFields)

deleteFromTableByPk
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable       -- ^ qualified name of the table
  -> G.Name               -- ^ field display name
  -> Maybe G.Description  -- ^ field description, if any
  -> DelPermInfo          -- ^ delete permissions of the table
  -> SelPermInfo          -- ^ select permissions of the table
  -> Bool
  -> m (Maybe (FieldsParser 'Output n (Maybe (RQL.AnnDelG UnpreparedValue))))
deleteFromTableByPk table fieldName description deletePerms selectPerms stringifyNum = runMaybeT $ do
  columns   <- lift   $ tableSelectColumns table selectPerms
  pkArgs    <- MaybeT $ primaryKeysArguments table selectPerms
  selection <- lift $ tableSelectionSet table selectPerms stringifyNum
  pure $ P.selection fieldName description pkArgs selection
    `mapField` (mkDeleteObject table columns deletePerms . third RQL.MOutSinglerowObject)

mkDeleteObject
  :: QualifiedTable
  -> [PGColumnInfo]
  -> DelPermInfo
  -> (G.Name, AnnBoolExp UnpreparedValue, RQL.MutationOutputG UnpreparedValue)
  -> RQL.AnnDelG UnpreparedValue
mkDeleteObject table columns deletePerms (_, whereExp, mutationOutput) =
  RQL.AnnDel { RQL.dqp1Table   = table
             , RQL.dqp1Where   = (permissionFilter, whereExp)
             , RQL.dqp1Output  = mutationOutput
             , RQL.dqp1AllCols = columns
             }
  where
    permissionFilter = fmapAnnBoolExp partialSQLExpToUnpreparedValue $ dpiFilter deletePerms



-- common

mutationSelectionSet
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> Maybe SelPermInfo
  -> Bool
  -> m (Parser 'Output n (RQL.MutFldsG UnpreparedValue))
mutationSelectionSet table selectPerms stringifyNum = do
  tableName <- qualifiedObjectToName table
  returning <- runMaybeT do
    permissions <- MaybeT $ pure selectPerms
    tableSet    <- lift $ tableSelectionSet table permissions stringifyNum
    let returningName = $$(G.litName "returning")
        returningDesc = "data from the rows affected by the mutation"
    pure $ P.selection_ returningName  (Just returningDesc) tableSet
        `mapField` (FieldName . G.unName *** RQL.MRet)
  let affectedRowsName = $$(G.litName "affected_rows")
      affectedRowsDesc = "number of rows affected by the mutation"
      selectionName    = tableName <> $$(G.litName "_mutation_response")
      selectionDesc    = G.Description $ "response of any mutation on the table \"" <> G.unName tableName <> "\""
      typenameRepr     = (FieldName "__typename", RQL.MExp $ G.unName selectionName)
      dummyIntParser   = undefined :: Parser 'Output n Int32

      selectionFields  = fmap catMaybes $ sequenceA $ catMaybes
        [ Just $ P.selection_ affectedRowsName (Just affectedRowsDesc) dummyIntParser
            `mapField` (FieldName . G.unName *** const RQL.MCount)
        , returning
        ]
  pure $ P.selectionSet selectionName (Just selectionDesc) typenameRepr selectionFields

primaryKeysArguments
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable
  -> SelPermInfo
  -> m (Maybe (FieldsParser 'Input n (AnnBoolExp UnpreparedValue)))
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
