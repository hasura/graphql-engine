-- | Postgres Execute Insert
--
-- Translates and executes IR to Postgres-specific SQL.
--
-- See 'Hasura.Backends.Postgres.Instances.Execute'.
module Hasura.Backends.Postgres.Execute.Insert
  ( convertToSQLTransaction,
    validateInsertInput,
    validateInsertRows,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Extended
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.Execute.Mutation qualified as PGE
import Hasura.Backends.Postgres.SQL.DML qualified as Postgres
import Hasura.Backends.Postgres.SQL.Types as PGTypes hiding (TableName)
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.BoolExp qualified as PGT
import Hasura.Backends.Postgres.Translate.Insert qualified as PGT
import Hasura.Backends.Postgres.Translate.Mutation qualified as PGT
import Hasura.Backends.Postgres.Translate.Returning qualified as PGT
import Hasura.Backends.Postgres.Translate.Select (PostgresTranslateSelect)
import Hasura.Backends.Postgres.Types.Insert
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Returning qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Headers
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Client.Transformable qualified as HTTP

convertToSQLTransaction ::
  forall pgKind m.
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  IR.AnnotatedInsert ('Postgres pgKind) Void Postgres.SQLExp ->
  UserInfo ->
  Seq.Seq PG.PrepArg ->
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  m EncJSON
convertToSQLTransaction (IR.AnnotatedInsert fieldName isSingle annIns mutationOutput _tCase) userInfo planVars stringifyNum tCase =
  if null $ IR._aiInsertObject annIns
    then pure $ IR.buildEmptyMutResp mutationOutput
    else
      withPaths ["selectionSet", fieldName, "args", suffix]
        $ insertMultipleObjects annIns mempty userInfo mutationOutput planVars stringifyNum tCase
  where
    withPaths p x = foldr ($) x $ withPathK <$> p
    suffix = bool "objects" "object" isSingle

insertMultipleObjects ::
  forall pgKind m.
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  IR.MultiObjectInsert ('Postgres pgKind) Postgres.SQLExp ->
  HashMap.HashMap PGCol Postgres.SQLExp ->
  UserInfo ->
  IR.MutationOutput ('Postgres pgKind) ->
  Seq.Seq PG.PrepArg ->
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  m EncJSON
insertMultipleObjects multiObjIns additionalColumns userInfo mutationOutput planVars stringifyNum tCase =
  bool withoutRelsInsert withRelsInsert anyRelsToInsert
  where
    IR.AnnotatedInsertData insObjs table checkCondition columnInfos _pk _extra presetRow (BackendInsert conflictClause) _validateInput = multiObjIns
    allInsObjRels = concatMap IR.getInsertObjectRelationships insObjs
    allInsArrRels = concatMap IR.getInsertArrayRelationships insObjs
    anyRelsToInsert = not $ null allInsArrRels && null allInsObjRels

    withoutRelsInsert = do
      indexedForM_ (IR.getInsertColumns <$> insObjs) \column ->
        validateInsert (map fst column) [] (HashMap.keys additionalColumns)
      let insObjRows = HashMap.fromList . IR.getInsertColumns <$> insObjs
          (columnNames, insertRows) = HashMap.homogenise Postgres.columnDefaultValue $ map ((presetRow <> additionalColumns) <>) insObjRows
          insertQuery =
            IR.InsertQueryP1
              table
              (toList columnNames)
              (map HashMap.elems insertRows)
              conflictClause
              checkCondition
              mutationOutput
              columnInfos
          rowCount = tshow . length $ IR._aiInsertObject multiObjIns
      Tracing.newSpan ("Insert (" <> rowCount <> ") " <> qualifiedObjectToText table) do
        Tracing.attachMetadata [("count", rowCount)]
        PGE.execInsertQuery stringifyNum tCase userInfo (insertQuery, planVars)

    withRelsInsert = do
      insertRequests <- indexedForM insObjs \obj -> do
        let singleObj = IR.AnnotatedInsertData (IR.Single obj) table checkCondition columnInfos _pk _extra presetRow (BackendInsert conflictClause) _validateInput
        insertObject singleObj additionalColumns userInfo planVars stringifyNum tCase
      let affectedRows = sum $ map fst insertRequests
          columnValues = mapMaybe snd insertRequests
      selectExpr <- PGT.mkSelectExpFromColumnValues table columnInfos columnValues
      PGE.executeMutationOutputQuery
        userInfo
        table
        columnInfos
        (Just affectedRows)
        (PGT.MCSelectValues selectExpr)
        mutationOutput
        stringifyNum
        tCase
        []

insertObject ::
  forall pgKind m.
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  IR.SingleObjectInsert ('Postgres pgKind) Postgres.SQLExp ->
  HashMap PGCol Postgres.SQLExp ->
  UserInfo ->
  Seq.Seq PG.PrepArg ->
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  m (Int, Maybe (ColumnValues ('Postgres pgKind) TxtEncodedVal))
insertObject singleObjIns additionalColumns userInfo planVars stringifyNum tCase =
  Tracing.newSpan ("Insert " <> qualifiedObjectToText table) do
    validateInsert (HashMap.keys columns) (map IR._riRelationInfo objectRels) (HashMap.keys additionalColumns)

    -- insert all object relations and fetch this insert dependent column values
    objInsRes <- forM beforeInsert $ insertObjRel planVars userInfo stringifyNum tCase

    -- prepare final insert columns
    let objRelAffRows = sum $ map fst objInsRes
        objRelDeterminedCols = HashMap.fromList $ concatMap snd objInsRes
        finalInsCols = presetValues <> columns <> objRelDeterminedCols <> additionalColumns

    cte <- mkInsertQ userInfo table onConflict finalInsCols checkCond

    PGE.MutateResp affRows colVals <-
      liftTx
        $ PGE.mutateAndFetchCols @pgKind userInfo table allColumns (PGT.MCCheckConstraint cte, planVars) stringifyNum tCase
    colValM <- asSingleObject colVals

    arrRelAffRows <- bool (withArrRels colValM) (return 0) $ null allAfterInsertRels
    let totAffRows = objRelAffRows + affRows + arrRelAffRows

    return (totAffRows, colValM)
  where
    IR.AnnotatedInsertData (IR.Single annObj) table checkCond allColumns _pk _extra presetValues (BackendInsert onConflict) _validateInput = singleObjIns
    columns = HashMap.fromList $ IR.getInsertColumns annObj
    objectRels = IR.getInsertObjectRelationships annObj
    arrayRels = IR.getInsertArrayRelationships annObj

    afterInsert, beforeInsert :: [IR.ObjectRelationInsert ('Postgres pgKind) Postgres.SQLExp]
    (afterInsert, beforeInsert) =
      L.partition ((== AfterParent) . riInsertOrder . IR._riRelationInfo) objectRels

    allAfterInsertRels :: [IR.ArrayRelationInsert ('Postgres pgKind) Postgres.SQLExp]
    allAfterInsertRels = arrayRels <> map objToArr afterInsert

    afterInsertDepCols :: [ColumnInfo ('Postgres pgKind)]
    afterInsertDepCols =
      flip (getColInfos @('Postgres pgKind)) allColumns
        $ concatMap (HashMap.keys . unRelMapping . riMapping . IR._riRelationInfo) allAfterInsertRels

    withArrRels ::
      Maybe (ColumnValues ('Postgres pgKind) TxtEncodedVal) ->
      m Int
    withArrRels colValM = do
      colVal <- onNothing colValM $ throw400 NotSupported cannotInsArrRelErr
      afterInsertDepColsWithVal <- fetchFromColVals colVal afterInsertDepCols
      arrInsARows <-
        forM allAfterInsertRels
          $ insertArrRel afterInsertDepColsWithVal userInfo planVars stringifyNum tCase
      return $ sum arrInsARows

    asSingleObject ::
      [ColumnValues ('Postgres pgKind) TxtEncodedVal] ->
      m (Maybe (ColumnValues ('Postgres pgKind) TxtEncodedVal))
    asSingleObject = \case
      [] -> pure Nothing
      [r] -> pure $ Just r
      _ -> throw500 "more than one row returned"

    cannotInsArrRelErr :: Text
    cannotInsArrRelErr =
      "cannot proceed to insert array relations since insert to table "
        <> table
        <<> " affects zero rows"

objToArr :: forall a b. IR.ObjectRelationInsert b a -> IR.ArrayRelationInsert b a
objToArr IR.RelationInsert {..} = IR.RelationInsert (singleToMulti _riInsertData) _riRelationInfo
  where
    singleToMulti :: IR.SingleObjectInsert b a -> IR.MultiObjectInsert b a
    singleToMulti annIns = annIns {IR._aiInsertObject = [IR.unSingle $ IR._aiInsertObject annIns]}

insertObjRel ::
  forall pgKind m.
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  Seq.Seq PG.PrepArg ->
  UserInfo ->
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  IR.ObjectRelationInsert ('Postgres pgKind) Postgres.SQLExp ->
  m (Int, [(PGCol, Postgres.SQLExp)])
insertObjRel planVars userInfo stringifyNum tCase objRelIns =
  withPathK (relNameToTxt relName) $ do
    (affRows, colValM) <- withPathK "data" $ insertObject singleObjIns mempty userInfo planVars stringifyNum tCase
    colVal <- onNothing colValM $ throw400 NotSupported errMsg
    retColsWithVals <- fetchFromColVals colVal rColInfos
    let columns = flip mapMaybe (HashMap.toList mapCols) \(column, target) -> do
          value <- lookup target retColsWithVals
          Just (column, value)
    pure (affRows, columns)
  where
    IR.RelationInsert singleObjIns relInfo = objRelIns
    relName = riName relInfo
    table = case riTarget relInfo of
      RelTargetNativeQuery _ -> error "insertObjRel RelTargetNativeQuery"
      RelTargetTable tn -> tn
    mapCols = unRelMapping $ riMapping relInfo
    allCols = IR._aiTableColumns singleObjIns
    rCols = HashMap.elems mapCols
    rColInfos = getColInfos rCols allCols
    errMsg =
      "cannot proceed to insert object relation "
        <> relName
        <<> " since insert to table "
        <> table
        <<> " affects zero rows"

insertArrRel ::
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresTranslateSelect pgKind,
    MonadReader QueryTagsComment m
  ) =>
  [(PGCol, Postgres.SQLExp)] ->
  UserInfo ->
  Seq.Seq PG.PrepArg ->
  Options.StringifyNumbers ->
  Maybe NamingCase ->
  IR.ArrayRelationInsert ('Postgres pgKind) Postgres.SQLExp ->
  m Int
insertArrRel resCols userInfo planVars stringifyNum tCase arrRelIns =
  withPathK (relNameToTxt $ riName relInfo) $ do
    let additionalColumns = HashMap.fromList
          $ flip mapMaybe resCols \(column, value) -> do
            target <- HashMap.lookup column mapping
            Just (target, value)
    resBS <-
      withPathK "data"
        $ insertMultipleObjects multiObjIns additionalColumns userInfo mutOutput planVars stringifyNum tCase
    resObj <- decodeEncJSON resBS
    onNothing (HashMap.lookup ("affected_rows" :: Text) resObj)
      $ throw500 "affected_rows not returned in array rel insert"
  where
    IR.RelationInsert multiObjIns relInfo = arrRelIns
    mapping = unRelMapping $ riMapping relInfo
    mutOutput = IR.MOutMultirowFields [("affected_rows", IR.MCount)]

-- | Validate an insert object based on insert columns,
-- insert object relations and additional columns from parent:
--
-- * There should be no overlap between 'insCols' and 'addCols'.
-- * There should be no overlap between any object relationship columns and
--   'insCols' and 'addCols'.
validateInsert ::
  (MonadError QErr m) =>
  -- | inserting columns
  [PGCol] ->
  -- | object relation inserts
  [RelInfo ('Postgres pgKind)] ->
  -- | additional fields from parent
  [PGCol] ->
  m ()
validateInsert insCols objRels addCols = do
  -- validate insertCols
  unless (null insConflictCols)
    $ throw400 ValidationFailed
    $ "cannot insert "
    <> showPGCols insConflictCols
    <> " columns as their values are already being determined by parent insert"

  forM_ objRels $ \relInfo -> do
    let lCols = HashMap.keys $ unRelMapping $ riMapping relInfo
        relName = riName relInfo
        relNameTxt = relNameToTxt relName
        lColConflicts = lCols `intersect` (addCols <> insCols)
        is_after_parent
          | AfterParent <- riInsertOrder relInfo = True
          | otherwise = False

    withPathK relNameTxt
      -- When inserting through relationships, we only care that inserted
      -- columns don't overlap with those defining the relationship when the
      -- remote table is inserted _before_ the parent table.
      -- When the remote table is inserted _after_ the parent table it's the
      -- parent table that (through some means) decide what the value of the
      -- key is.
      $ unless (null lColConflicts || is_after_parent)
      $ throw400 ValidationFailed
      $ "cannot insert object relationship "
      <> relName
      <<> " as "
      <> showPGCols lColConflicts
      <> " column values are already determined"
  where
    insConflictCols = insCols `intersect` addCols

mkInsertQ ::
  (Backend ('Postgres pgKind), MonadIO m, MonadError QErr m) =>
  UserInfo ->
  QualifiedTable ->
  Maybe (IR.OnConflictClause ('Postgres pgKind) Postgres.SQLExp) ->
  HashMap.HashMap PGCol Postgres.SQLExp ->
  (AnnBoolExpSQL ('Postgres pgKind), Maybe (AnnBoolExpSQL ('Postgres pgKind))) ->
  m Postgres.TopLevelCTE
mkInsertQ userInfo table onConflictM insertRow (insCheck, updCheck) = do
  insCheckBoolExp <- PGT.toSQLBoolExp userInfo (Postgres.QualTable table) insCheck
  updateCheckBoolExp <- traverse (PGT.toSQLBoolExp userInfo (Postgres.QualTable table)) updCheck
  sqlConflict <- traverse (PGT.toSQLConflict userInfo table) onConflictM
  let sqlExps = HashMap.elems insertRow
      valueExp = Postgres.ValuesExp [Postgres.TupleExp sqlExps]
      tableCols = HashMap.keys insertRow
      sqlInsert =
        Postgres.SQLInsert table tableCols valueExp sqlConflict
          . Just
          $ Postgres.RetExp
            [ Postgres.selectStar,
              PGT.insertOrUpdateCheckExpr
                table
                onConflictM
                insCheckBoolExp
                updateCheckBoolExp
            ]
  pure $ Postgres.CTEInsert sqlInsert

fetchFromColVals ::
  (MonadError QErr m) =>
  ColumnValues ('Postgres pgKind) TxtEncodedVal ->
  [ColumnInfo ('Postgres pgKind)] ->
  m [(PGCol, Postgres.SQLExp)]
fetchFromColVals colVal reqCols =
  forM reqCols $ \ci -> do
    let valM = HashMap.lookup (ciColumn ci) colVal
    val <-
      onNothing valM
        $ throw500
        $ "column "
        <> ciColumn ci
        <<> " not found in given colVal"
    let pgColVal = case val of
          TENull -> Postgres.SENull
          TELit t -> Postgres.SELit t
    return (ciColumn ci, pgColVal)

decodeEncJSON :: (J.FromJSON a, QErrM m) => EncJSON -> m a
decodeEncJSON =
  either (throw500 . T.pack) decodeValue
    . J.eitherDecode
    . encJToLBS

validateInsertInput ::
  forall m pgKind.
  ( MonadError QErr m,
    MonadIO m,
    Tracing.MonadTrace m,
    MonadState (PGE.InsertValidationPayloadMap pgKind) m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  IR.MultiObjectInsert ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind)) ->
  [HTTP.Header] ->
  m ()
validateInsertInput env manager logger userInfo IR.AnnotatedInsertData {..} reqHeaders = do
  for_ _aiValidateInput $ \validateInput -> do
    validatePaylodMap <- get
    case InsOrdHashMap.lookup _aiTableName validatePaylodMap of
      Nothing -> modify $ InsOrdHashMap.insert _aiTableName (_aiInsertObject, validateInput)
      Just _ -> modify $ InsOrdHashMap.adjust (\(insertedRows, validateInputDef) -> (insertedRows <> _aiInsertObject, validateInputDef)) _aiTableName

  -- Validate the nested insert data
  -- for each row
  for_ _aiInsertObject $ \row ->
    -- for each field
    for_ row $ \case
      IR.AIColumn {} -> pure () -- ignore columns
      IR.AIObjectRelationship _ objectRelationInsert -> do
        let multiObjectInsert = IR._riInsertData $ objToArr objectRelationInsert
        validateInsertInput env manager logger userInfo multiObjectInsert reqHeaders
      IR.AIArrayRelationship _ arrayRelationInsert ->
        validateInsertInput env manager logger userInfo (IR._riInsertData arrayRelationInsert) reqHeaders

validateInsertRows ::
  forall m pgKind.
  ( MonadError QErr m,
    MonadIO m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  L.Logger L.Hasura ->
  UserInfo ->
  ResolvedWebhook ->
  [HeaderConf] ->
  Timeout ->
  Bool ->
  [HTTP.Header] ->
  [IR.AnnotatedInsertRow ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind))] ->
  m ()
validateInsertRows env manager logger userInfo resolvedWebHook confHeaders timeout forwardClientHeaders reqHeaders rows = do
  let inputData = J.object ["input" J..= map convertInsertRow rows]
  PGE.validateMutation env manager logger userInfo resolvedWebHook confHeaders timeout forwardClientHeaders reqHeaders inputData
  where
    convertInsertRow :: IR.AnnotatedInsertRow ('Postgres pgKind) (IR.UnpreparedValue ('Postgres pgKind)) -> J.Value
    convertInsertRow fields = J.object $ flip mapMaybe fields $ \field ->
      let (fieldName, maybeFieldValue) = case field of
            IR.AIColumn (column, value) -> (toTxt column, convertValue value)
            IR.AIObjectRelationship _ objectRelationInsert ->
              let relationshipName = riName $ IR._riRelationInfo objectRelationInsert
                  insertRow = IR.unSingle $ IR._aiInsertObject $ IR._riInsertData objectRelationInsert
               in -- We want to follow the GQL types. And since nested objects (object and array relationships)
                  -- are always included inside 'data', we add the "data" key here.
                  (toTxt relationshipName, Just $ J.object ["data" J..= convertInsertRow insertRow])
            IR.AIArrayRelationship _ arrayRelationInsert ->
              let relationshipName = riName $ IR._riRelationInfo arrayRelationInsert
                  insertRows = IR._aiInsertObject $ IR._riInsertData arrayRelationInsert
               in (toTxt relationshipName, Just $ J.object ["data" J..= map convertInsertRow insertRows])
       in maybeFieldValue <&> \fieldValue -> (J.fromText fieldName J..= fieldValue)

    convertValue :: IR.UnpreparedValue ('Postgres pgKind) -> Maybe J.Value
    convertValue = \case
      IR.UVParameter _ columnValue -> Just $ pgScalarValueToJson $ cvValue columnValue
      IR.UVLiteral sqlExp -> Just $ J.toJSON sqlExp
      IR.UVSession -> Nothing
      IR.UVSessionVar {} -> Nothing
