-- | Postgres Execute Insert
--
-- Translates and executes IR to Postgres-specific SQL.
--
-- See 'Hasura.Backends.Postgres.Instances.Execute'.
module Hasura.Backends.Postgres.Execute.Insert
  ( convertToSQLTransaction,
  )
where

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as Map
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Extended
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.Execute.Mutation qualified as PGE
import Hasura.Backends.Postgres.SQL.DML qualified as PG
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.BoolExp qualified as PGT
import Hasura.Backends.Postgres.Translate.Insert qualified as PGT
import Hasura.Backends.Postgres.Translate.Mutation qualified as PGT
import Hasura.Backends.Postgres.Translate.Returning qualified as PGT
import Hasura.Backends.Postgres.Translate.Select (PostgresAnnotatedFieldJSON)
import Hasura.Backends.Postgres.Types.Insert
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Returning qualified as IR
import Hasura.RQL.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing

convertToSQLTransaction ::
  forall pgKind m.
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  IR.AnnInsert ('Postgres pgKind) Void PG.SQLExp ->
  UserInfo ->
  Seq.Seq Q.PrepArg ->
  Bool ->
  m EncJSON
convertToSQLTransaction (IR.AnnInsert fieldName isSingle annIns mutationOutput) userInfo planVars stringifyNum =
  if null $ IR._aiInsObj annIns
    then pure $ IR.buildEmptyMutResp mutationOutput
    else
      withPaths ["selectionSet", fieldName, "args", suffix] $
        insertMultipleObjects annIns [] userInfo mutationOutput planVars stringifyNum
  where
    withPaths p x = foldr ($) x $ withPathK <$> p
    suffix = bool "objects" "object" isSingle

insertMultipleObjects ::
  forall pgKind m.
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  IR.MultiObjIns ('Postgres pgKind) PG.SQLExp ->
  [(PGCol, PG.SQLExp)] ->
  UserInfo ->
  IR.MutationOutput ('Postgres pgKind) ->
  Seq.Seq Q.PrepArg ->
  Bool ->
  m EncJSON
insertMultipleObjects multiObjIns additionalColumns userInfo mutationOutput planVars stringifyNum =
  bool withoutRelsInsert withRelsInsert anyRelsToInsert
  where
    IR.AnnIns insObjs table checkCondition columnInfos defVals (BackendInsert conflictClause) = multiObjIns
    allInsObjRels = concatMap IR.getInsertObjectRelationships insObjs
    allInsArrRels = concatMap IR.getInsertArrayRelationships insObjs
    anyRelsToInsert = not $ null allInsArrRels && null allInsObjRels

    withoutRelsInsert = do
      indexedForM_ (IR.getInsertColumns <$> insObjs) \column ->
        validateInsert (map fst column) [] (map fst additionalColumns)
      let columnValues = map (mkSQLRow defVals) $ union additionalColumns . IR.getInsertColumns <$> insObjs
          columnNames = Map.keys defVals
          insertQuery =
            IR.InsertQueryP1
              table
              columnNames
              columnValues
              conflictClause
              checkCondition
              mutationOutput
              columnInfos
          rowCount = tshow . length $ IR._aiInsObj multiObjIns
      Tracing.trace ("Insert (" <> rowCount <> ") " <> qualifiedObjectToText table) do
        Tracing.attachMetadata [("count", rowCount)]
        PGE.execInsertQuery stringifyNum userInfo (insertQuery, planVars)

    withRelsInsert = do
      insertRequests <- indexedForM insObjs \obj -> do
        let singleObj = IR.AnnIns (IR.Single obj) table checkCondition columnInfos defVals (BackendInsert conflictClause)
        insertObject singleObj additionalColumns userInfo planVars stringifyNum
      let affectedRows = sum $ map fst insertRequests
          columnValues = mapMaybe snd insertRequests
      selectExpr <- PGT.mkSelectExpFromColumnValues table columnInfos columnValues
      PGE.executeMutationOutputQuery
        table
        columnInfos
        (Just affectedRows)
        (PGT.MCSelectValues selectExpr)
        mutationOutput
        stringifyNum
        []

insertObject ::
  forall pgKind m.
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  IR.SingleObjIns ('Postgres pgKind) PG.SQLExp ->
  [(PGCol, PG.SQLExp)] ->
  UserInfo ->
  Seq.Seq Q.PrepArg ->
  Bool ->
  m (Int, Maybe (ColumnValues ('Postgres pgKind) TxtEncodedVal))
insertObject singleObjIns additionalColumns userInfo planVars stringifyNum = Tracing.trace ("Insert " <> qualifiedObjectToText table) do
  validateInsert (map fst columns) (map IR._riRelInfo objectRels) (map fst additionalColumns)

  -- insert all object relations and fetch this insert dependent column values
  objInsRes <- forM beforeInsert $ insertObjRel planVars userInfo stringifyNum

  -- prepare final insert columns
  let objRelAffRows = sum $ map fst objInsRes
      objRelDeterminedCols = concatMap snd objInsRes
      finalInsCols = columns <> objRelDeterminedCols <> additionalColumns

  cte <- mkInsertQ table onConflict finalInsCols defaultValues checkCond

  PGE.MutateResp affRows colVals <-
    liftTx $
      PGE.mutateAndFetchCols @pgKind table allColumns (PGT.MCCheckConstraint cte, planVars) stringifyNum
  colValM <- asSingleObject colVals

  arrRelAffRows <- bool (withArrRels colValM) (return 0) $ null allAfterInsertRels
  let totAffRows = objRelAffRows + affRows + arrRelAffRows

  return (totAffRows, colValM)
  where
    IR.AnnIns (IR.Single annObj) table checkCond allColumns defaultValues (BackendInsert onConflict) = singleObjIns
    columns = IR.getInsertColumns annObj
    objectRels = IR.getInsertObjectRelationships annObj
    arrayRels = IR.getInsertArrayRelationships annObj

    afterInsert, beforeInsert :: [IR.ObjRelIns ('Postgres pgKind) PG.SQLExp]
    (afterInsert, beforeInsert) =
      L.partition ((== AfterParent) . riInsertOrder . IR._riRelInfo) objectRels

    allAfterInsertRels :: [IR.ArrRelIns ('Postgres pgKind) PG.SQLExp]
    allAfterInsertRels = arrayRels <> map objToArr afterInsert

    afterInsertDepCols :: [ColumnInfo ('Postgres pgKind)]
    afterInsertDepCols =
      flip (getColInfos @('Postgres pgKind)) allColumns $
        concatMap (Map.keys . riMapping . IR._riRelInfo) allAfterInsertRels

    objToArr :: forall a b. IR.ObjRelIns b a -> IR.ArrRelIns b a
    objToArr IR.RelIns {..} = IR.RelIns (singleToMulti _riAnnIns) _riRelInfo

    singleToMulti :: forall a b. IR.SingleObjIns b a -> IR.MultiObjIns b a
    singleToMulti annIns = annIns {IR._aiInsObj = [IR.unSingle $ IR._aiInsObj annIns]}

    withArrRels ::
      Maybe (ColumnValues ('Postgres pgKind) TxtEncodedVal) ->
      m Int
    withArrRels colValM = do
      colVal <- onNothing colValM $ throw400 NotSupported cannotInsArrRelErr
      afterInsertDepColsWithVal <- fetchFromColVals colVal afterInsertDepCols
      arrInsARows <-
        forM allAfterInsertRels $
          insertArrRel afterInsertDepColsWithVal userInfo planVars stringifyNum
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
        <> table <<> " affects zero rows"

insertObjRel ::
  forall pgKind m.
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  Seq.Seq Q.PrepArg ->
  UserInfo ->
  Bool ->
  IR.ObjRelIns ('Postgres pgKind) PG.SQLExp ->
  m (Int, [(PGCol, PG.SQLExp)])
insertObjRel planVars userInfo stringifyNum objRelIns =
  withPathK (relNameToTxt relName) $ do
    (affRows, colValM) <- withPathK "data" $ insertObject singleObjIns [] userInfo planVars stringifyNum
    colVal <- onNothing colValM $ throw400 NotSupported errMsg
    retColsWithVals <- fetchFromColVals colVal rColInfos
    let columns = flip mapMaybe (Map.toList mapCols) \(column, target) -> do
          value <- lookup target retColsWithVals
          Just (column, value)
    pure (affRows, columns)
  where
    IR.RelIns singleObjIns relInfo = objRelIns
    relName = riName relInfo
    table = riRTable relInfo
    mapCols = riMapping relInfo
    allCols = IR._aiTableCols singleObjIns
    rCols = Map.elems mapCols
    rColInfos = getColInfos rCols allCols
    errMsg =
      "cannot proceed to insert object relation "
        <> relName <<> " since insert to table "
        <> table <<> " affects zero rows"

insertArrRel ::
  ( MonadTx m,
    MonadIO m,
    Tracing.MonadTrace m,
    Backend ('Postgres pgKind),
    PostgresAnnotatedFieldJSON pgKind,
    MonadReader QueryTagsComment m
  ) =>
  [(PGCol, PG.SQLExp)] ->
  UserInfo ->
  Seq.Seq Q.PrepArg ->
  Bool ->
  IR.ArrRelIns ('Postgres pgKind) PG.SQLExp ->
  m Int
insertArrRel resCols userInfo planVars stringifyNum arrRelIns =
  withPathK (relNameToTxt $ riName relInfo) $ do
    let additionalColumns = flip mapMaybe resCols \(column, value) -> do
          target <- Map.lookup column mapping
          Just (target, value)
    resBS <-
      withPathK "data" $
        insertMultipleObjects multiObjIns additionalColumns userInfo mutOutput planVars stringifyNum
    resObj <- decodeEncJSON resBS
    onNothing (Map.lookup ("affected_rows" :: Text) resObj) $
      throw500 "affected_rows not returned in array rel insert"
  where
    IR.RelIns multiObjIns relInfo = arrRelIns
    mapping = riMapping relInfo
    mutOutput = IR.MOutMultirowFields [("affected_rows", IR.MCount)]

-- | validate an insert object based on insert columns,
-- | insert object relations and additional columns from parent
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
  unless (null insConflictCols) $
    throw400 ValidationFailed $
      "cannot insert " <> showPGCols insConflictCols
        <> " columns as their values are already being determined by parent insert"

  forM_ objRels $ \relInfo -> do
    let lCols = Map.keys $ riMapping relInfo
        relName = riName relInfo
        relNameTxt = relNameToTxt relName
        lColConflicts = lCols `intersect` (addCols <> insCols)
    withPathK relNameTxt $
      unless (null lColConflicts) $
        throw400 ValidationFailed $
          "cannot insert object relationship " <> relName
            <<> " as "
            <> showPGCols lColConflicts
            <> " column values are already determined"
  where
    insConflictCols = insCols `intersect` addCols

mkInsertQ ::
  (MonadError QErr m, Backend ('Postgres pgKind)) =>
  QualifiedTable ->
  Maybe (IR.OnConflictClause ('Postgres pgKind) PG.SQLExp) ->
  [(PGCol, PG.SQLExp)] ->
  Map.HashMap PGCol PG.SQLExp ->
  (AnnBoolExpSQL ('Postgres pgKind), Maybe (AnnBoolExpSQL ('Postgres pgKind))) ->
  m PG.CTE
mkInsertQ table onConflictM insCols defVals (insCheck, updCheck) = do
  let sqlConflict = PGT.toSQLConflict table <$> onConflictM
      sqlExps = mkSQLRow defVals insCols
      valueExp = PG.ValuesExp [PG.TupleExp sqlExps]
      tableCols = Map.keys defVals
      sqlInsert =
        PG.SQLInsert table tableCols valueExp sqlConflict
          . Just
          $ PG.RetExp
            [ PG.selectStar,
              PGT.insertOrUpdateCheckExpr
                table
                onConflictM
                (PGT.toSQLBoolExp (PG.QualTable table) insCheck)
                (fmap (PGT.toSQLBoolExp (PG.QualTable table)) updCheck)
            ]
  pure $ PG.CTEInsert sqlInsert

fetchFromColVals ::
  MonadError QErr m =>
  ColumnValues ('Postgres pgKind) TxtEncodedVal ->
  [ColumnInfo ('Postgres pgKind)] ->
  m [(PGCol, PG.SQLExp)]
fetchFromColVals colVal reqCols =
  forM reqCols $ \ci -> do
    let valM = Map.lookup (ciColumn ci) colVal
    val <-
      onNothing valM $
        throw500 $
          "column "
            <> ciColumn ci <<> " not found in given colVal"
    let pgColVal = case val of
          TENull -> PG.SENull
          TELit t -> PG.SELit t
    return (ciColumn ci, pgColVal)

mkSQLRow :: Map.HashMap PGCol PG.SQLExp -> [(PGCol, PG.SQLExp)] -> [PG.SQLExp]
mkSQLRow defVals withPGCol = map snd $
  flip map (Map.toList defVals) $
    \(col, defVal) -> (col,) $ fromMaybe defVal $ Map.lookup col withPGColMap
  where
    withPGColMap = Map.fromList withPGCol

decodeEncJSON :: (J.FromJSON a, QErrM m) => EncJSON -> m a
decodeEncJSON =
  either (throw500 . T.pack) decodeValue
    . J.eitherDecode
    . encJToLBS
