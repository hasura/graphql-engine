module Hasura.GraphQL.Execute.Insert
  ( traverseAnnInsert
  , convertToSQLTransaction
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                   as J
import qualified Data.Environment                             as Env
import qualified Data.HashMap.Strict                          as Map
import qualified Data.List                                    as L
import qualified Data.Sequence                                as Seq
import qualified Data.Text                                    as T
import qualified Database.PG.Query                            as Q

import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.Execute.Mutation    as PGE
import qualified Hasura.Backends.Postgres.Execute.RemoteJoin  as PGE
import qualified Hasura.Backends.Postgres.SQL.DML             as PG
import qualified Hasura.Backends.Postgres.Translate.BoolExp   as PGT
import qualified Hasura.Backends.Postgres.Translate.Insert    as PGT
import qualified Hasura.Backends.Postgres.Translate.Mutation  as PGT
import qualified Hasura.Backends.Postgres.Translate.Returning as PGT
import qualified Hasura.RQL.IR.Insert                         as IR
import qualified Hasura.RQL.IR.Returning                      as IR
import qualified Hasura.Tracing                               as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.EncJSON
import           Hasura.RQL.Types
import           Hasura.Server.Version                        (HasVersion)


traverseAnnInsert
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> IR.AnnInsert backend a
  -> f (IR.AnnInsert backend b)
traverseAnnInsert f (IR.AnnInsert fieldName isSingle (annIns, mutationOutput)) =
  IR.AnnInsert fieldName isSingle
  <$> ( (,)
        <$> traverseMulti annIns
        <*> IR.traverseMutationOutput f mutationOutput
      )
  where
    traverseMulti (IR.AnnIns objs tableName conflictClause checkCond columns defaultValues) = IR.AnnIns
      <$> traverse traverseObject objs
      <*> pure tableName
      <*> traverse (traverse f) conflictClause
      <*> ( (,)
            <$> traverseAnnBoolExp f (fst checkCond)
            <*> traverse (traverseAnnBoolExp f) (snd checkCond)
          )
      <*> pure columns
      <*> traverse f defaultValues
    traverseSingle (IR.AnnIns obj tableName conflictClause checkCond columns defaultValues) = IR.AnnIns
      <$> traverseObject obj
      <*> pure tableName
      <*> traverse (traverse f) conflictClause
      <*> ( (,)
            <$> traverseAnnBoolExp f (fst checkCond)
            <*> traverse (traverseAnnBoolExp f) (snd checkCond)
          )
      <*> pure columns
      <*> traverse f defaultValues
    traverseObject (IR.AnnInsObj columns objRels arrRels) = IR.AnnInsObj
      <$> traverse (traverse f) columns
      <*> traverse (traverseRel traverseSingle) objRels
      <*> traverse (traverseRel traverseMulti)  arrRels
    traverseRel z (IR.RelIns object relInfo) = IR.RelIns <$> z object <*> pure relInfo


convertToSQLTransaction
  :: (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> IR.AnnInsert 'Postgres PG.SQLExp
  -> PGE.MutationRemoteJoinCtx
  -> Seq.Seq Q.PrepArg
  -> Bool
  -> m EncJSON
convertToSQLTransaction env (IR.AnnInsert fieldName isSingle (annIns, mutationOutput)) remoteJoinCtx planVars stringifyNum =
  if null $ IR._aiInsObj annIns
  then pure $ IR.buildEmptyMutResp mutationOutput
  else withPaths ["selectionSet", fieldName, "args", suffix] $
    insertMultipleObjects env annIns [] remoteJoinCtx mutationOutput planVars stringifyNum
  where
    withPaths p x = foldr ($) x $ withPathK <$> p
    suffix = bool "objects" "object" isSingle

insertMultipleObjects
  :: (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> IR.MultiObjIns 'Postgres PG.SQLExp
  -> [(PGCol, PG.SQLExp)]
  -> PGE.MutationRemoteJoinCtx
  -> IR.MutationOutput 'Postgres
  -> Seq.Seq Q.PrepArg
  -> Bool
  -> m EncJSON
insertMultipleObjects env multiObjIns additionalColumns remoteJoinCtx mutationOutput planVars stringifyNum =
    bool withoutRelsInsert withRelsInsert anyRelsToInsert
  where
    IR.AnnIns insObjs table conflictClause checkCondition columnInfos defVals = multiObjIns
    allInsObjRels = concatMap IR._aioObjRels insObjs
    allInsArrRels = concatMap IR._aioArrRels insObjs
    anyRelsToInsert = not $ null allInsArrRels && null allInsObjRels

    withoutRelsInsert = do
      indexedForM_ (IR._aioColumns <$> insObjs) \column ->
        validateInsert (map fst column) [] (map fst additionalColumns)
      let columnValues = map (mkSQLRow defVals) $ union additionalColumns . IR._aioColumns <$> insObjs
          columnNames  = Map.keys defVals
          insertQuery  = IR.InsertQueryP1
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
        PGE.execInsertQuery env stringifyNum (Just remoteJoinCtx) (insertQuery, planVars)

    withRelsInsert = do
      insertRequests <- indexedForM insObjs \obj -> do
        let singleObj = IR.AnnIns obj table conflictClause checkCondition columnInfos defVals
        insertObject env singleObj additionalColumns remoteJoinCtx planVars stringifyNum
      let affectedRows = sum $ map fst insertRequests
          columnValues = mapMaybe snd insertRequests
      selectExpr <- PGT.mkSelectExpFromColumnValues table columnInfos columnValues
      let (mutOutputRJ, remoteJoins) = PGE.getRemoteJoinsMutationOutput mutationOutput
      PGE.executeMutationOutputQuery env table columnInfos (Just affectedRows) (PGT.MCSelectValues selectExpr)
        mutOutputRJ stringifyNum [] $ (, remoteJoinCtx) <$> remoteJoins

insertObject
  :: forall m
   . (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> IR.SingleObjIns 'Postgres PG.SQLExp
  -> [(PGCol, PG.SQLExp)]
  -> PGE.MutationRemoteJoinCtx
  -> Seq.Seq Q.PrepArg
  -> Bool
  -> m (Int, Maybe (ColumnValues 'Postgres TxtEncodedVal))
insertObject env singleObjIns additionalColumns remoteJoinCtx planVars stringifyNum = Tracing.trace ("Insert " <> qualifiedObjectToText table) do
  validateInsert (map fst columns) (map IR._riRelInfo objectRels) (map fst additionalColumns)

  -- insert all object relations and fetch this insert dependent column values
  objInsRes <- forM beforeInsert $ insertObjRel env planVars remoteJoinCtx stringifyNum

  -- prepare final insert columns
  let objRelAffRows = sum $ map fst objInsRes
      objRelDeterminedCols = concatMap snd objInsRes
      finalInsCols = columns <> objRelDeterminedCols <> additionalColumns

  cte <- mkInsertQ table onConflict finalInsCols defaultValues checkCond

  PGE.MutateResp affRows colVals <- liftTx $
    PGE.mutateAndFetchCols table allColumns (PGT.MCCheckConstraint cte, planVars) stringifyNum
  colValM <- asSingleObject colVals

  arrRelAffRows <- bool (withArrRels colValM) (return 0) $ null allAfterInsertRels
  let totAffRows = objRelAffRows + affRows + arrRelAffRows

  return (totAffRows, colValM)
  where
    IR.AnnIns annObj table onConflict checkCond allColumns defaultValues = singleObjIns
    IR.AnnInsObj columns objectRels arrayRels = annObj

    afterInsert, beforeInsert :: [IR.ObjRelIns 'Postgres PG.SQLExp]
    (afterInsert, beforeInsert) =
      L.partition ((== AfterParent) . riInsertOrder . IR._riRelInfo) objectRels

    allAfterInsertRels :: [IR.ArrRelIns 'Postgres PG.SQLExp]
    allAfterInsertRels = arrayRels <> map objToArr afterInsert

    afterInsertDepCols :: [ColumnInfo 'Postgres]
    afterInsertDepCols = flip getColInfos allColumns $
      concatMap (Map.keys . riMapping . IR._riRelInfo) allAfterInsertRels

    objToArr :: forall a b. IR.ObjRelIns b a -> IR.ArrRelIns b a
    objToArr IR.RelIns {..} = IR.RelIns (singleToMulti _riAnnIns) _riRelInfo

    singleToMulti :: forall a b. IR.SingleObjIns b a -> IR.MultiObjIns b a
    singleToMulti IR.AnnIns {..} =
      IR.AnnIns
        [_aiInsObj]
        _aiTableName
        _aiConflictClause
        _aiCheckCond
        _aiTableCols
        _aiDefVals

    withArrRels
      :: Maybe (ColumnValues 'Postgres TxtEncodedVal)
      -> m Int
    withArrRels colValM = do
      colVal <- onNothing colValM $ throw400 NotSupported cannotInsArrRelErr
      afterInsertDepColsWithVal <- fetchFromColVals colVal afterInsertDepCols
      arrInsARows <- forM allAfterInsertRels
        $ insertArrRel env afterInsertDepColsWithVal remoteJoinCtx planVars stringifyNum
      return $ sum arrInsARows

    asSingleObject
      :: [ColumnValues 'Postgres TxtEncodedVal]
      -> m (Maybe (ColumnValues 'Postgres TxtEncodedVal))
    asSingleObject = \case
      []  -> pure Nothing
      [r] -> pure $ Just r
      _   -> throw500 "more than one row returned"

    cannotInsArrRelErr :: Text
    cannotInsArrRelErr =
      "cannot proceed to insert array relations since insert to table "
      <> table <<> " affects zero rows"

insertObjRel
  :: (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> Seq.Seq Q.PrepArg
  -> PGE.MutationRemoteJoinCtx
  -> Bool
  -> IR.ObjRelIns 'Postgres PG.SQLExp
  -> m (Int, [(PGCol, PG.SQLExp)])
insertObjRel env planVars remoteJoinCtx stringifyNum objRelIns =
  withPathK (relNameToTxt relName) $ do
    (affRows, colValM) <- withPathK "data" $ insertObject env singleObjIns [] remoteJoinCtx planVars stringifyNum
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
    errMsg = "cannot proceed to insert object relation "
             <> relName <<> " since insert to table "
             <> table <<> " affects zero rows"

insertArrRel
  :: (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> [(PGCol, PG.SQLExp)]
  -> PGE.MutationRemoteJoinCtx
  -> Seq.Seq Q.PrepArg
  -> Bool
  -> IR.ArrRelIns 'Postgres PG.SQLExp
  -> m Int
insertArrRel env resCols remoteJoinCtx planVars stringifyNum arrRelIns =
  withPathK (relNameToTxt $ riName relInfo) $ do
    let additionalColumns = flip mapMaybe resCols \(column, value) -> do
          target <- Map.lookup column mapping
          Just (target, value)
    resBS <- withPathK "data" $
      insertMultipleObjects env multiObjIns additionalColumns remoteJoinCtx mutOutput planVars stringifyNum
    resObj <- decodeEncJSON resBS
    onNothing (Map.lookup ("affected_rows" :: Text) resObj) $
      throw500 "affected_rows not returned in array rel insert"
  where
    IR.RelIns multiObjIns relInfo = arrRelIns
    mapping   = riMapping relInfo
    mutOutput = IR.MOutMultirowFields [("affected_rows", IR.MCount)]

-- | validate an insert object based on insert columns,
-- | insert object relations and additional columns from parent
validateInsert
  :: (MonadError QErr m)
  => [PGCol]             -- ^ inserting columns
  -> [RelInfo 'Postgres] -- ^ object relation inserts
  -> [PGCol]             -- ^ additional fields from parent
  -> m ()
validateInsert insCols objRels addCols = do
  -- validate insertCols
  unless (null insConflictCols) $ throw400 ValidationFailed $
    "cannot insert " <> showPGCols insConflictCols
    <> " columns as their values are already being determined by parent insert"

  forM_ objRels $ \relInfo -> do
    let lCols = Map.keys $ riMapping relInfo
        relName = riName relInfo
        relNameTxt = relNameToTxt relName
        lColConflicts = lCols `intersect` (addCols <> insCols)
    withPathK relNameTxt $ unless (null lColConflicts) $ throw400 ValidationFailed $
      "cannot insert object relationship " <> relName
      <<> " as " <> showPGCols lColConflicts
      <> " column values are already determined"
  where
    insConflictCols = insCols `intersect` addCols


mkInsertQ
  :: MonadError QErr m
  => QualifiedTable
  -> Maybe (IR.ConflictClauseP1 'Postgres PG.SQLExp)
  -> [(PGCol, PG.SQLExp)]
  -> Map.HashMap PGCol PG.SQLExp
  -> (AnnBoolExpSQL 'Postgres, Maybe (AnnBoolExpSQL 'Postgres))
  -> m PG.CTE
mkInsertQ table onConflictM insCols defVals (insCheck, updCheck) = do
  let sqlConflict = PGT.toSQLConflict table <$> onConflictM
      sqlExps = mkSQLRow defVals insCols
      valueExp = PG.ValuesExp [PG.TupleExp sqlExps]
      tableCols = Map.keys defVals
      sqlInsert =
        PG.SQLInsert table tableCols valueExp sqlConflict
          . Just
          $ PG.RetExp
            [ PG.selectStar
            , PGT.insertOrUpdateCheckExpr table onConflictM
              (PGT.toSQLBoolExp (PG.QualTable table) insCheck)
              (fmap (PGT.toSQLBoolExp (PG.QualTable table)) updCheck)
            ]
  pure $ PG.CTEInsert sqlInsert

fetchFromColVals
  :: MonadError QErr m
  => ColumnValues 'Postgres TxtEncodedVal
  -> [ColumnInfo 'Postgres]
  -> m [(PGCol, PG.SQLExp)]
fetchFromColVals colVal reqCols =
  forM reqCols $ \ci -> do
    let valM = Map.lookup (pgiColumn ci) colVal
    val <- onNothing valM $ throw500 $ "column "
           <> pgiColumn ci <<> " not found in given colVal"
    let pgColVal = case val of
          TENull  -> PG.SENull
          TELit t -> PG.SELit t
    return (pgiColumn ci, pgColVal)

mkSQLRow :: Map.HashMap PGCol PG.SQLExp -> [(PGCol, PG.SQLExp)] -> [PG.SQLExp]
mkSQLRow defVals withPGCol = map snd $
  flip map (Map.toList defVals) $
    \(col, defVal) -> (col,) $ fromMaybe defVal $ Map.lookup col withPGColMap
  where
    withPGColMap = Map.fromList withPGCol

decodeEncJSON :: (J.FromJSON a, QErrM m) => EncJSON -> m a
decodeEncJSON =
  either (throw500 . T.pack) decodeValue .
  J.eitherDecode . encJToLBS
