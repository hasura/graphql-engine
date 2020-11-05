module Hasura.GraphQL.Execute.Insert
  ( traverseAnnInsert
  , convertToSQLTransaction
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                   as J
import qualified Data.Environment                             as Env
import qualified Data.HashMap.Strict                          as Map
import qualified Data.Sequence                                as Seq
import qualified Data.Text                                    as T
import qualified Database.PG.Query                            as Q

import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.Execute.Mutation    as RQL
import qualified Hasura.Backends.Postgres.Execute.RemoteJoin  as RQL
import qualified Hasura.Backends.Postgres.SQL.DML             as S
import qualified Hasura.Backends.Postgres.Translate.BoolExp   as RQL
import qualified Hasura.Backends.Postgres.Translate.Insert    as RQL
import qualified Hasura.Backends.Postgres.Translate.Mutation  as RQL
import qualified Hasura.Backends.Postgres.Translate.Returning as RQL
import qualified Hasura.RQL.IR.Insert                         as RQL
import qualified Hasura.RQL.IR.Returning                      as RQL
import qualified Hasura.Tracing                               as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.EncJSON
import           Hasura.GraphQL.Schema.Insert
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.Server.Version                        (HasVersion)


traverseAnnInsert
  :: (Applicative f)
  => (a -> f b)
  -> AnnInsert backend a
  -> f (AnnInsert backend b)
traverseAnnInsert f (AnnInsert fieldName isSingle (annIns, mutationOutput)) =
  AnnInsert fieldName isSingle
  <$> ( (,)
        <$> traverseMulti annIns
        <*> RQL.traverseMutationOutput f mutationOutput
      )
  where
    traverseMulti (AnnIns objs tableName conflictClause checkCond columns defaultValues) = AnnIns
      <$> traverse traverseObject objs
      <*> pure tableName
      <*> traverse (traverse f) conflictClause
      <*> ( (,)
            <$> traverseAnnBoolExp f (fst checkCond)
            <*> traverse (traverseAnnBoolExp f) (snd checkCond)
          )
      <*> pure columns
      <*> traverse f defaultValues
    traverseSingle (AnnIns obj tableName conflictClause checkCond columns defaultValues) = AnnIns
      <$> traverseObject obj
      <*> pure tableName
      <*> traverse (traverse f) conflictClause
      <*> ( (,)
            <$> traverseAnnBoolExp f (fst checkCond)
            <*> traverse (traverseAnnBoolExp f) (snd checkCond)
          )
      <*> pure columns
      <*> traverse f defaultValues
    traverseObject (AnnInsObj columns objRels arrRels) = AnnInsObj
      <$> traverse (traverse f) columns
      <*> traverse (traverseRel traverseSingle) objRels
      <*> traverse (traverseRel traverseMulti)  arrRels
    traverseRel z (RelIns object relInfo) = RelIns <$> z object <*> pure relInfo


convertToSQLTransaction
  :: (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> AnnInsert 'Postgres S.SQLExp
  -> RQL.MutationRemoteJoinCtx
  -> Seq.Seq Q.PrepArg
  -> Bool
  -> m EncJSON
convertToSQLTransaction env (AnnInsert fieldName isSingle (annIns, mutationOutput)) remoteJoinCtx planVars stringifyNum =
  if null $ _aiInsObj annIns
  then pure $ RQL.buildEmptyMutResp mutationOutput
  else withPaths ["selectionSet", fieldName, "args", suffix] $
    insertMultipleObjects env annIns [] remoteJoinCtx mutationOutput planVars stringifyNum
  where
    withPaths p x = foldr ($) x $ withPathK <$> p
    suffix = bool "objects" "object" isSingle

insertMultipleObjects
  :: (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> MultiObjIns 'Postgres S.SQLExp
  -> [(PGCol, S.SQLExp)]
  -> RQL.MutationRemoteJoinCtx
  -> RQL.MutationOutput 'Postgres
  -> Seq.Seq Q.PrepArg
  -> Bool
  -> m EncJSON
insertMultipleObjects env multiObjIns additionalColumns remoteJoinCtx mutationOutput planVars stringifyNum =
    bool withoutRelsInsert withRelsInsert anyRelsToInsert
  where
    AnnIns insObjs table conflictClause checkCondition columnInfos defVals = multiObjIns
    allInsObjRels = concatMap _aioObjRels insObjs
    allInsArrRels = concatMap _aioArrRels insObjs
    anyRelsToInsert = not $ null allInsArrRels && null allInsObjRels

    withoutRelsInsert = do
      indexedForM_ (_aioColumns <$> insObjs) \column ->
        validateInsert (map fst column) [] (map fst additionalColumns)
      let columnValues = map (mkSQLRow defVals) $ union additionalColumns . _aioColumns <$> insObjs
          columnNames  = Map.keys defVals
          insertQuery  = RQL.InsertQueryP1
            table
            columnNames
            columnValues
            conflictClause
            checkCondition
            mutationOutput
            columnInfos
          rowCount = T.pack . show . length $ _aiInsObj multiObjIns
      Tracing.trace ("Insert (" <> rowCount <> ") " <> qualifiedObjectToText table) do
        Tracing.attachMetadata [("count", rowCount)]
        RQL.execInsertQuery env stringifyNum (Just remoteJoinCtx) (insertQuery, planVars)

    withRelsInsert = do
      insertRequests <- indexedForM insObjs \obj -> do
        let singleObj = AnnIns obj table conflictClause checkCondition columnInfos defVals
        insertObject env singleObj additionalColumns remoteJoinCtx planVars stringifyNum
      let affectedRows = sum $ map fst insertRequests
          columnValues = mapMaybe snd insertRequests
      selectExpr <- RQL.mkSelCTEFromColVals table columnInfos columnValues
      let (mutOutputRJ, remoteJoins) = RQL.getRemoteJoinsMutationOutput mutationOutput
          sqlQuery = Q.fromBuilder $ toSQL $
                     RQL.mkMutationOutputExp table columnInfos (Just affectedRows) selectExpr mutOutputRJ stringifyNum
      RQL.executeMutationOutputQuery env sqlQuery [] $ (,remoteJoinCtx) <$> remoteJoins

insertObject
  :: (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> SingleObjIns 'Postgres S.SQLExp
  -> [(PGCol, S.SQLExp)]
  -> RQL.MutationRemoteJoinCtx
  -> Seq.Seq Q.PrepArg
  -> Bool
  -> m (Int, Maybe (ColumnValues TxtEncodedPGVal))
insertObject env singleObjIns additionalColumns remoteJoinCtx planVars stringifyNum = Tracing.trace ("Insert " <> qualifiedObjectToText table) do
  validateInsert (map fst columns) (map _riRelInfo objectRels) (map fst additionalColumns)

  -- insert all object relations and fetch this insert dependent column values
  objInsRes <- forM objectRels $ insertObjRel env planVars remoteJoinCtx stringifyNum

  -- prepare final insert columns
  let objRelAffRows = sum $ map fst objInsRes
      objRelDeterminedCols = concatMap snd objInsRes
      finalInsCols = columns <> objRelDeterminedCols <> additionalColumns

  cte <- mkInsertQ table onConflict finalInsCols defaultValues checkCond

  MutateResp affRows colVals <- liftTx $ RQL.mutateAndFetchCols table allColumns (cte, planVars) stringifyNum
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
      arrInsARows <- forM arrayRels $ insertArrRel env arrDepColsWithVal remoteJoinCtx planVars stringifyNum
      return $ sum arrInsARows

    asSingleObject = \case
      []  -> pure Nothing
      [r] -> pure $ Just r
      _   -> throw500 "more than one row returned"

    cannotInsArrRelErr =
      "cannot proceed to insert array relations since insert to table "
      <> table <<> " affects zero rows"

insertObjRel
  :: (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> Seq.Seq Q.PrepArg
  -> RQL.MutationRemoteJoinCtx
  -> Bool
  -> ObjRelIns 'Postgres S.SQLExp
  -> m (Int, [(PGCol, S.SQLExp)])
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
    RelIns singleObjIns relInfo = objRelIns
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
  :: (HasVersion, MonadTx m, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> [(PGCol, S.SQLExp)]
  -> RQL.MutationRemoteJoinCtx
  -> Seq.Seq Q.PrepArg
  -> Bool
  -> ArrRelIns 'Postgres S.SQLExp
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
    RelIns multiObjIns relInfo = arrRelIns
    mapping   = riMapping relInfo
    mutOutput = RQL.MOutMultirowFields [("affected_rows", RQL.MCount)]

-- | validate an insert object based on insert columns,
-- | insert object relations and additional columns from parent
validateInsert
  :: (MonadError QErr m)
  => [PGCol] -- ^ inserting columns
  -> [RelInfo] -- ^ object relation inserts
  -> [PGCol] -- ^ additional fields from parent
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
      "cannot insert object relation ship " <> relName
      <<> " as " <> showPGCols lColConflicts
      <> " column values are already determined"
  where
    insConflictCols = insCols `intersect` addCols


mkInsertQ
  :: MonadError QErr m
  => QualifiedTable
  -> Maybe (RQL.ConflictClauseP1 'Postgres S.SQLExp)
  -> [(PGCol, S.SQLExp)]
  -> Map.HashMap PGCol S.SQLExp
  -> (AnnBoolExpSQL 'Postgres, Maybe (AnnBoolExpSQL 'Postgres))
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
  -> [ColumnInfo 'Postgres]
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

decodeEncJSON :: (J.FromJSON a, QErrM m) => EncJSON -> m a
decodeEncJSON =
  either (throw500 . T.pack) decodeValue .
  J.eitherDecode . encJToLBS
