{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Resolve.Insert
  (convertInsert)
where

import           Data.Foldable                     (foldrM)
import           Data.Has
import           Data.List                         (intersect)
import           Hasura.Prelude

import qualified Data.Aeson                        as J
import qualified Data.ByteString.Builder           as BB
import qualified Data.HashMap.Strict               as Map
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T
import qualified Data.Vector                       as V
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Database.PG.Query                 as Q
import qualified Hasura.RQL.DML.Insert             as RI
import qualified Hasura.RQL.DML.Returning          as RR
import qualified Hasura.RQL.DML.Select             as RS
import qualified Hasura.RQL.GBoolExp               as RG
import qualified Hasura.RQL.GBoolExp               as RB

import qualified Hasura.SQL.DML                    as S

import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Resolve.Mutation
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

data RelData a
  = RelData
  { _rdInsObj         :: a
  , _rdConflictClause :: !(Maybe AnnGValue)
  } deriving (Show, Eq)

type ObjRelData = RelData AnnGObject
type ArrRelData = RelData [AnnGObject]

type PGColWithValue = (PGCol, PGColValue)
type PGColWithType = (PGCol, PGColType)

parseRelObj
  :: MonadError QErr m
  => AnnGObject
  -> m (Either ObjRelData ArrRelData)
parseRelObj annObj = do
  let conflictClauseM = Map.lookup "on_conflict" annObj
  dataVal <- onNothing (Map.lookup "data" annObj) $ throw500 "data object not found"
  case dataVal of
    AGObject _ (Just obj) -> return $ Left $ RelData obj conflictClauseM
    AGArray _ (Just vals) -> do
      objs <- forM vals asObject
      return $ Right $ RelData objs conflictClauseM
    _ -> throw500 "unexpected type for data "

pgColToAnnGVal
  :: (PGCol, PGColType, PGColValue)
  -> (PGCol, AnnGValue)
pgColToAnnGVal (col, colTy, colVal) = (col, pgColValToAnnGVal colTy colVal)

toSQLExps :: (MonadError QErr m, MonadState PrepArgs m)
     => [(PGCol, AnnGValue)] -> m [(PGCol, S.SQLExp)]
toSQLExps cols =
  forM cols $ \(c, v) -> do
    prepExpM <- asPGColValM v >>= mapM prepare
    let prepExp = fromMaybe (S.SEUnsafe "NULL") prepExpM
    return (c, prepExp)

-- | insert a single row with returning expected columns
insertRow
  :: (QualifiedTable, QualifiedTable) -- (table, view)
  -> Maybe AnnGValue -- ^ conflict clause
  -> [(PGCol, AnnGValue)] -- ^ inserting row columns with graphQL value
  -> [PGCol] -- ^ all table columns
  -> [PGColWithType] -- ^ expected returning columns
  -> RoleName -- ^ role
  -> Q.TxE QErr (Int, Maybe [PGColWithValue]) -- ^
insertRow (tn, vn) onConflictValM insCols tableCols expectedCols role = do
  (givenCols, args) <- flip runStateT Seq.Empty $ toSQLExps insCols
  onConflictM <- forM onConflictValM $ parseOnConflict (map fst insCols)
  let sqlExps = Map.elems $ Map.union (Map.fromList givenCols) defVals
      p1Query = RI.InsertQueryP1 tn vn tableCols [sqlExps] onConflictM mutFlds
      p1 = (p1Query, args)
  res <- bool (RI.nonAdminInsert p1) (RI.insertP2 p1) $ isAdmin role
  InsResp affRows respObjM <- decodeFromBS res
  retColValuesM <- mapM mkRetColValues respObjM
  return (affRows, retColValuesM)
  where
    defVals = Map.fromList $ zip tableCols (repeat $ S.SEUnsafe "DEFAULT")
    mutFlds = Map.fromList [ ("affected_rows", RR.MCount)
                           , ("response", RR.MRet selData)
                           ]
    selData = RS.SelectData flds tn frmExpM (S.BELit True, Nothing)
              Nothing [] Nothing Nothing True
    frmExpM = Just $ S.FromExp $ pure $
              S.FIIden $ qualTableToAliasIden tn
    flds = Map.fromList $ flip map expectedCols $ \(c, ty) ->
      (fromPGCol c, RS.FCol (c, ty))

    mkRetColValues obj = forM expectedCols $ \(col, colty) -> do
      val <- onNothing (Map.lookup (getPGColTxt col) obj) $
             throw500 $ "column " <> col <<> "not found in postgres returning"
      pgColValue <- RB.pgValParser colty val
      return (col, pgColValue)

-- | resolve a graphQL object to columns, object and array relations
fetchColsAndRels
  :: MonadError QErr m
  => AnnGObject
  -> m ( [(PGCol, PGColType, PGColValue)] -- ^ columns
       , [(RelName, ObjRelData)] -- ^ object relations
       , [(RelName, ArrRelData)] -- ^ array relations
       )
fetchColsAndRels annObj = foldrM go ([], [], []) $ Map.toList annObj
  where
    go (gName, annVal) (cols, objRels, arrRels) =
      case annVal of
        AGScalar colty mColVal -> do
          let col = PGCol $ G.unName gName
              colVal = fromMaybe (PGNull colty) mColVal
          return ((col, colty, colVal):cols, objRels, arrRels)
        AGObject _ (Just obj) -> do
          let relName = RelName $ G.unName gName
          relObj <- parseRelObj obj
          return $ either
            (\relData -> (cols, (relName, relData):objRels, arrRels))
            (\relData -> (cols, objRels, (relName, relData):arrRels))
            relObj
        _ -> throw500 "unexpected Array or Enum for input cols"

insertObjRel
  :: RoleName
  -> InsCtxMap
  -> InsCtx
  -> RelInfo
  -> ObjRelData
  -> Q.TxE QErr (Int, [(PGCol, PGColValue)])
insertObjRel role insCtxMap insCtx relInfo relData = do
  let mapCols = riMapping relInfo
      tn = riRTable relInfo
      rCols = map snd mapCols
      cs = icColumns insCtx
      insCols = map (\(PGColInfo cn ty _) -> (cn, ty)) $
        getColInfos rCols cs
  res <- processInsObj role insCtxMap tn insObj insCtx [] insCols onConflictM
  let aRows = fst res
      respColsM = snd res
  respCols <- maybe (cannotInsObjRelErr tn) return respColsM
  let c = mergeListsWith mapCols respCols
        (\(_, rCol) (col, _) -> rCol == col)
        (\(lCol, _) (_, colVal) -> (lCol, colVal))
  return (aRows, c)
  where
    relName = riName relInfo
    RelData insObj onConflictM = relData
    cannotInsObjRelErr tn = throwVE $
      "cannot insert object relation "
      <> relName <<> " since inserting into remote table "
      <> tn <<> " returns nothing"

processObjRel
  :: (MonadError QErr m)
  => InsCtxMap
  -> [(RelName, ObjRelData)]
  -> RelationInfoMap
  -> m [(ObjRelData, InsCtx, RelInfo)]
processObjRel insCtxMap objRels relInfoMap =
  forM objRels $ \(relName, rd) -> do
  relInfo <- onNothing (Map.lookup relName relInfoMap) $ throw500 $
    "object relationship with name " <> relName <<> " not found"
  let remoteTable = riRTable relInfo
  insCtx <- getInsCtx insCtxMap remoteTable
  return (rd, insCtx, relInfo)

-- | process array relation and return dependent columns,
-- | relation data, insert context of remote table and relation info
processArrRel
  :: (MonadError QErr m)
  => InsCtxMap
  -> [(RelName, ArrRelData)]
  -> RelationInfoMap
  -> m [([PGCol], ArrRelData, InsCtx, RelInfo)]
processArrRel insCtxMap arrRels relInfoMap =
  forM arrRels $ \(relName, rd) -> do
    relInfo <- onNothing (Map.lookup relName relInfoMap) $ throw500 $
      "relation with name " <> relName <<> " not found"
    let depCols = map fst $ riMapping relInfo
        remoteTable = riRTable relInfo
    insCtx <- getInsCtx insCtxMap remoteTable
    return (depCols, rd, insCtx, relInfo)

insertArrRel
  :: RoleName
  -> InsCtxMap
  -> InsCtx
  -> RelInfo
  -> [PGColWithValue]
  -> ArrRelData
  -> Q.TxE QErr Int
insertArrRel role insCtxMap insCtx relInfo resCols relData = do
  let addCols = mergeListsWith resCols colMapping
             (\(col, _) (lCol, _) -> col == lCol)
             (\(_, colVal) (_, rCol) -> (rCol, colVal))
  res <- forM insObjs $ \annGObj ->
    processInsObj role insCtxMap tn annGObj insCtx addCols [] onConflictM
  return $ sum $ map fst res
  where
    colMapping = riMapping relInfo
    tn = riRTable relInfo
    RelData insObjs onConflictM = relData

validateInsert
  :: (MonadError QErr m)
  => [PGCol] -- ^ inserting columns
  -> [RelInfo] -- ^ object relation inserts
  -> [PGCol] -- ^ additional fields from parent
  -> m ()
validateInsert insCols objRels addCols = do
  -- validate insertCols
  unless (null insConflictCols) $ throwVE $
    "cannot insert " <> pgColsToText insConflictCols
    <> " columns as their values are already being determined by parent insert"

  forM_ objRels $ \relInfo -> do
    let lCols = map fst $ riMapping relInfo
        relName = riName relInfo
        lColConflicts = lCols `intersect` (addCols <> insCols)
    unless (null lColConflicts) $ throwVE $
      "cannot insert object relation ship " <> relName
      <<> " as " <> pgColsToText lColConflicts
      <> " column values are already determined"
  where
    insConflictCols = insCols `intersect` addCols
    pgColsToText cols = T.intercalate ", " $ map getPGColTxt cols

mkPGColWithTypeAndVal :: [PGColInfo] -> [PGColWithValue] -> [(PGCol, PGColType, PGColValue)]
mkPGColWithTypeAndVal pgColInfos pgColWithVal =
    mergeListsWith pgColInfos pgColWithVal
    (\ci (c, _) -> pgiName ci == c)
    (\ci (c, v) -> (c, pgiType ci, v))

-- | insert a object with object and array relationships
processInsObj
  :: RoleName
  -> InsCtxMap
  -> QualifiedTable
  -> AnnGObject -- ^ object to be inserted
  -> InsCtx -- ^ required insert context
  -> [PGColWithValue] -- ^ additional fields
  -> [PGColWithType] -- ^ expected returning columns
  -> Maybe AnnGValue -- ^ on conflict context
  -> Q.TxE QErr (Int, Maybe [PGColWithValue])
processInsObj role insCtxMap tn annObj ctx addCols retCols onConflictM = do
  (cols, objRels, arrRels) <- fetchColsAndRels annObj

  processedObjRels <- processObjRel insCtxMap objRels relInfoMap

  validateInsert (map _1 cols) (map _3 processedObjRels) $ map fst addCols

  objInsRes <- forM processedObjRels $ \(relData, insCtx, relInfo) ->
    insertObjRel role insCtxMap insCtx relInfo relData

  -- prepare final insert columns
  let objInsAffRows = sum $ map fst objInsRes
      objRelDeterminedCols = concatMap snd objInsRes
      objRelInsCols = mkPGColWithTypeAndVal tableColInfos objRelDeterminedCols
      addInsCols = mkPGColWithTypeAndVal tableColInfos addCols
      finalInsCols =  map pgColToAnnGVal (cols <> objRelInsCols <> addInsCols)

  -- fetch array rel deps Cols
  processedArrRels <- processArrRel insCtxMap arrRels relInfoMap

  -- prepare final returning columns
  let arrDepCols = concatMap (\(a, _, _, _) -> a) processedArrRels
      arrDepColsWithType = mergeListsWith arrDepCols tableColInfos
                           (\c ci -> c == pgiName ci)
                           (\c ci -> (c, pgiType ci))
      finalRetCols = retCols <> arrDepColsWithType

  (insAffRows, resColsM) <- insertRow (tn, vn) onConflictM finalInsCols
                            (map pgiName tableColInfos) finalRetCols role


  arrInsRes <- forM processedArrRels $ \(_, rd, insCtx, relInfo) -> do
    resCols <- maybe cannotInsArrRelErr return resColsM
    insertArrRel role insCtxMap insCtx relInfo resCols rd

  let retColsWithValM = flip fmap resColsM $ \resCols ->
        mergeListsWith retCols resCols
          (\(colA, _) (colB, _) -> colA == colB)
          (\(col, _) (_, colVal) -> (col, colVal))
      arrInsAffRows = sum arrInsRes
  return (insAffRows + objInsAffRows + arrInsAffRows, retColsWithValM)

  where
    InsCtx vn tableColInfos relInfoMap = ctx
    cannotInsArrRelErr = throwVE $
      "cannot proceed to insert array relations since insert to "
      <> tn <<> " returns nothing"


mkBoolExp
  :: (MonadError QErr m, MonadState PrepArgs m)
  => QualifiedTable -> [(PGColInfo, PGColValue)]
  -> m (GBoolExp RG.AnnSQLBoolExp)
mkBoolExp tn colInfoVals =
  RG.convBoolRhs (RG.mkBoolExpBuilder prepare) (S.mkQual tn) boolExp
  where
    boolExp = BoolAnd $ map (BoolCol . uncurry f) colInfoVals
    f ci@(PGColInfo _ colTy _) colVal =
      RB.AVCol ci [RB.OEVal $ RB.AEQ (colTy, colVal)]

mkReturning
  :: QualifiedTable
  -> [(PGColInfo, PGColValue)]
  -> RS.AnnSelFlds
  -> Q.TxE QErr RespBody
mkReturning tn pkeyColVals annFlds = do
  (whereExp, args) <- flip runStateT Seq.empty $ mkBoolExp tn pkeyColVals
  let selData = RS.SelectData annFlds tn Nothing
        (S.BELit True, Just whereExp) Nothing [] Nothing Nothing True
  RS.selectP2 (selData, args)

buildReturningResp
  :: QualifiedTable
  -> Seq.Seq [(PGColInfo, PGColValue)]
  -> RS.AnnSelFlds
  -> Q.TxE QErr RespBody
buildReturningResp tn pkeyColSeq annFlds = do
  respList <- forM pkeyColSeq $ \pkeyCols ->
    mkReturning tn pkeyCols annFlds
  let bsVector = V.fromList $ toList respList
  return $ BB.toLazyByteString $ RR.encodeJSONVector BB.lazyByteString bsVector

getInsCtxMap
  :: (Has InsCtxMap r, MonadReader r m)
  => m InsCtxMap
getInsCtxMap = asks getter

getInsCtx
  :: MonadError QErr m
  => InsCtxMap -> QualifiedTable -> m InsCtx
getInsCtx ctxMap tn =
  onNothing (Map.lookup tn ctxMap) $ throw500 $ "table " <> tn <<> " not found"

convertInsert'
  :: RoleName
  -> QualifiedTable
  -> InsCtxMap
  -> InsCtx
  -> [PGCol]
  -> Field
  -> Convert RespTx
convertInsert' role tn insCtxMap insCtx pCols fld = do
  annVals <- withArg arguments "objects" asArray
  annObjs <- forM annVals asObject
  mutFlds <- convertMutResp tn (_fType fld) $ _fSelSet fld
  return $ buildInsertTx annObjs mutFlds
  where
    InsCtx _ tableColInfos _ = insCtx
    arguments = _fArguments fld
    onConflictM = Map.lookup "on_conflict" arguments
    -- consider all table columns if no primary key columns present
    pCols' = bool pCols (map pgiName tableColInfos) $ null pCols
    reqRetCols = mergeListsWith pCols' tableColInfos
                 (\c ti -> c == pgiName ti)
                 (\c ti -> (c, pgiType ti))

    buildInsertTx annObjs mutFlds = do
      insResps <- forM annObjs $ \obj -> do
        (affRows, retColValsM) <-
          processInsObj role insCtxMap tn obj insCtx [] reqRetCols onConflictM
        let retCols = flip fmap retColValsM $ \retColsVals ->
              mergeListsWith tableColInfos retColsVals
              (\ci (c, _) -> pgiName ci == c)
              (\ci (_, v) -> (ci, v))
        return (affRows, retCols)
      let affRows = sum $ map fst insResps
          pkeyColVals = map snd insResps
          pkeyColValSeqM = Seq.fromList <$> sequence pkeyColVals
      respTups <- forM (Map.toList mutFlds) $ \(t, mutFld) -> do
        jsonVal <- case mutFld of
          RR.MCount -> return $ J.toJSON affRows
          RR.MExp txt -> return $ J.toJSON txt
          RR.MRet selData -> do
            let annFlds = RS.sdFlds selData
            bs <- maybe (return "[]")
                  (\pkeyColValSeq -> buildReturningResp tn pkeyColValSeq annFlds)
                  pkeyColValSeqM
            decodeFromBS bs
        return (t, jsonVal)
      return $ J.encode $ Map.fromList respTups


convertInsert
  :: RoleName
  -> QualifiedTable -- table
  -> [PGCol] -- primary key columns
  -> Field -- the mutation field
  -> Convert RespTx
convertInsert role tn pCols fld = do
  insCtxMap <- getInsCtxMap
  insCtx <- getInsCtx insCtxMap tn
  convertInsert' role tn insCtxMap insCtx pCols fld

-- helper functions
mergeListsWith
  :: [a] -> [b] -> (a -> b -> Bool) -> (a -> b -> c) -> [c]
mergeListsWith _ [] _ _ = []
mergeListsWith [] _ _ _ = []
mergeListsWith (x:xs) l b f = case find (b x) l of
  Nothing -> mergeListsWith xs l b f
  Just y  ->  f x y : mergeListsWith xs l b f

_1 :: (a, b, c) -> a
_1 (x, _, _) = x

_3 :: (a, b, c) -> c
_3 (_, _, z) = z
