{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Resolve.Insert
  (convertInsert)
where

import           Data.Foldable                     (foldrM)
import           Data.Has
import           Data.List                         (intersect, union)
import           Hasura.Prelude

import qualified Data.Aeson                        as J
import qualified Data.ByteString.Builder           as BB
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Data.HashSet                      as Set
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
import           Hasura.GraphQL.Resolve.Select
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal           (dmlTxErrorHandler)
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

type AnnSelFlds = [(FieldName, RS.AnnFld)]
type WithExp = (S.CTE, Seq.Seq Q.PrepArg)

mkConflictClause :: RI.ConflictCtx -> RI.ConflictClauseP1
mkConflictClause (RI.CCDoNothing constrM) =
  RI.CP1DoNothing $ fmap RI.Constraint constrM
mkConflictClause (RI.CCUpdate constr updCols) =
  RI.CP1Update (RI.Constraint constr) updCols

parseAction
  :: (MonadError QErr m)
  => AnnGObject -> m (Maybe ConflictAction)
parseAction obj = withPathK "action" $
  mapM parseVal $ OMap.lookup "action" obj
  where
    parseVal val = do
      (enumTy, enumVal) <- asEnumVal val
      case G.unName $ G.unEnumValue enumVal of
        "ignore" -> return CAIgnore
        "update" -> return CAUpdate
        _ -> throw500 $
          "only \"ignore\" and \"updated\" allowed for enum type "
          <> showNamedTy enumTy

parseConstraint
  :: (MonadError QErr m)
  => AnnGObject -> m ConstraintName
parseConstraint obj = withPathK "constraint" $ do
  v <- onNothing (OMap.lookup "constraint" obj) $ throw500
    "\"constraint\" is expected, but not found"
  parseVal v
  where
    parseVal v = do
      (_, enumVal) <- asEnumVal v
      return $ ConstraintName $ G.unName $ G.unEnumValue enumVal

parseUpdCols
  :: (MonadError QErr m)
  => AnnGObject -> m (Maybe [PGCol])
parseUpdCols obj = withPathK "update_columns" $
  mapM parseVal $ OMap.lookup "update_columns" obj
  where
    parseVal val = flip withArray val $ \_ enumVals ->
      forM enumVals $ \eVal -> do
        (_, v) <- asEnumVal eVal
        return $ PGCol $ G.unName $ G.unEnumValue v

parseOnConflict
  :: (MonadError QErr m)
  => [PGCol] -> AnnGValue -> m RI.ConflictClauseP1
parseOnConflict inpCols val = withPathK "on_conflict" $
  flip withObject val $ \_ obj -> do
    actionM <- parseAction obj
    constraint <- parseConstraint obj
    updColsM <- parseUpdCols obj
    -- consider "action" if "update_columns" is not mentioned
    return $ mkConflictClause $ case (updColsM, actionM) of
      (Just [], _)             -> RI.CCDoNothing $ Just constraint
      (Just cols, _)           -> RI.CCUpdate constraint cols
      (Nothing, Just CAIgnore) -> RI.CCDoNothing $ Just constraint
      (Nothing, _)             -> RI.CCUpdate constraint inpCols

parseRelObj
  :: MonadError QErr m
  => AnnGObject
  -> m (Either ObjRelData ArrRelData)
parseRelObj annObj = do
  let conflictClauseM = OMap.lookup "on_conflict" annObj
  dataVal <- onNothing (OMap.lookup "data" annObj) $ throw500 "\"data\" object not found"
  case dataVal of
    AGObject _ (Just obj) -> return $ Left $ RelData obj conflictClauseM
    AGArray _ (Just vals) -> do
      objs <- forM vals asObject
      return $ Right $ RelData objs conflictClauseM
    _ -> throw500 "unexpected type for \"data\""

toSQLExps :: (MonadError QErr m, MonadState PrepArgs m)
     => [(PGCol, AnnGValue)] -> m [(PGCol, S.SQLExp)]
toSQLExps cols =
  forM cols $ \(c, v) -> do
    prepExpM <- asPGColValM v >>= mapM prepare
    let prepExp = fromMaybe (S.SEUnsafe "NULL") prepExpM
    return (c, prepExp)

mkSQLRow :: [PGCol] -> [(PGCol, S.SQLExp)] -> [S.SQLExp]
mkSQLRow tableCols withPGCol =
  Map.elems $ Map.union (Map.fromList withPGCol) defVals
  where
    defVals = Map.fromList $ zip tableCols (repeat $ S.SEUnsafe "DEFAULT")

mkInsertQ :: QualifiedTable
          -> Maybe RI.ConflictClauseP1 -> [(PGCol, AnnGValue)]
          -> [PGCol] -> RoleName
          -> Q.TxE QErr WithExp
mkInsertQ vn onConflictM insCols tableCols role = do
  (givenCols, args) <- flip runStateT Seq.Empty $ toSQLExps insCols
  let sqlConflict = RI.toSQLConflict <$> onConflictM
      sqlExps = mkSQLRow tableCols givenCols
      sqlInsert = S.SQLInsert vn tableCols [sqlExps] sqlConflict $ Just S.returningStar
  if isAdmin role then return (S.CTEInsert sqlInsert, args)
  else do
    ccM <- mapM RI.extractConflictCtx onConflictM
    RI.setConflictCtx ccM
    return (S.CTEInsert (sqlInsert{S.siConflict=Nothing}), args)

-- | resolve a graphQL object to columns, object and array relations
fetchColsAndRels
  :: MonadError QErr m
  => AnnGObject
  -> m ( [(PGCol, PGColType, PGColValue)] -- ^ columns
       , [(RelName, ObjRelData)] -- ^ object relations
       , [(RelName, ArrRelData)] -- ^ array relations
       )
fetchColsAndRels annObj = foldrM go ([], [], []) $ OMap.toList annObj
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

-- | process array relation and return relation data, insert context
-- | of remote table and relation info
processObjRel
  :: (MonadError QErr m)
  => InsCtxMap
  -> [(RelName, ObjRelData)]
  -> RelationInfoMap
  -> m [(ObjRelData, InsCtx, RelInfo)]
processObjRel insCtxMap objRels relInfoMap =
  forM objRels $ \(relName, rd) -> withPathK (getRelTxt relName) $ do
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
  forM arrRels $ \(relName, rd) -> withPathK (getRelTxt relName) $ do
    relInfo <- onNothing (Map.lookup relName relInfoMap) $ throw500 $
      "relation with name " <> relName <<> " not found"
    let depCols = map fst $ riMapping relInfo
        remoteTable = riRTable relInfo
    insCtx <- getInsCtx insCtxMap remoteTable
    return (depCols, rd, insCtx, relInfo)

-- | insert an object relationship and return affected rows
-- | and parent dependent columns
insertObjRel
  :: RoleName
  -> InsCtxMap
  -> InsCtx
  -> RelInfo
  -> ObjRelData
  -> Q.TxE QErr (Int, [PGColWithValue])
insertObjRel role insCtxMap insCtx relInfo relData =
  withPathK relNameTxt $ do
    (aRows, withExp) <- insertObj role insCtxMap tn insObj
                        insCtx [] onConflictM "data"
    when (aRows == 0) $ throwVE $ "cannot proceed to insert object relation "
      <> relName <<> " since insert to table " <> tn <<> " affects zero rows"
    retColsWithVals <- insertAndRetCols tn withExp $
                       getColInfos rCols allCols
    let c = mergeListsWith mapCols retColsWithVals
          (\(_, rCol) (col, _) -> rCol == col)
          (\(lCol, _) (_, colVal) -> (lCol, colVal))
    return (aRows, c)
  where
    RelData insObj onConflictM = relData
    relName = riName relInfo
    relNameTxt = getRelTxt relName
    mapCols = riMapping relInfo
    tn = riRTable relInfo
    rCols = map snd mapCols
    allCols = icColumns insCtx

-- | insert an array relationship and return affected rows
insertArrRel
  :: RoleName
  -> InsCtxMap
  -> InsCtx
  -> RelInfo
  -> [PGColWithValue]
  -> ArrRelData
  -> Q.TxE QErr Int
insertArrRel role insCtxMap insCtx relInfo resCols relData =
    withPathK relNameTxt $ do
    let addCols = mergeListsWith resCols colMapping
               (\(col, _) (lCol, _) -> col == lCol)
               (\(_, colVal) (_, rCol) -> (rCol, colVal))

    resBS <- insertMultipleObjects role insCtxMap tn insCtx
      insObjs addCols mutFlds onConflictM True
    resObj <- decodeFromBS resBS
    onNothing (Map.lookup ("affected_rows" :: T.Text) resObj) $
      throw500 "affected_rows not returned in array rel insert"
  where
    colMapping = riMapping relInfo
    tn = riRTable relInfo
    relNameTxt = getRelTxt $ riName relInfo
    RelData insObjs onConflictM = relData
    mutFlds = [("affected_rows", RR.MCount)]

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
  unless (null insConflictCols) $ throwVE $
    "cannot insert " <> pgColsToText insConflictCols
    <> " columns as their values are already being determined by parent insert"

  forM_ objRels $ \relInfo -> do
    let lCols = map fst $ riMapping relInfo
        relName = riName relInfo
        relNameTxt = getRelTxt relName
        lColConflicts = lCols `intersect` (addCols <> insCols)
    withPathK relNameTxt $ unless (null lColConflicts) $ throwVE $
      "cannot insert object relation ship " <> relName
      <<> " as " <> pgColsToText lColConflicts
      <> " column values are already determined"
  where
    insConflictCols = insCols `intersect` addCols
    pgColsToText cols = T.intercalate ", " $ map getPGColTxt cols

-- | insert an object with object and array relationships
insertObj
  :: RoleName
  -> InsCtxMap
  -> QualifiedTable
  -> AnnGObject -- ^ object to be inserted
  -> InsCtx -- ^ required insert context
  -> [PGColWithValue] -- ^ additional fields
  -> Maybe AnnGValue -- ^ on conflict context
  -> T.Text -- ^ error path
  -> Q.TxE QErr (Int, WithExp)
insertObj role insCtxMap tn annObj ctx addCols onConflictValM errP = do
  -- get all insertable columns, object and array relations
  (cols, objRels, arrRels) <- withErrPath $ fetchColsAndRels annObj

  processedObjRels <- processObjRel insCtxMap objRels relInfoMap

  -- validate insert
  validateInsert (map _1 cols) (map _3 processedObjRels) $ map fst addCols

  -- insert all object relations and fetch this insert dependent column values
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
      arrDepColsWithInfo = getColInfos arrDepCols tableColInfos

  onConflictM <- forM onConflictValM $ parseOnConflict (map fst finalInsCols)

  -- calculate affected rows
  let anyRowsAffected = not $ or $ fmap RI.isDoNothing onConflictM
      thisInsAffRows = bool 0 1 anyRowsAffected
      preArrRelInsAffRows = objInsAffRows + thisInsAffRows

  -- prepare insert query as with expression
  insQ <- mkInsertQ vn onConflictM finalInsCols (map pgiName tableColInfos) role

  let insertWithArrRels = cannotInsArrRelErr thisInsAffRows >>
                          withArrRels preArrRelInsAffRows insQ
                            arrDepColsWithInfo processedArrRels
      insertWithoutArrRels = withNoArrRels preArrRelInsAffRows insQ

  bool insertWithArrRels insertWithoutArrRels $ null arrDepColsWithInfo

  where
    InsCtx vn tableColInfos relInfoMap = ctx
    withErrPath = withPathK errP

    withNoArrRels affRows insQ = return (affRows, insQ)

    withArrRels affRows insQ arrDepColsWithType processedArrRels = do
      arrDepColsWithVal <- insertAndRetCols tn insQ arrDepColsWithType

      arrInsARows <- forM processedArrRels $ \(_, rd, insCtx, relInfo) ->
        insertArrRel role insCtxMap insCtx relInfo arrDepColsWithVal rd

      let totalAffRows = affRows + sum arrInsARows

      selQ <- mkSelQ tn tableColInfos arrDepColsWithVal
      return (totalAffRows, selQ)

    cannotInsArrRelErr affRows = when (affRows == 0) $ throwVE $
      "cannot proceed to insert array relations since insert to table "
      <> tn <<> " affects zero rows"


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

mkSelQ :: QualifiedTable
  -> [PGColInfo] -> [PGColWithValue] -> Q.TxE QErr WithExp
mkSelQ tn allColInfos pgColsWithVal = do
  (whereExp, args) <- flip runStateT Seq.Empty $ mkBoolExp tn colWithInfos
  let sqlSel = S.mkSelect { S.selExtr = [S.selectStar]
                          , S.selFrom = Just $ S.mkSimpleFromExp tn
                          , S.selWhere = Just $ S.WhereFrag $ RG.cBoolExp whereExp
                          }

  return (S.CTESelect sqlSel, args)
  where
    colWithInfos = mergeListsWith pgColsWithVal allColInfos
                   (\(c, _) ci -> c == pgiName ci)
                   (\(_, v) ci -> (ci, v))

execWithExp
  :: QualifiedTable
  -> WithExp
  -> AnnSelFlds
  -> Q.TxE QErr RespBody
execWithExp tn (withExp, args) annFlds = do
  let annSel = RS.AnnSel annFlds tn frmItemM
        (S.BELit True) Nothing RS.noTableArgs
      sqlSel = RS.mkSQLSelect True annSel
      selWith = S.SelectWith [(alias, withExp)] sqlSel
      sqlBuilder = toSQL selWith
  runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sqlBuilder) (toList args) True
  where
    alias = S.Alias $ Iden $ snakeCaseTable tn <> "__rel_insert_result"
    frmItemM = Just $ S.FIIden $ toIden alias

insertAndRetCols
  :: QualifiedTable
  -> WithExp
  -> [PGColInfo]
  -> Q.TxE QErr [PGColWithValue]
insertAndRetCols tn withExp retCols = do
  resBS <- execWithExp tn withExp annSelFlds
  resObj <- decodeFromBS resBS
  forM retCols $ \(PGColInfo col colty _) -> do
    val <- onNothing (Map.lookup (getPGColTxt col) resObj) $
      throw500 $ "column " <> col <<> "not returned by postgres"
    pgColVal <- RB.pgValParser colty val
    return (col, pgColVal)
  where
    annSelFlds = flip map retCols $ \pgci ->
      (fromPGCol $ pgiName pgci, RS.FCol pgci)

buildReturningResp
  :: QualifiedTable
  -> [WithExp]
  -> AnnSelFlds
  -> Q.TxE QErr RespBody
buildReturningResp tn withExps annFlds = do
  respList <- forM withExps $ \withExp ->
    execWithExp tn withExp annFlds
  let bsVector = V.fromList respList
  return $ BB.toLazyByteString $ RR.encodeJSONVector BB.lazyByteString bsVector

-- | insert multiple Objects in postgres
insertMultipleObjects
  :: RoleName -- ^ role name
  -> InsCtxMap -- ^ insert context map
  -> QualifiedTable -- ^ table
  -> InsCtx -- ^ insert context
  -> [AnnGObject] -- ^ objects to be inserted
  -> [PGColWithValue] -- ^ additional fields
  -> RR.MutFlds -- ^ returning fields
  -> Maybe AnnGValue -- ^ On Conflict Clause
  -> Bool -- ^ is an Array relation
  -> Q.TxE QErr RespBody
insertMultipleObjects role insCtxMap tn ctx insObjs
                   addCols mutFlds onConflictValM isArrRel
                   = do

  -- fetch insertable columns, object and array relationships
  colsObjArrRels <- withErrPath $ indexedMapM fetchColsAndRels insObjs
  let insCols = map _1 colsObjArrRels
      insColNames = Set.toList $ Set.fromList $
                    concatMap (map _1) insCols
      allInsObjRels = concatMap _2 colsObjArrRels
      allInsArrRels = concatMap _3 colsObjArrRels
      anyRelsToInsert = not $ null allInsArrRels && null allInsObjRels

  onConflictM <- forM onConflictValM $ parseOnConflict insColNames

  let withoutRels = withoutRelsInsert insCols onConflictM

  bool withoutRels withRelsInsert anyRelsToInsert

  where
    InsCtx vn tableColInfos _ = ctx
    tableCols = map pgiName tableColInfos

    errP = bool "objects" "data" isArrRel
    withErrPath = withPathK errP

    -- insert all column rows at one go
    withoutRelsInsert insCols onConflictM = withErrPath $ do
      indexedForM_ insCols $ \insCol ->
        validateInsert (map _1 insCol) [] $ map fst addCols

      let addColsWithType = mkPGColWithTypeAndVal tableColInfos addCols
          withAddCols = flip map insCols $ union addColsWithType

      (sqlRows, prepArgs) <- flip runStateT Seq.Empty $ do
        rowsWithCol <- mapM (toSQLExps . map pgColToAnnGVal) withAddCols
        return $ map (mkSQLRow tableCols) rowsWithCol

      let insQP1 = RI.InsertQueryP1 tn vn tableCols sqlRows onConflictM mutFlds
          p1 = (insQP1, prepArgs)
      bool (RI.nonAdminInsert p1) (RI.insertP2 p1) $ isAdmin role

    -- insert each object with relations
    withRelsInsert = withErrPath $ do
      insResps <- indexedForM insObjs $ \obj ->
          insertObj role insCtxMap tn obj ctx addCols onConflictValM errP

      let affRows = sum $ map fst insResps
          withExps = map snd insResps
      respTups <- forM mutFlds $ \(t, mutFld) -> do
        jsonVal <- case mutFld of
          RR.MCount -> do
            -- when it is a array relation perform insert
            -- and return calculated affected rows
            when isArrRel $ void $ buildReturningResp tn withExps []
            return $ J.toJSON affRows
          RR.MExp txt -> return $ J.toJSON txt
          RR.MRet annSel -> do
            let annFlds = RS._asFields annSel
            bs <- buildReturningResp tn withExps annFlds
            decodeFromBS bs
        return (t, jsonVal)
      return $ J.encode $ Map.fromList respTups

prefixErrPath :: (MonadError QErr m) => Field -> m a -> m a
prefixErrPath fld =
  withPathK "selectionSet" . fieldAsPath fld . withPathK "args"

convertInsert
  :: RoleName
  -> QualifiedTable -- table
  -> Field -- the mutation field
  -> Convert RespTx
convertInsert role tn fld = prefixErrPath fld $ do
  insCtxMap <- getInsCtxMap
  insCtx <- getInsCtx insCtxMap tn
  annVals <- withArg arguments "objects" asArray
  annObjs <- forM annVals asObject
  mutFlds <- convertMutResp tn (_fType fld) $ _fSelSet fld
  return $ prefixErrPath fld $ insertMultipleObjects role insCtxMap tn
    insCtx annObjs [] mutFlds onConflictM False
  where
    arguments = _fArguments fld
    onConflictM = Map.lookup "on_conflict" arguments

-- helper functions
getInsCtxMap
  :: (Has InsCtxMap r, MonadReader r m)
  => m InsCtxMap
getInsCtxMap = asks getter

getInsCtx
  :: MonadError QErr m
  => InsCtxMap -> QualifiedTable -> m InsCtx
getInsCtx ctxMap tn =
  onNothing (Map.lookup tn ctxMap) $ throw500 $ "table " <> tn <<> " not found"

mergeListsWith
  :: [a] -> [b] -> (a -> b -> Bool) -> (a -> b -> c) -> [c]
mergeListsWith _ [] _ _ = []
mergeListsWith [] _ _ _ = []
mergeListsWith (x:xs) l b f = case find (b x) l of
  Nothing -> mergeListsWith xs l b f
  Just y  ->  f x y : mergeListsWith xs l b f

mkPGColWithTypeAndVal :: [PGColInfo] -> [PGColWithValue]
                      -> [(PGCol, PGColType, PGColValue)]
mkPGColWithTypeAndVal pgColInfos pgColWithVal =
    mergeListsWith pgColInfos pgColWithVal
    (\ci (c, _) -> pgiName ci == c)
    (\ci (c, v) -> (c, pgiType ci, v))

pgColToAnnGVal
  :: (PGCol, PGColType, PGColValue)
  -> (PGCol, AnnGValue)
pgColToAnnGVal (col, colTy, colVal) =
  (col, pgColValToAnnGVal colTy colVal)

_1 :: (a, b, c) -> a
_1 (x, _, _) = x

_2 :: (a, b, c) -> b
_2 (_, y, _) = y

_3 :: (a, b, c) -> c
_3 (_, _, z) = z
