{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Hasura.GraphQL.Resolve.Insert
  (convertInsert)
where

import           Data.Foldable                     (foldrM)
import           Data.Has
import           Data.List                         (intersect, union)
import           Hasura.Prelude

import qualified Data.Aeson                        as J
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Data.HashSet                      as Set
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T
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

data RelIns a
  = RelIns
  { _riInsObj         :: a
  , _riRelInfo        :: !RelInfo
  , _riConflictClause :: !(Maybe RI.ConflictClauseP1)
  } deriving (Show, Eq)

type ObjRelIns = RelIns AnnInsObj
type ArrRelIns = RelIns [AnnInsObj]

type PGColWithValue = (PGCol, PGColValue)

type AnnSelFlds = [(FieldName, RS.AnnFld)]
type WithExp = (S.CTE, Seq.Seq Q.PrepArg)

data AnnInsObj
  = AnnInsObj
  { _aioColumns   :: ![(PGCol, PGColType, PGColValue)]
  , _aioObjRels   :: ![ObjRelIns]
  , _aioArrRels   :: ![ArrRelIns]
  , _aioView      :: !QualifiedTable
  , _aioTableCols :: ![PGColInfo]
  , _aioDefVals   :: !(Map.HashMap PGCol S.SQLExp)
  } deriving (Show, Eq)

type InsItems = ([(PGCol, PGColType, PGColValue)], [ObjRelIns], [ArrRelIns])

mkAnnInsObj
  :: (MonadError QErr m)
  => QualifiedTable
  -> InsCtxMap
  -> AnnGObject
  -> m AnnInsObj
mkAnnInsObj tn insCtxMap annObj = do
  InsCtx view colInfos relInfoMap <- getInsCtx insCtxMap tn
  let defValMap = Map.fromList $ flip zip (repeat $ S.SEUnsafe "DEFAULT") $
                  map pgiName colInfos
  (cols, objRels, arrRels) <-
    foldrM (traverseInsObj insCtxMap relInfoMap) emptyInsItems $ OMap.toList annObj
  return $ AnnInsObj cols objRels arrRels view colInfos defValMap
  where
    emptyInsItems = ([], [], [])

traverseInsObj
  :: MonadError QErr m
  => InsCtxMap
  -> RelationInfoMap
  -> (G.Name, AnnGValue)
  -> InsItems
  -> m InsItems
traverseInsObj insCtxMap rim (gName, annVal) (cols, objRels, arrRels) =
  case annVal of
    AGScalar colty mColVal -> do
      let col = PGCol $ G.unName gName
          colVal = fromMaybe (PGNull colty) mColVal
      return ((col, colty, colVal):cols, objRels, arrRels)

    _ -> do
      obj <- asObject annVal
      let relName = RelName $ G.unName gName
          confClaM = OMap.lookup "on_conflict" obj
      dataVal <- onNothing (OMap.lookup "data" obj) $
        throw500 "\"data\" object not found"
      relInfo <- onNothing (Map.lookup relName rim) $
        throw500 $ "relation " <> relName <<> " not found"
      case riType relInfo of
        ObjRel -> do
          dataObj <- asObject dataVal
          annDataObj <- mkAnnInsObj (riRTable relInfo) insCtxMap dataObj
          ccM <- forM confClaM $ parseConflictClause [annDataObj]
          let objRelIns = RelIns annDataObj relInfo ccM
          return (cols, objRelIns:objRels, arrRels)
        ArrRel -> do
          arrDataVals <- asArray dataVal
          annDataObjs <- forM arrDataVals $ \arrDataVal -> do
            dataObj <- asObject arrDataVal
            mkAnnInsObj (riRTable relInfo) insCtxMap dataObj
          ccM <- forM confClaM $ parseConflictClause annDataObjs
          let arrRelIns = RelIns annDataObjs relInfo ccM
          return (cols, objRels, arrRelIns:arrRels)

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

toSQLExps :: (MonadError QErr m, MonadState PrepArgs m)
     => [(PGCol, AnnGValue)] -> m [(PGCol, S.SQLExp)]
toSQLExps cols =
  forM cols $ \(c, v) -> do
    prepExpM <- asPGColValM v >>= mapM prepare
    let prepExp = fromMaybe (S.SEUnsafe "NULL") prepExpM
    return (c, prepExp)

mkSQLRow :: Map.HashMap PGCol S.SQLExp -> [(PGCol, S.SQLExp)] -> [S.SQLExp]
mkSQLRow defVals withPGCol =
  Map.elems $ Map.union (Map.fromList withPGCol) defVals

mkInsertQ :: QualifiedTable
          -> Maybe RI.ConflictClauseP1 -> [(PGCol, AnnGValue)]
          -> [PGCol] -> Map.HashMap PGCol S.SQLExp -> RoleName
          -> Q.TxE QErr WithExp
mkInsertQ vn onConflictM insCols tableCols defVals role = do
  (givenCols, args) <- flip runStateT Seq.Empty $ toSQLExps insCols
  let sqlConflict = RI.toSQLConflict <$> onConflictM
      sqlExps = mkSQLRow defVals givenCols
      sqlInsert = S.SQLInsert vn tableCols [sqlExps] sqlConflict $ Just S.returningStar
  if isAdmin role then return (S.CTEInsert sqlInsert, args)
  else do
    ccM <- mapM RI.extractConflictCtx onConflictM
    RI.setConflictCtx ccM
    return (S.CTEInsert (sqlInsert{S.siConflict=Nothing}), args)

-- | insert an object relationship and return affected rows
-- | and parent dependent columns
insertObjRel
  :: RoleName
  -> ObjRelIns
  -> Q.TxE QErr (Int, [PGColWithValue])
insertObjRel role objRelIns =
  withPathK relNameTxt $ do
    (aRows, withExp) <- insertObj role tn insObj onConflictM [] "data"
    when (aRows == 0) $ throwVE $ "cannot proceed to insert object relation "
      <> relName <<> " since insert to table " <> tn <<> " affects zero rows"
    retColsWithVals <- insertAndRetCols tn withExp $
                       getColInfos rCols allCols
    let c = mergeListsWith mapCols retColsWithVals
          (\(_, rCol) (col, _) -> rCol == col)
          (\(lCol, _) (_, colVal) -> (lCol, colVal))
    return (aRows, c)
  where
    RelIns insObj relInfo onConflictM = objRelIns
    relName = riName relInfo
    relNameTxt = getRelTxt relName
    mapCols = riMapping relInfo
    tn = riRTable relInfo
    rCols = map snd mapCols
    allCols = _aioTableCols insObj

-- | insert an array relationship and return affected rows
insertArrRel
  :: RoleName
  -> [PGColWithValue]
  -> ArrRelIns
  -> Q.TxE QErr Int
insertArrRel role resCols arrRelIns =
    withPathK relNameTxt $ do
    let addCols = mergeListsWith resCols colMapping
               (\(col, _) (lCol, _) -> col == lCol)
               (\(_, colVal) (_, rCol) -> (rCol, colVal))

    resBS <- insertMultipleObjects role tn insObjs onConflicM addCols mutFlds "data"
    resObj <- decodeFromBS resBS
    onNothing (Map.lookup ("affected_rows" :: T.Text) resObj) $
      throw500 "affected_rows not returned in array rel insert"
  where
    RelIns insObjs relInfo onConflicM = arrRelIns
    colMapping = riMapping relInfo
    tn = riRTable relInfo
    relNameTxt = getRelTxt $ riName relInfo
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
  -> QualifiedTable
  -> AnnInsObj -- ^ object to be inserted
  -> Maybe RI.ConflictClauseP1
  -> [PGColWithValue] -- ^ additional fields
  -> T.Text -- ^ error path
  -> Q.TxE QErr (Int, WithExp)
insertObj role tn annObj onConflictM addCols errP = do
  -- validate insert
  validateInsert (map _1 cols) (map _riRelInfo objRels) $ map fst addCols

  -- insert all object relations and fetch this insert dependent column values
  objInsRes <- forM objRels $ insertObjRel role

  -- prepare final insert columns
  let objInsAffRows = sum $ map fst objInsRes
      objRelDeterminedCols = concatMap snd objInsRes
      objRelInsCols = mkPGColWithTypeAndVal allCols objRelDeterminedCols
      addInsCols = mkPGColWithTypeAndVal allCols addCols
      finalInsCols =  map pgColToAnnGVal (cols <> objRelInsCols <> addInsCols)

  -- prepare final returning columns
  let arrDepCols = concatMap (map fst . riMapping . _riRelInfo) arrRels
      arrDepColsWithInfo = getColInfos arrDepCols allCols

  -- calculate affected rows
  let anyRowsAffected = not $ or $ fmap RI.isDoNothing onConflictM
      thisInsAffRows = bool 0 1 anyRowsAffected
      preArrRelInsAffRows = objInsAffRows + thisInsAffRows

  -- prepare insert query as with expression
  insQ <- mkInsertQ vn onConflictM finalInsCols (map pgiName allCols) defVals role

  let insertWithArrRels = cannotInsArrRelErr thisInsAffRows >>
                          withArrRels preArrRelInsAffRows insQ
                            arrDepColsWithInfo
      insertWithoutArrRels = withNoArrRels preArrRelInsAffRows insQ

  bool insertWithArrRels insertWithoutArrRels $ null arrDepColsWithInfo

  where
    AnnInsObj cols objRels arrRels vn allCols defVals = annObj

    withNoArrRels affRows insQ = return (affRows, insQ)

    withArrRels affRows insQ arrDepColsWithType = do
      arrDepColsWithVal <- insertAndRetCols tn insQ arrDepColsWithType

      arrInsARows <- forM arrRels $ insertArrRel role arrDepColsWithVal

      let totalAffRows = affRows + sum arrInsARows

      selQ <- mkSelQ tn allCols arrDepColsWithVal
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
  -> [(T.Text, AnnSelFlds)]
  -> Q.TxE QErr RespBody
execWithExp tn (withExp, args) selFlds =
  runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sqlBuilder) (toList args) True
  where
    alias = S.Alias $ Iden $ snakeCaseTable tn <> "__rel_insert_result"
    frmItemM = Just $ S.FIIden $ toIden alias

    sqlBuilder = toSQL selWith
    selWith = S.SelectWith [(alias, withExp)] sel
    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing]}
    extrExp = S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing

    mkSel annFlds = RS.mkSQLSelect True $
      RS.AnnSel annFlds tn frmItemM (S.BELit True) Nothing RS.noTableArgs
    jsonBuildObjArgs =
      flip concatMap selFlds $
      \(k, annFlds) -> [S.SELit k, S.SESelect $ mkSel annFlds]

insertAndRetCols
  :: QualifiedTable
  -> WithExp
  -> [PGColInfo]
  -> Q.TxE QErr [PGColWithValue]
insertAndRetCols tn withExp retCols = do
  resBS <- execWithExp tn withExp [("response", annSelFlds)]
  resObjRaw <- decodeFromBS resBS
  resObj <- fetchVal "response" resObjRaw
  forM retCols $ \(PGColInfo col colty _) -> do
    val <- onNothing (Map.lookup (getPGColTxt col) resObj) $
      throw500 $ "column " <> col <<> "not returned by postgres"
    pgColVal <- RB.pgValParser colty val
    return (col, pgColVal)
  where
    annSelFlds = flip map retCols $ \pgci ->
      (fromPGCol $ pgiName pgci, RS.FCol pgci)


parseConflictClause
  :: (MonadError QErr m)
  => [AnnInsObj]
  -> AnnGValue
  -> m RI.ConflictClauseP1
parseConflictClause annInsObjs =
  parseOnConflict insCols
  where
    insCols = Set.toList $ Set.fromList $
      concatMap (map _1 . _aioColumns) annInsObjs

-- | insert multiple Objects in postgres
insertMultipleObjects
  :: RoleName -- ^ role name
  -> QualifiedTable -- ^ table
  -> [AnnInsObj] -- ^ objects to be inserted
  -> Maybe RI.ConflictClauseP1
  -> [PGColWithValue] -- ^ additional fields
  -> RR.MutFlds -- ^ returning fields
  -> T.Text -- ^ error path
  -> Q.TxE QErr RespBody
insertMultipleObjects role tn insObjs onConflictM addCols mutFlds errP =

  bool withoutRelsInsert withRelsInsert anyRelsToInsert

  where
    insCols = map _aioColumns insObjs
    allInsObjRels = concatMap _aioObjRels insObjs
    allInsArrRels = concatMap _aioArrRels insObjs
    tableColInfos = concatMap _aioTableCols insObjs
    anyRelsToInsert = not $ null allInsArrRels && null allInsObjRels

    tableCols = map pgiName tableColInfos

    withErrPath = withPathK errP

    -- insert all column rows at one go
    withoutRelsInsert = withErrPath $ do
      indexedForM_ insCols $ \insCol ->
        validateInsert (map _1 insCol) [] $ map fst addCols

      let addColsWithType = mkPGColWithTypeAndVal tableColInfos addCols
          withAddCols = flip map insCols $ union addColsWithType

      (vn, defVals) <- case insObjs of
        []  -> throwVE "insert objects cannot be empty"
        x:_ -> return (_aioView x, _aioDefVals x)

      (sqlRows, prepArgs) <- flip runStateT Seq.Empty $ do
        rowsWithCol <- mapM (toSQLExps . map pgColToAnnGVal) withAddCols
        return $ map (mkSQLRow defVals) rowsWithCol

      let insQP1 = RI.InsertQueryP1 tn vn tableCols sqlRows onConflictM mutFlds
          p1 = (insQP1, prepArgs)
      bool (RI.nonAdminInsert p1) (RI.insertP2 p1) $ isAdmin role

    -- insert each object with relations
    withRelsInsert = withErrPath $ do
      insResps <- indexedForM insObjs $ \obj ->
          insertObj role tn obj onConflictM addCols errP

      let affRows = sum $ map fst insResps
          withExps = map snd insResps
          retFlds = mapMaybe getRet mutFlds
      rawResps <- forM withExps
        $ \withExp -> execWithExp tn withExp retFlds
      respVals :: [J.Object] <- mapM decodeFromBS rawResps
      respTups <- forM mutFlds $ \(t, mutFld) -> do
        jsonVal <- case mutFld of
          RR.MCount ->
            return $ J.toJSON affRows
          RR.MExp txt -> return $ J.toJSON txt
          RR.MRet _ -> J.toJSON <$> mapM (fetchVal t) respVals
        return (t, jsonVal)
      return $ J.encode $ OMap.fromList respTups

    getRet (t, RR.MRet annSel) = Just (t, RS._asFields annSel)
    getRet _                   = Nothing

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
  annVals <- withArg arguments "objects" asArray
  annObjs <- forM annVals asObject
  annInsObjs <- forM annObjs $ mkAnnInsObj tn insCtxMap
  conflictClauseM <- forM onConflictM $ parseConflictClause annInsObjs
  mutFlds <- convertMutResp tn (_fType fld) $ _fSelSet fld
  return $ prefixErrPath fld $ insertMultipleObjects role tn
    annInsObjs conflictClauseM [] mutFlds "objects"
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

fetchVal :: (MonadError QErr m)
  => T.Text -> Map.HashMap T.Text a -> m a
fetchVal t m = onNothing (Map.lookup t m) $ throw500 $
  "key " <> t <> " not found in hashmap"

_1 :: (a, b, c) -> a
_1 (x, _, _) = x

_2 :: (a, b, c) -> b
_2 (_, y, _) = y

_3 :: (a, b, c) -> c
_3 (_, _, z) = z
