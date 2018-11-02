{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Hasura.GraphQL.Resolve.Insert
  (convertInsert)
where

import           Data.Foldable                     (foldrM)
import           Data.Has
import           Data.List                         (intersect, union)
import           Hasura.Prelude
import           Hasura.Server.Utils

import qualified Data.Aeson                        as J
import qualified Data.Aeson.Casing                 as J
import qualified Data.Aeson.TH                     as J
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

newtype InsResp
  = InsResp
  { _irResponse     :: Maybe J.Object
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''InsResp)

data AnnIns a
  = AnnIns
  { _aiInsObj         :: !a
  , _aiConflictClause :: !(Maybe RI.ConflictClauseP1)
  , _aiView           :: !QualifiedTable
  , _aiTableCols      :: ![PGColInfo]
  , _aiDefVals        :: !(Map.HashMap PGCol S.SQLExp)
  } deriving (Show, Eq)

type SingleObjIns = AnnIns AnnInsObj
type MultiObjIns = AnnIns [AnnInsObj]

data RelIns a
  = RelIns
  { _riAnnIns  :: !a
  , _riRelInfo :: !RelInfo
  } deriving (Show, Eq)

type ObjRelIns = RelIns SingleObjIns
type ArrRelIns = RelIns MultiObjIns

type PGColWithValue = (PGCol, PGColValue)

data InsWithExp
  = InsWithExp
  { _iweExp         :: !S.CTE
  , _iweConflictCtx :: !(Maybe RI.ConflictCtx)
  , _iwePrepArgs    :: !(Seq.Seq Q.PrepArg)
  } deriving (Show, Eq)

data AnnInsObj
  = AnnInsObj
  { _aioColumns :: ![(PGCol, PGColType, PGColValue)]
  , _aioObjRels :: ![ObjRelIns]
  , _aioArrRels :: ![ArrRelIns]
  } deriving (Show, Eq)

getAllInsCols :: [AnnInsObj] -> [PGCol]
getAllInsCols =
  Set.toList . Set.fromList . concatMap (map _1 . _aioColumns)

mkAnnInsObj
  :: (MonadError QErr m, Has InsCtxMap r, MonadReader r m)
  => RelationInfoMap
  -> AnnGObject
  -> m AnnInsObj
mkAnnInsObj relInfoMap annObj =
  foldrM (traverseInsObj relInfoMap) emptyInsObj $ OMap.toList annObj
  where
    emptyInsObj = AnnInsObj [] [] []

traverseInsObj
  :: (MonadError QErr m, Has InsCtxMap r, MonadReader r m)
  => RelationInfoMap
  -> (G.Name, AnnGValue)
  -> AnnInsObj
  -> m AnnInsObj
traverseInsObj rim (gName, annVal) (AnnInsObj cols objRels arrRels) =
  case annVal of
    AGScalar colty mColVal -> do
      let col = PGCol $ G.unName gName
          colVal = fromMaybe (PGNull colty) mColVal
      return (AnnInsObj ((col, colty, colVal):cols) objRels arrRels)

    _ -> do
      obj <- asObject annVal
      let relName = RelName $ G.unName gName
          onConflictM = OMap.lookup "on_conflict" obj
      dataVal <- onNothing (OMap.lookup "data" obj) $
                 throw500 "\"data\" object not found"
      relInfo <- onNothing (Map.lookup relName rim) $
                 throw500 $ "relation " <> relName <<> " not found"

      (rtView, rtCols, rtDefVals, rtRelInfoMap) <- resolveInsCtx $ riRTable relInfo

      withPathK (G.unName gName) $ case riType relInfo of
        ObjRel -> do
          dataObj <- asObject dataVal
          annDataObj <- mkAnnInsObj rtRelInfoMap dataObj
          let insCols = getAllInsCols [annDataObj]
          ccM <- forM onConflictM $ parseOnConflict insCols
          let singleObjIns = AnnIns annDataObj ccM rtView rtCols rtDefVals
              objRelIns = RelIns singleObjIns relInfo
          return (AnnInsObj cols (objRelIns:objRels) arrRels)

        ArrRel -> do
          arrDataVals <- asArray dataVal
          annDataObjs <- forM arrDataVals $ \arrDataVal -> do
            dataObj <- asObject arrDataVal
            mkAnnInsObj rtRelInfoMap dataObj
          let insCols = getAllInsCols annDataObjs
          ccM <- forM onConflictM $ parseOnConflict insCols
          let multiObjIns = AnnIns annDataObjs ccM rtView rtCols rtDefVals
              arrRelIns = RelIns multiObjIns relInfo
          return (AnnInsObj cols objRels (arrRelIns:arrRels))

parseOnConflict
  :: (MonadError QErr m)
  => [PGCol] -> AnnGValue -> m RI.ConflictClauseP1
parseOnConflict inpCols val = withPathK "on_conflict" $
  flip withObject val $ \_ obj -> do
    actionM <- forM (OMap.lookup "action" obj) parseAction
    constraint <- parseConstraint obj
    updColsM <- forM (OMap.lookup "update_columns" obj) parseUpdCols
    -- consider "action" if "update_columns" is not mentioned
    return $ mkConflictClause $ case (updColsM, actionM) of
      (Just [], _)             -> RI.CCDoNothing $ Just constraint
      (Just cols, _)           -> RI.CCUpdate constraint cols
      (Nothing, Just CAIgnore) -> RI.CCDoNothing $ Just constraint
      (Nothing, _)             -> RI.CCUpdate constraint inpCols
  where
    parseAction v = do
      (enumTy, enumVal) <- asEnumVal v
      case G.unName $ G.unEnumValue enumVal of
        "ignore" -> return CAIgnore
        "update" -> return CAUpdate
        _ -> throw500 $
          "only \"ignore\" and \"update\" allowed for enum type "
          <> showNamedTy enumTy

    parseConstraint o = do
      v <- onNothing (OMap.lookup "constraint" o) $ throw500
           "\"constraint\" is expected, but not found"
      (_, enumVal) <- asEnumVal v
      return $ ConstraintName $ G.unName $ G.unEnumValue enumVal

    parseUpdCols v = flip withArray v $ \_ enumVals ->
      forM enumVals $ \eVal -> do
        (_, ev) <- asEnumVal eVal
        return $ PGCol $ G.unName $ G.unEnumValue ev

    mkConflictClause (RI.CCDoNothing constrM) =
      RI.CP1DoNothing $ fmap RI.Constraint constrM
    mkConflictClause (RI.CCUpdate constr updCols) =
      RI.CP1Update (RI.Constraint constr) updCols

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

mkInsertQ :: MonadError QErr m => QualifiedTable
          -> Maybe RI.ConflictClauseP1 -> [(PGCol, AnnGValue)]
          -> [PGCol] -> Map.HashMap PGCol S.SQLExp -> RoleName
          -> m InsWithExp
mkInsertQ vn onConflictM insCols tableCols defVals role = do
  (givenCols, args) <- flip runStateT Seq.Empty $ toSQLExps insCols
  let sqlConflict = RI.toSQLConflict <$> onConflictM
      sqlExps = mkSQLRow defVals givenCols
      sqlInsert = S.SQLInsert vn tableCols [sqlExps] sqlConflict $ Just S.returningStar
      adminIns = return $ InsWithExp (S.CTEInsert sqlInsert) Nothing args
      nonAdminInsert = do
        ccM <- mapM RI.extractConflictCtx onConflictM
        let cteIns = S.CTEInsert sqlInsert{S.siConflict=Nothing}
        return $ InsWithExp cteIns ccM args

  bool nonAdminInsert adminIns $ isAdmin role

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

mkSelQ :: MonadError QErr m => QualifiedTable
       -> [PGColInfo] -> [PGColWithValue] -> m InsWithExp
mkSelQ tn allColInfos pgColsWithVal = do
  (whereExp, args) <- flip runStateT Seq.Empty $ mkBoolExp tn colWithInfos
  let sqlSel = S.mkSelect { S.selExtr = [S.selectStar]
                          , S.selFrom = Just $ S.mkSimpleFromExp tn
                          , S.selWhere = Just $ S.WhereFrag $ RG.cBoolExp whereExp
                          }

  return $ InsWithExp (S.CTESelect sqlSel) Nothing args
  where
    colWithInfos = mergeListsWith pgColsWithVal allColInfos
                   (\(c, _) ci -> c == pgiName ci)
                   (\(_, v) ci -> (ci, v))

execWithExp
  :: QualifiedTable
  -> InsWithExp
  -> RR.MutFlds
  -> Q.TxE QErr RespBody
execWithExp tn (InsWithExp withExp ccM args) flds = do
  RI.setConflictCtx ccM
  runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sqlBuilder) (toList args) True
  where
    sqlBuilder = toSQL $ RR.mkSelWith tn withExp flds True


insertAndRetCols
  :: QualifiedTable
  -> InsWithExp
  -> T.Text
  -> [PGColInfo]
  -> Q.TxE QErr [PGColWithValue]
insertAndRetCols tn withExp errMsg retCols = do
  resBS <- execWithExp tn withExp [("response", RR.MRet annSelFlds)]
  insResp <- decodeFromBS resBS
  resObj <- onNothing (_irResponse insResp) $ throwVE errMsg
  forM retCols $ \(PGColInfo col colty _) -> do
    val <- onNothing (Map.lookup (getPGColTxt col) resObj) $
      throw500 $ "column " <> col <<> "not returned by postgres"
    pgColVal <- RB.pgValParser colty val
    return (col, pgColVal)
  where
    annSelFlds = flip map retCols $ \pgci ->
      (fromPGCol $ pgiName pgci, RS.FCol pgci)

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

-- | insert an object relationship and return affected rows
-- | and parent dependent columns
insertObjRel
  :: RoleName
  -> ObjRelIns
  -> Q.TxE QErr (Int, [PGColWithValue])
insertObjRel role objRelIns =
  withPathK relNameTxt $ do
    (aRows, withExp) <- insertObj role tn singleObjIns []
    let errMsg = "cannot proceed to insert object relation "
          <> relName <<> " since insert to table " <> tn <<> " affects zero rows"
    retColsWithVals <- insertAndRetCols tn withExp errMsg $
                       getColInfos rCols allCols
    let c = mergeListsWith mapCols retColsWithVals
          (\(_, rCol) (col, _) -> rCol == col)
          (\(lCol, _) (_, colVal) -> (lCol, colVal))
    return (aRows, c)
  where
    RelIns singleObjIns relInfo = objRelIns
    relName = riName relInfo
    relNameTxt = getRelTxt relName
    mapCols = riMapping relInfo
    tn = riRTable relInfo
    rCols = map snd mapCols
    allCols = _aiTableCols singleObjIns

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

    resBS <- insertMultipleObjects role tn multiObjIns addCols mutFlds "data"
    resObj <- decodeFromBS resBS
    onNothing (Map.lookup ("affected_rows" :: T.Text) resObj) $
      throw500 "affected_rows not returned in array rel insert"
  where
    RelIns multiObjIns relInfo = arrRelIns
    colMapping = riMapping relInfo
    tn = riRTable relInfo
    relNameTxt = getRelTxt $ riName relInfo
    mutFlds = [("affected_rows", RR.MCount)]

-- | insert an object with object and array relationships
insertObj
  :: RoleName
  -> QualifiedTable
  -> SingleObjIns
  -> [PGColWithValue] -- ^ additional fields
  -> Q.TxE QErr (Int, InsWithExp)
insertObj role tn singleObjIns addCols = do
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

  -- prepare insert query as with expression
  insQ <- mkInsertQ vn onConflictM finalInsCols (map pgiName allCols) defVals role

  let preArrRelInsAffRows = objInsAffRows + 1
      insertWithArrRels = withArrRels preArrRelInsAffRows insQ
                            arrDepColsWithInfo
      insertWithoutArrRels = return (preArrRelInsAffRows, insQ)

  -- insert object
  bool insertWithArrRels insertWithoutArrRels $ null arrDepColsWithInfo
  where
    AnnIns annObj onConflictM vn allCols defVals = singleObjIns
    AnnInsObj cols objRels arrRels = annObj

    withArrRels preAffRows insQ arrDepColsWithType = do
      arrDepColsWithVal <-
        insertAndRetCols tn insQ cannotInsArrRelErr arrDepColsWithType

      arrInsARows <- forM arrRels $ insertArrRel role arrDepColsWithVal

      let totalAffRows = preAffRows + sum arrInsARows

      selQ <- mkSelQ tn allCols arrDepColsWithVal
      return (totalAffRows, selQ)

    cannotInsArrRelErr =
      "cannot proceed to insert array relations since insert to table "
      <> tn <<> " affects zero rows"


-- | insert multiple Objects in postgres
insertMultipleObjects
  :: RoleName
  -> QualifiedTable
  -> MultiObjIns
  -> [PGColWithValue] -- ^ additional fields
  -> RR.MutFlds
  -> T.Text -- ^ error path
  -> Q.TxE QErr RespBody
insertMultipleObjects role tn multiObjIns addCols mutFlds errP =
  bool withoutRelsInsert withRelsInsert anyRelsToInsert
  where
    AnnIns insObjs onConflictM vn tableColInfos defVals = multiObjIns
    singleObjInserts = flip map insObjs $ \o ->
                       AnnIns o onConflictM vn tableColInfos defVals
    insCols = map _aioColumns insObjs
    allInsObjRels = concatMap _aioObjRels insObjs
    allInsArrRels = concatMap _aioArrRels insObjs
    anyRelsToInsert = not $ null allInsArrRels && null allInsObjRels

    withErrPath = withPathK errP

    -- insert all column rows at one go
    withoutRelsInsert = withErrPath $ do
      indexedForM_ insCols $ \insCol ->
        validateInsert (map _1 insCol) [] $ map fst addCols

      let addColsWithType = mkPGColWithTypeAndVal tableColInfos addCols
          withAddCols = flip map insCols $ union addColsWithType
          tableCols = map pgiName tableColInfos

      (sqlRows, prepArgs) <- flip runStateT Seq.Empty $ do
        rowsWithCol <- mapM (toSQLExps . map pgColToAnnGVal) withAddCols
        return $ map (mkSQLRow defVals) rowsWithCol

      let insQP1 = RI.InsertQueryP1 tn vn tableCols sqlRows onConflictM mutFlds
          p1 = (insQP1, prepArgs)
      bool (RI.nonAdminInsert p1) (RI.insertP2 p1) $ isAdmin role

    -- insert each object with relations
    withRelsInsert = withErrPath $ do
      insResps <- indexedForM singleObjInserts $ \objIns ->
          insertObj role tn objIns addCols

      let affRows = sum $ map fst insResps
          withExps = map snd insResps
          retFlds = mapMaybe getRet mutFlds
      rawResps <- forM withExps
        $ \withExp -> execWithExp tn withExp retFlds
      respVals :: [J.Object] <- mapM decodeFromBS rawResps
      respTups <- forM mutFlds $ \(t, mutFld) -> do
        jsonVal <- case mutFld of
          RR.MCount   -> return $ J.toJSON affRows
          RR.MExp txt -> return $ J.toJSON txt
          RR.MRet _   -> J.toJSON <$> mapM (fetchVal t) respVals
        return (t, jsonVal)
      return $ J.encode $ OMap.fromList respTups

    getRet (t, r@(RR.MRet _)) = Just (t, r)
    getRet _                  = Nothing

prefixErrPath :: (MonadError QErr m) => Field -> m a -> m a
prefixErrPath fld =
  withPathK "selectionSet" . fieldAsPath fld . withPathK "args"

convertInsert
  :: RoleName
  -> QualifiedTable -- table
  -> Field -- the mutation field
  -> Convert RespTx
convertInsert role tn fld = prefixErrPath fld $ do
  (vn, tableCols, defValMap, relInfoMap) <- resolveInsCtx tn
  annVals <- withArg arguments "objects" asArray
  annObjs <- mapM asObject annVals
  annInsObjs <- forM annObjs $ mkAnnInsObj relInfoMap
  let insCols = getAllInsCols annInsObjs
  conflictClauseM <- forM onConflictM $ parseOnConflict insCols
  mutFlds <- convertMutResp (_fType fld) $ _fSelSet fld
  let multiObjIns = AnnIns annInsObjs conflictClauseM vn tableCols defValMap
  return $ prefixErrPath fld $ insertMultipleObjects role tn
    multiObjIns [] mutFlds "objects"
  where
    arguments = _fArguments fld
    onConflictM = Map.lookup "on_conflict" arguments

-- helper functions
resolveInsCtx
  :: (MonadError QErr m, MonadReader r m, Has InsCtxMap r)
  => QualifiedTable
  -> m ( QualifiedTable
       , [PGColInfo]
       , Map.HashMap PGCol S.SQLExp
       , RelationInfoMap
       )
resolveInsCtx tn = do
  ctxMap <- asks getter
  InsCtx view colInfos setVals relInfoMap <-
    onNothing (Map.lookup tn ctxMap) $
    throw500 $ "table " <> tn <<> " not found"
  let defValMap = S.mkColDefValMap $ map pgiName colInfos
      defValWithSet = Map.union setVals defValMap
  return (view, colInfos, defValWithSet, relInfoMap)

fetchVal :: (MonadError QErr m)
  => T.Text -> Map.HashMap T.Text a -> m a
fetchVal t m = onNothing (Map.lookup t m) $ throw500 $
  "key " <> t <> " not found in hashmap"

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
pgColToAnnGVal (col, colTy, colVal) = (col, pgColValToAnnGVal colTy colVal)
