module Hasura.GraphQL.Resolve.Insert
  (convertInsert)
where

import           Data.Has
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.Server.Utils

import qualified Data.Aeson                        as J
import qualified Data.Aeson.Casing                 as J
import qualified Data.Aeson.TH                     as J
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Database.PG.Query                 as Q
import qualified Hasura.RQL.DML.Insert             as RI
import qualified Hasura.RQL.DML.Returning          as RR
import qualified Hasura.RQL.GBoolExp               as RB

import qualified Hasura.SQL.DML                    as S

import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Resolve.Mutation
import           Hasura.GraphQL.Resolve.Select
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal           (dmlTxErrorHandler)
import           Hasura.RQL.DML.Mutation
import           Hasura.RQL.GBoolExp               (toSQLBoolExp)
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
  , _aiUniqCols       :: !(Maybe [PGColInfo])
  } deriving (Show, Eq, Functor, Foldable, Traversable)

type SingleObjIns = AnnIns AnnInsObj
type MultiObjIns = AnnIns [AnnInsObj]

singleToMulti :: SingleObjIns -> MultiObjIns
singleToMulti = fmap pure

multiToSingles :: MultiObjIns -> [SingleObjIns]
multiToSingles = sequenceA

data RelIns a
  = RelIns
  { _riAnnIns  :: !a
  , _riRelInfo :: !RelInfo
  } deriving (Show, Eq)

type ObjRelIns = RelIns SingleObjIns
type ArrRelIns = RelIns MultiObjIns

type PGColWithValue = (PGCol, PGColValue)

data CTEExp
  = CTEExp
  { _iweExp      :: !S.CTE
  , _iwePrepArgs :: !(Seq.Seq Q.PrepArg)
  } deriving (Show, Eq)

data AnnInsObj
  = AnnInsObj
  { _aioColumns :: ![(PGCol, PGColType, PGColValue)]
  , _aioObjRels :: ![ObjRelIns]
  , _aioArrRels :: ![ArrRelIns]
  } deriving (Show, Eq)

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
traverseInsObj rim (gName, annVal) defVal@(AnnInsObj cols objRels arrRels) =
  case annVal of
    AGScalar colty mColVal -> do
      let col = PGCol $ G.unName gName
          colVal = fromMaybe (PGNull colty) mColVal
      return (AnnInsObj ((col, colty, colVal):cols) objRels arrRels)

    _ -> do
      objM <- asObjectM annVal
      -- if relational insert input is 'null' then ignore
      -- return default value
      fmap (fromMaybe defVal) $ forM objM $ \obj -> do
        let relName = RelName $ G.unName gName
            onConflictM = OMap.lookup "on_conflict" obj
        dataVal <- onNothing (OMap.lookup "data" obj) $
                   throw500 "\"data\" object not found"
        relInfo <- onNothing (Map.lookup relName rim) $
                   throw500 $ "relation " <> relName <<> " not found"

        let rTable = riRTable relInfo
        InsCtx rtView rtCols rtDefVals rtRelInfoMap rtUpdPerm rtUniqCols <- getInsCtx rTable

        withPathK (G.unName gName) $ case riType relInfo of
          ObjRel -> do
            dataObj <- asObject dataVal
            annDataObj <- mkAnnInsObj rtRelInfoMap dataObj
            ccM <- forM onConflictM $ parseOnConflict rTable rtUpdPerm
            let singleObjIns = AnnIns annDataObj ccM rtView rtCols rtDefVals rtUniqCols
                objRelIns = RelIns singleObjIns relInfo
            return (AnnInsObj cols (objRelIns:objRels) arrRels)

          ArrRel -> do
            arrDataVals <- asArray dataVal
            let withNonEmptyArrData = do
                  annDataObjs <- forM arrDataVals $ \arrDataVal -> do
                    dataObj <- asObject arrDataVal
                    mkAnnInsObj rtRelInfoMap dataObj
                  ccM <- forM onConflictM $ parseOnConflict rTable rtUpdPerm
                  let multiObjIns = AnnIns annDataObjs ccM rtView rtCols rtDefVals rtUniqCols
                      arrRelIns = RelIns multiObjIns relInfo
                  return (AnnInsObj cols objRels (arrRelIns:arrRels))
            -- if array relation insert input data has empty objects
            -- then ignore and return default value
            bool withNonEmptyArrData (return defVal) $ null arrDataVals

parseOnConflict
  :: (MonadError QErr m)
  => QualifiedTable -> Maybe UpdPermForIns
  -> AnnGValue -> m RI.ConflictClauseP1
parseOnConflict tn updPermM val = withPathK "on_conflict" $
  flip withObject val $ \_ obj -> do
    constraint <- RI.Constraint <$> parseConstraint obj
    updCols <- getUpdCols obj
    case updCols of
      [] -> return $ RI.CP1DoNothing $ Just constraint
      _  -> do
          UpdPermForIns _ updFiltr preSet <- onNothing updPermM $ throw500
            "cannot update columns since update permission is not defined"
          return $ RI.CP1Update constraint updCols preSet $
            toSQLBoolExp (S.mkQual tn) updFiltr

  where
    getUpdCols o = do
      updColsVal <- onNothing (OMap.lookup "update_columns" o) $ throw500
        "\"update_columns\" argument in expected in \"on_conflict\" field "
      parseColumns updColsVal

    parseConstraint o = do
      v <- onNothing (OMap.lookup "constraint" o) $ throw500
           "\"constraint\" is expected, but not found"
      (_, enumVal) <- asEnumVal v
      return $ ConstraintName $ G.unName $ G.unEnumValue enumVal

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
          -> m (CTEExp, Maybe RI.ConflictCtx)
mkInsertQ vn onConflictM insCols tableCols defVals role = do
  (givenCols, args) <- flip runStateT Seq.Empty $ toSQLExps insCols
  let sqlConflict = RI.toSQLConflict <$> onConflictM
      sqlExps = mkSQLRow defVals givenCols
      sqlInsert = S.SQLInsert vn tableCols [sqlExps] sqlConflict $ Just S.returningStar
      adminIns = return (CTEExp (S.CTEInsert sqlInsert) args, Nothing)
      nonAdminInsert = do
        ccM <- mapM RI.extractConflictCtx onConflictM
        let cteIns = S.CTEInsert sqlInsert{S.siConflict=Nothing}
        return (CTEExp cteIns args, ccM)

  bool nonAdminInsert adminIns $ isAdmin role

mkBoolExp
  :: (MonadError QErr m, MonadState PrepArgs m)
  => QualifiedTable -> [(PGColInfo, PGColValue)]
  -> m S.BoolExp
mkBoolExp _ []           = return $ S.BELit False
mkBoolExp tn colInfoVals =
  RB.toSQLBoolExp (S.mkQual tn) . BoolAnd <$>
  mapM (fmap BoolFld . uncurry f) colInfoVals
  where
    f ci@(PGColInfo _ colTy _) colVal =
      AVCol ci . pure . AEQ True <$> prepare (colTy, colVal)

asSingleObject
  :: MonadError QErr m
  => [ColVals] -> m (Maybe ColVals)
asSingleObject = \case
  []  -> return Nothing
  [a] -> return $ Just a
  _   -> throw500 "more than one row returned"

fetchFromColVals
  :: MonadError QErr m
  => ColVals
  -> [PGColInfo]
  -> (PGColInfo -> a)
  -> m [(a, PGColValue)]
fetchFromColVals colVal reqCols f =
  forM reqCols $ \ci -> do
    let valM = Map.lookup (pgiName ci) colVal
    val <- onNothing valM $ throw500 $ "column "
           <> pgiName ci <<> " not found in given colVal"
    pgColVal <- RB.pgValParser (pgiType ci) val
    return (f ci, pgColVal)

mkSelCTE
  :: MonadError QErr m
  => QualifiedTable
  -> [PGColInfo]
  -> Maybe ColVals
  -> m CTEExp
mkSelCTE tn uniqCols colValM = do
  (whereExp, args) <- case colValM of
    Nothing -> return (S.BELit False, Seq.empty)
    Just colVal -> do
      colInfoWithVals <- fetchFromColVals colVal uniqCols id
      flip runStateT Seq.Empty $ mkBoolExp tn colInfoWithVals
  let sqlSel = S.mkSelect { S.selExtr = [S.selectStar]
                          , S.selFrom = Just $ S.mkSimpleFromExp tn
                          , S.selWhere = Just $ S.WhereFrag whereExp
                          }

  return $ CTEExp (S.CTESelect sqlSel) args

execCTEExp
  :: Bool
  -> QualifiedTable
  -> CTEExp
  -> RR.MutFlds
  -> Q.TxE QErr J.Object
execCTEExp strfyNum tn (CTEExp cteExp args) flds =
  Q.getAltJ . runIdentity . Q.getRow
    <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sqlBuilder) (toList args) True
  where
    sqlBuilder = toSQL $ RR.mkSelWith tn cteExp flds True strfyNum

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
    "cannot insert " <> showPGCols insConflictCols
    <> " columns as their values are already being determined by parent insert"

  forM_ objRels $ \relInfo -> do
    let lCols = map fst $ riMapping relInfo
        relName = riName relInfo
        relNameTxt = getRelTxt relName
        lColConflicts = lCols `intersect` (addCols <> insCols)
    withPathK relNameTxt $ unless (null lColConflicts) $ throwVE $
      "cannot insert object relation ship " <> relName
      <<> " as " <> showPGCols lColConflicts
      <> " column values are already determined"
  where
    insConflictCols = insCols `intersect` addCols

-- | insert an object relationship and return affected rows
-- | and parent dependent columns
insertObjRel
  :: Bool
  -> RoleName
  -> ObjRelIns
  -> Q.TxE QErr (Int, [PGColWithValue])
insertObjRel strfyNum role objRelIns =
  withPathK relNameTxt $ do
    resp <- insertMultipleObjects strfyNum role tn multiObjIns [] mutFlds "data"
    MutateResp aRows colVals <- decodeEncJSON resp
    colValM <- asSingleObject colVals
    colVal <- onNothing colValM $ throw400 NotSupported errMsg
    retColsWithVals <- fetchFromColVals colVal rColInfos pgiName
    let c = mergeListsWith mapCols retColsWithVals
          (\(_, rCol) (col, _) -> rCol == col)
          (\(lCol, _) (_, cVal) -> (lCol, cVal))
    return (aRows, c)
  where
    RelIns singleObjIns relInfo = objRelIns
    multiObjIns = singleToMulti singleObjIns
    relName = riName relInfo
    relNameTxt = getRelTxt relName
    mapCols = riMapping relInfo
    tn = riRTable relInfo
    allCols = _aiTableCols singleObjIns
    rCols = map snd mapCols
    rColInfos = getColInfos rCols allCols
    errMsg = "cannot proceed to insert object relation "
             <> relName <<> " since insert to table "
             <> tn <<> " affects zero rows"
    mutFlds = [ ("affected_rows", RR.MCount)
              , ( "returning_columns"
                , RR.MRet $ RR.pgColsToSelFlds rColInfos
                )
              ]

decodeEncJSON :: (J.FromJSON a, QErrM m) => EncJSON -> m a
decodeEncJSON =
  either (throw500 . T.pack) decodeValue .
  J.eitherDecode . encJToLBS

-- | insert an array relationship and return affected rows
insertArrRel
  :: Bool
  -> RoleName
  -> [PGColWithValue]
  -> ArrRelIns
  -> Q.TxE QErr Int
insertArrRel strfyNum role resCols arrRelIns =
    withPathK relNameTxt $ do
    let addCols = mergeListsWith resCols colMapping
               (\(col, _) (lCol, _) -> col == lCol)
               (\(_, colVal) (_, rCol) -> (rCol, colVal))

    resBS <- insertMultipleObjects strfyNum role tn multiObjIns addCols mutFlds "data"
    resObj <- decodeEncJSON resBS
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
  :: Bool
  -> RoleName
  -> QualifiedTable
  -> SingleObjIns
  -> [PGColWithValue] -- ^ additional fields
  -> Q.TxE QErr (Int, CTEExp)
insertObj strfyNum role tn singleObjIns addCols = do
  -- validate insert
  validateInsert (map _1 cols) (map _riRelInfo objRels) $ map fst addCols

  -- insert all object relations and fetch this insert dependent column values
  objInsRes <- forM objRels $ insertObjRel strfyNum role

  -- prepare final insert columns
  let objRelAffRows = sum $ map fst objInsRes
      objRelDeterminedCols = concatMap snd objInsRes
      objRelInsCols = mkPGColWithTypeAndVal allCols objRelDeterminedCols
      addInsCols = mkPGColWithTypeAndVal allCols addCols
      finalInsCols =  map pgColToAnnGVal (cols <> objRelInsCols <> addInsCols)

  -- prepare final returning columns
  let arrDepCols = concatMap (map fst . riMapping . _riRelInfo) arrRels
      arrDepColsWithInfo = getColInfos arrDepCols allCols

  -- prepare insert query as with expression
  (CTEExp cte insPArgs, ccM) <-
    mkInsertQ vn onConflictM finalInsCols (map pgiName allCols) defVals role
  uniqCols <- onNothing uniqColsM $ throw500 "unique columns not found in relational insert"

  RI.setConflictCtx ccM
  MutateResp affRows colVals <-
    mutateAndFetchCols tn (uniqCols `union` arrDepColsWithInfo) (cte, insPArgs) strfyNum
  colValM <- asSingleObject colVals
  cteExp <- mkSelCTE tn uniqCols colValM

  arrRelAffRows <- bool (withArrRels arrDepColsWithInfo colValM) (return 0) $ null arrRels
  let totAffRows = objRelAffRows + affRows + arrRelAffRows

  return (totAffRows, cteExp)
  where
    AnnIns annObj onConflictM vn allCols defVals uniqColsM = singleObjIns
    AnnInsObj cols objRels arrRels = annObj

    withArrRels arrDepCols colValM = do
      colVal <- onNothing colValM $ throw400 NotSupported cannotInsArrRelErr
      arrDepColsWithVal <- fetchFromColVals colVal arrDepCols pgiName

      arrInsARows <- forM arrRels $ insertArrRel strfyNum role arrDepColsWithVal

      return $ sum arrInsARows

    cannotInsArrRelErr =
      "cannot proceed to insert array relations since insert to table "
      <> tn <<> " affects zero rows"


-- | insert multiple Objects in postgres
insertMultipleObjects
  :: Bool
  -> RoleName
  -> QualifiedTable
  -> MultiObjIns
  -> [PGColWithValue] -- ^ additional fields
  -> RR.MutFlds
  -> T.Text -- ^ error path
  -> Q.TxE QErr EncJSON
insertMultipleObjects strfyNum role tn multiObjIns addCols mutFlds errP =
  bool withoutRelsInsert withRelsInsert anyRelsToInsert
  where
    AnnIns insObjs onConflictM vn tableColInfos defVals uniqCols = multiObjIns
    singleObjInserts = multiToSingles multiObjIns
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

      let insQP1 = RI.InsertQueryP1 tn vn tableCols sqlRows onConflictM mutFlds uniqCols
          p1 = (insQP1, prepArgs)
      bool (RI.nonAdminInsert strfyNum p1) (RI.insertP2 strfyNum p1) $ isAdmin role

    -- insert each object with relations
    withRelsInsert = withErrPath $ do
      insResps <- indexedForM singleObjInserts $ \objIns ->
          insertObj strfyNum role tn objIns addCols

      let affRows = sum $ map fst insResps
          cteExps = map snd insResps
          retFlds = mapMaybe getRet mutFlds
      respVals <- forM cteExps $ \cteExp ->
        execCTEExp strfyNum tn cteExp retFlds
      respTups <- forM mutFlds $ \(t, mutFld) -> do
        jsonVal <- case mutFld of
          RR.MCount   -> return $ J.toJSON affRows
          RR.MExp txt -> return $ J.toJSON txt
          RR.MRet _   -> J.toJSON <$> mapM (fetchVal t) respVals
        return (t, jsonVal)
      return $ encJFromJValue $ OMap.fromList respTups

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
  mutFlds <- convertMutResp (_fType fld) $ _fSelSet fld
  annVals <- withArg arguments "objects" asArray
  -- if insert input objects is empty array then
  -- do not perform insert and return mutation response
  bool (withNonEmptyObjs annVals mutFlds) (withEmptyObjs mutFlds) $ null annVals
  where
    withNonEmptyObjs annVals mutFlds = do
      InsCtx vn tableCols defValMap relInfoMap updPerm uniqCols <- getInsCtx tn
      annObjs <- mapM asObject annVals
      annInsObjs <- forM annObjs $ mkAnnInsObj relInfoMap
      conflictClauseM <- forM onConflictM $ parseOnConflict tn updPerm
      let multiObjIns = AnnIns annInsObjs conflictClauseM vn tableCols defValMap uniqCols
      strfyNum <- stringifyNum <$> asks getter
      return $ prefixErrPath fld $ insertMultipleObjects strfyNum role tn
        multiObjIns [] mutFlds "objects"
    withEmptyObjs mutFlds =
      return $ return $ buildEmptyMutResp mutFlds
    arguments = _fArguments fld
    onConflictM = Map.lookup "on_conflict" arguments

-- helper functions
getInsCtx
  :: (MonadError QErr m, MonadReader r m, Has InsCtxMap r)
  => QualifiedTable -> m InsCtx
getInsCtx tn = do
  ctxMap <- asks getter
  insCtx <- onNothing (Map.lookup tn ctxMap) $
    throw500 $ "table " <> tn <<> " not found"
  let defValMap = S.mkColDefValMap $ map pgiName $ icColumns insCtx
      setCols = icSet insCtx
  return $ insCtx {icSet = Map.union setCols defValMap}

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
