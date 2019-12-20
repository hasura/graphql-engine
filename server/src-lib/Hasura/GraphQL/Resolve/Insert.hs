module Hasura.GraphQL.Resolve.Insert
  (convertInsert)
where

import           Control.Arrow                     ((>>>))
import           Data.Has
import           Hasura.EncJSON
import           Hasura.Prelude

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

import qualified Hasura.SQL.DML                    as S

import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Resolve.Mutation
import           Hasura.GraphQL.Resolve.Select
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal           (convPartialSQLExp,
                                                    dmlTxErrorHandler,
                                                    sessVarFromCurrentSetting)
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
  , _aiTableCols      :: ![PGColumnInfo]
  , _aiDefVals        :: !(Map.HashMap PGCol S.SQLExp)
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

type PGColWithValue = (PGCol, WithScalarType PGScalarValue)

data CTEExp
  = CTEExp
  { _iweExp      :: !S.CTE
  , _iwePrepArgs :: !(Seq.Seq Q.PrepArg)
  } deriving (Show, Eq)

data AnnInsObj
  = AnnInsObj
  { _aioColumns :: ![PGColWithValue]
  , _aioObjRels :: ![ObjRelIns]
  , _aioArrRels :: ![ArrRelIns]
  } deriving (Show, Eq)

mkAnnInsObj
  :: (MonadReusability m, MonadError QErr m, Has InsCtxMap r, MonadReader r m, Has FieldMap r)
  => RelationInfoMap
  -> PGColGNameMap
  -> AnnGObject
  -> m AnnInsObj
mkAnnInsObj relInfoMap allColMap annObj =
  foldrM (traverseInsObj relInfoMap allColMap) emptyInsObj $ OMap.toList annObj
  where
    emptyInsObj = AnnInsObj [] [] []

traverseInsObj
  :: (MonadReusability m, MonadError QErr m, Has InsCtxMap r, MonadReader r m, Has FieldMap r)
  => RelationInfoMap
  -> PGColGNameMap
  -> (G.Name, AnnInpVal)
  -> AnnInsObj
  -> m AnnInsObj
traverseInsObj rim allColMap (gName, annVal) defVal@(AnnInsObj cols objRels arrRels) =
  case _aivValue annVal of
    AGScalar{} -> parseValue
    AGEnum{}   -> parseValue
    _          -> parseObject
  where
    parseValue = do
      (_, WithScalarType scalarType maybeScalarValue) <- asPGColumnTypeAndValueM annVal
      columnInfo <- onNothing (Map.lookup gName allColMap) $
                 throw500 "column not found in PGColGNameMap"
      let columnName = pgiColumn columnInfo
      scalarValue <- maybe (pure $ PGNull scalarType) openOpaqueValue maybeScalarValue
      pure $ AnnInsObj ((columnName, WithScalarType scalarType scalarValue):cols) objRels arrRels

    parseObject = do
      objM <- asObjectM annVal
      -- if relational insert input is 'null' then ignore
      -- return default value
      fmap (fromMaybe defVal) $ forM objM $ \obj -> do
        let relNameM = RelName <$> mkNonEmptyText (G.unName gName)
            onConflictM = OMap.lookup "on_conflict" obj
        relName <- onNothing relNameM $ throw500 "found empty GName String"
        dataVal <- onNothing (OMap.lookup "data" obj) $
                   throw500 "\"data\" object not found"
        relInfo <- onNothing (Map.lookup relName rim) $
                   throw500 $ "relation " <> relName <<> " not found"

        let rTable = riRTable relInfo
        InsCtx rtView rtColMap rtDefVals rtRelInfoMap rtUpdPerm <- getInsCtx rTable
        let rtCols = Map.elems rtColMap
        rtDefValsRes <- mapM (convPartialSQLExp sessVarFromCurrentSetting) rtDefVals

        withPathK (G.unName gName) $ case riType relInfo of
          ObjRel -> do
            dataObj <- asObject dataVal
            annDataObj <- mkAnnInsObj rtRelInfoMap rtColMap dataObj
            ccM <- forM onConflictM $ parseOnConflict rTable rtUpdPerm rtColMap
            let singleObjIns = AnnIns annDataObj ccM rtView rtCols rtDefValsRes
                objRelIns = RelIns singleObjIns relInfo
            return (AnnInsObj cols (objRelIns:objRels) arrRels)

          ArrRel -> do
            arrDataVals <- asArray dataVal
            let withNonEmptyArrData = do
                  annDataObjs <- forM arrDataVals $ \arrDataVal -> do
                    dataObj <- asObject arrDataVal
                    mkAnnInsObj rtRelInfoMap rtColMap dataObj
                  ccM <- forM onConflictM $ parseOnConflict rTable rtUpdPerm rtColMap
                  let multiObjIns = AnnIns annDataObjs ccM rtView
                                    rtCols rtDefValsRes
                      arrRelIns = RelIns multiObjIns relInfo
                  return (AnnInsObj cols objRels (arrRelIns:arrRels))
            -- if array relation insert input data has empty objects
            -- then ignore and return default value
            bool withNonEmptyArrData (return defVal) $ null arrDataVals

parseOnConflict
  :: (MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r)
  => QualifiedTable
  -> Maybe UpdPermForIns
  -> PGColGNameMap
  -> AnnInpVal
  -> m RI.ConflictClauseP1
parseOnConflict tn updFiltrM allColMap val = withPathK "on_conflict" $
  flip withObject val $ \_ obj -> do
    constraint <- RI.Constraint <$> parseConstraint obj
    updCols <- getUpdCols obj
    case updCols of
      [] -> return $ RI.CP1DoNothing $ Just constraint
      _  -> do
          UpdPermForIns _ updFiltr preSet <- onNothing updFiltrM $ throw500
            "cannot update columns since update permission is not defined"
          preSetRes <- mapM (convPartialSQLExp sessVarFromCurrentSetting) preSet
          updFltrRes <- traverseAnnBoolExp
                        (convPartialSQLExp sessVarFromCurrentSetting)
                        updFiltr
          whereExp <- parseWhereExp obj
          let updateBoolExp = toSQLBoolExp (S.mkQual tn) updFltrRes
              whereCondition = S.BEBin S.AndOp updateBoolExp whereExp
          return $ RI.CP1Update constraint updCols preSetRes whereCondition

  where
    getUpdCols o = do
      updColsVal <- onNothing (OMap.lookup "update_columns" o) $ throw500
        "\"update_columns\" argument in expected in \"on_conflict\" field "
      parseColumns allColMap updColsVal

    parseConstraint o = do
      v <- onNothing (OMap.lookup "constraint" o) $ throw500
           "\"constraint\" is expected, but not found"
      (_, enumVal) <- asEnumVal v
      return $ ConstraintName $ G.unName $ G.unEnumValue enumVal

    parseWhereExp =
          OMap.lookup "where"
      >>> traverse (parseBoolExp >=> traverse (traverse resolveValTxt))
      >>> fmap (maybe (S.BELit True) (toSQLBoolExp (S.mkQual tn)))

toSQLExps
  :: (MonadError QErr m, MonadState PrepArgs m)
  => [PGColWithValue]
  -> m [(PGCol, S.SQLExp)]
toSQLExps cols =
  forM cols $ \(c, v) -> do
    prepExp <- prepareColVal v
    return (c, prepExp)

mkSQLRow :: Map.HashMap PGCol S.SQLExp -> [(PGCol, S.SQLExp)] -> [S.SQLExp]
mkSQLRow defVals withPGCol = map snd $
  flip map (Map.toList defVals) $
    \(col, defVal) -> (col,) $ fromMaybe defVal $ Map.lookup col withPGColMap
  where
    withPGColMap = Map.fromList withPGCol

mkInsertQ
  :: MonadError QErr m
  => QualifiedTable
  -> Maybe RI.ConflictClauseP1
  -> [PGColWithValue]
  -> Map.HashMap PGCol S.SQLExp
  -> RoleName
  -> m (CTEExp, Maybe RI.ConflictCtx)
mkInsertQ vn onConflictM insCols defVals role = do
  (givenCols, args) <- flip runStateT Seq.Empty $ toSQLExps insCols
  let sqlConflict = RI.toSQLConflict <$> onConflictM
      sqlExps = mkSQLRow defVals givenCols
      valueExp = S.ValuesExp [S.TupleExp sqlExps]
      tableCols = Map.keys defVals
      sqlInsert =
        S.SQLInsert vn tableCols valueExp sqlConflict $ Just S.returningStar
      adminIns = return (CTEExp (S.CTEInsert sqlInsert) args, Nothing)
      nonAdminInsert = do
        ccM <- mapM RI.extractConflictCtx onConflictM
        let cteIns = S.CTEInsert sqlInsert{S.siConflict=Nothing}
        return (CTEExp cteIns args, ccM)

  bool nonAdminInsert adminIns $ isAdmin role

asSingleObject
  :: MonadError QErr m
  => [ColumnValues J.Value] -> m (Maybe (ColumnValues J.Value))
asSingleObject = \case
  []  -> return Nothing
  [a] -> return $ Just a
  _   -> throw500 "more than one row returned"

fetchFromColVals
  :: MonadError QErr m
  => ColumnValues J.Value
  -> [PGColumnInfo]
  -> (PGColumnInfo -> a)
  -> m [(a, WithScalarType PGScalarValue)]
fetchFromColVals colVal reqCols f =
  forM reqCols $ \ci -> do
    let valM = Map.lookup (pgiColumn ci) colVal
    val <- onNothing valM $ throw500 $ "column "
           <> pgiColumn ci <<> " not found in given colVal"
    pgColVal <- parsePGScalarValue (pgiType ci) val
    return (f ci, pgColVal)

mkSelCTE
  :: MonadError QErr m
  => QualifiedTable
  -> [PGColumnInfo]
  -> Maybe (ColumnValues J.Value)
  -> m CTEExp
mkSelCTE tn allCols colValM = do
  selCTE <- mkSelCTEFromColVals parseFn tn allCols $ maybe [] pure colValM
  return $ CTEExp selCTE Seq.Empty
  where
    parseFn ty val = toTxtValue <$> parsePGScalarValue ty val

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
        relNameTxt = relNameToTxt relName
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
    retColsWithVals <- fetchFromColVals colVal rColInfos pgiColumn
    let c = mergeListsWith mapCols retColsWithVals
          (\(_, rCol) (col, _) -> rCol == col)
          (\(lCol, _) (_, cVal) -> (lCol, cVal))
    return (aRows, c)
  where
    RelIns singleObjIns relInfo = objRelIns
    multiObjIns = singleToMulti singleObjIns
    relName = riName relInfo
    relNameTxt = relNameToTxt relName
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
    relNameTxt = relNameToTxt $ riName relInfo
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
  validateInsert (map fst cols) (map _riRelInfo objRels) $ map fst addCols

  -- insert all object relations and fetch this insert dependent column values
  objInsRes <- forM objRels $ insertObjRel strfyNum role

  -- prepare final insert columns
  let objRelAffRows = sum $ map fst objInsRes
      objRelDeterminedCols = concatMap snd objInsRes
      finalInsCols = cols <> objRelDeterminedCols <> addCols

  -- prepare insert query as with expression
  (CTEExp cte insPArgs, ccM) <-
    mkInsertQ vn onConflictM finalInsCols defVals role

  RI.setConflictCtx ccM
  MutateResp affRows colVals <- mutateAndFetchCols tn allCols (cte, insPArgs) strfyNum
  colValM <- asSingleObject colVals
  cteExp <- mkSelCTE tn allCols colValM

  arrRelAffRows <- bool (withArrRels colValM) (return 0) $ null arrRels
  let totAffRows = objRelAffRows + affRows + arrRelAffRows

  return (totAffRows, cteExp)
  where
    AnnIns annObj onConflictM vn allCols defVals = singleObjIns
    AnnInsObj cols objRels arrRels = annObj

    arrRelDepCols = flip getColInfos allCols $
      concatMap (map fst . riMapping . _riRelInfo) arrRels

    withArrRels colValM = do
      colVal <- onNothing colValM $ throw400 NotSupported cannotInsArrRelErr
      arrDepColsWithVal <- fetchFromColVals colVal arrRelDepCols pgiColumn

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
    AnnIns insObjs onConflictM vn tableColInfos defVals = multiObjIns
    singleObjInserts = multiToSingles multiObjIns
    insCols = map _aioColumns insObjs
    allInsObjRels = concatMap _aioObjRels insObjs
    allInsArrRels = concatMap _aioArrRels insObjs
    anyRelsToInsert = not $ null allInsArrRels && null allInsObjRels

    withErrPath = withPathK errP

    -- insert all column rows at one go
    withoutRelsInsert = withErrPath $ do
      indexedForM_ insCols $ \insCol ->
        validateInsert (map fst insCol) [] $ map fst addCols

      let withAddCols = flip map insCols $ union addCols
          tableCols = Map.keys defVals

      (sqlRows, prepArgs) <- flip runStateT Seq.Empty $ do
        rowsWithCol <- mapM toSQLExps withAddCols
        return $ map (mkSQLRow defVals) rowsWithCol

      let insQP1 = RI.InsertQueryP1 tn vn tableCols sqlRows onConflictM mutFlds tableColInfos
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
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, Has InsCtxMap r
     )
  => RoleName
  -> QualifiedTable -- table
  -> Field -- the mutation field
  -> m RespTx
convertInsert role tn fld = prefixErrPath fld $ do
  mutFldsUnres <- convertMutResp (_fType fld) $ _fSelSet fld
  mutFldsRes <- RR.traverseMutFlds resolveValTxt mutFldsUnres
  annVals <- withArg arguments "objects" asArray
  -- if insert input objects is empty array then
  -- do not perform insert and return mutation response
  bool (withNonEmptyObjs annVals mutFldsRes)
    (withEmptyObjs mutFldsRes) $ null annVals
  where
    withNonEmptyObjs annVals mutFlds = do
      InsCtx vn tableColMap defValMap relInfoMap updPerm <- getInsCtx tn
      annObjs <- mapM asObject annVals
      annInsObjs <- forM annObjs $ mkAnnInsObj relInfoMap tableColMap
      conflictClauseM <- forM onConflictM $ parseOnConflict tn updPerm tableColMap
      defValMapRes <- mapM (convPartialSQLExp sessVarFromCurrentSetting)
                      defValMap
      let multiObjIns = AnnIns annInsObjs conflictClauseM
                        vn tableCols defValMapRes
          tableCols = Map.elems tableColMap
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
  let defValMap = fmap PSESQLExp $ S.mkColDefValMap $ map pgiColumn $
                  Map.elems $ icAllCols insCtx
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
