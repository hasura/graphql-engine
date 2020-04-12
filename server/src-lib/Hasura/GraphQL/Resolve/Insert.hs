module Hasura.GraphQL.Resolve.Insert
  ( convertInsert
  , convertInsertOne
  )
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
import           Hasura.RQL.DML.Insert             (insertOrUpdateCheckExpr)
import           Hasura.RQL.DML.Internal           (convAnnBoolExpPartialSQL, convPartialSQLExp,
                                                    dmlTxErrorHandler, sessVarFromCurrentSetting)
import           Hasura.RQL.DML.Mutation
import           Hasura.RQL.GBoolExp               (toSQLBoolExp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

type ColumnValuesText = ColumnValues TxtEncodedPGVal

newtype InsResp
  = InsResp
  { _irResponse     :: Maybe J.Object
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''InsResp)

data AnnIns a
  = AnnIns
  { _aiInsObj         :: !a
  , _aiConflictClause :: !(Maybe RI.ConflictClauseP1)
  , _aiCheckCond      :: !(AnnBoolExpPartialSQL, Maybe AnnBoolExpPartialSQL)
  , _aiTableCols      :: ![PGColumnInfo]
  , _aiDefVals        :: !(Map.HashMap PGCol S.SQLExp)
  } deriving (Show, Eq, Functor, Foldable, Traversable)

type SingleObjIns = AnnIns AnnInsObj
type MultiObjIns = AnnIns [AnnInsObj]

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
        InsCtx rtColMap checkCond rtDefVals rtRelInfoMap rtUpdPerm <- getInsCtx rTable
        let rtCols = Map.elems rtColMap
        rtDefValsRes <- mapM (convPartialSQLExp sessVarFromCurrentSetting) rtDefVals

        withPathK (G.unName gName) $ case riType relInfo of
          ObjRel -> do
            dataObj <- asObject dataVal
            annDataObj <- mkAnnInsObj rtRelInfoMap rtColMap dataObj
            ccM <- forM onConflictM $ parseOnConflict rTable rtUpdPerm rtColMap
            let singleObjIns = AnnIns annDataObj ccM (checkCond, rtUpdPerm >>= upfiCheck) rtCols rtDefValsRes
                objRelIns = RelIns singleObjIns relInfo
            return (AnnInsObj cols (objRelIns:objRels) arrRels)

          ArrRel -> do
            arrDataVals <- asArray dataVal
            let withNonEmptyArrData = do
                  annDataObjs <- forM arrDataVals $ \arrDataVal -> do
                    dataObj <- asObject arrDataVal
                    mkAnnInsObj rtRelInfoMap rtColMap dataObj
                  ccM <- forM onConflictM $ parseOnConflict rTable rtUpdPerm rtColMap
                  let multiObjIns = AnnIns annDataObjs ccM (checkCond, rtUpdPerm >>= upfiCheck) rtCols rtDefValsRes
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
    constraint <- RI.CTConstraint <$> parseConstraint obj
    updCols <- getUpdCols obj
    case updCols of
      [] -> return $ RI.CP1DoNothing $ Just constraint
      _  -> do
          UpdPermForIns _ _ updFiltr preSet <- onNothing updFiltrM $ throw500
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
  -> (AnnBoolExpSQL, Maybe AnnBoolExpSQL)
  -> m CTEExp
mkInsertQ tn onConflictM insCols defVals role (insCheck, updCheck) = do
  (givenCols, args) <- flip runStateT Seq.Empty $ toSQLExps insCols
  let sqlConflict = RI.toSQLConflict <$> onConflictM
      sqlExps = mkSQLRow defVals givenCols
      valueExp = S.ValuesExp [S.TupleExp sqlExps]
      tableCols = Map.keys defVals
      sqlInsert =
        S.SQLInsert tn tableCols valueExp sqlConflict
          . Just
          $ S.RetExp
            [ S.selectStar
            , S.Extractor
                (insertOrUpdateCheckExpr tn onConflictM
                  (toSQLBoolExp (S.QualTable tn) insCheck)
                  (fmap (toSQLBoolExp (S.QualTable tn)) updCheck))
                Nothing
            ]

      adminIns = return (CTEExp (S.CTEInsert sqlInsert) args)
      nonAdminInsert = do
        let cteIns = S.CTEInsert sqlInsert
        return (CTEExp cteIns args)

  bool nonAdminInsert adminIns $ isAdmin role

fetchFromColVals
  :: MonadError QErr m
  => ColumnValuesText
  -> [PGColumnInfo]
  -> m [(PGCol, WithScalarType PGScalarValue)]
fetchFromColVals colVal reqCols =
  forM reqCols $ \ci -> do
    let valM = Map.lookup (pgiColumn ci) colVal
    val <- onNothing valM $ throw500 $ "column "
           <> pgiColumn ci <<> " not found in given colVal"
    pgColVal <- parseTxtEncodedPGValue (pgiType ci) val
    return (pgiColumn ci, pgColVal)

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
    let lCols = Map.keys $ riMapping relInfo
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
    (affRows, colValM) <- withPathK "data" $ insertObj strfyNum role tn singleObjIns []
    colVal <- onNothing colValM $ throw400 NotSupported errMsg
    retColsWithVals <- fetchFromColVals colVal rColInfos
    let c = mergeListsWith (Map.toList mapCols) retColsWithVals
          (\(_, rCol) (col, _) -> rCol == col)
          (\(lCol, _) (_, cVal) -> (lCol, cVal))
    return (affRows, c)
  where
    RelIns singleObjIns relInfo = objRelIns
    -- multiObjIns = singleToMulti singleObjIns
    relName = riName relInfo
    relNameTxt = relNameToTxt relName
    mapCols = riMapping relInfo
    tn = riRTable relInfo
    allCols = _aiTableCols singleObjIns
    rCols = Map.elems mapCols
    rColInfos = getColInfos rCols allCols
    errMsg = "cannot proceed to insert object relation "
             <> relName <<> " since insert to table "
             <> tn <<> " affects zero rows"

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
    let addCols = mergeListsWith resCols (Map.toList colMapping)
               (\(col, _) (lCol, _) -> col == lCol)
               (\(_, colVal) (_, rCol) -> (rCol, colVal))

    resBS <- insertMultipleObjects strfyNum role tn multiObjIns addCols mutOutput "data"
    resObj <- decodeEncJSON resBS
    onNothing (Map.lookup ("affected_rows" :: T.Text) resObj) $
      throw500 "affected_rows not returned in array rel insert"
  where
    RelIns multiObjIns relInfo = arrRelIns
    colMapping = riMapping relInfo
    tn = riRTable relInfo
    relNameTxt = relNameToTxt $ riName relInfo
    mutOutput = RR.MOutMultirowFields [("affected_rows", RR.MCount)]

-- | insert an object with object and array relationships
insertObj
  :: Bool
  -> RoleName
  -> QualifiedTable
  -> SingleObjIns
  -> [PGColWithValue] -- ^ additional fields
  -> Q.TxE QErr (Int, Maybe ColumnValuesText)
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
  insCheck <- convAnnBoolExpPartialSQL sessVarFromCurrentSetting insCond
  updCheck <- traverse (convAnnBoolExpPartialSQL sessVarFromCurrentSetting) updCond

  CTEExp cte insPArgs <-
    mkInsertQ tn onConflictM finalInsCols defVals role (insCheck, updCheck)

  MutateResp affRows colVals <- mutateAndFetchCols tn allCols (cte, insPArgs) strfyNum
  colValM <- asSingleObject colVals

  arrRelAffRows <- bool (withArrRels colValM) (return 0) $ null arrRels
  let totAffRows = objRelAffRows + affRows + arrRelAffRows

  return (totAffRows, colValM)
  where
    AnnIns annObj onConflictM (insCond, updCond) allCols defVals = singleObjIns
    AnnInsObj cols objRels arrRels = annObj

    arrRelDepCols = flip getColInfos allCols $
      concatMap (Map.keys . riMapping . _riRelInfo) arrRels

    withArrRels colValM = do
      colVal <- onNothing colValM $ throw400 NotSupported cannotInsArrRelErr
      arrDepColsWithVal <- fetchFromColVals colVal arrRelDepCols
      arrInsARows <- forM arrRels $ insertArrRel strfyNum role arrDepColsWithVal
      return $ sum arrInsARows

    asSingleObject = \case
      [] -> pure Nothing
      [r] -> pure $ Just r
      _ -> throw500 "more than one row returned"

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
  -> RR.MutationOutput
  -> T.Text -- ^ error path
  -> Q.TxE QErr EncJSON
insertMultipleObjects strfyNum role tn multiObjIns addCols mutOutput errP =
  bool withoutRelsInsert withRelsInsert anyRelsToInsert
  where
    AnnIns insObjs onConflictM (insCond, updCond) tableColInfos defVals = multiObjIns
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

      insCheck <- convAnnBoolExpPartialSQL sessVarFromCurrentSetting insCond
      updCheck <- traverse (convAnnBoolExpPartialSQL sessVarFromCurrentSetting) updCond

      let insQP1 = RI.InsertQueryP1 tn tableCols sqlRows onConflictM
                     (insCheck, updCheck) mutOutput tableColInfos
          p1 = (insQP1, prepArgs)
      RI.insertP2 strfyNum p1

    -- insert each object with relations
    withRelsInsert = withErrPath $ do
      insResps <- indexedForM singleObjInserts $ \objIns ->
          insertObj strfyNum role tn objIns addCols

      let affRows = sum $ map fst insResps
          columnValues = mapMaybe snd insResps
      cteExp <- mkSelCTEFromColVals tn tableColInfos columnValues
      let sql = toSQL $ RR.mkMutationOutputExp tn tableColInfos (Just affRows) cteExp mutOutput strfyNum
      runIdentity . Q.getRow
               <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder sql) [] False

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
  mutOutputUnres <- RR.MOutMultirowFields <$> resolveMutationFields (_fType fld) (_fSelSet fld)
  mutOutputRes <- RR.traverseMutationOutput resolveValTxt mutOutputUnres
  annVals <- withArg arguments "objects" asArray
  -- if insert input objects is empty array then
  -- do not perform insert and return mutation response
  bool (withNonEmptyObjs annVals mutOutputRes)
    (withEmptyObjs mutOutputRes) $ null annVals
  where
    withNonEmptyObjs annVals mutOutput = do
      InsCtx tableColMap checkCond defValMap relInfoMap updPerm <- getInsCtx tn
      annObjs <- mapM asObject annVals
      annInsObjs <- forM annObjs $ mkAnnInsObj relInfoMap tableColMap
      conflictClauseM <- forM onConflictM $ parseOnConflict tn updPerm tableColMap
      defValMapRes <- mapM (convPartialSQLExp sessVarFromCurrentSetting)
                      defValMap
      let multiObjIns = AnnIns annInsObjs conflictClauseM (checkCond, updPerm >>= upfiCheck)
                          tableCols defValMapRes
          tableCols = Map.elems tableColMap
      strfyNum <- stringifyNum <$> asks getter
      return $ prefixErrPath fld $ insertMultipleObjects strfyNum role tn
        multiObjIns [] mutOutput "objects"
    withEmptyObjs mutOutput =
      return $ return $ buildEmptyMutResp mutOutput
    arguments = _fArguments fld
    onConflictM = Map.lookup "on_conflict" arguments

convertInsertOne
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, Has InsCtxMap r
     )
  => RoleName
  -> QualifiedTable -- table
  -> Field -- the mutation field
  -> m RespTx
convertInsertOne role qt field = prefixErrPath field $ do
  tableSelFields <- processTableSelectionSet (_fType field) $ _fSelSet field
  let mutationOutputUnresolved = RR.MOutSinglerowObject tableSelFields
  mutationOutputResolved <- RR.traverseMutationOutput resolveValTxt mutationOutputUnresolved
  annInputObj <- withArg arguments "object" asObject
  InsCtx tableColMap checkCond defValMap relInfoMap updPerm <- getInsCtx qt
  annInsertObj <- mkAnnInsObj relInfoMap tableColMap annInputObj
  conflictClauseM <- forM (Map.lookup "on_conflict" arguments) $ parseOnConflict qt updPerm tableColMap
  defValMapRes <- mapM (convPartialSQLExp sessVarFromCurrentSetting) defValMap
  let multiObjIns = AnnIns [annInsertObj] conflictClauseM (checkCond, updPerm >>= upfiCheck)
                    tableCols defValMapRes
      tableCols = Map.elems tableColMap
  strfyNum <- stringifyNum <$> asks getter
  pure $ prefixErrPath field $ insertMultipleObjects strfyNum role qt
         multiObjIns [] mutationOutputResolved "object"
  where
    arguments = _fArguments field

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

mergeListsWith
  :: [a] -> [b] -> (a -> b -> Bool) -> (a -> b -> c) -> [c]
mergeListsWith _ [] _ _ = []
mergeListsWith [] _ _ _ = []
mergeListsWith (x:xs) l b f = case find (b x) l of
  Nothing -> mergeListsWith xs l b f
  Just y  ->  f x y : mergeListsWith xs l b f
