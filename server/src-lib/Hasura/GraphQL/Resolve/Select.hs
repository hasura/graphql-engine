module Hasura.GraphQL.Resolve.Select
  ( convertSelect
  , convertSelectByPKey
  , convertAggSelect
  , convertFuncQuerySimple
  , convertFuncQueryAgg
  , parseColumns
  , fromSelSet
  , QueryRootFldAST(..)
  , traverseQueryRootFldAST
  , QueryRootFldUnresolved
  , QueryRootFldResolved
  , toPGQuery
  ) where

import           Control.Arrow                     (first)
import           Data.Has
import           Data.Parser.JSONPath
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Data.List.NonEmpty                as NE
import qualified Data.Text                         as T
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Database.PG.Query                 as Q
import qualified Hasura.RQL.DML.Select             as RS
import qualified Hasura.SQL.DML                    as S

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Schema             (isAggFld)
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal           (onlyPositiveInt)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

jsonPathToColExp :: (MonadError QErr m) => T.Text -> m S.SQLExp
jsonPathToColExp t = case parseJSONPath t of
  Left s       -> throw400 ParseFailed $ T.pack $ "parse json path error: " ++ s
  Right jPaths -> return $ S.SEArray $ map elToColExp jPaths
  where
    elToColExp (Key k)   = S.SELit k
    elToColExp (Index i) = S.SELit $ T.pack (show i)


argsToColOp :: (MonadError QErr m) => ArgsMap -> m (Maybe RS.ColOp)
argsToColOp args = maybe (return Nothing) toOp $ Map.lookup "path" args
  where
    toJsonPathExp = fmap (RS.ColOp S.jsonbPathOp) . jsonPathToColExp
    toOp v = asPGColTextM v >>= mapM toJsonPathExp

type AnnFlds = RS.AnnFldsG UnresolvedVal

fromSelSet
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> SelSet -> m AnnFlds
fromSelSet fldTy flds =
  forM (toList flds) $ \fld -> do
    let fldName = _fName fld
    let rqlFldName = FieldName $ G.unName $ G.unAlias $ _fAlias fld
    (rqlFldName,) <$> case fldName of
      "__typename" -> return $ RS.FExp $ G.unName $ G.unNamedType fldTy
      _ -> do
        fldInfo <- getFldInfo fldTy fldName
        case fldInfo of
          FldCol colInfo ->
            RS.FCol colInfo <$> argsToColOp (_fArguments fld)
          FldRel (relInfo, isAgg, tableFilter, tableLimit) -> do
            let relTN = riRTable relInfo
                colMapping = riMapping relInfo
                rn = riName relInfo
            if isAgg then do
              aggSel <- fromAggField relTN tableFilter tableLimit fld
              return $ RS.FArr $ RS.ASAgg $ RS.AnnRelG rn colMapping aggSel
            else do
              annSel <- fromField relTN tableFilter tableLimit fld
              let annRel = RS.AnnRelG rn colMapping annSel
              return $ case riType relInfo of
                ObjRel -> RS.FObj annRel
                ArrRel -> RS.FArr $ RS.ASSimple annRel
          FldRemote _ -> return RS.FRemote

type TableAggFlds = RS.TableAggFldsG UnresolvedVal

fromAggSelSet
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> SelSet -> m TableAggFlds
fromAggSelSet fldTy selSet = fmap toFields $
  withSelSet selSet $ \f -> do
    let fTy = _fType f
        fSelSet = _fSelSet f
    case _fName f of
      "__typename" -> return $ RS.TAFExp $ G.unName $ G.unNamedType fldTy
      "aggregate"  -> RS.TAFAgg <$> convertAggFld fTy fSelSet
      "nodes"      -> RS.TAFNodes <$> fromSelSet fTy fSelSet
      G.Name t     -> throw500 $ "unexpected field in _agg node: " <> t

type TableArgs = RS.TableArgsG UnresolvedVal

parseTableArgs
  :: ( MonadError QErr m, MonadReader r m
     , Has FieldMap r, Has OrdByCtx r
     )
  => ArgsMap -> m TableArgs
parseTableArgs args = do
  whereExpM  <- withArgM args "where" parseBoolExp
  ordByExpML <- withArgM args "order_by" parseOrderBy
  let ordByExpM = NE.nonEmpty =<< ordByExpML
  limitExpM  <- withArgM args "limit" parseLimit
  offsetExpM <- withArgM args "offset" $ asPGColVal >=> txtConverter
  distOnColsML <- withArgM args "distinct_on" parseColumns
  let distOnColsM = NE.nonEmpty =<< distOnColsML
  mapM_ (validateDistOn ordByExpM) distOnColsM
  return $ RS.TableArgs whereExpM ordByExpM limitExpM offsetExpM distOnColsM
  where
    validateDistOn Nothing _ = return ()
    validateDistOn (Just ordBys) cols = withPathK "args" $ do
      let colsLen = length cols
          initOrdBys = take colsLen $ toList ordBys
          initOrdByCols = flip mapMaybe initOrdBys $ \ob ->
            case obiColumn ob of
              RS.AOCPG ci -> Just $ pgiName ci
              _           -> Nothing
          isValid = (colsLen == length initOrdByCols)
                    && all (`elem` initOrdByCols) (toList cols)

      unless isValid $ throwVE
        "\"distinct_on\" columns must match initial \"order_by\" columns"

type AnnSimpleSelect = RS.AnnSimpleSelG UnresolvedVal

fromField
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => QualifiedTable -> AnnBoolExpPartialSQL
  -> Maybe Int -> Field -> m AnnSimpleSelect
fromField tn permFilter permLimitM fld = fieldAsPath fld $ do
  tableArgs <- parseTableArgs args
  annFlds   <- fromSelSet (_fType fld) $ _fSelSet fld
  let unresolvedPermFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal permFilter
  let tabFrom = RS.TableFrom tn Nothing
      tabPerm = RS.TablePerm unresolvedPermFltr permLimitM
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelG annFlds tabFrom tabPerm tableArgs strfyNum
  where
    args = _fArguments fld

getOrdByItemMap
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OrdByCtx r
     )
  => G.NamedType -> m OrdByItemMap
getOrdByItemMap nt = do
  ordByCtx <- asks getter
  onNothing (Map.lookup nt ordByCtx) $
    throw500 $ "could not lookup " <> showNamedTy nt

parseOrderBy
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OrdByCtx r
     )
  => AnnInpVal -> m [RS.AnnOrderByItemG UnresolvedVal]
parseOrderBy = fmap concat . withArray f
  where
    f _ = mapM (withObject (getAnnObItems id))

getAnnObItems
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OrdByCtx r
     )
  => (RS.AnnObColG UnresolvedVal -> RS.AnnObColG UnresolvedVal)
  -> G.NamedType
  -> AnnGObject
  -> m [RS.AnnOrderByItemG UnresolvedVal]
getAnnObItems f nt obj = do
  ordByItemMap <- getOrdByItemMap nt
  fmap concat $ forM (OMap.toList obj) $ \(k, v) -> do
    ordByItem <- onNothing (Map.lookup k ordByItemMap) $ throw500 $
      "cannot lookup " <> showName k <> " order by item in "
      <> showNamedTy nt <> " map"
    case ordByItem of
      OBIPGCol ci -> do
        let aobCol = f $ RS.AOCPG ci
        (_, enumVal) <- asEnumVal v
        (ordTy, nullsOrd) <- parseOrderByEnum enumVal
        return [mkOrdByItemG ordTy aobCol nullsOrd]
      OBIRel ri fltr -> do
        let unresolvedFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal fltr
        let annObColFn = f . RS.AOCObj ri unresolvedFltr
        withObject (getAnnObItems annObColFn) v

      OBIAgg ri fltr -> do
        let unresolvedFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal fltr
        let aobColFn = f . RS.AOCAgg ri unresolvedFltr
        flip withObject v $ \_ o -> parseAggOrdBy aobColFn o

mkOrdByItemG :: S.OrderType -> a -> S.NullsOrder -> OrderByItemG a
mkOrdByItemG ordTy aobCol nullsOrd =
  OrderByItemG (Just $ OrderType ordTy) aobCol (Just $ NullsOrder nullsOrd)

parseAggOrdBy
  :: (MonadError QErr m)
  => (RS.AnnAggOrdBy -> RS.AnnObColG UnresolvedVal)
  -> AnnGObject
  -> m [RS.AnnOrderByItemG UnresolvedVal]
parseAggOrdBy f annObj =
  fmap concat <$> forM (OMap.toList annObj) $ \(op, obVal) ->
    case op of
      "count" -> do
        (ordTy, nullsOrd) <- parseAsEnum obVal
        return [mkOrdByItemG ordTy (f RS.AAOCount) nullsOrd]

      G.Name opT ->
        flip withObject obVal $ \_ opObObj ->
          forM (OMap.toList opObObj) $ \(col, eVal) -> do
            (ordTy, nullsOrd) <- parseAsEnum eVal
            let aobCol = f $ RS.AAOOp opT $ PGCol $ G.unName col
            return $ mkOrdByItemG ordTy aobCol nullsOrd
  where
    parseAsEnum v = do
      (_, enumVal) <- asEnumVal v
      parseOrderByEnum enumVal

parseOrderByEnum
  :: (MonadError QErr m)
  => G.EnumValue
  -> m (S.OrderType, S.NullsOrder)
parseOrderByEnum = \case
  G.EnumValue "asc"              -> return (S.OTAsc, S.NLast)
  G.EnumValue "asc_nulls_last"  -> return (S.OTAsc, S.NLast)
  G.EnumValue "asc_nulls_first"  -> return (S.OTAsc, S.NFirst)
  G.EnumValue "desc"             -> return (S.OTDesc, S.NFirst)
  G.EnumValue "desc_nulls_first" -> return (S.OTDesc, S.NFirst)
  G.EnumValue "desc_nulls_last" -> return (S.OTDesc, S.NLast)
  G.EnumValue v                   -> throw500 $
    "enum value " <> showName v <> " not found in type order_by"

parseLimit :: ( MonadError QErr m ) => AnnInpVal -> m Int
parseLimit v = do
  pgColVal <- _apvValue <$> asPGColVal v
  limit <- maybe noIntErr return $ pgColValueToInt pgColVal
  -- validate int value
  onlyPositiveInt limit
  return limit
  where
    noIntErr = throwVE "expecting Integer value for \"limit\""

type AnnSimpleSel = RS.AnnSimpleSelG UnresolvedVal

type PGColValMap = Map.HashMap G.Name AnnInpVal

pgColValToBoolExp
  :: (MonadError QErr m)
  => PGColArgMap -> PGColValMap -> m AnnBoolExpUnresolved
pgColValToBoolExp colArgMap colValMap = do
  colExps <- forM colVals $ \(name, val) ->
    BoolFld <$> do
      opExp <- AEQ True . UVPG <$> asPGColVal val
      colInfo <- onNothing (Map.lookup name colArgMap) $
        throw500 $ "column name " <> showName name
        <> " not found in column arguments map"
      return $ AVCol colInfo [opExp]
  return $ BoolAnd colExps
  where
    colVals = Map.toList colValMap

fromFieldByPKey
  :: ( MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => QualifiedTable -> PGColArgMap
  -> AnnBoolExpPartialSQL -> Field -> m AnnSimpleSel
fromFieldByPKey tn colArgMap permFilter fld = fieldAsPath fld $ do
  boolExp <- pgColValToBoolExp colArgMap $ _fArguments fld
  annFlds <- fromSelSet fldTy $ _fSelSet fld
  let tabFrom = RS.TableFrom tn Nothing
      unresolvedPermFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal
                           permFilter
      tabPerm = RS.TablePerm unresolvedPermFltr Nothing
      tabArgs = RS.noTableArgs { RS._taWhere = Just boolExp}
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelG annFlds tabFrom tabPerm tabArgs strfyNum
  where
    fldTy = _fType fld

convertSelect
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => SelOpCtx -> Field -> m QueryRootFldUnresolved
convertSelect opCtx fld =
  withPathK "selectionSet" $ QRFSimple <$>
  fromField qt permFilter permLimit fld
  where
    SelOpCtx qt _ permFilter permLimit = opCtx

convertSelectByPKey
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => SelPkOpCtx -> Field -> m QueryRootFldUnresolved
convertSelectByPKey opCtx fld =
  withPathK "selectionSet" $ QRFPk <$>
    fromFieldByPKey qt colArgMap permFilter fld
  where
    SelPkOpCtx qt _ permFilter colArgMap = opCtx

-- agg select related
parseColumns :: MonadError QErr m => AnnInpVal -> m [PGCol]
parseColumns val =
  flip withArray val $ \_ vals ->
    forM vals $ \v -> do
      (_, enumVal) <- asEnumVal v
      return $ PGCol $ G.unName $ G.unEnumValue enumVal

convertCount :: MonadError QErr m => ArgsMap -> m S.CountType
convertCount args = do
  columnsM <- withArgM args "columns" parseColumns
  isDistinct <- or <$> withArgM args "distinct" parseDistinct
  maybe (return S.CTStar) (mkCType isDistinct) columnsM
  where
    parseDistinct v = do
      val <- _apvValue <$> asPGColVal v
      case val of
        PGValBoolean b -> return b
        _              ->
          throw500 "expecting Boolean for \"distinct\""

    mkCType isDistinct cols = return $
      bool (S.CTSimple cols) (S.CTDistinct cols) isDistinct

toFields :: [(T.Text, a)] -> RS.Fields a
toFields = map (first FieldName)

convertColFlds
  :: Monad m => G.NamedType -> SelSet -> m RS.ColFlds
convertColFlds ty selSet = fmap toFields $
  withSelSet selSet $ \fld ->
    case _fName fld of
      "__typename" -> return $ RS.PCFExp $ G.unName $ G.unNamedType ty
      n            -> return $ RS.PCFCol $ PGCol $ G.unName n

convertAggFld
  :: (Monad m, MonadError QErr m)
  => G.NamedType -> SelSet -> m RS.AggFlds
convertAggFld ty selSet = fmap toFields $
  withSelSet selSet $ \fld -> do
    let fType = _fType fld
        fSelSet = _fSelSet fld
    case _fName fld of
      "__typename" -> return $ RS.AFExp $ G.unName $ G.unNamedType ty
      "count"      -> RS.AFCount <$> convertCount (_fArguments fld)
      n            -> do
        colFlds <- convertColFlds fType fSelSet
        unless (isAggFld n) $ throwInvalidFld n
        return $ RS.AFOp $ RS.AggOp (G.unName n) colFlds
  where
      throwInvalidFld (G.Name t) =
        throw500 $ "unexpected field in _aggregate node: " <> t

type AnnAggSel = RS.AnnAggSelG UnresolvedVal

fromAggField
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => QualifiedTable -> AnnBoolExpPartialSQL
  -> Maybe Int -> Field -> m AnnAggSel
fromAggField tn permFilter permLimit fld = fieldAsPath fld $ do
  tableArgs   <- parseTableArgs args
  aggSelFlds  <- fromAggSelSet (_fType fld) (_fSelSet fld)
  let unresolvedPermFltr =
        fmapAnnBoolExp partialSQLExpToUnresolvedVal permFilter
  let tabFrom = RS.TableFrom tn Nothing
      tabPerm = RS.TablePerm unresolvedPermFltr permLimit
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelG aggSelFlds tabFrom tabPerm tableArgs strfyNum
  where
    args = _fArguments fld

convertAggSelect
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => SelOpCtx -> Field -> m QueryRootFldUnresolved
convertAggSelect opCtx fld =
  withPathK "selectionSet" $ QRFAgg <$>
  fromAggField qt permFilter permLimit fld
  -- return $ RS.selectAggQuerySQL selData
  where
    SelOpCtx qt _ permFilter permLimit = opCtx

parseFunctionArgs
  ::( MonadError QErr m)
  => FuncArgSeq -> AnnInpVal -> m [UnresolvedVal]
parseFunctionArgs argSeq val = fmap catMaybes $
  flip withObject val $ \_ obj ->
    fmap toList $ forM argSeq $ \(FuncArgItem argName) ->
      forM (OMap.lookup argName obj) $ fmap (maybe nullSQL UVPG) . asPGColValM
  where
    nullSQL = UVSQL $ S.SEUnsafe "NULL"

fromFuncQueryField
  :: (MonadError QErr m)
  => (Field -> m s)
  -> QualifiedFunction -> FuncArgSeq
  -> Field
  -> m (RS.AnnFnSelG s UnresolvedVal)
fromFuncQueryField fn qf argSeq fld = fieldAsPath fld $ do
  funcArgsM <- withArgM (_fArguments fld) "args" $ parseFunctionArgs argSeq
  let funcArgs = fromMaybe [] funcArgsM
  RS.AnnFnSel qf funcArgs <$> fn fld

convertFuncQuerySimple
  :: ( MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => FuncQOpCtx -> Field -> m QueryRootFldUnresolved
convertFuncQuerySimple funcOpCtx fld =
  withPathK "selectionSet" $ QRFFnSimple <$>
    fromFuncQueryField (fromField qt permFilter permLimit) qf argSeq fld
  where
    FuncQOpCtx qt _ permFilter permLimit qf argSeq = funcOpCtx

convertFuncQueryAgg
  :: ( MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => FuncQOpCtx -> Field -> m QueryRootFldUnresolved
convertFuncQueryAgg funcOpCtx fld =
  withPathK "selectionSet" $ QRFFnAgg <$>
    fromFuncQueryField (fromAggField qt permFilter permLimit) qf argSeq fld
  where
    FuncQOpCtx qt _ permFilter permLimit qf argSeq = funcOpCtx

data QueryRootFldAST v
  = QRFPk !(RS.AnnSimpleSelG v)
  | QRFSimple !(RS.AnnSimpleSelG v)
  | QRFAgg !(RS.AnnAggSelG v)
  | QRFFnSimple !(RS.AnnFnSelSimpleG v)
  | QRFFnAgg !(RS.AnnFnSelAggG v)
  deriving (Show, Eq)

type QueryRootFldUnresolved = QueryRootFldAST UnresolvedVal
type QueryRootFldResolved = QueryRootFldAST S.SQLExp

traverseQueryRootFldAST
  :: (Applicative f)
  => (a -> f b)
  -> QueryRootFldAST a
  -> f (QueryRootFldAST b)
traverseQueryRootFldAST f = \case
  QRFPk s       -> QRFPk <$> RS.traverseAnnSimpleSel f s
  QRFSimple s   -> QRFSimple <$> RS.traverseAnnSimpleSel f s
  QRFAgg s      -> QRFAgg <$> RS.traverseAnnAggSel f s
  QRFFnSimple s -> QRFFnSimple <$> RS.traverseAnnFnSimple f s
  QRFFnAgg s    -> QRFFnAgg <$> RS.traverseAnnFnAgg f s

toPGQuery :: QueryRootFldResolved -> Q.Query
toPGQuery = \case
  QRFPk s       -> RS.selectQuerySQL True s
  QRFSimple s   -> RS.selectQuerySQL False s
  QRFAgg s      -> RS.selectAggQuerySQL s
  QRFFnSimple s -> RS.mkFuncSelectSimple s
  QRFFnAgg s    -> RS.mkFuncSelectAgg s
