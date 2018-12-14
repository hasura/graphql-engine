module Hasura.GraphQL.Resolve.Select
  ( convertSelect
  , convertSelectByPKey
  , convertAggSelect
  , parseColumns
  , withSelSet
  , fromSelSet
  , fieldAsPath
  , fromField
  , fromFieldByPKey
  , fromAggField
  ) where

import           Control.Arrow                     (first)
import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Data.List.NonEmpty                as NE
import qualified Data.Text                         as T
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Hasura.RQL.DML.Select             as RS
import qualified Hasura.SQL.DML                    as S

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

withSelSet :: (Monad m) => SelSet -> (Field -> m a) -> m [(Text, a)]
withSelSet selSet f =
  forM (toList selSet) $ \fld -> do
    res <- f fld
    return (G.unName $ G.unAlias $ _fAlias fld, res)

fromSelSet
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByCtx r)
  => PrepFn m -> G.NamedType -> SelSet -> m [(FieldName, RS.AnnFld)]
fromSelSet f fldTy flds =
  forM (toList flds) $ \fld -> do
    let fldName = _fName fld
    let rqlFldName = FieldName $ G.unName $ G.unAlias $ _fAlias fld
    (rqlFldName,) <$> case fldName of
      "__typename" -> return $ RS.FExp $ G.unName $ G.unNamedType fldTy
      _ -> do
        fldInfo <- getFldInfo fldTy fldName
        case fldInfo of
          Left colInfo -> return $ RS.FCol colInfo
          Right (relInfo, isAgg, tableFilter, tableLimit) -> do
            let relTN = riRTable relInfo
                colMapping = riMapping relInfo
                rn = riName relInfo
            if isAgg then do
              aggSel <- fromAggField f relTN tableFilter tableLimit fld
              return $ RS.FArr $ RS.ASAgg $ RS.AnnRelG rn colMapping aggSel
            else do
              annSel <- fromField f relTN tableFilter tableLimit fld
              let annRel = RS.AnnRelG rn colMapping annSel
              return $ case riType relInfo of
                ObjRel -> RS.FObj annRel
                ArrRel -> RS.FArr $ RS.ASSimple annRel

fieldAsPath :: (MonadError QErr m) => Field -> m a -> m a
fieldAsPath = nameAsPath . _fName

parseTableArgs
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByCtx r)
  => PrepFn m -> ArgsMap -> m RS.TableArgs
parseTableArgs f args = do
  whereExpM  <- withArgM args "where" $ parseBoolExp f
  ordByExpML <- withArgM args "order_by" parseOrderBy
  let ordByExpM = NE.nonEmpty =<< ordByExpML
  limitExpM  <- withArgM args "limit" parseLimit
  offsetExpM <- withArgM args "offset" $ asPGColVal >=> f
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

fromField
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByCtx r)
  => PrepFn m -> QualifiedTable -> AnnBoolExpSQL
  -> Maybe Int -> Field -> m RS.AnnSel
fromField f tn permFilter permLimitM fld =
  fieldAsPath fld $ do
  tableArgs <- parseTableArgs f args
  annFlds   <- fromSelSet f (_fType fld) $ _fSelSet fld
  let tabFrom = RS.TableFrom tn Nothing
      tabPerm = RS.TablePerm permFilter permLimitM
  return $ RS.AnnSelG annFlds tabFrom tabPerm tableArgs
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
  => AnnGValue -> m [RS.AnnOrderByItem]
parseOrderBy = fmap concat . withArray f
  where
    f _ = mapM (withObject (getAnnObItems id))

getAnnObItems
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OrdByCtx r
     )
  => (RS.AnnObCol -> RS.AnnObCol)
  -> G.NamedType
  -> AnnGObject
  -> m [RS.AnnOrderByItem]
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
        let annObColFn = f . RS.AOCObj ri fltr
        withObject (getAnnObItems annObColFn) v

      OBIAgg ri fltr -> do
        let aobColFn = f . RS.AOCAgg ri fltr
        flip withObject v $ \_ o -> parseAggOrdBy aobColFn o

mkOrdByItemG :: S.OrderType -> a -> S.NullsOrder -> OrderByItemG a
mkOrdByItemG ordTy aobCol nullsOrd =
  OrderByItemG (Just $ OrderType ordTy) aobCol (Just $ NullsOrder nullsOrd)

parseAggOrdBy
  :: (MonadError QErr m)
  => (RS.AnnAggOrdBy -> RS.AnnObCol)
  -> AnnGObject
  -> m [RS.AnnOrderByItem]
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

parseLimit :: ( MonadError QErr m ) => AnnGValue -> m Int
parseLimit v = do
  (_, pgColVal) <- asPGColVal v
  limit <- maybe noIntErr return $ pgColValueToInt pgColVal
  -- validate int value
  onlyPositiveInt limit
  return limit
  where
    noIntErr = throwVE "expecting Integer value for \"limit\""

fromFieldByPKey
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByCtx r)
  => ((PGColType, PGColValue) -> m S.SQLExp)
  -> QualifiedTable -> AnnBoolExpSQL -> Field -> m RS.AnnSel
fromFieldByPKey f tn permFilter fld = fieldAsPath fld $ do
  boolExp <- pgColValToBoolExp f $ _fArguments fld
  annFlds <- fromSelSet f (_fType fld) $ _fSelSet fld
  let tabFrom = RS.TableFrom tn Nothing
      tabPerm = RS.TablePerm permFilter Nothing
  return $ RS.AnnSelG annFlds tabFrom tabPerm $
    RS.noTableArgs { RS._taWhere = Just boolExp}

convertSelect
  :: QualifiedTable -> AnnBoolExpSQL -> Maybe Int -> Field -> Convert RespTx
convertSelect qt permFilter permLimit fld = do
  selData <- withPathK "selectionSet" $
             fromField prepare qt permFilter permLimit fld
  prepArgs <- get
  return $ RS.selectP2 False (selData, prepArgs)

convertSelectByPKey
  :: QualifiedTable -> AnnBoolExpSQL -> Field -> Convert RespTx
convertSelectByPKey qt permFilter fld = do
  selData <- withPathK "selectionSet" $
             fromFieldByPKey prepare qt permFilter fld
  prepArgs <- get
  return $ RS.selectP2 True (selData, prepArgs)

-- agg select related
parseColumns :: MonadError QErr m => AnnGValue -> m [PGCol]
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
      (_, val) <- asPGColVal v
      case val of
        PGValBoolean b -> return b
        _              ->
          throw500 "expecting Boolean for \"distinct\""

    mkCType isDistinct cols = return $
      bool (S.CTSimple cols) (S.CTDistinct cols) isDistinct

toFields :: [(T.Text, a)] -> [(FieldName, a)]
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

fromAggField
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByCtx r)
  => PrepFn m -> QualifiedTable -> AnnBoolExpSQL
  -> Maybe Int -> Field -> m RS.AnnAggSel
fromAggField fn tn permFilter permLimitM fld = fieldAsPath fld $ do
  tableArgs <- parseTableArgs fn args
  aggSelFlds <- toFields <$>
                fromAggSel (_fType fld) (_fSelSet fld)
  let tabFrom = RS.TableFrom tn Nothing
      tabPerm = RS.TablePerm permFilter permLimitM
  return $ RS.AnnSelG aggSelFlds tabFrom tabPerm tableArgs
  where
    args = _fArguments fld
    fromAggSel ty selSet =
      withSelSet selSet $ \f -> do
        let fTy = _fType f
            fSelSet = _fSelSet f
        case _fName f of
          "__typename" -> return $ RS.TAFExp $ G.unName $ G.unNamedType ty
          "aggregate"  -> RS.TAFAgg <$> convertAggFld fTy fSelSet
          "nodes"      -> RS.TAFNodes <$> fromSelSet fn fTy fSelSet
          G.Name t     -> throw500 $ "unexpected field in _agg node: " <> t

convertAggSelect
  :: QualifiedTable -> AnnBoolExpSQL -> Maybe Int -> Field -> Convert RespTx
convertAggSelect qt permFilter permLimit fld = do
  selData <- withPathK "selectionSet" $
             fromAggField prepare qt permFilter permLimit fld
  prepArgs <- get
  return $ RS.selectAggP2 (selData, prepArgs)
