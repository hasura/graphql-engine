module Hasura.GraphQL.Resolve.Select
  ( convertSelect
  , convertConnectionSelect
  , convertConnectionFuncQuery
  , convertSelectByPKey
  , convertAggSelect
  , convertFuncQuerySimple
  , convertFuncQueryAgg
  , parseColumns
  , processTableSelectionSet
  , AnnSimpleSelect
  ) where

import           Control.Lens                      ((^?), _2)
import           Data.Has
import           Data.Parser.JSONPath
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Data.List.NonEmpty                as NE
import qualified Data.Sequence                     as Seq
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

jsonPathToColExp :: (MonadError QErr m) => T.Text -> m S.SQLExp
jsonPathToColExp t = case parseJSONPath t of
  Left s       -> throw400 ParseFailed $ T.pack $ "parse json path error: " ++ s
  Right jPaths -> return $ S.SEArray $ map elToColExp jPaths
  where
    elToColExp (Key k)   = S.SELit k
    elToColExp (Index i) = S.SELit $ T.pack (show i)


argsToColOp :: (MonadReusability m, MonadError QErr m) => ArgsMap -> m (Maybe RS.ColOp)
argsToColOp args = maybe (return Nothing) toOp $ Map.lookup "path" args
  where
    toJsonPathExp = fmap (RS.ColOp S.jsonbPathOp) . jsonPathToColExp
    toOp v = asPGColTextM v >>= traverse toJsonPathExp

type AnnFlds = RS.AnnFldsG UnresolvedVal

resolveComputedField
  :: ( MonadReusability m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, MonadError QErr m
     )
  => ComputedField -> Field -> m (RS.ComputedFieldSel UnresolvedVal)
resolveComputedField computedField fld = fieldAsPath fld $ do
  funcArgs <- parseFunctionArgs argSeq argFn $ Map.lookup "args" $ _fArguments fld
  let argsWithTableArgument = withTableArgument funcArgs
  case fieldType of
    CFTScalar scalarTy -> do
      colOpM <- argsToColOp $ _fArguments fld
      pure $ RS.CFSScalar $
        RS.ComputedFieldScalarSel qf argsWithTableArgument scalarTy colOpM
    CFTTable (ComputedFieldTable _ cols permFilter permLimit) -> do
      let functionFrom = RS.FromFunction qf argsWithTableArgument Nothing
      RS.CFSTable RS.JASMultipleRows <$> fromField functionFrom cols permFilter permLimit fld
  where
    ComputedField _ function argSeq fieldType = computedField
    ComputedFieldFunction qf _ tableArg _ = function
    argFn = IFAUnknown
    withTableArgument resolvedArgs =
      let argsExp@(RS.FunctionArgsExp positional named) = RS.AEInput <$> resolvedArgs
          tableRowArg = RS.AETableRow Nothing
      in case tableArg of
        FTAFirst      ->
          RS.FunctionArgsExp (tableRowArg:positional) named
        FTANamed argName index ->
          RS.insertFunctionArg argName index tableRowArg argsExp

processTableSelectionSet
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> SelSet -> m AnnFlds
processTableSelectionSet fldTy flds =
  forM (toList flds) $ \fld -> do
    let fldName = _fName fld
    let rqlFldName = FieldName $ G.unName $ G.unAlias $ _fAlias fld
    (rqlFldName,) <$> case fldName of
      "__typename" -> return $ RS.FExp $ G.unName $ G.unNamedType fldTy
      _ -> do
        fldInfo <- getFldInfo fldTy fldName
        case fldInfo of
          RFPGColumn colInfo ->
            RS.mkAnnColField colInfo <$> argsToColOp (_fArguments fld)
          RFComputedField computedField ->
            RS.FComputedField <$> resolveComputedField computedField fld
          RFRelationship (RelationshipField relInfo isAgg colGNameMap tableFilter tableLimit) -> do
            let relTN = riRTable relInfo
                colMapping = riMapping relInfo
                rn = riName relInfo
            if isAgg then do
              aggSel <- fromAggField (RS.FromTable relTN) colGNameMap tableFilter tableLimit fld
              return $ RS.FArr $ RS.ASAgg $ RS.AnnRelG rn colMapping aggSel
            else do
              annSel <- fromField (RS.FromTable relTN) colGNameMap tableFilter tableLimit fld
              let annRel = RS.AnnRelG rn colMapping annSel
              return $ case riType relInfo of
                ObjRel -> RS.FObj annRel
                ArrRel -> RS.FArr $ RS.ASSimple annRel

type TableAggFlds = RS.TableAggFldsG UnresolvedVal

fromAggSelSet
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => PGColGNameMap -> G.NamedType -> SelSet -> m TableAggFlds
fromAggSelSet colGNameMap fldTy selSet = fmap toFields $
  withSelSet selSet $ \f -> do
    let fTy = _fType f
        fSelSet = _fSelSet f
    case _fName f of
      "__typename" -> return $ RS.TAFExp $ G.unName $ G.unNamedType fldTy
      "aggregate"  -> RS.TAFAgg <$> convertAggFld colGNameMap fTy fSelSet
      "nodes"      -> RS.TAFNodes <$> processTableSelectionSet fTy fSelSet
      G.Name t     -> throw500 $ "unexpected field in _agg node: " <> t

fromConnectionSelSet
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> SelSet -> m (RS.ConnectionFields UnresolvedVal)
fromConnectionSelSet fldTy selSet = fmap toFields $
  withSelSet selSet $ \f -> do
    let fTy = _fType f
        fSelSet = _fSelSet f
    case _fName f of
      "__typename" -> return $ RS.ConnectionTypename $ G.unName $ G.unNamedType fldTy
      "pageInfo"   -> RS.ConnectionPageInfo <$> parsePageInfoSelectionSet fTy fSelSet
      "edges"      -> RS.ConnectionEdges <$> parseEdgeSelectionSet fTy fSelSet
      -- "aggregate"  -> RS.TAFAgg <$> convertAggFld colGNameMap fTy fSelSet
      -- "nodes"      -> RS.TAFNodes <$> processTableSelectionSet fTy fSelSet
      G.Name t     -> throw500 $ "unexpected field in _connection node: " <> t

parseEdgeSelectionSet
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> SelSet -> m (RS.EdgeFields UnresolvedVal)
parseEdgeSelectionSet fldTy selSet = fmap toFields $
  withSelSet selSet $ \f -> do
    let fTy = _fType f
        fSelSet = _fSelSet f
    case _fName f of
      "__typename" -> pure $ RS.EdgeTypename $ G.unName $ G.unNamedType fldTy
      "cursor"     -> pure $ RS.EdgeCursor
      "node"       -> RS.EdgeNode <$> processTableSelectionSet fTy fSelSet
      G.Name t     -> throw500 $ "unexpected field in Edge node: " <> t

parsePageInfoSelectionSet
  :: ( MonadReusability m, MonadError QErr m)
  => G.NamedType -> SelSet -> m RS.PageInfoFields
parsePageInfoSelectionSet fldTy selSet =
  fmap toFields $ withSelSet selSet $ \f ->
    case _fName f of
      "__typename"      -> pure $ RS.PageInfoTypename $ G.unName $ G.unNamedType fldTy
      "hasNextPage"     -> pure $ RS.PageInfoHasNextPage
      "hasPreviousPage" -> pure $ RS.PageInfoHasPreviousPage
      "startCursor"     -> pure $ RS.PageInfoStartCursor
      "endCursor"       -> pure $ RS.PageInfoEndCursor
      -- "aggregate"  -> RS.TAFAgg <$> convertAggFld colGNameMap fTy fSelSet
      -- "nodes"      -> RS.TAFNodes <$> processTableSelectionSet fTy fSelSet
      G.Name t          -> throw500 $ "unexpected field in PageInfo node: " <> t

type TableArgs = RS.TableArgsG UnresolvedVal

parseTableArgs
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m
     , Has FieldMap r, Has OrdByCtx r
     )
  => PGColGNameMap -> ArgsMap -> m TableArgs
parseTableArgs colGNameMap args = do
  whereExpM  <- withArgM args "where" parseBoolExp
  ordByExpML <- withArgM args "order_by" parseOrderBy
  let ordByExpM = NE.nonEmpty =<< ordByExpML
  limitExpM  <- withArgM args "limit" parseLimit
  offsetExpM <- withArgM args "offset" $ asPGColumnValue >=> openOpaqueValue >=> txtConverter
  distOnColsML <- withArgM args "distinct_on" $ parseColumns colGNameMap
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
              RS.AOCPG pgCol -> Just pgCol
              _              -> Nothing
          isValid = (colsLen == length initOrdByCols)
                    && all (`elem` initOrdByCols) (toList cols)

      unless isValid $ throwVE
        "\"distinct_on\" columns must match initial \"order_by\" columns"

type AnnSimpleSelect = RS.AnnSimpleSelG UnresolvedVal

fromField
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => RS.SelectFromG UnresolvedVal
  -> PGColGNameMap
  -> AnnBoolExpPartialSQL
  -> Maybe Int
  -> Field -> m AnnSimpleSelect
fromField selFrom colGNameMap permFilter permLimitM fld = fieldAsPath fld $ do
  tableArgs <- parseTableArgs colGNameMap args
  annFlds   <- processTableSelectionSet (_fType fld) $ _fSelSet fld
  let unresolvedPermFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal permFilter
  let tabPerm = RS.TablePerm unresolvedPermFltr permLimitM
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelG annFlds selFrom tabPerm tableArgs strfyNum
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
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has OrdByCtx r
     )
  => AnnInpVal -> m [RS.AnnOrderByItemG UnresolvedVal]
parseOrderBy = fmap concat . withArray f
  where
    f _ = mapM (withObject (getAnnObItems id))

getAnnObItems
  :: ( MonadReusability m
     , MonadError QErr m
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
        let aobCol = f $ RS.AOCPG $ pgiColumn ci
        (_, enumValM) <- asEnumValM v
        ordByItemM <- forM enumValM $ \enumVal -> do
          (ordTy, nullsOrd) <- parseOrderByEnum enumVal
          return $ mkOrdByItemG ordTy aobCol nullsOrd
        return $ maybe [] pure ordByItemM

      OBIRel ri fltr -> do
        let unresolvedFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal fltr
        let annObColFn = f . RS.AOCObj ri unresolvedFltr
        flip withObjectM v $ \nameTy objM ->
          maybe (pure []) (getAnnObItems annObColFn nameTy) objM

      OBIAgg ri relColGNameMap fltr -> do
        let unresolvedFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal fltr
        let aobColFn = f . RS.AOCAgg ri unresolvedFltr
        flip withObjectM v $ \_ objM ->
          maybe (pure []) (parseAggOrdBy relColGNameMap aobColFn) objM

mkOrdByItemG :: S.OrderType -> a -> S.NullsOrder -> OrderByItemG a
mkOrdByItemG ordTy aobCol nullsOrd =
  OrderByItemG (Just $ OrderType ordTy) aobCol (Just $ NullsOrder nullsOrd)

parseAggOrdBy
  :: (MonadReusability m, MonadError QErr m)
  => PGColGNameMap
  -> (RS.AnnAggOrdBy -> RS.AnnObColG UnresolvedVal)
  -> AnnGObject
  -> m [RS.AnnOrderByItemG UnresolvedVal]
parseAggOrdBy colGNameMap f annObj =
  fmap concat <$> forM (OMap.toList annObj) $ \(op, obVal) ->
    case op of
      "count" -> do
        (_, enumValM) <- asEnumValM obVal
        ordByItemM <- forM enumValM $ \enumVal -> do
          (ordTy, nullsOrd) <- parseOrderByEnum enumVal
          return $ mkOrdByItemG ordTy (f RS.AAOCount) nullsOrd
        return $ maybe [] pure ordByItemM

      G.Name opT ->
        flip withObject obVal $ \_ opObObj -> fmap catMaybes $
          forM (OMap.toList opObObj) $ \(colName, eVal) -> do
            (_, enumValM) <- asEnumValM eVal
            forM enumValM $ \enumVal -> do
              (ordTy, nullsOrd) <- parseOrderByEnum enumVal
              col <- pgiColumn <$> resolvePGCol colGNameMap colName
              let aobCol = f $ RS.AAOOp opT col
              return $ mkOrdByItemG ordTy aobCol nullsOrd

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

parseLimit :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m Int
parseLimit v = do
  pgColVal <- openOpaqueValue =<< asPGColumnValue v
  limit <- maybe noIntErr return . pgColValueToInt . pstValue $ _apvValue pgColVal
  -- validate int value
  onlyPositiveInt limit
  return limit
  where
    noIntErr = throwVE "expecting Integer value for \"limit\""

type AnnSimpleSel = RS.AnnSimpleSelG UnresolvedVal

fromFieldByPKey
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => QualifiedTable -> PGColArgMap
  -> AnnBoolExpPartialSQL -> Field -> m AnnSimpleSel
fromFieldByPKey tn colArgMap permFilter fld = fieldAsPath fld $ do
  boolExp <- pgColValToBoolExp colArgMap $ _fArguments fld
  annFlds <- processTableSelectionSet fldTy $ _fSelSet fld
  let tabFrom = RS.FromTable tn
      unresolvedPermFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal
                           permFilter
      tabPerm = RS.TablePerm unresolvedPermFltr Nothing
      tabArgs = RS.noTableArgs { RS._taWhere = Just boolExp}
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelG annFlds tabFrom tabPerm tabArgs strfyNum
  where
    fldTy = _fType fld

convertSelect
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => SelOpCtx -> Field -> m (RS.AnnSimpleSelG UnresolvedVal)
convertSelect opCtx fld =
  withPathK "selectionSet" $
  fromField (RS.FromTable qt) colGNameMap permFilter permLimit fld
  where
    SelOpCtx qt _ colGNameMap permFilter permLimit = opCtx

convertSelectByPKey
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => SelPkOpCtx -> Field -> m (RS.AnnSimpleSelG UnresolvedVal)
convertSelectByPKey opCtx fld =
  withPathK "selectionSet" $
    fromFieldByPKey qt colArgMap permFilter fld
  where
    SelPkOpCtx qt _ permFilter colArgMap = opCtx

-- agg select related
parseColumns
  :: (MonadReusability m, MonadError QErr m)
  => PGColGNameMap -> AnnInpVal -> m [PGCol]
parseColumns allColFldMap val =
  flip withArray val $ \_ vals ->
    forM vals $ \v -> do
      (_, G.EnumValue enumVal) <- asEnumVal v
      pgiColumn <$> resolvePGCol allColFldMap enumVal

convertCount
  :: (MonadReusability m, MonadError QErr m)
  => PGColGNameMap -> ArgsMap -> m S.CountType
convertCount colGNameMap args = do
  columnsM <- withArgM args "columns" $ parseColumns colGNameMap
  isDistinct <- or <$> withArgM args "distinct" parseDistinct
  maybe (return S.CTStar) (mkCType isDistinct) columnsM
  where
    parseDistinct v = do
      val <- openOpaqueValue =<< asPGColumnValue v
      case pstValue $ _apvValue val of
        PGValBoolean b -> return b
        _              ->
          throw500 "expecting Boolean for \"distinct\""

    mkCType isDistinct cols = return $
      bool (S.CTSimple cols) (S.CTDistinct cols) isDistinct

toFields :: [(T.Text, a)] -> RS.Fields a
toFields = map (first FieldName)

convertColFlds
  :: (MonadError QErr m)
  => PGColGNameMap -> G.NamedType -> SelSet -> m RS.ColFlds
convertColFlds colGNameMap ty selSet = fmap toFields $
  withSelSet selSet $ \fld ->
    case _fName fld of
      "__typename" -> return $ RS.PCFExp $ G.unName $ G.unNamedType ty
      n            -> (RS.PCFCol . pgiColumn) <$> resolvePGCol colGNameMap n

convertAggFld
  :: (MonadReusability m, MonadError QErr m)
  => PGColGNameMap -> G.NamedType -> SelSet -> m RS.AggFlds
convertAggFld colGNameMap ty selSet = fmap toFields $
  withSelSet selSet $ \fld -> do
    let fType = _fType fld
        fSelSet = _fSelSet fld
    case _fName fld of
      "__typename" -> return $ RS.AFExp $ G.unName $ G.unNamedType ty
      "count"      -> RS.AFCount <$> convertCount colGNameMap (_fArguments fld)
      n            -> do
        colFlds <- convertColFlds colGNameMap fType fSelSet
        unless (isAggFld n) $ throwInvalidFld n
        return $ RS.AFOp $ RS.AggOp (G.unName n) colFlds
  where
      throwInvalidFld (G.Name t) =
        throw500 $ "unexpected field in _aggregate node: " <> t

type AnnAggSel = RS.AnnAggSelG UnresolvedVal

fromAggField
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => RS.SelectFromG UnresolvedVal
  -> PGColGNameMap
  -> AnnBoolExpPartialSQL
  -> Maybe Int
  -> Field -> m AnnAggSel
fromAggField selectFrom colGNameMap permFilter permLimit fld = fieldAsPath fld $ do
  tableArgs   <- parseTableArgs colGNameMap args
  aggSelFlds  <- fromAggSelSet colGNameMap (_fType fld) (_fSelSet fld)
  let unresolvedPermFltr =
        fmapAnnBoolExp partialSQLExpToUnresolvedVal permFilter
  let tabPerm = RS.TablePerm unresolvedPermFltr permLimit
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelG aggSelFlds selectFrom tabPerm tableArgs strfyNum
  where
    args = _fArguments fld

fromConnectionField
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => RS.SelectFromG UnresolvedVal
  -> AnnBoolExpPartialSQL
  -> Maybe Int
  -> Field -> m (RS.ConnectionSelect UnresolvedVal)
fromConnectionField selectFrom permFilter permLimit fld = fieldAsPath fld $ do
  tableArgs   <- parseConnectionArgs args
  aggSelFlds  <- fromConnectionSelSet (_fType fld) (_fSelSet fld)
  let unresolvedPermFltr =
        fmapAnnBoolExp partialSQLExpToUnresolvedVal permFilter
  let tabPerm = RS.TablePerm unresolvedPermFltr permLimit
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelG aggSelFlds selectFrom tabPerm tableArgs strfyNum
  where
    args = _fArguments fld

parseConnectionArgs
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m
     , Has FieldMap r, Has OrdByCtx r
     )
  => ArgsMap -> m TableArgs
parseConnectionArgs args = do
  whereExpM  <- withArgM args "where" parseBoolExp
  ordByExpML <- withArgM args "order_by" parseOrderBy
  let ordByExpM = NE.nonEmpty =<< ordByExpML
  -- limitExpM  <- withArgM args "limit" parseLimit
  return $ RS.TableArgs whereExpM ordByExpM Nothing Nothing Nothing

convertAggSelect
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => SelOpCtx -> Field -> m (RS.AnnAggSelG UnresolvedVal)
convertAggSelect opCtx fld =
  withPathK "selectionSet" $
  fromAggField (RS.FromTable qt) colGNameMap permFilter permLimit fld
  where
    SelOpCtx qt _ colGNameMap permFilter permLimit = opCtx

convertConnectionSelect
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => SelOpCtx -> Field -> m (RS.ConnectionSelect UnresolvedVal)
convertConnectionSelect opCtx fld =
  withPathK "selectionSet" $
  fromConnectionField (RS.FromTable qt) permFilter permLimit fld
  where
    SelOpCtx qt _ _ permFilter permLimit = opCtx

parseFunctionArgs
  :: (MonadReusability m, MonadError QErr m)
  => Seq.Seq a
  -> (a -> InputFunctionArgument)
  -> Maybe AnnInpVal
  -> m (RS.FunctionArgsExpG UnresolvedVal)
parseFunctionArgs argSeq argFn = withPathK "args" . \case
  Nothing  -> do
    -- The input "args" field is not provided, hence resolve only known
    -- input arguments as positional arguments
    let positionalArgs = mapMaybe ((^? _IFAKnown._2) . argFn) $ toList argSeq
    pure RS.emptyFunctionArgsExp{RS._faePositional = positionalArgs}

  Just val -> flip withObject val $ \_ obj -> do
    (positionalArgs, argsLeft) <- spanMaybeM (parsePositionalArg obj) argSeq
    namedArgs <- Map.fromList . catMaybes <$> traverse (parseNamedArg obj) argsLeft
    pure $ RS.FunctionArgsExp positionalArgs namedArgs
  where
    parsePositionalArg obj inputArg = case argFn inputArg of
      IFAKnown _ resolvedVal -> pure $ Just resolvedVal
      IFAUnknown (FunctionArgItem gqlName _ _) ->
        maybe (pure Nothing) (fmap Just . parseArg) $ OMap.lookup gqlName obj

    parseArg = fmap (maybe (UVSQL S.SENull) mkParameterizablePGValue) . asPGColumnValueM

    parseNamedArg obj inputArg = case argFn inputArg of
      IFAKnown argName resolvedVal ->
        pure $ Just (getFuncArgNameTxt argName, resolvedVal)
      IFAUnknown (FunctionArgItem gqlName maybeSqlName hasDefault) ->
        case OMap.lookup gqlName obj of
          Just argInpVal -> case maybeSqlName of
            Just sqlName -> Just . (getFuncArgNameTxt sqlName,) <$> parseArg argInpVal
            Nothing -> throw400 NotSupported
                       "Only last set of positional arguments can be omitted"
          Nothing -> if not (unHasDefault hasDefault) then
                       throw400 NotSupported "Non default arguments cannot be omitted"
                     else pure Nothing

makeFunctionSelectFrom
  :: (MonadReusability m, MonadError QErr m)
  => QualifiedFunction
  -> FunctionArgSeq
  -> Field
  -> m (RS.SelectFromG UnresolvedVal)
makeFunctionSelectFrom qf argSeq fld = withPathK "args" $ do
  funcArgs <- parseFunctionArgs argSeq argFn $ Map.lookup "args" $ _fArguments fld
  pure $ RS.FromFunction qf (RS.AEInput <$> funcArgs) Nothing
  where
    argFn (IAUserProvided val)         = IFAUnknown val
    argFn (IASessionVariables argName) = IFAKnown argName UVSession

convertFuncQuerySimple
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => FuncQOpCtx -> Field -> m AnnSimpleSelect
convertFuncQuerySimple funcOpCtx fld =
  withPathK "selectionSet" $ fieldAsPath fld $ do
    selectFrom <- makeFunctionSelectFrom qf argSeq fld
    fromField selectFrom colGNameMap permFilter permLimit fld
  where
    FuncQOpCtx qf argSeq _ colGNameMap permFilter permLimit = funcOpCtx

convertFuncQueryAgg
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => FuncQOpCtx -> Field -> m AnnAggSel
convertFuncQueryAgg funcOpCtx fld =
  withPathK "selectionSet" $ fieldAsPath fld $ do
    selectFrom <- makeFunctionSelectFrom qf argSeq fld
    fromAggField selectFrom colGNameMap permFilter permLimit fld
  where
    FuncQOpCtx qf argSeq _ colGNameMap permFilter permLimit = funcOpCtx

convertConnectionFuncQuery
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => FuncQOpCtx -> Field -> m (RS.ConnectionSelect UnresolvedVal)
convertConnectionFuncQuery funcOpCtx fld =
  withPathK "selectionSet" $ fieldAsPath fld $ do
    selectFrom <- makeFunctionSelectFrom qf argSeq fld
    fromConnectionField selectFrom permFilter permLimit fld
  where
    FuncQOpCtx qf argSeq _ _ permFilter permLimit = funcOpCtx
