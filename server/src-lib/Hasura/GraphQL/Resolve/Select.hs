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
  , resolveNodeId
  , convertNodeSelect
  , AnnSimpleSelect
  ) where

import           Control.Lens                         (to, (^..), (^?), _2)
import           Data.Has
import           Data.Parser.JSONPath
import           Hasura.Prelude

import qualified Data.Aeson                           as J
import qualified Data.Aeson.Internal                  as J
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.HashMap.Strict                  as Map
import qualified Data.HashMap.Strict.InsOrd           as OMap
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Sequence                        as Seq
import qualified Data.Text                            as T
import qualified Language.GraphQL.Draft.Syntax        as G

import qualified Hasura.RQL.DML.Select                as RS
import qualified Hasura.SQL.DML                       as S

import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Schema                (isAggregateField)
import           Hasura.GraphQL.Schema.Common         (mkTableTy)
import           Hasura.GraphQL.Validate
import           Hasura.GraphQL.Validate.SelectionSet
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal              (onlyPositiveInt)
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.SQL.Types
import           Hasura.SQL.Value

jsonPathToColExp :: (MonadError QErr m) => T.Text -> m (Maybe S.SQLExp)
jsonPathToColExp t = case parseJSONPath t of
  Left s       -> throw400 ParseFailed $ T.pack $ "parse json path error: " ++ s
  Right []     -> return Nothing
  Right jPaths -> return $ Just $ S.SEArray $ map elToColExp jPaths
  where
    elToColExp (Key k)   = S.SELit k
    elToColExp (Index i) = S.SELit $ T.pack (show i)


argsToColumnOp :: (MonadReusability m, MonadError QErr m) => ArgsMap -> m (Maybe RS.ColumnOp)
argsToColumnOp args = case Map.lookup "path" args of
  Nothing -> return Nothing
  Just txt -> do
    mColTxt <- asPGColTextM txt
    mColExps <- maybe (return Nothing) jsonPathToColExp mColTxt
    pure $ RS.ColumnOp S.jsonbPathOp <$> mColExps

type AnnFields = RS.AnnFieldsG UnresolvedVal

resolveComputedField
  :: ( MonadReusability m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, MonadError QErr m
     )
  => ComputedField -> Field -> m (RS.ComputedFieldSelect UnresolvedVal)
resolveComputedField computedField fld = fieldAsPath fld $ do
  funcArgs <- parseFunctionArgs argSeq argFn $ Map.lookup "args" $ _fArguments fld
  let argsWithTableArgument = withTableAndSessionArgument funcArgs
  case fieldType of
    CFTScalar scalarTy -> do
      colOpM <- argsToColumnOp $ _fArguments fld
      pure $ RS.CFSScalar $
        RS.ComputedFieldScalarSelect qf argsWithTableArgument scalarTy colOpM
    CFTTable (ComputedFieldTable _ cols permFilter permLimit) -> do
      let functionFrom = RS.FromFunction qf argsWithTableArgument Nothing
      RS.CFSTable RS.JASMultipleRows <$> fromField functionFrom cols permFilter permLimit fld
  where
    ComputedField _ function argSeq fieldType = computedField
    ComputedFieldFunction qf _ tableArg sessionArg _ = function
    argFn :: FunctionArgItem -> InputFunctionArgument
    argFn = IFAUnknown
    withTableAndSessionArgument :: RS.FunctionArgsExpG        UnresolvedVal
                                -> RS.FunctionArgsExpTableRow UnresolvedVal
    withTableAndSessionArgument resolvedArgs =
      let argsExp@(RS.FunctionArgsExp positional named) = RS.AEInput <$> resolvedArgs
          tableRowArg = RS.AETableRow Nothing
          withTable = case tableArg of
            FTAFirst      ->
              RS.FunctionArgsExp (tableRowArg:positional) named
            FTANamed argName index ->
              RS.insertFunctionArg argName index tableRowArg argsExp
          sessionArgVal = RS.AESession UVSession
          alsoWithSession = case sessionArg of
            Nothing -> withTable
            Just (FunctionSessionArgument argName index) ->
              RS.insertFunctionArg argName index sessionArgVal withTable
      in alsoWithSession

processTableSelectionSet
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> ObjectSelectionSet -> m AnnFields
processTableSelectionSet fldTy flds =
  fmap (map (\(a, b) -> (FieldName a, b))) $ traverseObjectSelectionSet flds $ \fld -> do
    let fldName = _fName fld
    case fldName of
      "__typename" -> return $ RS.AFExpression $ G.unName $ G.unNamedType fldTy
      _ -> do
        fldInfo <- getFldInfo fldTy fldName
        case fldInfo of
          RFNodeId tn pkeys -> pure $ RS.AFNodeId tn pkeys
          RFPGColumn colInfo ->
            RS.mkAnnColumnField colInfo <$> argsToColumnOp (_fArguments fld)
          RFComputedField computedField ->
            RS.AFComputedField <$> resolveComputedField computedField fld
          RFRelationship (RelationshipField relInfo fieldKind colGNameMap tableFilter tableLimit) -> do
            let relTN = riRTable relInfo
                colMapping = riMapping relInfo
                rn = riName relInfo
            case fieldKind of
              RFKSimple -> do
                annSel <- fromField (RS.FromTable relTN) colGNameMap tableFilter tableLimit fld
                let annRel = RS.AnnRelationSelectG rn colMapping annSel
                pure $ case riType relInfo of
                  ObjRel -> RS.AFObjectRelation annRel
                  ArrRel -> RS.AFArrayRelation $ RS.ASSimple annRel
              RFKAggregate -> do
                aggSel <- fromAggField (RS.FromTable relTN) colGNameMap tableFilter tableLimit fld
                pure $ RS.AFArrayRelation $ RS.ASAggregate $ RS.AnnRelationSelectG rn colMapping aggSel
              RFKConnection pkCols -> do
                connSel <- fromConnectionField (RS.FromTable relTN) pkCols tableFilter tableLimit fld
                pure $ RS.AFArrayRelation $ RS.ASConnection $ RS.AnnRelationSelectG rn colMapping connSel

          RFRemoteRelationship info ->
            pure $ RS.AFRemote $ RS.RemoteSelect
                                (unValidateArgsMap $ _fArguments fld) -- Unvalidate the input arguments
                                (unValidateSelectionSet $ _fSelSet fld) -- Unvalidate the selection fields
                                (_rfiHasuraFields info)
                                (_rfiRemoteFields info)
                                (_rfiRemoteSchema info)

type TableAggregateFields = RS.TableAggregateFieldsG UnresolvedVal

fromAggSelSet
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => PGColGNameMap -> G.NamedType -> ObjectSelectionSet -> m TableAggregateFields
fromAggSelSet colGNameMap fldTy selSet = fmap toFields $
  traverseObjectSelectionSet selSet $ \Field{..} ->
    case _fName of
      "__typename" -> return $ RS.TAFExp $ G.unName $ G.unNamedType fldTy
      "aggregate"  -> do
        objSelSet <-  asObjectSelectionSet _fSelSet
        RS.TAFAgg <$> convertAggregateField colGNameMap _fType objSelSet
      "nodes"      -> do
        objSelSet <-  asObjectSelectionSet _fSelSet
        RS.TAFNodes <$> processTableSelectionSet _fType objSelSet
      G.Name t     -> throw500 $ "unexpected field in _agg node: " <> t

fromConnectionSelSet
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> ObjectSelectionSet -> m (RS.ConnectionFields UnresolvedVal)
fromConnectionSelSet fldTy selSet = fmap toFields $
  traverseObjectSelectionSet selSet $ \Field{..} ->
    case _fName of
      "__typename" -> return $ RS.ConnectionTypename $ G.unName $ G.unNamedType fldTy
      "pageInfo"   -> do
        fSelSet <-  asObjectSelectionSet _fSelSet
        RS.ConnectionPageInfo <$> parsePageInfoSelectionSet _fType fSelSet
      "edges"      -> do
        fSelSet <-  asObjectSelectionSet _fSelSet
        RS.ConnectionEdges <$> parseEdgeSelectionSet _fType fSelSet
      -- "aggregate"  -> RS.TAFAgg <$> convertAggregateField colGNameMap fTy fSelSet
      -- "nodes"      -> RS.TAFNodes <$> processTableSelectionSet fTy fSelSet
      G.Name t     -> throw500 $ "unexpected field in _connection node: " <> t

parseEdgeSelectionSet
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> ObjectSelectionSet -> m (RS.EdgeFields UnresolvedVal)
parseEdgeSelectionSet fldTy selSet = fmap toFields $
  traverseObjectSelectionSet selSet $ \f -> do
    let fTy = _fType f
    case _fName f of
      "__typename" -> pure $ RS.EdgeTypename $ G.unName $ G.unNamedType fldTy
      "cursor"     -> pure RS.EdgeCursor
      "node"       -> do
        fSelSet <- asObjectSelectionSet $ _fSelSet f
        RS.EdgeNode <$> processTableSelectionSet fTy fSelSet
      G.Name t     -> throw500 $ "unexpected field in Edge node: " <> t

parsePageInfoSelectionSet
  :: ( MonadReusability m, MonadError QErr m)
  => G.NamedType -> ObjectSelectionSet -> m RS.PageInfoFields
parsePageInfoSelectionSet fldTy selSet =
  fmap toFields $ traverseObjectSelectionSet selSet $ \f ->
    case _fName f of
      "__typename"      -> pure $ RS.PageInfoTypename $ G.unName $ G.unNamedType fldTy
      "hasNextPage"     -> pure RS.PageInfoHasNextPage
      "hasPreviousPage" -> pure RS.PageInfoHasPreviousPage
      "startCursor"     -> pure RS.PageInfoStartCursor
      "endCursor"       -> pure RS.PageInfoEndCursor
      -- "aggregate"  -> RS.TAFAgg <$> convertAggregateField colGNameMap fTy fSelSet
      -- "nodes"      -> RS.TAFNodes <$> processTableSelectionSet fTy fSelSet
      G.Name t          -> throw500 $ "unexpected field in PageInfo node: " <> t

type SelectArgs = RS.SelectArgsG UnresolvedVal

parseSelectArgs
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m
     , Has FieldMap r, Has OrdByCtx r
     )
  => PGColGNameMap -> ArgsMap -> m SelectArgs
parseSelectArgs colGNameMap args = do
  whereExpM  <- withArgM args "where" parseBoolExp
  ordByExpML <- withArgM args "order_by" parseOrderBy
  let ordByExpM = NE.nonEmpty =<< ordByExpML
  limitExpM  <- withArgM args "limit" $
                parseNonNegativeInt "expecting Integer value for \"limit\""
  offsetExpM <- withArgM args "offset" $ asPGColumnValue >=> openOpaqueValue >=> txtConverter
  distOnColsML <- withArgM args "distinct_on" $ parseColumns colGNameMap
  let distOnColsM = NE.nonEmpty =<< distOnColsML
  mapM_ (validateDistOn ordByExpM) distOnColsM
  return $ RS.SelectArgs whereExpM ordByExpM limitExpM offsetExpM distOnColsM
  where
    validateDistOn Nothing _ = return ()
    validateDistOn (Just ordBys) cols = withPathK "args" $ do
      let colsLen = length cols
          initOrdBys = take colsLen $ toList ordBys
          initOrdByCols = flip mapMaybe initOrdBys $ \ob ->
            case obiColumn  ob of
              RS.AOCColumn pgCol -> Just $ pgiColumn pgCol
              _                  -> Nothing
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
  tableArgs <- parseSelectArgs colGNameMap args
  selSet <- asObjectSelectionSet $ _fSelSet fld
  annFlds   <- processTableSelectionSet (_fType fld) selSet
  let unresolvedPermFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal permFilter
  let tabPerm = RS.TablePerm unresolvedPermFltr permLimitM
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelectG annFlds selFrom tabPerm tableArgs strfyNum
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
  => AnnInpVal
  -> m [RS.AnnOrderByItemG UnresolvedVal]
parseOrderBy = fmap concat . withArray f
  where
    f _ = mapM (withObject (getAnnObItems id))

getAnnObItems
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has OrdByCtx r
     )
  => (RS.AnnOrderByElement UnresolvedVal -> RS.AnnOrderByElement UnresolvedVal)
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
        let aobCol = f $ RS.AOCColumn ci
        (_, enumValM) <- asEnumValM v
        ordByItemM <- forM enumValM $ \enumVal -> do
          (ordTy, nullsOrd) <- parseOrderByEnum enumVal
          return $ mkOrdByItemG ordTy aobCol nullsOrd
        return $ maybe [] pure ordByItemM

      OBIRel ri fltr -> do
        let unresolvedFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal fltr
        let annObColFn = f . RS.AOCObjectRelation ri unresolvedFltr
        flip withObjectM v $ \nameTy objM ->
          maybe (pure []) (getAnnObItems annObColFn nameTy) objM

      OBIAgg ri relColGNameMap fltr -> do
        let unresolvedFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal fltr
        let aobColFn = f . RS.AOCArrayAggregation ri unresolvedFltr
        flip withObjectM v $ \_ objM ->
          maybe (pure []) (parseAggOrdBy relColGNameMap aobColFn) objM

mkOrdByItemG :: S.OrderType -> a -> S.NullsOrder -> OrderByItemG a
mkOrdByItemG ordTy aobCol nullsOrd =
  OrderByItemG (Just $ OrderType ordTy) aobCol (Just $ NullsOrder nullsOrd)

parseAggOrdBy
  :: (MonadReusability m, MonadError QErr m)
  => PGColGNameMap
  -> (RS.AnnAggregateOrderBy -> RS.AnnOrderByElement UnresolvedVal)
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

      G.Name opText ->
        flip withObject obVal $ \_ opObObj -> fmap catMaybes $
          forM (OMap.toList opObObj) $ \(colName, eVal) -> do
            (_, enumValM) <- asEnumValM eVal
            forM enumValM $ \enumVal -> do
              (ordTy, nullsOrd) <- parseOrderByEnum enumVal
              col <- resolvePGCol colGNameMap colName
              let aobCol = f $ RS.AAOOp opText col
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

parseNonNegativeInt
  :: (MonadReusability m, MonadError QErr m) => Text -> AnnInpVal -> m Int
parseNonNegativeInt errMsg v = do
  pgColVal <- openOpaqueValue =<< asPGColumnValue v
  limit <- maybe (throwVE errMsg) return . pgColValueToInt . pstValue $ _apvValue pgColVal
  -- validate int value
  onlyPositiveInt limit
  return limit

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
  selSet <- asObjectSelectionSet $ _fSelSet fld
  annFlds <- processTableSelectionSet fldTy selSet
  let tabFrom = RS.FromTable tn
      unresolvedPermFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal
                           permFilter
      tabPerm = RS.TablePerm unresolvedPermFltr Nothing
      tabArgs = RS.noSelectArgs { RS._saWhere = Just boolExp}
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelectG annFlds tabFrom tabPerm tabArgs strfyNum
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

convertColumnFields
  :: (MonadError QErr m)
  => PGColGNameMap -> G.NamedType -> ObjectSelectionSet -> m RS.ColumnFields
convertColumnFields colGNameMap ty selSet = fmap toFields $
  traverseObjectSelectionSet selSet $ \fld ->
    case _fName fld of
      "__typename" -> return $ RS.PCFExp $ G.unName $ G.unNamedType ty
      n            -> RS.PCFCol . pgiColumn <$> resolvePGCol colGNameMap n

convertAggregateField
  :: (MonadReusability m, MonadError QErr m)
  => PGColGNameMap -> G.NamedType -> ObjectSelectionSet -> m RS.AggregateFields
convertAggregateField colGNameMap ty selSet = fmap toFields $
  traverseObjectSelectionSet selSet $ \Field{..} ->
    case _fName of
      "__typename" -> return $ RS.AFExp $ G.unName $ G.unNamedType ty
      "count"      -> RS.AFCount <$> convertCount colGNameMap _fArguments
      n            -> do
        fSelSet <- asObjectSelectionSet _fSelSet
        colFlds <- convertColumnFields colGNameMap _fType fSelSet
        unless (isAggregateField n) $ throwInvalidFld n
        return $ RS.AFOp $ RS.AggregateOp (G.unName n) colFlds
  where
      throwInvalidFld (G.Name t) =
        throw500 $ "unexpected field in _aggregate node: " <> t

type AnnAggregateSelect = RS.AnnAggregateSelectG UnresolvedVal

fromAggField
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => RS.SelectFromG UnresolvedVal
  -> PGColGNameMap
  -> AnnBoolExpPartialSQL
  -> Maybe Int
  -> Field -> m AnnAggregateSelect
fromAggField selectFrom colGNameMap permFilter permLimit fld = fieldAsPath fld $ do
  tableArgs   <- parseSelectArgs colGNameMap args
  selSet <- asObjectSelectionSet $ _fSelSet fld
  aggSelFlds  <- fromAggSelSet colGNameMap (_fType fld) selSet
  let unresolvedPermFltr =
        fmapAnnBoolExp partialSQLExpToUnresolvedVal permFilter
  let tabPerm = RS.TablePerm unresolvedPermFltr permLimit
  strfyNum <- stringifyNum <$> asks getter
  return $ RS.AnnSelectG aggSelFlds selectFrom tabPerm tableArgs strfyNum
  where
    args = _fArguments fld

fromConnectionField
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => RS.SelectFromG UnresolvedVal
  -> NonEmpty PGColumnInfo
  -> AnnBoolExpPartialSQL
  -> Maybe Int
  -> Field -> m (RS.ConnectionSelect UnresolvedVal)
fromConnectionField selectFrom pkCols permFilter permLimit fld = fieldAsPath fld $ do
  (tableArgs, slice, split)   <- parseConnectionArgs pkCols args
  selSet <- asObjectSelectionSet $ _fSelSet fld
  connSelFlds  <- fromConnectionSelSet (_fType fld) selSet
  strfyNum <- stringifyNum <$> asks getter
  let unresolvedPermFltr =
        fmapAnnBoolExp partialSQLExpToUnresolvedVal permFilter
      tabPerm = RS.TablePerm unresolvedPermFltr permLimit
      annSel = RS.AnnSelectG connSelFlds selectFrom tabPerm tableArgs strfyNum
  pure $ RS.ConnectionSelect pkCols split slice annSel
  where
    args = _fArguments fld

parseConnectionArgs
  :: forall r m.
     ( MonadReusability m, MonadError QErr m, MonadReader r m
     , Has FieldMap r, Has OrdByCtx r
     )
  => NonEmpty PGColumnInfo
  -> ArgsMap
  -> m ( SelectArgs
       , Maybe RS.ConnectionSlice
       , Maybe (NE.NonEmpty (RS.ConnectionSplit UnresolvedVal))
       )
parseConnectionArgs pKeyColumns args = do
  whereExpM  <- withArgM args "where" parseBoolExp
  ordByExpML <- withArgM args "order_by" parseOrderBy

  slice <- case (Map.lookup "first" args, Map.lookup "last" args) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Just _)   -> throwVE "\"first\" and \"last\" are not allowed at once"
    (Just v, Nothing)  -> Just . RS.SliceFirst <$> parseNonNegativeInt
      "expecting Integer value for \"first\"" v
    (Nothing, Just v)  -> Just . RS.SliceLast <$> parseNonNegativeInt
      "expecting Integer value for \"last\"" v

  maybeSplit <- case (Map.lookup "after" args, Map.lookup "before" args) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Just _)   -> throwVE "\"after\" and \"before\" are not allowed at once"
    (Just v, Nothing)  -> fmap ((RS.CSKAfter,) . base64Decode) <$> asPGColTextM v
    (Nothing, Just v)  -> fmap ((RS.CSKBefore,) . base64Decode) <$> asPGColTextM v

  let ordByExpM = NE.nonEmpty =<< appendPrimaryKeyOrderBy <$> ordByExpML
      tableArgs = RS.SelectArgs whereExpM ordByExpM Nothing Nothing Nothing

  split <- mapM (uncurry (validateConnectionSplit ordByExpM)) maybeSplit
  pure (tableArgs, slice, split)
  where
    appendPrimaryKeyOrderBy :: [RS.AnnOrderByItemG v] -> [RS.AnnOrderByItemG v]
    appendPrimaryKeyOrderBy orderBys =
      let orderByColumnNames =
            orderBys ^.. traverse . to obiColumn . RS._AOCColumn . to pgiColumn
          pkeyOrderBys = flip mapMaybe (toList pKeyColumns) $ \pgColumnInfo ->
                         if pgiColumn pgColumnInfo `elem` orderByColumnNames then Nothing
                         else Just $ OrderByItemG Nothing (RS.AOCColumn pgColumnInfo) Nothing
      in orderBys <> pkeyOrderBys

    validateConnectionSplit
      :: Maybe (NonEmpty (RS.AnnOrderByItemG UnresolvedVal))
      -> RS.ConnectionSplitKind
      -> BL.ByteString
      -> m (NonEmpty (RS.ConnectionSplit UnresolvedVal))
    validateConnectionSplit maybeOrderBys splitKind cursorSplit = do
      cursorValue <- either (const throwInvalidCursor) pure $
                      J.eitherDecode cursorSplit
      case maybeOrderBys of
        Nothing -> forM pKeyColumns $
          \pgColumnInfo -> do
            let columnJsonPath = [J.Key $ getPGColTxt $ pgiColumn pgColumnInfo]
            pgColumnValue <- maybe throwInvalidCursor pure $ iResultToMaybe $
                             executeJSONPath columnJsonPath cursorValue
            pgValue <- parsePGScalarValue (pgiType pgColumnInfo) pgColumnValue
            let unresolvedValue = UVPG $ AnnPGVal Nothing False pgValue
            pure $ RS.ConnectionSplit splitKind unresolvedValue $
                   OrderByItemG Nothing (RS.AOCColumn pgColumnInfo) Nothing
        Just orderBys ->
          forM orderBys $ \orderBy -> do
            let OrderByItemG orderType annObCol nullsOrder = orderBy
            orderByItemValue <- maybe throwInvalidCursor pure $ iResultToMaybe $
                                executeJSONPath (getPathFromOrderBy annObCol) cursorValue
            pgValue <- parsePGScalarValue (getOrderByColumnType annObCol) orderByItemValue
            let unresolvedValue = UVPG $ AnnPGVal Nothing False pgValue
            pure $ RS.ConnectionSplit splitKind unresolvedValue $
                   OrderByItemG orderType (() <$ annObCol) nullsOrder
      where
        throwInvalidCursor = throwVE "the \"after\" or \"before\" cursor is invalid"

        iResultToMaybe = \case
          J.ISuccess v -> Just v
          J.IError{}   -> Nothing

        getPathFromOrderBy = \case
          RS.AOCColumn pgColInfo ->
            let pathElement = J.Key $ getPGColTxt $ pgiColumn pgColInfo
            in [pathElement]
          RS.AOCObjectRelation relInfo _ obCol ->
            let pathElement = J.Key $ relNameToTxt $ riName relInfo
            in pathElement : getPathFromOrderBy obCol
          RS.AOCArrayAggregation relInfo _ aggOb ->
            let fieldName = J.Key $ relNameToTxt (riName relInfo) <> "_aggregate"
            in fieldName : case aggOb of
                 RS.AAOCount    -> [J.Key "count"]
                 RS.AAOOp t col -> [J.Key t, J.Key $ getPGColTxt $ pgiColumn col]

        getOrderByColumnType = \case
          RS.AOCColumn pgColInfo -> pgiType pgColInfo
          RS.AOCObjectRelation _ _ obCol -> getOrderByColumnType obCol
          RS.AOCArrayAggregation _ _ aggOb ->
            case aggOb of
              RS.AAOCount        -> PGColumnScalar PGInteger
              RS.AAOOp _ colInfo -> pgiType colInfo

convertAggSelect
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => SelOpCtx -> Field -> m (RS.AnnAggregateSelectG UnresolvedVal)
convertAggSelect opCtx fld =
  withPathK "selectionSet" $
  fromAggField (RS.FromTable qt) colGNameMap permFilter permLimit fld
  where
    SelOpCtx qt _ colGNameMap permFilter permLimit = opCtx

convertConnectionSelect
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => NonEmpty PGColumnInfo -> SelOpCtx -> Field -> m (RS.ConnectionSelect UnresolvedVal)
convertConnectionSelect pkCols opCtx fld =
  withPathK "selectionSet" $
  fromConnectionField (RS.FromTable qt) pkCols permFilter permLimit fld
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
  => FuncQOpCtx -> Field -> m AnnAggregateSelect
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
  => NonEmpty PGColumnInfo -> FuncQOpCtx -> Field -> m (RS.ConnectionSelect UnresolvedVal)
convertConnectionFuncQuery pkCols funcOpCtx fld =
  withPathK "selectionSet" $ fieldAsPath fld $ do
    selectFrom <- makeFunctionSelectFrom qf argSeq fld
    fromConnectionField selectFrom pkCols permFilter permLimit fld
  where
    FuncQOpCtx qf argSeq _ _ permFilter permLimit = funcOpCtx

resolveNodeId
  :: forall m. ( MonadError QErr m
               , MonadReusability m
               )
  => Field -> m NodeIdData
resolveNodeId field =
  withPathK "selectionSet" $ fieldAsPath field $ do
    nodeIdText <- asPGColText =<< getArg (_fArguments field) "id"
    either (const throwInvalidNodeId) pure $
      J.eitherDecode $ base64Decode nodeIdText
  where
    throwInvalidNodeId = throwVE "the node id is invalid"

convertNodeSelect
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     )
  => SelOpCtx
  -> Map.HashMap PGCol J.Value
  -> Field
  -> m (RS.AnnSimpleSelG UnresolvedVal)
convertNodeSelect selOpCtx pkeyColumnValues field =
  withPathK "selectionSet" $ fieldAsPath field $ do
    -- Parse selection set as interface
    ifaceSelectionSet <- asInterfaceSelectionSet $ _fSelSet field
    let tableObjectType = mkTableTy table
        selSet = getMemberSelectionSet tableObjectType ifaceSelectionSet
        unresolvedPermFilter = fmapAnnBoolExp partialSQLExpToUnresolvedVal permFilter
        tablePerm = RS.TablePerm unresolvedPermFilter permLimit
    -- Resolve the table selection set
    annFields <- processTableSelectionSet tableObjectType selSet
    -- Resolve the Node id primary key column values
    unresolvedPkeyValues <- flip Map.traverseWithKey pkeyColumnValues $
      \pgColumn jsonValue -> case Map.lookup pgColumn pgColumnMap of
        Nothing -> throwVE $ "column " <> pgColumn <<> " not found"
        Just columnInfo -> (,columnInfo) . UVPG . AnnPGVal Nothing False <$>
                           parsePGScalarValue (pgiType columnInfo) jsonValue
    -- Generate the bool expression from the primary key column values
    let pkeyBoolExp = BoolAnd $ flip map (Map.elems unresolvedPkeyValues) $
          \(unresolvedValue, columnInfo) -> (BoolFld . AVCol columnInfo) [AEQ True unresolvedValue]
        selectArgs = RS.noSelectArgs{RS._saWhere = Just pkeyBoolExp}
    strfyNum <- stringifyNum <$> asks getter
    pure $ RS.AnnSelectG annFields (RS.FromTable table) tablePerm selectArgs strfyNum
  where
    SelOpCtx table _ allColumns permFilter permLimit = selOpCtx
    pgColumnMap = mapFromL pgiColumn $ Map.elems allColumns
