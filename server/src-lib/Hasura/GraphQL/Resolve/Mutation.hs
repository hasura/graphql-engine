{-# LANGUAGE ScopedTypeVariables #-}

module Hasura.GraphQL.Resolve.Mutation
  ( convertUpdate
  , convertUpdateByPk
  , convertDelete
  , convertDeleteByPk
  , resolveMutationFields
  , buildEmptyMutResp
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Control.Monad.Validate              as MV
import qualified Data.Aeson                          as J
import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Data.HashMap.Strict.InsOrd.Extended as OMap
import qualified Data.Sequence.NonEmpty              as NESeq
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import qualified Hasura.RQL.DML.Delete               as RD
import qualified Hasura.RQL.DML.Returning            as RR
import qualified Hasura.RQL.DML.Update               as RU

import qualified Hasura.RQL.DML.Select               as RS
import qualified Hasura.SQL.DML                      as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Resolve.Select       (processTableSelectionSet)
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal             (currentSession, sessVarFromCurrentSetting)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

resolveMutationFields
  :: ( MonadReusability m, MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> SelSet -> m (RR.MutFldsG UnresolvedVal)
resolveMutationFields ty selSet = fmap (map (first FieldName)) $
  withSelSet selSet $ \fld -> case _fName fld of
    "__typename"    -> return $ RR.MExp $ G.unName $ G.unNamedType ty
    "affected_rows" -> return RR.MCount
    "returning"     -> do
      annFlds <- processTableSelectionSet (_fType fld) $ _fSelSet fld
      annFldsResolved <- traverse
        (traverse (RS.traverseAnnFld convertPGValueToTextValue)) annFlds
      return $ RR.MRet annFldsResolved
    G.Name t        -> throw500 $ "unexpected field in mutation resp : " <> t
  where
    convertPGValueToTextValue = \case
      UVPG annPGVal -> UVSQL <$> txtConverter annPGVal
      UVSessVar colTy sessVar -> pure $ UVSessVar colTy sessVar
      UVSQL sqlExp -> pure $ UVSQL sqlExp
      UVSession    -> pure UVSession

convertRowObj
  :: (MonadReusability m, MonadError QErr m)
  => PGColGNameMap
  -> AnnInpVal
  -> m [(PGCol, UnresolvedVal)]
convertRowObj colGNameMap val =
  flip withObject val $ \_ obj ->
  forM (OMap.toList obj) $ \(k, v) -> do
    prepExpM <- fmap mkParameterizablePGValue <$> asPGColumnValueM v
    pgCol <- pgiColumn <$> resolvePGCol colGNameMap k
    let prepExp = fromMaybe (UVSQL S.SENull) prepExpM
    return (pgCol, prepExp)

type ApplySQLOp =  (PGCol, S.SQLExp) -> S.SQLExp

-- SET x = x <op> <value>
rhsExpOp :: S.SQLOp -> S.TypeAnn -> ApplySQLOp
rhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op (S.SEIden $ toIden col) annExp
  where
    annExp = S.SETyAnn e annTy

lhsExpOp :: S.SQLOp -> S.TypeAnn -> ApplySQLOp
lhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op annExp $ S.SEIden $ toIden col
  where
    annExp = S.SETyAnn e annTy

-- Automatically generate type annotation by looking up the column name
typedRhsExpOp :: S.SQLOp -> S.TypeAnn -> PGColGNameMap -> ApplySQLOp
typedRhsExpOp op defaultAnnTy colGNameMap (colName, e) =
  let annTypeM :: Maybe S.TypeAnn
      annTypeM = do
        fieldType <- pgiType <$> Map.lookup (G.Name $ getPGColTxt colName) colGNameMap
        case fieldType of
          PGColumnScalar x -> return $ S.mkTypeAnn $ PGTypeScalar x
          _ -> Nothing
      annType :: S.TypeAnn
      annType = fromMaybe defaultAnnTy annTypeM
  in rhsExpOp op annType (colName, e)

convObjWithOp
  :: (MonadReusability m, MonadError QErr m)
  => PGColGNameMap -> ApplySQLOp -> AnnInpVal -> m [(PGCol, UnresolvedVal)]
convObjWithOp colGNameMap opFn val =
  flip withObject val $ \_ obj -> forM (OMap.toList obj) $ \(k, v) -> do
  colVal <- openOpaqueValue =<< asPGColumnValue v
  pgCol <- pgiColumn <$> resolvePGCol colGNameMap k
  -- TODO: why are we using txtEncoder here?
  let encVal = txtEncoder $ pstValue $ _apvValue colVal
      sqlExp = opFn (pgCol, encVal)
  return (pgCol, UVSQL sqlExp)

convDeleteAtPathObj
  :: (MonadReusability m, MonadError QErr m)
  => PGColGNameMap -> AnnInpVal -> m [(PGCol, UnresolvedVal)]
convDeleteAtPathObj colGNameMap val =
  flip withObject val $ \_ obj -> forM (OMap.toList obj) $ \(k, v) -> do
    vals <- traverse (openOpaqueValue <=< asPGColumnValue) =<< asArray v
    pgCol <- pgiColumn <$> resolvePGCol colGNameMap k
    let valExps = map (txtEncoder . pstValue . _apvValue) vals
        annEncVal = S.SETyAnn (S.SEArray valExps) S.textArrTypeAnn
        sqlExp = S.SEOpApp S.jsonbDeleteAtPathOp
                 [S.SEIden $ toIden pgCol, annEncVal]
    return (pgCol, UVSQL sqlExp)

convertUpdateP1
  :: forall m . (MonadReusability m, MonadError QErr m)
  => UpdOpCtx -- the update context
  -> (ArgsMap -> m AnnBoolExpUnresolved) -- the bool expression parser
  -> (Field -> m (RR.MutationOutputG UnresolvedVal)) -- the selection set resolver
  -> Field -- the mutation field
  -> m (RU.AnnUpdG UnresolvedVal)
convertUpdateP1 opCtx boolExpParser selectionResolver fld = do
  -- a set expression is same as a row object
  setExpM   <- resolveUpdateOperator "_set" $ convertRowObj colGNameMap
  -- where bool expression to filter column
  whereExp <- boolExpParser args
  -- increment operator on integer columns
  incExpM <- resolveUpdateOperator "_inc" $
    convObjWithOp' $ typedRhsExpOp S.incOp S.numericTypeAnn colGNameMap
  -- append jsonb value
  appendExpM <- resolveUpdateOperator "_append" $
    convObjWithOp' $ rhsExpOp S.jsonbConcatOp S.jsonbTypeAnn
  -- prepend jsonb value
  prependExpM <- resolveUpdateOperator "_prepend" $
    convObjWithOp' $ lhsExpOp S.jsonbConcatOp S.jsonbTypeAnn
  -- delete a key in jsonb object
  deleteKeyExpM <- resolveUpdateOperator "_delete_key" $
    convObjWithOp' $ rhsExpOp S.jsonbDeleteOp S.textTypeAnn
  -- delete an element in jsonb array
  deleteElemExpM <- resolveUpdateOperator "_delete_elem" $
    convObjWithOp' $ rhsExpOp S.jsonbDeleteOp S.intTypeAnn
  -- delete at path in jsonb value
  deleteAtPathExpM <- resolveUpdateOperator "_delete_at_path" $
    convDeleteAtPathObj colGNameMap

  updateItems <- combineUpdateExpressions
                 [ setExpM, incExpM, appendExpM, prependExpM
                 , deleteKeyExpM, deleteElemExpM, deleteAtPathExpM
                 ]

  mutOutput <- selectionResolver fld

  pure $ RU.AnnUpd tn updateItems (unresolvedPermFilter, whereExp) unresolvedPermCheck mutOutput allCols
  where
    convObjWithOp' = convObjWithOp colGNameMap
    allCols = Map.elems colGNameMap
    UpdOpCtx tn _ colGNameMap filterExp checkExpr preSetCols = opCtx
    args = _fArguments fld
    resolvedPreSetItems = Map.toList $ fmap partialSQLExpToUnresolvedVal preSetCols
    unresolvedPermFilter = fmapAnnBoolExp partialSQLExpToUnresolvedVal filterExp
    unresolvedPermCheck = maybe annBoolExpTrue (fmapAnnBoolExp partialSQLExpToUnresolvedVal) checkExpr

    resolveUpdateOperator operator resolveAction =
      (operator,) <$> withArgM args operator resolveAction

    combineUpdateExpressions :: [(G.Name, Maybe [(PGCol, UnresolvedVal)])]
                             -> m [(PGCol, UnresolvedVal)]
    combineUpdateExpressions updateExps = do
      let allOperatorNames = map fst updateExps
          updateItems :: [(G.Name, [(PGCol, UnresolvedVal)])]
          updateItems = mapMaybe (\(op, itemsM) -> (op,) <$> itemsM) updateExps
      -- Atleast any one of operator is expected or preset expressions shouldn't be empty
      if null updateItems && null resolvedPreSetItems then
        throwVE $ "at least any one of " <> showNames allOperatorNames <> " is expected"
      else do
        let itemsWithOps :: [(PGCol, (G.Name, UnresolvedVal))]
            itemsWithOps = concatMap (\(op, items) -> map (second (op,)) items) updateItems
            validateMultiOps col items = do
              when (length items > 1) $ MV.dispute [(col, map fst $ toList items)]
              pure $ snd $ NESeq.head items
            eitherResult :: Either
                              [(PGCol, [G.Name])]
                              (OMap.InsOrdHashMap PGCol UnresolvedVal)
            eitherResult = MV.runValidate $ OMap.traverseWithKey validateMultiOps $
                           OMap.groupTuples itemsWithOps
        case eitherResult of
          -- A column shouldn't be present in more than one operator.
          -- If present, then generated UPDATE statement throws unexpected query error
          Left columnsWithMultiOps -> throwVE $
                                      "column found in multiple operators; "
                                      <> T.intercalate ". "
                                      (map (\(col, ops) -> col <<> " in " <> showNames ops)
                                       columnsWithMultiOps)
          Right items -> pure $ resolvedPreSetItems <> OMap.toList items

convertUpdateGeneric
  :: ( MonadReusability m, MonadError QErr m
     , MonadReader r m
     , Has SQLGenCtx r
     )
  => UpdOpCtx -- the update context
  -> (ArgsMap -> m AnnBoolExpUnresolved) -- the bool exp parser
  -> (Field -> m (RR.MutationOutputG UnresolvedVal)) -- the selection set resolver
  -> Field
  -> m RespTx
convertUpdateGeneric opCtx boolExpParser selectionResolver fld = do
  annUpdUnresolved <- convertUpdateP1 opCtx boolExpParser selectionResolver fld
  (annUpdResolved, prepArgs) <- withPrepArgs $ RU.traverseAnnUpd
                                resolveValPrep annUpdUnresolved
  strfyNum <- stringifyNum <$> asks getter
  let whenNonEmptyItems = return $ RU.updateQueryToTx strfyNum
                          (annUpdResolved, prepArgs)
      whenEmptyItems    = return $ return $
                          buildEmptyMutResp $ RU.uqp1Output annUpdResolved
   -- if there are not set items then do not perform
   -- update and return empty mutation response
  bool whenNonEmptyItems whenEmptyItems $ null $ RU.uqp1SetExps annUpdResolved

convertUpdate
  :: ( MonadReusability m, MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => UpdOpCtx -- the update context
  -> Field -- the mutation field
  -> m RespTx
convertUpdate opCtx =
  convertUpdateGeneric opCtx whereExpressionParser mutationFieldsResolver

convertUpdateByPk
  :: ( MonadReusability m, MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => UpdOpCtx -- the update context
  -> Field -- the mutation field
  -> m RespTx
convertUpdateByPk opCtx field =
  convertUpdateGeneric opCtx boolExpParser tableSelectionAsMutationOutput field
  where
    boolExpParser args =  withArg args "pk_columns" $ \inpVal -> do
      obj <- asObject inpVal
      pgColValToBoolExp (_uocAllCols opCtx) $ Map.fromList $ OMap.toList obj


convertDeleteGeneric
  :: ( MonadReusability m
     , MonadReader r m
     , Has SQLGenCtx r
     )
  => DelOpCtx -- the delete context
  -> (ArgsMap -> m AnnBoolExpUnresolved) -- the bool exp parser
  -> (Field -> m (RR.MutationOutputG UnresolvedVal)) -- the selection set resolver
  -> Field -- the mutation field
  -> m RespTx
convertDeleteGeneric opCtx boolExpParser selectionResolver fld = do
  whereExp <- boolExpParser $ _fArguments fld
  mutOutput  <- selectionResolver fld
  let unresolvedPermFltr =
        fmapAnnBoolExp partialSQLExpToUnresolvedVal filterExp
      annDelUnresolved = RD.AnnDel tn (unresolvedPermFltr, whereExp)
                         mutOutput allCols
  (annDelResolved, prepArgs) <- withPrepArgs $ RD.traverseAnnDel
                                resolveValPrep annDelUnresolved
  strfyNum <- stringifyNum <$> asks getter
  return $ RD.deleteQueryToTx strfyNum (annDelResolved, prepArgs)
  where
    DelOpCtx tn _ colGNameMap filterExp = opCtx
    allCols = Map.elems colGNameMap

convertDelete
  :: ( MonadReusability m, MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => DelOpCtx -- the delete context
  -> Field -- the mutation field
  -> m RespTx
convertDelete opCtx =
  convertDeleteGeneric opCtx whereExpressionParser mutationFieldsResolver

convertDeleteByPk
  :: ( MonadReusability m, MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => DelOpCtx -- the delete context
  -> Field -- the mutation field
  -> m RespTx
convertDeleteByPk opCtx field =
  convertDeleteGeneric opCtx boolExpParser tableSelectionAsMutationOutput field
  where
    boolExpParser =  pgColValToBoolExp (_docAllCols opCtx)

whereExpressionParser
  :: ( MonadReusability m, MonadError QErr m
     , MonadReader r m, Has FieldMap r
     )
  => ArgsMap -> m AnnBoolExpUnresolved
whereExpressionParser args = withArg args "where" parseBoolExp

mutationFieldsResolver
  :: ( MonadReusability m, MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => Field -> m (RR.MutationOutputG UnresolvedVal)
mutationFieldsResolver field =
  RR.MOutMultirowFields <$> resolveMutationFields (_fType field) (_fSelSet field)

tableSelectionAsMutationOutput
  :: ( MonadReusability m, MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => Field -> m (RR.MutationOutputG UnresolvedVal)
tableSelectionAsMutationOutput field =
  RR.MOutSinglerowObject <$> processTableSelectionSet (_fType field) (_fSelSet field)

-- | build mutation response for empty objects
buildEmptyMutResp :: RR.MutationOutput -> EncJSON
buildEmptyMutResp = mkTx
  where
    mkTx = \case
      RR.MOutMultirowFields mutFlds -> encJFromJValue $ OMap.fromList $ map (second convMutFld) mutFlds
      RR.MOutSinglerowObject _       -> encJFromJValue $ J.Object mempty
    -- generate empty mutation response
    convMutFld = \case
      RR.MCount -> J.toJSON (0 :: Int)
      RR.MExp e -> J.toJSON e
      RR.MRet _ -> J.toJSON ([] :: [J.Value])

resolveValPrep
  :: (MonadState PrepArgs m)
  => UnresolvedVal -> m S.SQLExp
resolveValPrep = \case
  UVPG annPGVal -> prepare annPGVal
  UVSessVar colTy sessVar -> sessVarFromCurrentSetting colTy sessVar
  UVSQL sqlExp -> pure sqlExp
  UVSession -> pure currentSession
