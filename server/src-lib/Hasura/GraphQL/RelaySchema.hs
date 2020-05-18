module Hasura.GraphQL.RelaySchema where

import           Control.Lens.Extended          hiding (op)

import qualified Data.HashMap.Strict            as Map
import qualified Data.HashSet                   as Set

import qualified Data.Text                      as T

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils            (duplicates)
import           Hasura.SQL.Types

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Builder
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Function
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Schema.Select

mkRelayGCtxMap
  :: forall m. (MonadError QErr m)
  => TableCache -> FunctionCache -> m GCtxMap
mkRelayGCtxMap tableCache functionCache = do
  typesMapL <- mapM (mkRelayGCtxMapTable tableCache functionCache) $
               filter (tableFltr . _tiCoreInfo) $ Map.elems tableCache
  typesMap <- combineTypes typesMapL
  let gCtxMap  = flip Map.map typesMap $
                 \(ty, flds) -> mkGCtx ty flds mempty
  return gCtxMap
  where
    tableFltr ti = not (isSystemDefined $ _tciSystemDefined ti) && isValidObjectName (_tciName ti)

    combineTypes
      :: [Map.HashMap RoleName (TyAgg, RootFields)]
      -> m (Map.HashMap RoleName (TyAgg, RootFields))
    combineTypes maps = do
      let listMap = foldr (Map.unionWith (++) . Map.map pure) mempty maps
      flip Map.traverseWithKey listMap $ \_ typeList -> do
        let tyAgg = mconcat $ map (^. _1) typeList
        rootFields <- combineRootFields $ map (^. _2) typeList
        pure (tyAgg, rootFields)

    combineRootFields :: [RootFields] -> m RootFields
    combineRootFields rootFields = do
      let duplicateQueryFields = duplicates $
            concatMap (Map.keys . _rootQueryFields) rootFields
          duplicateMutationFields = duplicates $
            concatMap (Map.keys . _rootMutationFields) rootFields

      -- TODO: The following exception should result in inconsistency
      when (not $ null duplicateQueryFields) $
        throw400 Unexpected $ "following query root fields are duplicated: "
        <> showNames duplicateQueryFields

      when (not $ null duplicateMutationFields) $
        throw400 Unexpected $ "following mutation root fields are duplicated: "
        <> showNames duplicateMutationFields

      pure $ mconcat rootFields

mkRelayGCtxMapTable
  :: (MonadError QErr m)
  => TableCache
  -> FunctionCache
  -> TableInfo
  -> m (Map.HashMap RoleName (TyAgg, RootFields))
mkRelayGCtxMapTable tableCache funcCache tabInfo = do
  m <- flip Map.traverseWithKey rolePerms $
       mkRelayGCtxRole tableCache tn descM fields primaryKey tabFuncs
  adminSelFlds <- mkAdminSelFlds fields tableCache
  let adminCtx = mkRelayGCtxRole' tn descM (Just (True, adminSelFlds))
                 primaryKey tabFuncs
  return $ Map.insert adminRole (adminCtx, adminRootFlds) m
  where
    TableInfo coreInfo rolePerms _ = tabInfo
    TableCoreInfo tn descM _ fields primaryKey _ _ _ _ _ = coreInfo
    tabFuncs = filter (isValidObjectName . fiName) $
               getFuncsOfTable tn funcCache
    adminRootFlds =
      getRelayRootFldsRole' tn primaryKey fields tabFuncs
      (Just (noFilter, Nothing, [], True))

mkRelayGCtxRole
  :: (MonadError QErr m)
  => TableCache
  -> QualifiedTable
  -> Maybe PGDescription
  -> FieldInfoMap FieldInfo
  -> Maybe (PrimaryKey PGColumnInfo)
  -> [FunctionInfo]
  -> RoleName
  -> RolePermInfo
  -> m (TyAgg, RootFields)
mkRelayGCtxRole tableCache tn descM fields primaryKey funcs role permInfo = do
  selPermM <- mapM (getSelPerm tableCache fields role) $ _permSel permInfo
  let tyAgg = mkRelayGCtxRole' tn descM selPermM primaryKey funcs
      rootFlds = getRelayRootFldsRole' tn primaryKey fields funcs
                 (mkSel <$> _permSel permInfo)
  return (tyAgg, rootFlds)
  where
    mkSel s = ( spiFilter s, spiLimit s
              , spiRequiredHeaders s, spiAllowAgg s
              )

mkRelayGCtxRole'
  :: QualifiedTable
  -> Maybe PGDescription
  -- ^ Postgres description
  -> Maybe (Bool, [SelField])
  -- ^ select permission
  -> Maybe (PrimaryKey PGColumnInfo)
  -> [FunctionInfo]
  -- ^ all functions
  -> TyAgg
mkRelayGCtxRole' tn descM selPermM pkeyCols funcs =
  TyAgg (mkTyInfoMap allTypes) fieldMap scalars ordByCtx
  where
    ordByCtx = fromMaybe Map.empty ordByCtxM

    funcInpArgTys = bool [] (map TIInpObj funcArgInpObjs) $ isJust selFldsM

    allTypes = queryTypes <> aggQueryTypes <> funcInpArgTys <> computedFieldFuncArgsInps

    queryTypes = map TIObj selectObjects <>
      catMaybes
      [ TIInpObj <$> boolExpInpObjM
      , TIInpObj <$> ordByInpObjM
      , TIEnum <$> selColInpTyM
      ]
    aggQueryTypes = map TIObj aggObjs <> map TIInpObj aggOrdByInps

    fieldMap = Map.unions $ catMaybes [boolExpInpObjFldsM, selObjFldsM]
    scalars = selByPkScalarSet <> funcArgScalarSet <> computedFieldFuncArgScalars

    selFldsM = snd <$> selPermM
    selColNamesM = (map pgiName . getPGColumnFields) <$> selFldsM
    selColInpTyM = mkSelColumnTy tn <$> selColNamesM
    -- boolexp input type
    boolExpInpObjM = (mkBoolExpInp tn) <$> selFldsM
    -- funcargs input type
    funcArgInpObjs = flip mapMaybe funcs $ \func ->
      mkFuncArgsInp (fiName func) (getInputArgs func)
    -- funcArgCtx = Map.unions funcArgCtxs
    funcArgScalarSet = funcs ^.. folded.to getInputArgs.folded.to (_qptName.faType)

    -- helper
    mkFldMap ty = Map.fromList . concatMap (mkFld ty)
    mkFld ty = \case
      SFPGColumn ci -> [((ty, pgiName ci), RFPGColumn ci)]
      SFRelationship (RelationshipFieldInfo relInfo allowAgg cols permFilter permLimit maybePkCols _) ->
        let relationshipName = riName relInfo
            relFld = ( (ty, mkRelName relationshipName)
                     , RFRelationship $ RelationshipField relInfo RFKSimple cols permFilter permLimit
                     )
            aggRelFld = ( (ty, mkAggRelName relationshipName)
                        , RFRelationship $ RelationshipField relInfo RFKAggregate cols permFilter permLimit
                        )
            maybeConnFld = maybePkCols <&> \pkCols ->
                           ( (ty, mkConnectionRelName relationshipName)
                           , RFRelationship $ RelationshipField relInfo
                             (RFKConnection pkCols) cols permFilter permLimit
                           )
        in case riType relInfo of
          ObjRel -> [relFld]
          ArrRel -> bool [relFld] ([relFld, aggRelFld] <> maybe [] pure maybeConnFld) allowAgg
      SFComputedField cf -> pure
        ( (ty, mkComputedFieldName $ _cfName cf)
        , RFComputedField cf
        )

    -- the fields used in bool exp
    boolExpInpObjFldsM = mkFldMap (mkBoolExpTy tn) <$> selFldsM

    -- table obj
    selectObjects = case selPermM of
      Just (_, selFlds) ->
        [ mkTableObj tn True descM selFlds
        , mkTableEdgeObj tn
        , mkTableConnectionObj tn
        ]
      Nothing -> []

    -- aggregate objs and order by inputs
    (aggObjs, aggOrdByInps) = case selPermM of
      Just (True, selFlds) ->
        let cols = getPGColumnFields selFlds
            numCols = onlyNumCols cols
            compCols = onlyComparableCols cols
            objs = [ mkTableAggObj tn
                   , mkTableAggregateFieldsObj tn (numCols, numAggregateOps) (compCols, compAggregateOps)
                   ] <> mkColAggregateFieldsObjs selFlds
            ordByInps = mkTabAggOrdByInpObj tn (numCols, numAggregateOps) (compCols, compAggregateOps)
                        : mkTabAggregateOpOrdByInpObjs tn (numCols, numAggregateOps) (compCols, compAggregateOps)
        in (objs, ordByInps)
      _ -> ([], [])

    getNumericCols = onlyNumCols . getPGColumnFields
    getComparableCols = onlyComparableCols . getPGColumnFields
    onlyFloat = const $ mkScalarTy PGFloat

    mkTypeMaker "sum" = mkColumnType
    mkTypeMaker _     = onlyFloat

    mkColAggregateFieldsObjs flds =
      let numCols = getNumericCols flds
          compCols = getComparableCols flds
          mkNumObjFld n = mkTableColAggregateFieldsObj tn n (mkTypeMaker n) numCols
          mkCompObjFld n = mkTableColAggregateFieldsObj tn n mkColumnType compCols
          numFldsObjs = bool (map mkNumObjFld numAggregateOps) [] $ null numCols
          compFldsObjs = bool (map mkCompObjFld compAggregateOps) [] $ null compCols
      in numFldsObjs <> compFldsObjs
    -- the fields used in table object
    selObjFldsM = mkFldMap (mkTableTy tn) <$> selFldsM
    -- the scalar set for table_by_pk arguments
    selByPkScalarSet = pkeyCols ^.. folded.to _pkColumns.folded.to pgiType._PGColumnScalar

    ordByInpCtxM = mkOrdByInpObj tn <$> selFldsM
    (ordByInpObjM, ordByCtxM) = case ordByInpCtxM of
      Just (a, b) -> (Just a, Just b)
      Nothing     -> (Nothing, Nothing)

    -- computed fields' function args input objects and scalar types
    mkComputedFieldRequiredTypes computedFieldInfo =
      let ComputedFieldFunction qf inputArgs _ _ = _cfFunction computedFieldInfo
          scalarArgs = map (_qptName . faType) $ toList inputArgs
      in (, scalarArgs) <$> mkFuncArgsInp qf inputArgs

    computedFieldReqTypes = catMaybes $
      maybe [] (map mkComputedFieldRequiredTypes . getComputedFields) selFldsM

    computedFieldFuncArgsInps = map (TIInpObj . fst) computedFieldReqTypes
    computedFieldFuncArgScalars = Set.fromList $ concatMap snd computedFieldReqTypes

getRelayRootFldsRole'
  :: QualifiedTable
  -> Maybe (PrimaryKey PGColumnInfo)
  -> FieldInfoMap FieldInfo
  -> [FunctionInfo]
  -> Maybe (AnnBoolExpPartialSQL, Maybe Int, [T.Text], Bool) -- select filter
  -> RootFields
getRelayRootFldsRole' tn primaryKey fields funcs selM =
  RootFields
    { _rootQueryFields = makeFieldMap $
        funcConnectionQueries
        <> catMaybes
          [ getSelConnectionDet <$> selM <*> maybePrimaryKeyColumns
          ]
    , _rootMutationFields = mempty
    }
  where
    maybePrimaryKeyColumns = fmap _pkColumns primaryKey
    makeFieldMap = mapFromL (_fiName . snd)
    colGNameMap = mkPGColGNameMap $ getCols fields

    funcConnectionQueries = fromMaybe [] $ getFuncQueryConnectionFlds
                            <$> selM <*> maybePrimaryKeyColumns

    getSelConnectionDet (selFltr, pLimit, hdrs, _) primaryKeyColumns =
      selFldHelper (QCSelectConnection primaryKeyColumns)
      (mkSelFldConnection Nothing) selFltr pLimit hdrs

    selFldHelper f g pFltr pLimit hdrs =
      ( f $ SelOpCtx tn hdrs colGNameMap pFltr pLimit
      , g tn
      )

    getFuncQueryConnectionFlds (selFltr, pLimit, hdrs, _) primaryKeyColumns =
      funcFldHelper (QCFuncConnection primaryKeyColumns) mkFuncQueryConnectionFld selFltr pLimit hdrs

    funcFldHelper f g pFltr pLimit hdrs =
      flip map funcs $ \fi ->
      ( f $ FuncQOpCtx (fiName fi) (mkFuncArgItemSeq fi) hdrs colGNameMap pFltr pLimit
      , g fi $ fiDescription fi
      )
