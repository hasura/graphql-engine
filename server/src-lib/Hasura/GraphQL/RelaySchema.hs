module Hasura.GraphQL.RelaySchema where

import           Control.Lens.Extended          hiding (op)

import qualified Data.HashMap.Strict            as Map
import qualified Data.HashSet                   as Set
import qualified Data.Text                      as T
import qualified Language.GraphQL.Draft.Syntax  as G

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils            (duplicates)
import           Hasura.Session
import           Hasura.SQL.Types

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Builder
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Function
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Schema.Select

mkNodeInterface :: [QualifiedTable] -> IFaceTyInfo
mkNodeInterface relayTableNames =
  let description = G.Description "An object with globally unique ID"
  in mkIFaceTyInfo (Just description) nodeType (mapFromL _fiName [idField]) $
     Set.fromList $ map mkTableTy relayTableNames
  where
    idField =
        let description = G.Description "A globally unique identifier"
        in mkHsraObjFldInfo (Just description) "id" mempty nodeIdType

mkRelayGCtxMap
  :: forall m. (MonadError QErr m)
  => TableCache -> FunctionCache -> m GCtxMap
mkRelayGCtxMap tableCache functionCache = do
  typesMapL <- mapM (mkRelayGCtxMapTable tableCache functionCache) relayTables
  typesMap <- combineTypes typesMapL
  let gCtxMap  = flip Map.map typesMap $
                 \(ty, flds, insCtx) -> mkGCtx ty flds insCtx
  pure $ Map.map (flip RoleContext Nothing) gCtxMap
  where
    relayTables =
      filter (tableFltr . _tiCoreInfo) $ Map.elems tableCache

    tableFltr ti =
      not (isSystemDefined $ _tciSystemDefined ti)
      && isValidObjectName (_tciName ti)
      && isJust (_tciPrimaryKey ti)

    combineTypes
      :: [Map.HashMap RoleName (TyAgg, RootFields, InsCtxMap)]
      -> m (Map.HashMap RoleName (TyAgg, RootFields, InsCtxMap))
    combineTypes maps = do
      let listMap = foldr (Map.unionWith (++) . Map.map pure) mempty maps
      flip Map.traverseWithKey listMap $ \roleName typeList -> do
        let relayTableNames = map (_tciName . _tiCoreInfo) relayTables
            tyAgg = addTypeInfoToTyAgg
                    (TIIFace $ mkNodeInterface relayTableNames) $
                    mconcat $ map (^. _1) typeList
            insCtx = mconcat $ map (^. _3) typeList
        rootFields <- combineRootFields roleName $ map (^. _2) typeList
        pure (tyAgg, rootFields, insCtx)

    combineRootFields :: RoleName -> [RootFields] -> m RootFields
    combineRootFields roleName rootFields = do
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

      pure $ mconcat $ mkNodeQueryRootFields roleName relayTables : rootFields

mkRelayGCtxMapTable
  :: (MonadError QErr m)
  => TableCache
  -> FunctionCache
  -> TableInfo
  -> m (Map.HashMap RoleName (TyAgg, RootFields, InsCtxMap))
mkRelayGCtxMapTable tableCache funcCache tabInfo = do
  m <- flip Map.traverseWithKey rolePerms $
       mkRelayGCtxRole tableCache tn descM fields primaryKey validConstraints  tabFuncs viewInfo customConfig
  adminSelFlds <- mkAdminSelFlds fields tableCache
  adminInsCtx <- mkAdminInsCtx tableCache fields
  let adminCtx = mkRelayTyAggRole tn descM (Just (cols, icRelations adminInsCtx))
                 (Just (True, adminSelFlds)) (Just cols) (Just ())
                 primaryKey validConstraints viewInfo tabFuncs
      adminInsCtxMap = Map.singleton tn adminInsCtx
  return $ Map.insert adminRoleName (adminCtx, adminRootFlds, adminInsCtxMap) m
  where
    TableInfo coreInfo rolePerms _ = tabInfo
    TableCoreInfo tn descM _ fields primaryKey _ _ viewInfo _ customConfig = coreInfo
    validConstraints = mkValidConstraints $ map _cName (tciUniqueOrPrimaryKeyConstraints coreInfo)
    tabFuncs = filter (isValidObjectName . fiName) $
               getFuncsOfTable tn funcCache
    cols = getValidCols fields
    adminRootFlds =
      let insertPermDetails = Just ([], True)
          selectPermDetails = Just (noFilter, Nothing, [], True)
          updatePermDetails = Just (getValidCols fields, mempty, noFilter, Nothing, [])
          deletePermDetails = Just (noFilter, [])

          queryFields = getRelayQueryRootFieldsRole tn primaryKey fields tabFuncs
                        selectPermDetails
          mutationFields = getMutationRootFieldsRole tn primaryKey
                           validConstraints fields insertPermDetails
                           selectPermDetails updatePermDetails
                           deletePermDetails viewInfo customConfig
      in RootFields queryFields mutationFields

mkRelayGCtxRole
  :: (MonadError QErr m)
  => TableCache
  -> QualifiedTable
  -> Maybe PGDescription
  -> FieldInfoMap FieldInfo
  -> Maybe (PrimaryKey PGColumnInfo)
  -> [ConstraintName]
  -> [FunctionInfo]
  -> Maybe ViewInfo
  -> TableConfig
  -> RoleName
  -> RolePermInfo
  -> m (TyAgg, RootFields, InsCtxMap)
mkRelayGCtxRole tableCache tn descM fields primaryKey constraints funcs viM tabConfigM role permInfo = do
  selPermM <- mapM (getSelPerm tableCache fields role) selM
  tabInsInfoM <- forM (_permIns permInfo) $ \ipi -> do
    ctx <- mkInsCtx role tableCache fields ipi $ _permUpd permInfo
    let permCols = flip getColInfos allCols $ Set.toList $ ipiCols ipi
    return (ctx, (permCols, icRelations ctx))
  let insPermM = snd <$> tabInsInfoM
      insCtxM = fst <$> tabInsInfoM
      updColsM = filterColumnFields . upiCols <$> _permUpd permInfo
      tyAgg = mkRelayTyAggRole tn descM insPermM selPermM updColsM
              (void $ _permDel permInfo) primaryKey constraints viM funcs
      queryRootFlds = getRelayQueryRootFieldsRole tn primaryKey fields funcs
                 (mkSel <$> _permSel permInfo)
      mutationRootFlds = getMutationRootFieldsRole tn primaryKey constraints fields
                       (mkIns <$> insM) (mkSel <$> selM)
                       (mkUpd <$> updM) (mkDel <$> delM) viM tabConfigM
      insCtxMap = maybe Map.empty (Map.singleton tn) insCtxM
  return (tyAgg, RootFields queryRootFlds mutationRootFlds, insCtxMap)
  where
    RolePermInfo insM selM updM delM = permInfo
    allCols = getCols fields
    filterColumnFields allowedSet =
      filter ((`Set.member` allowedSet) . pgiColumn) $ getValidCols fields
    mkIns i = (ipiRequiredHeaders i, isJust updM)
    mkSel s = ( spiFilter s, spiLimit s
              , spiRequiredHeaders s, spiAllowAgg s
              )
    mkUpd u = ( flip getColInfos allCols $ Set.toList $ upiCols u
              , upiSet u
              , upiFilter u
              , upiCheck u
              , upiRequiredHeaders u
              )
    mkDel d = (dpiFilter d, dpiRequiredHeaders d)

mkRelayTyAggRole
  :: QualifiedTable
  -> Maybe PGDescription
  -- ^ Postgres description
  -> Maybe ([PGColumnInfo], RelationInfoMap)
  -- ^ insert permission
  -> Maybe (Bool, [SelField])
  -- ^ select permission
  -> Maybe [PGColumnInfo]
  -- ^ update cols
  -> Maybe ()
  -- ^ delete cols
  -> Maybe (PrimaryKey PGColumnInfo)
  -> [ConstraintName]
  -- ^ constraints
  -> Maybe ViewInfo
  -> [FunctionInfo]
  -- ^ all functions
  -> TyAgg
mkRelayTyAggRole tn descM insPermM selPermM updColsM delPermM pkeyCols constraints viM funcs =
  let (mutationTypes, mutationFields) =
        mkMutationTypesAndFieldsRole tn insPermM selFldsM updColsM delPermM pkeyCols constraints viM
  in TyAgg (mkTyInfoMap allTypes <> mutationTypes)
           (fieldMap <> mutationFields)
            scalars ordByCtx
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
    selColNamesM = map pgiName . getPGColumnFields <$> selFldsM
    selColInpTyM = mkSelColumnTy tn <$> selColNamesM
    -- boolexp input type
    boolExpInpObjM = case selFldsM of
      Just selFlds  -> Just $ mkBoolExpInp tn selFlds
      -- no select permission
      Nothing ->
        -- but update/delete is defined
        if isJust updColsM || isJust delPermM
        then Just $ mkBoolExpInp tn []
        else Nothing

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
      SFRemoteRelationship remoteField -> pure
        ( (ty, G.Name (remoteRelationshipNameToText (_rfiName remoteField)))
        , RFRemoteRelationship remoteField
        )

    -- the fields used in bool exp
    boolExpInpObjFldsM = mkFldMap (mkBoolExpTy tn) <$> selFldsM

    -- table obj
    selectObjects = case selPermM of
      Just (_, selFlds) ->
        [ (mkRelayTableObj tn descM selFlds)
          {_otiImplIFaces = Set.singleton nodeType}
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
    nodeFieldM =  RFNodeId tn . _pkColumns <$> pkeyCols
    selObjFldsM = mkFldMap (mkTableTy tn) <$> selFldsM >>=
                  \fm -> nodeFieldM <&> \nodeField ->
                    Map.insert (mkTableTy tn, "id") nodeField fm
    -- the scalar set for table_by_pk arguments
    selByPkScalarSet = pkeyCols ^.. folded.to _pkColumns.folded.to pgiType._PGColumnScalar

    ordByInpCtxM = mkOrdByInpObj tn <$> selFldsM
    (ordByInpObjM, ordByCtxM) = case ordByInpCtxM of
      Just (a, b) -> (Just a, Just b)
      Nothing     -> (Nothing, Nothing)

    -- computed fields' function args input objects and scalar types
    mkComputedFieldRequiredTypes computedFieldInfo =
      let ComputedFieldFunction qf inputArgs _ _ _ = _cfFunction computedFieldInfo
          scalarArgs = map (_qptName . faType) $ toList inputArgs
      in (, scalarArgs) <$> mkFuncArgsInp qf inputArgs

    computedFieldReqTypes = catMaybes $
      maybe [] (map mkComputedFieldRequiredTypes . getComputedFields) selFldsM

    computedFieldFuncArgsInps = map (TIInpObj . fst) computedFieldReqTypes
    computedFieldFuncArgScalars = Set.fromList $ concatMap snd computedFieldReqTypes

mkSelectOpCtx
  :: QualifiedTable
  -> [PGColumnInfo]
  -> (AnnBoolExpPartialSQL, Maybe Int, [T.Text]) -- select filter
  -> SelOpCtx
mkSelectOpCtx tn allCols (fltr, pLimit, hdrs) =
  SelOpCtx tn hdrs colGNameMap fltr pLimit
  where
    colGNameMap = mkPGColGNameMap allCols

getRelayQueryRootFieldsRole
  :: QualifiedTable
  -> Maybe (PrimaryKey PGColumnInfo)
  -> FieldInfoMap FieldInfo
  -> [FunctionInfo]
  -> Maybe (AnnBoolExpPartialSQL, Maybe Int, [T.Text], Bool) -- select filter
  -> QueryRootFieldMap
getRelayQueryRootFieldsRole tn primaryKey fields funcs selM =
    makeFieldMap $
        funcConnectionQueries
        <> catMaybes
          [ getSelConnectionDet <$> selM <*> maybePrimaryKeyColumns
          ]
  where
    maybePrimaryKeyColumns = fmap _pkColumns primaryKey
    colGNameMap = mkPGColGNameMap $ getCols fields

    funcConnectionQueries = fromMaybe [] $ getFuncQueryConnectionFlds
                            <$> selM <*> maybePrimaryKeyColumns

    getSelConnectionDet (selFltr, pLimit, hdrs, _) primaryKeyColumns =
      selFldHelper (QCSelectConnection primaryKeyColumns)
      (mkSelFldConnection Nothing) selFltr pLimit hdrs

    selFldHelper f g pFltr pLimit hdrs =
      ( f $ mkSelectOpCtx tn (getCols fields) (pFltr, pLimit, hdrs)
      , g tn
      )

    getFuncQueryConnectionFlds (selFltr, pLimit, hdrs, _) primaryKeyColumns =
      funcFldHelper (QCFuncConnection primaryKeyColumns) mkFuncQueryConnectionFld selFltr pLimit hdrs

    funcFldHelper f g pFltr pLimit hdrs =
      flip map funcs $ \fi ->
      ( f $ FuncQOpCtx (fiName fi) (mkFuncArgItemSeq fi) hdrs colGNameMap pFltr pLimit
      , g fi $ fiDescription fi
      )

mkNodeQueryRootFields :: RoleName -> [TableInfo] -> RootFields
mkNodeQueryRootFields roleName relayTables =
  RootFields (mapFromL (_fiName . snd) [nodeQueryDet]) mempty
  where
    nodeQueryDet =
      ( QCNodeSelect nodeSelMap
      , nodeQueryField
      )

    nodeQueryField =
      let nodeParams = fromInpValL $ pure $
                       InpValInfo (Just $ G.Description "A globally unique id")
                       "id" Nothing nodeIdType
      in mkHsraObjFldInfo Nothing "node" nodeParams $ G.toGT nodeType

    nodeSelMap =
      Map.fromList $ flip mapMaybe relayTables $ \table ->
      let tableName = _tciName $ _tiCoreInfo table
          allColumns = getCols $ _tciFieldInfoMap $ _tiCoreInfo table
          selectPermM = _permSel <$> Map.lookup roleName
                       (_tiRolePermInfoMap table)
          permDetailsM = join selectPermM <&> \perm ->
            ( spiFilter perm
            , spiLimit perm
            , spiRequiredHeaders perm
            )
          adminPermDetails = (noFilter, Nothing, [])
      in (mkTableTy tableName,) . mkSelectOpCtx tableName allColumns
         <$> bool permDetailsM (Just adminPermDetails) (isAdmin roleName)
