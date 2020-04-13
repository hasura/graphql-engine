module Hasura.GraphQL.Schema
  ( mkGCtxMap
  , GCtxMap
  , getGCtx
  , GCtx(..)
  , QueryCtx(..)
  , MutationCtx(..)
  , InsCtx(..)
  , InsCtxMap
  , RelationInfoMap
  , isAggFld
  , qualObjectToName
  , ppGCtx

  , checkConflictingNode
  , checkSchemaConflicts
  ) where

import           Control.Lens.Extended                 hiding (op)

import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashSet                          as Set
import qualified Data.Sequence                         as Seq

import qualified Data.Text                             as T
import qualified Language.GraphQL.Draft.Syntax         as G

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal               (mkAdminRolePermInfo)
import           Hasura.RQL.Types
import           Hasura.Server.Utils                   (duplicates)
import           Hasura.Session
import           Hasura.SQL.Types

import           Hasura.GraphQL.Schema.Action
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Builder
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Function
import           Hasura.GraphQL.Schema.Merge
import           Hasura.GraphQL.Schema.Mutation.Common
import           Hasura.GraphQL.Schema.Mutation.Delete
import           Hasura.GraphQL.Schema.Mutation.Insert
import           Hasura.GraphQL.Schema.Mutation.Update
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Schema.Select

type TableSchemaCtx = RoleContext (TyAgg, RootFields, InsCtxMap)

getInsPerm :: TableInfo -> RoleName -> Maybe InsPermInfo
getInsPerm tabInfo roleName
  | roleName == adminRoleName = _permIns $ mkAdminRolePermInfo (_tiCoreInfo tabInfo)
  | otherwise = Map.lookup roleName rolePermInfoMap >>= _permIns
  where
    rolePermInfoMap = _tiRolePermInfoMap tabInfo

getTabInfo
  :: MonadError QErr m
  => TableCache -> QualifiedTable -> m TableInfo
getTabInfo tc t =
  onNothing (Map.lookup t tc) $
     throw500 $ "table not found: " <>> t

isValidObjectName :: (ToTxt a) => QualifiedObject a -> Bool
isValidObjectName = G.isValidName . qualObjectToName

isValidCol :: PGColumnInfo -> Bool
isValidCol = G.isValidName . pgiName

isValidRel :: ToTxt a => RelName -> QualifiedObject a -> Bool
isValidRel rn rt = G.isValidName (mkRelName rn) && isValidObjectName rt

upsertable :: [ConstraintName] -> Bool -> Bool -> Bool
upsertable uniqueOrPrimaryCons isUpsertAllowed isAView =
  not (null uniqueOrPrimaryCons) && isUpsertAllowed && not isAView

getValidCols
  :: FieldInfoMap FieldInfo -> [PGColumnInfo]
getValidCols = filter isValidCol . getCols

getValidRels :: FieldInfoMap FieldInfo -> [RelInfo]
getValidRels = filter isValidRel' . getRels
  where
    isValidRel' (RelInfo rn _ _ remTab _) = isValidRel rn remTab

mkValidConstraints :: [ConstraintName] -> [ConstraintName]
mkValidConstraints =
  filter (G.isValidName . G.Name . getConstraintTxt)

isRelNullable
  :: FieldInfoMap FieldInfo -> RelInfo -> Bool
isRelNullable fim ri = isNullable
  where
    lCols = Map.keys $ riMapping ri
    allCols = getValidCols fim
    lColInfos = getColInfos lCols allCols
    isNullable = any pgiIsNullable lColInfos

mkPGColGNameMap :: [PGColumnInfo] -> PGColGNameMap
mkPGColGNameMap cols = Map.fromList $
  flip map cols $ \ci -> (pgiName ci, ci)

numAggOps :: [G.Name]
numAggOps = [ "sum", "avg", "stddev", "stddev_samp", "stddev_pop"
            , "variance", "var_samp", "var_pop"
            ]

compAggOps :: [G.Name]
compAggOps = ["max", "min"]

isAggFld :: G.Name -> Bool
isAggFld = flip elem (numAggOps <> compAggOps)

mkComputedFieldFunctionArgSeq :: Seq.Seq FunctionArg -> ComputedFieldFunctionArgSeq
mkComputedFieldFunctionArgSeq inputArgs =
    Seq.fromList $ procFuncArgs inputArgs faName $
    \fa t -> FunctionArgItem (G.Name t) (faName fa) (faHasDefault fa)

-- see Note [Split schema generation (TODO)]
mkGCtxRole'
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
mkGCtxRole' tn descM insPermM selPermM updColsM delPermM pkeyCols constraints viM funcs =
  TyAgg (mkTyInfoMap allTypes) fieldMap scalars ordByCtx
  where

    ordByCtx = fromMaybe Map.empty ordByCtxM
    upsertPerm = isJust updColsM
    isUpsertable = upsertable constraints upsertPerm $ isJust viM
    updatableCols = maybe [] (map pgiName) updColsM
    onConflictTypes = mkOnConflictTypes tn constraints updatableCols isUpsertable
    jsonOpTys = fromMaybe [] updJSONOpInpObjTysM
    relInsInpObjTys = maybe [] (map TIInpObj) $
                      mutHelper viIsInsertable relInsInpObjsM

    funcInpArgTys = bool [] (map TIInpObj funcArgInpObjs) $ isJust selFldsM

    allTypes = relInsInpObjTys <> onConflictTypes <> jsonOpTys
               <> queryTypes <> aggQueryTypes <> mutationTypes
               <> funcInpArgTys <> referencedEnumTypes <> computedFieldFuncArgsInps

    queryTypes = catMaybes
      [ TIInpObj <$> boolExpInpObjM
      , TIInpObj <$> ordByInpObjM
      , TIObj <$> selObjM
      ]
    aggQueryTypes = map TIObj aggObjs <> map TIInpObj aggOrdByInps

    mutationTypes = catMaybes
      [ TIInpObj <$> mutHelper viIsInsertable insInpObjM
      , TIInpObj <$> mutHelper viIsUpdatable updSetInpObjM
      , TIInpObj <$> mutHelper viIsUpdatable updIncInpObjM
      , TIInpObj <$> mutHelper viIsUpdatable primaryKeysInpObjM
      , TIObj <$> mutRespObjM
      , TIEnum <$> selColInpTyM
      ]

    mutHelper :: (ViewInfo -> Bool) -> Maybe a -> Maybe a
    mutHelper f objM = bool Nothing objM $ isMutable f viM

    fieldMap = Map.unions $ catMaybes
               [ insInpObjFldsM, updSetInpObjFldsM
               , boolExpInpObjFldsM , selObjFldsM
               ]
    scalars = selByPkScalarSet <> funcArgScalarSet <> computedFieldFuncArgScalars

    -- helper
    mkColFldMap ty cols = Map.fromList $ flip map cols $
      \ci -> ((ty, pgiName ci), RFPGColumn ci)

    -- insert input type
    insInpObjM = uncurry (mkInsInp tn) <$> insPermM
    -- column fields used in insert input object
    insInpObjFldsM = (mkColFldMap (mkInsInpTy tn) . fst) <$> insPermM
    -- relationship input objects
    relInsInpObjsM = const (mkRelInsInps tn isUpsertable) <$> insPermM
    -- update set input type
    updSetInpObjM = mkUpdSetInp tn <$> updColsM
    -- update increment input type
    updIncInpObjM = mkUpdIncInp tn updColsM
    -- update json operator input type
    updJSONOpInpObjsM = mkUpdJSONOpInp tn <$> updColsM
    updJSONOpInpObjTysM = map TIInpObj <$> updJSONOpInpObjsM
    -- fields used in set input object
    updSetInpObjFldsM = mkColFldMap (mkUpdSetTy tn) <$> updColsM

    -- primary key columns input object for update_by_pk
    primaryKeysInpObjM = guard (isJust selPermM) *> (mkPKeyColumnsInpObj tn <$> pkeyCols)

    selFldsM = snd <$> selPermM
    selColNamesM = (map pgiName . getPGColumnFields) <$> selFldsM
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
      SFRelationship (RelationshipFieldInfo relInfo allowAgg cols permFilter permLimit _) ->
        let relationshipName = riName relInfo
            relFld = ( (ty, mkRelName relationshipName)
                     , RFRelationship $ RelationshipField relInfo False cols permFilter permLimit
                     )
            aggRelFld = ( (ty, mkAggRelName relationshipName)
                        , RFRelationship $ RelationshipField relInfo True cols permFilter permLimit
                        )
        in case riType relInfo of
          ObjRel -> [relFld]
          ArrRel -> bool [relFld] [relFld, aggRelFld] allowAgg
      SFComputedField cf -> pure
        ( (ty, mkComputedFieldName $ _cfName cf)
        , RFComputedField cf
        )

    -- the fields used in bool exp
    boolExpInpObjFldsM = mkFldMap (mkBoolExpTy tn) <$> selFldsM

    -- mut resp obj
    mutRespObjM =
      if isMut
      then Just $ mkMutRespObj tn $ isJust selFldsM
      else Nothing

    isMut = (isJust insPermM || isJust updColsM || isJust delPermM)
            && any (`isMutable` viM) [viIsInsertable, viIsUpdatable, viIsDeletable]

    -- table obj
    selObjM = mkTableObj tn descM <$> selFldsM

    -- aggregate objs and order by inputs
    (aggObjs, aggOrdByInps) = case selPermM of
      Just (True, selFlds) ->
        let cols = getPGColumnFields selFlds
            numCols = onlyNumCols cols
            compCols = onlyComparableCols cols
            objs = [ mkTableAggObj tn
                   , mkTableAggFldsObj tn (numCols, numAggOps) (compCols, compAggOps)
                   ] <> mkColAggFldsObjs selFlds
            ordByInps = mkTabAggOrdByInpObj tn (numCols, numAggOps) (compCols, compAggOps)
                        : mkTabAggOpOrdByInpObjs tn (numCols, numAggOps) (compCols, compAggOps)
        in (objs, ordByInps)
      _ -> ([], [])

    getNumericCols = onlyNumCols . getPGColumnFields
    getComparableCols = onlyComparableCols . getPGColumnFields
    onlyFloat = const $ mkScalarTy PGFloat

    mkTypeMaker "sum" = mkColumnType
    mkTypeMaker _     = onlyFloat

    mkColAggFldsObjs flds =
      let numCols = getNumericCols flds
          compCols = getComparableCols flds
          mkNumObjFld n = mkTableColAggFldsObj tn n (mkTypeMaker n) numCols
          mkCompObjFld n = mkTableColAggFldsObj tn n mkColumnType compCols
          numFldsObjs = bool (map mkNumObjFld numAggOps) [] $ null numCols
          compFldsObjs = bool (map mkCompObjFld compAggOps) [] $ null compCols
      in numFldsObjs <> compFldsObjs
    -- the fields used in table object
    selObjFldsM = mkFldMap (mkTableTy tn) <$> selFldsM
    -- the scalar set for table_by_pk arguments
    selByPkScalarSet = pkeyCols ^.. folded.to _pkColumns.folded.to pgiType._PGColumnScalar

    ordByInpCtxM = mkOrdByInpObj tn <$> selFldsM
    (ordByInpObjM, ordByCtxM) = case ordByInpCtxM of
      Just (a, b) -> (Just a, Just b)
      Nothing     -> (Nothing, Nothing)

    -- the types for all enums that are /referenced/ by this table (not /defined/ by this table;
    -- there isn’t actually any need to generate a GraphQL enum type for an enum table if it’s
    -- never referenced anywhere else)
    referencedEnumTypes =
      let allColumnInfos =
               (selPermM ^.. _Just._2.traverse._SFPGColumn)
            <> (insPermM ^. _Just._1)
            <> (updColsM ^. _Just)
          allEnumReferences = allColumnInfos ^.. traverse.to pgiType._PGColumnEnumReference
      in flip map allEnumReferences $ \enumReference@(EnumReference referencedTableName _) ->
           let typeName = mkTableEnumType referencedTableName
           in TIEnum $ mkHsraEnumTyInfo Nothing typeName (EnumValuesReference enumReference)


    -- computed fields' function args input objects and scalar types
    mkComputedFieldRequiredTypes computedFieldInfo =
      let ComputedFieldFunction qf inputArgs _ _ = _cfFunction computedFieldInfo
          scalarArgs = map (_qptName . faType) $ toList inputArgs
      in (, scalarArgs) <$> mkFuncArgsInp qf inputArgs

    computedFieldReqTypes = catMaybes $
      maybe [] (map mkComputedFieldRequiredTypes . getComputedFields) selFldsM

    computedFieldFuncArgsInps = map (TIInpObj . fst) computedFieldReqTypes
    computedFieldFuncArgScalars = Set.fromList $ concatMap snd computedFieldReqTypes

-- see Note [Split schema generation (TODO)]
getRootFldsRole'
  :: QualifiedTable
  -> Maybe (PrimaryKey PGColumnInfo)
  -> [ConstraintName]
  -> FieldInfoMap FieldInfo
  -> [FunctionInfo]
  -> Maybe ([T.Text], Bool) -- insert perm
  -> Maybe (AnnBoolExpPartialSQL, Maybe Int, [T.Text], Bool) -- select filter
  -> Maybe ([PGColumnInfo], PreSetColsPartial, AnnBoolExpPartialSQL, Maybe AnnBoolExpPartialSQL, [T.Text]) -- update filter
  -> Maybe (AnnBoolExpPartialSQL, [T.Text]) -- delete filter
  -> Maybe ViewInfo
  -> TableConfig -- custom config
  -> RootFields
getRootFldsRole' tn primaryKey constraints fields funcs insM
                 selM updM delM viM tableConfig =
  RootFields
    { _rootQueryFields = makeFieldMap $
        funcQueries
        <> funcAggQueries
        <> catMaybes
          [ getSelDet <$> selM
          , getSelAggDet selM
          , getPKeySelDet <$> selM <*> primaryKey
          ]
    , _rootMutationFields = makeFieldMap $ catMaybes
        [ mutHelper viIsInsertable getInsDet insM
        , onlyIfSelectPermExist $ mutHelper viIsInsertable getInsOneDet insM
        , mutHelper viIsUpdatable getUpdDet updM
        , onlyIfSelectPermExist $ mutHelper viIsUpdatable getUpdByPkDet $ (,) <$> updM <*> primaryKey
        , mutHelper viIsDeletable getDelDet delM
        , onlyIfSelectPermExist $ mutHelper viIsDeletable getDelByPkDet $ (,) <$> delM <*> primaryKey
        ]
    }
  where
    makeFieldMap = mapFromL (_fiName . snd)
    customRootFields = _tcCustomRootFields tableConfig
    colGNameMap = mkPGColGNameMap $ getCols fields

    funcQueries = maybe [] getFuncQueryFlds selM
    funcAggQueries = maybe [] getFuncAggQueryFlds selM

    mutHelper :: (ViewInfo -> Bool) -> (a -> b) -> Maybe a -> Maybe b
    mutHelper f getDet mutM =
      bool Nothing (getDet <$> mutM) $ isMutable f viM

    onlyIfSelectPermExist v = guard (isJust selM) *> v

    getCustomNameWith f = f customRootFields

    insCustName = getCustomNameWith _tcrfInsert
    getInsDet (hdrs, upsertPerm) =
      let isUpsertable = upsertable constraints upsertPerm $ isJust viM
      in ( MCInsert $ InsOpCtx tn $ hdrs `union` maybe [] (^. _5) updM
         , mkInsMutFld insCustName tn isUpsertable
         )

    insOneCustName = getCustomNameWith _tcrfInsertOne
    getInsOneDet (hdrs, upsertPerm) =
      let isUpsertable = upsertable constraints upsertPerm $ isJust viM
      in ( MCInsertOne $ InsOpCtx tn $ hdrs `union` maybe [] (^. _5) updM
         , mkInsertOneMutationField insOneCustName tn isUpsertable
         )

    updCustName = getCustomNameWith _tcrfUpdate
    getUpdDet (updCols, preSetCols, updFltr, updCheck, hdrs) =
      ( MCUpdate $ UpdOpCtx tn hdrs colGNameMap updFltr updCheck preSetCols
      , mkUpdMutFld updCustName tn updCols
      )

    updByPkCustName = getCustomNameWith _tcrfUpdateByPk
    getUpdByPkDet ((updCols, preSetCols, updFltr, updCheck, hdrs), pKey) =
      ( MCUpdateByPk $ UpdOpCtx tn hdrs colGNameMap updFltr updCheck preSetCols
      , mkUpdateByPkMutationField updByPkCustName tn updCols pKey
      )

    delCustName = getCustomNameWith _tcrfDelete
    getDelDet (delFltr, hdrs) =
      ( MCDelete $ DelOpCtx tn hdrs colGNameMap delFltr
      , mkDelMutFld delCustName tn
      )
    delByPkCustName = getCustomNameWith _tcrfDeleteByPk
    getDelByPkDet ((delFltr, hdrs), pKey) =
      ( MCDeleteByPk $ DelOpCtx tn hdrs colGNameMap delFltr
      , mkDeleteByPkMutationField delByPkCustName tn pKey
      )


    selCustName = getCustomNameWith _tcrfSelect
    getSelDet (selFltr, pLimit, hdrs, _) =
      selFldHelper QCSelect (mkSelFld selCustName) selFltr pLimit hdrs

    selAggCustName = getCustomNameWith _tcrfSelectAggregate
    getSelAggDet (Just (selFltr, pLimit, hdrs, True)) =
      Just $ selFldHelper QCSelectAgg (mkAggSelFld selAggCustName)
               selFltr pLimit hdrs
    getSelAggDet _                                    = Nothing

    selFldHelper f g pFltr pLimit hdrs =
      ( f $ SelOpCtx tn hdrs colGNameMap pFltr pLimit
      , g tn
      )

    selByPkCustName = getCustomNameWith _tcrfSelectByPk
    getPKeySelDet (selFltr, _, hdrs, _) key =
      let keyColumns = toList $ _pkColumns key
      in ( QCSelectPkey . SelPkOpCtx tn hdrs selFltr $ mkPGColGNameMap keyColumns
         , mkSelFldPKey selByPkCustName tn keyColumns
         )

    getFuncQueryFlds (selFltr, pLimit, hdrs, _) =
      funcFldHelper QCFuncQuery mkFuncQueryFld selFltr pLimit hdrs

    getFuncAggQueryFlds (selFltr, pLimit, hdrs, True) =
      funcFldHelper QCFuncAggQuery mkFuncAggQueryFld selFltr pLimit hdrs
    getFuncAggQueryFlds _                             = []

    funcFldHelper f g pFltr pLimit hdrs =
      flip map funcs $ \fi ->
      ( f $ FuncQOpCtx (fiName fi) (mkFuncArgItemSeq fi) hdrs colGNameMap pFltr pLimit
      , g fi $ fiDescription fi
      )

    mkFuncArgItemSeq functionInfo =
      let inputArgs = fiInputArgs functionInfo
      in Seq.fromList $ procFuncArgs inputArgs nameFn resultFn
      where
        nameFn = \case
          IAUserProvided fa       -> faName fa
          IASessionVariables name -> Just name
        resultFn arg gName = flip fmap arg $
          \fa -> FunctionArgItem (G.Name gName) (faName fa) (faHasDefault fa)


getSelPermission :: TableInfo -> RoleName -> Maybe SelPermInfo
getSelPermission tabInfo roleName =
  Map.lookup roleName (_tiRolePermInfoMap tabInfo) >>= _permSel

getSelPerm
  :: (MonadError QErr m)
  => TableCache
  -- all the fields of a table
  -> FieldInfoMap FieldInfo
  -- role and its permission
  -> RoleName -> SelPermInfo
  -> m (Bool, [SelField])
getSelPerm tableCache fields roleName selPermInfo = do

  relFlds <- fmap catMaybes $ forM validRels $ \relInfo -> do
      remTableInfo <- getTabInfo tableCache $ riRTable relInfo
      let remTableSelPermM = getSelPermission remTableInfo roleName
          remTableFlds = _tciFieldInfoMap $ _tiCoreInfo remTableInfo
          remTableColGNameMap =
            mkPGColGNameMap $ getValidCols remTableFlds
      return $ flip fmap remTableSelPermM $
        \rmSelPermM -> SFRelationship RelationshipFieldInfo
                             { _rfiInfo       = relInfo
                             , _rfiAllowAgg   = spiAllowAgg rmSelPermM
                             , _rfiColumns    = remTableColGNameMap
                             , _rfiPermFilter = spiFilter rmSelPermM
                             , _rfiPermLimit  = spiLimit rmSelPermM
                             , _rfiIsNullable = isRelNullable fields relInfo
                             }

  computedSelFields <- fmap catMaybes $ forM computedFields $ \info -> do
    let ComputedFieldInfo name function returnTy _ = info
        inputArgSeq = mkComputedFieldFunctionArgSeq $ _cffInputArgs function
    fmap (SFComputedField . ComputedField name function inputArgSeq) <$>
      case returnTy of
        CFRScalar scalarTy  -> pure $ Just $ CFTScalar scalarTy
        CFRSetofTable retTable -> do
          retTableInfo <- getTabInfo tableCache retTable
          let retTableSelPermM = getSelPermission retTableInfo roleName
              retTableFlds = _tciFieldInfoMap $ _tiCoreInfo retTableInfo
              retTableColGNameMap =
                mkPGColGNameMap $ getValidCols retTableFlds
          pure $ flip fmap retTableSelPermM $
            \selPerm -> CFTTable ComputedFieldTable
                        { _cftTable = retTable
                        , _cftCols = retTableColGNameMap
                        , _cftPermFilter = spiFilter selPerm
                        , _cftPermLimit = spiLimit selPerm
                        }

  return (spiAllowAgg selPermInfo, cols <> relFlds <> computedSelFields)
  where
    validRels = getValidRels fields
    validCols = getValidCols fields
    cols = map SFPGColumn $ getColInfos (toList allowedCols) validCols
    computedFields = flip filter (getComputedFieldInfos fields) $
                      \info -> case _cfiReturnType info of
                        CFRScalar _     ->
                          _cfiName info `Set.member` allowedScalarComputedFields
                        CFRSetofTable _ -> True

    allowedCols = spiCols selPermInfo
    allowedScalarComputedFields = spiScalarComputedFields selPermInfo

mkInsCtx
  :: MonadError QErr m
  => RoleName
  -> TableCache
  -> FieldInfoMap FieldInfo
  -> InsPermInfo
  -> Maybe UpdPermInfo
  -> m InsCtx
mkInsCtx role tableCache fields insPermInfo updPermM = do
  relTupsM <- forM rels $ \relInfo -> do
    let remoteTable = riRTable relInfo
        relName = riName relInfo
    remoteTableInfo <- getTabInfo tableCache remoteTable
    let insPermM = getInsPerm remoteTableInfo role
        viewInfoM = _tciViewInfo $ _tiCoreInfo remoteTableInfo
    return $ bool Nothing (Just (relName, relInfo)) $
      isInsertable insPermM viewInfoM && isValidRel relName remoteTable

  let relInfoMap = Map.fromList $ catMaybes relTupsM
  return $ InsCtx gNamePGColMap checkCond setCols relInfoMap updPermForIns
  where
    gNamePGColMap = mkPGColGNameMap allCols
    allCols = getCols fields
    rels = getValidRels fields
    setCols = ipiSet insPermInfo
    checkCond = ipiCheck insPermInfo
    updPermForIns = mkUpdPermForIns <$> updPermM
    mkUpdPermForIns upi = UpdPermForIns (toList $ upiCols upi) (upiCheck upi)
                          (upiFilter upi) (upiSet upi)

    isInsertable Nothing _          = False
    isInsertable (Just _) viewInfoM = isMutable viIsInsertable viewInfoM

mkAdminInsCtx
  :: MonadError QErr m
  => TableCache
  -> FieldInfoMap FieldInfo
  -> m InsCtx
mkAdminInsCtx tc fields = do
  relTupsM <- forM rels $ \relInfo -> do
    let remoteTable = riRTable relInfo
        relName = riName relInfo
    remoteTableInfo <- getTabInfo tc remoteTable
    let viewInfoM = _tciViewInfo $ _tiCoreInfo remoteTableInfo
    return $ bool Nothing (Just (relName, relInfo)) $
      isMutable viIsInsertable viewInfoM && isValidRel relName remoteTable

  let relInfoMap = Map.fromList $ catMaybes relTupsM
      updPerm = UpdPermForIns updCols Nothing noFilter Map.empty

  return $ InsCtx colGNameMap noFilter Map.empty relInfoMap (Just updPerm)
  where
    allCols = getCols fields
    colGNameMap = mkPGColGNameMap allCols
    updCols = map pgiColumn allCols
    rels = getValidRels fields

mkAdminSelFlds
  :: MonadError QErr m
  => FieldInfoMap FieldInfo
  -> TableCache
  -> m [SelField]
mkAdminSelFlds fields tableCache = do
  relSelFlds <- forM validRels $ \relInfo -> do
    let remoteTable = riRTable relInfo
    remoteTableInfo <- _tiCoreInfo <$> getTabInfo tableCache remoteTable
    let remoteTableFlds = _tciFieldInfoMap remoteTableInfo
        remoteTableColGNameMap =
          mkPGColGNameMap $ getValidCols remoteTableFlds
    return $ SFRelationship RelationshipFieldInfo
                   { _rfiInfo       = relInfo
                   , _rfiAllowAgg   = True
                   , _rfiColumns    = remoteTableColGNameMap
                   , _rfiPermFilter = noFilter
                   , _rfiPermLimit  = Nothing
                   , _rfiIsNullable = isRelNullable fields relInfo
                   }

  computedSelFields <- forM computedFields $ \info -> do
    let ComputedFieldInfo name function returnTy _ = info
        inputArgSeq = mkComputedFieldFunctionArgSeq $ _cffInputArgs function
    (SFComputedField . ComputedField name function inputArgSeq) <$>
      case returnTy of
        CFRScalar scalarTy  -> pure $ CFTScalar scalarTy
        CFRSetofTable retTable -> do
          retTableInfo <- _tiCoreInfo <$> getTabInfo tableCache retTable
          let retTableFlds = _tciFieldInfoMap retTableInfo
              retTableColGNameMap =
                mkPGColGNameMap $ getValidCols retTableFlds
          pure $ CFTTable ComputedFieldTable
                        { _cftTable = retTable
                        , _cftCols = retTableColGNameMap
                        , _cftPermFilter = noFilter
                        , _cftPermLimit = Nothing
                        }

  return $ colSelFlds <> relSelFlds <> computedSelFields
  where
    cols = getValidCols fields
    colSelFlds = map SFPGColumn cols
    validRels = getValidRels fields
    computedFields = getComputedFieldInfos fields

mkGCtxRole
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
mkGCtxRole tableCache tn descM fields primaryKey constraints funcs viM tabConfigM role permInfo = do
  selPermM <- mapM (getSelPerm tableCache fields role) $ _permSel permInfo
  tabInsInfoM <- forM (_permIns permInfo) $ \ipi -> do
    ctx <- mkInsCtx role tableCache fields ipi $ _permUpd permInfo
    let permCols = flip getColInfos allCols $ Set.toList $ ipiCols ipi
    return (ctx, (permCols, icRelations ctx))
  let insPermM = snd <$> tabInsInfoM
      insCtxM = fst <$> tabInsInfoM
      updColsM = filterColFlds . upiCols <$> _permUpd permInfo
      tyAgg = mkGCtxRole' tn descM insPermM selPermM updColsM
              (void $ _permDel permInfo) primaryKey constraints viM funcs
      rootFlds = getRootFldsRole tn primaryKey constraints fields funcs
                 viM permInfo tabConfigM
      insCtxMap = maybe Map.empty (Map.singleton tn) insCtxM
  return (tyAgg, rootFlds, insCtxMap)
  where
    allCols = getCols fields
    cols = getValidCols fields
    filterColFlds allowedSet =
      filter ((`Set.member` allowedSet) . pgiColumn) cols

getRootFldsRole
  :: QualifiedTable
  -> Maybe (PrimaryKey PGColumnInfo)
  -> [ConstraintName]
  -> FieldInfoMap FieldInfo
  -> [FunctionInfo]
  -> Maybe ViewInfo
  -> RolePermInfo
  -> TableConfig
  -> RootFields
getRootFldsRole tn pCols constraints fields funcs viM (RolePermInfo insM selM updM delM)=
  getRootFldsRole' tn pCols constraints fields funcs
  (mkIns <$> insM) (mkSel <$> selM)
  (mkUpd <$> updM) (mkDel <$> delM) viM
  where
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

    allCols = getCols fields

mkGCtxMapTable
  :: (MonadError QErr m)
  => TableCache
  -> FunctionCache
  -> TableInfo
  -> m (Map.HashMap RoleName TableSchemaCtx)
mkGCtxMapTable tableCache funcCache tabInfo = do
  m <- flip Map.traverseWithKey rolePermsMap $ \roleName rolePerm ->
    for rolePerm $ mkGCtxRole tableCache tn descM fields primaryKey validConstraints
                             tabFuncs viewInfo customConfig roleName
  adminInsCtx <- mkAdminInsCtx tableCache fields
  adminSelFlds <- mkAdminSelFlds fields tableCache
  let adminCtx = mkGCtxRole' tn descM (Just (cols, icRelations adminInsCtx))
                 (Just (True, adminSelFlds)) (Just cols) (Just ())
                 primaryKey validConstraints viewInfo tabFuncs
      adminInsCtxMap = Map.singleton tn adminInsCtx
      adminTableCtx = RoleContext (adminCtx, adminRootFlds, adminInsCtxMap) Nothing
  pure $ Map.insert adminRoleName adminTableCtx m
  where
    TableInfo coreInfo rolePerms _ = tabInfo
    TableCoreInfo tn descM _ fields primaryKey _ _ viewInfo _ customConfig = coreInfo
    validConstraints = mkValidConstraints $ map _cName (tciUniqueOrPrimaryKeyConstraints coreInfo)
    cols = getValidCols fields
    tabFuncs = filter (isValidObjectName . fiName) $ getFuncsOfTable tn funcCache

    adminRootFlds =
      getRootFldsRole' tn primaryKey validConstraints fields tabFuncs
      (Just ([], True)) (Just (noFilter, Nothing, [], True))
      (Just (cols, mempty, noFilter, Nothing, [])) (Just (noFilter, []))
      viewInfo customConfig

    rolePermsMap :: Map.HashMap RoleName (RoleContext RolePermInfo)
    rolePermsMap = flip Map.map rolePerms $ \permInfo ->
      case _permIns permInfo of
        Nothing      -> RoleContext permInfo Nothing
        Just insPerm ->
          if ipiBackendOnly insPerm then
            -- Remove insert permission from 'default' context and keep it in 'backend' context.
            RoleContext { _rctxDefault = permInfo{_permIns = Nothing}
                        , _rctxBackend = Just permInfo
                        }
          else RoleContext permInfo Nothing

noFilter :: AnnBoolExpPartialSQL
noFilter = annBoolExpTrue

{- Note [Split schema generation (TODO)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As of writing this, the schema is generated per table per role and for all permissions.
See functions  "mkGCtxRole'" and "getRootFldsRole'". This approach makes hard to
differentiate schema generation for each operation (select, insert, delete and update)
based on respective permission information and safe merging those schemas eventually.
For backend-only inserts (see https://github.com/hasura/graphql-engine/pull/4224)
we need to somehow defer the logic of merging schema for inserts with others based on its
backend-only credibility. This requires significant refactor of this module and
we can't afford to do it as of now since we're going to rewrite the entire GraphQL schema
generation (see https://github.com/hasura/graphql-engine/pull/4111). For aforementioned
backend-only inserts, we're following a hacky implementation of generating schema for
both default session and one with backend privilege -- the later differs with the former by
only having the schema related to insert operation.
-}

mkGCtxMap
  :: forall m. (MonadError QErr m)
  => AnnotatedObjects -> TableCache -> FunctionCache -> ActionCache -> m GCtxMap
mkGCtxMap annotatedObjects tableCache functionCache actionCache = do
  typesMapL <- mapM (mkGCtxMapTable tableCache functionCache) $
               filter (tableFltr . _tiCoreInfo) $ Map.elems tableCache
  actionsSchema <- mkActionsSchema annotatedObjects actionCache
  typesMap <- combineTypes actionsSchema typesMapL
  let gCtxMap  = flip Map.map typesMap $
                 fmap (\(ty, flds, insCtxMap) -> mkGCtx ty flds insCtxMap)
  pure gCtxMap
  where
    tableFltr ti = not (isSystemDefined $ _tciSystemDefined ti) && isValidObjectName (_tciName ti)

    combineTypes
      :: Map.HashMap RoleName (RootFields, TyAgg)
      -> [Map.HashMap RoleName TableSchemaCtx]
      -> m (Map.HashMap RoleName TableSchemaCtx)
    combineTypes actionsSchema tableCtxMaps = do
      let tableCtxsMap =
            foldr (Map.unionWith (++) . Map.map pure)
            ((\(rf, tyAgg) -> pure $ RoleContext (tyAgg, rf, mempty) Nothing) <$> actionsSchema)
            tableCtxMaps

      flip Map.traverseWithKey tableCtxsMap $ \_ tableSchemaCtxs -> do
        let defaultTableSchemaCtxs = map _rctxDefault tableSchemaCtxs
            backendGCtxTypesMaybe =
              -- If no table has 'backend' schema context then
              -- aggregated context should be Nothing
              if all (isNothing . _rctxBackend) tableSchemaCtxs then Nothing
              else Just $ flip map tableSchemaCtxs $
                   -- Consider 'default' if 'backend' doesn't exist for any table
                   -- see Note [Split schema generation (TODO)]
                   \(RoleContext def backend) -> fromMaybe def backend

        RoleContext <$> combineTypes' defaultTableSchemaCtxs
                    <*> mapM combineTypes' backendGCtxTypesMaybe
      where
        combineTypes' :: [(TyAgg, RootFields, InsCtxMap)] -> m (TyAgg, RootFields, InsCtxMap)
        combineTypes' typeList = do
          let tyAgg = mconcat $ map (^. _1) typeList
              insCtx = mconcat $ map (^. _3) typeList
          rootFields <- combineRootFields $ map (^. _2) typeList
          pure (tyAgg, rootFields, insCtx)

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

getGCtx :: (MonadError QErr m)
        => BackendOnlyPermissions -> SchemaCache -> RoleName -> m GCtx
getGCtx useBackendOnlyPermissions sc roleName =
  case Map.lookup roleName (scGCtxMap sc) of
    Nothing                                           -> pure $ scDefaultRemoteGCtx sc
    Just (RoleContext defaultGCtx maybeBackendGCtx)   ->
      case useBackendOnlyPermissions of
        BOPEnabled -> maybe
          (throw400 BadRequest $ "Backend privilage not found for the role " <>> roleName)
          pure
          maybeBackendGCtx
        BOPDisabled -> pure defaultGCtx

-- pretty print GCtx
ppGCtx :: GCtx -> String
ppGCtx gCtx =
  "GCtx ["
  <> "\n  types = " <> show types
  <> "\n  query root = " <> show qRoot
  <> "\n  mutation root = " <> show mRoot
  <> "\n  subscription root = " <> show sRoot
  <> "\n]"

  where
    types = map (G.unName . G.unNamedType) $ Map.keys $ _gTypes gCtx
    qRoot = (,) (_otiName qRootO) $
            map G.unName $ Map.keys $ _otiFields qRootO
    mRoot = (,) (_otiName <$> mRootO) $
            maybe [] (map G.unName . Map.keys . _otiFields) mRootO
    sRoot = (,) (_otiName <$> sRootO) $
            maybe [] (map G.unName . Map.keys . _otiFields) sRootO
    qRootO = _gQueryRoot gCtx
    mRootO = _gMutRoot gCtx
    sRootO = _gSubRoot gCtx

mkGCtx :: TyAgg -> RootFields -> InsCtxMap -> GCtx
mkGCtx tyAgg (RootFields queryFields mutationFields) insCtxMap =
  let queryRoot = mkQueryRootTyInfo qFlds
      scalarTys = map (TIScalar . mkHsraScalarTyInfo) (Set.toList allScalarTypes)
      compTys   = map (TIInpObj . mkCompExpInp) (Set.toList allComparableTypes)
      ordByEnumTyM = bool (Just ordByEnumTy) Nothing $ null qFlds
      allTys    = Map.union tyInfos $ mkTyInfoMap $
                  catMaybes [ Just $ TIObj queryRoot
                            , TIObj <$> mutRootM
                            , TIObj <$> subRootM
                            , TIEnum <$> ordByEnumTyM
                            ] <>
                  scalarTys <> compTys <> defaultTypes <> wiredInGeoInputTypes
                  <> wiredInRastInputTypes
  -- for now subscription root is query root
  in GCtx allTys fldInfos queryRoot mutRootM subRootM ordByEnums
     (Map.map fst queryFields) (Map.map fst mutationFields) insCtxMap
  where
    TyAgg tyInfos fldInfos scalars ordByEnums = tyAgg
    colTys = Set.fromList $ map pgiType $ mapMaybe (^? _RFPGColumn) $
             Map.elems fldInfos
    mkMutRoot =
      mkHsraObjTyInfo (Just "mutation root") mutationRootNamedType Set.empty .
      mapFromL _fiName
    mutRootM = bool (Just $ mkMutRoot mFlds) Nothing $ null mFlds
    mkSubRoot =
      mkHsraObjTyInfo (Just "subscription root")
      subscriptionRootNamedType Set.empty . mapFromL _fiName
    subRootM = bool (Just $ mkSubRoot qFlds) Nothing $ null qFlds

    qFlds = rootFieldInfos queryFields
    mFlds = rootFieldInfos mutationFields
    rootFieldInfos = map snd . Map.elems

    anyGeoTypes = any (isScalarColumnWhere isGeoType) colTys
    allComparableTypes =
      if anyGeoTypes
        -- due to casting, we need to generate both geometry and geography
        -- operations even if just one of the two appears in the schema
        then Set.union (Set.fromList [PGColumnScalar PGGeometry, PGColumnScalar PGGeography]) colTys
        else colTys

    additionalScalars = Set.fromList $
        -- raster comparison expression needs geometry input
      (guard anyRasterTypes *> pure PGGeometry)
        -- scalar computed field return types
      <> mapMaybe (^? _RFComputedField.cfType._CFTScalar) (Map.elems fldInfos)

    allScalarTypes = (allComparableTypes ^.. folded._PGColumnScalar)
                     <> additionalScalars <> scalars

    wiredInGeoInputTypes = guard anyGeoTypes *> map TIInpObj geoInputTypes

    anyRasterTypes = any (isScalarColumnWhere (== PGRaster)) colTys
    wiredInRastInputTypes = guard anyRasterTypes *>
                            map TIInpObj rasterIntersectsInputTypes
