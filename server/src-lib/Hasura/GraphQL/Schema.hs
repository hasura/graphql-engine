module Hasura.GraphQL.Schema
  ( mkGCtxMap
  , GCtxMap
  , buildGCtxMapPG
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
import           Hasura.SQL.Types

import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Function
import           Hasura.GraphQL.Schema.Merge
import           Hasura.GraphQL.Schema.Mutation.Common
import           Hasura.GraphQL.Schema.Mutation.Delete
import           Hasura.GraphQL.Schema.Mutation.Insert
import           Hasura.GraphQL.Schema.Mutation.Update
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Schema.Select

getInsPerm :: TableInfo PGColumnInfo -> RoleName -> Maybe InsPermInfo
getInsPerm tabInfo role
  | role == adminRole = _permIns $ mkAdminRolePermInfo tabInfo
  | otherwise = Map.lookup role rolePermInfoMap >>= _permIns
  where
    rolePermInfoMap = _tiRolePermInfoMap tabInfo

getTabInfo
  :: MonadError QErr m
  => TableCache PGColumnInfo -> QualifiedTable -> m (TableInfo PGColumnInfo)
getTabInfo tc t =
  onNothing (Map.lookup t tc) $
     throw500 $ "table not found: " <>> t

isValidObjectName :: (ToTxt a) => QualifiedObject a -> Bool
isValidObjectName = isValidName . qualObjectToName

isValidCol :: PGCol -> Bool
isValidCol = isValidName . G.Name . getPGColTxt

isValidRel :: ToTxt a => RelName -> QualifiedObject a -> Bool
isValidRel rn rt = isValidName (mkRelName rn) && isValidObjectName rt

isValidField :: FieldInfo PGColumnInfo -> Bool
isValidField = \case
  FIColumn (PGColumnInfo col _ _) -> isValidCol col
  FIRelationship (RelInfo rn _ _ remTab _) -> isValidRel rn remTab

upsertable :: [ConstraintName] -> Bool -> Bool -> Bool
upsertable uniqueOrPrimaryCons isUpsertAllowed isAView =
  not (null uniqueOrPrimaryCons) && isUpsertAllowed && not isAView

toValidFieldInfos :: FieldInfoMap PGColumnInfo -> [FieldInfo PGColumnInfo]
toValidFieldInfos = filter isValidField . Map.elems

validPartitionFieldInfoMap :: FieldInfoMap PGColumnInfo -> ([PGColumnInfo], [RelInfo])
validPartitionFieldInfoMap = partitionFieldInfos . toValidFieldInfos

getValidCols :: FieldInfoMap PGColumnInfo -> [PGColumnInfo]
getValidCols = fst . validPartitionFieldInfoMap

getValidRels :: FieldInfoMap PGColumnInfo -> [RelInfo]
getValidRels = snd . validPartitionFieldInfoMap

mkValidConstraints :: [ConstraintName] -> [ConstraintName]
mkValidConstraints =
  filter (isValidName . G.Name . getConstraintTxt)

isRelNullable :: FieldInfoMap PGColumnInfo -> RelInfo -> Bool
isRelNullable fim ri = isNullable
  where
    lCols = map fst $ riMapping ri
    allCols = getValidCols fim
    lColInfos = getColInfos lCols allCols
    isNullable = any pgiIsNullable lColInfos

numAggOps :: [G.Name]
numAggOps = [ "sum", "avg", "stddev", "stddev_samp", "stddev_pop"
            , "variance", "var_samp", "var_pop"
            ]

compAggOps :: [G.Name]
compAggOps = ["max", "min"]

isAggFld :: G.Name -> Bool
isAggFld = flip elem (numAggOps <> compAggOps)

mkGCtxRole'
  :: QualifiedTable
  -> Maybe ([PGColumnInfo], RelationInfoMap)
  -- ^ insert permission
  -> Maybe (Bool, [SelField])
  -- ^ select permission
  -> Maybe [PGColumnInfo]
  -- ^ update cols
  -> Maybe ()
  -- ^ delete cols
  -> [PGColumnInfo]
  -- ^ primary key columns
  -> [ConstraintName]
  -- ^ constraints
  -> Maybe ViewInfo
  -> [FunctionInfo]
  -- ^ all functions
  -> Maybe EnumValues
  -- ^ present iff this table is an enum table (see "Hasura.RQL.Schema.Enum")
  -> TyAgg
mkGCtxRole' tn insPermM selPermM updColsM
            delPermM pkeyCols constraints viM funcs enumValuesM =

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
               <> funcInpArgTys

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
      , TIObj <$> mutRespObjM
      , TIEnum <$> selColInpTyM
      , TIEnum <$> tableEnumTypeM
      ]

    mutHelper :: (ViewInfo -> Bool) -> Maybe a -> Maybe a
    mutHelper f objM = bool Nothing objM $ isMutable f viM

    fieldMap = Map.unions $ catMaybes
               [ insInpObjFldsM, updSetInpObjFldsM
               , boolExpInpObjFldsM , selObjFldsM
               ]
    scalars = selByPkScalarSet <> funcArgScalarSet

    -- helper
    mkColFldMap ty cols = Map.fromList $ flip map cols $
      \c -> ((ty, mkColName $ pgiName c), Left c)

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

    selFldsM = snd <$> selPermM
    selColsM = (map pgiName . lefts) <$> selFldsM
    selColInpTyM = mkSelColumnTy tn <$> selColsM
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
    funcArgInpObjs = mapMaybe mkFuncArgsInp funcs
    -- funcArgCtx = Map.unions funcArgCtxs
    funcArgScalarSet = funcs ^.. folded.to fiInputArgs.folded.to faType

    -- helper
    mkFldMap ty = Map.fromList . concatMap (mkFld ty)
    mkFld ty = \case
      Left ci -> [((ty, mkColName $ pgiName ci), Left ci)]
      Right (ri, allowAgg, perm, lim, _) ->
        let relFld = ( (ty, mkRelName $ riName ri)
                     , Right (ri, False, perm, lim)
                     )
            aggRelFld = ( (ty, mkAggRelName $ riName ri)
                        , Right (ri, True, perm, lim)
                        )
        in case riType ri of
          ObjRel -> [relFld]
          ArrRel -> bool [relFld] [relFld, aggRelFld] allowAgg

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
    selObjM = mkTableObj tn <$> selFldsM

    -- aggregate objs and order by inputs
    (aggObjs, aggOrdByInps) = case selPermM of
      Just (True, selFlds) ->
        let numCols = (map pgiName . getNumCols) selFlds
            compCols = (map pgiName . getCompCols) selFlds
            objs = [ mkTableAggObj tn
                   , mkTableAggFldsObj tn (numCols, numAggOps) (compCols, compAggOps)
                   ] <> mkColAggFldsObjs selFlds
            ordByInps = mkTabAggOrdByInpObj tn (numCols, numAggOps) (compCols, compAggOps)
                        : mkTabAggOpOrdByInpObjs tn (numCols, numAggOps) (compCols, compAggOps)
        in (objs, ordByInps)
      _ -> ([], [])

    getNumCols = onlyNumCols . lefts
    getCompCols = onlyComparableCols . lefts
    onlyFloat = const $ mkScalarTy PGFloat

    mkTypeMaker "sum" = mkColumnType
    mkTypeMaker _     = onlyFloat

    mkColAggFldsObjs flds =
      let numCols = getNumCols flds
          compCols = getCompCols flds
          mkNumObjFld n = mkTableColAggFldsObj tn n (mkTypeMaker n) numCols
          mkCompObjFld n = mkTableColAggFldsObj tn n mkColumnType compCols
          numFldsObjs = bool (map mkNumObjFld numAggOps) [] $ null numCols
          compFldsObjs = bool (map mkCompObjFld compAggOps) [] $ null compCols
      in numFldsObjs <> compFldsObjs
    -- the fields used in table object
    selObjFldsM = mkFldMap (mkTableTy tn) <$> selFldsM
    -- the scalar set for table_by_pk arguments
    selByPkScalarSet = pkeyCols ^.. folded.to pgiType._PGColumnScalar

    ordByInpCtxM = mkOrdByInpObj tn <$> selFldsM
    (ordByInpObjM, ordByCtxM) = case ordByInpCtxM of
      Just (a, b) -> (Just a, Just b)
      Nothing     -> (Nothing, Nothing)

    tableEnumTypeM = enumValuesM <&> \enumValues ->
      mkHsraEnumTyInfo Nothing (mkTableEnumType tn) $
        EnumValuesReference (EnumReference tn enumValues)

getRootFldsRole'
  :: QualifiedTable
  -> [PGCol]
  -> [ConstraintName]
  -> FieldInfoMap PGColumnInfo
  -> [FunctionInfo]
  -> Maybe ([T.Text], Bool) -- insert perm
  -> Maybe (AnnBoolExpPartialSQL, Maybe Int, [T.Text], Bool) -- select filter
  -> Maybe ([PGCol], PreSetColsPartial, AnnBoolExpPartialSQL, [T.Text]) -- update filter
  -> Maybe (AnnBoolExpPartialSQL, [T.Text]) -- delete filter
  -> Maybe ViewInfo
  -> RootFields
getRootFldsRole' tn primCols constraints fields funcs insM selM updM delM viM =
  RootFields
    { rootQueryFields = makeFieldMap
        $  funcQueries
        <> funcAggQueries
        <> catMaybes
          [ getSelDet <$> selM
          , getSelAggDet selM
          , getPKeySelDet selM $ getColInfos primCols colInfos
          ]
    , rootMutationFields = makeFieldMap $ catMaybes
        [ mutHelper viIsInsertable getInsDet insM
        , mutHelper viIsUpdatable getUpdDet updM
        , mutHelper viIsDeletable getDelDet delM
        ]
    }
  where
    makeFieldMap = mapFromL (_fiName . snd)
    allCols = getCols fields
    funcQueries = maybe [] getFuncQueryFlds selM
    funcAggQueries = maybe [] getFuncAggQueryFlds selM

    mutHelper :: (ViewInfo -> Bool) -> (a -> b) -> Maybe a -> Maybe b
    mutHelper f getDet mutM =
      bool Nothing (getDet <$> mutM) $ isMutable f viM

    colInfos = fst $ validPartitionFieldInfoMap fields
    getInsDet (hdrs, upsertPerm) =
      let isUpsertable = upsertable constraints upsertPerm $ isJust viM
      in ( MCInsert . InsOpCtx tn $ hdrs `union` maybe [] (\(_, _, _, x) -> x) updM
         , mkInsMutFld tn isUpsertable
         )

    getUpdDet (updCols, preSetCols, updFltr, hdrs) =
      ( MCUpdate $ UpdOpCtx tn hdrs updFltr preSetCols allCols
      , mkUpdMutFld tn $ getColInfos updCols colInfos
      )

    getDelDet (delFltr, hdrs) =
      ( MCDelete $ DelOpCtx tn hdrs delFltr allCols
      , mkDelMutFld tn
      )
    getSelDet (selFltr, pLimit, hdrs, _) =
      selFldHelper QCSelect mkSelFld selFltr pLimit hdrs

    getSelAggDet (Just (selFltr, pLimit, hdrs, True)) =
      Just $ selFldHelper QCSelectAgg mkAggSelFld selFltr pLimit hdrs
    getSelAggDet _                                    = Nothing

    selFldHelper f g pFltr pLimit hdrs =
      ( f $ SelOpCtx tn hdrs pFltr pLimit
      , g tn
      )

    getPKeySelDet Nothing _ = Nothing
    getPKeySelDet _ [] = Nothing
    getPKeySelDet (Just (selFltr, _, hdrs, _)) pCols = Just
      ( QCSelectPkey . SelPkOpCtx tn hdrs selFltr $
        mapFromL (mkColName . pgiName) pCols
      , mkSelFldPKey tn pCols
      )

    getFuncQueryFlds (selFltr, pLimit, hdrs, _) =
      funcFldHelper QCFuncQuery mkFuncQueryFld selFltr pLimit hdrs

    getFuncAggQueryFlds (selFltr, pLimit, hdrs, True) =
      funcFldHelper QCFuncAggQuery mkFuncAggQueryFld selFltr pLimit hdrs
    getFuncAggQueryFlds _                             = []

    funcFldHelper f g pFltr pLimit hdrs =
      flip map funcs $ \fi ->
      ( f . FuncQOpCtx tn hdrs pFltr pLimit (fiName fi) $ mkFuncArgItemSeq fi
      , g fi
      )

    mkFuncArgItemSeq fi = Seq.fromList $ procFuncArgs (fiInputArgs fi)
                          $ \fa t -> FuncArgItem (G.Name t) (faName fa) (faHasDefault fa)


getSelPermission :: TableInfo PGColumnInfo -> RoleName -> Maybe SelPermInfo
getSelPermission tabInfo role =
  Map.lookup role (_tiRolePermInfoMap tabInfo) >>= _permSel

getSelPerm
  :: (MonadError QErr m)
  => TableCache PGColumnInfo
  -- all the fields of a table
  -> FieldInfoMap PGColumnInfo
  -- role and its permission
  -> RoleName -> SelPermInfo
  -> m (Bool, [SelField])
getSelPerm tableCache fields role selPermInfo = do
  selFlds <- fmap catMaybes $ forM (toValidFieldInfos fields) $ \case
    FIColumn pgColInfo ->
      return $ fmap Left $ bool Nothing (Just pgColInfo) $
      Set.member (pgiName pgColInfo) allowedCols
    FIRelationship relInfo -> do
      remTableInfo <- getTabInfo tableCache $ riRTable relInfo
      let remTableSelPermM = getSelPermission remTableInfo role
      return $ flip fmap remTableSelPermM $
        \rmSelPermM -> Right ( relInfo
                             , spiAllowAgg rmSelPermM
                             , spiFilter rmSelPermM
                             , spiLimit rmSelPermM
                             , isRelNullable fields relInfo
                             )
  return (spiAllowAgg selPermInfo, selFlds)
  where
    allowedCols = spiCols selPermInfo

mkInsCtx
  :: MonadError QErr m
  => RoleName
  -> TableCache PGColumnInfo
  -> FieldInfoMap PGColumnInfo
  -> InsPermInfo
  -> Maybe UpdPermInfo
  -> m InsCtx
mkInsCtx role tableCache fields insPermInfo updPermM = do
  relTupsM <- forM rels $ \relInfo -> do
    let remoteTable = riRTable relInfo
        relName = riName relInfo
    remoteTableInfo <- getTabInfo tableCache remoteTable
    let insPermM = getInsPerm remoteTableInfo role
        viewInfoM = _tiViewInfo remoteTableInfo
    return $ bool Nothing (Just (relName, relInfo)) $
      isInsertable insPermM viewInfoM && isValidRel relName remoteTable

  let relInfoMap = Map.fromList $ catMaybes relTupsM
  return $ InsCtx iView allCols setCols relInfoMap updPermForIns
  where
    allCols = getCols fields
    rels = getValidRels fields
    iView = ipiView insPermInfo
    setCols = ipiSet insPermInfo
    updPermForIns = mkUpdPermForIns <$> updPermM
    mkUpdPermForIns upi = UpdPermForIns (toList $ upiCols upi)
                          (upiFilter upi) (upiSet upi)

    isInsertable Nothing _          = False
    isInsertable (Just _) viewInfoM = isMutable viIsInsertable viewInfoM

mkAdminInsCtx
  :: MonadError QErr m
  => QualifiedTable
  -> TableCache PGColumnInfo
  -> FieldInfoMap PGColumnInfo
  -> m InsCtx
mkAdminInsCtx tn tc fields = do
  relTupsM <- forM rels $ \relInfo -> do
    let remoteTable = riRTable relInfo
        relName = riName relInfo
    remoteTableInfo <- getTabInfo tc remoteTable
    let viewInfoM = _tiViewInfo remoteTableInfo
    return $ bool Nothing (Just (relName, relInfo)) $
      isMutable viIsInsertable viewInfoM && isValidRel relName remoteTable

  let relInfoMap = Map.fromList $ catMaybes relTupsM
      updPerm = UpdPermForIns updCols noFilter Map.empty

  return $ InsCtx tn allCols Map.empty relInfoMap (Just updPerm)
  where
    allCols = getCols fields
    updCols = map pgiName $ getValidCols fields
    rels = getValidRels fields

mkGCtxRole
  :: (MonadError QErr m)
  => TableCache PGColumnInfo
  -> QualifiedTable
  -> FieldInfoMap PGColumnInfo
  -> [PGCol]
  -> [ConstraintName]
  -> [FunctionInfo]
  -> Maybe ViewInfo
  -> Maybe EnumValues
  -> RoleName
  -> RolePermInfo
  -> m (TyAgg, RootFields, InsCtxMap)
mkGCtxRole tableCache tn fields pCols constraints funcs viM enumValuesM role permInfo = do
  selPermM <- mapM (getSelPerm tableCache fields role) $ _permSel permInfo
  tabInsInfoM <- forM (_permIns permInfo) $ \ipi -> do
    ctx <- mkInsCtx role tableCache fields ipi $ _permUpd permInfo
    let permCols = flip getColInfos allCols $
                   filter isValidCol $ Set.toList $ ipiCols ipi
    return (ctx, (permCols, icRelations ctx))
  let insPermM = snd <$> tabInsInfoM
      insCtxM = fst <$> tabInsInfoM
      updColsM = filterColInfos . upiCols <$> _permUpd permInfo
      tyAgg = mkGCtxRole' tn insPermM selPermM updColsM
              (void $ _permDel permInfo) pColInfos constraints viM funcs enumValuesM
      rootFlds = getRootFldsRole tn pCols constraints fields funcs viM permInfo
      insCtxMap = maybe Map.empty (Map.singleton tn) insCtxM
  return (tyAgg, rootFlds, insCtxMap)
  where
    allCols = getCols fields
    colInfos = getValidCols fields
    pColInfos = getColInfos pCols allCols
    filterColInfos allowedSet =
      filter ((`Set.member` allowedSet) . pgiName) colInfos

getRootFldsRole
  :: QualifiedTable
  -> [PGCol]
  -> [ConstraintName]
  -> FieldInfoMap PGColumnInfo
  -> [FunctionInfo]
  -> Maybe ViewInfo
  -> RolePermInfo
  -> RootFields
getRootFldsRole tn pCols constraints fields funcs viM (RolePermInfo insM selM updM delM) =
  getRootFldsRole' tn pCols constraints fields funcs
  (mkIns <$> insM) (mkSel <$> selM)
  (mkUpd <$> updM) (mkDel <$> delM)
  viM
  where
    mkIns i = (ipiRequiredHeaders i, isJust updM)
    mkSel s = ( spiFilter s, spiLimit s
              , spiRequiredHeaders s, spiAllowAgg s
              )
    mkUpd u = ( Set.toList $ upiCols u
              , upiSet u
              , upiFilter u
              , upiRequiredHeaders u
              )
    mkDel d = (dpiFilter d, dpiRequiredHeaders d)

mkGCtxMapTable
  :: (MonadError QErr m)
  => TableCache PGColumnInfo
  -> FunctionCache
  -> TableInfo PGColumnInfo
  -> m (Map.HashMap RoleName (TyAgg, RootFields, InsCtxMap))
mkGCtxMapTable tableCache funcCache tabInfo = do
  m <- Map.traverseWithKey
       (mkGCtxRole tableCache tn fields pkeyCols validConstraints tabFuncs viewInfo enumValues)
       rolePerms
  adminInsCtx <- mkAdminInsCtx tn tableCache fields
  let adminCtx = mkGCtxRole' tn (Just (colInfos, icRelations adminInsCtx))
                 (Just (True, selFlds)) (Just colInfos) (Just ())
                 pkeyColInfos validConstraints viewInfo tabFuncs enumValues
      adminInsCtxMap = Map.singleton tn adminInsCtx
  return $ Map.insert adminRole (adminCtx, adminRootFlds, adminInsCtxMap) m
  where
    TableInfo tn _ fields rolePerms constraints pkeyCols viewInfo _ enumValues = tabInfo
    validConstraints = mkValidConstraints constraints
    colInfos = getValidCols fields
    validColNames = map pgiName colInfos
    pkeyColInfos = getColInfos pkeyCols colInfos
    tabFuncs = filter (isValidObjectName . fiName) $
               getFuncsOfTable tn funcCache
    selFlds = flip map (toValidFieldInfos fields) $ \case
      FIColumn pgColInfo     -> Left pgColInfo
      FIRelationship relInfo -> Right (relInfo, True, noFilter, Nothing, isRelNullable fields relInfo)
    adminRootFlds =
      getRootFldsRole' tn pkeyCols validConstraints fields tabFuncs
      (Just ([], True)) (Just (noFilter, Nothing, [], True))
      (Just (validColNames, mempty, noFilter, [])) (Just (noFilter, []))
      viewInfo

noFilter :: AnnBoolExpPartialSQL
noFilter = annBoolExpTrue

mkGCtxMap
  :: (MonadError QErr m)
  => TableCache PGColumnInfo -> FunctionCache -> m GCtxMap
mkGCtxMap tableCache functionCache = do
  typesMapL <- mapM (mkGCtxMapTable tableCache functionCache) $
               filter tableFltr $ Map.elems tableCache
  let typesMap = foldr (Map.unionWith mappend) Map.empty typesMapL
  return $ flip Map.map typesMap $ \(ty, flds, insCtxMap) ->
    mkGCtx ty flds insCtxMap
  where
    tableFltr ti = not (_tiSystemDefined ti)
                   && isValidObjectName (_tiName ti)

-- | build GraphQL schema from postgres tables and functions
buildGCtxMapPG
  :: (QErrM m, CacheRWM m)
  => m ()
buildGCtxMapPG = do
  sc <- askSchemaCache
  gCtxMap <- mkGCtxMap (scTables sc) (scFunctions sc)
  writeSchemaCache sc {scGCtxMap = gCtxMap}

getGCtx :: (CacheRM m) => RoleName -> GCtxMap -> m GCtx
getGCtx rn ctxMap = do
  sc <- askSchemaCache
  return $ fromMaybe (scDefaultRemoteGCtx sc) $ Map.lookup rn ctxMap

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

-- | A /types aggregate/, which holds role-specific information about visible GraphQL types.
-- Importantly, it holds more than just the information needed by GraphQL: it also includes how the
-- GraphQL types relate to Postgres types, which is used to validate literals provided for
-- Postgres-specific scalars.
data TyAgg
  = TyAgg
  { _taTypes   :: !TypeMap
  , _taFields  :: !FieldMap
  , _taScalars :: !(Set.HashSet PGScalarType)
  , _taOrdBy   :: !OrdByCtx
  } deriving (Show, Eq)

instance Semigroup TyAgg where
  (TyAgg t1 f1 s1 o1) <> (TyAgg t2 f2 s2 o2) =
    TyAgg (Map.union t1 t2) (Map.union f1 f2)
          (Set.union s1 s2) (Map.union o1 o2)

instance Monoid TyAgg where
  mempty = TyAgg Map.empty Map.empty Set.empty Map.empty
  mappend = (<>)

-- | A role-specific mapping from root field names to allowed operations.
data RootFields
  = RootFields
  { rootQueryFields    :: !(Map.HashMap G.Name (QueryCtx, ObjFldInfo))
  , rootMutationFields :: !(Map.HashMap G.Name (MutationCtx, ObjFldInfo))
  } deriving (Show, Eq)

instance Semigroup RootFields where
  RootFields a1 b1 <> RootFields a2 b2
    = RootFields (Map.union a1 a2) (Map.union b1 b2)

instance Monoid RootFields where
  mempty = RootFields Map.empty Map.empty
  mappend  = (<>)

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
    colTys = Set.fromList $ map pgiType $ lefts $ Map.elems fldInfos
    mkMutRoot =
      mkHsraObjTyInfo (Just "mutation root") (G.NamedType "mutation_root") Set.empty .
      mapFromL _fiName
    mutRootM = bool (Just $ mkMutRoot mFlds) Nothing $ null mFlds
    mkSubRoot =
      mkHsraObjTyInfo (Just "subscription root")
      (G.NamedType "subscription_root") Set.empty . mapFromL _fiName
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

    additionalScalars =
      Set.fromList
        -- raster comparison expression needs geometry input
      (guard anyRasterTypes *> pure PGGeometry)

    allScalarTypes = (allComparableTypes ^.. folded._PGColumnScalar)
                     <> additionalScalars <> scalars

    wiredInGeoInputTypes = guard anyGeoTypes *> map TIInpObj geoInputTypes

    anyRasterTypes = any (isScalarColumnWhere (== PGRaster)) colTys
    wiredInRastInputTypes = guard anyRasterTypes *>
                            map TIInpObj rasterIntersectsInputTypes
