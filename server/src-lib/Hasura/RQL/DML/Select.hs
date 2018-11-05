{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.DML.Select
  ( selectP2
  , selectAggP2
  , mkSQLSelect
  , mkAggSelect
  , convSelectQuery
  , getSelectDeps
  , module Hasura.RQL.DML.Select.Internal
  )
where

import           Data.Aeson.Types
import           Data.List                      (unionBy)
import           Instances.TH.Lift              ()

import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Data.List.NonEmpty             as NE
import qualified Data.Sequence                  as DS

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Select.Internal
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Rewrite             (prefixNumToAliases)
import           Hasura.SQL.Types

import qualified Database.PG.Query              as Q
import qualified Hasura.SQL.DML                 as S

convSelCol :: (P1C m)
           => FieldInfoMap
           -> SelPermInfo
           -> SelCol
           -> m [ExtCol]
convSelCol _ _ (SCExtSimple cn) =
  return [ECSimple cn]
convSelCol fieldInfoMap _ (SCExtRel rn malias selQ) = do
  -- Point to the name key
  let pgWhenRelErr = "only relationships can be expanded"
  relInfo <- withPathK "name" $
    askRelType fieldInfoMap rn pgWhenRelErr
  let (RelInfo _ _ _ relTab _ _) = relInfo
  (rfim, rspi) <- fetchRelDet rn relTab
  resolvedSelQ <- resolveStar rfim rspi selQ
  return [ECRel rn malias resolvedSelQ]
convSelCol fieldInfoMap spi (SCStar wildcard) =
  convWildcard fieldInfoMap spi wildcard

convWildcard
  :: (P1C m)
  => FieldInfoMap
  -> SelPermInfo
  -> Wildcard
  -> m [ExtCol]
convWildcard fieldInfoMap (SelPermInfo cols _ _ _ _ _ _) wildcard =
  case wildcard of
  Star         -> return simpleCols
  (StarDot wc) -> (simpleCols ++) <$> (catMaybes <$> relExtCols wc)
  where
    (pgCols, relColInfos) = partitionFieldInfosWith (pgiName, id) $
                            HM.elems fieldInfoMap

    simpleCols = map ECSimple $ filter (`HS.member` cols) pgCols

    mkRelCol wc relInfo = do
      let relName = riName relInfo
          relTab  = riRTable relInfo
      relTabInfo <- fetchRelTabInfo relTab
      mRelSelPerm <- askPermInfo' PASelect relTabInfo

      forM mRelSelPerm $ \rspi -> do
        rExtCols <- convWildcard (tiFieldInfoMap relTabInfo) rspi wc
        return $ ECRel relName Nothing $
          SelectG rExtCols Nothing Nothing Nothing Nothing

    relExtCols wc = mapM (mkRelCol wc) relColInfos

resolveStar :: (P1C m)
            => FieldInfoMap
            -> SelPermInfo
            -> SelectQ
            -> m SelectQExt
resolveStar fim spi (SelectG selCols mWh mOb mLt mOf) = do
  procOverrides <- fmap (concat . catMaybes) $ withPathK "columns" $
    indexedForM selCols $ \selCol -> case selCol of
    (SCStar _) -> return Nothing
    _          -> Just <$> convSelCol fim spi selCol
  everything <- case wildcards of
    [] -> return []
    _  -> convWildcard fim spi $ maximum wildcards
  let extCols = unionBy equals procOverrides everything
  return $ SelectG extCols mWh mOb mLt mOf
  where
    wildcards = lefts $ map mkEither selCols

    mkEither (SCStar wc) = Left wc
    mkEither selCol      = Right selCol

    equals (ECSimple x) (ECSimple y)   = x == y
    equals (ECRel x _ _) (ECRel y _ _) = x == y
    equals _ _                         = False

convOrderByElem
  :: (P1C m)
  => (FieldInfoMap, SelPermInfo)
  -> OrderByCol
  -> m AnnObCol
convOrderByElem (flds, spi) = \case
  OCPG fldName -> do
    fldInfo <- askFieldInfo flds fldName
    case fldInfo of
      FIColumn colInfo -> do
        checkSelOnCol spi (pgiName colInfo)
        let ty = pgiType colInfo
        if ty == PGGeography || ty == PGGeometry
          then throw400 UnexpectedPayload $ mconcat
           [ fldName <<> " has type 'geometry'"
           , " and cannot be used in order_by"
           ]
          else return $ AOCPG colInfo
      FIRelationship _ -> throw400 UnexpectedPayload $ mconcat
        [ fldName <<> " is a"
        , " relationship and should be expanded"
        ]
  OCRel fldName rest -> do
    fldInfo <- askFieldInfo flds fldName
    case fldInfo of
      FIColumn _ -> throw400 UnexpectedPayload $ mconcat
        [ fldName <<> " is a Postgres column"
        , " and cannot be chained further"
        ]
      FIRelationship relInfo -> do
        when (riType relInfo == ArrRel) $
          throw400 UnexpectedPayload $ mconcat
          [ fldName <<> " is an array relationship"
          ," and can't be used in 'order_by'"
          ]
        (relFim, relSpi) <- fetchRelDet (riName relInfo) (riRTable relInfo)
        AOCRel relInfo (spiFilter relSpi) <$>
          convOrderByElem (relFim, relSpi) rest

convSelectQ
  :: (P1C m)
  => FieldInfoMap  -- Table information of current table
  -> SelPermInfo   -- Additional select permission info
  -> SelectQExt     -- Given Select Query
  -> (PGColType -> Value -> m S.SQLExp)
  -> m AnnSel
convSelectQ fieldInfoMap selPermInfo selQ prepValBuilder = do

  annFlds <- withPathK "columns" $
    indexedForM (sqColumns selQ) $ \case
    (ECSimple pgCol) -> do
      colInfo <- convExtSimple fieldInfoMap selPermInfo pgCol
      return (fromPGCol pgCol, FCol colInfo)
    (ECRel relName mAlias relSelQ) -> do
      annRel <- convExtRel fieldInfoMap relName mAlias relSelQ prepValBuilder
      return (fromRel $ fromMaybe relName mAlias, FRel annRel)

  let spiT = spiTable selPermInfo

  -- Convert where clause
  wClause <- forM (sqWhere selQ) $ \be ->
    withPathK "where" $
    convBoolExp' fieldInfoMap spiT selPermInfo be prepValBuilder

  annOrdByML <- forM (sqOrderBy selQ) $ \(OrderByExp obItems) ->
    withPathK "order_by" $ indexedForM obItems $ mapM $
    convOrderByElem (fieldInfoMap, selPermInfo)

  let annOrdByM = NE.nonEmpty =<< annOrdByML

  -- validate limit and offset values
  withPathK "limit" $ mapM_ onlyPositiveInt mQueryLimit
  withPathK "offset" $ mapM_ onlyPositiveInt mQueryOffset

  let tabFrom = TableFrom (spiTable selPermInfo) Nothing
      tabPerm = TablePerm (spiFilter selPermInfo) mPermLimit
  return $ AnnSelG annFlds tabFrom tabPerm $
    TableArgs wClause annOrdByM mQueryLimit (S.intToSQLExp <$> mQueryOffset)

  where
    mQueryOffset = sqOffset selQ
    mQueryLimit = sqLimit selQ
    mPermLimit = spiLimit selPermInfo

convExtSimple
  :: (P1C m)
  => FieldInfoMap
  -> SelPermInfo
  -> PGCol
  -> m PGColInfo
convExtSimple fieldInfoMap selPermInfo pgCol = do
  checkSelOnCol selPermInfo pgCol
  askPGColInfo fieldInfoMap pgCol relWhenPGErr
  where
    relWhenPGErr = "relationships have to be expanded"

convExtRel
  :: (P1C m)
  => FieldInfoMap
  -> RelName
  -> Maybe RelName
  -> SelectQExt
  -> (PGColType -> Value -> m S.SQLExp)
  -> m AnnRel
convExtRel fieldInfoMap relName mAlias selQ prepValBuilder = do
  -- Point to the name key
  relInfo <- withPathK "name" $
    askRelType fieldInfoMap relName pgWhenRelErr
  let (RelInfo _ relTy colMapping relTab _ _) = relInfo
  (relCIM, relSPI) <- fetchRelDet relName relTab
  when (relTy == ObjRel && misused) $
    throw400 UnexpectedPayload objRelMisuseMsg
  annSel <- convSelectQ relCIM relSPI selQ prepValBuilder
  return $ AnnRel (fromMaybe relName mAlias) relTy colMapping annSel
  where
    pgWhenRelErr = "only relationships can be expanded"
    misused      =
      or [ isJust (sqWhere selQ)
         , isJust (sqLimit selQ)
         , isJust (sqOffset selQ)
         , isJust (sqOrderBy selQ)
         ]
    objRelMisuseMsg =
      mconcat [ "when selecting an 'obj_relationship' "
              , "'where', 'order_by', 'limit' and 'offset' "
              , " can't be used"
              ]

partAnnFlds
  :: [AnnFld] -> ([(PGCol, PGColType)], [AnnRel])
partAnnFlds flds =
  partitionEithers $ catMaybes $ flip map flds $ \case
  FCol c -> Just $ Left (pgiName c, pgiType c)
  FRel r -> Just $ Right r
  FAgg _ -> Nothing
  FExp _ -> Nothing

getSelectDeps
  :: AnnSel
  -> [SchemaDependency]
getSelectDeps (AnnSelG flds tabFrm _ tableArgs) =
  mkParentDep tn
  : fromMaybe [] whereDeps
  <> colDeps
  <> relDeps
  <> nestedDeps
  where
    TableFrom tn _ = tabFrm
    annWc = _taWhere tableArgs
    (sCols, rCols) = partAnnFlds $ map snd flds
    colDeps     = map (mkColDep "untyped" tn . fst) sCols
    relDeps     = map (mkRelDep . arName) rCols
    nestedDeps  = concatMap (getSelectDeps . arAnnSel) rCols
    whereDeps   = getBoolExpDeps tn <$> annWc
    mkRelDep rn =
      SchemaDependency (SOTableObj tn (TORel rn)) "untyped"

convSelectQuery
  :: (P1C m)
  => (PGColType -> Value -> m S.SQLExp)
  -> SelectQuery
  -> m AnnSel
convSelectQuery prepArgBuilder (DMLQuery qt selQ) = do
  tabInfo     <- withPathK "table" $ askTabInfo qt
  selPermInfo <- askSelPermInfo tabInfo
  extSelQ <- resolveStar (tiFieldInfoMap tabInfo) selPermInfo selQ
  validateHeaders $ spiRequiredHeaders selPermInfo
  convSelectQ (tiFieldInfoMap tabInfo) selPermInfo extSelQ prepArgBuilder

mkAggSelect :: AnnAggSel -> S.Select
mkAggSelect annAggSel =
  prefixNumToAliases $ aggNodeToSelect bn extr $ S.BELit True
  where
    aggSel = AggSel [] annAggSel
    AggNode _ extr bn =
      aggSelToAggNode (Iden "root") (FieldName "root") aggSel

selectAggP2 :: (AnnAggSel, DS.Seq Q.PrepArg) -> Q.TxE QErr RespBody
selectAggP2 (sel, p) =
  runIdentity . Q.getRow
  <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder selectSQL) (toList p) True
  where
    selectSQL = toSQL $ mkAggSelect sel

mkSQLSelect :: Bool -> AnnSel -> S.Select
mkSQLSelect isSingleObject annSel =
  prefixNumToAliases $ asJsonAggSel isSingleObject rootFldAls (S.BELit True)
  $ annSelToBaseNode (toIden rootFldName)
  rootFldName annSel
  where
    rootFldName = FieldName "root"
    rootFldAls  = S.Alias $ toIden rootFldName

-- selectP2 :: (P2C m) => (SelectQueryP1, DS.Seq Q.PrepArg) -> m RespBody
selectP2 :: Bool -> (AnnSel, DS.Seq Q.PrepArg) -> Q.TxE QErr RespBody
selectP2 asSingleObject (sel, p) =
  runIdentity . Q.getRow
  <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder selectSQL) (toList p) True
  where
    selectSQL = toSQL $ mkSQLSelect asSingleObject sel

instance HDBQuery SelectQuery where

  -- type Phase1Res SelectQuery = (SelectQueryP1, DS.Seq Q.PrepArg)
  type Phase1Res SelectQuery = (AnnSel, DS.Seq Q.PrepArg)
  phaseOne q = flip runStateT DS.empty $ convSelectQuery binRHSBuilder q

  phaseTwo _ = liftTx . selectP2 False

  schemaCachePolicy = SCPNoChange
