{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Hasura.RQL.DML.Select where

import           Data.Aeson.Types
import           Data.List                  (unionBy)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import qualified Data.Sequence              as DS
import qualified Data.Text                  as T

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.SQL.Rewrite         (prefixNumToAliases)
import           Hasura.SQL.Types

import qualified Database.PG.Query          as Q
import qualified Hasura.SQL.DML             as S

-- Conversion of SelectQ happens in 2 Stages.
-- Stage 1 : Convert input query into an annotated AST
-- Stage 2 : Convert annotated AST to SQL Select

type SelectQExt = SelectG ExtCol BoolExp Int
-- Columns in RQL
data ExtCol
  = ECSimple !PGCol
  | ECRel !RelName !(Maybe RelName) !SelectQExt
  deriving (Show, Eq, Lift)

instance ToJSON ExtCol where
  toJSON (ECSimple s) = toJSON s
  toJSON (ECRel rn mrn selq) =
    object $ [ "name" .= rn
             , "alias" .= mrn
             ] ++ selectGToPairs selq

instance FromJSON ExtCol where
  parseJSON v@(Object o) =
    ECRel
    <$> o .:  "name"
    <*> o .:? "alias"
    <*> parseJSON v
  parseJSON (String s) =
    return $ ECSimple $ PGCol s
  parseJSON _ =
    fail $ mconcat
    [ "A column should either be a string or an "
    , "object (relationship)"
    ]

data AnnRel
  = AnnRel
  { arName    :: !RelName    -- Relationship name
  , arType    :: !RelType    -- Relationship type (ObjRel, ArrRel)
  , arMapping :: ![(PGCol, PGCol)]      -- Column of the left table to join with
  , arAnnSel  :: !AnnSel -- Current table. Almost ~ to SQL Select
  } deriving (Show, Eq)

data AnnFld
  = FCol !PGColInfo
  | FExp !T.Text
  | FRel !AnnRel
  deriving (Show, Eq)

data TableArgs
  = TableArgs
  { _taWhere   :: !(Maybe (GBoolExp AnnSQLBoolExp))
  , _taOrderBy :: !(Maybe [AnnOrderByItem])
  , _taLimit   :: !(Maybe Int)
  , _taOffset  :: !(Maybe S.SQLExp)
  } deriving (Show, Eq)

noTableArgs :: TableArgs
noTableArgs = TableArgs Nothing Nothing Nothing Nothing

data AnnSel
  = AnnSel
  { _asFields :: ![(FieldName, AnnFld)]
  -- this is currently only used for computing
  -- dependencies of query templates
  , _asTable  :: !QualifiedTable
  , _asFrom   :: !(Maybe S.FromItem)
  , _asFilter :: !S.BoolExp
  , _asLimit  :: !(Maybe Int)

  , _asArgs   :: !TableArgs

  } deriving (Show, Eq)

data BaseNode
  = BaseNode
  { _bnPrefix  :: !Iden
  , _bnFrom    :: !S.FromItem
  , _bnWhere   :: !S.BoolExp
  , _bnOrderBy :: !(Maybe S.OrderByExp)
  , _bnLimit   :: !(Maybe Int)
  , _bnOffset  :: !(Maybe S.SQLExp)

  , _bnExtrs   :: !(HM.HashMap S.Alias S.SQLExp)
  , _bnObjRels :: !(HM.HashMap RelName RelNode)
  , _bnArrRels :: !(HM.HashMap S.Alias RelNode)

  } deriving (Show, Eq)

-- array relationships are not grouped, so have to be prefixed by
-- parent's alias
mkUniqArrRelAls :: FieldName -> FieldName -> Iden
mkUniqArrRelAls parAls relAls =
  Iden $
  getFieldNameTxt parAls <> "." <> getFieldNameTxt relAls

mkArrRelTableAls :: Iden -> FieldName -> FieldName -> Iden
mkArrRelTableAls pfx parAls relAls =
  pfx <> Iden ".ar." <> uniqArrRelAls
  where
    uniqArrRelAls = mkUniqArrRelAls parAls relAls

mkObjRelTableAls :: Iden -> RelName -> Iden
mkObjRelTableAls pfx relName =
  pfx <> Iden ".or." <> toIden relName

mkBaseTableAls :: Iden -> Iden
mkBaseTableAls pfx =
  pfx <> Iden ".base"

-- posttgres ignores anything beyond 63 chars for an iden
-- in this case, we'll need to use json_build_object function
-- json_build_object is slower than row_to_json hence it is only
-- used when needed
buildJsonObject
  :: Iden -> FieldName
  -> [(FieldName, AnnFld)] -> (S.Alias, S.SQLExp)
buildJsonObject pfx parAls flds =
  if any ( (> 63) . T.length . getFieldNameTxt . fst ) flds
  then withJsonBuildObj pfx parAls flds
  else withRowToJSON pfx parAls flds

-- uses row_to_json to build a json object
withRowToJSON
  :: Iden -> FieldName
  -> [(FieldName, AnnFld)] -> (S.Alias, S.SQLExp)
withRowToJSON pfx parAls flds =
  (S.toAlias parAls, jsonRow)
  where
    withAls fldName sqlExp =
      S.Extractor sqlExp $ Just $ S.toAlias fldName
    jsonRow = S.SEFnApp "row_to_json" [S.mkRowExp $ map toFldExtr flds] Nothing
    toFldExtr (fldAls, fld) = withAls fldAls $ case fld of
      FCol col    -> toJSONableExp (pgiType col) $
                     S.mkQIdenExp (mkBaseTableAls pfx) $ pgiName col
      FExp e      -> S.SELit e
      FRel annRel ->
        let qual = case arType annRel of
              ObjRel -> mkObjRelTableAls pfx $ arName annRel
              ArrRel -> mkArrRelTableAls pfx parAls fldAls
        in S.mkQIdenExp qual fldAls

-- uses json_build_object to build a json object
withJsonBuildObj
  :: Iden -> FieldName
  -> [(FieldName, AnnFld)] -> (S.Alias, S.SQLExp)
withJsonBuildObj pfx parAls flds =
  (S.toAlias parAls, jsonRow)
  where
    withAls fldName sqlExp =
      [S.SELit $ getFieldNameTxt fldName, sqlExp]

    jsonRow = S.SEFnApp "json_build_object" (concatMap toFldExtr flds) Nothing

    toFldExtr (fldAls, fld) = withAls fldAls $ case fld of
      FCol col    -> toJSONableExp (pgiType col) $
                     S.mkQIdenExp (mkBaseTableAls pfx) $ pgiName col
      FExp e      -> S.SELit e
      FRel annRel ->
        let qual = case arType annRel of
              ObjRel -> mkObjRelTableAls pfx $ arName annRel
              ArrRel -> mkArrRelTableAls pfx parAls fldAls
        in S.mkQIdenExp qual fldAls

processAnnOrderByItem
  :: Iden
  -> AnnOrderByItem
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- the sql order by item that is attached to the final select
     , S.OrderByItem
       -- optionally we may have to add an obj rel node
     , Maybe (RelName, RelNode)
     )
processAnnOrderByItem pfx (OrderByItemG obTyM annObCol obNullsM) =
  ( (obColAls, obColExp)
  , sqlOrdByItem
  , relNodeM
  )
  where
    ((obColAls, obColExp), relNodeM) = processAnnOrderByCol pfx annObCol

    sqlOrdByItem =
      S.OrderByItem (S.SEIden $ toIden obColAls) obTyM obNullsM

processAnnOrderByCol
  :: Iden
  -> AnnObCol
       -- the extractors which will select the needed columns
  -> ( (S.Alias, S.SQLExp)
       -- optionally we may have to add an obj rel node
     , Maybe (RelName, RelNode)
     )
processAnnOrderByCol pfx = \case
  AOCPG colInfo ->
    let
      qualCol  = S.mkQIdenExp (mkBaseTableAls pfx) (toIden $ pgiName colInfo)
      obColAls = pfx <> Iden ".pg." <> toIden (pgiName colInfo)
    in ( (S.Alias obColAls, qualCol)
       , Nothing
       )
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCRel (RelInfo rn _ colMapping relTab _ _) relFltr rest ->
    let relPfx  = mkObjRelTableAls pfx rn
        ((nesAls, nesCol), nesNodeM) = processAnnOrderByCol relPfx rest
        qualCol = S.mkQIdenExp relPfx nesAls
        relBaseNode =
          BaseNode relPfx (S.FISimple relTab Nothing) relFltr
          Nothing Nothing Nothing
          (HM.singleton nesAls nesCol)
          (maybe HM.empty (uncurry HM.singleton) nesNodeM)
          HM.empty
        relNode = RelNode rn (fromRel rn) colMapping relBaseNode
    in ( (nesAls, qualCol)
       , Just (rn, relNode)
       )

mkBaseNode :: Iden -> FieldName -> AnnSel -> BaseNode
mkBaseNode pfx fldAls (AnnSel flds tn fromItemM fltr permLimitM tableArgs) =
  BaseNode pfx fromItem finalWhere ordByExpM finalLimit offsetM
  allExtrs allObjsWithOb allArrs
  where

    TableArgs whereM orderByM limitM offsetM = tableArgs

    finalWhere = maybe fltr (S.BEBin S.AndOp fltr . cBoolExp) whereM
    finalLimit = applyPermLimit permLimitM limitM

    fromItem = fromMaybe (S.FISimple tn Nothing) fromItemM

    -- the selection extractor
    selExtr  = buildJsonObject pfx fldAls flds

    -- all the relationships
    (allObjs, allArrs) =
      foldl' addRel (HM.empty, HM.empty) $
      mapMaybe (\(als, f) -> (als,) <$> getAnnRel f) flds

    _1 (a, _, _) = a
    _2 (_, b, _) = b
    _3 (_, _, c) = c

    procOrdByM = unzip3 . map (processAnnOrderByItem pfx) <$> orderByM
    ordByExpM  = S.OrderByExp . _2 <$> procOrdByM

    -- the columns needed for orderby
    obExtrs  = maybe [] _1 procOrdByM
    allExtrs = HM.fromList $ selExtr:obExtrs
    allObjsWithOb =
      foldl' (\objs (rn, relNode) -> HM.insertWith mergeRelNodes rn relNode objs)
      allObjs $ catMaybes $ maybe [] _3 procOrdByM

    mkRelPfx rTy rn relAls = case rTy of
      ObjRel -> mkObjRelTableAls pfx rn
      ArrRel -> mkArrRelTableAls pfx fldAls relAls

    -- process a relationship
    addRel (objs, arrs) (relAls, annRel) =
      let relName    = arName annRel
          relNodePfx = mkRelPfx (arType annRel) relName relAls
          relNode    = mkRelNode relNodePfx (relAls, annRel)
      in case arType annRel of
        -- in case of object relationships, we merge
        ObjRel ->
          (HM.insertWith mergeRelNodes relName relNode objs, arrs)
        ArrRel ->
          let arrRelTableAls = S.Alias $ mkUniqArrRelAls fldAls relAls
          in (objs, HM.insert arrRelTableAls relNode arrs)

    getAnnRel = \case
      FCol _  -> Nothing
      FExp _  -> Nothing
      FRel ar -> Just ar

mergeBaseNodes :: BaseNode -> BaseNode -> BaseNode
mergeBaseNodes lNodeDet rNodeDet =
  BaseNode pfx f whr ordBy limit offset
  (HM.union lExtrs rExtrs)
  (HM.unionWith mergeRelNodes lObjs rObjs)
  (HM.union lArrs rArrs)
  where
    (BaseNode pfx f whr ordBy limit offset lExtrs lObjs lArrs) = lNodeDet
    (BaseNode _   _ _   _     _     _      rExtrs rObjs rArrs) = rNodeDet

-- should only be used to merge obj rel nodes
mergeRelNodes :: RelNode -> RelNode -> RelNode
mergeRelNodes lNode rNode =
  RelNode rn rAls rMapn $ mergeBaseNodes lNodeDet rNodeDet
  where
    (RelNode rn rAls rMapn lNodeDet) = lNode
    (RelNode _  _    _     rNodeDet) = rNode

data RelNode
  = RelNode
  { _rnRelName    :: !RelName
  , _rnRelAlias   :: !FieldName
  , _rnRelMapping :: ![(PGCol, PGCol)]
  , _rnNodeDet    :: !BaseNode
  } deriving (Show, Eq)

mkRelNode :: Iden -> (FieldName, AnnRel) -> RelNode
mkRelNode pfx (relAls, AnnRel rn _ rMapn rAnnSel) =
  RelNode rn relAls rMapn $ mkBaseNode pfx relAls rAnnSel

withJsonAgg :: Maybe S.OrderByExp -> S.Alias -> S.FromItem -> S.Select
withJsonAgg orderByM col fromItem =
  S.mkSelect
  { S.selExtr = [S.Extractor extr $ Just col]
  , S.selFrom = Just $ S.FromExp [fromItem]
  }
  where
    extr    = S.SEFnApp "coalesce" [jsonAgg, S.SELit "[]"] Nothing
    jsonAgg = S.SEFnApp "json_agg" [S.SEIden $ toIden col] orderByM

baseNodeToSel :: S.BoolExp -> BaseNode -> S.Select
baseNodeToSel joinCond (BaseNode pfx fromItem whr ordByM limitM offsetM extrs objRels arrRels) =
  S.mkSelect
  { S.selExtr    = [S.Extractor e $ Just a | (a, e) <- HM.toList extrs]
  , S.selFrom    = Just $ S.FromExp [joinedFrom]
  , S.selOrderBy = ordByM
  , S.selLimit   = S.LimitExp . S.intToSQLExp <$> limitM
  , S.selOffset  = S.OffsetExp <$> offsetM
  }
  where
    -- this is the table which is aliased as "pfx.base"
    baseSel = S.mkSelect
      { S.selExtr  = [S.Extractor S.SEStar Nothing]
      , S.selFrom  = Just $ S.FromExp [fromItem]
      , S.selWhere = Just $ injectJoinCond joinCond whr
      }
    baseSelAls = S.Alias $ mkBaseTableAls pfx
    baseFromItem = S.FISelect (S.Lateral False) baseSel baseSelAls

    -- function to create a joined from item from two from items
    leftOuterJoin current new =
      S.FIJoin $ S.JoinExpr current S.LeftOuter new $
      S.JoinOn $ S.BELit True

    -- this is the from eexp for the final select
    joinedFrom :: S.FromItem
    joinedFrom = foldl' leftOuterJoin baseFromItem $
                 map objRelToFromItem (HM.elems objRels) <>
                 map arrRelToFromItem (HM.elems arrRels)

    relNodeToSelect :: RelNode -> (S.Select, S.Alias)
    relNodeToSelect (RelNode _ _ relMapn relBaseNode) =
      ( baseNodeToSel (mkJoinCond baseSelAls relMapn) relBaseNode
      , S.Alias $ _bnPrefix relBaseNode
      )

    objRelToFromItem :: RelNode -> S.FromItem
    objRelToFromItem =
      uncurry S.mkLateralFromItem . relNodeToSelect

    arrRelToFromItem :: RelNode -> S.FromItem
    arrRelToFromItem relNode =
      let (sel, als)  = relNodeToSelect relNode
          aggOrderByM = _bnOrderBy $ _rnNodeDet relNode
          jsonAggSel  = withJsonAgg aggOrderByM
                        (S.Alias $ toIden $ _rnRelAlias relNode)
                        (S.mkSelFromItem sel als)
      in S.mkLateralFromItem jsonAggSel als

mkJoinCond :: S.Alias -> [(PGCol, PGCol)] -> S.BoolExp
mkJoinCond baseTableAls colMapn =
  foldl' (S.BEBin S.AndOp) (S.BELit True) $ flip map colMapn $
  \(lCol, rCol) ->
    S.BECompare S.SEQ (S.mkQIdenExp baseTableAls lCol) (S.mkSIdenExp rCol)

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
convWildcard fieldInfoMap (SelPermInfo cols _ _ _ _ _) wildcard =
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

data AnnObCol
  = AOCPG !PGColInfo
  | AOCRel !RelInfo !S.BoolExp !AnnObCol
  deriving (Show, Eq)

type AnnOrderByItem = OrderByItemG AnnObCol

partAnnFlds
  :: [AnnFld] -> ([(PGCol, PGColType)], [AnnRel])
partAnnFlds flds =
  partitionEithers $ catMaybes $ flip map flds $ \case
  FCol c -> Just $ Left (pgiName c, pgiType c)
  FRel r -> Just $ Right r
  FExp _ -> Nothing

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

-- If query limit > permission limit then consider permission limit Else consider query limit
applyPermLimit
  :: Maybe Int -- Permission limit
  -> Maybe Int -- Query limit
  -> Maybe Int -- Return SQL exp
applyPermLimit mPermLimit mQueryLimit =
  maybe mQueryLimit compareWithPermLimit mPermLimit
  where
    compareWithPermLimit pLimit =
      maybe (Just pLimit) (compareLimits pLimit) mQueryLimit
    compareLimits pLimit qLimit = Just $
      if qLimit > pLimit then pLimit else qLimit

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

  annOrdByM <- forM (sqOrderBy selQ) $ \(OrderByExp obItems) ->
    withPathK "order_by" $ indexedForM obItems $ mapM $
    convOrderByElem (fieldInfoMap, selPermInfo)

  -- validate limit and offset values
  withPathK "limit" $ mapM_ onlyPositiveInt mQueryLimit
  withPathK "offset" $ mapM_ onlyPositiveInt mQueryOffset

  return $ AnnSel annFlds (spiTable selPermInfo) Nothing
    (spiFilter selPermInfo) mPermLimit $
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

injectJoinCond :: S.BoolExp       -- ^ Join condition
               -> S.BoolExp -- ^ Where condition
               -> S.WhereFrag     -- ^ New where frag
injectJoinCond joinCond whereCond =
  S.WhereFrag $ S.simplifyBoolExp $ S.BEBin S.AndOp joinCond whereCond

getSelectDeps
  :: AnnSel
  -> [SchemaDependency]
getSelectDeps (AnnSel flds tn _ _ _ tableArgs) =
  mkParentDep tn
  : fromMaybe [] whereDeps
  <> colDeps
  <> relDeps
  <> nestedDeps
  where
    annWc = _taWhere tableArgs
    (sCols, rCols) = partAnnFlds $ map snd flds
    colDeps     = map (mkColDep "untyped" tn . fst) sCols
    relDeps     = map (mkRelDep . arName) rCols
    nestedDeps  = concatMap (getSelectDeps . arAnnSel) rCols
    whereDeps   = getBoolExpDeps tn <$> annWc
    mkRelDep rn =
      SchemaDependency (SOTableObj tn (TORel rn)) "untyped"

mkSQLSelect :: Bool -> AnnSel -> S.Select
mkSQLSelect isSingleObject annSel =
  prefixNumToAliases $
  if isSingleObject
  then asSingleRow rootFldAls rootSelAsSubQuery
  else withJsonAgg Nothing rootFldAls rootSelAsSubQuery
  where
    rootSelAsSubQuery = S.mkSelFromItem rootSel $ S.Alias $ Iden "root_alias"
    rootSel  = baseNodeToSel (S.BELit True) rootNode
    rootFldName = FieldName "root"
    rootFldAls  = S.Alias $ toIden rootFldName
    rootNode = mkBaseNode (Iden "root") rootFldName annSel

asSingleRow :: S.Alias -> S.FromItem -> S.Select
asSingleRow col fromItem =
  S.mkSelect
  { S.selExtr  = [S.Extractor extr $ Just col]
  , S.selFrom  = Just $ S.FromExp [fromItem]
  }
  where
    extr    = S.SEFnApp "coalesce" [jsonAgg, S.SELit "null"] Nothing
    jsonAgg = S.SEOpApp (S.SQLOp "->")
              [ S.SEFnApp "json_agg" [S.SEIden $ toIden col] Nothing
              , S.SEUnsafe "0"
              ]

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
