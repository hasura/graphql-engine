{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Hasura.SQL.Types

import qualified Database.PG.Query          as Q
import qualified Hasura.SQL.DML             as S

-- Conversion of SelectQ happens in 2 Stages.
-- Stage 1 : Convert input query into an annotated AST
-- Stage 2 : Convert annotated AST to SQL Select

type SelectQExt = SelectG ExtCol BoolExp
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

data AnnRel = AnnRel
    { arName    :: !RelName    -- Relationship name
    , arType    :: !RelType    -- Relationship type (ObjRel, ArrRel)
    , arMapping :: ![(PGCol, PGCol)]      -- Column of the left table to join with
    , arSelData :: !SelectData -- Current table. Almost ~ to SQL Select
    } deriving (Show, Eq)

data SelectData = SelectData
      -- Nested annotated columns
    { sdFlds    :: !(HM.HashMap FieldName AnnFld)
    , sdTable   :: !QualifiedTable     -- Postgres table name
    , sdWhere   :: !(S.BoolExp, Maybe (GBoolExp AnnSQLBoolExp))
    , sdOrderBy :: !(Maybe S.OrderByExp)
    , sdAddCols :: ![PGCol]             -- additional order by columns
    , sdLimit   :: !(Maybe S.SQLExp)
    , sdOffset  :: !(Maybe S.SQLExp)
    } deriving (Show, Eq)

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
  let (RelInfo _ _ _ relTab _) = relInfo
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
convWildcard fieldInfoMap (SelPermInfo cols _ _ _ _) wildcard =
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

      case mRelSelPerm of
        Nothing -> return Nothing
        Just rspi -> do
          rExtCols <- convWildcard (tiFieldInfoMap relTabInfo) rspi wc
          return $ Just $ ECRel relName Nothing $
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

data AnnFld
  = FCol (PGCol, PGColType)
  | FRel AnnRel
  | FExp T.Text
  deriving (Show, Eq)

partAnnFlds
  :: [AnnFld] -> ([(PGCol, PGColType)], [AnnRel])
partAnnFlds flds =
  partitionEithers $ catMaybes $ flip map flds $ \case
  FCol c -> Just $ Left c
  FRel r -> Just $ Right r
  FExp _ -> Nothing


processOrderByElem
  :: (P1C m)
  => HM.HashMap FieldName AnnFld
  -> [T.Text]
  -> m (HM.HashMap FieldName AnnFld)
processOrderByElem _ [] =
  withPathK "column" $ throw400 UnexpectedPayload "can't be empty"
processOrderByElem annFlds [colTxt] =
  case HM.lookup (FieldName colTxt) annFlds of
    Just (FCol (_, ty)) -> if ty == PGGeography || ty == PGGeometry
      then throw400 UnexpectedPayload $ mconcat
           [ (PGCol colTxt) <<> " has type 'geometry'"
           , " and cannot be used in order_by"
           ]
      else return annFlds
    Just (FRel _) -> throw400 UnexpectedPayload $ mconcat
        [ (PGCol colTxt) <<> " is a"
        , " relationship and should be expanded"
        ]
    Just (FExp t) -> throw500 $
        " found __typename in order_by?: " <> t
    Nothing -> throw400 UnexpectedPayload $ mconcat
        [ (PGCol colTxt) <<> " should be"
        , " included in 'columns'"
        ]
processOrderByElem annFlds (colTxt:xs) =
  case HM.lookup (FieldName colTxt) annFlds of
    Just (FRel annRel) -> case arType annRel of
      ObjRel -> do
        let relSelData = arSelData annRel
            relFlds    = sdFlds relSelData
        newRelFlds <- processOrderByElem relFlds xs
        let newRelSelData = relSelData
              { sdAddCols = (PGCol $ T.intercalate "__" xs):(sdAddCols relSelData)
              , sdFlds    = newRelFlds
              }
            newAnnRel = annRel { arSelData = newRelSelData }
        return $ HM.insert (FieldName colTxt) (FRel newAnnRel) annFlds
      ArrRel ->
        throw400 UnexpectedPayload $ mconcat
        [ (RelName colTxt) <<> " is an array relationship"
        ," and can't be used in 'order_by'"
        ]
    Just (FCol _) -> throw400 UnexpectedPayload $ mconcat
        [ (PGCol colTxt) <<> " is a Postgres column"
        , " and cannot be chained further"
        ]
    Just (FExp t) -> throw500 $
        " found __typename in order_by?: " <> t
    Nothing -> throw400 UnexpectedPayload $ mconcat
        [ (PGCol colTxt) <<> " should be"
        , " included in 'columns'"
        ]

convOrderByItem :: OrderByItem -> S.OrderByItem
convOrderByItem (OrderByItem ot (OrderByCol path) nulls) =
  S.OrderByItem obiExp ot nulls
  where
    obiExp = Left $ PGCol $ T.intercalate "__" path

convOrderByExp
  :: (P1C m)
  => OrderByExp
  -> m S.OrderByExp
convOrderByExp (OrderByExp obItems) = do
  when (null obItems) $ throw400 UnexpectedPayload
    "order_by array should not be empty"
  return $
    S.OrderByExp $ map convOrderByItem obItems

partitionExtCols :: [ExtCol]
                 -> ([PGCol], [(RelName, Maybe RelName, SelectQExt)])
partitionExtCols = foldr f ([], [])
 where
   f (ECSimple pgCol)     ~(l, r)        = (pgCol:l, r)
   f (ECRel relName mAlias selQ) ~(l, r) = (l, (relName, mAlias, selQ):r)

convSelectQ
  :: (P1C m)
  => FieldInfoMap  -- Table information of current table
  -> SelPermInfo   -- Additional select permission info
  -> SelectQExt     -- Given Select Query
  -> (PGColType -> Value -> m S.SQLExp)
  -> m SelectData
convSelectQ fieldInfoMap selPermInfo selQ prepValBuilder = do
  -- let (extPGCols, extRels) = partitionExtCols $ sqColumns selQ

  annFlds <- fmap HM.fromList $ withPathK "columns" $
    indexedForM (sqColumns selQ) $ \case
    (ECSimple pgCol) -> do
      colTy <- convExtSimple fieldInfoMap selPermInfo pgCol
      return (fromPGCol pgCol, FCol (pgCol, colTy))
    (ECRel relName mAlias relSelQ) -> do
      annRel <- convExtRel fieldInfoMap relName mAlias relSelQ prepValBuilder
      return (fromRel $ fromMaybe relName mAlias, FRel annRel)

  -- pgColTypes <- withPathK "columns" $
  --   indexedForM extPGCols $ \extCol ->
  --   convExtSimple fieldInfoMap selPermInfo extCol

  -- let pgColMap = HM.fromList $ zip extPGCols pgColTypes

  -- annRels <- withPathK "columns" $
  --   indexedForM extRels $ \(relName, mAlias, extCol) -> do

  -- let annRelMap = HM.fromList annRels
  let spiT = spiTable selPermInfo

  -- Convert where clause
  wClause <- forM (sqWhere selQ) $ \be ->
    withPathK "where" $
    convBoolExp' fieldInfoMap spiT selPermInfo be prepValBuilder

  newAnnFldsM <- forM (sqOrderBy selQ) $ \(OrderByExp obItems) ->
    withPathK "order_by" $
    indexedFoldM processOrderByElem annFlds $
    map (getOrderByColPath . obiColumn) obItems

  let newAnnFlds = fromMaybe annFlds newAnnFldsM

  -- Convert order by
  sqlOrderBy <- mapM convOrderByExp $ sqOrderBy selQ

  -- convert limit expression
  limitExp <- mapM (prepValBuilder PGBigInt) $ sqLimit selQ

  -- convert offest value
  offsetExp <- mapM (prepValBuilder PGBigInt) $ sqOffset selQ

  return $ SelectData newAnnFlds (spiTable selPermInfo)
    (spiFilter selPermInfo, wClause) sqlOrderBy [] limitExp offsetExp

convExtSimple
  :: (P1C m)
  => FieldInfoMap
  -> SelPermInfo
  -> PGCol
  -> m PGColType
convExtSimple fieldInfoMap selPermInfo pgCol = do
  checkSelOnCol selPermInfo pgCol
  askPGType fieldInfoMap pgCol relWhenPGErr
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
  let (RelInfo _ relTy colMapping relTab _) = relInfo
  (relCIM, relSPI) <- fetchRelDet relName relTab
  selectData <- case relTy of
    ObjRel ->
      if misused
        then throw400 UnexpectedPayload $ mconcat
             [ "when selecting an 'obj_relationship' "
             , "'where', 'order_by', 'limit' and 'offset' "
             , " can't be used"
             ]
        else convSelectQ relCIM relSPI selQ prepValBuilder
    ArrRel -> convSelectQ relCIM relSPI selQ prepValBuilder
  return $ AnnRel (fromMaybe relName mAlias) relTy colMapping selectData
  where
    pgWhenRelErr = "only relationships can be expanded"
    misused      = or [ isJust (sqWhere selQ)
                      , isJust (sqLimit selQ)
                      , isJust (sqOffset selQ)
                      , isJust (sqOrderBy selQ)
                      ]

-- SQL Generation helper functions
----------------------------------

-- | Lateral joins are different. For example
-- A typical join looks like :
-- FromExp1 JOIN FromExp2 ON (condition)
--
-- A lateral join is as follows :
-- FromExp1 LATERAL JOIN FromExp2' ON (true)
-- where condition exists inside FromExp2'

joinSel :: S.Select  -- ^ left Select expression
        -> S.Select  -- ^ right Select expression
        -> S.FromExp -- ^ From expression
joinSel leftSel rightSel =
  S.FromExp [S.FIJoin $ S.JoinExpr lhsFI S.LeftOuter rhsFI joinCond]
  where
    lhsFI = S.mkSelFromExp False leftSel $ TableName "l"
    rhsFI = S.mkSelFromExp True rightSel $ TableName "r"
    joinCond = S.JoinOn $ S.BELit True

-- | Injects lateral join condition into given Select expression

injectJoinCond :: S.BoolExp       -- ^ Join condition
               -> S.BoolExp -- ^ Where condition
               -> S.WhereFrag     -- ^ New where frag
injectJoinCond joinCond whereCond =
  S.WhereFrag $ S.BEBin S.AndOp joinCond whereCond

mkJoinCond :: AnnRel -> S.BoolExp
mkJoinCond annRel =
  foldr (S.BEBin S.AndOp) (S.BELit True) $ flip map colMapping $
  \(lCol, rCol) -> S.BECompare S.SEQ (mkLJColFn lCol) (S.mkSIdenExp rCol)
  where
    colMapping = arMapping annRel
    mkLJColFn  = S.mkQIdenExp (TableName "l") . mkLJCol (arName annRel)

-- | Generates SQL Exp of form
--
--   fn_name((SELECT r FROM (SELECT ext1, ext2 ..) as r))
--           |              |--------------------------|
--           |              |      inner select        |
--           |-----------------------------------------|
--           |              outer select               |
--
--   This is needed because
--
--     row_to_json(col1, col2)
--
--   would result in
--
--     { "f1" : v1, "f2" : v2 }
--
--   But,
--
--     row_to_json((SELECT r FROM (SELECT col1, col2) as r))
--
--   would result in
--
--     { "col1" : v1, "col2" : v2 }

mkInnerSelExtr :: (FieldName, AnnFld) -> S.Extractor
mkInnerSelExtr (alias, annFld) =
  S.mkAliasedExtrFromExp colExp $
  Just alias
  where
    colExp = case annFld of
      FCol (pgCol, _) -> S.mkQIdenExp (TableName "r")  pgCol
      FRel annRel     -> S.mkQIdenExp (TableName "r") $ arName annRel
      FExp t          -> S.SELit t

mkLJCol :: RelName -> PGCol -> PGCol
mkLJCol (RelName rTxt) (PGCol cTxt) =
  PGCol ("__l_" <> rTxt <> "_" <> cTxt)

-- | Generates
--
--   IF (r.__r_col IS NULL) THEN 'null' ELSE row_to_json(..)
mkObjRelExtr :: PGCol -> RelName -> [S.Extractor] -> S.Extractor
mkObjRelExtr compCol relName flds =
  let idCol   = S.mkQIdenExp (TableName "r") compCol
      rowExp  = S.mkRowExp flds
      objAgg  = S.SEFnApp "row_to_json" [rowExp] Nothing
      condExp = S.SECond (S.BENull idCol) (S.SELit "null") objAgg
  in S.mkAliasedExtrFromExp condExp $ Just relName

-- | Generates
--
--   IF (first(r.__r_col) IS NULL) THEN '[]' ELSE json_agg(..)
mkArrRelExtr :: (Maybe S.OrderByExp) -> PGCol -> RelName -> [S.Extractor] -> S.Extractor
mkArrRelExtr mOb compCol relName flds =
  let refCol  = S.SEFnApp "hdb_catalog.first"
                [ S.mkQIdenExp (TableName "r") compCol ] Nothing
      rowExp  = S.mkRowExp flds
      arrAgg  = S.SEFnApp "json_agg" [rowExp] mOb
      condExp = S.SECond (S.BENull refCol) (S.SELit "[]") arrAgg
  in S.mkAliasedExtrFromExp condExp $ Just relName

-- | Make order by extr
mkOrderByColExtr :: RelName -> PGCol -> S.Extractor
mkOrderByColExtr (RelName rTxt) t@(PGCol cTxt) =
  S.mkAliasedExtrFromExp orderByCol $ Just alias
  where
    orderByCol = S.mkQIdenExp (TableName "r") t
    alias = PGCol ( rTxt <> "__" <> cTxt)

-- |
mkLColExtrs :: AnnRel -> [S.Extractor]
mkLColExtrs ar =
  map (\lCol -> S.mkAliasedExtr lCol $ Just $ mkLJCol relName lCol) lCols
  where
    lCols   = map fst $ arMapping ar
    relName = arName ar

-- |
mkCompColAlias :: RelName -> PGCol -> PGCol
mkCompColAlias relName rCol =
    PGCol ("__r_" <> getRelTxt relName <> "_" <> getPGColTxt rCol)
    -- TODO : exception prone, mapping should be nonempty list

selDataToSQL :: [S.Extractor] -- ^ Parent's RCol
             -> S.BoolExp     -- ^ Join Condition if any
             -> SelectData    -- ^ Select data
             -> S.Select      -- ^ SQL Select (needs wrapping)
selDataToSQL parRCols joinCond (SelectData annFlds tn (fltr, mWc) ob _ lt offst) =
  let
    (sCols, relCols) = partAnnFlds $ HM.elems annFlds
    -- relCols        = HM.elems relColsMap
    childrenLCols  = concatMap mkLColExtrs relCols
    thisTableExtrs = parRCols
                       <> map mkColExtr sCols
                       -- <> (map mkOrderByColExtr obeCols)
                       <> childrenLCols

    finalWC = S.BEBin S.AndOp fltr $ maybe (S.BELit True) cBoolExp mWc

    -- Add order by if
    -- limit or offset is used or when no relationships are requested
    -- orderByExp = bool Nothing ob $ or [isJust lt, isJust offst, null relCols]
    baseSel = S.mkSelect
              { S.selExtr    = thisTableExtrs
              , S.selFrom    = Just $ S.mkSimpleFromExp tn
              , S.selWhere   = Just $ injectJoinCond joinCond finalWC
              }
    joinedSel = foldr ($) baseSel $ map annRelColToSQL relCols
  in
    joinedSel { S.selOrderBy = ob
              , S.selLimit   = S.LimitExp  <$> lt
              , S.selOffset  = S.OffsetExp <$> offst
              }

-- | Brings the left select columns into the scope of outer select
--   If group by, then use first, else just qualify with l
exposeLSelExtrs :: Bool          -- is group by on outer select?
                -> [S.Extractor] -- left select's extractors
                -> [S.Extractor] -- extrs that can be used in outer select
exposeLSelExtrs isGrpBy lExtrs =
  -- TODO : This looks error prone. We'll definitely have
  -- alised columns as extractors, but type system doesn't
  -- guarantee it. Fix this.
  map exposeLCol $ mapMaybe S.getExtrAlias lExtrs
  where
    toQual = S.QualIden . toIden
    exposeLCol al@(S.Alias lCol) =
      let qLCol  = S.SEQIden $ S.QIden (toQual (TableName "l")) lCol
          faLCol = S.SEFnApp "hdb_catalog.first" [qLCol] Nothing
      in S.Extractor (bool qLCol faLCol isGrpBy) $ Just al

-- | Generates
--
--    SELECT
--      cols_of_left_sel,
--      relationship_extr
--    FROM
--      left_sel as l
--      {JOIN TYPE} generated_right_sel_from_sel_data as r
--        ON {JOIN COND}
--        {GROUP BY}?
annRelColToSQL :: AnnRel
               -> S.Select
               -> S.Select
annRelColToSQL ar leftSel =
  let
    selData  = arSelData ar
    relName  = arName ar
    joinCond = mkJoinCond ar
    -- The column used to determine whether there the object is null
    -- or array is empty
    compCol  = snd $ head $ arMapping ar
    -- An alias for this
    compColAlias = mkCompColAlias relName compCol
    -- the comparison column should also be selected
    rightSel = selDataToSQL [S.mkAliasedExtr compCol $ Just compColAlias] joinCond selData

    allFlds  = map mkInnerSelExtr (HM.toList $ sdFlds selData)
               -- <> map mkInnerSelExtr (HM.keys $ sdRels selData)
    -- Lateral joins left and right select
    fromExp   = joinSel leftSel rightSel
  in case arType ar of
  ObjRel ->
    let
      -- Current relationship's extractor, using row_to_json
      relExtr   = mkObjRelExtr compColAlias relName allFlds

      -- Qualified left select's columns
      qLSelCols = exposeLSelExtrs False $ S.selExtr leftSel

      -- Relationship's order columns
      relOrderByCols = map (mkOrderByColExtr relName) $ sdAddCols selData

    in
      S.mkSelect { S.selExtr = qLSelCols ++ relExtr:relOrderByCols
                 , S.selFrom = Just fromExp
                 }
  ArrRel ->
    let
      -- Current relationship's extractor, using json_agg
      -- Also add order by in the aggregation as postgres doesn't guarantee it
      relExtr   = mkArrRelExtr (qualifyOrderBy <$> sdOrderBy selData) compColAlias relName allFlds

      -- Firstified left select's columns
      qLSelCols = exposeLSelExtrs True $ S.selExtr leftSel


      -- Group by exp to aggregate relationship as json_array
      grpByExp  = S.GroupByExp $ map (S.mkQIdenExp (TableName "l") . mkLJCol relName) $
                  map fst $ arMapping ar
    in
      S.mkSelect { S.selExtr    = relExtr:qLSelCols
                 , S.selFrom    = Just fromExp
                 , S.selGroupBy = Just grpByExp
                 }
  where
    qualifyOrderByItem (S.OrderByItem e t n) =
      let qe = case e of
            Left c  -> Right $ S.mkQIden (TableName "r") c
            Right c -> Right $ c
      in S.OrderByItem qe t n
    qualifyOrderBy (S.OrderByExp items) =
      S.OrderByExp $ map qualifyOrderByItem items

-- wrapFinalSel :: S.Select -> [ExtCol] -> S.Select
-- wrapFinalSel initSel extCols =
--   S.mkSelect
--   { S.selExtr = [S.Extractor rowToJSONedCol Nothing]
--   , S.selFrom = Just $ S.FromExp [S.mkSelFromExp False initSel (TableName "r")]
--   }
--   where
--     rowExp = S.mkRowExp $ map toExtr extCols
--     rowToJSONedCol = S.SEFnApp "coalesce"
--       [ S.SEFnApp "json_agg" [rowExp] Nothing
--       , S.SELit "[]"] Nothing
--     toExtr (ECSimple pgCol)  =
--       S.mkAliasedExtrFromExp (S.mkQIdenExp (TableName "r") pgCol) $
--       Just pgCol
--     toExtr (ECRel relName mAlias _) =
--       let rName = fromMaybe relName mAlias
--       in S.mkAliasedExtrFromExp (S.mkQIdenExp (TableName "r") rName) $
--          Just rName

wrapFinalSel :: S.Select -> [(FieldName, AnnFld)] -> S.Select
wrapFinalSel initSel extCols =
  S.mkSelect
  { S.selExtr = [S.Extractor rowToJSONedCol Nothing]
  , S.selFrom = Just $ S.FromExp [S.mkSelFromExp False initSel (TableName "r")]
  }
  where
    rowExp = S.mkRowExp $ map mkInnerSelExtr extCols
    rowToJSONedCol = S.SEFnApp "coalesce"
      [ S.SEFnApp "json_agg" [rowExp] Nothing
      , S.SELit "[]"] Nothing

getSelectDeps
  :: SelectData
  -> [SchemaDependency]
getSelectDeps (SelectData flds tn (_, annWc) _ _ _ _) =
  mkParentDep tn
  : fromMaybe [] whereDeps
  <> colDeps
  <> relDeps
  <> nestedDeps
  where
    (sCols, rCols) = partAnnFlds $ HM.elems flds
    colDeps     = map (mkColDep "untyped" tn . fst) sCols
    relDeps     = map (mkRelDep . arName) rCols
    nestedDeps  = concatMap (getSelectDeps . arSelData) rCols
    whereDeps   = getBoolExpDeps tn <$> annWc
    mkRelDep rn =
      SchemaDependency (SOTableObj tn (TORel rn)) "untyped"

-- data SelectQueryP1
--   = SelectQueryP1
--   { sqp1Cols :: ![ExtCol]
--   , sqp1Data :: !SelectData
--   } deriving (Show, Eq)

-- mkSQLSelect :: SelectQueryP1 -> S.Select
-- mkSQLSelect (SelectQueryP1 extCols selData) =
--   wrapFinalSel (selDataToSQL [] (S.BELit True) selData) extCols

mkSQLSelect :: SelectData -> S.Select
mkSQLSelect selData =
  wrapFinalSel (selDataToSQL [] (S.BELit True) selData) $
  HM.toList $ sdFlds selData

-- convSelectQuery
--   :: (P1C m)
--   => (PGColType -> Value -> m S.SQLExp)
--   -> SelectQuery
--   -> m SelectQueryP1
-- convSelectQuery prepArgBuilder (DMLQuery qt selQ) = do
--   tabInfo     <- withPathK "table" $ askTabInfo qt
--   selPermInfo <- askSelPermInfo tabInfo
--   extSelQ <- resolveStar (tiFieldInfoMap tabInfo) selPermInfo selQ
--   let extCols = sqColumns extSelQ
--   selData <- convSelectQ (tiFieldInfoMap tabInfo) selPermInfo extSelQ prepArgBuilder
--   return $ SelectQueryP1 extCols selData

convSelectQuery
  :: (P1C m)
  => (PGColType -> Value -> m S.SQLExp)
  -> SelectQuery
  -> m SelectData
convSelectQuery prepArgBuilder (DMLQuery qt selQ) = do
  tabInfo     <- withPathK "table" $ askTabInfo qt
  selPermInfo <- askSelPermInfo tabInfo
  extSelQ <- resolveStar (tiFieldInfoMap tabInfo) selPermInfo selQ
  validateHeaders $ spiRequiredHeaders selPermInfo
  convSelectQ (tiFieldInfoMap tabInfo) selPermInfo extSelQ prepArgBuilder

-- selectP2 :: (P2C m) => (SelectQueryP1, DS.Seq Q.PrepArg) -> m RespBody
selectP2 :: (SelectData, DS.Seq Q.PrepArg) -> Q.TxE QErr RespBody
selectP2 (sel, p) =
  runIdentity . Q.getRow
  <$> Q.rawQE dmlTxErrorHandler (Q.fromBuilder selectSQL) (toList p) True
  where
    selectSQL = toSQL $ mkSQLSelect sel

instance HDBQuery SelectQuery where

  -- type Phase1Res SelectQuery = (SelectQueryP1, DS.Seq Q.PrepArg)
  type Phase1Res SelectQuery = (SelectData, DS.Seq Q.PrepArg)
  phaseOne q = flip runStateT DS.empty $ convSelectQuery binRHSBuilder q

  phaseTwo _ = liftTx . selectP2

  schemaCachePolicy = SCPNoChange
