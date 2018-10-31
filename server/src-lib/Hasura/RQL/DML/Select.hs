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
import qualified Data.List.NonEmpty         as NE
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
  , _taOrderBy :: !(Maybe (NE.NonEmpty AnnOrderByItem))
  , _taLimit   :: !(Maybe Int)
  , _taOffset  :: !(Maybe S.SQLExp)
  } deriving (Show, Eq)

noTableArgs :: TableArgs
noTableArgs = TableArgs Nothing Nothing Nothing Nothing

data PGColFld
  = PCFCol !PGCol
  | PCFExp !T.Text
  deriving (Show, Eq)

type ColFlds = [(T.Text, PGColFld)]

data AggFld
  = AFCount
  | AFSum !ColFlds
  | AFAvg !ColFlds
  | AFMax !ColFlds
  | AFMin !ColFlds
  | AFExp !T.Text
  deriving (Show, Eq)

type AggFlds = [(T.Text, AggFld)]

data TableAggFld
  = TAFAgg !AggFlds
  | TAFNodes ![(FieldName, AnnFld)]
  | TAFExp !T.Text
  deriving (Show, Eq)

data AnnSelFields
  = ASFSimple ![(FieldName, AnnFld)]
  | ASFWithAgg ![(T.Text, TableAggFld)]
  deriving (Show, Eq)

fetchAnnFlds :: AnnSelFields -> [(FieldName, AnnFld)]
fetchAnnFlds (ASFSimple flds) = flds
fetchAnnFlds (ASFWithAgg aggFlds) =
  concatMap (fromAggFld . snd) aggFlds
  where
    fromAggFld (TAFNodes f) = f
    fromAggFld _            = []

data TableFrom
  = TableFrom
  { _tfTable :: !QualifiedTable
  , _tfFrom  :: !(Maybe S.FromItem)
  } deriving (Show, Eq)

data TablePerm
  = TablePerm
  { _tpFilter :: !S.BoolExp
  , _tpLimit  :: !(Maybe Int)
  } deriving (Show, Eq)

data AnnSel
  = AnnSel
  { _asnFields :: !AnnSelFields
  , _asnFrom   :: !TableFrom
  , _asnPerm   :: !TablePerm
  , _asnArgs   :: !TableArgs
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

data AggBaseNode
  = AggBaseNode
  { _abnFields :: ![(T.Text, TableAggFld)]
  , _abnNode   :: !BaseNode
  } deriving (Show, Eq)

data AnnNode
  = ANSimple !BaseNode
  | ANWithAgg !AggBaseNode
  deriving (Show, Eq)

data AnnNodeToSelOpts
  = AnnNodeToSelOpts
    { _antsSingleObj :: !Bool
    , _antsIsObjRel  :: !Bool
    } deriving (Show, Eq)

objRelOpts :: AnnNodeToSelOpts
objRelOpts = AnnNodeToSelOpts False True

arrRelOpts :: AnnNodeToSelOpts
arrRelOpts = AnnNodeToSelOpts False False

txtToAlias :: Text -> S.Alias
txtToAlias = S.Alias . Iden

aggFldToExp :: Iden -> AggFlds -> S.SQLExp
aggFldToExp pfx aggFlds = jsonRow
  where
    jsonRow = S.applyJsonBuildObj (concatMap aggToFlds aggFlds)
    withAls fldName sqlExp = [S.SELit fldName, sqlExp]
    aggToFlds (t, fld) = withAls t $ case fld of
      AFCount       -> S.SEUnsafe "count(*)"
      AFSum sumFlds -> colFldsToObj "sum" sumFlds
      AFAvg avgFlds -> colFldsToObj "avg" avgFlds
      AFMax maxFlds -> colFldsToObj "max" maxFlds
      AFMin minFlds -> colFldsToObj "min" minFlds
      AFExp e       -> S.SELit e

    colFldsToObj op flds =
      S.applyJsonBuildObj $ concatMap (colFldsToExtr op) flds

    colFldsToExtr op (t, PCFCol col) =
      [ S.SELit t
      , S.SEFnApp op [S.SEIden $ mkBaseTableColAls pfx col] Nothing
      ]
    colFldsToExtr _ (t, PCFExp e) =
      [ S.SELit t , S.SELit e]

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

annNodeToSel
  :: AnnNodeToSelOpts -> S.BoolExp -> S.Alias -> AnnNode -> S.Select
annNodeToSel opts joinCond als = \case
  ANSimple bn -> bool (asJsonAggSel bn) (bnToSel bn) isObjRel
  ANWithAgg (AggBaseNode flds bn) ->
    let pfx = _bnPrefix bn
        baseSelFrom = S.mkSelFromItem (bnToSel bn) (mkAliasFromBN bn)
        ordBy = _bnOrderBy bn
    in S.mkSelect
       { S.selExtr = [ flip S.Extractor (Just als) $
                       S.applyJsonBuildObj $
                       concatMap (selFldToExtr pfx ordBy) flds
                     ]
       , S.selFrom = Just $ S.FromExp [baseSelFrom]
       }
  where
    AnnNodeToSelOpts singleObj isObjRel = opts
    bnToSel = baseNodeToSel joinCond
    mkAliasFromBN = S.Alias . _bnPrefix
    asJsonAggSel n =
      let ordByM = _bnOrderBy n
          fromItem = S.mkSelFromItem (bnToSel n) $
                     mkAliasFromBN n
      in bool
         (withJsonAgg ordByM als fromItem)
         (asSingleRow als fromItem)
         singleObj

    selFldToExtr pfx ordBy (t, fld) = (:) (S.SELit t) $ pure $ case fld of
      TAFAgg aggFlds ->
        aggFldToExp pfx aggFlds
      TAFNodes _ ->
        let jsonAgg = S.SEFnApp "json_agg" [S.SEIden $ Iden t] ordBy
        in S.SEFnApp "coalesce" [jsonAgg, S.SELit "[]"] Nothing
      TAFExp e ->
        -- bool_or to force aggregation
        S.SEFnApp "coalesce"
        [ S.SELit e , S.SEUnsafe "bool_or('true')::text"] Nothing

getAliasFromAnnNode :: AnnNode -> S.Alias
getAliasFromAnnNode = \case
  ANSimple bn -> S.Alias $ _bnPrefix bn
  ANWithAgg abn -> S.Alias $ _bnPrefix $ _abnNode abn

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

mkBaseTableColAls :: Iden -> PGCol -> Iden
mkBaseTableColAls pfx pgCol =
  pfx <> Iden ".pg." <> toIden pgCol

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
    jsonRow = S.applyRowToJson (map toFldExtr flds)
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

    jsonRow = S.applyJsonBuildObj (concatMap toFldExtr flds)

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
      obColAls = mkBaseTableColAls pfx $ pgiName colInfo
    in ( (S.Alias obColAls, qualCol)
       , Nothing
       )
  -- "pfx.or.relname"."pfx.ob.or.relname.rest" AS "pfx.ob.or.relname.rest"
  AOCRel (RelInfo rn _ colMapping relTab _ _) relFltr rest ->
    let relPfx  = mkObjRelTableAls pfx rn
        ((nesAls, nesCol), nesNodeM) = processAnnOrderByCol relPfx rest
        qualCol = S.mkQIdenExp relPfx nesAls
        relBaseNode = ANSimple $
          BaseNode relPfx (S.FISimple relTab Nothing) relFltr
          Nothing Nothing Nothing
          (HM.singleton nesAls nesCol)
          (maybe HM.empty (uncurry HM.singleton) nesNodeM)
          HM.empty
        relNode = RelNode rn (fromRel rn) colMapping relBaseNode
    in ( (nesAls, qualCol)
       , Just (rn, relNode)
       )

mkEmptyBaseNode :: Iden -> TableFrom -> BaseNode
mkEmptyBaseNode pfx tableFrom =
  BaseNode pfx fromItem (S.BELit True) Nothing Nothing Nothing
  selOne HM.empty HM.empty
  where
    selOne = HM.singleton (S.Alias $ pfx <> Iden "__one") (S.SEUnsafe "1")
    TableFrom tn fromItemM = tableFrom
    fromItem = fromMaybe (S.FISimple tn Nothing) fromItemM

mkBaseNode
  :: Iden
  -> FieldName
  -> TableAggFld
  -> TableFrom
  -> TablePerm
  -> TableArgs
  -> BaseNode
mkBaseNode pfx fldAls annSelFlds tableFrom tablePerm tableArgs =
  BaseNode pfx fromItem finalWhere ordByExpM finalLimit offsetM
  allExtrs allObjsWithOb allArrs
  where
    TableFrom tn fromItemM = tableFrom
    TablePerm fltr permLimitM = tablePerm
    TableArgs whereM orderByM limitM offsetM = tableArgs
    (allExtrs, allObjsWithOb, allArrs) = case annSelFlds of
      TAFNodes flds ->
        let selExtr = buildJsonObject pfx fldAls flds
            -- all the relationships
            (allObjs, allArrRels) =
              foldl' addRel (HM.empty, HM.empty) $
              mapMaybe (\(als, f) -> (als,) <$> getAnnRel f) flds
            allObjRelsWithOb =
              foldl' (\objs (rn, relNode) -> HM.insertWith mergeRelNodes rn relNode objs)
              allObjs $ catMaybes $ maybe [] _3 procOrdByM
        in ( HM.fromList $ selExtr:obExtrs
           , allObjRelsWithOb
           , allArrRels
           )
      TAFAgg aggFlds ->
        let extrs = concatMap (fetchExtrFromAggFld . snd) aggFlds
        in ( HM.fromList $ extrs <> obExtrs
           , HM.empty
           , HM.empty
           )
      TAFExp _ -> (HM.fromList obExtrs, HM.empty, HM.empty)

    fetchExtrFromAggFld AFCount         = []
    fetchExtrFromAggFld (AFSum sumFlds) = colFldsToExps sumFlds
    fetchExtrFromAggFld (AFAvg avgFlds) = colFldsToExps avgFlds
    fetchExtrFromAggFld (AFMax maxFlds) = colFldsToExps maxFlds
    fetchExtrFromAggFld (AFMin minFlds) = colFldsToExps minFlds
    fetchExtrFromAggFld (AFExp _)       = []

    colFldsToExps = mapMaybe (mkColExp . snd)

    mkColExp (PCFCol c) =
      let qualCol = S.mkQIdenExp (mkBaseTableAls pfx) (toIden c)
          colAls = mkBaseTableColAls pfx c
      in Just (S.Alias colAls, qualCol)
    mkColExp _ = Nothing

    finalWhere = maybe fltr (S.BEBin S.AndOp fltr . cBoolExp) whereM
    finalLimit = applyPermLimit permLimitM limitM

    fromItem = fromMaybe (S.FISimple tn Nothing) fromItemM

    _1 (a, _, _) = a
    _2 (_, b, _) = b
    _3 (_, _, c) = c

    procOrdByM = unzip3 . map (processAnnOrderByItem pfx) . toList <$> orderByM
    ordByExpM  = S.OrderByExp . _2 <$> procOrdByM

    -- the columns needed for orderby
    obExtrs  = maybe [] _1 procOrdByM

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

annSelToAnnNode :: Iden -> FieldName -> AnnSel -> AnnNode
annSelToAnnNode pfx fldAls annSel =
  case selFlds of
    ASFSimple flds -> ANSimple $ mkBaseNode pfx fldAls (TAFNodes flds)
      tabFrm tabPerm tabArgs
    ASFWithAgg aggFlds ->
      let allBNs = map mkAggBaseNode aggFlds
          emptyBN = mkEmptyBaseNode pfx tabFrm
          mergedBN = foldr mergeBaseNodes emptyBN allBNs
      in ANWithAgg $ AggBaseNode aggFlds mergedBN
  where
    AnnSel selFlds tabFrm tabPerm tabArgs = annSel
    mkAggBaseNode (t, selFld) =
      mkBaseNode pfx (FieldName t) selFld tabFrm tabPerm tabArgs

mergeBaseNodes :: BaseNode -> BaseNode -> BaseNode
mergeBaseNodes lNodeDet rNodeDet =
  BaseNode pfx f whr ordBy limit offset
  (HM.union lExtrs rExtrs)
  (HM.unionWith mergeRelNodes lObjs rObjs)
  (HM.union lArrs rArrs)
  where
    (BaseNode pfx f whr ordBy limit offset lExtrs lObjs lArrs) = lNodeDet
    (BaseNode _   _ _   _     _     _      rExtrs rObjs rArrs) = rNodeDet

mergeAnnNodes :: AnnNode -> AnnNode -> AnnNode
mergeAnnNodes lNode rNode =
  case (lNode, rNode) of
    (ANSimple lbn, ANSimple rbn) -> ANSimple $ mergeBaseNodes lbn rbn
    (ANSimple lbn, ANWithAgg (AggBaseNode _ rbn)) ->
      ANSimple $ mergeBaseNodes lbn rbn
    (ANWithAgg (AggBaseNode flds lbn), ANSimple rbn) ->
      ANWithAgg $ AggBaseNode flds $ mergeBaseNodes lbn rbn

    (ANWithAgg (AggBaseNode lflds lbn), ANWithAgg (AggBaseNode rflds rbn)) ->
      ANWithAgg $ AggBaseNode (lflds <> rflds) $ mergeBaseNodes lbn rbn


-- should only be used to merge obj rel nodes
mergeRelNodes :: RelNode -> RelNode -> RelNode
mergeRelNodes lNode rNode =
  RelNode rn rAls rMapn $ mergeAnnNodes lNodeDet rNodeDet
  where
    (RelNode rn rAls rMapn lNodeDet) = lNode
    (RelNode _  _    _     rNodeDet) = rNode

data RelNode
  = RelNode
  { _rnRelName    :: !RelName
  , _rnRelAlias   :: !FieldName
  , _rnRelMapping :: ![(PGCol, PGCol)]
  , _rnNodeDet    :: !AnnNode
  } deriving (Show, Eq)

mkRelNode :: Iden -> (FieldName, AnnRel) -> RelNode
mkRelNode pfx (relAls, AnnRel rn _ rMapn rAnnSel) =
  RelNode rn relAls rMapn $ annSelToAnnNode pfx relAls rAnnSel

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

    relNodeToSelect :: AnnNodeToSelOpts -> RelNode -> (S.Select, S.Alias)
    relNodeToSelect opts (RelNode _ fldName relMapn relBaseNode) =
      let als = S.Alias $ toIden fldName
      in ( annNodeToSel opts (mkJoinCond baseSelAls relMapn) als relBaseNode
         , getAliasFromAnnNode relBaseNode
         )

    objRelToFromItem :: RelNode -> S.FromItem
    objRelToFromItem =
      uncurry S.mkLateralFromItem . relNodeToSelect objRelOpts

    arrRelToFromItem :: RelNode -> S.FromItem
    arrRelToFromItem relNode =
      let (sel, als)  = relNodeToSelect arrRelOpts relNode
      in S.mkLateralFromItem sel als

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

  annOrdByML <- forM (sqOrderBy selQ) $ \(OrderByExp obItems) ->
    withPathK "order_by" $ indexedForM obItems $ mapM $
    convOrderByElem (fieldInfoMap, selPermInfo)

  let annOrdByM = NE.nonEmpty =<< annOrdByML

  -- validate limit and offset values
  withPathK "limit" $ mapM_ onlyPositiveInt mQueryLimit
  withPathK "offset" $ mapM_ onlyPositiveInt mQueryOffset

  let selFlds = ASFSimple annFlds
      tabFrom = TableFrom (spiTable selPermInfo) Nothing
      tabPerm = TablePerm (spiFilter selPermInfo) mPermLimit
  return $ AnnSel selFlds tabFrom tabPerm $
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
getSelectDeps (AnnSel flds tabFrm _ tableArgs) =
  mkParentDep tn
  : fromMaybe [] whereDeps
  <> colDeps
  <> relDeps
  <> nestedDeps
  where
    TableFrom tn _ = tabFrm
    annWc = _taWhere tableArgs
    (sCols, rCols) = partAnnFlds $ map snd $ fetchAnnFlds flds
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

mkSQLSelect :: Bool -> AnnSel -> S.Select
mkSQLSelect isSingleObject annSel =
  prefixNumToAliases $ annNodeToSel selOpts (S.BELit True)
  rootFldAls $ annSelToAnnNode (toIden rootFldName)
  rootFldName annSel
  where
    selOpts = AnnNodeToSelOpts isSingleObject False
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
