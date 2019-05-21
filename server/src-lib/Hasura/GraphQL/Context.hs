module Hasura.GraphQL.Context where

import           Data.Aeson
import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashSet                        as Set
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.GraphQL.Resolve.ContextTypes
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types

type OpCtxMap = Map.HashMap G.Name OpCtx

data InsOpCtx
  = InsOpCtx
  { _iocTable   :: !QualifiedTable
  , _iocHeaders :: ![T.Text]
  } deriving (Show, Eq)

data SelOpCtx
  = SelOpCtx
  { _socTable   :: !QualifiedTable
  , _socHeaders :: ![T.Text]
  , _socFilter  :: !AnnBoolExpPartialSQL
  , _socLimit   :: !(Maybe Int)
  } deriving (Show, Eq)

data SelPkOpCtx
  = SelPkOpCtx
  { _spocTable   :: !QualifiedTable
  , _spocHeaders :: ![T.Text]
  , _spocFilter  :: !AnnBoolExpPartialSQL
  , _spocArgMap  :: !PGColArgMap
  } deriving (Show, Eq)

data FuncQOpCtx
  = FuncQOpCtx
  { _fqocTable    :: !QualifiedTable
  , _fqocHeaders  :: ![T.Text]
  , _fqocFilter   :: !AnnBoolExpPartialSQL
  , _fqocLimit    :: !(Maybe Int)
  , _fqocFunction :: !QualifiedFunction
  , _fqocArgs     :: !FuncArgSeq
  } deriving (Show, Eq)

data UpdOpCtx
  = UpdOpCtx
  { _uocTable      :: !QualifiedTable
  , _uocHeaders    :: ![T.Text]
  , _uocFilter     :: !AnnBoolExpPartialSQL
  , _uocPresetCols :: !PreSetColsPartial
  , _uocAllCols    :: ![PGColInfo]
  } deriving (Show, Eq)

data DelOpCtx
  = DelOpCtx
  { _docTable   :: !QualifiedTable
  , _docHeaders :: ![T.Text]
  , _docFilter  :: !AnnBoolExpPartialSQL
  , _docAllCols :: ![PGColInfo]
  } deriving (Show, Eq)

data OpCtx
  = OCSelect !SelOpCtx
  | OCSelectPkey !SelPkOpCtx
  | OCSelectAgg !SelOpCtx
  | OCFuncQuery !FuncQOpCtx
  | OCFuncAggQuery !FuncQOpCtx
  | OCInsert !InsOpCtx
  | OCUpdate !UpdOpCtx
  | OCDelete !DelOpCtx
  deriving (Show, Eq)

data GCtx
  = GCtx
  { _gTypes     :: !TypeMap
  , _gFields    :: !FieldMap
  , _gOrdByCtx  :: !OrdByCtx
  , _gQueryRoot :: !ObjTyInfo
  , _gMutRoot   :: !(Maybe ObjTyInfo)
  , _gSubRoot   :: !(Maybe ObjTyInfo)
  , _gOpCtxMap  :: !OpCtxMap
  , _gInsCtxMap :: !InsCtxMap
  } deriving (Show, Eq)

instance Has TypeMap GCtx where
  getter = _gTypes
  modifier f ctx = ctx { _gTypes = f $ _gTypes ctx }

instance ToJSON GCtx where
  toJSON _ = String "GCtx"

type GCtxMap = Map.HashMap RoleName GCtx

data TyAgg
  = TyAgg
  { _taTypes   :: !TypeMap
  , _taFields  :: !FieldMap
  , _taScalars :: !(Set.HashSet PGColType)
  , _taOrdBy   :: !OrdByCtx
  } deriving (Show, Eq)

instance Semigroup TyAgg where
  (TyAgg t1 f1 s1 o1) <> (TyAgg t2 f2 s2 o2) =
    TyAgg (Map.union t1 t2) (Map.union f1 f2)
          (Set.union s1 s2) (Map.union o1 o2)

instance Monoid TyAgg where
  mempty = TyAgg Map.empty Map.empty Set.empty Map.empty
  mappend = (<>)

newtype RootFlds
  = RootFlds
  { _taMutation :: Map.HashMap G.Name (OpCtx, Either ObjFldInfo ObjFldInfo)
  } deriving (Show, Eq)

instance Semigroup RootFlds where
  (RootFlds m1) <> (RootFlds m2)
    = RootFlds (Map.union m1 m2)

instance Monoid RootFlds where
  mempty = RootFlds Map.empty
  mappend  = (<>)

mkHsraObjFldInfo
  :: Maybe G.Description
  -> G.Name
  -> ParamMap
  -> Maybe PGColTyAnn
  -> G.GType
  -> ObjFldInfo
mkHsraObjFldInfo descM name params pgTy ty =
  ObjFldInfo descM name params pgTy ty HasuraType

mkHsraPGTyObjFld :: Maybe G.Description -> G.Name -> ParamMap -> PGColType -> ObjFldInfo
mkHsraPGTyObjFld descM name params colTy =
  mkHsraObjFldInfo descM name params (Just $ PTCol colTy) $ mkPGColGTy colTy

mkHsraObjTyInfo
  :: Maybe G.Description
  -> G.NamedType
  -> IFacesSet
  -> ObjFieldMap
  -> ObjTyInfo
mkHsraObjTyInfo descM ty implIFaces flds =
  mkObjTyInfo descM ty implIFaces flds HasuraType

mkHsraInpTyInfo
  :: Maybe G.Description
  -> G.NamedType
  -> InpObjFldMap
  -> InpObjTyInfo
mkHsraInpTyInfo descM ty flds =
  InpObjTyInfo descM ty flds HasuraType

mkHsraEnumTyInfo
  :: Maybe G.Description
  -> G.NamedType
  -> Map.HashMap G.EnumValue EnumValInfo
  -> EnumTyInfo
mkHsraEnumTyInfo descM ty enumVals =
  EnumTyInfo descM ty enumVals HasuraType

mkHsraScalarTyInfo :: PGColType -> ScalarTyInfo
mkHsraScalarTyInfo ty =
  ScalarTyInfo Nothing (G.Name $ pgColTyToScalar ty) (Just ty) HasuraType

fromInpValL :: [InpValInfo] -> Map.HashMap G.Name InpValInfo
fromInpValL = mapFromL _iviName

mkCompExpName :: PGColType -> G.Name
mkCompExpName colTy =
  G.Name $ colTyTxt colTy  <> "_comparison_exp"
  where
    colTyTxt t = case pgColTyDetails t of
      PGTyBase b   -> T.pack (show b)
      PGTyDomain b -> colTyTxt b
      _            -> asArray t

    asArray t = case getArrayBaseTy t of
      Nothing -> qualTyToScalar $ pgColTyName t
        -- Array type
      Just b  -> case pgColTyDetails b of
        PGTyBase bb -> T.pack $ show bb <> "_"
                       <> show (getPGTyArrDim t) <> "d"
        _           -> qualTyToScalar (pgColTyName b)


mkCompExpTy :: PGColType -> G.NamedType
mkCompExpTy =
  G.NamedType . mkCompExpName

-- TODO(shahidhk) this should ideally be st_d_within_geometry
{-
input st_d_within_input {
  distance: Float!
  from: geometry!
}
-}
stDWithinGeometryInpTy :: G.NamedType
stDWithinGeometryInpTy = G.NamedType "st_d_within_input"

{-
input st_d_within_geography_input {
  distance: Float!
  from: geography!
  use_spheroid: Bool!
}
-}
stDWithinGeographyInpTy :: G.NamedType
stDWithinGeographyInpTy = G.NamedType "st_d_within_geography_input"


--- | make compare expression input type
mkCompExpInp :: PGColType -> InpObjTyInfo
mkCompExpInp colTy =
  InpObjTyInfo (Just tyDesc) (mkCompExpTy colTy) (fromInpValL $ concat
  [ map (mkPGTy colTy) typedOps
  , map (mk (Just $ arrOfCol colTy) $ G.toLT colGQLTy) listOps
  , bool [] (map (mkPGTy textColTy) stringOps) isStringTy
  , bool [] (map (mkPGTy colTy) arrOps) isArrTy
  , bool [] (map jsonbOpToInpVal jsonbOps) isJsonbTy
  , bool [] (stDWithinGeoOpInpVal stDWithinGeometryInpTy :
             map geoOpToInpVal (geoOps ++ geomOps)) isGeometryType
  , bool [] (stDWithinGeoOpInpVal stDWithinGeographyInpTy :
             map geoOpToInpVal geoOps) isGeographyType
  , [ InpValInfo Nothing "_is_null" Nothing (Just $ PTCol boolColTy) $
      mkPGColGTy boolColTy
    ]
  ]) HasuraType
  where
    colDtls = pgColTyDetails colTy
    arrOfCol = PTArr . PTCol
    tyDesc = mconcat
      [ "expression to compare columns of type "
      , G.Description (G.showGT $ mkPGColGTy colTy)
      , ". All fields are combined with logical 'AND'."
      ]
    bTy = case colDtls of
      PGTyBase b -> return b
      _          -> Nothing
    isStringTy = case bTy of
      Just PGVarchar -> True
      Just PGText    -> True
      _              -> False
    isArrTy = getPGTyArrDim colTy > 0
    mk pt t n = InpValInfo Nothing n Nothing pt $ G.toGT t
    mkPGTy ty = mk (Just $ PTCol ty) $ mkPGColGTy ty
    colGQLTy = mkPGColGTy colTy
    -- colScalarListTy = GA.GTList colGTy
    typedOps =
       ["_eq", "_neq", "_gt", "_lt", "_gte", "_lte"]
    listOps =
      [ "_in", "_nin" ]
    -- TODO
    -- columnOps =
    --   [ "_ceq", "_cneq", "_cgt", "_clt", "_cgte", "_clte"]
    stringOps =
      [ "_like", "_nlike", "_ilike", "_nilike"
      , "_similar", "_nsimilar"
      ]

    arrOps =
      [ "_contains", "_is_contained_by"]

    isJsonbTy = case bTy of
      Just PGJSONB -> True
      _            -> False
    jsonbOpToInpVal (op, pgTy, desc) = InpValInfo (Just desc) op Nothing (Just pgTy) $ pgTyAnnToGTy pgTy
    jsonbOps =
      [ ( "_contains"
        , PTCol jsonbColTy
        , "does the column contain the given json value at the top level"
        )
      , ( "_contained_in"
        , PTCol jsonbColTy
        , "is the column contained in the given json value"
        )
      , ( "_has_key"
        , PTCol textColTy
        , "does the string exist as a top-level key in the column"
        )
      , ( "_has_keys_any"
        , PTArr $ PTCol textColTy
        , "do any of these strings exist as top-level keys in the column"
        )
      , ( "_has_keys_all"
        , PTArr $ PTCol textColTy
        , "do all of these strings exist as top-level keys in the column"
        )
      ]

    stDWithinGeoOpInpVal ty =
      InpValInfo (Just stDWithinGeoDesc) "_st_d_within" Nothing Nothing $ G.toGT ty
    stDWithinGeoDesc =
      "is the column within a distance from a " <> colTyDesc <> " value"

    -- Geometry related ops
    isGeometryType = case bTy of
      Just PGGeometry -> True
      _               -> False

    -- Geography related ops
    isGeographyType = case bTy of
      Just PGGeography -> True
      _                -> False

    geoOpToInpVal (op, desc) =
      InpValInfo (Just desc) op Nothing (Just $ PTCol colTy) $
      G.toGT $ mkScalarTy colTy

    colTyDesc = G.Description $ T.pack $ show colTy

    -- operators applicable only to geometry types
    geomOps :: [(G.Name, G.Description)]
    geomOps =
      [
        ( "_st_contains"
        , "does the column contain the given geometry value"
        )
      , ( "_st_crosses"
        , "does the column crosses the given geometry value"
        )
      , ( "_st_equals"
        , "is the column equal to given geometry value. Directionality is ignored"
        )
      , ( "_st_overlaps"
        , "does the column 'spatially overlap' (intersect but not completely contain) the given geometry value"
        )
      , ( "_st_touches"
        , "does the column have atleast one point in common with the given geometry value"
        )
      , ( "_st_within"
        , "is the column contained in the given geometry value"
        )
      ]

    -- operators applicable to geometry and geography types
    geoOps =
      [
        ( "_st_intersects"
        , "does the column spatially intersect the given " <> colTyDesc <> " value"
        )
      ]

ordByTy :: G.NamedType
ordByTy = G.NamedType "order_by"

ordByEnumTy :: EnumTyInfo
ordByEnumTy =
  mkHsraEnumTyInfo (Just desc) ordByTy $ mapFromL _eviVal $
  map mkEnumVal enumVals
  where
    desc = G.Description "column ordering options"
    mkEnumVal (n, d) =
      EnumValInfo (Just d) (G.EnumValue n) False
    enumVals =
      [ ( "asc"
        , "in the ascending order, nulls last"
        ),
        ( "asc_nulls_last"
        , "in the ascending order, nulls last"
        ),
        ( "asc_nulls_first"
        , "in the ascending order, nulls first"
        ),
        ( "desc"
        , "in the descending order, nulls first"
        ),
        ( "desc_nulls_first"
        , "in the descending order, nulls first"
        ),
        ( "desc_nulls_last"
        , "in the descending order, nulls last"
        )
      ]

defaultTypes :: [TypeInfo]
defaultTypes = $(fromSchemaDocQ defaultSchema defaultPGColTyMap HasuraType)


mkGCtx :: TyAgg -> RootFlds -> InsCtxMap -> GCtx
mkGCtx tyAgg (RootFlds flds) insCtxMap =
  let queryRoot = mkHsraObjTyInfo (Just "query root")
                  (G.NamedType "query_root") Set.empty $
                  mapFromL _fiName (schemaFld:typeFld:qFlds)
      scalarTys = map (TIScalar . mkHsraScalarTyInfo) (colTys <> toList scalars)
      compTys   = map (TIInpObj . mkCompExpInp) colTys
      ordByEnumTyM = bool (Just ordByEnumTy) Nothing $ null qFlds
      allTys    = Map.union tyInfos $ mkTyInfoMap $
                  catMaybes [ Just $ TIObj queryRoot
                            , TIObj <$> mutRootM
                            , TIObj <$> subRootM
                            , TIEnum <$> ordByEnumTyM
                            , TIInpObj <$> stDWithinGeometryInpM
                            , TIInpObj <$> stDWithinGeographyInpM
                            ] <>
                  scalarTys <> compTys <> defaultTypes
  -- for now subscription root is query root
  in GCtx allTys fldInfos ordByEnums queryRoot mutRootM subRootM
     (Map.map fst flds) insCtxMap
  where
    TyAgg tyInfos fldInfos scalars ordByEnums = tyAgg
    colTys    = Set.toList $ Set.fromList $ map pgiType $
                  lefts $ Map.elems fldInfos
    colTyDets = map pgColTyDetails colTys
    mkMutRoot =
      mkHsraObjTyInfo (Just "mutation root") (G.NamedType "mutation_root") Set.empty .
      mapFromL _fiName
    mutRootM = bool (Just $ mkMutRoot mFlds) Nothing $ null mFlds
    mkSubRoot =
      mkHsraObjTyInfo (Just "subscription root")
      (G.NamedType "subscription_root") Set.empty . mapFromL _fiName
    subRootM = bool (Just $ mkSubRoot qFlds) Nothing $ null qFlds
    (qFlds, mFlds) = partitionEithers $ map snd $ Map.elems flds
    schemaFld = mkHsraObjFldInfo Nothing "__schema" Map.empty Nothing $
                  G.toGT $ G.toNT $ G.NamedType "__Schema"
    typeFld = mkHsraObjFldInfo Nothing "__type" typeFldArgs Nothing $
                G.toGT $ G.NamedType "__Type"
      where
        typeFldArgs = mapFromL _iviName [
          mkPGTyInpValNT (Just "name of the type") "name" textColTy
          ]

    -- _st_d_within has to stay with geometry type
    stDWithinGeometryInpM =
      bool Nothing (Just stDWithinGeomInp) (PGTyBase PGGeometry `elem` colTyDets)
    -- _st_d_within_geography is created for geography type
    stDWithinGeographyInpM =
      bool Nothing (Just stDWithinGeogInp) (PGTyBase PGGeography `elem` colTyDets)

    stDWithinGeomInp =
      mkHsraInpTyInfo Nothing stDWithinGeometryInpTy $ fromInpValL
      [ mkPGTyInpValNT Nothing "from" geometryColTy
      , mkPGTyInpValNT Nothing "distance" floatColTy
      ]
    stDWithinGeogInp =
      mkHsraInpTyInfo Nothing stDWithinGeographyInpTy $ fromInpValL
      [ mkPGTyInpValNT Nothing "from" geographyColTy
      , mkPGTyInpValNT Nothing "distance" floatColTy
      , InpValInfo Nothing "use_spheroid"
        (Just $ G.VCBoolean True) (Just $ PTCol boolColTy) $
        G.toGT $ mkPGColGTy boolColTy
      ]

emptyGCtx :: GCtx
emptyGCtx = mkGCtx mempty mempty mempty
