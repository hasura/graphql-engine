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

data OpCtx
  -- table, req hdrs
  = OCInsert QualifiedTable [T.Text]
  -- tn, filter exp, limit, req hdrs
  | OCSelect QualifiedTable AnnBoolExpSQL (Maybe Int) [T.Text]
  -- tn, filter exp, reqt hdrs
  | OCSelectPkey QualifiedTable AnnBoolExpSQL [T.Text]
  -- tn, filter exp, limit, req hdrs
  | OCSelectAgg QualifiedTable AnnBoolExpSQL (Maybe Int) [T.Text]
  -- tn, filter exp, req hdrs
  | OCUpdate QualifiedTable AnnBoolExpSQL [T.Text]
  -- tn, filter exp, req hdrs
  | OCDelete QualifiedTable AnnBoolExpSQL [T.Text]
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

-- data OpCtx
--   -- table, req hdrs
--   = OCInsert QualifiedTable [T.Text]
--   -- tn, filter exp, limit, req hdrs
--   | OCSelect QualifiedTable S.BoolExp (Maybe Int) [T.Text]
--   -- tn, filter exp, reqt hdrs
--   | OCSelectPkey QualifiedTable S.BoolExp [T.Text]
--   -- tn, filter exp, limit, req hdrs
--   | OCSelectAgg QualifiedTable S.BoolExp (Maybe Int) [T.Text]
--   -- tn, filter exp, req hdrs
--   | OCUpdate QualifiedTable S.BoolExp [T.Text]
--   -- tn, filter exp, req hdrs
--   | OCDelete QualifiedTable S.BoolExp [T.Text]
--   deriving (Show, Eq)

-- data GCtx
--   = GCtx
--   { _gTypes     :: !TypeMap
--   , _gFields    :: !FieldMap
--   , _gOrdByCtx  :: !OrdByCtx
--   , _gQueryRoot :: !ObjTyInfo
--   , _gMutRoot   :: !(Maybe ObjTyInfo)
--   , _gSubRoot   :: !(Maybe ObjTyInfo)
--   , _gOpCtxMap  :: !OpCtxMap
--   , _gInsCtxMap :: !InsCtxMap
--   } deriving (Show, Eq)

-- instance Has TypeMap GCtx where
--   getter = _gTypes
--   modifier f ctx = ctx { _gTypes = f $ _gTypes ctx }

instance ToJSON GCtx where
  toJSON _ = String "GCtx"

type GCtxMap = Map.HashMap RoleName GCtx

data TyAgg
  = TyAgg
  { _taTypes  :: !TypeMap
  , _taFields :: !FieldMap
  , _taOrdBy  :: !OrdByCtx
  } deriving (Show, Eq)

instance Semigroup TyAgg where
  (TyAgg t1 f1 o1) <> (TyAgg t2 f2 o2) =
    TyAgg (Map.union t1 t2) (Map.union f1 f2) (Map.union o1 o2)

instance Monoid TyAgg where
  mempty = TyAgg Map.empty Map.empty Map.empty
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
  -> G.GType
  -> ObjFldInfo
mkHsraObjFldInfo descM name params ty =
  ObjFldInfo descM name params ty HasuraType

mkHsraObjTyInfo
  :: Maybe G.Description
  -> G.NamedType
  -> ObjFieldMap
  -> ObjTyInfo
mkHsraObjTyInfo descM ty flds =
  mkObjTyInfo descM ty flds HasuraType

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
mkHsraScalarTyInfo ty = ScalarTyInfo Nothing ty HasuraType

fromInpValL :: [InpValInfo] -> Map.HashMap G.Name InpValInfo
fromInpValL = mapFromL _iviName

mkCompExpName :: PGColType -> G.Name
mkCompExpName pgColTy =
  G.Name $ T.pack (show pgColTy) <> "_comparison_exp"

mkCompExpTy :: PGColType -> G.NamedType
mkCompExpTy =
  G.NamedType . mkCompExpName

{-
input st_d_within_input {
  distance: Float!
  from: geometry!
}
-}

stDWithinInpTy :: G.NamedType
stDWithinInpTy = G.NamedType "st_d_within_input"


--- | make compare expression input type
mkCompExpInp :: PGColType -> InpObjTyInfo
mkCompExpInp colTy =
  InpObjTyInfo (Just tyDesc) (mkCompExpTy colTy) (fromInpValL $ concat
  [ map (mk colScalarTy) typedOps
  , map (mk $ G.toLT colScalarTy) listOps
  , bool [] (map (mk $ mkScalarTy PGText) stringOps) isStringTy
  , bool [] (map jsonbOpToInpVal jsonbOps) isJsonbTy
  , bool [] (stDWithinOpInpVal : map geomOpToInpVal geomOps) isGeometryTy
  , [InpValInfo Nothing "_is_null" Nothing $ G.TypeNamed (G.Nullability True) $ G.NamedType "Boolean"]
  ]) HasuraType
  where
    tyDesc = mconcat
      [ "expression to compare columns of type "
      , G.Description (T.pack $ show colTy)
      , ". All fields are combined with logical 'AND'."
      ]
    isStringTy = case colTy of
      PGVarchar -> True
      PGText    -> True
      _         -> False
    mk t n = InpValInfo Nothing n Nothing $ G.toGT t
    colScalarTy = mkScalarTy colTy
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

    isJsonbTy = case colTy of
      PGJSONB -> True
      _       -> False
    jsonbOpToInpVal (op, ty, desc) = InpValInfo (Just desc) op Nothing ty
    jsonbOps =
      [ ( "_contains"
        , G.toGT $ mkScalarTy PGJSONB
        , "does the column contain the given json value at the top level"
        )
      , ( "_contained_in"
        , G.toGT $ mkScalarTy PGJSONB
        , "is the column contained in the given json value"
        )
      , ( "_has_key"
        , G.toGT $ mkScalarTy PGText
        , "does the string exist as a top-level key in the column"
        )
      , ( "_has_keys_any"
        , G.toGT $ G.toLT $ G.toNT $ mkScalarTy PGText
        , "do any of these strings exist as top-level keys in the column"
        )
      , ( "_has_keys_all"
        , G.toGT $ G.toLT $ G.toNT $ mkScalarTy PGText
        , "do all of these strings exist as top-level keys in the column"
        )
      ]

    -- Geometry related ops
    stDWithinOpInpVal =
      InpValInfo (Just stDWithinDesc) "_st_d_within" Nothing $ G.toGT stDWithinInpTy
    stDWithinDesc =
      "is the column within a distance from a geometry value"

    isGeometryTy = case colTy of
      PGGeometry -> True
      _          -> False

    geomOpToInpVal (op, desc) =
      InpValInfo (Just desc) op Nothing $ G.toGT $ mkScalarTy PGGeometry
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
      , ( "_st_intersects"
        , "does the column spatially intersect the given geometry value"
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
defaultTypes = $(fromSchemaDocQ defaultSchema HasuraType)


mkGCtx :: TyAgg -> RootFlds -> InsCtxMap -> GCtx
mkGCtx (TyAgg tyInfos fldInfos ordByEnums) (RootFlds flds) insCtxMap =
  let queryRoot = mkHsraObjTyInfo (Just "query root")
                  (G.NamedType "query_root") $
                  mapFromL _fiName (schemaFld:typeFld:qFlds)
      scalarTys = map (TIScalar . mkHsraScalarTyInfo) colTys
      compTys   = map (TIInpObj . mkCompExpInp) colTys
      ordByEnumTyM = bool (Just ordByEnumTy) Nothing $ null qFlds
      allTys    = Map.union tyInfos $ mkTyInfoMap $
                  catMaybes [ Just $ TIObj queryRoot
                            , TIObj <$> mutRootM
                            , TIObj <$> subRootM
                            , TIEnum <$> ordByEnumTyM
                            , TIInpObj <$> stDWithinInpM
                            ] <>
                  scalarTys <> compTys <> defaultTypes
  -- for now subscription root is query root
  in GCtx allTys fldInfos ordByEnums queryRoot mutRootM subRootM
     (Map.map fst flds) insCtxMap
  where
    colTys    = Set.toList $ Set.fromList $ map pgiType $
                  lefts $ Map.elems fldInfos
    mkMutRoot =
      mkHsraObjTyInfo (Just "mutation root") (G.NamedType "mutation_root") .
      mapFromL _fiName
    mutRootM = bool (Just $ mkMutRoot mFlds) Nothing $ null mFlds
    mkSubRoot =
      mkHsraObjTyInfo (Just "subscription root")
      (G.NamedType "subscription_root") . mapFromL _fiName
    subRootM = bool (Just $ mkSubRoot qFlds) Nothing $ null qFlds
    (qFlds, mFlds) = partitionEithers $ map snd $ Map.elems flds
    schemaFld = mkHsraObjFldInfo Nothing "__schema" Map.empty $
                  G.toGT $ G.toNT $ G.NamedType "__Schema"
    typeFld = mkHsraObjFldInfo Nothing "__type" typeFldArgs $
                G.toGT $ G.NamedType "__Type"
      where
        typeFldArgs = mapFromL _iviName [
          InpValInfo (Just "name of the type") "name" Nothing
          $ G.toGT $ G.toNT $ G.NamedType "String"
          ]

    stDWithinInpM = bool Nothing (Just stDWithinInp) (PGGeometry `elem` colTys)
    stDWithinInp =
      mkHsraInpTyInfo Nothing stDWithinInpTy $ fromInpValL
      [ InpValInfo Nothing "from" Nothing $ G.toGT $ G.toNT $ mkScalarTy PGGeometry
      , InpValInfo Nothing "distance" Nothing $ G.toNT $ G.toNT $ mkScalarTy PGFloat
      ]

emptyGCtx :: GCtx
emptyGCtx = mkGCtx mempty mempty mempty
