module Hasura.GraphQL.Schema
  ( mkGCtxMap
  , GCtxMap
  , getGCtx
  , GCtx(..)
  , OpCtx(..)
  , InsCtx(..)
  , InsCtxMap
  , RelationInfoMap
  , isAggFld
  -- Schema stitching related
  , RemoteGCtx (..)
  , checkSchemaConflicts
  , checkConflictingNode
  , emptyGCtx
  , mergeMaybeMaps
  , ppGCtx
  ) where


import           Data.Has
import           Data.Maybe                     (maybeToList)

import qualified Data.HashMap.Strict            as Map
import qualified Data.HashSet                   as Set
import qualified Data.Text                      as T
import qualified Language.GraphQL.Draft.Syntax  as G

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal        (mkAdminRolePermInfo)
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.SQL.Types


-- defaultTypes :: [TypeInfo]
-- defaultTypes = $(fromSchemaDocQ defaultSchema HasuraType)

getInsPerm :: TableInfo -> RoleName -> Maybe InsPermInfo
getInsPerm tabInfo role
  | role == adminRole = _permIns $ mkAdminRolePermInfo tabInfo
  | otherwise = Map.lookup role rolePermInfoMap >>= _permIns
  where
    rolePermInfoMap = tiRolePermInfoMap tabInfo

getTabInfo
  :: MonadError QErr m
  => TableCache -> QualifiedTable -> m TableInfo
getTabInfo tc t =
  onNothing (Map.lookup t tc) $
     throw500 $ "table not found: " <>> t

-- type OpCtxMap = Map.HashMap G.Name OpCtx

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


data RemoteGCtx
  = RemoteGCtx
  { _rgTypes            :: !TypeMap
  , _rgQueryRoot        :: !ObjTyInfo
  , _rgMutationRoot     :: !(Maybe ObjTyInfo)
  , _rgSubscriptionRoot :: !(Maybe ObjTyInfo)
  } deriving (Show, Eq)

instance Has TypeMap RemoteGCtx where
  getter = _rgTypes
  modifier f ctx = ctx { _rgTypes = f $ _rgTypes ctx }

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

-- data TyAgg
--   = TyAgg
--   { _taTypes  :: !TypeMap
--   , _taFields :: !FieldMap
--   , _taOrdBy  :: !OrdByCtx
--   } deriving (Show, Eq)

-- instance Semigroup TyAgg where
--   (TyAgg t1 f1 o1) <> (TyAgg t2 f2 o2) =
--     TyAgg (Map.union t1 t2) (Map.union f1 f2) (Map.union o1 o2)

-- instance Monoid TyAgg where
--   mempty = TyAgg Map.empty Map.empty Map.empty
--   mappend = (<>)

type SelField = Either PGColInfo (RelInfo, Bool, AnnBoolExpSQL, Maybe Int, Bool)

-- mkHsraObjFldInfo
--   :: Maybe G.Description
--   -> G.Name
--   -> ParamMap
--   -> G.GType
--   -> ObjFldInfo
-- mkHsraObjFldInfo descM name params ty =
--   ObjFldInfo descM name params ty HasuraType

-- mkHsraObjTyInfo
--   :: Maybe G.Description
--   -> G.NamedType
--   -> ObjFieldMap
--   -> ObjTyInfo
-- mkHsraObjTyInfo descM ty flds =
--   mkObjTyInfo descM ty flds HasuraType

-- mkHsraInpTyInfo
--   :: Maybe G.Description
--   -> G.NamedType
--   -> InpObjFldMap
--   -> InpObjTyInfo
-- mkHsraInpTyInfo descM ty flds =
--   InpObjTyInfo descM ty flds HasuraType

-- mkHsraEnumTyInfo
--   :: Maybe G.Description
--   -> G.NamedType
--   -> Map.HashMap G.EnumValue EnumValInfo
--   -> EnumTyInfo
-- mkHsraEnumTyInfo descM ty enumVals =
--   EnumTyInfo descM ty enumVals HasuraType

-- mkHsraScalarTyInfo :: PGColType -> ScalarTyInfo
-- mkHsraScalarTyInfo ty = ScalarTyInfo Nothing ty HasuraType

qualTableToName :: QualifiedTable -> G.Name
qualTableToName = G.Name <$> \case
  QualifiedTable (SchemaName "public") tn -> getTableTxt tn
  QualifiedTable sn tn -> getSchemaTxt sn <> "_" <> getTableTxt tn

isValidTableName :: QualifiedTable -> Bool
isValidTableName = isValidName . qualTableToName

isValidField :: FieldInfo -> Bool
isValidField = \case
  FIColumn (PGColInfo col _ _) -> isColEligible col
  FIRelationship (RelInfo rn _ _ remTab _) -> isRelEligible rn remTab
  where
    isColEligible = isValidName . G.Name . getPGColTxt
    isRelEligible rn rt = isValidName (G.Name $ getRelTxt rn)
                          && isValidTableName rt

upsertable :: [ConstraintName] -> Bool -> Bool -> Bool
upsertable uniqueOrPrimaryCons isUpsertAllowed view =
  not (null uniqueOrPrimaryCons) && isUpsertAllowed && not view

toValidFieldInfos :: FieldInfoMap -> [FieldInfo]
toValidFieldInfos = filter isValidField . Map.elems

validPartitionFieldInfoMap :: FieldInfoMap -> ([PGColInfo], [RelInfo])
validPartitionFieldInfoMap = partitionFieldInfos . toValidFieldInfos

getValidCols :: FieldInfoMap -> [PGColInfo]
getValidCols = fst . validPartitionFieldInfoMap

getValidRels :: FieldInfoMap -> [RelInfo]
getValidRels = snd . validPartitionFieldInfoMap

mkValidConstraints :: [ConstraintName] -> [ConstraintName]
mkValidConstraints =
  filter (isValidName . G.Name . getConstraintTxt)

isRelNullable :: FieldInfoMap -> RelInfo -> Bool
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

mkColName :: PGCol -> G.Name
mkColName (PGCol n) = G.Name n

mkRelName :: RelName -> G.Name
mkRelName (RelName r) = G.Name r

mkAggRelName :: RelName -> G.Name
mkAggRelName (RelName r) = G.Name $ r <> "_aggregate"

-- mkCompExpName :: PGColType -> G.Name
-- mkCompExpName pgColTy =
--   G.Name $ T.pack (show pgColTy) <> "_comparison_exp"

-- mkCompExpTy :: PGColType -> G.NamedType
-- mkCompExpTy =
--   G.NamedType . mkCompExpName

mkBoolExpName :: QualifiedTable -> G.Name
mkBoolExpName tn =
  qualTableToName tn <> "_bool_exp"

mkBoolExpTy :: QualifiedTable -> G.NamedType
mkBoolExpTy =
  G.NamedType . mkBoolExpName

mkTableTy :: QualifiedTable -> G.NamedType
mkTableTy =
  G.NamedType . qualTableToName

mkTableAggTy :: QualifiedTable -> G.NamedType
mkTableAggTy tn =
  G.NamedType $ qualTableToName tn <> "_aggregate"

mkTableAggFldsTy :: QualifiedTable -> G.NamedType
mkTableAggFldsTy tn =
  G.NamedType $ qualTableToName tn <> "_aggregate_fields"

mkTableColAggFldsTy :: G.Name -> QualifiedTable -> G.NamedType
mkTableColAggFldsTy op tn =
  G.NamedType $ qualTableToName tn <> "_" <> op <> "_fields"

mkTableByPKeyTy :: QualifiedTable -> G.Name
mkTableByPKeyTy tn = qualTableToName tn <> "_by_pk"

-- --- | make compare expression input type
-- mkCompExpInp :: PGColType -> InpObjTyInfo
-- mkCompExpInp colTy =
--   InpObjTyInfo (Just tyDesc) (mkCompExpTy colTy) (fromInpValL $ concat
--   [ map (mk colScalarTy) typedOps
--   , map (mk $ G.toLT colScalarTy) listOps
--   , bool [] (map (mk $ mkScalarTy PGText) stringOps) isStringTy
--   , bool [] (map jsonbOpToInpVal jsonbOps) isJsonbTy
--   , [InpValInfo Nothing "_is_null" $ G.TypeNamed (G.Nullability True) $ G.NamedType "Boolean"]
--   ]) HasuraType
--   where
--     tyDesc = mconcat
--       [ "expression to compare columns of type "
--       , G.Description (T.pack $ show colTy)
--       , ". All fields are combined with logical 'AND'."
--       ]

--     isStringTy = case colTy of
--       PGVarchar -> True
--       PGText    -> True
--       _         -> False

--     mk t n = InpValInfo Nothing n $ G.toGT t

--     colScalarTy = mkScalarTy colTy
--     -- colScalarListTy = GA.GTList colGTy

--     typedOps =
--        ["_eq", "_neq", "_gt", "_lt", "_gte", "_lte"]

--     listOps =
--       [ "_in", "_nin" ]

--     -- TODO
--     -- columnOps =
--     --   [ "_ceq", "_cneq", "_cgt", "_clt", "_cgte", "_clte"]

--     stringOps =
--       [ "_like", "_nlike", "_ilike", "_nilike"
--       , "_similar", "_nsimilar"
--       ]

--     isJsonbTy = case colTy of
--       PGJSONB -> True
--       _       -> False

--     jsonbOpToInpVal (op, ty, desc) = InpValInfo (Just desc) op ty

--     jsonbOps =
--       [ ( "_contains"
--         , G.toGT $ mkScalarTy PGJSONB
--         , "does the column contain the given json value at the top level"
--         )
--       , ( "_contained_in"
--         , G.toGT $ mkScalarTy PGJSONB
--         , "is the column contained in the given json value"
--         )
--       , ( "_has_key"
--         , G.toGT $ mkScalarTy PGText
--         , "does the string exist as a top-level key in the column"
--         )
--       , ( "_has_keys_any"
--         , G.toGT $ G.toLT $ G.toNT $ mkScalarTy PGText
--         , "do any of these strings exist as top-level keys in the column"
--         )
--       , ( "_has_keys_all"
--         , G.toGT $ G.toLT $ G.toNT $ mkScalarTy PGText
--         , "do all of these strings exist as top-level keys in the column"
--         )
--       ]

mkPGColFld :: PGColInfo -> ObjFldInfo
mkPGColFld (PGColInfo colName colTy isNullable) =
  mkHsraObjFldInfo Nothing n Map.empty ty
  where
    n  = G.Name $ getPGColTxt colName
    ty = bool notNullTy nullTy isNullable
    scalarTy = mkScalarTy colTy
    notNullTy = G.toGT $ G.toNT scalarTy
    nullTy = G.toGT scalarTy

-- where: table_bool_exp
-- limit: Int
-- offset: Int
-- distinct_on: [table_select_column!]
mkSelArgs :: QualifiedTable -> [InpValInfo]
mkSelArgs tn =
  [ InpValInfo (Just whereDesc) "where" Nothing $ G.toGT $ mkBoolExpTy tn
  , InpValInfo (Just limitDesc) "limit" Nothing $ G.toGT $ mkScalarTy PGInteger
  , InpValInfo (Just offsetDesc) "offset" Nothing $ G.toGT $ mkScalarTy PGInteger
  , InpValInfo (Just orderByDesc) "order_by" Nothing $ G.toGT $ G.toLT $ G.toNT $
    mkOrdByTy tn
  , InpValInfo (Just distinctDesc) "distinct_on" Nothing $ G.toGT $ G.toLT $
    G.toNT $ mkSelColumnInpTy tn
  ]
  where
    whereDesc   = "filter the rows returned"
    limitDesc   = "limit the nuber of rows returned"
    offsetDesc  = "skip the first n rows. Use only with order_by"
    orderByDesc = "sort the rows by one or more columns"
    distinctDesc = "distinct select on columns"

-- fromInpValL :: [InpValInfo] -> Map.HashMap G.Name InpValInfo
-- fromInpValL = mapFromL _iviName

{-

array_relationship(
  where: remote_table_bool_exp
  limit: Int
  offset: Int
):  [remote_table!]!
array_relationship_aggregate(
  where: remote_table_bool_exp
  limit: Int
  offset: Int
):  remote_table_aggregate!
object_relationship: remote_table

-}
mkRelFld
  :: Bool
  -> RelInfo
  -> Bool
  -> [ObjFldInfo]
mkRelFld allowAgg (RelInfo rn rTy _ remTab isManual) isNullable = case rTy of
  ArrRel -> bool [arrRelFld] [arrRelFld, aggArrRelFld] allowAgg
  ObjRel -> [objRelFld]
  where
    objRelFld = mkHsraObjFldInfo (Just "An object relationship")
      (G.Name $ getRelTxt rn) Map.empty objRelTy
    objRelTy = bool (G.toGT $ G.toNT relTabTy) (G.toGT relTabTy) isObjRelNullable
    isObjRelNullable = isManual || isNullable
    relTabTy = mkTableTy remTab

    arrRelFld =
      mkHsraObjFldInfo (Just "An array relationship") (G.Name $ getRelTxt rn)
      (fromInpValL $ mkSelArgs remTab) arrRelTy
    arrRelTy = G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableTy remTab
    aggArrRelFld = mkHsraObjFldInfo (Just "An aggregated array relationship")
      (mkAggRelName rn) (fromInpValL $ mkSelArgs remTab) $
      G.toGT $ G.toNT $ mkTableAggTy remTab

{-
type table {
  col1: colty1
  .
  .
  rel1: relty1
}
-}
mkTableObj
  :: QualifiedTable
  -> [SelField]
  -> ObjTyInfo
mkTableObj tn allowedFlds =
  mkObjTyInfo (Just desc) (mkTableTy tn) (mapFromL _fiName flds) HasuraType
  where
    flds = concatMap (either (pure . mkPGColFld) mkRelFld') allowedFlds
    mkRelFld' (relInfo, allowAgg, _, _, isNullable) =
      mkRelFld allowAgg relInfo isNullable
    desc = G.Description $ "columns and relationships of " <>> tn

{-
type table_aggregate {
  agg: table_aggregate_fields
  nodes: [table!]!
}
-}
mkTableAggObj
  :: QualifiedTable -> ObjTyInfo
mkTableAggObj tn =
  mkHsraObjTyInfo (Just desc) (mkTableAggTy tn) $ mapFromL _fiName
  [aggFld, nodesFld]
  where
    desc = G.Description $
      "aggregated selection of " <>> tn

    aggFld = mkHsraObjFldInfo Nothing "aggregate" Map.empty $ G.toGT $
             mkTableAggFldsTy tn
    nodesFld = mkHsraObjFldInfo Nothing "nodes" Map.empty $ G.toGT $
               G.toNT $ G.toLT $ G.toNT $ mkTableTy tn

{-
type table_aggregate_fields{
  count: Int
  sum: table_sum_fields
  avg: table_avg_fields
  stddev: table_stddev_fields
  stddev_pop: table_stddev_pop_fields
  variance: table_variance_fields
  var_pop: table_var_pop_fields
  max: table_max_fields
  min: table_min_fields
}
-}
mkTableAggFldsObj
  :: QualifiedTable -> [PGCol] -> [PGCol] -> ObjTyInfo
mkTableAggFldsObj tn numCols compCols =
  mkHsraObjTyInfo (Just desc) (mkTableAggFldsTy tn) $ mapFromL _fiName $
  countFld : (numFlds <> compFlds)
  where
    desc = G.Description $
      "aggregate fields of " <>> tn

    countFld = mkHsraObjFldInfo Nothing "count" countParams $ G.toGT $
               mkScalarTy PGInteger

    countParams = fromInpValL [countColInpVal, distinctInpVal]

    countColInpVal = InpValInfo Nothing "columns" Nothing $ G.toGT $
                     G.toLT $ G.toNT $ mkSelColumnInpTy tn
    distinctInpVal = InpValInfo Nothing "distinct" Nothing $ G.toGT $
                     mkScalarTy PGBoolean

    numFlds = bool (map mkColOpFld numAggOps) [] $ null numCols
    compFlds = bool (map mkColOpFld compAggOps) [] $ null compCols

    mkColOpFld op = mkHsraObjFldInfo Nothing op Map.empty $ G.toGT $
                    mkTableColAggFldsTy op tn

{-
type table_<agg-op>_fields{
   num_col: Int
   .        .
   .        .
}
-}
mkTableColAggFldsObj
  :: QualifiedTable
  -> G.Name
  -> (PGColType -> G.NamedType)
  -> [PGColInfo]
  -> ObjTyInfo
mkTableColAggFldsObj tn op f cols =
  mkHsraObjTyInfo (Just desc) (mkTableColAggFldsTy op tn) $ mapFromL _fiName $
  map mkColObjFld cols
  where
    desc = G.Description $ "aggregate " <> G.unName op <> " on columns"

    mkColObjFld c = mkHsraObjFldInfo Nothing (G.Name $ getPGColTxt $ pgiName c)
                    Map.empty $ G.toGT $ f $ pgiType c

{-

table(
  where: table_bool_exp
  limit: Int
  offset: Int
):  [table!]!

-}
mkSelFld
  :: QualifiedTable
  -> ObjFldInfo
mkSelFld tn =
  mkHsraObjFldInfo (Just desc) fldName args ty
  where
    desc    = G.Description $ "fetch data from the table: " <>> tn
    fldName = qualTableToName tn
    args    = fromInpValL $ mkSelArgs tn
    ty      = G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableTy tn

{-
table_by_pk(
  col1: value1!,
  .     .
  .     .
  coln: valuen!
): table
-}
mkSelFldPKey
  :: QualifiedTable -> [PGColInfo]
  -> ObjFldInfo
mkSelFldPKey tn cols =
  mkHsraObjFldInfo (Just desc) fldName args ty
  where
    desc = G.Description $ "fetch data from the table: " <> tn
           <<> " using primary key columns"
    fldName = mkTableByPKeyTy tn
    args = fromInpValL $ map colInpVal cols
    ty = G.toGT $ mkTableTy tn
    colInpVal (PGColInfo n typ _) =
      InpValInfo Nothing (mkColName n) Nothing $ G.toGT $ G.toNT $ mkScalarTy typ

{-

table_aggregate(
  where: table_bool_exp
  limit: Int
  offset: Int
): table_aggregate!

-}
mkAggSelFld
  :: QualifiedTable
  -> ObjFldInfo
mkAggSelFld tn =
  mkHsraObjFldInfo (Just desc) fldName args ty
  where
    desc = G.Description $ "fetch aggregated fields from the table: "
           <>> tn
    fldName = qualTableToName tn <> "_aggregate"
    args = fromInpValL $ mkSelArgs tn
    ty = G.toGT $ G.toNT $ mkTableAggTy tn

-- table_mutation_response
mkMutRespTy :: QualifiedTable -> G.NamedType
mkMutRespTy tn =
  G.NamedType $ qualTableToName tn <> "_mutation_response"

{-
type table_mutation_response {
  affected_rows: Int!
  returning: [table!]!
}
-}
mkMutRespObj
  :: QualifiedTable
  -> Bool -- is sel perm defined
  -> ObjTyInfo
mkMutRespObj tn sel =
  mkHsraObjTyInfo (Just objDesc) (mkMutRespTy tn) $ mapFromL _fiName
    $ affectedRowsFld : bool [] [returningFld] sel
  where
    objDesc = G.Description $
      "response of any mutation on the table " <>> tn
    affectedRowsFld =
      mkHsraObjFldInfo (Just desc) "affected_rows" Map.empty $
        G.toGT $ G.toNT $ mkScalarTy PGInteger
      where
        desc = "number of affected rows by the mutation"
    returningFld =
      mkHsraObjFldInfo (Just desc) "returning" Map.empty $
        G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableTy tn
      where
        desc = "data of the affected rows by the mutation"

mkBoolExpInp
  :: QualifiedTable
  -- the fields that are allowed
  -> [SelField]
  -> InpObjTyInfo
mkBoolExpInp tn fields =
  mkHsraInpTyInfo (Just desc) boolExpTy $ Map.fromList
    [(_iviName inpVal, inpVal) | inpVal <- inpValues]
  where
    desc = G.Description $
      "Boolean expression to filter rows from the table " <> tn <<>
      ". All fields are combined with a logical 'AND'."

    -- the type of this boolean expression
    boolExpTy = mkBoolExpTy tn

    -- all the fields of this input object
    inpValues = combinators <> map mkFldExpInp fields

    mk n ty = InpValInfo Nothing n Nothing $ G.toGT ty

    boolExpListTy = G.toLT boolExpTy

    combinators =
      [ mk "_not" boolExpTy
      , mk "_and" boolExpListTy
      , mk "_or"  boolExpListTy
      ]

    mkFldExpInp = \case
      Left (PGColInfo colName colTy _) ->
        mk (mkColName colName) (mkCompExpTy colTy)
      Right (RelInfo relName _ _ remTab _, _, _, _, _) ->
        mk (G.Name $ getRelTxt relName) (mkBoolExpTy remTab)

mkPGColInp :: PGColInfo -> InpValInfo
mkPGColInp (PGColInfo colName colTy _) =
  InpValInfo Nothing (G.Name $ getPGColTxt colName) Nothing $
  G.toGT $ mkScalarTy colTy

-- table_set_input
mkUpdSetTy :: QualifiedTable -> G.NamedType
mkUpdSetTy tn =
  G.NamedType $ qualTableToName tn <> "_set_input"

{-
input table_set_input {
  col1: colty1
  .
  .
  coln: coltyn
}
-}
mkUpdSetInp
  :: QualifiedTable -> [PGColInfo] -> InpObjTyInfo
mkUpdSetInp tn cols  =
  mkHsraInpTyInfo (Just desc) (mkUpdSetTy tn) $
    fromInpValL $ map mkPGColInp cols
  where
    desc = G.Description $
      "input type for updating data in table " <>> tn

-- table_inc_input
mkUpdIncTy :: QualifiedTable -> G.NamedType
mkUpdIncTy tn =
  G.NamedType $ qualTableToName tn <> "_inc_input"

{-
input table_inc_input {
  integer-col1: int
  .
  .
  integer-coln: int
}
-}

mkUpdIncInp
  :: QualifiedTable -> Maybe [PGColInfo] -> Maybe InpObjTyInfo
mkUpdIncInp tn = maybe Nothing mkType
  where
    mkType cols = let intCols = onlyIntCols cols
                      incObjTy =
                        mkHsraInpTyInfo (Just desc) (mkUpdIncTy tn) $
                          fromInpValL $ map mkPGColInp intCols
                  in bool (Just incObjTy) Nothing $ null intCols
    desc = G.Description $
      "input type for incrementing integer columne in table " <>> tn

-- table_<json-op>_input
mkJSONOpTy :: QualifiedTable -> G.Name -> G.NamedType
mkJSONOpTy tn op =
  G.NamedType $ qualTableToName tn <> op <> "_input"

-- json ops are _concat, _delete_key, _delete_elem, _delete_at_path
{-
input table_concat_input {
  jsonb-col1: json
  .
  .
  jsonb-coln: json
}
-}

{-
input table_delete_key_input {
  jsonb-col1: string
  .
  .
  jsonb-coln: string
}
-}

{-
input table_delete_elem_input {
  jsonb-col1: int
  .
  .
  jsonb-coln: int
}
-}

{-
input table_delete_at_path_input {
  jsonb-col1: [string]
  .
  .
  jsonb-coln: [string]
}
-}

-- jsonb operators and descriptions
prependOp :: G.Name
prependOp = "_prepend"

prependDesc :: G.Description
prependDesc = "prepend existing jsonb value of filtered columns with new jsonb value"

appendOp :: G.Name
appendOp = "_append"

appendDesc :: G.Description
appendDesc = "append existing jsonb value of filtered columns with new jsonb value"

deleteKeyOp :: G.Name
deleteKeyOp = "_delete_key"

deleteKeyDesc :: G.Description
deleteKeyDesc = "delete key/value pair or string element."
                <> " key/value pairs are matched based on their key value"

deleteElemOp :: G.Name
deleteElemOp = "_delete_elem"

deleteElemDesc :: G.Description
deleteElemDesc = "delete the array element with specified index (negative integers count from the end)."
                 <> " throws an error if top level container is not an array"

deleteAtPathOp :: G.Name
deleteAtPathOp = "_delete_at_path"

deleteAtPathDesc :: G.Description
deleteAtPathDesc = "delete the field or element with specified path"
                   <> " (for JSON arrays, negative integers count from the end)"

mkUpdJSONOpInp
  :: QualifiedTable -> [PGColInfo] -> [InpObjTyInfo]
mkUpdJSONOpInp tn cols = bool inpObjs [] $ null jsonbCols
  where
    jsonbCols = onlyJSONBCols cols
    jsonbColNames = map pgiName jsonbCols

    inpObjs = [ prependInpObj, appendInpObj, deleteKeyInpObj
              , deleteElemInpObj, deleteAtPathInpObj
              ]

    appendInpObj =
      mkHsraInpTyInfo (Just appendDesc) (mkJSONOpTy tn appendOp) $
      fromInpValL $ map mkPGColInp jsonbCols

    prependInpObj =
      mkHsraInpTyInfo (Just prependDesc) (mkJSONOpTy tn prependOp) $
      fromInpValL $ map mkPGColInp jsonbCols

    deleteKeyInpObj =
      mkHsraInpTyInfo (Just deleteKeyDesc) (mkJSONOpTy tn deleteKeyOp) $
      fromInpValL $ map deleteKeyInpVal jsonbColNames
    deleteKeyInpVal c = InpValInfo Nothing (G.Name $ getPGColTxt c) Nothing $
      G.toGT $ G.NamedType "String"

    deleteElemInpObj =
      mkHsraInpTyInfo (Just deleteElemDesc) (mkJSONOpTy tn deleteElemOp) $
      fromInpValL $ map deleteElemInpVal jsonbColNames
    deleteElemInpVal c = InpValInfo Nothing (G.Name $ getPGColTxt c) Nothing $
      G.toGT $ G.NamedType "Int"

    deleteAtPathInpObj =
      mkHsraInpTyInfo (Just deleteAtPathDesc) (mkJSONOpTy tn deleteAtPathOp) $
      fromInpValL $ map deleteAtPathInpVal jsonbColNames
    deleteAtPathInpVal c = InpValInfo Nothing (G.Name $ getPGColTxt c) Nothing $
      G.toGT $ G.toLT $ G.NamedType "String"

{-

update_table(
  where : table_bool_exp!
  _set  : table_set_input
  _inc  : table_inc_input
  _concat: table_concat_input
  _delete_key: table_delete_key_input
  _delete_elem: table_delete_elem_input
  _delete_path_at: table_delete_path_at_input
): table_mutation_response

-}

mkIncInpVal :: QualifiedTable -> [PGColInfo] -> Maybe InpValInfo
mkIncInpVal tn cols = bool (Just incArg) Nothing $ null intCols
  where
    intCols = onlyIntCols cols
    incArgDesc = "increments the integer columns with given value of the filtered values"
    incArg =
      InpValInfo (Just incArgDesc) "_inc" Nothing $ G.toGT $ mkUpdIncTy tn

mkJSONOpInpVals :: QualifiedTable -> [PGColInfo] -> [InpValInfo]
mkJSONOpInpVals tn cols = bool jsonbOpArgs [] $ null jsonbCols
  where
    jsonbCols = onlyJSONBCols cols
    jsonbOpArgs = [appendArg, prependArg, deleteKeyArg, deleteElemArg, deleteAtPathArg]

    appendArg =
      InpValInfo (Just appendDesc) appendOp Nothing $ G.toGT $ mkJSONOpTy tn appendOp

    prependArg =
      InpValInfo (Just prependDesc) prependOp Nothing $ G.toGT $ mkJSONOpTy tn prependOp

    deleteKeyArg =
      InpValInfo (Just deleteKeyDesc) deleteKeyOp Nothing $
      G.toGT $ mkJSONOpTy tn deleteKeyOp

    deleteElemArg =
      InpValInfo (Just deleteElemDesc) deleteElemOp Nothing $
      G.toGT $ mkJSONOpTy tn deleteElemOp

    deleteAtPathArg =
      InpValInfo (Just deleteAtPathDesc) deleteAtPathOp Nothing $
      G.toGT $ mkJSONOpTy tn deleteAtPathOp

mkUpdMutFld
  :: QualifiedTable -> [PGColInfo] -> ObjFldInfo
mkUpdMutFld tn cols =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL inputValues) $
    G.toGT $ mkMutRespTy tn
  where
    inputValues = [filterArg, setArg] <> incArg
                  <> mkJSONOpInpVals tn cols
    desc = G.Description $ "update data of the table: " <>> tn

    fldName = "update_" <> qualTableToName tn

    filterArgDesc = "filter the rows which have to be updated"
    filterArg =
      InpValInfo (Just filterArgDesc) "where" Nothing $ G.toGT $
      G.toNT $ mkBoolExpTy tn

    setArgDesc = "sets the columns of the filtered rows to the given values"
    setArg =
      InpValInfo (Just setArgDesc) "_set" Nothing $ G.toGT $ mkUpdSetTy tn

    incArg = maybeToList $ mkIncInpVal tn cols

{-

delete_table(
  where : table_bool_exp!
): table_mutation_response

-}

mkDelMutFld
  :: QualifiedTable -> ObjFldInfo
mkDelMutFld tn =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL [filterArg]) $
    G.toGT $ mkMutRespTy tn
  where
    desc = G.Description $ "delete data from the table: " <>> tn

    fldName = "delete_" <> qualTableToName tn

    filterArgDesc = "filter the rows which have to be deleted"
    filterArg =
      InpValInfo (Just filterArgDesc) "where" Nothing $ G.toGT $
      G.toNT $ mkBoolExpTy tn

-- table_insert_input
mkInsInpTy :: QualifiedTable -> G.NamedType
mkInsInpTy tn =
  G.NamedType $ qualTableToName tn <> "_insert_input"

-- table_obj_rel_insert_input
mkObjInsInpTy :: QualifiedTable -> G.NamedType
mkObjInsInpTy tn =
  G.NamedType $ qualTableToName tn <> "_obj_rel_insert_input"

-- table_arr_rel_insert_input
mkArrInsInpTy :: QualifiedTable -> G.NamedType
mkArrInsInpTy tn =
  G.NamedType $ qualTableToName tn <> "_arr_rel_insert_input"


-- table_on_conflict
mkOnConflictInpTy :: QualifiedTable -> G.NamedType
mkOnConflictInpTy tn =
  G.NamedType $ qualTableToName tn <> "_on_conflict"

-- table_constraint
mkConstraintInpTy :: QualifiedTable -> G.NamedType
mkConstraintInpTy tn =
  G.NamedType $ qualTableToName tn <> "_constraint"

-- conflict_action
conflictActionTy :: G.NamedType
conflictActionTy = G.NamedType "conflict_action"

-- table_update_column
mkUpdColumnInpTy :: QualifiedTable -> G.NamedType
mkUpdColumnInpTy tn =
  G.NamedType $ qualTableToName tn <> "_update_column"

--table_select_column
mkSelColumnInpTy :: QualifiedTable -> G.NamedType
mkSelColumnInpTy tn =
  G.NamedType $ qualTableToName tn <> "_select_column"

{-
input table_obj_rel_insert_input {
  data: table_insert_input!
  on_conflict: table_on_conflict
}

-}

{-
input table_arr_rel_insert_input {
  data: [table_insert_input!]!
  on_conflict: table_on_conflict
}

-}

mkRelInsInps
  :: QualifiedTable -> Bool -> [InpObjTyInfo]
mkRelInsInps tn upsertAllowed = [objRelInsInp, arrRelInsInp]
  where
    onConflictInpVal =
      InpValInfo Nothing "on_conflict" Nothing $ G.toGT $ mkOnConflictInpTy tn

    onConflictInp = bool [] [onConflictInpVal] upsertAllowed

    objRelDesc = G.Description $
      "input type for inserting object relation for remote table " <>> tn

    objRelDataInp = InpValInfo Nothing "data" Nothing $ G.toGT $
                    G.toNT $ mkInsInpTy tn
    objRelInsInp = mkHsraInpTyInfo (Just objRelDesc) (mkObjInsInpTy tn)
                   $ fromInpValL $ objRelDataInp : onConflictInp

    arrRelDesc = G.Description $
      "input type for inserting array relation for remote table " <>> tn

    arrRelDataInp = InpValInfo Nothing "data" Nothing $ G.toGT $
                    G.toNT $ G.toLT $ G.toNT $ mkInsInpTy tn
    arrRelInsInp = mkHsraInpTyInfo (Just arrRelDesc) (mkArrInsInpTy tn)
                   $ fromInpValL $ arrRelDataInp : onConflictInp

{-

input table_insert_input {
  col1: colty1
  .
  .
  coln: coltyn
}

-}

mkInsInp
  :: QualifiedTable -> InsCtx -> InpObjTyInfo
mkInsInp tn insCtx =
  mkHsraInpTyInfo (Just desc) (mkInsInpTy tn) $ fromInpValL $
  map mkPGColInp insCols <> relInps
  where
    desc = G.Description $
      "input type for inserting data into table " <>> tn
    cols = icColumns insCtx
    setCols = Map.keys $ icSet insCtx
    insCols = flip filter cols $ \ci -> pgiName ci `notElem` setCols
    relInfoMap = icRelations insCtx

    relInps = flip map (Map.toList relInfoMap) $
      \(relName, relInfo) ->
         let rty = riType relInfo
             remoteQT = riRTable relInfo
         in case rty of
            ObjRel -> InpValInfo Nothing (G.Name $ getRelTxt relName) Nothing $
                      G.toGT $ mkObjInsInpTy remoteQT
            ArrRel -> InpValInfo Nothing (G.Name $ getRelTxt relName) Nothing $
                      G.toGT $ mkArrInsInpTy remoteQT

{-

input table_on_conflict {
  action: conflict_action
  constraint: table_constraint!
  update_columns: [table_column!]
}

-}

mkOnConflictInp :: QualifiedTable -> InpObjTyInfo
mkOnConflictInp tn =
  mkHsraInpTyInfo (Just desc) (mkOnConflictInpTy tn) $ fromInpValL
  [constraintInpVal, updateColumnsInpVal]
  where
    desc = G.Description $
      "on conflict condition type for table " <>> tn

    constraintInpVal = InpValInfo Nothing (G.Name "constraint") Nothing $
      G.toGT $ G.toNT $ mkConstraintInpTy tn

    updateColumnsInpVal = InpValInfo Nothing (G.Name "update_columns") Nothing $
      G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkUpdColumnInpTy tn
{-

insert_table(
  objects: [table_insert_input!]!
  on_conflict: table_on_conflict
  ): table_mutation_response!
-}

mkInsMutFld
  :: QualifiedTable -> Bool -> ObjFldInfo
mkInsMutFld tn isUpsertable =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL inputVals) $
    G.toGT $ mkMutRespTy tn
  where
    inputVals = catMaybes [Just objectsArg , onConflictInpVal]
    desc = G.Description $
      "insert data into the table: " <>> tn

    fldName = "insert_" <> qualTableToName tn

    objsArgDesc = "the rows to be inserted"
    objectsArg =
      InpValInfo (Just objsArgDesc) "objects" Nothing $ G.toGT $
      G.toNT $ G.toLT $ G.toNT $ mkInsInpTy tn

    onConflictInpVal = bool Nothing (Just onConflictArg) isUpsertable

    onConflictDesc = "on conflict condition"
    onConflictArg =
      InpValInfo (Just onConflictDesc) "on_conflict" Nothing $ G.toGT $ mkOnConflictInpTy tn

mkConstriantTy :: QualifiedTable -> [ConstraintName] -> EnumTyInfo
mkConstriantTy tn cons = enumTyInfo
  where
    enumTyInfo = mkHsraEnumTyInfo (Just desc) (mkConstraintInpTy tn) $
                 mapFromL _eviVal $ map mkConstraintEnumVal cons

    desc = G.Description $
      "unique or primary key constraints on table " <>> tn

    mkConstraintEnumVal (ConstraintName n) =
      EnumValInfo (Just "unique or primary key constraint")
      (G.EnumValue $ G.Name n) False

mkColumnEnumVal :: PGCol -> EnumValInfo
mkColumnEnumVal (PGCol col) =
  EnumValInfo (Just "column name") (G.EnumValue $ G.Name col) False

mkUpdColumnTy :: QualifiedTable -> [PGCol] -> EnumTyInfo
mkUpdColumnTy tn cols = enumTyInfo
  where
    enumTyInfo = mkHsraEnumTyInfo (Just desc) (mkUpdColumnInpTy tn) $
                 mapFromL _eviVal $ map mkColumnEnumVal cols

    desc = G.Description $
      "update columns of table " <>> tn

mkSelColumnTy :: QualifiedTable -> [PGCol] -> EnumTyInfo
mkSelColumnTy tn cols = enumTyInfo
  where
    enumTyInfo = mkHsraEnumTyInfo (Just desc) (mkSelColumnInpTy tn) $
                 mapFromL _eviVal $ map mkColumnEnumVal cols

    desc = G.Description $
      "select columns of table " <>> tn

mkConflictActionTy :: Bool -> EnumTyInfo
mkConflictActionTy updAllowed =
  mkHsraEnumTyInfo (Just desc) conflictActionTy $ mapFromL _eviVal $
  [enumValIgnore] <> bool [] [enumValUpdate] updAllowed
  where
    desc = G.Description "conflict action"
    enumValIgnore = EnumValInfo (Just "ignore the insert on this row")
                    (G.EnumValue "ignore") False
    enumValUpdate = EnumValInfo (Just "update the row with the given values")
                    (G.EnumValue "update") False

-- ordByTy :: G.NamedType
-- ordByTy = G.NamedType "order_by"

-- ordByEnumTy :: EnumTyInfo
-- ordByEnumTy =
--   mkHsraEnumTyInfo (Just desc) ordByTy $ mapFromL _eviVal $
--   map mkEnumVal enumVals
--   where
--     desc = G.Description "column ordering options"
--     mkEnumVal (n, d) =
--       EnumValInfo (Just d) (G.EnumValue n) False
--     enumVals =
--       [ ( "asc"
--         , "in the ascending order, nulls last"
--         ),
--         ( "desc"
--         , "in the descending order, nulls last"
--         ),
--         ( "asc_nulls_first"
--         , "in the ascending order, nulls first"
--         ),
--         ( "desc_nulls_first"
--         , "in the ascending order, nulls first"
--         )
--       ]

mkTabAggOpOrdByTy :: QualifiedTable -> G.Name -> G.NamedType
mkTabAggOpOrdByTy tn op =
  G.NamedType $ qualTableToName tn <> "_" <> op <> "_order_by"

{-
input table_<op>_order_by {
  col1: order_by
  .     .
  .     .
}
-}

mkTabAggOpOrdByInpObjs
  :: QualifiedTable -> [PGCol] -> [PGCol] -> [InpObjTyInfo]
mkTabAggOpOrdByInpObjs tn numCols compCols =
  mapMaybe (mkInpObjTyM numCols) numAggOps
  <> mapMaybe (mkInpObjTyM compCols) compAggOps
  where

    mkDesc (G.Name op) = G.Description $ "order by " <> op <> "() on columns of table " <>> tn

    mkInpObjTyM cols op = bool (Just $ mkInpObjTy cols op) Nothing $ null cols
    mkInpObjTy cols op = mkHsraInpTyInfo (Just $ mkDesc op) (mkTabAggOpOrdByTy tn op) $
                         fromInpValL $ map mkColInpVal cols

    mkColInpVal c = InpValInfo Nothing (mkColName c) Nothing $ G.toGT
                    ordByTy

mkTabAggOrdByTy :: QualifiedTable -> G.NamedType
mkTabAggOrdByTy tn =
  G.NamedType $ qualTableToName tn <> "_aggregate_order_by"

{-
input table_aggregate_order_by {
count: order_by
  <op-name>: table_<op-name>_order_by
}
-}

mkTabAggOrdByInpObj
  :: QualifiedTable -> [PGCol] -> [PGCol] -> InpObjTyInfo
mkTabAggOrdByInpObj tn numCols compCols =
  mkHsraInpTyInfo (Just desc) (mkTabAggOrdByTy tn) $ fromInpValL $
  numOpOrdBys <> compOpOrdBys <> [countInpVal]
  where
    desc = G.Description $
      "order by aggregate values of table " <>> tn

    numOpOrdBys = bool (map mkInpValInfo numAggOps) [] $ null numCols
    compOpOrdBys = bool (map mkInpValInfo compAggOps) [] $ null compCols
    mkInpValInfo op = InpValInfo Nothing op Nothing $ G.toGT $
                     mkTabAggOpOrdByTy tn op

    countInpVal = InpValInfo Nothing "count" Nothing $ G.toGT ordByTy

mkOrdByTy :: QualifiedTable -> G.NamedType
mkOrdByTy tn =
  G.NamedType $ qualTableToName tn <> "_order_by"

{-
input table_order_by {
  col1: order_by
  col2: order_by
  .     .
  .     .
  coln: order_by
  obj-rel: <remote-table>_order_by
}
-}

mkOrdByInpObj
  :: QualifiedTable -> [SelField] -> (InpObjTyInfo, OrdByCtx)
mkOrdByInpObj tn selFlds = (inpObjTy, ordByCtx)
  where
    inpObjTy =
      mkHsraInpTyInfo (Just desc) namedTy $ fromInpValL $
      map mkColOrdBy pgCols <> map mkObjRelOrdBy objRels
      <> mapMaybe mkArrRelAggOrdBy arrRels

    namedTy = mkOrdByTy tn
    desc = G.Description $
      "ordering options when selecting data from " <>> tn

    pgCols = lefts selFlds
    relFltr ty = flip filter (rights selFlds) $ \(ri, _, _, _, _) ->
      riType ri == ty
    objRels = relFltr ObjRel
    arrRels = relFltr ArrRel

    mkColOrdBy ci = InpValInfo Nothing (mkColName $ pgiName ci) Nothing $
                    G.toGT ordByTy
    mkObjRelOrdBy (ri, _, _, _, _) =
      InpValInfo Nothing (mkRelName $ riName ri) Nothing $
      G.toGT $ mkOrdByTy $ riRTable ri

    mkArrRelAggOrdBy (ri, isAggAllowed, _, _, _) =
      let ivi = InpValInfo Nothing (mkAggRelName $ riName ri) Nothing $
            G.toGT $ mkTabAggOrdByTy $ riRTable ri
      in bool Nothing (Just ivi) isAggAllowed

    ordByCtx = Map.singleton namedTy $ Map.fromList $
               colOrdBys <> relOrdBys <> arrRelOrdBys
    colOrdBys = flip map pgCols $ \ci ->
                                    ( mkColName $ pgiName ci
                                    , OBIPGCol ci
                                    )
    relOrdBys = flip map objRels $ \(ri, _, fltr, _, _) ->
                                     ( mkRelName $ riName ri
                                     , OBIRel ri fltr
                                     )
    arrRelOrdBys = flip mapMaybe arrRels $ \(ri, isAggAllowed, fltr, _, _) ->
                     let obItem = ( mkAggRelName $ riName ri
                                  , OBIAgg ri fltr
                                  )
                     in bool Nothing (Just obItem) isAggAllowed

-- newtype RootFlds
--   = RootFlds
--   { _taMutation :: Map.HashMap G.Name (OpCtx, Either ObjFldInfo ObjFldInfo)
--   } deriving (Show, Eq)

-- instance Semigroup RootFlds where
--   (RootFlds m1) <> (RootFlds m2)
--     = RootFlds (Map.union m1 m2)

-- instance Monoid RootFlds where
--   mempty = RootFlds Map.empty
--   mappend  = (<>)

mkOnConflictTypes
  :: QualifiedTable -> [ConstraintName] -> [PGCol] -> Bool -> [TypeInfo]
mkOnConflictTypes tn uniqueOrPrimaryCons cols =
  bool [] tyInfos
  where
    tyInfos = [ TIEnum $ mkConflictActionTy isUpdAllowed
              , TIEnum $ mkConstriantTy tn uniqueOrPrimaryCons
              , TIEnum $ mkUpdColumnTy tn cols
              , TIInpObj $ mkOnConflictInp tn
              ]
    isUpdAllowed = not $ null cols

mkGCtxRole'
  :: QualifiedTable
  -- insert perm
  -> Maybe (InsCtx, Bool)
  -- select permission
  -> Maybe (Bool, [SelField])
  -- update cols
  -> Maybe [PGColInfo]
  -- delete cols
  -> Maybe ()
  -- primary key columns
  -> [PGColInfo]
  -- constraints
  -> [ConstraintName]
  -> Maybe ViewInfo
  -> TyAgg
mkGCtxRole' tn insPermM selPermM updColsM delPermM pkeyCols constraints viM =
  TyAgg (mkTyInfoMap allTypes) fieldMap ordByCtx

  where

    ordByCtx = fromMaybe Map.empty ordByCtxM
    upsertPerm = or $ fmap snd insPermM
    isUpsertable = upsertable constraints upsertPerm $ isJust viM
    updatableCols = maybe [] (map pgiName) updColsM
    onConflictTypes = mkOnConflictTypes tn constraints updatableCols isUpsertable
    jsonOpTys = fromMaybe [] updJSONOpInpObjTysM
    relInsInpObjTys = maybe [] (map TIInpObj) $
                      mutHelper viIsInsertable relInsInpObjsM

    allTypes = relInsInpObjTys <> onConflictTypes <> jsonOpTys
               <> queryTypes <> aggQueryTypes <> mutationTypes

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
      ]

    mutHelper :: (ViewInfo -> Bool) -> Maybe a -> Maybe a
    mutHelper f objM = bool Nothing objM $ isMutable f viM

    fieldMap = Map.unions $ catMaybes
               [ insInpObjFldsM, updSetInpObjFldsM, boolExpInpObjFldsM
               , selObjFldsM, Just selByPKeyObjFlds
               ]

    -- helper
    mkColFldMap ty cols = Map.fromList $ flip map cols $
      \c -> ((ty, mkColName $ pgiName c), Left c)

    insCtxM = fst <$> insPermM
    insColsM = icColumns <$> insCtxM
    -- insert input type
    insInpObjM = mkInsInp tn <$> insCtxM
    -- column fields used in insert input object
    insInpObjFldsM = mkColFldMap (mkInsInpTy tn) <$> insColsM
    -- relationship input objects
    relInsInpObjsM = const (mkRelInsInps tn isUpsertable) <$> insCtxM
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

    -- helper
    mkFldMap ty = Map.fromList . concatMap (mkFld ty)
    mkFld ty = \case
      Left ci -> [((ty, mkColName $ pgiName ci), Left ci)]
      Right (ri, allowAgg, perm, lim, _) ->
        let relFld = ( (ty, G.Name $ getRelTxt $ riName ri)
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

    isMut = (isJust insColsM || isJust updColsM || isJust delPermM)
            && any (`isMutable` viM) [viIsInsertable, viIsUpdatable, viIsDeletable]

    -- table obj
    selObjM = mkTableObj tn <$> selFldsM
    -- aggregate objs and order by inputs
    (aggObjs, aggOrdByInps) = case selPermM of
      Just (True, selFlds) ->
        let numCols = (map pgiName . getNumCols) selFlds
            compCols = (map pgiName . getCompCols) selFlds
            objs = [ mkTableAggObj tn
                   , mkTableAggFldsObj tn numCols compCols
                   ] <> mkColAggFldsObjs selFlds
            ordByInps = mkTabAggOrdByInpObj tn numCols compCols
                        : mkTabAggOpOrdByInpObjs tn numCols compCols
        in (objs, ordByInps)
      _ -> ([], [])

    getNumCols = onlyNumCols . lefts
    getCompCols = onlyComparableCols . lefts
    onlyFloat = const $ mkScalarTy PGFloat

    mkTypeMaker "sum" = mkScalarTy
    mkTypeMaker _     = onlyFloat

    mkColAggFldsObjs flds =
      let numCols = getNumCols flds
          compCols = getCompCols flds
          mkNumObjFld n = mkTableColAggFldsObj tn n (mkTypeMaker n) numCols
          mkCompObjFld n = mkTableColAggFldsObj tn n mkScalarTy compCols
          numFldsObjs = bool (map mkNumObjFld numAggOps) [] $ null numCols
          compFldsObjs = bool (map mkCompObjFld compAggOps) [] $ null compCols
      in numFldsObjs <> compFldsObjs
    -- the fields used in table object
    selObjFldsM = mkFldMap (mkTableTy tn) <$> selFldsM
    -- the field used in table_by_pkey object
    selByPKeyObjFlds = Map.fromList $ flip map pkeyCols $
      \pgi@(PGColInfo col ty _) -> ((mkScalarTy ty, mkColName col), Left pgi)

    ordByInpCtxM = mkOrdByInpObj tn <$> selFldsM
    (ordByInpObjM, ordByCtxM) = case ordByInpCtxM of
      Just (a, b) -> (Just a, Just b)
      Nothing     -> (Nothing, Nothing)


getRootFldsRole'
  :: QualifiedTable
  -> [PGCol]
  -> [ConstraintName]
  -> FieldInfoMap
  -> Maybe ([T.Text], Bool) -- insert perm
  -> Maybe (AnnBoolExpSQL, Maybe Int, [T.Text], Bool) -- select filter
  -> Maybe ([PGCol], AnnBoolExpSQL, [T.Text]) -- update filter
  -> Maybe (AnnBoolExpSQL, [T.Text]) -- delete filter
  -> Maybe ViewInfo
  -> RootFlds
getRootFldsRole' tn primCols constraints fields insM selM updM delM viM =
  RootFlds mFlds
  where
    mFlds = mapFromL (either _fiName _fiName . snd) $ catMaybes
            [ mutHelper viIsInsertable getInsDet insM
            , mutHelper viIsUpdatable getUpdDet updM
            , mutHelper viIsDeletable getDelDet delM
            , getSelDet <$> selM, getSelAggDet selM
            , getPKeySelDet selM $ getColInfos primCols colInfos
            ]

    mutHelper :: (ViewInfo -> Bool) -> (a -> b) -> Maybe a -> Maybe b
    mutHelper f getDet mutM =
      bool Nothing (getDet <$> mutM) $ isMutable f viM

    colInfos = fst $ validPartitionFieldInfoMap fields
    getInsDet (hdrs, upsertPerm) =
      let isUpsertable = upsertable constraints upsertPerm $ isJust viM
      in ( OCInsert tn $ hdrs `union` maybe [] _3 updM
         , Right $ mkInsMutFld tn isUpsertable
         )

    getUpdDet (updCols, updFltr, hdrs) =
      ( OCUpdate tn updFltr hdrs
      , Right $ mkUpdMutFld tn $ getColInfos updCols colInfos
      )
    getDelDet (delFltr, hdrs) =
      (OCDelete tn delFltr hdrs, Right $ mkDelMutFld tn)
    getSelDet (selFltr, pLimit, hdrs, _) =
      (OCSelect tn selFltr pLimit hdrs, Left $ mkSelFld tn)

    getSelAggDet (Just (selFltr, pLimit, hdrs, True)) = Just
      (OCSelectAgg tn selFltr pLimit hdrs, Left $ mkAggSelFld tn)
    getSelAggDet _ = Nothing

    getPKeySelDet Nothing _ = Nothing
    getPKeySelDet _ [] = Nothing
    getPKeySelDet (Just (selFltr, _, hdrs, _)) pCols = Just
      (OCSelectPkey tn selFltr hdrs, Left $ mkSelFldPKey tn pCols)

-- getRootFlds
--   :: TableCache
--   -> Map.HashMap RoleName RootFlds
-- getRootFlds tables =
--   foldr (Map.unionWith mappend . getRootFldsTable) Map.empty $
--   Map.elems tables

-- gets all the selectable fields (cols and rels) of a
-- table for a role

getSelPermission :: TableInfo -> RoleName -> Maybe SelPermInfo
getSelPermission tabInfo role =
  Map.lookup role (tiRolePermInfoMap tabInfo) >>= _permSel

getSelPerm
  :: (MonadError QErr m)
  => TableCache
  -- all the fields of a table
  -> FieldInfoMap
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
  -> TableCache -> FieldInfoMap -> InsPermInfo -> Maybe UpdPermInfo -> m InsCtx
mkInsCtx role tableCache fields insPermInfo updPermM = do
  relTupsM <- forM rels $ \relInfo -> do
    let remoteTable = riRTable relInfo
        relName = riName relInfo
    remoteTableInfo <- getTabInfo tableCache remoteTable
    let insPermM = getInsPerm remoteTableInfo role
        viewInfoM = tiViewInfo remoteTableInfo
    return $ bool Nothing (Just (relName, relInfo)) $
      isInsertable insPermM viewInfoM

  let relInfoMap = Map.fromList $ catMaybes relTupsM
  return $ InsCtx iView cols setCols relInfoMap updPermForIns
  where
    cols = getValidCols fields
    rels = getValidRels fields
    iView = ipiView insPermInfo
    setCols = ipiSet insPermInfo
    mkUpdPermForIns upi =
      (Set.toList $ upiCols upi, upiFilter upi)
    updPermForIns = mkUpdPermForIns <$> updPermM

    isInsertable Nothing _          = False
    isInsertable (Just _) viewInfoM = isMutable viIsInsertable viewInfoM

mkAdminInsCtx
  :: MonadError QErr m
  => QualifiedTable -> TableCache -> FieldInfoMap -> m InsCtx
mkAdminInsCtx tn tc fields = do
  relTupsM <- forM rels $ \relInfo -> do
    let remoteTable = riRTable relInfo
        relName = riName relInfo
    remoteTableInfo <- getTabInfo tc remoteTable
    let viewInfoM = tiViewInfo remoteTableInfo
    return $ bool Nothing (Just (relName, relInfo)) $
      isMutable viIsInsertable viewInfoM

  let relInfoMap = Map.fromList $ catMaybes relTupsM
      updPerm = (map pgiName cols, noFilter)

  return $ InsCtx tn cols Map.empty relInfoMap $ Just updPerm
  where
    cols = getValidCols fields
    rels = getValidRels fields

mkGCtxRole
  :: (MonadError QErr m)
  => TableCache
  -> QualifiedTable
  -> FieldInfoMap
  -> [PGCol]
  -> [ConstraintName]
  -> Maybe ViewInfo
  -> RoleName
  -> RolePermInfo
  -> m (TyAgg, RootFlds, InsCtxMap)
mkGCtxRole tableCache tn fields pCols constraints viM role permInfo = do
  selPermM <- mapM (getSelPerm tableCache fields role) $ _permSel permInfo
  tabInsCtxM <- forM (_permIns permInfo) $ \ipi -> do
    tic <- mkInsCtx role tableCache fields ipi $ _permUpd permInfo
    return (tic, isJust $ _permUpd permInfo)
  let updColsM = filterColInfos . upiCols <$> _permUpd permInfo
      tyAgg = mkGCtxRole' tn tabInsCtxM selPermM updColsM
              (void $ _permDel permInfo) pColInfos constraints viM
      rootFlds = getRootFldsRole tn pCols constraints fields viM permInfo
      insCtxMap = maybe Map.empty (Map.singleton tn) $ fmap fst tabInsCtxM
  return (tyAgg, rootFlds, insCtxMap)
  where
    colInfos = getValidCols fields
    pColInfos = getColInfos pCols colInfos
    filterColInfos allowedSet =
      filter ((`Set.member` allowedSet) . pgiName) colInfos

getRootFldsRole
  :: QualifiedTable
  -> [PGCol]
  -> [ConstraintName]
  -> FieldInfoMap
  -> Maybe ViewInfo
  -> RolePermInfo
  -> RootFlds
getRootFldsRole tn pCols constraints fields viM (RolePermInfo insM selM updM delM) =
  getRootFldsRole' tn pCols constraints fields
  (mkIns <$> insM) (mkSel <$> selM)
  (mkUpd <$> updM) (mkDel <$> delM)
  viM
  where
    mkIns i = (ipiRequiredHeaders i, isJust updM)
    mkSel s = ( spiFilter s, spiLimit s
              , spiRequiredHeaders s, spiAllowAgg s
              )
    mkUpd u = ( Set.toList $ upiCols u
              , upiFilter u
              , upiRequiredHeaders u
              )
    mkDel d = (dpiFilter d, dpiRequiredHeaders d)

mkGCtxMapTable
  :: (MonadError QErr m)
  => TableCache
  -> TableInfo
  -> m (Map.HashMap RoleName (TyAgg, RootFlds, InsCtxMap))
mkGCtxMapTable tableCache (TableInfo tn _ fields rolePerms constraints pkeyCols viewInfo _) = do
  m <- Map.traverseWithKey
       (mkGCtxRole tableCache tn fields pkeyCols validConstraints viewInfo) rolePerms
  adminInsCtx <- mkAdminInsCtx tn tableCache fields
  let adminCtx = mkGCtxRole' tn (Just (adminInsCtx, True))
                 (Just (True, selFlds)) (Just colInfos) (Just ())
                 pkeyColInfos validConstraints viewInfo
      adminInsCtxMap = Map.singleton tn adminInsCtx
  return $ Map.insert adminRole (adminCtx, adminRootFlds, adminInsCtxMap) m
  where
    validConstraints = mkValidConstraints constraints
    colInfos = getValidCols fields
    allCols = map pgiName colInfos
    pkeyColInfos = getColInfos pkeyCols colInfos
    selFlds = flip map (toValidFieldInfos fields) $ \case
      FIColumn pgColInfo     -> Left pgColInfo
      FIRelationship relInfo -> Right (relInfo, True, noFilter, Nothing, isRelNullable fields relInfo)
    adminRootFlds =
      getRootFldsRole' tn pkeyCols validConstraints fields
      (Just ([], True)) (Just (noFilter, Nothing, [], True))
      (Just (allCols, noFilter, [])) (Just (noFilter, []))
      viewInfo

noFilter :: AnnBoolExpSQL
noFilter = annBoolExpTrue


checkSchemaConflicts
  :: (MonadError QErr m)
  => GCtx -> GCtx -> m ()
checkSchemaConflicts gCtx remoteCtx = do
  let typeMap     = _gTypes gCtx -- hasura typemap
  -- check type conflicts
  let hTypes      = Map.elems typeMap
      hTyNames    = map G.unNamedType $ Map.keys typeMap
      -- get the root names from the remote schema
      rmQRootName = _otiName $ _gQueryRoot remoteCtx
      rmMRootName = maybeToList $ _otiName <$> _gMutRoot remoteCtx
      rmSRootName = maybeToList $ _otiName <$> _gSubRoot remoteCtx
      rmRootNames = map G.unNamedType (rmQRootName:(rmMRootName ++ rmSRootName))
  let rmTypes     = Map.filterWithKey
                    (\k _ -> G.unNamedType k `notElem` builtinTy ++ rmRootNames)
                    $ _gTypes remoteCtx

      isTyInfoSame ty = any (`tyinfoEq` ty) hTypes
      -- name is same and structure is not same
      isSame n ty = G.unNamedType n `elem` hTyNames &&
                    not (isTyInfoSame ty)
      conflictedTypes = Map.filterWithKey isSame rmTypes
      conflictedTyNames = map G.unNamedType $ Map.keys conflictedTypes

  unless (Map.null conflictedTypes) $
    throw400 RemoteSchemaConflicts $ tyMsg conflictedTyNames

  -- check node conflicts
  let rmQRoot = _otiFields $ _gQueryRoot remoteCtx
      rmMRoot = _otiFields <$> _gMutRoot remoteCtx
      rmRoots = filter (`notElem` builtinNodes ++ rmRootNames) . Map.keys <$>
                mergeMaybeMaps (Just rmQRoot) rmMRoot
      hQR     = _otiFields <$>
                join (getObjTyM <$> Map.lookup hQRName typeMap)
      hMR     = _otiFields <$>
                join (getObjTyM <$> Map.lookup hMRName typeMap)
      hRoots  = Map.keys <$> mergeMaybeMaps hQR hMR

  case (rmRoots, hRoots) of
    (Just rmR, Just hR) -> do
      let conflictedNodes = filter (`elem` hR) rmR
      unless (null conflictedNodes) $
        throw400 RemoteSchemaConflicts $ nodesMsg conflictedNodes
    _ -> return ()

  where
    tyinfoEq a b = case (a, b) of
      (TIScalar t1, TIScalar t2) -> typeEq t1 t2
      (TIObj t1, TIObj t2)       -> typeEq t1 t2
      (TIEnum t1, TIEnum t2)     -> typeEq t1 t2
      (TIInpObj t1, TIInpObj t2) -> typeEq t1 t2
      _                          -> False

    hQRName = G.NamedType "query_root"
    hMRName = G.NamedType "mutation_root"
    tyMsg ty = "types: [" <> namesToTxt ty <>
               "] already exist in current graphql schema"
    nodesMsg n = "nodes : [" <> namesToTxt n <>
                 "] already exist in current graphql schema"
    namesToTxt = T.intercalate ", " . map G.unName
    builtinNodes = ["__type", "__schema", "__typename"]
    builtinTy = [ "__Directive"
                , "__DirectiveLocation"
                , "__EnumValue"
                , "__Field"
                , "__InputValue"
                , "__Schema"
                , "__Type"
                , "__TypeKind"
                , "Int"
                , "Float"
                , "String"
                , "Boolean"
                , "ID"
                ]

checkConflictingNode
  :: (MonadError QErr m)
  => GCtx
  -> G.Name
  -> m ()
checkConflictingNode gCtx node = do
  let typeMap = _gTypes gCtx
      hQR     = _otiFields <$>
                join (getObjTyM <$> Map.lookup hQRName typeMap)
      hMR     = _otiFields <$>
                join (getObjTyM <$> Map.lookup hMRName typeMap)
      hRoots  = Map.keys <$> mergeMaybeMaps hQR hMR
  case hRoots of
    Just hR ->
      when (node `elem` hR) $
        throw400 RemoteSchemaConflicts msg
    _ -> return ()
  where
    hQRName = G.NamedType "query_root"
    hMRName = G.NamedType "mutation_root"
    msg = "node " <> G.unName node <>
          " already exists in current graphql schema"


mkGCtxMap
  :: (MonadError QErr m)
  => TableCache -> m GCtxMap
mkGCtxMap tableCache = do
  typesMapL <- mapM (mkGCtxMapTable tableCache) $
               filter tableFltr $ Map.elems tableCache
  let typesMap = foldr (Map.unionWith mappend) Map.empty typesMapL
  return $ flip Map.map typesMap $ \(ty, flds, insCtxMap) ->
    mkGCtx ty flds insCtxMap
  where
    tableFltr ti = not (tiSystemDefined ti)
                   && isValidTableName (tiName ti)


-- mkGCtx :: TyAgg -> RootFlds -> InsCtxMap -> GCtx
-- mkGCtx (TyAgg tyInfos fldInfos ordByEnums) (RootFlds flds) insCtxMap =
--   let queryRoot = mkHsraObjTyInfo (Just "query root")
--                   (G.NamedType "query_root") $
--                   mapFromL _fiName (schemaFld:typeFld:qFlds)
--       colTys    = Set.toList $ Set.fromList $ map pgiType $
--                   lefts $ Map.elems fldInfos
--       scalarTys = map (TIScalar . mkHsraScalarTyInfo) colTys
--       compTys   = map (TIInpObj . mkCompExpInp) colTys
--       ordByEnumTyM = bool (Just ordByEnumTy) Nothing $ null qFlds
--       allTys    = Map.union tyInfos $ mkTyInfoMap $
--                   catMaybes [ Just $ TIObj queryRoot
--                             , TIObj <$> mutRootM
--                             , TIObj <$> subRootM
--                             , TIEnum <$> ordByEnumTyM
--                             ] <>
--                   scalarTys <> compTys <> defaultTypes
--   -- for now subscription root is query root
--   in GCtx allTys fldInfos ordByEnums queryRoot mutRootM (Just queryRoot)
--      (Map.map fst flds) insCtxMap
--   where

--     mkMutRoot =
--       mkHsraObjTyInfo (Just "mutation root") (G.NamedType "mutation_root") .
--       mapFromL _fiName

--     mutRootM = bool (Just $ mkMutRoot mFlds) Nothing $ null mFlds

--     mkSubRoot =
--       mkHsraObjTyInfo (Just "subscription root")
--       (G.NamedType "subscription_root") . mapFromL _fiName

--     subRootM = bool (Just $ mkSubRoot qFlds) Nothing $ null qFlds

--     (qFlds, mFlds) = partitionEithers $ map snd $ Map.elems flds

--     schemaFld = mkHsraObjFldInfo Nothing "__schema" Map.empty $
--                   G.toGT $ G.toNT $ G.NamedType "__Schema"

--     typeFld = mkHsraObjFldInfo Nothing "__type" typeFldArgs $
--                 G.toGT $ G.NamedType "__Type"
--       where
--         typeFldArgs = mapFromL _iviName [
--           InpValInfo (Just "name of the type") "name"
--           $ G.toGT $ G.toNT $ G.NamedType "String"
--           ]


-- emptyGCtx :: GCtx
-- emptyGCtx = mkGCtx mempty mempty mempty

getGCtx :: (CacheRM m) => RoleName -> GCtxMap -> m GCtx
getGCtx rn ctxMap = do
  sc <- askSchemaCache
  return $ fromMaybe (scDefaultRemoteGCtx sc) $ Map.lookup rn ctxMap

mergeMaybeMaps
  :: (Eq k, Hashable k)
  => Maybe (Map.HashMap k v)
  -> Maybe (Map.HashMap k v)
  -> Maybe (Map.HashMap k v)
mergeMaybeMaps m1 m2 = case (m1, m2) of
  (Nothing, Nothing)   -> Nothing
  (Just m1', Nothing)  -> Just m1'
  (Nothing, Just m2')  -> Just m2'
  (Just m1', Just m2') -> Just $ Map.union m1' m2'


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
