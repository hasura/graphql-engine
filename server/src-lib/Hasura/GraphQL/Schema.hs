module Hasura.GraphQL.Schema
  ( mkGCtxMap
  , GCtxMap
  , buildGCtxMapPG
  , getGCtx
  , GCtx(..)
  , OpCtx(..)
  , InsCtx(..)
  , InsCtxMap
  , RelationInfoMap
  , isAggFld
  , qualObjectToName
  -- Schema stitching related
  , checkSchemaConflicts
  , checkConflictingNode
  , emptyGCtx
  , mergeMaybeMaps
  , ppGCtx
  ) where


import           Data.Maybe                          (maybeToList)

import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashSet                        as Set
import qualified Data.Sequence                       as Seq

import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.ContextTypes
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal             (mkAdminRolePermInfo)
import           Hasura.RQL.Types
import           Hasura.SQL.Types


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

data ColField
  = ColField
  { _cfPGCol :: !PGColInfo
  , _cfName  :: !G.Name
  } deriving (Show, Eq)

onlyIntCols :: [ColField] -> [ColField]
onlyIntCols = filter (isIntegerType . pgiType . _cfPGCol)

onlyNumCols :: [ColField] -> [ColField]
onlyNumCols = filter (isNumType . pgiType . _cfPGCol)

onlyJSONBCols :: [ColField] -> [ColField]
onlyJSONBCols = filter (isJSONBType . pgiType . _cfPGCol)

onlyComparableCols :: [ColField] -> [ColField]
onlyComparableCols = filter (isComparableType . pgiType . _cfPGCol)

type SelField = Either ColField (RelInfo, Bool, PGColGNameMap, AnnBoolExpPartialSQL, Maybe Int, Bool)

qualObjectToName :: (ToTxt a) => QualifiedObject a -> G.Name
qualObjectToName = G.Name . snakeCaseQualObject

isValidObjectName :: (ToTxt a) => QualifiedObject a -> Bool
isValidObjectName = isValidName . qualObjectToName

isValidCol :: ColField -> Bool
isValidCol = isValidName . _cfName

isValidRel :: ToTxt a => RelName -> QualifiedObject a -> Bool
isValidRel rn rt = isValidName (mkRelName rn) && isValidObjectName rt

upsertable :: [ConstraintName] -> Bool -> Bool -> Bool
upsertable uniqueOrPrimaryCons isUpsertAllowed view =
  not (null uniqueOrPrimaryCons) && isUpsertAllowed && not view

getValidCols :: FieldInfoMap -> Maybe CustomColFields -> [ColField]
getValidCols fim = filter isValidCol . mkColumnFields cols
  where
    cols = fst $ partitionFieldInfos $ Map.elems fim

getValidRels :: FieldInfoMap -> [RelInfo]
getValidRels = filter isValidRel' . snd . partitionFieldInfos . Map.elems
  where
    isValidRel' (RelInfo rn _ _ remTab _) = isValidRel rn remTab

mkValidConstraints :: [ConstraintName] -> [ConstraintName]
mkValidConstraints =
  filter (isValidName . G.Name . getConstraintTxt)

isRelNullable :: FieldInfoMap -> RelInfo -> Maybe CustomColFields -> Bool
isRelNullable fim ri ccfM = isNullable
  where
    lCols = map fst $ riMapping ri
    allCols = getValidCols fim ccfM
    lColInfos = getColInfos lCols $ map _cfPGCol allCols
    isNullable = any pgiIsNullable lColInfos

mkPGColGNameMap :: [ColField] -> PGColGNameMap
mkPGColGNameMap colFlds = Map.fromList $
  flip map colFlds $ \(ColField ci name) -> (name, ci)

numAggOps :: [G.Name]
numAggOps = [ "sum", "avg", "stddev", "stddev_samp", "stddev_pop"
            , "variance", "var_samp", "var_pop"
            ]

compAggOps :: [G.Name]
compAggOps = ["max", "min"]

isAggFld :: G.Name -> Bool
isAggFld = flip elem (numAggOps <> compAggOps)

mkRelName :: RelName -> G.Name
mkRelName rn = G.Name $ relNameToTxt rn

mkAggRelName :: RelName -> G.Name
mkAggRelName rn = G.Name $ relNameToTxt rn <> "_aggregate"

mkBoolExpName :: QualifiedTable -> G.Name
mkBoolExpName tn =
  qualObjectToName tn <> "_bool_exp"

mkBoolExpTy :: QualifiedTable -> G.NamedType
mkBoolExpTy =
  G.NamedType . mkBoolExpName

mkFuncArgsName :: QualifiedFunction -> G.Name
mkFuncArgsName fn =
  qualObjectToName fn <> "_args"

mkFuncArgsTy :: QualifiedFunction -> G.NamedType
mkFuncArgsTy =
  G.NamedType . mkFuncArgsName

mkTableTy :: QualifiedTable -> G.NamedType
mkTableTy =
  G.NamedType . qualObjectToName

mkTableAggTy :: QualifiedTable -> G.NamedType
mkTableAggTy tn =
  G.NamedType $ qualObjectToName tn <> "_aggregate"

mkTableAggFldsTy :: QualifiedTable -> G.NamedType
mkTableAggFldsTy tn =
  G.NamedType $ qualObjectToName tn <> "_aggregate_fields"

mkTableColAggFldsTy :: G.Name -> QualifiedTable -> G.NamedType
mkTableColAggFldsTy op tn =
  G.NamedType $ qualObjectToName tn <> "_" <> op <> "_fields"

mkTableByPkName :: QualifiedTable -> G.Name
mkTableByPkName tn = qualObjectToName tn <> "_by_pk"

-- Support argument params for PG columns
mkPGColParams :: PGColType -> ParamMap
mkPGColParams = \case
  PGJSONB -> jsonParams
  PGJSON  -> jsonParams
  _       -> Map.empty
  where
    pathDesc = "JSON select path"
    jsonParams = Map.fromList
      [ (G.Name "path", InpValInfo (Just pathDesc) "path" Nothing $ G.toGT $ mkScalarTy PGText)
      ]

mkPGColFld :: ColField -> ObjFldInfo
mkPGColFld (ColField colInfo name) =
  mkHsraObjFldInfo Nothing name (mkPGColParams colTy) ty
  where
    PGColInfo _ colTy isNullable = colInfo
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
      (mkRelName rn) Map.empty objRelTy
    objRelTy = bool (G.toGT $ G.toNT relTabTy) (G.toGT relTabTy) isObjRelNullable
    isObjRelNullable = isManual || isNullable
    relTabTy = mkTableTy remTab

    arrRelFld =
      mkHsraObjFldInfo (Just "An array relationship") (mkRelName rn)
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
  mkObjTyInfo (Just desc) (mkTableTy tn) Set.empty (mapFromL _fiName flds) TLHasuraType
  where
    flds = concatMap (either (pure . mkPGColFld) mkRelFld') allowedFlds
    mkRelFld' (relInfo, allowAgg, _, _, _, isNullable) =
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
  mkHsraObjTyInfo (Just desc) (mkTableAggTy tn) Set.empty $ mapFromL _fiName
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
  :: QualifiedTable -> [ColField] -> [ColField] -> ObjTyInfo
mkTableAggFldsObj tn numCols compCols =
  mkHsraObjTyInfo (Just desc) (mkTableAggFldsTy tn) Set.empty $ mapFromL _fiName $
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
  -> [ColField]
  -> ObjTyInfo
mkTableColAggFldsObj tn op f cols =
  mkHsraObjTyInfo (Just desc) (mkTableColAggFldsTy op tn) Set.empty $ mapFromL _fiName $
  map mkColObjFld cols
  where
    desc = G.Description $ "aggregate " <> G.unName op <> " on columns"

    mkColObjFld (ColField ci name) =
      mkHsraObjFldInfo Nothing name Map.empty $ G.toGT $ f $ pgiType ci

{-

table(
  where: table_bool_exp
  limit: Int
  offset: Int
):  [table!]!

-}
mkSelFld :: Maybe G.Name -> QualifiedTable -> ObjFldInfo
mkSelFld mCustomName tn =
  mkHsraObjFldInfo (Just desc) fldName args ty
  where
    desc    = G.Description $ "fetch data from the table: " <>> tn
    fldName = fromMaybe (qualObjectToName tn) mCustomName
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
mkSelFldPKey :: Maybe G.Name -> QualifiedTable -> [ColField] -> ObjFldInfo
mkSelFldPKey mCustomName tn cols =
  mkHsraObjFldInfo (Just desc) fldName args ty
  where
    desc = G.Description $ "fetch data from the table: " <> tn
           <<> " using primary key columns"
    fldName = fromMaybe (mkTableByPkName tn) mCustomName
    args = fromInpValL $ map colInpVal cols
    ty = G.toGT $ mkTableTy tn
    colInpVal (ColField ci name) =
      InpValInfo Nothing name Nothing $ G.toGT $ G.toNT $ mkScalarTy $ pgiType ci

{-

table_aggregate(
  where: table_bool_exp
  limit: Int
  offset: Int
): table_aggregate!

-}
mkAggSelFld
  :: Maybe G.Name -> QualifiedTable -> ObjFldInfo
mkAggSelFld mCustomName tn =
  mkHsraObjFldInfo (Just desc) fldName args ty
  where
    desc = G.Description $ "fetch aggregated fields from the table: "
           <>> tn
    defFldName = qualObjectToName tn <> "_aggregate"
    fldName = fromMaybe defFldName mCustomName
    args = fromInpValL $ mkSelArgs tn
    ty = G.toGT $ G.toNT $ mkTableAggTy tn

{-

function(
  args: function_args
  where: table_bool_exp
  limit: Int
  offset: Int
): [table!]!

-}

mkFuncArgs :: FunctionInfo -> ParamMap
mkFuncArgs funInfo =
  fromInpValL $ funcInpArgs <> mkSelArgs retTable
  where
    funcName = fiName funInfo
    funcArgs = fiInputArgs funInfo
    retTable = fiReturnType funInfo

    funcArgDesc = G.Description $ "input parameters for function " <>> funcName
    funcInpArg = InpValInfo (Just funcArgDesc) "args" Nothing $ G.toGT $ G.toNT $
                 mkFuncArgsTy funcName
    funcInpArgs = bool [funcInpArg] [] $ null funcArgs

mkFuncQueryFld
  :: FunctionInfo -> ObjFldInfo
mkFuncQueryFld funInfo =
  mkHsraObjFldInfo (Just desc) fldName (mkFuncArgs funInfo) ty
  where
    retTable = fiReturnType funInfo
    funcName = fiName funInfo

    desc = G.Description $ "execute function " <> funcName
           <<> " which returns " <>> retTable
    fldName = qualObjectToName funcName

    ty      = G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableTy retTable

{-

function_aggregate(
  args: function_args
  where: table_bool_exp
  limit: Int
  offset: Int
): table_aggregate!

-}

mkFuncAggQueryFld
  :: FunctionInfo -> ObjFldInfo
mkFuncAggQueryFld funInfo =
  mkHsraObjFldInfo (Just desc) fldName (mkFuncArgs funInfo) ty
  where
    funcName = fiName funInfo
    retTable = fiReturnType funInfo

    desc = G.Description $ "execute function " <> funcName
           <<> " and query aggregates on result of table type "
           <>> retTable

    fldName = qualObjectToName funcName <> "_aggregate"

    ty = G.toGT $ G.toNT $ mkTableAggTy retTable


-- table_mutation_response
mkMutRespTy :: QualifiedTable -> G.NamedType
mkMutRespTy tn =
  G.NamedType $ qualObjectToName tn <> "_mutation_response"

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
  mkHsraObjTyInfo (Just objDesc) (mkMutRespTy tn) Set.empty $ mapFromL _fiName
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

-- table_bool_exp
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
      Left (ColField (PGColInfo _ colTy _) name) ->
        mk name (mkCompExpTy colTy)
      Right (RelInfo relName _ _ remTab _, _, _, _, _, _) ->
        mk (mkRelName relName) (mkBoolExpTy remTab)

mkPGColInp :: ColField -> InpValInfo
mkPGColInp (ColField ci name) =
  InpValInfo Nothing name Nothing $ G.toGT $ mkScalarTy $ pgiType ci

{-
input function_args {
  arg1: arg-type1!
  .     .
  .     .
  argn: arg-typen!
}
-}

procFuncArgs
  :: Seq.Seq FunctionArg
  -> (PGColType -> Text -> a) -> [a]
procFuncArgs argSeq f =
  fst $ foldl mkItem ([], 1::Int) argSeq
  where
    mkItem (items, argNo) (FunctionArg nameM ty) =
      case nameM of
        Just argName ->
          let argT = getFuncArgNameTxt argName
          in (items <> pure (f ty argT), argNo)
        Nothing ->
          let argT = "arg_" <> T.pack (show argNo)
          in (items <> pure (f ty argT), argNo + 1)

mkFuncArgsInp :: FunctionInfo -> Maybe InpObjTyInfo
mkFuncArgsInp funcInfo =
  bool (Just inpObj) Nothing $ null funcArgs
  where
    funcName = fiName funcInfo
    funcArgs = fiInputArgs funcInfo
    funcArgsTy = mkFuncArgsTy funcName

    inpObj = mkHsraInpTyInfo Nothing funcArgsTy $
             fromInpValL argInps

    argInps = procFuncArgs funcArgs mkInpVal

    mkInpVal ty t =
      InpValInfo Nothing (G.Name t) Nothing $
      G.toGT $ mkScalarTy ty

-- table_set_input
mkUpdSetTy :: QualifiedTable -> G.NamedType
mkUpdSetTy tn =
  G.NamedType $ qualObjectToName tn <> "_set_input"

{-
input table_set_input {
  col1: colty1
  .
  .
  coln: coltyn
}
-}
mkUpdSetInp
  :: QualifiedTable -> [ColField] -> InpObjTyInfo
mkUpdSetInp tn cols  =
  mkHsraInpTyInfo (Just desc) (mkUpdSetTy tn) $
    fromInpValL $ map mkPGColInp cols
  where
    desc = G.Description $
      "input type for updating data in table " <>> tn

-- table_inc_input
mkUpdIncTy :: QualifiedTable -> G.NamedType
mkUpdIncTy tn =
  G.NamedType $ qualObjectToName tn <> "_inc_input"

{-
input table_inc_input {
  integer-col1: int
  .
  .
  integer-coln: int
}
-}

mkUpdIncInp
  :: QualifiedTable -> Maybe [ColField] -> Maybe InpObjTyInfo
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
  G.NamedType $ qualObjectToName tn <> op <> "_input"

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
  :: QualifiedTable -> [ColField] -> [InpObjTyInfo]
mkUpdJSONOpInp tn cols = bool inpObjs [] $ null jsonbCols
  where
    jsonbCols = onlyJSONBCols cols
    jsonbColNames = map _cfName jsonbCols

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
    deleteKeyInpVal n =
      InpValInfo Nothing n Nothing $ G.toGT $ G.NamedType "String"

    deleteElemInpObj =
      mkHsraInpTyInfo (Just deleteElemDesc) (mkJSONOpTy tn deleteElemOp) $
      fromInpValL $ map deleteElemInpVal jsonbColNames
    deleteElemInpVal n =
      InpValInfo Nothing n Nothing $ G.toGT $ G.NamedType "Int"

    deleteAtPathInpObj =
      mkHsraInpTyInfo (Just deleteAtPathDesc) (mkJSONOpTy tn deleteAtPathOp) $
      fromInpValL $ map deleteAtPathInpVal jsonbColNames
    deleteAtPathInpVal n =
      InpValInfo Nothing n Nothing $ G.toGT $ G.toLT $ G.NamedType "String"

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

mkIncInpVal :: QualifiedTable -> [ColField] -> Maybe InpValInfo
mkIncInpVal tn cols = bool (Just incArg) Nothing $ null intCols
  where
    intCols = onlyIntCols cols
    incArgDesc = "increments the integer columns with given value of the filtered values"
    incArg =
      InpValInfo (Just incArgDesc) "_inc" Nothing $ G.toGT $ mkUpdIncTy tn

mkJSONOpInpVals :: QualifiedTable -> [ColField] -> [InpValInfo]
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

mkUpdMutFld :: Maybe G.Name -> QualifiedTable -> [ColField] -> ObjFldInfo
mkUpdMutFld mCustomName tn cols =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL inputValues) $
    G.toGT $ mkMutRespTy tn
  where
    inputValues = [filterArg, setArg] <> incArg
                  <> mkJSONOpInpVals tn cols
    desc = G.Description $ "update data of the table: " <>> tn

    defFldName = "update_" <> qualObjectToName tn
    fldName = fromMaybe defFldName mCustomName

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

mkDelMutFld :: Maybe G.Name -> QualifiedTable -> ObjFldInfo
mkDelMutFld mCustomName tn =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL [filterArg]) $
    G.toGT $ mkMutRespTy tn
  where
    desc = G.Description $ "delete data from the table: " <>> tn

    defFldName = "delete_" <> qualObjectToName tn
    fldName = fromMaybe defFldName mCustomName

    filterArgDesc = "filter the rows which have to be deleted"
    filterArg =
      InpValInfo (Just filterArgDesc) "where" Nothing $ G.toGT $
      G.toNT $ mkBoolExpTy tn

-- table_insert_input
mkInsInpTy :: QualifiedTable -> G.NamedType
mkInsInpTy tn =
  G.NamedType $ qualObjectToName tn <> "_insert_input"

-- table_obj_rel_insert_input
mkObjInsInpTy :: QualifiedTable -> G.NamedType
mkObjInsInpTy tn =
  G.NamedType $ qualObjectToName tn <> "_obj_rel_insert_input"

-- table_arr_rel_insert_input
mkArrInsInpTy :: QualifiedTable -> G.NamedType
mkArrInsInpTy tn =
  G.NamedType $ qualObjectToName tn <> "_arr_rel_insert_input"


-- table_on_conflict
mkOnConflictInpTy :: QualifiedTable -> G.NamedType
mkOnConflictInpTy tn =
  G.NamedType $ qualObjectToName tn <> "_on_conflict"

-- table_constraint
mkConstraintInpTy :: QualifiedTable -> G.NamedType
mkConstraintInpTy tn =
  G.NamedType $ qualObjectToName tn <> "_constraint"

-- conflict_action
conflictActionTy :: G.NamedType
conflictActionTy = G.NamedType "conflict_action"

-- table_update_column
mkUpdColumnInpTy :: QualifiedTable -> G.NamedType
mkUpdColumnInpTy tn =
  G.NamedType $ qualObjectToName tn <> "_update_column"

--table_select_column
mkSelColumnInpTy :: QualifiedTable -> G.NamedType
mkSelColumnInpTy tn =
  G.NamedType $ qualObjectToName tn <> "_select_column"

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
  :: QualifiedTable -> [ColField] -> RelationInfoMap -> InpObjTyInfo
mkInsInp tn insCols relInfoMap =
  mkHsraInpTyInfo (Just desc) (mkInsInpTy tn) $ fromInpValL $
  map mkPGColInp insCols <> relInps
  where
    desc = G.Description $
      "input type for inserting data into table " <>> tn

    relInps = flip map (Map.toList relInfoMap) $
      \(relName, relInfo) ->
         let remoteQT = riRTable relInfo
             tyMaker = case riType relInfo of
               ObjRel -> mkObjInsInpTy
               ArrRel -> mkArrInsInpTy
         in  InpValInfo Nothing (mkRelName relName) Nothing $
               G.toGT $ tyMaker remoteQT


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

mkInsMutFld :: Maybe G.Name -> QualifiedTable -> Bool -> ObjFldInfo
mkInsMutFld mCustomName tn isUpsertable =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL inputVals) $
    G.toGT $ mkMutRespTy tn
  where
    inputVals = catMaybes [Just objectsArg , onConflictInpVal]
    desc = G.Description $
      "insert data into the table: " <>> tn

    defFldName = "insert_" <> qualObjectToName tn
    fldName = fromMaybe defFldName mCustomName

    objsArgDesc = "the rows to be inserted"
    objectsArg =
      InpValInfo (Just objsArgDesc) "objects" Nothing $ G.toGT $
      G.toNT $ G.toLT $ G.toNT $ mkInsInpTy tn

    onConflictInpVal = bool Nothing (Just onConflictArg) isUpsertable

    onConflictDesc = "on conflict condition"
    onConflictArg = InpValInfo (Just onConflictDesc) "on_conflict"
                    Nothing $ G.toGT $ mkOnConflictInpTy tn

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

mkColumnEnumVal :: G.Name -> EnumValInfo
mkColumnEnumVal colName =
  EnumValInfo (Just "column name") (G.EnumValue colName) False

mkUpdColumnTy :: QualifiedTable -> [G.Name] -> EnumTyInfo
mkUpdColumnTy tn cols = enumTyInfo
  where
    enumTyInfo = mkHsraEnumTyInfo (Just desc) (mkUpdColumnInpTy tn) $
                 mapFromL _eviVal $ map mkColumnEnumVal cols

    desc = G.Description $
      "update columns of table " <>> tn

mkSelColumnTy :: QualifiedTable -> [G.Name] -> EnumTyInfo
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
  G.NamedType $ qualObjectToName tn <> "_" <> op <> "_order_by"

{-
input table_<op>_order_by {
  col1: order_by
  .     .
  .     .
}
-}

mkTabAggOpOrdByInpObjs
  :: QualifiedTable -> [ColField] -> [ColField] -> [InpObjTyInfo]
mkTabAggOpOrdByInpObjs tn numCols compCols =
  mapMaybe (mkInpObjTyM numCols) numAggOps
  <> mapMaybe (mkInpObjTyM compCols) compAggOps
  where

    mkDesc (G.Name op) = G.Description $ "order by " <> op <> "() on columns of table " <>> tn

    mkInpObjTyM cols op = bool (Just $ mkInpObjTy cols op) Nothing $ null cols
    mkInpObjTy cols op = mkHsraInpTyInfo (Just $ mkDesc op) (mkTabAggOpOrdByTy tn op) $
                         fromInpValL $ map mkColInpVal cols

    mkColInpVal cf =
      InpValInfo Nothing (_cfName cf) Nothing $ G.toGT ordByTy

mkTabAggOrdByTy :: QualifiedTable -> G.NamedType
mkTabAggOrdByTy tn =
  G.NamedType $ qualObjectToName tn <> "_aggregate_order_by"

{-
input table_aggregate_order_by {
count: order_by
  <op-name>: table_<op-name>_order_by
}
-}

mkTabAggOrdByInpObj
  :: QualifiedTable -> [ColField] -> [ColField] -> InpObjTyInfo
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
  G.NamedType $ qualObjectToName tn <> "_order_by"

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
      map mkColOrdBy pgColFlds <> map mkObjRelOrdBy objRels
      <> mapMaybe mkArrRelAggOrdBy arrRels

    namedTy = mkOrdByTy tn
    desc = G.Description $
      "ordering options when selecting data from " <>> tn

    pgColFlds = lefts selFlds
    relFltr ty = flip filter (rights selFlds) $ \(ri, _, _, _, _, _) ->
      riType ri == ty
    objRels = relFltr ObjRel
    arrRels = relFltr ArrRel

    mkColOrdBy (ColField _ name) =
      InpValInfo Nothing name Nothing $ G.toGT ordByTy
    mkObjRelOrdBy (ri, _, _, _, _, _) =
      InpValInfo Nothing (mkRelName $ riName ri) Nothing $
      G.toGT $ mkOrdByTy $ riRTable ri

    mkArrRelAggOrdBy (ri, isAggAllowed, _, _, _, _) =
      let ivi = InpValInfo Nothing (mkAggRelName $ riName ri) Nothing $
            G.toGT $ mkTabAggOrdByTy $ riRTable ri
      in bool Nothing (Just ivi) isAggAllowed

    ordByCtx = Map.singleton namedTy $ Map.fromList $
               colOrdBys <> relOrdBys <> arrRelOrdBys
    colOrdBys = flip map pgColFlds $ \(ColField ci name) ->
                                    ( name
                                    , OBIPGCol ci
                                    )
    relOrdBys = flip map objRels $ \(ri, _, _, fltr, _, _) ->
                                     ( mkRelName $ riName ri
                                     , OBIRel ri fltr
                                     )
    arrRelOrdBys = flip mapMaybe arrRels $
                   \(ri, isAggAllowed, colGNameMap, fltr, _, _) ->
                     let obItem = ( mkAggRelName $ riName ri
                                  , OBIAgg ri colGNameMap fltr
                                  )
                     in bool Nothing (Just obItem) isAggAllowed

mkOnConflictTypes
  :: QualifiedTable -> [ConstraintName] -> [G.Name] -> Bool -> [TypeInfo]
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
  -- insert permission
  -> Maybe ([ColField], RelationInfoMap)
  -- select permission
  -> Maybe (Bool, [SelField])
  -- update cols
  -> Maybe [ColField]
  -- delete cols
  -> Maybe ()
  -- primary key columns
  -> [PGColInfo]
  -- constraints
  -> [ConstraintName]
  -> Maybe ViewInfo
  -- all functions
  -> [FunctionInfo]
  -> TyAgg
mkGCtxRole' tn insPermM selPermM updColFldsM
            delPermM pkeyCols constraints viM funcs =

  TyAgg (mkTyInfoMap allTypes) fieldMap scalars ordByCtx
  where

    ordByCtx = fromMaybe Map.empty ordByCtxM
    updColsM = map _cfPGCol <$> updColFldsM
    upsertPerm = isJust updColFldsM
    isUpsertable = upsertable constraints upsertPerm $ isJust viM
    updatableCols = maybe [] (map _cfName) updColFldsM
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
      ]

    mutHelper :: (ViewInfo -> Bool) -> Maybe a -> Maybe a
    mutHelper f objM = bool Nothing objM $ isMutable f viM

    fieldMap = Map.unions $ catMaybes
               [ insInpObjFldsM, updSetInpObjFldsM
               , boolExpInpObjFldsM , selObjFldsM
               ]
    scalars = Set.unions [selByPkScalarSet, funcArgScalarSet]

    -- helper
    mkColFldMap ty cols = Map.fromList $ flip map cols $
      \(ColField ci name) -> ((ty, name), Left ci)

    -- insert input type
    insInpObjM = uncurry (mkInsInp tn) <$> insPermM
    -- column fields used in insert input object
    insInpObjFldsM = (mkColFldMap (mkInsInpTy tn) . fst) <$> insPermM
    -- relationship input objects
    relInsInpObjsM = const (mkRelInsInps tn isUpsertable) <$> insPermM
    -- update set input type
    updSetInpObjM = mkUpdSetInp tn <$> updColFldsM
    -- update increment input type
    updIncInpObjM = mkUpdIncInp tn updColFldsM
    -- update json operator input type
    updJSONOpInpObjsM = mkUpdJSONOpInp tn <$> updColFldsM
    updJSONOpInpObjTysM = map TIInpObj <$> updJSONOpInpObjsM
    -- fields used in set input object
    updSetInpObjFldsM = mkColFldMap (mkUpdSetTy tn) <$> updColFldsM

    selFldsM = snd <$> selPermM
    selColNamesM = (map _cfName . lefts) <$> selFldsM
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
    funcArgInpObjs = mapMaybe mkFuncArgsInp funcs
    -- funcArgCtx = Map.unions funcArgCtxs
    funcArgScalarSet = Set.fromList $
                       concatMap (map faType . toList . fiInputArgs) funcs

    -- helper
    mkFldMap ty = Map.fromList . concatMap (mkFld ty)
    mkFld ty = \case
      Left (ColField ci name) -> [((ty, name), Left ci)]
      Right (ri, allowAgg, colGNameMap, perm, lim, _) ->
        let relFld = ( (ty, mkRelName $ riName ri)
                     , Right $ RelFld ri False colGNameMap perm lim
                     )
            aggRelFld = ( (ty, mkAggRelName $ riName ri)
                        , Right $ RelFld ri True colGNameMap perm lim
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
        let colFlds = lefts selFlds
            numCols = onlyNumCols colFlds
            compCols = onlyComparableCols colFlds
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
    -- the scalar set for table_by_pk arguments
    selByPkScalarSet = Set.fromList $ map pgiType pkeyCols

    ordByInpCtxM = mkOrdByInpObj tn <$> selFldsM
    (ordByInpObjM, ordByCtxM) = case ordByInpCtxM of
      Just (a, b) -> (Just a, Just b)
      Nothing     -> (Nothing, Nothing)

getRootFldsRole'
  :: QualifiedTable
  -> [ColField]
  -> [ConstraintName]
  -> FieldInfoMap
  -> [FunctionInfo]
  -> Maybe ([T.Text], Bool) -- insert perm
  -> Maybe (AnnBoolExpPartialSQL, Maybe Int, [T.Text], Bool) -- select filter
  -> Maybe ([ColField], PreSetColsPartial, AnnBoolExpPartialSQL, [T.Text]) -- update filter
  -> Maybe (AnnBoolExpPartialSQL, [T.Text]) -- delete filter
  -> Maybe ViewInfo
  -> Maybe TableConfig -- custom config
  -> RootFlds
getRootFldsRole' tn primCols constraints fields funcs insM
  selM updM delM viM mTableConfig = RootFlds mFlds
  where
    mCustomRootFields = _tcCustomRootFields <$> mTableConfig
    mCustomColFields = _tcCustomColumnFields <$> mTableConfig
    allCols = getCols fields
    colGNameMap = mkPGColGNameMap $ getValidCols fields mCustomColFields
    mFlds = mapFromL (either _fiName _fiName . snd) $
      funcQueries <>
      funcAggQueries <>
      catMaybes
            [ mutHelper viIsInsertable getInsDet insM
            , mutHelper viIsUpdatable getUpdDet updM
            , mutHelper viIsDeletable getDelDet delM
            , getSelDet <$> selM, getSelAggDet selM
            , getPKeySelDet selM primCols
            ]

    funcQueries = maybe [] getFuncQueryFlds selM
    funcAggQueries = maybe [] getFuncAggQueryFlds selM

    mutHelper :: (ViewInfo -> Bool) -> (a -> b) -> Maybe a -> Maybe b
    mutHelper f getDet mutM =
      bool Nothing (getDet <$> mutM) $ isMutable f viM

    getCustomNameWith = getCustomName mCustomRootFields

    insCustName = getCustomNameWith _tcrfInsert
    getInsDet (hdrs, upsertPerm) =
      let isUpsertable = upsertable constraints upsertPerm $ isJust viM
      in ( OCInsert $ InsOpCtx tn $ hdrs `union` maybe [] (\(_, _, _, x) -> x) updM
         , Right $ mkInsMutFld insCustName tn isUpsertable
         )

    updCustName = getCustomNameWith _tcrfUpdate
    getUpdDet (updCols, preSetCols, updFltr, hdrs) =
      ( OCUpdate $ UpdOpCtx tn hdrs colGNameMap updFltr preSetCols
      , Right $ mkUpdMutFld updCustName tn updCols
      )

    delCustName = getCustomNameWith _tcrfDelete
    getDelDet (delFltr, hdrs) =
      ( OCDelete $ DelOpCtx tn hdrs delFltr allCols
      , Right $ mkDelMutFld delCustName tn
      )


    selCustName = getCustomNameWith _tcrfSelect
    getSelDet (selFltr, pLimit, hdrs, _) =
      selFldHelper OCSelect (mkSelFld selCustName) selFltr pLimit hdrs

    selAggCustName = getCustomNameWith _tcrfSelectAggregate
    getSelAggDet (Just (selFltr, pLimit, hdrs, True)) =
      Just $ selFldHelper OCSelectAgg (mkAggSelFld selAggCustName)
               selFltr pLimit hdrs
    getSelAggDet _                                    = Nothing

    selFldHelper f g pFltr pLimit hdrs =
      ( f $ SelOpCtx tn hdrs colGNameMap pFltr pLimit
      , Left $ g tn
      )

    selByPkCustName = getCustomNameWith _tcrfSelectByPk
    getPKeySelDet Nothing _ = Nothing
    getPKeySelDet _ [] = Nothing
    getPKeySelDet (Just (selFltr, _, hdrs, _)) pCols = Just
      ( OCSelectPkey $ SelPkOpCtx tn hdrs selFltr $ mkPGColGNameMap pCols
      , Left $ mkSelFldPKey selByPkCustName tn pCols
      )

    getFuncQueryFlds (selFltr, pLimit, hdrs, _) =
      funcFldHelper OCFuncQuery mkFuncQueryFld selFltr pLimit hdrs

    getFuncAggQueryFlds (selFltr, pLimit, hdrs, True) =
      funcFldHelper OCFuncAggQuery mkFuncAggQueryFld selFltr pLimit hdrs
    getFuncAggQueryFlds _                             = []

    funcFldHelper f g pFltr pLimit hdrs =
      flip map funcs $ \fi ->
      ( f $ FuncQOpCtx tn hdrs colGNameMap pFltr pLimit (fiName fi) $ mkFuncArgItemSeq fi
      , Left $ g fi
      )

    mkFuncArgItemSeq fi = Seq.fromList $
      procFuncArgs (fiInputArgs fi) $ \_ t -> FuncArgItem $ G.Name t


getSelPermission :: TableInfo -> RoleName -> Maybe SelPermInfo
getSelPermission tabInfo role =
  Map.lookup role (tiRolePermInfoMap tabInfo) >>= _permSel

getSelPerm
  :: (MonadError QErr m)
  => TableCache
  -- all the fields of a table
  -> FieldInfoMap
  -- custom column field names
  -> Maybe CustomColFields
  -- role and its permission
  -> RoleName -> SelPermInfo
  -> m (Bool, [SelField])
getSelPerm tableCache fields ccfM role selPermInfo = do

  relFlds <- fmap catMaybes $ forM validRels $ \relInfo -> do
      remTableInfo <- getTabInfo tableCache $ riRTable relInfo
      let remTableSelPermM = getSelPermission remTableInfo role
          remTableFlds = tiFieldInfoMap remTableInfo
          remTableCustomColFlds =
            _tcCustomColumnFields <$> tiCustomConfig remTableInfo
          remTableColGNameMap =
            mkPGColGNameMap $ getValidCols remTableFlds remTableCustomColFlds
      return $ flip fmap remTableSelPermM $
        \rmSelPermM -> Right ( relInfo
                             , spiAllowAgg rmSelPermM
                             , remTableColGNameMap
                             , spiFilter rmSelPermM
                             , spiLimit rmSelPermM
                             , isRelNullable fields relInfo ccfM
                             )

  return (spiAllowAgg selPermInfo, colFlds <> relFlds)
  where
    validRels = getValidRels fields
    validCols = getValidCols fields ccfM
    colFlds = catMaybes $ flip map validCols $
              \colFld -> fmap Left $ bool Nothing (Just colFld) $
                         Set.member (pgiName $ _cfPGCol colFld) allowedCols

    allowedCols = spiCols selPermInfo

mkInsCtx
  :: MonadError QErr m
  => RoleName
  -> TableCache
  -> FieldInfoMap
  -> Maybe CustomColFields
  -> InsPermInfo
  -> Maybe UpdPermInfo
  -> m InsCtx
mkInsCtx role tableCache fields ccfM insPermInfo updPermM = do
  relTupsM <- forM rels $ \relInfo -> do
    let remoteTable = riRTable relInfo
        relName = riName relInfo
    remoteTableInfo <- getTabInfo tableCache remoteTable
    let insPermM = getInsPerm remoteTableInfo role
        viewInfoM = tiViewInfo remoteTableInfo
    return $ bool Nothing (Just (relName, relInfo)) $
      isInsertable insPermM viewInfoM && isValidRel relName remoteTable

  let relInfoMap = Map.fromList $ catMaybes relTupsM
  return $ InsCtx iView gNamePGColMap setCols relInfoMap updPermForIns
  where
    allColFlds = flip mkColumnFields ccfM $ getCols fields
    gNamePGColMap = mkPGColGNameMap allColFlds
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
  -> TableCache
  -> FieldInfoMap
  -> Maybe CustomColFields
  -> m InsCtx
mkAdminInsCtx tn tc fields ccfM = do
  relTupsM <- forM rels $ \relInfo -> do
    let remoteTable = riRTable relInfo
        relName = riName relInfo
    remoteTableInfo <- getTabInfo tc remoteTable
    let viewInfoM = tiViewInfo remoteTableInfo
    return $ bool Nothing (Just (relName, relInfo)) $
      isMutable viIsInsertable viewInfoM && isValidRel relName remoteTable

  let relInfoMap = Map.fromList $ catMaybes relTupsM
      updPerm = UpdPermForIns updCols noFilter Map.empty

  return $ InsCtx tn colGNameMap Map.empty relInfoMap (Just updPerm)
  where
    allCols = getCols fields
    colGNameMap = mkPGColGNameMap $ mkColumnFields allCols ccfM
    updCols = map pgiName allCols
    rels = getValidRels fields

mkAdminSelFlds
  :: MonadError QErr m
  => FieldInfoMap
  -> Maybe CustomColFields
  -> TableCache
  -> m [SelField]
mkAdminSelFlds fields customColFlds tableCache = do
  relSelFlds <- forM validRels $ \relInfo -> do
    let remoteTable = riRTable relInfo
    remoteTableInfo <- getTabInfo tableCache remoteTable
    let remoteTableFlds = tiFieldInfoMap remoteTableInfo
        remoteTableCustomColFlds =
          _tcCustomColumnFields <$> tiCustomConfig remoteTableInfo
        remoteTableColGNameMap =
          mkPGColGNameMap $ getValidCols remoteTableFlds remoteTableCustomColFlds
    return $ Right ( relInfo
                   , True
                   , remoteTableColGNameMap
                   , noFilter
                   , Nothing
                   , isRelNullable fields relInfo customColFlds
                   )
  return $ colSelFlds <> relSelFlds
  where
    colFlds = getValidCols fields customColFlds
    colSelFlds = map Left colFlds
    validRels = getValidRels fields

mkColumnFields :: [PGColInfo] -> Maybe CustomColFields -> [ColField]
mkColumnFields colInfos = \case
  Nothing -> map mkDefColFld colInfos
  Just customColFlds -> mkCustomFlds customColFlds
  where
    mkDefColFld ci = ColField ci $ G.Name $ getPGColTxt $ pgiName ci
    mkCustomFlds ccf = flip map colInfos $
      \ci -> case Map.lookup (pgiName ci) ccf of
        Nothing                 -> mkDefColFld ci
        Just (GraphQLName name) -> ColField ci name

mkGCtxRole
  :: (MonadError QErr m)
  => TableCache
  -> QualifiedTable
  -> FieldInfoMap
  -> [ColField]
  -> [ConstraintName]
  -> [FunctionInfo]
  -> Maybe ViewInfo
  -> Maybe TableConfig
  -> RoleName
  -> RolePermInfo
  -> m (TyAgg, RootFlds, InsCtxMap)
mkGCtxRole tableCache tn fields pColFlds constraints funcs viM tabConfigM role permInfo = do
  selPermM <- mapM (getSelPerm tableCache fields ccfM role) $ _permSel permInfo
  tabInsInfoM <- forM (_permIns permInfo) $ \ipi -> do
    ctx <- mkInsCtx role tableCache fields ccfM ipi $ _permUpd permInfo
    let permCols = flip getColInfos allCols $ Set.toList $ ipiCols ipi
        permColFlds = mkColumnFields permCols ccfM
    return (ctx, (permColFlds, icRelations ctx))
  let insPermM = snd <$> tabInsInfoM
      insCtxM = fst <$> tabInsInfoM
      updColsM = filterColFlds . upiCols <$> _permUpd permInfo
      tyAgg = mkGCtxRole' tn insPermM selPermM updColsM
              (void $ _permDel permInfo) pColInfos constraints viM funcs
      rootFlds = getRootFldsRole tn pColFlds constraints fields funcs permInfo viM tabConfigM
      insCtxMap = maybe Map.empty (Map.singleton tn) insCtxM
  return (tyAgg, rootFlds, insCtxMap)
  where
    ccfM = _tcCustomColumnFields <$> tabConfigM
    allCols = getCols fields
    colFlds = getValidCols fields ccfM
    pCols = map (pgiName . _cfPGCol) pColFlds
    pColInfos = getColInfos pCols allCols
    filterColFlds allowedSet =
      filter ((`Set.member` allowedSet) . pgiName . _cfPGCol) colFlds

getRootFldsRole
  :: QualifiedTable
  -> [ColField]
  -> [ConstraintName]
  -> FieldInfoMap
  -> [FunctionInfo]
  -> RolePermInfo
  -> Maybe ViewInfo
  -> Maybe TableConfig
  -> RootFlds
getRootFldsRole tn pCols constraints fields funcs (RolePermInfo insM selM updM delM) viM tableConfigM =
  getRootFldsRole' tn pCols constraints fields funcs
  (mkIns <$> insM) (mkSel <$> selM)
  (mkUpd <$> updM) (mkDel <$> delM) viM tableConfigM
  where
    ccfM = _tcCustomColumnFields <$> tableConfigM
    mkIns i = (ipiRequiredHeaders i, isJust updM)
    mkSel s = ( spiFilter s, spiLimit s
              , spiRequiredHeaders s, spiAllowAgg s
              )
    mkUpd u = ( getColFlds $ Set.toList $ upiCols u
              , upiSet u
              , upiFilter u
              , upiRequiredHeaders u
              )
    mkDel d = (dpiFilter d, dpiRequiredHeaders d)

    getColFlds cols =
      let colInfos = getColInfos cols $ getCols fields
      in mkColumnFields colInfos ccfM

mkGCtxMapTable
  :: (MonadError QErr m)
  => TableCache
  -> FunctionCache
  -> TableInfo
  -> m (Map.HashMap RoleName (TyAgg, RootFlds, InsCtxMap))
mkGCtxMapTable tableCache funcCache tabInfo = do
  m <- Map.traverseWithKey
       (mkGCtxRole tableCache tn fields pkeyColFlds validConstraints tabFuncs viewInfo customConfig) rolePerms
  adminInsCtx <- mkAdminInsCtx tn tableCache fields customColFlds
  adminSelFlds <- mkAdminSelFlds fields customColFlds tableCache
  let adminCtx = mkGCtxRole' tn (Just (colFlds, icRelations adminInsCtx))
                 (Just (True, adminSelFlds)) (Just colFlds) (Just ())
                 pkeyColInfos validConstraints viewInfo tabFuncs
      adminInsCtxMap = Map.singleton tn adminInsCtx
  return $ Map.insert adminRole (adminCtx, adminRootFlds, adminInsCtxMap) m
  where
    TableInfo tn _ fields rolePerms constraints pkeyCols viewInfo _ customConfig = tabInfo
    customColFlds = _tcCustomColumnFields <$> customConfig
    validConstraints = mkValidConstraints constraints
    colFlds = getValidCols fields customColFlds
    colInfos = getCols fields
    pkeyColInfos = getColInfos pkeyCols colInfos
    pkeyColFlds = mkColumnFields pkeyColInfos customColFlds
    tabFuncs = filter (isValidObjectName . fiName) $
               getFuncsOfTable tn funcCache
    adminRootFlds =
      getRootFldsRole' tn pkeyColFlds validConstraints fields tabFuncs
      (Just ([], True)) (Just (noFilter, Nothing, [], True))
      (Just (colFlds, mempty, noFilter, [])) (Just (noFilter, []))
      viewInfo customConfig

noFilter :: AnnBoolExpPartialSQL
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
    tyMsg ty = "types: [ " <> namesToTxt ty <>
               " ] have mismatch with current graphql schema. HINT: Types must be same."
    nodesMsg n = "top-level nodes: [ " <> namesToTxt n <>
                 " ] already exist in current graphql schema. HINT: Top-level nodes can't be same."
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
  => TableCache -> FunctionCache -> m GCtxMap
mkGCtxMap tableCache functionCache = do
  typesMapL <- mapM (mkGCtxMapTable tableCache functionCache) $
               filter tableFltr $ Map.elems tableCache
  let typesMap = foldr (Map.unionWith mappend) Map.empty typesMapL
  return $ flip Map.map typesMap $ \(ty, flds, insCtxMap) ->
    mkGCtx ty flds insCtxMap
  where
    tableFltr ti = not (tiSystemDefined ti)
                   && isValidObjectName (tiName ti)

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
