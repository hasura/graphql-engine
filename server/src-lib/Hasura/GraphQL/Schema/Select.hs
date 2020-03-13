module Hasura.GraphQL.Schema.Select
  ( mkTableObj
  , mkTableAggObj
  , mkSelColumnTy
  , mkTableAggFldsObj
  , mkTableColAggFldsObj

  , mkSelFld
  , mkAggSelFld
  , mkSelFldPKey

  , mkSelArgs
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.OrderBy
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

mkSelColumnTy :: QualifiedTable -> [G.Name] -> EnumTyInfo
mkSelColumnTy tn cols = enumTyInfo
  where
    enumTyInfo = mkHsraEnumTyInfo (Just desc) (mkSelColumnInpTy tn) $
      EnumValuesSynthetic . mapFromL _eviVal $ map mkColumnEnumVal cols

    desc = G.Description $
      "select columns of table " <>> tn

--table_select_column
mkSelColumnInpTy :: QualifiedTable -> G.NamedType
mkSelColumnInpTy tn =
  G.NamedType $ qualObjectToName tn <> "_select_column"

mkTableAggFldsTy :: QualifiedTable -> G.NamedType
mkTableAggFldsTy = addTypeSuffix "_aggregate_fields" . mkTableTy

mkTableColAggFldsTy :: G.Name -> QualifiedTable -> G.NamedType
mkTableColAggFldsTy op tn =
  G.NamedType $ qualObjectToName tn <> "_" <> op <> "_fields"

mkTableByPkName :: QualifiedTable -> G.Name
mkTableByPkName tn = qualObjectToName tn <> "_by_pk"

-- Support argument params for PG columns
mkPGColParams :: PGColumnType -> ParamMap
mkPGColParams colType
  | isScalarColumnWhere isJSONType colType =
    let pathDesc = "JSON select path"
    in Map.fromList
      [ (G.Name "path", InpValInfo (Just pathDesc) "path" Nothing $ G.toGT $ mkScalarTy PGText) ]
  | otherwise = Map.empty

mkPGColFld :: PGColumnInfo -> ObjFldInfo
mkPGColFld colInfo =
  mkHsraObjFldInfo desc name (mkPGColParams colTy) ty
  where
    PGColumnInfo _ name _ colTy isNullable pgDesc = colInfo
    desc = (G.Description . getPGDescription) <$> pgDesc
    ty = bool notNullTy nullTy isNullable
    columnType = mkColumnType colTy
    notNullTy = G.toGT $ G.toNT columnType
    nullTy = G.toGT columnType

mkComputedFieldFld :: ComputedField -> ObjFldInfo
mkComputedFieldFld field =
  uncurry (mkHsraObjFldInfo (Just desc) fieldName) $ case fieldType of
    CFTScalar scalarTy    ->
      let inputParams = mkPGColParams (PGColumnScalar scalarTy)
                        <> fromInpValL (maybeToList maybeFunctionInputArg)
      in (inputParams, G.toGT $ mkScalarTy scalarTy)
    CFTTable computedFieldtable ->
      let table = _cftTable computedFieldtable
      in ( fromInpValL $ maybeToList maybeFunctionInputArg <> mkSelArgs table
         , G.toGT $ G.toLT $ G.toNT $ mkTableTy table
         )
  where
    columnDescription = "A computed field, executes function " <>> qf
    desc = mkDescriptionWith (_cffDescription function) columnDescription
    fieldName = mkComputedFieldName name
    ComputedField name function _ fieldType = field
    qf = _cffName function

    maybeFunctionInputArg =
      let funcArgDesc = G.Description $ "input parameters for function " <>> qf
          inputValue = InpValInfo (Just funcArgDesc) "args" Nothing $
                       G.toGT $ G.toNT $ mkFuncArgsTy qf
          inputArgs = _cffInputArgs function
      in bool (Just inputValue) Nothing $ null inputArgs


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
    limitDesc   = "limit the number of rows returned"
    offsetDesc  = "skip the first n rows. Use only with order_by"
    orderByDesc = "sort the rows by one or more columns"
    distinctDesc = "distinct select on columns"

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
mkRelationshipField
  :: Bool
  -> RelInfo
  -> Bool
  -> [ObjFldInfo]
mkRelationshipField allowAgg (RelInfo rn rTy _ remTab isManual) isNullable = case rTy of
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
  -> Maybe PGDescription
  -> [SelField]
  -> ObjTyInfo
mkTableObj tn descM allowedFlds =
  mkObjTyInfo (Just desc) (mkTableTy tn) Set.empty (mapFromL _fiName flds) TLHasuraType
  where
    flds = pgColFlds <> relFlds <> computedFlds
    pgColFlds = map mkPGColFld $ getPGColumnFields allowedFlds
    relFlds = concatMap mkRelationshipField' $ getRelationshipFields allowedFlds
    computedFlds = map mkComputedFieldFld $ getComputedFields allowedFlds
    mkRelationshipField' (RelationshipFieldInfo relInfo allowAgg _ _ _ isNullable) =
      mkRelationshipField allowAgg relInfo isNullable
    desc = mkDescriptionWith descM $ "columns and relationships of " <>> tn

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
  :: QualifiedTable
  -> ([PGColumnInfo], [G.Name])
  -> ([PGColumnInfo], [G.Name])
  -> ObjTyInfo
mkTableAggFldsObj tn (numCols, numAggOps) (compCols, compAggOps) =
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
  -> (PGColumnType -> G.NamedType)
  -> [PGColumnInfo]
  -> ObjTyInfo
mkTableColAggFldsObj tn op f cols =
  mkHsraObjTyInfo (Just desc) (mkTableColAggFldsTy op tn) Set.empty $ mapFromL _fiName $
  map mkColObjFld cols
  where
    desc = G.Description $ "aggregate " <> G.unName op <> " on columns"

    mkColObjFld ci = mkHsraObjFldInfo Nothing (pgiName ci) Map.empty $
                     G.toGT $ f $ pgiType ci

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
mkSelFldPKey :: Maybe G.Name -> QualifiedTable -> [PGColumnInfo] -> ObjFldInfo
mkSelFldPKey mCustomName tn cols =
  mkHsraObjFldInfo (Just desc) fldName args ty
  where
    desc = G.Description $ "fetch data from the table: " <> tn
           <<> " using primary key columns"
    fldName = fromMaybe (mkTableByPkName tn) mCustomName
    args = fromInpValL $ map mkColumnInputVal cols
    ty = G.toGT $ mkTableTy tn

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
