module Hasura.GraphQL.Schema.Select
  ( mkTableObj
  , mkRelayTableObj
  , mkTableAggObj
  , mkSelColumnTy
  , mkTableAggregateFieldsObj
  , mkTableColAggregateFieldsObj
  , mkTableEdgeObj
  , mkPageInfoObj
  , mkTableConnectionObj
  , mkTableConnectionTy

  , mkSelFld
  , mkAggSelFld
  , mkSelFldPKey
  , mkSelFldConnection

  , mkRemoteRelationshipName
  , mkSelArgs
  , mkConnectionArgs
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

mkTableAggregateFieldsTy :: QualifiedTable -> G.NamedType
mkTableAggregateFieldsTy = addTypeSuffix "_aggregate_fields" . mkTableTy

mkTableColAggregateFieldsTy :: G.Name -> QualifiedTable -> G.NamedType
mkTableColAggregateFieldsTy op tn =
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
      -- TODO: connection stuff
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

-- distinct_on: [table_select_column!]
-- where: table_bool_exp
-- order_by: table_order_by
-- first: Int
-- after: String
-- last: Int
-- before: String
mkConnectionArgs :: QualifiedTable -> [InpValInfo]
mkConnectionArgs tn =
  [ InpValInfo (Just whereDesc) "where" Nothing $ G.toGT $ mkBoolExpTy tn
  , InpValInfo (Just orderByDesc) "order_by" Nothing $ G.toGT $ G.toLT $ G.toNT $
    mkOrdByTy tn
  , InpValInfo (Just distinctDesc) "distinct_on" Nothing $ G.toGT $ G.toLT $
    G.toNT $ mkSelColumnInpTy tn
  , InpValInfo Nothing "first" Nothing $ G.toGT $ mkScalarTy PGInteger
  , InpValInfo Nothing "after" Nothing $ G.toGT $ mkScalarTy PGText
  , InpValInfo Nothing "last" Nothing $ G.toGT $ mkScalarTy PGInteger
  , InpValInfo Nothing "before" Nothing $ G.toGT $ mkScalarTy PGText
  ]
  where
    whereDesc   = "filter the rows returned"
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
  -> Maybe (NonEmpty PGColumnInfo)
  -> Bool
  -> [ObjFldInfo]
mkRelationshipField allowAgg (RelInfo rn rTy _ remTab isManual) isRelay maybePkCols isNullable =
  case rTy of
    ArrRel -> bool [arrRelFld] ([arrRelFld, aggArrRelFld] <> connFields) allowAgg
    ObjRel -> [objRelFld]
  where
    objRelFld = mkHsraObjFldInfo (Just "An object relationship")
      (mkRelName rn) Map.empty objRelTy
    objRelTy = bool (G.toGT $ G.toNT relTabTy) (G.toGT relTabTy) isObjRelNullable
    isObjRelNullable = isManual || isNullable
    relTabTy = mkTableTy remTab

    arrRelFld =
      mkHsraObjFldInfo (Just "An array relationship") (mkRelName rn)
      (fromInpValL $ mkSelArgs remTab) $
      G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableTy remTab

    connFields = if isNothing maybePkCols || not isRelay then [] else pure $
      mkHsraObjFldInfo Nothing (mkConnectionRelName rn)
      (fromInpValL $ mkConnectionArgs remTab) $
      G.toGT $ G.toNT $ mkTableConnectionTy remTab

    aggArrRelFld = mkHsraObjFldInfo (Just "An aggregated array relationship")
      (mkAggRelName rn) (fromInpValL $ mkSelArgs remTab) $
      G.toGT $ G.toNT $ mkTableAggTy remTab

mkTableObjectDescription :: QualifiedTable -> Maybe PGDescription -> G.Description
mkTableObjectDescription tn pgDescription =
  mkDescriptionWith pgDescription $ "columns and relationships of " <>> tn

mkTableObjectFields :: Bool -> [SelField] -> [ObjFldInfo]
mkTableObjectFields isRelay =
  concatMap \case
    SFPGColumn info -> pure $ mkPGColFld info
    SFRelationship info -> mkRelationshipField' info
    SFComputedField info -> pure $ mkComputedFieldFld info
    SFRemoteRelationship info -> pure $ mkRemoteRelationshipFld info
  where
    mkRelationshipField' (RelationshipFieldInfo relInfo allowAgg _ _ _ maybePkCols isNullable) =
      mkRelationshipField allowAgg relInfo isRelay maybePkCols isNullable

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
mkTableObj tn descM allowedFields =
  mkObjTyInfo (Just desc) (mkTableTy tn) Set.empty (mapFromL _fiName fields) TLHasuraType
  where
    fields = mkTableObjectFields False allowedFields
    desc = mkTableObjectDescription tn descM

mkRelayTableObj
  :: QualifiedTable
  -> Maybe PGDescription
  -> [SelField]
  -> ObjTyInfo
mkRelayTableObj tn descM allowedFields =
  mkObjTyInfo (Just desc) (mkTableTy tn) Set.empty (mapFromL _fiName fields) TLHasuraType
  where
    fields =
      let idColumnFilter = \case
            SFPGColumn columnInfo -> (/=) "id" $ getPGColTxt $ pgiColumn columnInfo
            _                     -> True
      in (:) nodeIdField $ mkTableObjectFields True $
             -- Remove "id" column
             filter idColumnFilter allowedFields

    nodeIdField = mkHsraObjFldInfo Nothing "id" mempty nodeIdType
    desc = mkTableObjectDescription tn descM

mkRemoteRelationshipName :: RemoteRelationshipName -> G.Name
mkRemoteRelationshipName =
  G.Name . remoteRelationshipNameToText

mkRemoteRelationshipFld :: RemoteFieldInfo -> ObjFldInfo
mkRemoteRelationshipFld remoteField =
  mkHsraObjFldInfo description fieldName paramMap gType
  where
    description = Just "Remote relationship field"
    fieldName = mkRemoteRelationshipName $ _rfiName remoteField
    paramMap = _rfiParamMap remoteField
    gType = _rfiGType remoteField

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
             mkTableAggregateFieldsTy tn
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
mkTableAggregateFieldsObj
  :: QualifiedTable
  -> ([PGColumnInfo], [G.Name])
  -> ([PGColumnInfo], [G.Name])
  -> ObjTyInfo
mkTableAggregateFieldsObj tn (numCols, numericAggregateOps) (compCols, compareAggregateOps) =
  mkHsraObjTyInfo (Just desc) (mkTableAggregateFieldsTy tn) Set.empty $ mapFromL _fiName $
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

    numFlds = bool (map mkColumnOpFld numericAggregateOps) [] $ null numCols
    compFlds = bool (map mkColumnOpFld compareAggregateOps) [] $ null compCols

    mkColumnOpFld op = mkHsraObjFldInfo Nothing op Map.empty $ G.toGT $
                    mkTableColAggregateFieldsTy op tn

{-
type table_<agg-op>_fields{
   num_col: Int
   .        .
   .        .
}
-}
mkTableColAggregateFieldsObj
  :: QualifiedTable
  -> G.Name
  -> (PGColumnType -> G.NamedType)
  -> [PGColumnInfo]
  -> ObjTyInfo
mkTableColAggregateFieldsObj tn op f cols =
  mkHsraObjTyInfo (Just desc) (mkTableColAggregateFieldsTy op tn) Set.empty $ mapFromL _fiName $
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

table(
  where: table_bool_exp
  limit: Int
  offset: Int
):  tableConnection!

-}

mkSelFldConnection :: Maybe G.Name -> QualifiedTable -> ObjFldInfo
mkSelFldConnection mCustomName tn =
  mkHsraObjFldInfo (Just desc) fldName args ty
  where
    desc    = G.Description $ "fetch data from the table: " <>> tn
    fldName = fromMaybe (qualObjectToName tn <> "_connection") mCustomName
    args    = fromInpValL $ mkConnectionArgs tn
    ty      = G.toGT $ G.toNT $ mkTableConnectionTy tn

{-
type tableConnection {
  pageInfo: PageInfo!
  edges: [tableEdge!]!
}
-}
mkTableConnectionObj
  :: QualifiedTable -> ObjTyInfo
mkTableConnectionObj tn =
  mkHsraObjTyInfo (Just desc) (mkTableConnectionTy tn) Set.empty $ mapFromL _fiName
  [pageInfoFld, edgesFld]
  where
    desc = G.Description $ "A Relay Connection object on " <>> tn
    pageInfoFld = mkHsraObjFldInfo Nothing "pageInfo" Map.empty $
                  G.toGT $ G.toNT pageInfoTy
    edgesFld = mkHsraObjFldInfo Nothing "edges" Map.empty $ G.toGT $
               G.toNT $ G.toLT $ G.toNT $ mkTableEdgeTy tn

booleanScalar :: G.NamedType
booleanScalar = G.NamedType "Boolean"

stringScalar :: G.NamedType
stringScalar = G.NamedType "String"

pageInfoTyName :: G.Name
pageInfoTyName = "PageInfo"

pageInfoTy :: G.NamedType
pageInfoTy = G.NamedType pageInfoTyName
{-
type PageInfo {
  hasNextPage: Boolean!
  hasPrevousPage: Boolean!
  startCursor: String!
  endCursor: String!
}
-}
mkPageInfoObj :: ObjTyInfo
mkPageInfoObj =
  mkHsraObjTyInfo Nothing pageInfoTy Set.empty $ mapFromL _fiName
  [hasNextPage, hasPreviousPage, startCursor, endCursor]
  where
    hasNextPage = mkHsraObjFldInfo Nothing "hasNextPage" Map.empty $
                  G.toGT $ G.toNT booleanScalar
    hasPreviousPage = mkHsraObjFldInfo Nothing "hasPreviousPage" Map.empty $
                      G.toGT $ G.toNT booleanScalar
    startCursor = mkHsraObjFldInfo Nothing "startCursor" Map.empty $
                  G.toGT $ G.toNT stringScalar
    endCursor = mkHsraObjFldInfo Nothing "endCursor" Map.empty $
                G.toGT $ G.toNT stringScalar

{-
type tableConnection {
  cursor: String!
  node: table
}
-}
mkTableEdgeObj
  :: QualifiedTable -> ObjTyInfo
mkTableEdgeObj tn =
  mkHsraObjTyInfo Nothing (mkTableEdgeTy tn) Set.empty $ mapFromL _fiName
  [cursor, node]
  where
    cursor = mkHsraObjFldInfo Nothing "cursor" Map.empty $
             G.toGT $ G.toNT stringScalar
    node = mkHsraObjFldInfo Nothing "node" Map.empty $ G.toGT $
           mkTableTy tn

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
