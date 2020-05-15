module Hasura.GraphQL.Schema.OrderBy
  ( mkOrdByTy
  , ordByEnumTy
  , mkOrdByInpObj
  , mkTabAggOrdByInpObj
  , mkTabAggOpOrdByInpObjs
  ) where

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

ordByTy :: G.NamedType
ordByTy = G.NamedType "order_by"

ordByEnumTy :: EnumTyInfo
ordByEnumTy =
  mkHsraEnumTyInfo (Just desc) ordByTy $
    EnumValuesSynthetic . mapFromL _eviVal $ map mkEnumVal enumVals
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
  :: QualifiedTable
  -> ([PGColumnInfo], [G.Name])
  -> ([PGColumnInfo], [G.Name])
  -> [InpObjTyInfo]
mkTabAggOpOrdByInpObjs tn (numCols, numAggOps) (compCols, compAggOps) =
  mapMaybe (mkInpObjTyM numCols) numAggOps
  <> mapMaybe (mkInpObjTyM compCols) compAggOps
  where

    mkDesc (G.Name op) =
      G.Description $ "order by " <> op <> "() on columns of table " <>> tn

    mkInpObjTyM cols op = bool (Just $ mkInpObjTy cols op) Nothing $ null cols
    mkInpObjTy cols op =
      mkHsraInpTyInfo (Just $ mkDesc op) (mkTabAggOpOrdByTy tn op) $
      fromInpValL $ map mkColInpVal cols

    mkColInpVal ci = InpValInfo Nothing (pgiName ci) Nothing $ G.toGT
                    ordByTy

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
  :: QualifiedTable
  -> ([PGColumnInfo], [G.Name])
  -> ([PGColumnInfo], [G.Name])
  -> InpObjTyInfo
mkTabAggOrdByInpObj tn (numCols, numAggOps) (compCols, compAggOps) =
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

    pgColFlds = getPGColumnFields selFlds
    relFltr ty = flip filter (getRelationshipFields selFlds) $
                 \rf -> riType (_rfiInfo rf) == ty
    objRels = relFltr ObjRel
    arrRels = relFltr ArrRel

    mkColOrdBy columnInfo =
      InpValInfo Nothing (pgiName columnInfo) Nothing $ G.toGT ordByTy
    mkObjRelOrdBy relationshipField =
      let ri = _rfiInfo relationshipField
      in InpValInfo Nothing (mkRelName $ riName ri) Nothing $
         G.toGT $ mkOrdByTy $ riRTable ri

    mkArrRelAggOrdBy relationshipField =
      let ri = _rfiInfo relationshipField
          isAggAllowed = _rfiAllowAgg relationshipField
          ivi = InpValInfo Nothing (mkAggRelName $ riName ri) Nothing $
            G.toGT $ mkTabAggOrdByTy $ riRTable ri
      in bool Nothing (Just ivi) isAggAllowed

    ordByCtx = Map.singleton namedTy $ Map.fromList $
               colOrdBys <> relOrdBys <> arrRelOrdBys
    colOrdBys = map (pgiName &&& OBIPGCol) pgColFlds
    relOrdBys = flip map objRels $
                \relationshipField ->
                  let ri = _rfiInfo relationshipField
                      fltr = _rfiPermFilter relationshipField
                  in ( mkRelName $ riName ri
                     , OBIRel ri fltr
                     )

    arrRelOrdBys = flip mapMaybe arrRels $
                   \(RelationshipFieldInfo ri isAggAllowed colGNameMap fltr _ _) ->
                     let obItem = ( mkAggRelName $ riName ri
                                  , OBIAgg ri colGNameMap fltr
                                  )
                     in bool Nothing (Just obItem) isAggAllowed
