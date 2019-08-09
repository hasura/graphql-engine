module Hasura.GraphQL.Schema.Mutation.Insert
  ( mkInsInp
  , mkInsInpTy
  , mkRelInsInps
  , mkInsMutFld
  , mkOnConflictTypes
  ) where

import qualified Data.HashMap.Strict                  as Map
import qualified Language.GraphQL.Draft.Syntax        as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Mutation.Common
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

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
  :: QualifiedTable -> [PGColInfo] -> RelationInfoMap -> InpObjTyInfo
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

mkInsMutFld
  :: QualifiedTable -> Bool -> ObjFldInfo
mkInsMutFld tn isUpsertable =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL inputVals) $
    G.toGT $ mkMutRespTy tn
  where
    inputVals = catMaybes [Just objectsArg , onConflictInpVal]
    desc = G.Description $
      "insert data into the table: " <>> tn

    fldName = "insert_" <> qualObjectToName tn

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

mkUpdColumnTy :: QualifiedTable -> [PGCol] -> EnumTyInfo
mkUpdColumnTy tn cols = enumTyInfo
  where
    enumTyInfo = mkHsraEnumTyInfo (Just desc) (mkUpdColumnInpTy tn) $
                 mapFromL _eviVal $ map mkColumnEnumVal cols

    desc = G.Description $
      "update columns of table " <>> tn

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
