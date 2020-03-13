module Hasura.GraphQL.Schema.Mutation.Insert
  ( mkInsInp
  , mkInsInpTy
  , mkRelInsInps
  , mkInsMutFld
  , mkInsertOneMutationField
  , mkOnConflictTypes
  ) where

import qualified Data.HashMap.Strict                   as Map
import qualified Language.GraphQL.Draft.Syntax         as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Schema.BoolExp
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
  :: QualifiedTable -> [PGColumnInfo] -> RelationInfoMap -> InpObjTyInfo
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
  constraint: table_constraint!
  update_columns: [table_column!]
  where: table_bool_exp
}

-}

mkOnConflictInp :: QualifiedTable -> InpObjTyInfo
mkOnConflictInp tn =
  mkHsraInpTyInfo (Just desc) (mkOnConflictInpTy tn) $ fromInpValL
  [constraintInpVal, updateColumnsInpVal, whereInpVal]
  where
    desc = G.Description $
      "on conflict condition type for table " <>> tn

    constraintInpVal = InpValInfo Nothing (G.Name "constraint") Nothing $
      G.toGT $ G.toNT $ mkConstraintInpTy tn

    updateColumnsInpVal = InpValInfo Nothing (G.Name "update_columns") Nothing $
      G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkUpdColumnInpTy tn

    whereInpVal = InpValInfo Nothing (G.Name "where") Nothing $
                  G.toGT $ mkBoolExpTy tn
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
    inputVals = catMaybes [Just objectsArg , mkOnConflictInputVal tn isUpsertable]
    desc = G.Description $
      "insert data into the table: " <>> tn

    defFldName = "insert_" <> qualObjectToName tn
    fldName = fromMaybe defFldName mCustomName

    objsArgDesc = "the rows to be inserted"
    objectsArg =
      InpValInfo (Just objsArgDesc) "objects" Nothing $ G.toGT $
      G.toNT $ G.toLT $ G.toNT $ mkInsInpTy tn

mkConstraintTy :: QualifiedTable -> [ConstraintName] -> EnumTyInfo
mkConstraintTy tn cons = enumTyInfo
  where
    enumTyInfo = mkHsraEnumTyInfo (Just desc) (mkConstraintInpTy tn) $
      EnumValuesSynthetic . mapFromL _eviVal $ map mkConstraintEnumVal cons

    desc = G.Description $
      "unique or primary key constraints on table " <>> tn

    mkConstraintEnumVal (ConstraintName n) =
      EnumValInfo (Just "unique or primary key constraint")
      (G.EnumValue $ G.Name n) False

mkUpdColumnTy :: QualifiedTable -> [G.Name] -> EnumTyInfo
mkUpdColumnTy tn cols = enumTyInfo
  where
    enumTyInfo = mkHsraEnumTyInfo (Just desc) (mkUpdColumnInpTy tn) $
      EnumValuesSynthetic . mapFromL _eviVal $ map mkColumnEnumVal cols

    desc = G.Description $
      "update columns of table " <>> tn

mkOnConflictTypes
  :: QualifiedTable -> [ConstraintName] -> [G.Name] -> Bool -> [TypeInfo]
mkOnConflictTypes tn uniqueOrPrimaryCons cols =
  bool [] tyInfos
  where
    tyInfos = [ TIEnum $ mkConstraintTy tn uniqueOrPrimaryCons
              , TIEnum $ mkUpdColumnTy tn cols
              , TIInpObj $ mkOnConflictInp tn
              ]

mkOnConflictInputVal :: QualifiedTable -> Bool -> Maybe InpValInfo
mkOnConflictInputVal qt =
  bool Nothing (Just onConflictArg)
  where
    onConflictDesc = "on conflict condition"
    onConflictArg = InpValInfo (Just onConflictDesc) "on_conflict"
                    Nothing $ G.toGT $ mkOnConflictInpTy qt


{-
insert_table_one(
  object: table_insert_input!
  on_conflict: table_on_conflict
  ): table
-}

mkInsertOneMutationField :: Maybe G.Name -> QualifiedTable -> Bool -> ObjFldInfo
mkInsertOneMutationField mCustomName qt isUpsertable =
  mkHsraObjFldInfo (Just description) fieldName (fromInpValL inputVals) $
  G.toGT $ mkTableTy qt
  where
    description = G.Description $ "insert a single row into the table: " <>> qt

    fieldName = flip fromMaybe mCustomName $ "insert_" <> qualObjectToName qt <> "_one"

    inputVals = catMaybes [Just objectArg, mkOnConflictInputVal qt isUpsertable]

    objectArgDesc = "the row to be inserted"
    objectArg = InpValInfo (Just objectArgDesc) "object" Nothing $ G.toGT $
                G.toNT $ mkInsInpTy qt
