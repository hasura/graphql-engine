module Hasura.GraphQL.Schema.Mutation.Update
  ( mkUpdSetInp
  , mkUpdIncInp
  , mkUpdJSONOpInp
  , mkUpdSetTy
  , mkUpdMutFld
  , mkPKeyColumnsInpObj
  , mkUpdateByPkMutationField
  ) where

import qualified Language.GraphQL.Draft.Syntax         as G

import           Hasura.GraphQL.Schema.BoolExp
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Mutation.Common
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

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
  :: QualifiedTable -> [PGColumnInfo] -> InpObjTyInfo
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
  :: QualifiedTable -> Maybe [PGColumnInfo] -> Maybe InpObjTyInfo
mkUpdIncInp tn = maybe Nothing mkType
  where
    mkType cols = let numCols = onlyNumCols cols
                      incObjTy =
                        mkHsraInpTyInfo (Just desc) (mkUpdIncTy tn) $
                          fromInpValL $ map mkPGColInp numCols
                  in bool (Just incObjTy) Nothing $ null numCols
    desc = G.Description $
      "input type for incrementing integer column in table " <>> tn

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
  :: QualifiedTable -> [PGColumnInfo] -> [InpObjTyInfo]
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

mkIncInpVal :: QualifiedTable -> [PGColumnInfo] -> Maybe InpValInfo
mkIncInpVal tn cols = bool (Just incArg) Nothing $ null numCols
  where
    numCols = onlyNumCols cols
    incArgDesc = "increments the integer columns with given value of the filtered values"
    incArg =
      InpValInfo (Just incArgDesc) "_inc" Nothing $ G.toGT $ mkUpdIncTy tn

mkJSONOpInpVals :: QualifiedTable -> [PGColumnInfo] -> [InpValInfo]
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

mkUpdateOpInputs :: QualifiedTable -> [PGColumnInfo] -> [InpValInfo]
mkUpdateOpInputs qt cols =
  catMaybes [Just setInp , mkIncInpVal qt cols] <> mkJSONOpInpVals qt cols
  where
    setArgDesc = "sets the columns of the filtered rows to the given values"
    setInp =
      InpValInfo (Just setArgDesc) "_set" Nothing $ G.toGT $ mkUpdSetTy qt


mkUpdMutFld :: Maybe G.Name -> QualifiedTable -> [PGColumnInfo] -> ObjFldInfo
mkUpdMutFld mCustomName tn cols =
  mkHsraObjFldInfo (Just desc) fldName (fromInpValL inputValues) $
    G.toGT $ mkMutRespTy tn
  where
    inputValues = [filterArg] <> mkUpdateOpInputs tn cols
    desc = G.Description $ "update data of the table: " <>> tn

    defFldName = "update_" <> qualObjectToName tn
    fldName = fromMaybe defFldName mCustomName

    filterArgDesc = "filter the rows which have to be updated"
    filterArg =
      InpValInfo (Just filterArgDesc) "where" Nothing $ G.toGT $
      G.toNT $ mkBoolExpTy tn

{-

update_table_by_pk(
  columns: table_pk_columns_input!
  _set  : table_set_input
  _inc  : table_inc_input
  _concat: table_concat_input
  _delete_key: table_delete_key_input
  _delete_elem: table_delete_elem_input
  _delete_path_at: table_delete_path_at_input
)
-}

{-
input table_pk_columns_input {
  col1: col-ty1!
  col2: col-ty2!
}

where col1, col2 are primary key columns
-}

mkPKeyColumnsInpTy :: QualifiedTable -> G.NamedType
mkPKeyColumnsInpTy qt =
  G.NamedType $ qualObjectToName qt <> "_pk_columns_input"

mkPKeyColumnsInpObj :: QualifiedTable -> PrimaryKey PGColumnInfo -> InpObjTyInfo
mkPKeyColumnsInpObj qt primaryKey =
  mkHsraInpTyInfo (Just description) (mkPKeyColumnsInpTy qt) $
  fromInpValL $ map mkColumnInputVal $ toList $ _pkColumns primaryKey
  where
    description = G.Description $ "primary key columns input for table: " <>> qt

mkUpdateByPkMutationField
  :: Maybe G.Name
  -> QualifiedTable
  -> [PGColumnInfo]
  -> PrimaryKey PGColumnInfo
  -> ObjFldInfo
mkUpdateByPkMutationField mCustomName qt cols _ =
  mkHsraObjFldInfo (Just description) fieldName (fromInpValL inputArgs) $
  G.toGT $ mkTableTy qt
  where
    description = G.Description $ "update single row of the table: " <>> qt
    fieldName = flip fromMaybe mCustomName $ "update_" <> qualObjectToName qt <> "_by_pk"

    inputArgs = pure primaryKeyColumnsInp <> mkUpdateOpInputs qt cols
    primaryKeyColumnsInp =
      InpValInfo Nothing "pk_columns" Nothing $ G.toGT $ G.toNT $ mkPKeyColumnsInpTy qt
