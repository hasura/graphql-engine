{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Hasura.GraphQL.Schema
  ( UpdPermForIns
  , mkGCtxMap
  , GCtxMap
  , getGCtx
  , GCtx(..)
  , OpCtx(..)
  , OrdByResolveCtx
  , OrdByResolveCtxElem
  , NullsOrder(..)
  , OrdTy(..)
  ) where

import           Data.Has

import qualified Data.HashMap.Strict            as Map
import qualified Data.HashSet                   as Set
import qualified Data.List                      as L

import qualified Data.Text                      as T
import qualified Language.GraphQL.Draft.Syntax  as G

import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Validate.Types

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML                 as S

defaultTypes :: [TypeInfo]
defaultTypes = $(fromSchemaDocQ defaultSchema)

type OpCtxMap = Map.HashMap G.Name OpCtx
type UpdPermForIns = ([PGCol], S.BoolExp)

data OpCtx
  -- tn, vn, cols, update filter req hdrs
  = OCInsert QualifiedTable QualifiedTable [PGCol] (Maybe UpdPermForIns) [T.Text]
  -- tn, filter exp, limit, req hdrs
  | OCSelect QualifiedTable S.BoolExp (Maybe Int) [T.Text]
  -- tn, filter exp, reqt hdrs
  | OCSelectPkey QualifiedTable S.BoolExp [T.Text]
  -- tn, filter exp, req hdrs
  | OCUpdate QualifiedTable S.BoolExp [T.Text]

  -- tn, filter exp, req hdrs
  | OCDelete QualifiedTable S.BoolExp [T.Text]
  deriving (Show, Eq)

data GCtx
  = GCtx
  { _gTypes      :: !TypeMap
  , _gFields     :: !FieldMap
  , _gOrdByEnums :: !OrdByResolveCtx
  , _gQueryRoot  :: !ObjTyInfo
  , _gMutRoot    :: !(Maybe ObjTyInfo)
  , _gSubRoot    :: !(Maybe ObjTyInfo)
  , _gOpCtxMap   :: !OpCtxMap
  } deriving (Show, Eq)

instance Has TypeMap GCtx where
  getter = _gTypes
  modifier f ctx = ctx { _gTypes = f $ _gTypes ctx }

data TyAgg
  = TyAgg
  { _taTypes      :: !TypeMap
  , _taFields     :: !FieldMap
  , _taOrdByEnums :: !OrdByResolveCtx
  } deriving (Show, Eq)

instance Semigroup TyAgg where
  (TyAgg t1 f1 o1) <> (TyAgg t2 f2 o2) =
    TyAgg (Map.union t1 t2) (Map.union f1 f2) (Map.union o1 o2)

instance Monoid TyAgg where
  mempty = TyAgg Map.empty Map.empty Map.empty
  mappend = (<>)

type SelField = Either PGColInfo (RelInfo, S.BoolExp, Maybe Int, Bool)

qualTableToName :: QualifiedTable -> G.Name
qualTableToName = G.Name <$> \case
  QualifiedTable (SchemaName "public") tn -> getTableTxt tn
  QualifiedTable sn tn -> getSchemaTxt sn <> "_" <> getTableTxt tn

isValidTableName :: QualifiedTable -> Bool
isValidTableName = isValidName . qualTableToName

isValidField :: FieldInfo -> Bool
isValidField = \case
  FIColumn (PGColInfo col _ _) -> isColEligible col
  FIRelationship (RelInfo rn _ _ remTab _ _) -> isRelEligible rn remTab
  where
    isColEligible = isValidName . G.Name . getPGColTxt
    isRelEligible rn rt = isValidName (G.Name $ getRelTxt rn)
                          && isValidTableName rt

toValidFieldInfos :: FieldInfoMap -> [FieldInfo]
toValidFieldInfos = filter isValidField . Map.elems

validPartitionFieldInfoMap :: FieldInfoMap -> ([PGColInfo], [RelInfo])
validPartitionFieldInfoMap = partitionFieldInfos . toValidFieldInfos

mkValidConstraints :: [TableConstraint] -> [TableConstraint]
mkValidConstraints = filter isValid
  where
    isValid (TableConstraint _ n) =
      isValidName $ G.Name $ getConstraintTxt n

isRelNullable :: FieldInfoMap -> RelInfo -> Bool
isRelNullable fim ri = isNullable
  where
    lCols = map fst $ riMapping ri
    allCols = getCols fim
    lColInfos = getColInfos lCols allCols
    isNullable = any pgiIsNullable lColInfos

mkColName :: PGCol -> G.Name
mkColName (PGCol n) = G.Name n

mkCompExpName :: PGColType -> G.Name
mkCompExpName pgColTy =
  G.Name $ T.pack (show pgColTy) <> "_comparison_exp"

mkCompExpTy :: PGColType -> G.NamedType
mkCompExpTy =
  G.NamedType . mkCompExpName

mkBoolExpName :: QualifiedTable -> G.Name
mkBoolExpName tn =
  qualTableToName tn <> "_bool_exp"

mkBoolExpTy :: QualifiedTable -> G.NamedType
mkBoolExpTy =
  G.NamedType . mkBoolExpName

mkTableTy :: QualifiedTable -> G.NamedType
mkTableTy =
  G.NamedType . qualTableToName

mkTableByPKeyTy :: QualifiedTable -> G.Name
mkTableByPKeyTy tn = qualTableToName tn <> "_by_pk"

mkCompExpInp :: PGColType -> InpObjTyInfo
mkCompExpInp colTy =
  InpObjTyInfo (Just tyDesc) (mkCompExpTy colTy) $ fromInpValL $ concat
  [ map (mk colScalarTy) typedOps
  , map (mk $ G.toLT colScalarTy) listOps
  , bool [] (map (mk $ mkScalarTy PGText) stringOps) isStringTy
  , bool [] (map jsonbOpToInpVal jsonbOps) isJsonbTy
  , [InpValInfo Nothing "_is_null" $ G.TypeNamed $ G.NamedType "Boolean"]
  ]
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

    mk t n = InpValInfo Nothing n $ G.toGT t

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

    jsonbOpToInpVal (op, ty, desc) = InpValInfo (Just desc) op ty

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

mkPGColFld :: PGColInfo -> ObjFldInfo
mkPGColFld (PGColInfo colName colTy isNullable) =
  ObjFldInfo Nothing n Map.empty ty
  where
    n  = G.Name $ getPGColTxt colName
    ty = bool notNullTy nullTy isNullable
    scalarTy = mkScalarTy colTy
    notNullTy = G.toGT $ G.toNT scalarTy
    nullTy = G.toGT scalarTy

-- where: table_bool_exp
-- limit: Int
-- offset: Int
mkSelArgs :: QualifiedTable -> [InpValInfo]
mkSelArgs tn =
  [ InpValInfo (Just whereDesc) "where" $ G.toGT $ mkBoolExpTy tn
  , InpValInfo (Just limitDesc) "limit" $ G.toGT $ mkScalarTy PGInteger
  , InpValInfo (Just offsetDesc) "offset" $ G.toGT $ mkScalarTy PGInteger
  , InpValInfo (Just orderByDesc) "order_by" $ G.toGT $ G.toLT $ G.toNT $
    mkOrdByTy tn
  ]
  where
    whereDesc   = "filter the rows returned"
    limitDesc   = "limit the nuber of rows returned"
    offsetDesc  = "skip the first n rows. Use only with order_by"
    orderByDesc = "sort the rows by one or more columns"

fromInpValL :: [InpValInfo] -> Map.HashMap G.Name InpValInfo
fromInpValL = mapFromL _iviName

{-

array_relationship(
  where: remote_table_bool_exp
  limit: Int
  offset: Int
):  [remote_table!]!
object_relationship: remote_table

-}
mkRelFld :: RelInfo -> Bool -> ObjFldInfo
mkRelFld (RelInfo rn rTy _ remTab _ isManual) isNullable = case rTy of
  ArrRel ->
    ObjFldInfo (Just "An array relationship") (G.Name $ getRelTxt rn)
    (fromInpValL $ mkSelArgs remTab)
    (G.toGT $ G.toNT $ G.toLT $ G.toNT relTabTy)
  ObjRel ->
    ObjFldInfo (Just "An object relationship") (G.Name $ getRelTxt rn)
    Map.empty
    objRelTy
  where
    objRelTy = bool (G.toGT $ G.toNT relTabTy) (G.toGT relTabTy) isObjRelNullable
    isObjRelNullable = isManual || isNullable
    relTabTy = mkTableTy remTab

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
  mkObjTyInfo (Just desc) (mkTableTy tn) $ mapFromL _fiName flds
  where
    flds = map (either mkPGColFld mkRelFld') allowedFlds
    mkRelFld' (relInfo, _, _, isNullable) = mkRelFld relInfo isNullable
    desc = G.Description $
      "columns and relationships of " <>> tn

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
  ObjFldInfo (Just desc) fldName args ty
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
  ObjFldInfo (Just desc) fldName args ty
  where
    desc = G.Description $ "fetch data from the table: " <> tn
           <<> " using primary key columns"
    fldName = mkTableByPKeyTy tn
    args = fromInpValL $ map colInpVal cols
    ty = G.toGT $ mkTableTy tn
    colInpVal (PGColInfo n typ _) =
      InpValInfo Nothing (mkColName n) $ G.toGT $ G.toNT $ mkScalarTy typ

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
  mkObjTyInfo (Just objDesc) (mkMutRespTy tn) $ mapFromL _fiName
  $ affectedRowsFld : bool [] [returningFld] sel
  where
    objDesc = G.Description $
      "response of any mutation on the table " <>> tn
    affectedRowsFld =
      ObjFldInfo (Just desc) "affected_rows" Map.empty $
      G.toGT $ G.toNT $ mkScalarTy PGInteger
      where
        desc = "number of affected rows by the mutation"
    returningFld =
      ObjFldInfo (Just desc) "returning" Map.empty $
      G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableTy tn
      where
        desc = "data of the affected rows by the mutation"

mkBoolExpInp
  :: QualifiedTable
  -- the fields that are allowed
  -> [SelField]
  -> InpObjTyInfo
mkBoolExpInp tn fields =
  InpObjTyInfo (Just desc) boolExpTy $ Map.fromList
  [(_iviName inpVal, inpVal) | inpVal <- inpValues]
  where
    desc = G.Description $
      "Boolean expression to filter rows from the table " <> tn <<>
      ". All fields are combined with a logical 'AND'."

    -- the type of this boolean expression
    boolExpTy = mkBoolExpTy tn

    -- all the fields of this input object
    inpValues = combinators <> map mkFldExpInp fields

    mk n ty = InpValInfo Nothing n $ G.toGT ty

    boolExpListTy = G.toLT boolExpTy

    combinators =
      [ mk "_not" boolExpTy
      , mk "_and" boolExpListTy
      , mk "_or"  boolExpListTy
      ]

    mkFldExpInp = \case
      Left (PGColInfo colName colTy _) ->
        mk (G.Name $ getPGColTxt colName) (mkCompExpTy colTy)
      Right (RelInfo relName _ _ remTab _ _, _, _, _) ->
        mk (G.Name $ getRelTxt relName) (mkBoolExpTy remTab)

mkPGColInp :: PGColInfo -> InpValInfo
mkPGColInp (PGColInfo colName colTy _) =
  InpValInfo Nothing (G.Name $ getPGColTxt colName) $
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
  InpObjTyInfo (Just desc) (mkUpdSetTy tn) $ fromInpValL $
  map mkPGColInp cols
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
                        InpObjTyInfo (Just desc) (mkUpdIncTy tn) $
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
      InpObjTyInfo (Just appendDesc) (mkJSONOpTy tn appendOp) $
      fromInpValL $ map mkPGColInp jsonbCols

    prependInpObj =
      InpObjTyInfo (Just prependDesc) (mkJSONOpTy tn prependOp) $
      fromInpValL $ map mkPGColInp jsonbCols

    deleteKeyInpObj =
      InpObjTyInfo (Just deleteKeyDesc) (mkJSONOpTy tn deleteKeyOp) $
      fromInpValL $ map deleteKeyInpVal jsonbColNames
    deleteKeyInpVal c = InpValInfo Nothing (G.Name $ getPGColTxt c) $
      G.toGT $ G.NamedType "String"

    deleteElemInpObj =
      InpObjTyInfo (Just deleteElemDesc) (mkJSONOpTy tn deleteElemOp) $
      fromInpValL $ map deleteElemInpVal jsonbColNames
    deleteElemInpVal c = InpValInfo Nothing (G.Name $ getPGColTxt c) $
      G.toGT $ G.NamedType "Int"

    deleteAtPathInpObj =
      InpObjTyInfo (Just deleteAtPathDesc) (mkJSONOpTy tn deleteAtPathOp) $
      fromInpValL $ map deleteAtPathInpVal jsonbColNames
    deleteAtPathInpVal c = InpValInfo Nothing (G.Name $ getPGColTxt c) $
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
      InpValInfo (Just incArgDesc) "_inc" $ G.toGT $ mkUpdIncTy tn

mkJSONOpInpVals :: QualifiedTable -> [PGColInfo] -> [InpValInfo]
mkJSONOpInpVals tn cols = bool jsonbOpArgs [] $ null jsonbCols
  where
    jsonbCols = onlyJSONBCols cols
    jsonbOpArgs = [appendArg, prependArg, deleteKeyArg, deleteElemArg, deleteAtPathArg]

    appendArg =
      InpValInfo (Just appendDesc) appendOp $ G.toGT $ mkJSONOpTy tn appendOp

    prependArg =
      InpValInfo (Just prependDesc) prependOp $ G.toGT $ mkJSONOpTy tn prependOp

    deleteKeyArg =
      InpValInfo (Just deleteKeyDesc) deleteKeyOp $
      G.toGT $ mkJSONOpTy tn deleteKeyOp

    deleteElemArg =
      InpValInfo (Just deleteElemDesc) deleteElemOp $
      G.toGT $ mkJSONOpTy tn deleteElemOp

    deleteAtPathArg =
      InpValInfo (Just deleteAtPathDesc) deleteAtPathOp $
      G.toGT $ mkJSONOpTy tn deleteAtPathOp

mkUpdMutFld
  :: QualifiedTable -> [PGColInfo] -> ObjFldInfo
mkUpdMutFld tn cols =
  ObjFldInfo (Just desc) fldName (fromInpValL inputValues) $
  G.toGT $ mkMutRespTy tn
  where
    inputValues = [filterArg, setArg] <> incArg
                  <> mkJSONOpInpVals tn cols
    desc = G.Description $ "update data of the table: " <>> tn

    fldName = "update_" <> qualTableToName tn

    filterArgDesc = "filter the rows which have to be updated"
    filterArg =
      InpValInfo (Just filterArgDesc) "where" $ G.toGT $
      G.toNT $ mkBoolExpTy tn

    setArgDesc = "sets the columns of the filtered rows to the given values"
    setArg =
      InpValInfo (Just setArgDesc) "_set" $ G.toGT $ mkUpdSetTy tn

    incArg = maybeToList $ mkIncInpVal tn cols

{-

delete_table(
  where : table_bool_exp!
): table_mutation_response

-}

mkDelMutFld
  :: QualifiedTable -> ObjFldInfo
mkDelMutFld tn =
  ObjFldInfo (Just desc) fldName (fromInpValL [filterArg]) $
  G.toGT $ mkMutRespTy tn
  where
    desc = G.Description $ "delete data from the table: " <>> tn

    fldName = "delete_" <> qualTableToName tn

    filterArgDesc = "filter the rows which have to be deleted"
    filterArg =
      InpValInfo (Just filterArgDesc) "where" $ G.toGT $
      G.toNT $ mkBoolExpTy tn

-- table_insert_input
mkInsInpTy :: QualifiedTable -> G.NamedType
mkInsInpTy tn =
  G.NamedType $ qualTableToName tn <> "_insert_input"

-- table_on_conflict
mkOnConflictInpTy :: QualifiedTable -> G.NamedType
mkOnConflictInpTy tn =
  G.NamedType $ qualTableToName tn <> "_on_conflict"

-- table_constraint
mkConstraintInpTy :: QualifiedTable -> G.NamedType
mkConstraintInpTy tn =
  G.NamedType $ qualTableToName tn <> "_constraint"

-- table_column
mkColumnInpTy :: QualifiedTable -> G.NamedType
mkColumnInpTy tn =
  G.NamedType $ qualTableToName tn <> "_column"

{-

input table_insert_input {
  col1: colty1
  .
  .
  coln: coltyn
}

-}

mkInsInp
  :: QualifiedTable -> [PGColInfo] -> InpObjTyInfo
mkInsInp tn cols =
  InpObjTyInfo (Just desc) (mkInsInpTy tn) $ fromInpValL $
  map mkPGColInp cols
  where
    desc = G.Description $
      "input type for inserting data into table " <>> tn

{-

input table_on_conflict {
  action: conflict_action
  constraint: table_constraint!
  update_columns: [table_column!]
}

-}

mkOnConflictInp :: QualifiedTable -> InpObjTyInfo
mkOnConflictInp tn =
  InpObjTyInfo (Just desc) (mkOnConflictInpTy tn) $ fromInpValL
  [actionInpVal, constraintInpVal, updateColumnsInpVal]
  where
    desc = G.Description $
      "on conflict condition type for table " <>> tn

    actionDesc = "action when conflict occurs (deprecated)"

    actionInpVal = InpValInfo (Just actionDesc) (G.Name "action") $
      G.toGT $ G.NamedType "conflict_action"

    constraintInpVal = InpValInfo Nothing (G.Name "constraint") $
      G.toGT $ G.toNT $ mkConstraintInpTy tn

    updateColumnsInpVal = InpValInfo Nothing (G.Name "update_columns") $
      G.toGT $ G.toLT $ G.toNT $ mkColumnInpTy tn
{-

insert_table(
  objects: [table_insert_input!]!
  on_conflict: table_on_conflict
  ): table_mutation_response!
-}

mkInsMutFld
  :: QualifiedTable -> [TableConstraint] -> Bool -> ObjFldInfo
mkInsMutFld tn constraints isUpsertAllowed =
  ObjFldInfo (Just desc) fldName (fromInpValL inputVals) $
  G.toGT $ mkMutRespTy tn
  where
    inputVals = catMaybes [Just objectsArg , onConflictInpVal]
    desc = G.Description $
      "insert data into the table: " <>> tn

    fldName = "insert_" <> qualTableToName tn

    objsArgDesc = "the rows to be inserted"
    objectsArg =
      InpValInfo (Just objsArgDesc) "objects" $ G.toGT $
      G.toNT $ G.toLT $ G.toNT $ mkInsInpTy tn

    uniqueOrPrimaryCons = filter isUniqueOrPrimary constraints
    onConflictInpVal = bool (Just onConflictArg) Nothing
                       (null uniqueOrPrimaryCons || not isUpsertAllowed)

    onConflictDesc = "on conflict condition"
    onConflictArg =
      InpValInfo (Just onConflictDesc) "on_conflict" $ G.toGT $ mkOnConflictInpTy tn

mkConstriantTy :: QualifiedTable -> [TableConstraint] -> EnumTyInfo
mkConstriantTy tn cons = enumTyInfo
  where
    enumTyInfo = EnumTyInfo (Just desc) (mkConstraintInpTy tn) $
                 mapFromL _eviVal $ map (mkConstraintEnumVal . tcName ) cons

    desc = G.Description $
      "unique or primary key constraints on table " <>> tn

    mkConstraintEnumVal (ConstraintName n) =
      EnumValInfo (Just "unique or primary key constraint")
      (G.EnumValue $ G.Name n) False

mkColumnTy :: QualifiedTable -> [PGCol] -> EnumTyInfo
mkColumnTy tn cols = enumTyInfo
  where
    enumTyInfo = EnumTyInfo (Just desc) (mkColumnInpTy tn) $
                 mapFromL _eviVal $ map mkColumnEnumVal cols

    desc = G.Description $
      "columns of table " <>> tn

    mkColumnEnumVal (PGCol col) =
      EnumValInfo (Just "column name") (G.EnumValue $ G.Name col) False

mkConflictActionTy :: Bool -> EnumTyInfo
mkConflictActionTy updAllowed =
  EnumTyInfo (Just desc) ty $ mapFromL _eviVal $
  [enumValIgnore] <> bool [] [enumValUpdate] updAllowed
  where
    desc = G.Description "conflict action"
    ty = G.NamedType "conflict_action"
    enumValIgnore = EnumValInfo (Just "ignore the insert on this row")
                    (G.EnumValue "ignore") False
    enumValUpdate = EnumValInfo (Just "update the row with the given values")
                    (G.EnumValue "update") False

mkOrdByTy :: QualifiedTable -> G.NamedType
mkOrdByTy tn =
  G.NamedType $ qualTableToName tn <> "_order_by"

mkOrdByCtx
  :: QualifiedTable -> [PGColInfo] -> (EnumTyInfo, OrdByResolveCtx)
mkOrdByCtx tn cols =
  (enumTyInfo, resolveCtx)
  where
    enumTyInfo = EnumTyInfo (Just desc) enumTy $
                 mapFromL _eviVal $ map toEnumValInfo enumValsInt
    enumTy = mkOrdByTy tn
    desc = G.Description $
      "ordering options when selecting data from " <>> tn

    toEnumValInfo (v, enumValDesc, _) =
      EnumValInfo (Just $ G.Description enumValDesc) (G.EnumValue v) False

    resolveCtx = Map.fromList $ map toResolveCtxPair enumValsInt

    toResolveCtxPair (v, _, ctx) = ((enumTy, G.EnumValue v), ctx)

    enumValsInt = concatMap mkOrdByEnumsOfCol cols


mkOrdByEnumsOfCol
  :: PGColInfo
  -> [(G.Name, Text, (PGColInfo, OrdTy, NullsOrder))]
mkOrdByEnumsOfCol colInfo@(PGColInfo col _ _) =
  [ ( colN <> "_asc"
    , "in the ascending order of " <> col <<> ", nulls last"
    , (colInfo, OAsc, NLast)
    )
  , ( colN <> "_desc"
    , "in the descending order of " <> col <<> ", nulls last"
    , (colInfo, ODesc, NLast)
    )
  , ( colN <> "_asc_nulls_first"
    , "in the ascending order of " <> col <<> ", nulls first"
    , (colInfo, OAsc, NFirst)
    )
  , ( colN <> "_desc_nulls_first"
    , "in the descending order of " <> col <<> ", nulls first"
    ,(colInfo, ODesc, NFirst)
    )
  ]
  where
    colN = pgColToFld col
    pgColToFld = G.Name . getPGColTxt

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

mkOnConflictTypes
  :: QualifiedTable -> [TableConstraint] -> [PGCol] -> Bool -> [TypeInfo]
mkOnConflictTypes tn c updCols isUpsertAllowed =
  bool tyInfos [] (null constraints || not isUpsertAllowed)
  where
    tyInfos = [ TIEnum $ mkConflictActionTy isUpdAllowed
              , TIEnum $ mkConstriantTy tn constraints
              , TIEnum $ mkColumnTy tn updCols
              , TIInpObj $ mkOnConflictInp tn
              ]
    constraints = filter isUniqueOrPrimary c
    isUpdAllowed = not $ null updCols

mkGCtxRole'
  :: QualifiedTable
  -- insert cols, is upsert allowed
  -> Maybe ([PGColInfo], Bool)
  -- select permission
  -> Maybe [SelField]
  -- update cols
  -> Maybe [PGColInfo]
  -- delete cols
  -> Maybe ()
  -- primary key columns
  -> [PGColInfo]
  -- constraints
  -> [TableConstraint]
  -> TyAgg
mkGCtxRole' tn insPermM selFldsM updColsM delPermM pkeyCols constraints =
  TyAgg (mkTyInfoMap allTypes) fieldMap ordByEnums

  where

    ordByEnums = fromMaybe Map.empty ordByResCtxM
    updCols = maybe [] (map pgiName) updColsM
    onConflictTypes = mkOnConflictTypes tn constraints updCols $
      or $ fmap snd insPermM
    jsonOpTys = fromMaybe [] updJSONOpInpObjTysM

    allTypes = onConflictTypes <> jsonOpTys <> catMaybes
      [ TIInpObj <$> insInpObjM
      , TIInpObj <$> updSetInpObjM
      , TIInpObj <$> updIncInpObjM
      , TIInpObj <$> boolExpInpObjM
      , TIObj <$> mutRespObjM
      , TIObj <$> selObjM
      , TIEnum <$> ordByTyInfoM
      ]

    fieldMap = Map.unions $ catMaybes
               [ insInpObjFldsM, updSetInpObjFldsM, boolExpInpObjFldsM
               , selObjFldsM, Just selByPKeyObjFlds
               ]

    nameFromSelFld = \case
      Left colInfo -> G.Name $ getPGColTxt $ pgiName colInfo
      Right (relInfo, _, _, _) -> G.Name $ getRelTxt $ riName relInfo

    -- helper
    mkColFldMap ty = mapFromL ((ty,) . nameFromSelFld) . map Left

    insColsM = fst <$> insPermM
    -- insert input type
    insInpObjM = mkInsInp tn <$> insColsM
    -- fields used in insert input object
    insInpObjFldsM = mkColFldMap (mkInsInpTy tn) <$> insColsM

    -- update set input type
    updSetInpObjM = mkUpdSetInp tn <$> updColsM
    -- update increment input type
    updIncInpObjM = mkUpdIncInp tn updColsM
    -- update json operator input type
    updJSONOpInpObjsM = mkUpdJSONOpInp tn <$> updColsM
    updJSONOpInpObjTysM = map TIInpObj <$> updJSONOpInpObjsM
    -- fields used in set input object
    updSetInpObjFldsM = mkColFldMap (mkUpdSetTy tn) <$> updColsM

    -- boolexp input type
    boolExpInpObjM = case selFldsM of
      Just selFlds -> Just $ mkBoolExpInp tn selFlds
      -- no select permission
      Nothing ->
        -- but update/delete is defined
        if isJust updColsM || isJust delPermM
        then Just $ mkBoolExpInp tn []
        else Nothing

    -- helper
    mkFldMap ty = mapFromL ((ty,) . nameFromSelFld)
    -- the fields used in bool exp
    boolExpInpObjFldsM = mkFldMap (mkBoolExpTy tn) <$> selFldsM

    -- mut resp obj
    mutRespObjM =
      if isJust insColsM || isJust updColsM || isJust delPermM
      then Just $ mkMutRespObj tn $ isJust selFldsM
      else Nothing

    -- table obj
    selObjM = mkTableObj tn <$> selFldsM
    -- the fields used in table object
    selObjFldsM = mkFldMap (mkTableTy tn) <$> selFldsM
    -- the field used in table_by_pkey object
    selByPKeyObjFlds = Map.fromList $ flip map pkeyCols $
      \pgi@(PGColInfo col ty _) -> ((mkScalarTy ty, mkColName col), Left pgi)

    ordByEnumsCtxM = mkOrdByCtx tn . lefts <$> selFldsM

    (ordByTyInfoM, ordByResCtxM) = case ordByEnumsCtxM of
      (Just (a, b)) -> (Just a, Just b)
      Nothing       -> (Nothing, Nothing)

getRootFldsRole'
  :: QualifiedTable
  -> [PGCol]
  -> [TableConstraint]
  -> FieldInfoMap
  -> Maybe (QualifiedTable, [T.Text], Bool) -- insert perm
  -> Maybe (S.BoolExp, Maybe Int, [T.Text]) -- select filter
  -> Maybe ([PGCol], S.BoolExp, [T.Text]) -- update filter
  -> Maybe (S.BoolExp, [T.Text]) -- delete filter
  -> RootFlds
getRootFldsRole' tn primCols constraints fields insM selM updM delM =
  RootFlds mFlds
  where
    mFlds = mapFromL (either _fiName _fiName . snd) $ catMaybes
            [ getInsDet <$> insM, getSelDet <$> selM
            , getUpdDet <$> updM, getDelDet <$> delM
            , getPKeySelDet selM $ getColInfos primCols colInfos
            ]
    colInfos = fst $ validPartitionFieldInfoMap fields
    getUpdPermForIns (c, b, _) = (c, b)
    getUpdPermHdrs (_, _, h) = h
    getInsDet (vn, hdrs, isUpsertAllowed) =
      let updPermForInsM = getUpdPermForIns <$> updM
          totalHdrs = hdrs `L.union` maybe [] getUpdPermHdrs updM
      in ( OCInsert tn vn (map pgiName colInfos) updPermForInsM totalHdrs
         , Right $ mkInsMutFld tn constraints isUpsertAllowed
         )
    getUpdDet (updCols, updFltr, hdrs) =
      ( OCUpdate tn updFltr hdrs
      , Right $ mkUpdMutFld tn $ getColInfos updCols colInfos
      )
    getDelDet (delFltr, hdrs) =
      (OCDelete tn delFltr hdrs, Right $ mkDelMutFld tn)
    getSelDet (selFltr, pLimit, hdrs) =
      (OCSelect tn selFltr pLimit hdrs, Left $ mkSelFld tn)

    getPKeySelDet Nothing _ = Nothing
    getPKeySelDet _ [] = Nothing
    getPKeySelDet (Just (selFltr, _, hdrs)) pCols = Just
      (OCSelectPkey tn selFltr hdrs, Left $ mkSelFldPKey tn pCols)

-- getRootFlds
--   :: TableCache
--   -> Map.HashMap RoleName RootFlds
-- getRootFlds tables =
--   foldr (Map.unionWith mappend . getRootFldsTable) Map.empty $
--   Map.elems tables

-- gets all the selectable fields (cols and rels) of a
-- table for a role

getSelFlds
  :: (MonadError QErr m)
  => TableCache
  -- all the fields of a table
  -> FieldInfoMap
  -- role and its permission
  -> RoleName -> SelPermInfo
  -> m [SelField]
getSelFlds tableCache fields role selPermInfo =
  fmap catMaybes $ forM (toValidFieldInfos fields) $ \case
    FIColumn pgColInfo ->
      return $ fmap Left $ bool Nothing (Just pgColInfo) $
      Set.member (pgiName pgColInfo) allowedCols
    FIRelationship relInfo -> do
      remTableInfo <- getTabInfo $ riRTable relInfo
      let remTableSelPermM =
            Map.lookup role (tiRolePermInfoMap remTableInfo) >>= _permSel
      return $ flip fmap remTableSelPermM $
        \rmSelPermM -> Right ( relInfo
                             , spiFilter rmSelPermM
                             , spiLimit rmSelPermM
                             , isRelNullable fields relInfo
                             )
  where
    allowedCols = spiCols selPermInfo
    getTabInfo tn =
      onNothing (Map.lookup tn tableCache) $
      throw500 $ "remote table not found: " <>> tn

mkGCtxRole
  :: (MonadError QErr m)
  => TableCache
  -> QualifiedTable
  -> FieldInfoMap
  -> [PGCol]
  -> [TableConstraint]
  -> RoleName
  -> RolePermInfo
  -> m (TyAgg, RootFlds)
mkGCtxRole tableCache tn fields pCols constraints role permInfo = do
  selFldsM <- mapM (getSelFlds tableCache fields role) $ _permSel permInfo
  let insColsM = ((colInfos,) . ipiAllowUpsert) <$> _permIns permInfo
      updColsM = filterColInfos . upiCols <$> _permUpd permInfo
      tyAgg = mkGCtxRole' tn insColsM selFldsM updColsM
              (void $ _permDel permInfo) pColInfos constraints
      rootFlds = getRootFldsRole tn pCols constraints fields permInfo
  return (tyAgg, rootFlds)
  where
    colInfos = fst $ validPartitionFieldInfoMap fields
    pColInfos = getColInfos pCols colInfos
    filterColInfos allowedSet =
      filter ((`Set.member` allowedSet) . pgiName) colInfos

getRootFldsRole
  :: QualifiedTable
  -> [PGCol]
  -> [TableConstraint]
  -> FieldInfoMap
  -> RolePermInfo
  -> RootFlds
getRootFldsRole tn pCols constraints fields (RolePermInfo insM selM updM delM) =
  getRootFldsRole' tn pCols constraints fields
  (mkIns <$> insM) (mkSel <$> selM)
  (mkUpd <$> updM) (mkDel <$> delM)
  where
    mkIns i = (ipiView i, ipiRequiredHeaders i, ipiAllowUpsert i)
    mkSel s = (spiFilter s, spiLimit s, spiRequiredHeaders s)
    mkUpd u = ( Set.toList $ upiCols u
              , upiFilter u
              , upiRequiredHeaders u
              )
    mkDel d = (dpiFilter d, dpiRequiredHeaders d)

mkGCtxMapTable
  :: (MonadError QErr m)
  => TableCache
  -> TableInfo
  -> m (Map.HashMap RoleName (TyAgg, RootFlds))
mkGCtxMapTable tableCache (TableInfo tn _ fields rolePerms constraints pkeyCols _) = do
  m <- Map.traverseWithKey (mkGCtxRole tableCache tn fields pkeyCols validConstraints) rolePerms
  let adminCtx = mkGCtxRole' tn (Just (colInfos, True))
                 (Just selFlds) (Just colInfos) (Just ())
                 pkeyColInfos validConstraints
  return $ Map.insert adminRole (adminCtx, adminRootFlds) m
  where
    validConstraints = mkValidConstraints constraints
    colInfos = fst $ validPartitionFieldInfoMap fields
    allCols = map pgiName colInfos
    pkeyColInfos = getColInfos pkeyCols colInfos
    selFlds = flip map (toValidFieldInfos fields) $ \case
      FIColumn pgColInfo     -> Left pgColInfo
      FIRelationship relInfo -> Right (relInfo, noFilter, Nothing, isRelNullable fields relInfo)
    noFilter = S.BELit True
    adminRootFlds =
      getRootFldsRole' tn pkeyCols constraints fields
      (Just (tn, [], True)) (Just (noFilter, Nothing, []))
      (Just (allCols, noFilter, [])) (Just (noFilter, []))

mkScalarTyInfo :: PGColType -> ScalarTyInfo
mkScalarTyInfo = ScalarTyInfo Nothing

type GCtxMap = Map.HashMap RoleName GCtx

mkGCtxMap
  :: (MonadError QErr m)
  => TableCache -> m (Map.HashMap RoleName GCtx)
mkGCtxMap tableCache = do
  typesMapL <- mapM (mkGCtxMapTable tableCache) $
               filter tableFltr $ Map.elems tableCache
  let typesMap = foldr (Map.unionWith mappend) Map.empty typesMapL
  return $ Map.map (uncurry mkGCtx) typesMap
  where
    tableFltr ti = not (tiSystemDefined ti)
                   && isValidTableName (tiName ti)

mkGCtx :: TyAgg -> RootFlds -> GCtx
mkGCtx (TyAgg tyInfos fldInfos ordByEnums) (RootFlds flds) =
  let queryRoot = mkObjTyInfo (Just "query root") (G.NamedType "query_root") $
                  mapFromL _fiName (schemaFld:typeFld:qFlds)
      colTys    = Set.toList $ Set.fromList $ map pgiType $
                  lefts $ Map.elems fldInfos
      scalarTys = map (TIScalar . mkScalarTyInfo) colTys
      compTys   = map (TIInpObj . mkCompExpInp) colTys
      allTys    = Map.union tyInfos $ mkTyInfoMap $
                  catMaybes [ Just $ TIObj queryRoot
                            , TIObj <$> mutRootM
                            , TIObj <$> subRootM
                            ] <>
                  scalarTys <> compTys <> defaultTypes
  -- for now subscription root is query root
  in GCtx allTys fldInfos ordByEnums queryRoot mutRootM (Just queryRoot) $
     Map.map fst flds
  where

    mkMutRoot =
      mkObjTyInfo (Just "mutation root") (G.NamedType "mutation_root") .
      mapFromL _fiName

    mutRootM = bool (Just $ mkMutRoot mFlds) Nothing $ null mFlds

    mkSubRoot =
      mkObjTyInfo (Just "subscription root") (G.NamedType "subscription_root") .
      mapFromL _fiName

    subRootM = bool (Just $ mkSubRoot qFlds) Nothing $ null qFlds

    (qFlds, mFlds) = partitionEithers $ map snd $ Map.elems flds

    schemaFld = ObjFldInfo Nothing "__schema" Map.empty $ G.toGT $
                G.toNT $ G.NamedType "__Schema"

    typeFld = ObjFldInfo Nothing "__type" typeFldArgs $ G.toGT $
              G.NamedType "__Type"
      where
        typeFldArgs = mapFromL _iviName [
          InpValInfo (Just "name of the type") "name"
          $ G.toGT $ G.toNT $ G.NamedType "String"
          ]

getGCtx :: RoleName -> Map.HashMap RoleName GCtx -> GCtx
getGCtx rn =
  fromMaybe (mkGCtx mempty mempty) . Map.lookup rn
