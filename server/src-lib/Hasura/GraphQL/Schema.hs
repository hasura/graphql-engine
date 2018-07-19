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
  ( mkGCtxMap
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
-- defaultTypes = undefined

type OpCtxMap = Map.HashMap G.Name OpCtx

data OpCtx
  -- tn, vn, cols, req hdrs
  = OCInsert QualifiedTable QualifiedTable [PGCol] [T.Text]
  -- tn, filter exp, req hdrs
  | OCSelect QualifiedTable S.BoolExp [T.Text]
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

type SelField = Either PGColInfo (RelInfo, S.BoolExp)

qualTableToName :: QualifiedTable -> G.Name
qualTableToName = G.Name <$> \case
  QualifiedTable (SchemaName "public") tn -> getTableTxt tn
  QualifiedTable sn tn -> getSchemaTxt sn <> "_" <> getTableTxt tn

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

mkCompExpInp :: PGColType -> InpObjTyInfo
mkCompExpInp colTy =
  InpObjTyInfo (Just tyDesc) (mkCompExpTy colTy) $ fromInpValL $ concat
  [ map (mk colScalarTy) typedOps
  , map (mk $ G.toLT colScalarTy) listOps
  , bool [] (map (mk $ mkScalarTy PGText) stringOps) isStringTy
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

mkPGColFld :: PGColInfo -> ObjFldInfo
mkPGColFld (PGColInfo colName colTy) =
  ObjFldInfo Nothing n Map.empty ty
  where
    n  = G.Name $ getPGColTxt colName
    ty = G.toGT $ mkScalarTy colTy

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
mkRelFld :: RelInfo -> ObjFldInfo
mkRelFld (RelInfo rn rTy _ remTab _) = case rTy of
  ArrRel ->
    ObjFldInfo (Just "An array relationship") (G.Name $ getRelTxt rn)
    (fromInpValL $ mkSelArgs remTab)
    (G.toGT $ G.toNT $ G.toLT $ G.toNT relTabTy)
  ObjRel ->
    ObjFldInfo (Just "An object relationship") (G.Name $ getRelTxt rn)
    Map.empty
    (G.toGT relTabTy)
  where
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
    flds = map (either mkPGColFld (mkRelFld . fst)) allowedFlds
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

-- table_mutation_response
mkMutRespTy :: QualifiedTable -> G.NamedType
mkMutRespTy tn =
  G.NamedType $ qualTableToName tn <> "_mutation_response"

{-
type table_mutation_response {
  affected_rows: Int!
  returning: [table_no_rels!]!
}
-}
mkMutRespObj
  :: QualifiedTable
  -> ObjTyInfo
mkMutRespObj tn =
  mkObjTyInfo (Just objDesc) (mkMutRespTy tn) $ mapFromL _fiName
  [affectedRowsFld, returningFld]
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
      G.toGT $ G.toNT $ G.toLT $ G.toNT $ mkTableNoRelsTy tn
      where
        desc = "data of the affected rows by the mutation"

-- table_no_rels
mkTableNoRelsTy :: QualifiedTable -> G.NamedType
mkTableNoRelsTy tn =
  G.NamedType $ qualTableToName tn <> "_no_rels"

mkTableNoRelsObj
  :: QualifiedTable
  -> [SelField]
  -> ObjTyInfo
mkTableNoRelsObj tn fields =
  mkObjTyInfo (Just desc) (mkTableNoRelsTy tn) $ mapFromL _fiName pgCols
  where
    pgCols = map mkPGColFld $ lefts fields
    desc = G.Description $
      "only postgres columns (no relationships) from " <>> tn

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
      Left (PGColInfo colName colTy) ->
        mk (G.Name $ getPGColTxt colName) (mkCompExpTy colTy)
      Right (RelInfo relName _ _ remTab _, _) ->
        mk (G.Name $ getRelTxt relName) (mkBoolExpTy remTab)

mkPGColInp :: PGColInfo -> InpValInfo
mkPGColInp (PGColInfo colName colTy) =
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
mkUpdInp
  :: QualifiedTable -> [PGColInfo] -> InpObjTyInfo
mkUpdInp tn cols  =
  InpObjTyInfo (Just desc) (mkUpdSetTy tn) $ fromInpValL $
  map mkPGColInp cols
  where
    desc = G.Description $
      "input type for updating data in table " <>> tn

{-

update_table(
  where : table_bool_exp!
  _set  : table_set_input!
): table_mutation_response

-}

mkUpdMutFld
  :: QualifiedTable -> ObjFldInfo
mkUpdMutFld tn =
  ObjFldInfo (Just desc) fldName (fromInpValL [filterArg, setArg]) $
  G.toGT $ mkMutRespTy tn
  where
    desc = G.Description $ "update data of the table: " <>> tn

    fldName = "update_" <> qualTableToName tn

    filterArgDesc = "filter the rows which have to be updated"
    filterArg =
      InpValInfo (Just filterArgDesc) "where" $ G.toGT $
      G.toNT $ mkBoolExpTy tn

    setArgDesc = "sets the columns of the filtered rows to the given values"
    setArg =
      InpValInfo (Just setArgDesc) "_set" $ G.toGT $
      G.toNT $ mkUpdSetTy tn

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
  action: conflict_action!
  constraint: table_constraint
}

-}

mkOnConflictInp :: QualifiedTable -> InpObjTyInfo
mkOnConflictInp tn =
  InpObjTyInfo (Just desc) (mkOnConflictInpTy tn) $ fromInpValL
  [actionInpVal, constraintInpVal]
  where
    desc = G.Description $
      "on conflict condition type for table " <>> tn

    actionInpVal = InpValInfo Nothing (G.Name "action") $
      G.toGT $ G.toNT $ G.NamedType "conflict_action"

    constraintInpVal = InpValInfo Nothing (G.Name "constraint") $
      G.toGT $ mkConstraintInpTy tn
{-

insert_table(
  objects: [table_insert_input!]!
  on_conflict: table_on_conflict
  ): table_mutation_response!
-}

mkInsMutFld
  :: QualifiedTable -> [TableConstraint] -> ObjFldInfo
mkInsMutFld tn constraints =
  ObjFldInfo (Just desc) fldName (fromInpValL inputVals) $
  G.toGT $ mkMutRespTy tn
  where
    inputVals = catMaybes [ Just objectsArg
                          , onConflictInpVal
                          ]
    desc = G.Description $
      "insert data into the table: " <>> tn

    fldName = "insert_" <> qualTableToName tn

    objsArgDesc = "the rows to be inserted"
    objectsArg =
      InpValInfo (Just objsArgDesc) "objects" $ G.toGT $
      G.toNT $ G.toLT $ G.toNT $ mkInsInpTy tn

    onConflictInpVal = case filter isUniqueOrPrimary constraints of
      [] -> Nothing
      _  -> Just onConflictArg

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

mkConflictActionTy :: EnumTyInfo
mkConflictActionTy = EnumTyInfo (Just desc) ty $ mapFromL _eviVal
                     [enumValIgnore, enumValUpdate]
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
mkOrdByEnumsOfCol colInfo@(PGColInfo col _) =
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

data RootFlds
  = RootFlds
  { _taMutation :: !(Map.HashMap G.Name (OpCtx, Either ObjFldInfo ObjFldInfo))
  } deriving (Show, Eq)

instance Semigroup RootFlds where
  (RootFlds m1) <> (RootFlds m2)
    = RootFlds (Map.union m1 m2)

instance Monoid RootFlds where
  mempty = RootFlds Map.empty
  mappend  = (<>)

mkOnConflictTypes :: QualifiedTable -> [TableConstraint] -> [TypeInfo]
mkOnConflictTypes tn c = case filter isUniqueOrPrimary c of
  [] -> []
  constraints -> [ TIEnum mkConflictActionTy
                 , TIEnum $ mkConstriantTy tn constraints
                 , TIInpObj $ mkOnConflictInp tn
                 ]

mkGCtxRole'
  :: QualifiedTable
  -- insert cols
  -> Maybe [PGColInfo]
  -- select permission
  -> Maybe [SelField]
  -- update cols
  -> Maybe [PGColInfo]
  -- delete cols
  -> Maybe ()
  -- constraints
  -> [TableConstraint]
  -> TyAgg
mkGCtxRole' tn insColsM selFldsM updColsM delPermM constraints =
  TyAgg (mkTyInfoMap allTypes) fieldMap ordByEnums

  where

    ordByEnums = fromMaybe Map.empty ordByResCtxM
    onConflictTypes = mkOnConflictTypes tn constraints

    allTypes = onConflictTypes <> catMaybes
      [ TIInpObj <$> insInpObjM
      , TIInpObj <$> updSetInpObjM
      , TIInpObj <$> boolExpInpObjM
      , TIObj <$> noRelsObjM
      , TIObj <$> mutRespObjM
      , TIObj <$> selObjM
      , TIEnum <$> ordByTyInfoM
      ]

    fieldMap = Map.unions $ catMaybes
               [ insInpObjFldsM, updSetInpObjFldsM, boolExpInpObjFldsM
               , noRelsObjFldsM, selObjFldsM
               ]

    nameFromSelFld = \case
      Left colInfo -> G.Name $ getPGColTxt $ pgiName colInfo
      Right (relInfo, _) -> G.Name $ getRelTxt $ riName relInfo

    -- helper
    mkColFldMap ty = mapFromL ((ty,) . nameFromSelFld) . map Left

    -- insert input type
    insInpObjM = mkInsInp tn <$> insColsM
    -- fields used in insert input object
    insInpObjFldsM = mkColFldMap (mkInsInpTy tn) <$> insColsM

    -- update set input type
    updSetInpObjM = mkUpdInp tn <$> updColsM
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

    -- no rels obj
    noRelsObjM =
      if isJust insColsM || isJust updColsM || isJust delPermM
      then Just $ mkTableNoRelsObj tn $ fromMaybe [] selFldsM
      else Nothing
    -- the fields used in returning object
    noRelsObjFldsM = const (
      mkColFldMap (mkTableNoRelsTy tn) $ lefts $ fromMaybe [] selFldsM
      ) <$> noRelsObjM

    -- mut resp obj (only when noRelsObjM is needed)
    mutRespObjM = const (mkMutRespObj tn) <$> noRelsObjM

    -- table obj
    selObjM = mkTableObj tn <$> selFldsM
    -- the fields used in table object
    selObjFldsM = mkFldMap (mkTableTy tn) <$> selFldsM

    ordByEnumsCtxM = mkOrdByCtx tn . lefts <$> selFldsM

    (ordByTyInfoM, ordByResCtxM) = case ordByEnumsCtxM of
      (Just (a, b)) -> (Just a, Just b)
      Nothing       -> (Nothing, Nothing)

getRootFldsRole'
  :: QualifiedTable
  -> [TableConstraint]
  -> FieldInfoMap
  -> Maybe (QualifiedTable, [T.Text]) -- insert view
  -> Maybe (S.BoolExp, [T.Text]) -- select filter
  -> Maybe (S.BoolExp, [T.Text]) -- update filter
  -> Maybe (S.BoolExp, [T.Text]) -- delete filter
  -> RootFlds
getRootFldsRole' tn constraints fields insM selM updM delM =
  RootFlds mFlds
  where
    mFlds = mapFromL (either _fiName _fiName . snd) $ catMaybes
            [ getInsDet <$> insM, getSelDet <$> selM
            , getUpdDet <$> updM, getDelDet <$> delM]
    colInfos = fst $ partitionFieldInfos $ Map.elems fields
    getInsDet (vn, hdrs) =
      (OCInsert tn vn (map pgiName colInfos) hdrs, Right $ mkInsMutFld tn constraints)
    getUpdDet (updFltr, hdrs) =
      (OCUpdate tn updFltr hdrs, Right $ mkUpdMutFld tn)
    getDelDet (delFltr, hdrs) =
      (OCDelete tn delFltr hdrs, Right $ mkDelMutFld tn)
    getSelDet (selFltr, hdrs) =
      (OCSelect tn selFltr hdrs, Left $ mkSelFld tn)

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
  fmap catMaybes $ forM (Map.elems fields) $ \case
    FIColumn pgColInfo ->
      return $ fmap Left $ bool Nothing (Just pgColInfo) $
      Set.member (pgiName pgColInfo) allowedCols
    FIRelationship relInfo -> do
      remTableInfo <- getTabInfo $ riRTable relInfo
      let remTableSelPermM =
            Map.lookup role (tiRolePermInfoMap remTableInfo) >>= _permSel
      return $ fmap (Right . (relInfo,) . spiFilter) remTableSelPermM
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
  -> [TableConstraint]
  -> RoleName
  -> RolePermInfo
  -> m (TyAgg, RootFlds)
mkGCtxRole tableCache tn fields constraints role permInfo = do
  selFldsM <- mapM (getSelFlds tableCache fields role) $ _permSel permInfo
  let insColsM = const colInfos <$> _permIns permInfo
      updColsM = filterColInfos . upiCols <$> _permUpd permInfo
      tyAgg = mkGCtxRole' tn insColsM selFldsM updColsM
              (void $ _permDel permInfo) constraints
      rootFlds = getRootFldsRole tn constraints fields permInfo
  return (tyAgg, rootFlds)
  where
    colInfos = fst $ partitionFieldInfos $ Map.elems fields
    filterColInfos allowedSet =
      filter ((`Set.member` allowedSet) . pgiName) colInfos

getRootFldsRole
  :: QualifiedTable
  -> [TableConstraint]
  -> FieldInfoMap
  -> RolePermInfo
  -> RootFlds
getRootFldsRole tn constraints fields (RolePermInfo insM selM updM delM) =
  getRootFldsRole' tn constraints fields
  (mkIns <$> insM) (mkSel <$> selM)
  (mkUpd <$> updM) (mkDel <$> delM)
  where
    mkIns i = (ipiView i, ipiRequiredHeaders i)
    mkSel s = (spiFilter s, spiRequiredHeaders s)
    mkUpd u = (upiFilter u, upiRequiredHeaders u)
    mkDel d = (dpiFilter d, dpiRequiredHeaders d)

mkGCtxMapTable
  :: (MonadError QErr m)
  => TableCache
  -> TableInfo
  -> m (Map.HashMap RoleName (TyAgg, RootFlds))
mkGCtxMapTable tableCache (TableInfo tn _ fields rolePerms constraints) = do
  m <- Map.traverseWithKey (mkGCtxRole tableCache tn fields constraints) rolePerms
  let adminCtx = mkGCtxRole' tn (Just colInfos)
                 (Just selFlds) (Just colInfos) (Just ()) constraints
  return $ Map.insert adminRole (adminCtx, adminRootFlds) m
  where
    colInfos = fst $ partitionFieldInfos $ Map.elems fields
    selFlds = flip map (Map.elems fields) $ \case
      FIColumn pgColInfo     -> Left pgColInfo
      FIRelationship relInfo -> Right (relInfo, noFilter)
    noFilter = S.BELit True
    adminRootFlds =
      getRootFldsRole' tn constraints fields (Just (tn, [])) (Just (noFilter, []))
      (Just (noFilter, [])) (Just (noFilter, []))

mkScalarTyInfo :: PGColType -> ScalarTyInfo
mkScalarTyInfo = ScalarTyInfo Nothing

type GCtxMap = Map.HashMap RoleName GCtx

mkGCtxMap
  :: (MonadError QErr m)
  => TableCache -> m (Map.HashMap RoleName GCtx)
mkGCtxMap tableCache = do
  typesMapL <- mapM (mkGCtxMapTable tableCache) $
               filter (not . tiSystemDefined) $ Map.elems tableCache
  let typesMap = foldr (Map.unionWith mappend) Map.empty typesMapL
  return $ Map.map (uncurry mkGCtx) typesMap

mkGCtx :: TyAgg -> RootFlds -> GCtx
mkGCtx (TyAgg tyInfos fldInfos ordByEnums) (RootFlds flds) =
  let queryRoot = mkObjTyInfo (Just "query root") (G.NamedType "query_root") $
                  mapFromL _fiName (schemaFld:typeFld:qFlds)
      colTys    = Set.toList $ Set.fromList $ map pgiType $
                  lefts $ Map.elems fldInfos
      scalarTys = map (TIScalar . mkScalarTyInfo) colTys
      compTys   = map (TIInpObj . mkCompExpInp) colTys
      allTys    = Map.union tyInfos $ mkTyInfoMap $
                  catMaybes [Just $ TIObj queryRoot, TIObj <$> mutRootM] <>
                  scalarTys <> compTys <> defaultTypes
  in GCtx allTys fldInfos ordByEnums queryRoot mutRootM $ Map.map fst flds
  where

    mkMutRoot =
      mkObjTyInfo (Just "mutation root") (G.NamedType "mutation_root") .
      mapFromL _fiName

    mutRootM = bool (Just $ mkMutRoot mFlds) Nothing $ null mFlds

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
