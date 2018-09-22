{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Resolve.Mutation
  ( convertUpdate
  , convertInsert
  , convertDelete
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.HashSet                      as Set
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Hasura.RQL.DML.Delete             as RD
import qualified Hasura.RQL.DML.Insert             as RI
import qualified Hasura.RQL.DML.Returning          as RR
import qualified Hasura.RQL.DML.Select             as RS
import qualified Hasura.RQL.DML.Update             as RU

import qualified Hasura.SQL.DML                    as S

import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Resolve.Select     (fromSelSet)
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

withSelSet :: (Monad m) => SelSet -> (Field -> m a) -> m (Map.HashMap Text a)
withSelSet selSet f =
  fmap (Map.fromList . toList) $ forM selSet $ \fld -> do
    res <- f fld
    return (G.unName $ G.unAlias $ _fAlias fld, res)

convertReturning
  :: QualifiedTable -> G.NamedType -> SelSet -> Convert RS.SelectData
convertReturning qt ty selSet = do
  annFlds <- fromSelSet ty selSet
  return $ RS.SelectData annFlds qt frmExpM
    (S.BELit True, Nothing) Nothing [] Nothing Nothing False
  where
    frmExpM = Just $ S.FromExp $ pure $
              S.FIIden $ qualTableToAliasIden qt

convertMutResp
  :: QualifiedTable -> G.NamedType -> SelSet -> Convert RR.MutFlds
convertMutResp qt ty selSet =
  withSelSet selSet $ \fld ->
    case _fName fld of
      "__typename"    -> return $ RR.MExp $ G.unName $ G.unNamedType ty
      "affected_rows" -> return RR.MCount
      _ -> fmap RR.MRet $ convertReturning qt (_fType fld) $ _fSelSet fld

convertRowObj
  :: (MonadError QErr m, MonadState PrepArgs m)
  => AnnGValue
  -> m [(PGCol, S.SQLExp)]
convertRowObj val =
  flip withObject val $ \_ obj -> forM (Map.toList obj) $ \(k, v) -> do
    prepExpM <- asPGColValM v >>= mapM prepare
    let prepExp = fromMaybe (S.SEUnsafe "NULL") prepExpM
    return (PGCol $ G.unName k, prepExp)

mkConflictClause :: RI.ConflictCtx -> RI.ConflictClauseP1
mkConflictClause (RI.CCDoNothing constrM) =
  RI.CP1DoNothing $ fmap RI.Constraint constrM
mkConflictClause (RI.CCUpdate constr updCols) =
  RI.CP1Update (RI.Constraint constr) updCols

parseAction
  :: (MonadError QErr m)
  => AnnGObject -> m (Maybe ConflictAction)
parseAction obj =
  mapM parseVal $ Map.lookup "action" obj
  where
    parseVal val = do
      (enumTy, enumVal) <- asEnumVal val
      withPathK "action" $ case G.unName $ G.unEnumValue enumVal of
        "ignore" -> return CAIgnore
        "update" -> return CAUpdate
        _ -> throw500 $
          "only \"ignore\" and \"updated\" allowed for enum type "
          <> showNamedTy enumTy

parseConstraint
  :: (MonadError QErr m)
  => AnnGObject -> m ConstraintName
parseConstraint obj = do
  v <- onNothing (Map.lookup "constraint" obj) $ throw500
    "\"constraint\" is expected, but not found"
  parseVal v
  where
    parseVal v = do
      (_, enumVal) <- asEnumVal v
      return $ ConstraintName $ G.unName $ G.unEnumValue enumVal

parseUpdCols
  :: (MonadError QErr m)
  => AnnGObject -> m (Maybe [PGCol])
parseUpdCols obj =
  mapM parseVal $ Map.lookup "update_columns" obj
  where
    parseVal val = flip withArray val $ \_ enumVals ->
      forM enumVals $ \eVal -> do
        (_, v) <- asEnumVal eVal
        return $ PGCol $ G.unName $ G.unEnumValue v

parseOnConflict
  :: (MonadError QErr m)
  => [PGCol] -> AnnGValue -> m RI.ConflictCtx
parseOnConflict inpCols val =
  flip withObject val $ \_ obj -> do
    actionM <- parseAction obj
    constraint <- parseConstraint obj
    updColsM <- parseUpdCols obj
    -- consider "action" if "update_columns" is not mentioned
    return $ case (updColsM, actionM) of
      (Just [], _)             -> RI.CCDoNothing $ Just constraint
      (Just cols, _)           -> RI.CCUpdate constraint cols
      (Nothing, Just CAIgnore) -> RI.CCDoNothing $ Just constraint
      (Nothing, _)             -> RI.CCUpdate constraint inpCols

convertInsert
  :: RoleName
  -> (QualifiedTable, QualifiedTable) -- table, view
  -> [PGCol] -- all the columns in this table
  -> Field -- the mutation field
  -> Convert RespTx
convertInsert role (tn, vn) tableCols fld = do
  insTuples    <- withArg arguments "objects" asRowExps
  let inpCols = Set.toList $ Set.fromList $ concatMap fst insTuples
  conflictCtxM <- withArgM arguments "on_conflict" $ parseOnConflict inpCols
  let onConflictM = fmap mkConflictClause conflictCtxM
  mutFlds <- convertMutResp tn (_fType fld) $ _fSelSet fld
  args <- get
  let rows = map snd insTuples
      p1Query = RI.InsertQueryP1 tn vn tableCols rows onConflictM mutFlds
      p1 = (p1Query, args)
  return $
      bool (RI.nonAdminInsert p1) (RI.insertP2 p1) $ isAdmin role
  where
    arguments = _fArguments fld
    asRowExps = withArray (const $ mapM rowExpWithDefaults)
    rowExpWithDefaults val = do
      givenCols <- convertRowObj val
      let inpCols = map fst givenCols
          sqlExps = Map.elems $ Map.union (Map.fromList givenCols) defVals
      return (inpCols, sqlExps)

    defVals = Map.fromList $ zip tableCols (repeat $ S.SEUnsafe "DEFAULT")

type ApplySQLOp =  (PGCol, S.SQLExp) -> S.SQLExp

rhsExpOp :: S.SQLOp -> S.AnnType -> ApplySQLOp
rhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op (S.SEIden $ toIden col) annExp
  where
    annExp = S.SETyAnn e annTy

lhsExpOp :: S.SQLOp -> S.AnnType -> ApplySQLOp
lhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op annExp $ S.SEIden $ toIden col
  where
    annExp = S.SETyAnn e annTy

convObjWithOp
  :: (MonadError QErr m)
  => ApplySQLOp -> AnnGValue -> m [(PGCol, S.SQLExp)]
convObjWithOp opFn val =
  flip withObject val $ \_ obj -> forM (Map.toList obj) $ \(k, v) -> do
  (_, colVal) <- asPGColVal v
  let pgCol = PGCol $ G.unName k
      encVal = txtEncoder colVal
      sqlExp = opFn (pgCol, encVal)
  return (pgCol, sqlExp)

convDeleteAtPathObj
  :: (MonadError QErr m)
  => AnnGValue -> m [(PGCol, S.SQLExp)]
convDeleteAtPathObj val =
  flip withObject val $ \_ obj -> forM (Map.toList obj) $ \(k, v) -> do
    vals <- flip withArray v $ \_ annVals -> mapM asPGColVal annVals
    let valExps = map (txtEncoder . snd) vals
        pgCol = PGCol $ G.unName k
        annEncVal = S.SETyAnn (S.SEArray valExps) S.textArrType
        sqlExp = S.SEOpApp S.jsonbDeleteAtPathOp
                 [S.SEIden $ toIden pgCol, annEncVal]
    return (pgCol, sqlExp)

convertUpdate
  :: QualifiedTable -- table
  -> S.BoolExp -- the filter expression
  -> Field -- the mutation field
  -> Convert RespTx
convertUpdate tn filterExp fld = do
  -- a set expression is same as a row object
  setExpM   <- withArgM args "_set" convertRowObj
  -- where bool expression to filter column
  whereExp <- withArg args "where" $ convertBoolExp (S.mkQual tn)
  -- increment operator on integer columns
  incExpM <- withArgM args "_inc" $
    convObjWithOp $ rhsExpOp S.incOp S.intType
  -- append jsonb value
  appendExpM <- withArgM args "_append" $
    convObjWithOp $ rhsExpOp S.jsonbConcatOp S.jsonbType
  -- prepend jsonb value
  prependExpM <- withArgM args "_prepend" $
    convObjWithOp $ lhsExpOp S.jsonbConcatOp S.jsonbType
  -- delete a key in jsonb object
  deleteKeyExpM <- withArgM args "_delete_key" $
    convObjWithOp $ rhsExpOp S.jsonbDeleteOp S.textType
  -- delete an element in jsonb array
  deleteElemExpM <- withArgM args "_delete_elem" $
    convObjWithOp $ rhsExpOp S.jsonbDeleteOp S.intType
  -- delete at path in jsonb value
  deleteAtPathExpM <- withArgM args "_delete_at_path" convDeleteAtPathObj

  mutFlds  <- convertMutResp tn (_fType fld) $ _fSelSet fld
  prepArgs <- get
  let updExpsM = [ setExpM, incExpM, appendExpM, prependExpM
                 , deleteKeyExpM, deleteElemExpM, deleteAtPathExpM
                 ]
      updExp = concat $ catMaybes updExpsM
  -- atleast one of update operators is expected
  unless (any isJust updExpsM) $ throwVE $
    "atleast any one of _set, _inc, _append, _prepend, _delete_key, _delete_elem and "
    <> " _delete_at_path operator is expected"
  let p1 = RU.UpdateQueryP1 tn updExp (filterExp, whereExp) mutFlds
  return $ RU.updateP2 (p1, prepArgs)
  where
    args = _fArguments fld

convertDelete
  :: QualifiedTable -- table
  -> S.BoolExp -- the filter expression
  -> Field -- the mutation field
  -> Convert RespTx
convertDelete tn filterExp fld = do
  whereExp <- withArg (_fArguments fld) "where" $ convertBoolExp (S.mkQual tn)
  mutFlds  <- convertMutResp tn (_fType fld) $ _fSelSet fld
  args <- get
  let p1 = RD.DeleteQueryP1 tn (filterExp, whereExp) mutFlds
  return $ RD.deleteP2 (p1, args)
