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

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Hasura.RQL.DML.Delete             as RD
import qualified Hasura.RQL.DML.Insert             as RI
import qualified Hasura.RQL.DML.Returning          as RR
import qualified Hasura.RQL.DML.Update             as RU

import qualified Hasura.SQL.DML                    as S

import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
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
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => G.NamedType -> SelSet -> m RR.RetFlds
convertReturning ty selSet =
  withSelSet selSet $ \fld ->
    case _fName fld of
      "__typename" -> return $ RR.RExp $ G.unName $ G.unNamedType ty
      _ -> do
        PGColInfo col colTy <- getPGColInfo ty $ _fName fld
        return $ RR.RCol (col, colTy)

convertMutResp
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => G.NamedType -> SelSet -> m RR.MutFlds
convertMutResp ty selSet =
  withSelSet selSet $ \fld ->
    case _fName fld of
      "__typename"    -> return $ RR.MExp $ G.unName $ G.unNamedType ty
      "affected_rows" -> return RR.MCount
      _ -> fmap RR.MRet $ convertReturning (_fType fld) $ _fSelSet fld

convertRowObj
  :: (MonadError QErr m, MonadState PrepArgs m)
  => AnnGValue
  -> m [(PGCol, S.SQLExp)]
convertRowObj val =
  flip withObject val $ \_ obj -> forM (Map.toList obj) $ \(k, v) -> do
    prepExp <- asPGColVal v >>= prepare
    return (PGCol $ G.unName k, prepExp)

mkConflictClause
  :: (MonadError QErr m)
  => [PGCol]
  -> ConflictAction
  -> Maybe ConstraintName
  -> m RI.ConflictClauseP1
mkConflictClause cols act conM = case (act , conM) of
  (CAIgnore, Nothing) -> return $ RI.CP1DoNothing Nothing
  (CAIgnore, Just cons) -> return $ RI.CP1DoNothing $ Just $ RI.Constraint cons
  (CAUpdate, Nothing) -> throw400 Unexpected
    "expecting \"constraint\" when \"action\" is \"update\" "
  (CAUpdate, Just cons) -> return $ RI.CP1Update (RI.Constraint cons) cols

parseAction
  :: (MonadError QErr m)
  => AnnGObject -> m ConflictAction
parseAction obj = do
  val <- onNothing (Map.lookup "action" obj) $ throw500
    "\"action\" field is expected but not found"
  (enumTy, enumVal) <- asEnumVal val
  withPathK "action" $ case G.unName $ G.unEnumValue enumVal of
    "ignore" -> return CAIgnore
    "update" -> return CAUpdate
    _ -> throw500 $ "only \"ignore\" and \"updated\" allowed for enum type " <> showNamedTy enumTy

parseConstraint
  :: (MonadError QErr m)
  => AnnGObject -> m (Maybe ConstraintName)
parseConstraint obj = do
  t <- mapM parseVal $ Map.lookup "constraint" obj
  return $ fmap ConstraintName t
  where
    parseVal v = do
      (_, enumVal) <- asEnumVal v
      return $ G.unName $ G.unEnumValue enumVal

parseOnConflict
  :: (MonadError QErr m)
  => [PGCol] -> AnnGValue -> m RI.ConflictClauseP1
parseOnConflict cols val =
  flip withObject val $ \_ obj -> do
    action <- parseAction obj
    constraintM <- parseConstraint obj
    mkConflictClause cols action constraintM

convertInsert
  :: (QualifiedTable, QualifiedTable) -- table, view
  -> [PGCol] -- all the columns in this table
  -> Field -- the mutation field
  -> Convert RespTx
convertInsert (tn, vn) tableCols fld = do
  rows    <- withArg arguments "objects" asRowExps
  onConflictM <- withPathK "on_conflict" $
                 withArgM arguments "on_conflict" $
                 parseOnConflict tableCols
  mutFlds <- convertMutResp (_fType fld) $ _fSelSet fld
  args <- get
  let p1 = RI.InsertQueryP1 tn vn tableCols rows onConflictM mutFlds
  return $ RI.insertP2 (p1, args)
  where
    arguments = _fArguments fld
    asRowExps = withArray (const $ mapM rowExpWithDefaults)
    rowExpWithDefaults val = do
      givenCols <- convertRowObj val
      return $ Map.elems $ Map.union (Map.fromList givenCols) defVals
    defVals = Map.fromList $ zip tableCols (repeat $ S.SEUnsafe "DEFAULT")

convObjWithOp
  :: (MonadError QErr m)
  => S.SQLOp -> S.AnnType -> AnnGValue -> m [(PGCol, S.SQLExp)]
convObjWithOp op annTy val =
  flip withObject val $ \_ obj -> forM (Map.toList obj) $ \(k, v) -> do
  (_, colVal) <- asPGColVal v
  let pgCol = PGCol $ G.unName k
      encVal = txtEncoder colVal
      annEncVal = S.SETyAnn encVal annTy
      sqlExp = S.SEOpApp op [S.SEIden $ toIden pgCol, annEncVal]
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
  whereExp <- withArg args "where" $ convertBoolExp tn
  incExpM <- withArgM args "_inc" $ convObjWithOp S.incOp S.intType
  concatExpM <- withArgM args "_concat" $
    convObjWithOp S.jsonbConcatOp S.jsonbType
  deleteKeyExpM <- withArgM args "_delete_key" $
    convObjWithOp S.jsonbDeleteOp S.textType
  deleteElemExpM <- withArgM args "_delete_elem" $
    convObjWithOp S.jsonbDeleteOp S.intType
  deleteAtPathExpM <- withArgM args "_delete_at_path" convDeleteAtPathObj
  mutFlds  <- convertMutResp (_fType fld) $ _fSelSet fld
  prepArgs <- get
  let updExpsM = [setExpM, incExpM, concatExpM, deleteKeyExpM, deleteElemExpM, deleteAtPathExpM]
      updExp = concat $ catMaybes updExpsM
  -- atleast one of update operators is expected
  unless (any isJust updExpsM) $ throw400 Unexpected $
    "atleast any one of _set, _inc, _concat, _delete_key, _delete_elem and "
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
  whereExp <- withArg (_fArguments fld) "where" $ convertBoolExp tn
  mutFlds  <- convertMutResp (_fType fld) $ _fSelSet fld
  args <- get
  let p1 = RD.DeleteQueryP1 tn (filterExp, whereExp) mutFlds
  return $ RD.deleteP2 (p1, args)
