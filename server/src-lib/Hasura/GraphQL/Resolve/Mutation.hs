{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- TODO: add conflict clause
convertInsert
  :: (QualifiedTable, QualifiedTable) -- table, view
  -> [PGCol] -- all the columns in this table
  -> Field -- the mutation field
  -> Convert RespTx
convertInsert (tn, vn) tableCols fld = do
  rows    <- withArg (_fArguments fld) "objects" asRowExps
  mutFlds <- convertMutResp (_fType fld) $ _fSelSet fld
  args <- get
  let p1 = RI.InsertQueryP1 tn vn tableCols rows Nothing mutFlds
  return $ RI.insertP2 (p1, args)
  where
    asRowExps = withArray (const $ mapM rowExpWithDefaults)
    rowExpWithDefaults val = do
      givenCols <- convertRowObj val
      return $ Map.elems $ Map.union (Map.fromList givenCols) defVals
    defVals = Map.fromList $ zip tableCols (repeat $ S.SEUnsafe "DEFAULT")

convertUpdate
  :: QualifiedTable -- table
  -> S.BoolExp -- the filter expression
  -> Field -- the mutation field
  -> Convert RespTx
convertUpdate tn filterExp fld = do
  -- a set expression is same as a row object
  setExp   <- withArg args "_set" convertRowObj
  whereExp <- withArg args "where" $ convertBoolExp tn
  mutFlds  <- convertMutResp (_fType fld) $ _fSelSet fld
  prepArgs <- get
  let p1 = RU.UpdateQueryP1 tn setExp (filterExp, whereExp) mutFlds
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
