{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

module Hasura.GraphQL.Resolve.Select
  ( convertSelect
  , convertSelectByPKey
  , fromSelSet
  , fieldAsPath
  , fromField
  , fromFieldByPKey
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Hasura.RQL.DML.Select             as RS

import qualified Hasura.SQL.DML                    as S

import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal           (onlyPositiveInt)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

fromSelSet
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByResolveCtx r)
  => ((PGColType, PGColValue) -> m S.SQLExp)
  -> G.NamedType
  -> SelSet
  -> m [(FieldName, RS.AnnFld)]
fromSelSet f fldTy flds =
  forM (toList flds) $ \fld -> do
    let fldName = _fName fld
    let rqlFldName = FieldName $ G.unName $ G.unAlias $ _fAlias fld
    (rqlFldName,) <$> case fldName of
      "__typename" -> return $ RS.FExp $ G.unName $ G.unNamedType fldTy
      _ -> do
        fldInfo <- getFldInfo fldTy fldName
        case fldInfo of
          Left colInfo -> return $ RS.FCol colInfo
          Right (relInfo, tableFilter, tableLimit, _) -> do
            let relTN = riRTable relInfo
            relSelData <- fromField f relTN tableFilter tableLimit fld
            let annRel = RS.AnnRel (riName relInfo) (riType relInfo)
                         (riMapping relInfo) relSelData
            return $ RS.FRel annRel

fieldAsPath :: (MonadError QErr m) => Field -> m a -> m a
fieldAsPath fld = nameAsPath $ _fName fld

parseTableArgs
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByResolveCtx r)
  => ((PGColType, PGColValue) -> m S.SQLExp)
  -> QualifiedTable -> ArgsMap -> m RS.TableArgs
parseTableArgs f tn args = do
  whereExpM  <- withArgM args "where" $ convertBoolExpG f tn
  ordByExpM  <- withArgM args "order_by" parseOrderBy
  limitExpM  <- withArgM args "limit" parseLimit
  offsetExpM <- withArgM args "offset" $ asPGColVal >=> f
  return $ RS.TableArgs whereExpM ordByExpM limitExpM offsetExpM

fromField
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByResolveCtx r)
  => ((PGColType, PGColValue) -> m S.SQLExp)
  -> QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> m RS.AnnSel
fromField f tn permFilter permLimitM fld =
  fieldAsPath fld $ do
  tableArgs <- parseTableArgs f tn args
  annFlds   <- fromSelSet f (_fType fld) $ _fSelSet fld
  return $ RS.AnnSel annFlds tn Nothing permFilter permLimitM tableArgs
  where
    args = _fArguments fld

getEnumInfo
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OrdByResolveCtx r
     )
  => G.NamedType -> G.EnumValue -> m OrdByResolveCtxElem
getEnumInfo nt v = do
  -- fldMap <- _gcFieldMap <$> ask
  ordByCtx <- asks getter
  onNothing (Map.lookup (nt,v) ordByCtx) $
    throw500 $ "could not lookup " <> showName (G.unEnumValue v) <> " in " <>
    showNamedTy nt

parseOrderBy
  :: (MonadError QErr m
     , MonadReader r m
     , Has OrdByResolveCtx r
     )
  => AnnGValue -> m [RS.AnnOrderByItem]
parseOrderBy v = do
  enums <- withArray (const $ mapM asEnumVal) v
  mapM (uncurry getEnumInfo) enums

parseLimit :: ( MonadError QErr m ) => AnnGValue -> m Int
parseLimit v = do
  (_, pgColVal) <- asPGColVal v
  limit <- maybe noIntErr return $ pgColValueToInt pgColVal
  -- validate int value
  onlyPositiveInt limit
  return limit
  where
    noIntErr = throw400 Unexpected "expecting Integer value for \"limit\""

fromFieldByPKey
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByResolveCtx r)
  => ((PGColType, PGColValue) -> m S.SQLExp)
  -> QualifiedTable -> S.BoolExp -> Field -> m RS.AnnSel
fromFieldByPKey f tn permFilter fld = fieldAsPath fld $ do
  boolExp <- pgColValToBoolExpG f tn $ _fArguments fld
  annFlds <- fromSelSet f (_fType fld) $ _fSelSet fld
  return $ RS.AnnSel annFlds tn Nothing permFilter Nothing $
    RS.noTableArgs { RS._taWhere = Just boolExp}

convertSelect
  :: QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> Convert RespTx
convertSelect qt permFilter permLimit fld = do
  selData <- withPathK "selectionSet" $
             fromField prepare qt permFilter permLimit fld
  prepArgs <- get
  return $ RS.selectP2 False (selData, prepArgs)

convertSelectByPKey
  :: QualifiedTable -> S.BoolExp -> Field -> Convert RespTx
convertSelectByPKey qt permFilter fld = do
  selData <- withPathK "selectionSet" $
             fromFieldByPKey prepare qt permFilter fld
  prepArgs <- get
  return $ RS.selectP2 True (selData, prepArgs)
