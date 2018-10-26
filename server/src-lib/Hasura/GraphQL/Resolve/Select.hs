{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}

module Hasura.GraphQL.Resolve.Select
  ( convertSelect
  , convertSelectByPKey
  , convertAggSelect
  , withSelSet
  , fromSelSet
  , fieldAsPath
  , fromField
  , fromFieldByPKey
  , fromAggField
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

withSelSet :: (Monad m) => SelSet -> (Field -> m a) -> m [(Text, a)]
withSelSet selSet f =
  forM (toList selSet) $ \fld -> do
    res <- f fld
    return (G.unName $ G.unAlias $ _fAlias fld, res)

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
          Right (relInfo, isAgg, tableFilter, tableLimit) -> do
            let relTN = riRTable relInfo
                annSelMaker = bool fromField fromAggField isAgg
            annSel <- annSelMaker f relTN tableFilter tableLimit fld
            let annRel = RS.AnnRel (riName relInfo) (riType relInfo)
                         (riMapping relInfo) annSel
            return $ RS.FRel annRel

fieldAsPath :: (MonadError QErr m) => Field -> m a -> m a
fieldAsPath = nameAsPath . _fName

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
  let selFlds = RS.ASFSimple annFlds
      tabFrom = RS.TableFrom tn Nothing
      tabPerm = RS.TablePerm permFilter permLimitM
  return $ RS.AnnSel selFlds tabFrom tabPerm tableArgs
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
  let selFlds = RS.ASFSimple annFlds
      tabFrom = RS.TableFrom tn Nothing
      tabPerm = RS.TablePerm permFilter Nothing
  return $ RS.AnnSel selFlds tabFrom tabPerm $
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

-- agg select related
convertColFlds
  :: Monad m => G.NamedType -> SelSet -> m RS.ColFlds
convertColFlds ty selSet =
  withSelSet selSet $ \fld ->
    case _fName fld of
      "__typename" -> return $ RS.PCFExp $ G.unName $ G.unNamedType ty
      n            -> return $ RS.PCFCol $ PGCol $ G.unName n

convertAggFld
  :: (Monad m, MonadError QErr m)
  => G.NamedType -> SelSet -> m RS.AggFlds
convertAggFld ty selSet =
  withSelSet selSet $ \fld -> do
    let fType = _fType fld
        fSelSet = _fSelSet fld
    case _fName fld of
      "__typename" -> return $ RS.AFExp $ G.unName $ G.unNamedType ty
      "count"      -> return RS.AFCount
      "sum"        -> RS.AFSum <$> convertColFlds fType fSelSet
      "avg"        -> RS.AFAvg <$> convertColFlds fType fSelSet
      "max"        -> RS.AFMax <$> convertColFlds fType fSelSet
      "min"        -> RS.AFMin <$> convertColFlds fType fSelSet
      G.Name t     -> throw500 $ "unexpected field in _agg node: " <> t

fromAggField
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r, Has OrdByResolveCtx r)
  => ((PGColType, PGColValue) -> m S.SQLExp)
  -> QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> m RS.AnnSel
fromAggField fn tn permFilter permLimitM fld = fieldAsPath fld $ do
  tableArgs <- parseTableArgs fn tn args
  aggSelFlds   <- fromAggSel (_fType fld) $ _fSelSet fld
  let selFlds = RS.ASFWithAgg aggSelFlds
      tabFrom = RS.TableFrom tn Nothing
      tabPerm = RS.TablePerm permFilter permLimitM
  return $ RS.AnnSel selFlds tabFrom tabPerm tableArgs
  where
    args = _fArguments fld
    fromAggSel ty selSet =
      withSelSet selSet $ \f -> do
        let fTy = _fType f
            fSelSet = _fSelSet f
        case _fName f of
          "__typename" -> return $ RS.SFExp $ G.unName $ G.unNamedType ty
          "agg"        -> RS.SFAggFld <$> convertAggFld fTy fSelSet
          "nodes"      -> RS.SFNodes <$> fromSelSet fn fTy fSelSet
          G.Name t     -> throw500 $ "unexpected field in _agg node: " <> t

convertAggSelect
  :: QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> Convert RespTx
convertAggSelect qt permFilter permLimit fld = do
  selData <- withPathK "selectionSet" $
             fromAggField prepare qt permFilter permLimit fld
  prepArgs <- get
  return $ RS.selectP2 False (selData, prepArgs)
