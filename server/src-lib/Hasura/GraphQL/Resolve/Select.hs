{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
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
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
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
  :: G.NamedType
  -> SelSet
  -> Convert [(FieldName, RS.AnnFld)]
fromSelSet fldTy flds =
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
            relSelData <- fromField relTN tableFilter tableLimit fld
            let annRel = RS.AnnRel (riName relInfo) (riType relInfo)
                         (riMapping relInfo) relSelData
            return $ RS.FRel annRel

fieldAsPath :: (MonadError QErr m) => Field -> m a -> m a
fieldAsPath fld = nameAsPath $ _fName fld

parseTableArgs
  :: QualifiedTable -> ArgsMap -> Convert RS.TableArgs
parseTableArgs tn args = do
  whereExpM  <- withArgM args "where" $ convertBoolExp tn
  ordByExpM  <- withArgM args "order_by" parseOrderBy
  limitExpM  <- withArgM args "limit" parseLimit
  offsetExpM <- withArgM args "offset" $ asPGColVal >=> prepare
  return $ RS.TableArgs whereExpM ordByExpM limitExpM offsetExpM

fromField
  :: QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> Convert RS.AnnSel
fromField tn permFilter permLimitM fld = fieldAsPath fld $ do
  tableArgs <- parseTableArgs tn args
  annFlds   <- fromSelSet (_fType fld) $ _fSelSet fld
  return $ RS.AnnSel annFlds tn Nothing permFilter permLimitM tableArgs
  where
    args = _fArguments fld

getOrdByItemMap
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OrdByCtx r
     )
  => G.NamedType -> m OrdByItemMap
getOrdByItemMap nt = do
  ordByCtx <- asks getter
  onNothing (Map.lookup nt ordByCtx) $
    throw500 $ "could not lookup " <> showNamedTy nt

parseOrderBy
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OrdByCtx r
     )
  => AnnGValue -> m [RS.AnnOrderByItem]
parseOrderBy = fmap concat . withArray f
  where
    f _ = mapM (withObject (getAnnObItems id))

getAnnObItems
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OrdByCtx r
     )
  => (RS.AnnObCol -> RS.AnnObCol)
  -> G.NamedType
  -> AnnGObject
  -> m [RS.AnnOrderByItem]
getAnnObItems f nt obj = do
  ordByItemMap <- getOrdByItemMap nt
  fmap concat $ forM (OMap.toList obj) $ \(k, v) -> do
    ordByItem <- onNothing (Map.lookup k ordByItemMap) $ throw500 $
      "cannot lookup " <> showName k <> " order by item in "
      <> showNamedTy nt <> " map"
    case ordByItem of
      OBIPGCol ci -> do
        let aobCol = f $ RS.AOCPG ci
        (_, enumVal) <- asEnumVal v
        (ordTy, nullsOrd) <- parseOrderByEnum enumVal
        return [OrderByItemG (Just ordTy) aobCol (Just nullsOrd)]
      OBIRel ri fltr -> do
        let annObColFn = f . RS.AOCRel ri fltr
        withObject (getAnnObItems annObColFn) v

parseOrderByEnum
  :: (MonadError QErr m)
  => G.EnumValue
  -> m (S.OrderType, S.NullsOrder)
parseOrderByEnum = \case
  G.EnumValue "_asc"              -> return (S.OTAsc, S.NLast)
  G.EnumValue "_desc"             -> return (S.OTDesc, S.NLast)
  G.EnumValue "_asc_nulls_first"  -> return (S.OTAsc, S.NFirst)
  G.EnumValue "_desc_nulls_first" -> return (S.OTDesc, S.NFirst)
  G.EnumValue v                   -> throw500 $
    "enum value " <> showName v <> " not found in type order_by"

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
  :: QualifiedTable -> S.BoolExp -> Field -> Convert RS.AnnSel
fromFieldByPKey tn permFilter fld = fieldAsPath fld $ do
  boolExp <- pgColValToBoolExp tn $ _fArguments fld
  annFlds <- fromSelSet (_fType fld) $ _fSelSet fld
  return $ RS.AnnSel annFlds tn Nothing permFilter Nothing $
    RS.noTableArgs { RS._taWhere = Just boolExp}

convertSelect
  :: QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> Convert RespTx
convertSelect qt permFilter permLimit fld = do
  selData <- withPathK "selectionSet" $
             fromField qt permFilter permLimit fld
  prepArgs <- get
  return $ RS.selectP2 False (selData, prepArgs)

convertSelectByPKey
  :: QualifiedTable -> S.BoolExp -> Field -> Convert RespTx
convertSelectByPKey qt permFilter fld = do
  selData <- withPathK "selectionSet" $
             fromFieldByPKey qt permFilter fld
  prepArgs <- get
  return $ RS.selectP2 True (selData, prepArgs)
