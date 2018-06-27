{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.GraphQL.Resolve.Select
  ( convertSelect
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
import           Hasura.RQL.Types
import           Hasura.SQL.Types

fromSelSet
  :: G.NamedType
  -> SelSet
  -> Convert (Map.HashMap FieldName RS.AnnFld)
fromSelSet fldTy flds =
  fmap Map.fromList $ forM (toList flds) $ \fld -> do
    let fldName = _fName fld
    let rqlFldName = FieldName $ G.unName $ G.unAlias $ _fAlias fld
    case fldName of
      "__typename" -> return (rqlFldName, RS.FExp $ G.unName $ G.unNamedType fldTy)
      _ -> do
        fldInfo <- getFldInfo fldTy fldName
        case fldInfo of
          Left (PGColInfo pgCol colTy) -> return (rqlFldName, RS.FCol (pgCol, colTy))
          Right (relInfo, tableFilter) -> do
            let relTN = riRTable relInfo
            relSelData <- fromField relTN tableFilter fld
            let annRel = RS.AnnRel (riName relInfo) (riType relInfo)
                         (riMapping relInfo) relSelData
            return (rqlFldName, RS.FRel annRel)

fromField
  :: QualifiedTable -> S.BoolExp -> Field -> Convert RS.SelectData
fromField tn permFilter fld = do
  whereExpM  <- withArgM args "where" $ convertBoolExp tn
  ordByExpM  <- withArgM args "order_by" parseOrderBy
  limitExpM  <- withArgM args "limit" $ asPGColVal >=> prepare
  offsetExpM <- withArgM args "offset" $ asPGColVal >=> prepare
  annFlds    <- fromSelSet (_fType fld) $ _fSelSet fld
  return $ RS.SelectData annFlds tn (permFilter, whereExpM) ordByExpM
    [] limitExpM offsetExpM
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
  => AnnGValue -> m S.OrderByExp
parseOrderBy v = do
  enums <- withArray (const $ mapM asEnumVal) v
  fmap S.OrderByExp $ forM enums $ \(nt, ev) ->
    convOrdByElem <$> getEnumInfo nt ev
  -- return $ map convOrdByElem enums
  -- undefined
  where
    convOrdByElem (PGColInfo col _, ordTy, nullsOrd) =
      S.OrderByItem (Left col)
      (Just $ convOrdTy ordTy)
      (Just $ convNullsOrd nullsOrd)

    convOrdTy = \case
      OAsc  -> S.OTAsc
      ODesc -> S.OTDesc

    convNullsOrd = \case
      NFirst -> S.NFirst
      NLast  -> S.NLast

convertSelect
  :: QualifiedTable -> S.BoolExp -> Field -> Convert RespTx
convertSelect qt permFilter fld = do
  selData <- fromField qt permFilter fld
  prepArgs <- get
  return $ RS.selectP2 (selData, prepArgs)
