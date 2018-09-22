{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Resolve.Select
  ( convertSelect
  , convertSelectByPKey
  , fromSelSet
  , convertFuncQuery
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
          Left (PGColInfo pgCol colTy _) -> return (rqlFldName, RS.FCol (pgCol, colTy))
          Right (relInfo, tableFilter, tableLimit, _) -> do
            let relTN = riRTable relInfo
            relSelData <- fromField relTN tableFilter tableLimit fld
            let annRel = RS.AnnRel (riName relInfo) (riType relInfo)
                         (riMapping relInfo) relSelData
            return (rqlFldName, RS.FRel annRel)

fieldAsPath :: (MonadError QErr m) => Field -> m a -> m a
fieldAsPath fld = nameAsPath $ _fName fld

fromField
  :: QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> Convert RS.SelectData
fromField tn permFilter permLimit fld = fieldAsPath fld $ do
  whereExpM  <- withArgM args "where" $ convertBoolExp (S.mkQual tn)
  ordByExpM  <- withArgM args "order_by" parseOrderBy
  limitExpM  <- RS.applyPermLimit permLimit
                <$> withArgM args "limit" parseLimit
  offsetExpM <- withArgM args "offset" $ asPGColVal >=> prepare
  annFlds    <- fromSelSet (_fType fld) $ _fSelSet fld
  return $ RS.SelectData annFlds tn Nothing (permFilter, whereExpM) ordByExpM
    [] limitExpM offsetExpM False
  where
    args = _fArguments fld

fromFieldByPKey
  :: QualifiedTable -> S.BoolExp -> Field -> Convert RS.SelectData
fromFieldByPKey tn permFilter fld = fieldAsPath fld $ do
  boolExp <- pgColValToBoolExp tn $ _fArguments fld
  annFlds <- fromSelSet (_fType fld) $ _fSelSet fld
  return $ RS.SelectData annFlds tn Nothing (permFilter, Just boolExp)
    Nothing [] Nothing Nothing True

fromFuncQueryField
  :: QualifiedTable -> QualifiedFunction -> Maybe Int -> Field -> Convert RS.SelectData
fromFuncQueryField qt qf permLimit fld = fieldAsPath fld $ do
  funcArgsM <- withArgM args "args" parseFunctionArgs
  let funcArgs = fromMaybe [] funcArgsM
      funFrmExp = S.mkFuncFromExp qf funcArgs
      qual = S.QualIden $ toIden $ S.mkFuncAlias qf
  whereExpM  <- withArgM args "where" $ convertBoolExp qual
  ordByExpM  <- withArgM args "order_by" parseOrderBy
  limitExpM  <- RS.applyPermLimit permLimit
                <$> withArgM args "limit" parseLimit
  offsetExpM <- withArgM args "offset" $ asPGColVal >=> prepare
  annFlds    <- fromSelSet (_fType fld) $ _fSelSet fld
  return $ RS.SelectData annFlds qt (Just funFrmExp) (S.BELit True, whereExpM)
    ordByExpM [] limitExpM offsetExpM False
  where
    args = _fArguments fld

parseFunctionArgs :: AnnGValue -> Convert [S.SQLExp]
parseFunctionArgs val =
  flip withObject val $ \_ obj ->
    forM (Map.elems obj) $ prepare <=< asPGColVal

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
    convOrdByElem (PGColInfo col _ _, ordTy, nullsOrd) =
      S.OrderByItem (Left col)
      (Just $ convOrdTy ordTy)
      (Just $ convNullsOrd nullsOrd)

    convOrdTy = \case
      OAsc  -> S.OTAsc
      ODesc -> S.OTDesc

    convNullsOrd = \case
      NFirst -> S.NFirst
      NLast  -> S.NLast

parseLimit :: ( MonadError QErr m ) => AnnGValue -> m Int
parseLimit v = do
  (_, pgColVal) <- asPGColVal v
  limit <- maybe noIntErr return $ pgColValueToInt pgColVal
  -- validate int value
  onlyPositiveInt limit
  return limit
  where
    noIntErr = throw400 Unexpected "expecting Integer value for \"limit\""

convertSelect
  :: QualifiedTable -> S.BoolExp -> Maybe Int -> Field -> Convert RespTx
convertSelect qt permFilter permLimit fld = do
  selData <- withPathK "selectionSet" $
             fromField qt permFilter permLimit fld
  prepArgs <- get
  return $ RS.selectP2 (selData, prepArgs)

convertSelectByPKey
  :: QualifiedTable -> S.BoolExp -> Field -> Convert RespTx
convertSelectByPKey qt permFilter fld = do
  selData <- withPathK "selectionSet" $
             fromFieldByPKey qt permFilter fld
  prepArgs <- get
  return $ RS.selectP2 (selData, prepArgs)

convertFuncQuery
  :: QualifiedTable -> QualifiedFunction -> Maybe Int -> Field -> Convert RespTx
convertFuncQuery qt qf permLimit fld = do
  selData <- withPathK "selectionSet" $
             fromFuncQueryField qt qf permLimit fld
  prepArgs <- get
  return $ RS.selectP2 (selData, prepArgs)
