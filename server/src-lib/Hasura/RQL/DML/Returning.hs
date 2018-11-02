{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.DML.Returning where

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.ByteString.Builder as BB
import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified Hasura.SQL.DML          as S

data MutFld
  = MCount
  | MExp !T.Text
  | MRet ![(FieldName, AnnFld)]
  deriving (Show, Eq)

type MutFlds = [(T.Text, MutFld)]

pgColsFromMutFld :: MutFld -> [(PGCol, PGColType)]
pgColsFromMutFld = \case
  MCount -> []
  MExp _ -> []
  MRet selFlds ->
    flip mapMaybe selFlds $ \(_, annFld) -> case annFld of
    FCol (PGColInfo col colTy _) -> Just (col, colTy)
    _                            -> Nothing

pgColsFromMutFlds :: MutFlds -> [(PGCol, PGColType)]
pgColsFromMutFlds = concatMap (pgColsFromMutFld . snd)

mkDefaultMutFlds :: Maybe [PGColInfo] -> MutFlds
mkDefaultMutFlds = \case
  Nothing   -> mutFlds
  Just cols -> ("returning", MRet $ pgColsToSelFlds cols):mutFlds
  where
    mutFlds = [("affected_rows", MCount)]
    pgColsToSelFlds cols = flip map cols $ \pgColInfo ->
      (fromPGCol $ pgiName pgColInfo, FCol pgColInfo)

qualTableToAliasIden :: QualifiedTable -> Iden
qualTableToAliasIden (QualifiedTable sn tn) =
  Iden $ getSchemaTxt sn <> "_" <> getTableTxt tn
  <> "__mutation_result_alias"

mkMutFldExp :: QualifiedTable -> Bool -> MutFld -> S.SQLExp
mkMutFldExp qt singleObj = \case
  MCount -> S.SESelect $
    S.mkSelect
    { S.selExtr = [S.Extractor (S.SEUnsafe "count(*)") Nothing]
    , S.selFrom = Just $ S.FromExp $ pure frmItem
    }
  MExp t -> S.SELit t
  MRet selFlds ->
    let tabFrom = TableFrom qt $ Just frmItem
        tabPerm = TablePerm (S.BELit True) Nothing
    in S.SESelect $ mkSQLSelect singleObj $
       AnnSelG selFlds tabFrom tabPerm noTableArgs
  where
    frmItem = S.FIIden $ qualTableToAliasIden qt

mkSelWith
  :: QualifiedTable -> S.CTE -> MutFlds -> Bool -> S.SelectWith
mkSelWith qt cte mutFlds singleObj =
  S.SelectWith [(alias, cte)] sel
  where
    alias = S.Alias $ qualTableToAliasIden qt
    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing] }

    extrExp = S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing

    jsonBuildObjArgs =
      flip concatMap mutFlds $
      \(k, mutFld) -> [S.SELit k, mkMutFldExp qt singleObj mutFld]

encodeJSONVector :: (a -> BB.Builder) -> V.Vector a -> BB.Builder
encodeJSONVector builder xs
  | V.null xs = BB.char7 '[' <> BB.char7 ']'
  | otherwise = BB.char7 '[' <> builder (V.unsafeHead xs) <>
                V.foldr go (BB.char7 ']') (V.unsafeTail xs)
    where go v b  = BB.char7 ',' <> builder v <> b

checkRetCols
  :: (P1C m)
  => FieldInfoMap
  -> SelPermInfo
  -> [PGCol]
  -> m [PGColInfo]
checkRetCols fieldInfoMap selPermInfo cols = do
  mapM_ (checkSelOnCol selPermInfo) cols
  forM cols $ \col -> askPGColInfo fieldInfoMap col relInRetErr
  where
    relInRetErr = "Relationships can't be used in \"returning\"."
