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
  | MRet !AnnSel
  deriving (Show, Eq)

type MutFlds = [(T.Text, MutFld)]

pgColsFromMutFld :: MutFld -> [(PGCol, PGColType)]
pgColsFromMutFld = \case
  MCount -> []
  MExp _ -> []
  MRet selData ->
    flip mapMaybe (_asFields selData) $ \(_, annFld) -> case annFld of
    FCol (PGColInfo col colTy _) -> Just (col, colTy)
    _                            -> Nothing

pgColsToSelData :: QualifiedTable -> [PGColInfo] -> AnnSel
pgColsToSelData qt cols =
  AnnSel flds qt (Just frmItem) (S.BELit True) Nothing noTableArgs
  where
    flds = flip map cols $ \pgColInfo ->
      (fromPGCol $ pgiName pgColInfo, FCol pgColInfo)
    frmItem = S.FIIden $ qualTableToAliasIden qt

pgColsFromMutFlds :: MutFlds -> [(PGCol, PGColType)]
pgColsFromMutFlds = concatMap (pgColsFromMutFld . snd)

mkDefaultMutFlds :: QualifiedTable -> Maybe [PGColInfo] -> MutFlds
mkDefaultMutFlds qt = \case
  Nothing   -> mutFlds
  Just cols -> ("returning", (MRet $ pgColsToSelData qt cols)):mutFlds
  where
    mutFlds = [("affected_rows", MCount)]

qualTableToAliasIden :: QualifiedTable -> Iden
qualTableToAliasIden (QualifiedTable sn tn) =
  Iden $ getSchemaTxt sn <> "_" <> getTableTxt tn
  <> "__mutation_result_alias"

mkMutFldExp :: QualifiedTable -> MutFld -> S.SQLExp
mkMutFldExp qt = \case
  MCount -> S.SESelect $
    S.mkSelect
    { S.selExtr = [S.Extractor (S.SEUnsafe "count(*)") Nothing]
    , S.selFrom = Just $ S.FromExp $ pure $
                  S.FIIden $ qualTableToAliasIden qt
    }
  MExp t -> S.SELit t
  MRet selData -> S.SESelect $ mkSQLSelect False selData

mkSelWith :: QualifiedTable -> S.CTE -> MutFlds -> S.SelectWith
mkSelWith qt cte mutFlds =
  S.SelectWith [(alias, cte)] sel
  where
    alias = S.Alias $ qualTableToAliasIden qt
    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing] }

    extrExp = S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing

    jsonBuildObjArgs =
      flip concatMap mutFlds $
      \(k, mutFld) -> [S.SELit k, mkMutFldExp qt mutFld]

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
