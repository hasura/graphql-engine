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
import qualified Data.HashMap.Strict     as Map
import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified Hasura.SQL.DML          as S

data MutFld
  = MCount
  | MExp !T.Text
  | MRet !SelectData
  deriving (Show, Eq)

type MutFlds = Map.HashMap T.Text MutFld

pgColsFromMutFld :: MutFld -> [(PGCol, PGColType)]
pgColsFromMutFld = \case
  MCount -> []
  MExp _ -> []
  MRet selData -> fst $ partAnnFlds $ Map.elems $ sdFlds selData

pgColsToSelData :: QualifiedTable -> [(PGCol, PGColType)] -> SelectData
pgColsToSelData qt cols = SelectData flds (Left qt) (S.BELit True, Nothing)
                          Nothing [] Nothing Nothing False
  where
    flds = Map.fromList $ flip map cols $ \(c, ty) ->
      (fromPGCol c, FCol (c, ty))

pgColsFromMutFlds :: MutFlds -> [(PGCol, PGColType)]
pgColsFromMutFlds = concatMap pgColsFromMutFld . Map.elems

mkDefaultMutFlds :: QualifiedTable -> Maybe [(PGCol, PGColType)] -> MutFlds
mkDefaultMutFlds qt = \case
  Nothing   -> mutFlds
  Just cols -> Map.insert "returning" (MRet $ pgColsToSelData qt cols) mutFlds
  where
    mutFlds = Map.singleton "affected_rows" MCount

mkMutFldExp :: MutationType -> QualifiedTable -> MutFld -> S.SQLExp
mkMutFldExp mutTy qt = \case
  MCount -> S.SESelect $
    S.mkSelect
    { S.selExtr = [S.Extractor (S.SEUnsafe "count(*)") Nothing]
    , S.selFrom = Just $ S.FromExp $ pure $
                  S.FIIden $ qualTableToAliasIden mutTy qt
    }
  MExp t -> S.SELit t
  MRet selData -> S.SESelect $ mkSQLSelect selData

mkSelWith :: MutationType -> QualifiedTable -> S.CTE -> MutFlds -> S.SelectWith
mkSelWith mutTy qt cte mutFlds =
  S.SelectWith [(alias, cte)] sel
  where
    alias = S.Alias $ qualTableToAliasIden mutTy qt
    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing] }

    extrExp = S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing

    jsonBuildObjArgs =
      flip concatMap (Map.toList mutFlds) $
      \(k, mutFld) -> [S.SELit k, mkMutFldExp mutTy qt mutFld]

encodeJSONVector :: (a -> BB.Builder) -> V.Vector a -> BB.Builder
encodeJSONVector builder xs
  | V.null xs = BB.char7 '[' <> BB.char7 ']'
  | otherwise = BB.char7 '[' <> builder (V.unsafeHead xs) <>
                V.foldr go (BB.char7 ']') (V.unsafeTail xs)
    where go v b  = BB.char7 ',' <> builder v <> b

-- newtype RetRes = RetRes { getRetRes :: BL.ByteString }

-- instance Q.FromRes RetRes where
--   fromRes (Q.ResultOkEmpty _) =
--     throwError "Expecting data. Instead, status is 'CommandOk'"

--   fromRes (Q.ResultOkData pqRes) = do
--     nc <- liftIO $ PQ.nfields pqRes

--     -- We are only expecting tuples with single element
--     unless (nc == 1) $
--       throwError "select is expecting only 1 column in the result"

--     -- Now get the number of rows
--     nr  <- liftIO $ PQ.ntuples pqRes

--     -- comma separated value bulider
--     valsBB <- rowLoop nr (getValue pqRes)

--     return $ RetRes $ BB.toLazyByteString $ (BB.char7 '[' <> valsBB <> BB.char7 ']')

-- getValue :: PQ.Result -> PQ.Row -> ExceptT String IO BB.Builder
-- getValue res i = do
--   bs <- liftIO $ PQ.getvalue res i 0
--   case bs of
--     Just bs' -> return $ BB.byteString bs'
--     Nothing  -> throwError "null encountered when processing select result"

-- rowLoop :: PQ.Row -> (PQ.Row -> ExceptT String IO BB.Builder)
--         -> ExceptT String IO BB.Builder
-- rowLoop n f = loop (n - 1) mempty
--   where
--     loop !i !accum
--       | i < 0 = return accum
--       | i == 0 = do
--           a <- f 0
--           return (a <> accum)
--       | otherwise = do
--            a <- f i
--            loop (i-1) (BB.char7 ',' <> a <> accum)

checkRetCols
  :: (P1C m)
  => FieldInfoMap
  -> SelPermInfo
  -> [PGCol]
  -> m [PGColType]
checkRetCols fieldInfoMap selPermInfo cols = do
  mapM_ (checkSelOnCol selPermInfo) cols
  forM cols $ \col -> askPGType fieldInfoMap col relInRetErr
  where
    relInRetErr = "Relationships can't be used in \"returning\"."

-- | Converts the given columns into :
-- RETURNING row_to_json((SELECT r FROM (SELECT col1, col2, .. ) AS r))
-- toRetExp :: [(PGCol, PGColType)] -> S.RetExp
-- toRetExp cols =
--   S.RetExp [S.Extractor jsonAgg Nothing]
--   where
--     -- row_to_json((SELECT r FROM (SELECT col1, col2, .. ) AS r))
--     jsonAgg = S.SEFnApp "row_to_json" [S.mkRowExp $ map mkColExtr cols] Nothing

-- encodeReturning :: Q.WithReturning (V.Vector (Identity BS.ByteString))
--                 -> BB.Builder
-- encodeReturning (Q.WithReturning c mval) =
--   BB.byteString "{\"affected_rows\":" <>
--   BB.word64Dec c <> retBuilder <> BB.char7 '}'
--   where
--     retBuilder =
--       case mval of
--       Just ret ->
--         BB.byteString ",\"returning\":" <>
--         encodeJSONVector (BB.byteString . runIdentity) ret
--       Nothing  -> mempty
