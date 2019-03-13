module Hasura.RQL.DML.Returning where

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.ByteString.Builder as BB
import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified Database.PG.Query       as Q

data MutFld
  = MCount
  | MExp !T.Text
  | MRet ![(FieldName, AnnFld)]
  | MQuery !(Q.TxE QErr RespBody)

instance Show MutFld where
  show MCount     = "count"
  show (MExp t)   = "expression: " ++ T.unpack t
  show (MRet l)   = "returning: " ++ show l
  show (MQuery _) = "query tx"

instance Eq MutFld where
  MCount == MCount = True
  MCount == _ = False
  (MExp l) == (MExp r) = l == r
  (MExp _) == _ = False
  (MRet l) == (MRet r) = l == r
  (MRet _) == _ = False
  (MQuery _) == (MQuery _) = True
  (MQuery _) == _ = False

type MutFlds = [(T.Text, MutFld)]

hasNestedFld :: MutFlds -> Bool
hasNestedFld = any isNestedMutFld
  where
    isNestedMutFld (_, mutFld) = case mutFld of
      MRet annFlds -> any isNestedAnnFld annFlds
      _            -> False
    isNestedAnnFld (_, annFld) = case annFld of
      FObj _ -> True
      FArr _ -> True
      _      -> False

pgColsFromMutFld :: MutFld -> [(PGCol, PGColType)]
pgColsFromMutFld = \case
  MCount       -> []
  MExp _       -> []
  MQuery _     -> []
  MRet selFlds ->
    flip mapMaybe selFlds $ \(_, annFld) -> case annFld of
    FCol (PGColInfo col colTy _) -> Just (col, colTy)
    _                            -> Nothing

pgColsFromMutFlds :: MutFlds -> [(PGCol, PGColType)]
pgColsFromMutFlds = concatMap (pgColsFromMutFld . snd)

pgColsToSelFlds :: [PGColInfo] -> [(FieldName, AnnFld)]
pgColsToSelFlds cols =
  flip map cols $
  \pgColInfo -> (fromPGCol $ pgiName pgColInfo, FCol pgColInfo)

mkDefaultMutFlds :: Maybe [PGColInfo] -> MutFlds
mkDefaultMutFlds = \case
  Nothing   -> mutFlds
  Just cols -> ("returning", MRet $ pgColsToSelFlds cols):mutFlds
  where
    mutFlds = [("affected_rows", MCount)]

encodeJSONVector :: (a -> BB.Builder) -> V.Vector a -> BB.Builder
encodeJSONVector builder xs
  | V.null xs = BB.char7 '[' <> BB.char7 ']'
  | otherwise = BB.char7 '[' <> builder (V.unsafeHead xs) <>
                V.foldr go (BB.char7 ']') (V.unsafeTail xs)
    where go v b  = BB.char7 ',' <> builder v <> b

checkRetCols
  :: (UserInfoM m, QErrM m)
  => FieldInfoMap
  -> SelPermInfo
  -> [PGCol]
  -> m [PGColInfo]
checkRetCols fieldInfoMap selPermInfo cols = do
  mapM_ (checkSelOnCol selPermInfo) cols
  forM cols $ \col -> askPGColInfo fieldInfoMap col relInRetErr
  where
    relInRetErr = "Relationships can't be used in \"returning\"."
