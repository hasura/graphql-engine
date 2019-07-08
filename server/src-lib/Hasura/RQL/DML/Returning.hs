module Hasura.RQL.DML.Returning where

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.Text               as T
import qualified Hasura.SQL.DML          as S

data MutFldG v
  = MCount
  | MExp !T.Text
  | MRet ![(FieldName, AnnFldG v)]
  | MQuery !RespTx

instance Show v => Show (MutFldG v) where
  show MCount     = "count"
  show (MExp t)   = "expression: " ++ T.unpack t
  show (MRet l)   = "returning: " ++ show l
  show (MQuery _) = "query tx"

instance Eq v => Eq (MutFldG v) where
  MCount == MCount = True
  MCount == _ = False
  (MExp l) == (MExp r) = l == r
  (MExp _) == _ = False
  (MRet l) == (MRet r) = l == r
  (MRet _) == _ = False
  (MQuery _) == (MQuery _) = True
  (MQuery _) == _ = False

traverseMutFld
  :: (Applicative f)
  => (a -> f b)
  -> MutFldG a
  -> f (MutFldG b)
traverseMutFld f = \case
  MCount    -> pure MCount
  MExp t    -> pure $ MExp t
  MRet flds -> MRet <$> traverse (traverse (traverseAnnFld f)) flds
  MQuery q  -> pure $ MQuery q

type MutFld = MutFldG S.SQLExp

type MutFldsG v = [(T.Text, MutFldG v)]

traverseMutFlds
  :: (Applicative f)
  => (a -> f b)
  -> MutFldsG a
  -> f (MutFldsG b)
traverseMutFlds f =
  traverse (traverse (traverseMutFld f))

type MutFlds = MutFldsG S.SQLExp

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
    FCol (PGColInfo col colTy _) _ -> Just (col, colTy)
    _                              -> Nothing

pgColsFromMutFlds :: MutFlds -> [(PGCol, PGColType)]
pgColsFromMutFlds = concatMap (pgColsFromMutFld . snd)

pgColsToSelFlds :: [PGColInfo] -> [(FieldName, AnnFld)]
pgColsToSelFlds cols =
  flip map cols $
  \pgColInfo -> (fromPGCol $ pgiName pgColInfo, FCol pgColInfo Nothing)

mkDefaultMutFlds :: Maybe [PGColInfo] -> MutFlds
mkDefaultMutFlds = \case
  Nothing   -> mutFlds
  Just cols -> ("returning", MRet $ pgColsToSelFlds cols):mutFlds
  where
    mutFlds = [("affected_rows", MCount)]

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
