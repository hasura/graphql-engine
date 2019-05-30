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
  deriving (Show, Eq)

traverseMutFld
  :: (Applicative f)
  => (a -> f b)
  -> MutFldG a
  -> f (MutFldG b)
traverseMutFld f = \case
  MCount    -> pure MCount
  MExp t    -> pure $ MExp t
  MRet flds -> MRet <$> traverse (traverse (traverseAnnFld f)) flds

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
  MCount -> []
  MExp _ -> []
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

qualTableToAliasIden :: QualifiedTable -> Iden
qualTableToAliasIden qt =
  Iden $ snakeCaseTable qt <> "__mutation_result_alias"

mkMutFldExp :: QualifiedTable -> Bool -> Bool -> MutFld -> S.SQLExp
mkMutFldExp qt singleObj strfyNum = \case
  MCount -> S.SESelect $
    S.mkSelect
    { S.selExtr = [S.Extractor S.countStar Nothing]
    , S.selFrom = Just $ S.FromExp $ pure frmItem
    }
  MExp t -> S.SELit t
  MRet selFlds ->
    -- let tabFrom = TableFrom qt $ Just frmItem
    let tabFrom = TableFrom qt $ Just  $ qualTableToAliasIden qt
        tabPerm = TablePerm annBoolExpTrue Nothing
    in S.SESelect $ mkSQLSelect singleObj $
       AnnSelG selFlds tabFrom tabPerm noTableArgs strfyNum
  where
    frmItem = S.FIIden $ qualTableToAliasIden qt

mkSelWith
  :: QualifiedTable -> S.CTE -> MutFlds -> Bool -> Bool -> S.SelectWith
mkSelWith qt cte mutFlds singleObj strfyNum =
  S.SelectWith [(alias, cte)] sel
  where
    alias = S.Alias $ qualTableToAliasIden qt
    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing] }

    extrExp = S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing

    jsonBuildObjArgs =
      flip concatMap mutFlds $
      \(k, mutFld) -> [S.SELit k, mkMutFldExp qt singleObj strfyNum mutFld]

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
