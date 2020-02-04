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

pgColsFromMutFld :: MutFld -> [(PGCol, PGColumnType)]
pgColsFromMutFld = \case
  MCount -> []
  MExp _ -> []
  MRet selFlds ->
    flip mapMaybe selFlds $ \(_, annFld) -> case annFld of
    FCol (AnnColField (PGColumnInfo col _ _ colTy _ _) _ _) -> Just (col, colTy)
    _                                                       -> Nothing

pgColsFromMutFlds :: MutFlds -> [(PGCol, PGColumnType)]
pgColsFromMutFlds = concatMap (pgColsFromMutFld . snd)

pgColsToSelFlds :: [PGColumnInfo] -> [(FieldName, AnnFld)]
pgColsToSelFlds cols =
  flip map cols $
  \pgColInfo -> (fromPGCol $ pgiColumn pgColInfo, mkAnnColField pgColInfo Nothing)

mkDefaultMutFlds :: Maybe [PGColumnInfo] -> MutFlds
mkDefaultMutFlds = \case
  Nothing   -> mutFlds
  Just cols -> ("returning", MRet $ pgColsToSelFlds cols):mutFlds
  where
    mutFlds = [("affected_rows", MCount)]

qualTableToAliasIden :: QualifiedTable -> Iden
qualTableToAliasIden qt =
  Iden $ snakeCaseTable qt <> "__mutation_result_alias"

mkMutFldExp :: QualifiedTable -> Maybe Int -> Bool -> MutFld -> S.SQLExp
mkMutFldExp qt preCalAffRows strfyNum = \case
  MCount ->
    let countExp = S.SESelect $
          S.mkSelect
          { S.selExtr = [S.Extractor S.countStar Nothing]
          , S.selFrom = Just $ S.FromExp $ pure frmItem
          }
    in maybe countExp (S.SEUnsafe . T.pack . show) preCalAffRows
  MExp t -> S.SELit t
  MRet selFlds ->
    -- let tabFrom = TableFrom qt $ Just frmItem
    let tabFrom = FromIden $ qualTableToAliasIden qt
        tabPerm = TablePerm annBoolExpTrue Nothing
    in S.SESelect $ mkSQLSelect False $
       AnnSelG selFlds tabFrom tabPerm noTableArgs strfyNum
  where
    frmItem = S.FIIden $ qualTableToAliasIden qt

mkMutationOutputExp
  :: QualifiedTable -> Maybe Int -> S.CTE -> MutFlds -> Bool -> S.SelectWith
mkMutationOutputExp qt preCalAffRows cte mutFlds strfyNum =
  S.SelectWith [(alias, cte)] sel
  where
    alias = S.Alias $ qualTableToAliasIden qt
    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing] }

    extrExp = S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing

    jsonBuildObjArgs =
      flip concatMap mutFlds $
      \(k, mutFld) -> [S.SELit k, mkMutFldExp qt preCalAffRows strfyNum mutFld]

checkRetCols
  :: (UserInfoM m, QErrM m)
  => FieldInfoMap FieldInfo
  -> SelPermInfo
  -> [PGCol]
  -> m [PGColumnInfo]
checkRetCols fieldInfoMap selPermInfo cols = do
  mapM_ (checkSelOnCol selPermInfo) cols
  forM cols $ \col -> askPGColInfo fieldInfoMap col relInRetErr
  where
    relInRetErr = "Relationships can't be used in \"returning\"."
