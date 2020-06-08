module Hasura.RQL.DML.Returning where

import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Data.Text                      as T
import qualified Hasura.SQL.DML                 as S

traverseMutFld
  :: (Applicative f)
  => (a -> f b)
  -> MutFldG a
  -> f (MutFldG b)
traverseMutFld f = \case
  MCount    -> pure MCount
  MExp t    -> pure $ MExp t
  MRet flds -> MRet <$> traverse (traverse (traverseAnnField f)) flds

traverseMutationOutput
  :: (Applicative f)
  => (a -> f b)
  -> MutationOutputG a -> f (MutationOutputG b)
traverseMutationOutput f = \case
  MOutMultirowFields mutationFields ->
    MOutMultirowFields <$> traverse (traverse (traverseMutFld f)) mutationFields
  MOutSinglerowObject annFields ->
    MOutSinglerowObject <$> traverseAnnFields f annFields

traverseMutFlds
  :: (Applicative f)
  => (a -> f b)
  -> MutFldsG a
  -> f (MutFldsG b)
traverseMutFlds f =
  traverse (traverse (traverseMutFld f))

hasNestedFld :: MutationOutputG a -> Bool
hasNestedFld = \case
  MOutMultirowFields flds -> any isNestedMutFld flds
  MOutSinglerowObject annFlds -> any isNestedAnnField annFlds
  where
    isNestedMutFld (_, mutFld) = case mutFld of
      MRet annFlds -> any isNestedAnnField annFlds
      _            -> False
    isNestedAnnField (_, annFld) = case annFld of
      AFObjectRelation _ -> True
      AFArrayRelation _  -> True
      _                  -> False

pgColsFromMutFld :: MutFld -> [(PGCol, PGColumnType)]
pgColsFromMutFld = \case
  MCount -> []
  MExp _ -> []
  MRet selFlds ->
    flip mapMaybe selFlds $ \(_, annFld) -> case annFld of
    AFColumn (AnnColumnField (PGColumnInfo col _ _ colTy _ _) _ _) -> Just (col, colTy)
    _                                                              -> Nothing

pgColsFromMutFlds :: MutFlds -> [(PGCol, PGColumnType)]
pgColsFromMutFlds = concatMap (pgColsFromMutFld . snd)

pgColsToSelFlds :: [PGColumnInfo] -> [(FieldName, AnnField)]
pgColsToSelFlds cols =
  flip map cols $
  \pgColInfo -> (fromPGCol $ pgiColumn pgColInfo, mkAnnColumnField pgColInfo Nothing)

mkDefaultMutFlds :: Maybe [PGColumnInfo] -> MutationOutput
mkDefaultMutFlds = MOutMultirowFields . \case
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
          , S.selFrom = Just $ S.FromExp $ pure $ S.FIIden cteAlias
          }
    in maybe countExp (S.SEUnsafe . T.pack . show) preCalAffRows
  MExp t -> S.SELit t
  MRet selFlds ->
    let tabFrom = FromIden cteAlias
        tabPerm = TablePerm annBoolExpTrue Nothing
    in S.SESelect $ mkSQLSelect JASMultipleRows $
       AnnSelectG selFlds tabFrom tabPerm noSelectArgs strfyNum
  where
    cteAlias = qualTableToAliasIden qt

mkMutationOutputExp
  :: QualifiedTable -> Maybe Int -> S.CTE -> MutationOutput -> Bool -> S.SelectWith
mkMutationOutputExp qt preCalAffRows cte mutOutput strfyNum =
  S.SelectWith [(S.Alias cteAlias, cte)] sel
  where
    cteAlias = qualTableToAliasIden qt
    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing] }

    extrExp = case mutOutput of
      MOutMultirowFields mutFlds ->
        let jsonBuildObjArgs = flip concatMap mutFlds $
              \(FieldName k, mutFld) -> [S.SELit k, mkMutFldExp qt preCalAffRows strfyNum mutFld]
        in S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing

      MOutSinglerowObject annFlds ->
        let tabFrom = FromIden cteAlias
            tabPerm = TablePerm annBoolExpTrue Nothing
        in S.SESelect $ mkSQLSelect JASSingleObject $
           AnnSelectG annFlds tabFrom tabPerm noSelectArgs strfyNum


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
