module Hasura.Backends.Postgres.Translate.Returning
  ( mkMutFldExp
  , mkDefaultMutFlds
  , mkMutationOutputExp
  , checkRetCols
  ) where

import           Hasura.Prelude

import qualified Data.Text                                 as T

import qualified Hasura.Backends.Postgres.SQL.DML          as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Translate.Select
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types


pgColsToSelFlds :: [ColumnInfo 'Postgres] -> [(FieldName, AnnField 'Postgres)]
pgColsToSelFlds cols =
  flip map cols $
  \pgColInfo -> (fromPGCol $ pgiColumn pgColInfo, mkAnnColumnField pgColInfo Nothing)

mkDefaultMutFlds :: Maybe [ColumnInfo 'Postgres] -> MutationOutput 'Postgres
mkDefaultMutFlds = MOutMultirowFields . \case
  Nothing   -> mutFlds
  Just cols -> ("returning", MRet $ pgColsToSelFlds cols):mutFlds
  where
    mutFlds = [("affected_rows", MCount)]

mkMutFldExp :: Identifier -> Maybe Int -> Bool -> MutFld 'Postgres -> S.SQLExp
mkMutFldExp cteAlias preCalAffRows strfyNum = \case
  MCount ->
    let countExp = S.SESelect $
          S.mkSelect
          { S.selExtr = [S.Extractor S.countStar Nothing]
          , S.selFrom = Just $ S.FromExp $ pure $ S.FIIdentifier cteAlias
          }
    in maybe countExp (S.SEUnsafe . T.pack . show) preCalAffRows
  MExp t -> S.SELit t
  MRet selFlds ->
    let tabFrom = FromIdentifier cteAlias
        tabPerm = TablePerm annBoolExpTrue Nothing
    in S.SESelect $ mkSQLSelect JASMultipleRows $
       AnnSelectG selFlds tabFrom tabPerm noSelectArgs strfyNum


{- Note [Mutation output expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An example output expression for INSERT mutation:

WITH "<table-name>__mutation_result_alias" AS (
  INSERT INTO <table-name> (<insert-column>[..])
  VALUES
    (<insert-value-row>[..])
    ON CONFLICT ON CONSTRAINT "<table-constraint-name>" DO NOTHING RETURNING *,
    -- An extra column expression which performs the 'CHECK' validation
    CASE
      WHEN (<CHECK Condition>) THEN NULL
      ELSE "hdb_catalog"."check_violation"('insert check constraint failed')
    END
),
"<table-name>__all_columns_alias" AS (
  -- Only extract columns from mutated rows. Columns sorted by ordinal position so that
  -- resulted rows can be casted to table type.
  SELECT (<table-column>[..])
  FROM
    "<table-name>__mutation_result_alias"
)
<SELECT statement to generate mutation response using '<table-name>__all_columns_alias' as FROM>
-}

-- | Generate mutation output expression with given mutation CTE statement.
-- See Note [Mutation output expression].
mkMutationOutputExp
  :: QualifiedTable
  -> [ColumnInfo 'Postgres]
  -> Maybe Int
  -> S.CTE
  -> MutationOutput 'Postgres
  -> Bool
  -> S.SelectWith
mkMutationOutputExp qt allCols preCalAffRows cte mutOutput strfyNum =
  S.SelectWith [ (S.Alias mutationResultAlias, cte)
               , (S.Alias allColumnsAlias, allColumnsSelect)
               ] sel
  where
    mutationResultAlias = Identifier $ snakeCaseQualifiedObject qt <> "__mutation_result_alias"
    allColumnsAlias = Identifier $ snakeCaseQualifiedObject qt <> "__all_columns_alias"
    allColumnsSelect = S.CTESelect $ S.mkSelect
                       { S.selExtr = map (S.mkExtr . pgiColumn) $ sortCols allCols
                       , S.selFrom = Just $ S.mkIdenFromExp mutationResultAlias
                       }

    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing] }
          where
            extrExp = case mutOutput of
              MOutMultirowFields mutFlds ->
                let jsonBuildObjArgs = flip concatMap mutFlds $
                      \(FieldName k, mutFld) -> [ S.SELit k
                                                , mkMutFldExp allColumnsAlias preCalAffRows strfyNum mutFld
                                                ]
                in S.SEFnApp "json_build_object" jsonBuildObjArgs Nothing

              MOutSinglerowObject annFlds ->
                let tabFrom = FromIdentifier allColumnsAlias
                    tabPerm = TablePerm annBoolExpTrue Nothing
                in S.SESelect $ mkSQLSelect JASSingleObject $
                   AnnSelectG annFlds tabFrom tabPerm noSelectArgs strfyNum


checkRetCols
  :: (UserInfoM m, QErrM m)
  => FieldInfoMap (FieldInfo 'Postgres)
  -> SelPermInfo 'Postgres
  -> [PGCol]
  -> m [ColumnInfo 'Postgres]
checkRetCols fieldInfoMap selPermInfo cols = do
  mapM_ (checkSelOnCol selPermInfo) cols
  forM cols $ \col -> askPGColInfo fieldInfoMap col relInRetErr
  where
    relInRetErr = "Relationships can't be used in \"returning\"."
